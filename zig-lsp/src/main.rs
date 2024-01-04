#![allow(dead_code)]
#![allow(unreachable_code)]
#![allow(unused_assignments)]
#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(unused_variables)]

use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::Arc;

use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use zig::Ast;

mod analysis;
mod offsets;

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[derive(Debug)]
struct Backend {
    client: Client,
    encoding: Arc<RwLock<PositionEncodingKind>>,
    ast_map: Arc<RwLock<HashMap<Url, Ast<'static>>>>,
    semantic_tokens_map: Arc<RwLock<HashMap<Url, Vec<SemanticToken>>>>,
}

impl Backend {
    fn new(client: Client) -> Backend {
        Backend {
            client,
            encoding: Arc::new(RwLock::new(PositionEncodingKind::UTF16)),
            ast_map: Arc::new(RwLock::new(HashMap::new())),
            semantic_tokens_map: Arc::new(RwLock::new(HashMap::new())),
        }
    }
}

macro_rules! semantic_tokens_types {
    ($($name:ident),*) => {
        fn semantic_token_types() -> Vec<SemanticTokenType> {
            let types = vec![$(SemanticTokenType::$name),*];
            $(debug_assert_eq!(SemanticTokenType::$name, types[semantic_tokens::$name as usize]);)*
            types
        }
    };
}

semantic_tokens_types!(TYPE);

mod semantic_tokens {
    use super::*;
    const _: Option<SemanticTokenType> = None;
    pub const TYPE: u32 = 0;
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // params.capabilities.general.map(|g| g.position_encodings)
        let position_encoding = self.encoding.read().await;
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                position_encoding: Some(position_encoding.to_owned()),
                text_document_sync: Some(TextDocumentSyncKind::FULL.into()),
                // document_symbol_provider: Some(OneOf::Left(true)),
                // folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions::default()),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            legend: SemanticTokensLegend {
                                token_types: semantic_token_types(),
                                ..Default::default()
                            },
                            ..Default::default()
                        },
                    ),
                ),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
            server_info: None,
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(
        &self,
        DidOpenTextDocumentParams {
            text_document:
                TextDocumentItem {
                    uri,
                    text,
                    language_id,
                    version,
                },
        }: DidOpenTextDocumentParams,
    ) {
        self.parse_text(uri, version, &text).await;
    }

    async fn did_change(
        &self,
        DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier { uri, version },
            content_changes,
        }: DidChangeTextDocumentParams,
    ) {
        debug_assert_eq!(content_changes.len(), 1);
        let text = &content_changes[0].text;
        self.parse_text(uri, version, text).await;
    }

    async fn semantic_tokens_full(
        &self,
        SemanticTokensParams {
            work_done_progress_params,
            partial_result_params,
            text_document: TextDocumentIdentifier { uri },
        }: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let semantic_tokens = {
            let map = self.semantic_tokens_map.read().await;
            map.get(&uri).cloned()
        };
        Ok(semantic_tokens.map(|data| {
            SemanticTokensResult::Tokens(SemanticTokens {
                data,
                ..Default::default()
            })
        }))
    }

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
            CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
        ])))
    }

    async fn hover(
        &self,
        HoverParams {
            text_document_position_params:
                TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position,
                },
            work_done_progress_params,
        }: HoverParams,
    ) -> Result<Option<Hover>> {
        let ast_map = self.ast_map.read().await;
        let Some(tree) = ast_map.get(&uri) else {
            return Ok(None);
        };

        let source_index = {
            let encoding = self.encoding.read().await;
            offsets::position_to_index(&tree.source, position, &encoding)
        };

        let token_index = offsets::source_index_to_token_index(tree, source_index);

        let pos_context = analysis::get_position_context(&tree.source, source_index, true);

        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!(
                    "## You're hovering!\n{:?} {pos_context:?}",
                    String::from_utf8_lossy(tree.token_slice(token_index))
                ),
            }),
            range: None,
        }))
    }
}

impl Backend {
    async fn parse_text(&self, uri: Url, version: i32, text: &str) {
        {
            let tree = zig::Ast::parse(text.as_bytes(), zig::ast::Mode::Zig);
            let mut map = self.ast_map.write().await;
            map.insert(
                uri.clone(),
                zig::Ast {
                    source: Cow::from(tree.source.into_owned()),
                    ..tree
                },
            );
        }

        let ast_map = self.ast_map.read().await;
        let tree = ast_map.get(&uri).unwrap();

        {
            let semantic_tokens = self.get_semantic_tokens(&tree);
            let mut map = self.semantic_tokens_map.write().await;
            map.insert(uri.clone(), semantic_tokens);
        }

        {
            let encoding = self.encoding.read().await;
            let diags = get_diagnostics(&uri, &tree, &encoding);
            self.client
                .publish_diagnostics(uri.clone(), diags, Some(version))
                .await;
        }
    }
}

fn get_diagnostics(
    uri: &Url,
    tree: &zig::Ast<'_>,
    encoding: &PositionEncodingKind,
) -> Vec<Diagnostic> {
    let mut diags = Vec::with_capacity(tree.errors.len());
    let mut related_info = Vec::with_capacity(2);

    for error in tree.errors.iter().rev() {
        let start = offsets::offset_to_position(
            tree.source.as_ref(),
            tree.token_start(error.token),
            encoding,
        );
        let end = start; // TODO: get token end
        let range = Range { start, end };
        let message = tree.render_error(error).into_owned();

        if error.is_note {
            let uri = uri.clone();
            let location = Location { uri, range };
            related_info.push(DiagnosticRelatedInformation { location, message });
            continue;
        }

        let mut diagnostic = Diagnostic::new_simple(range, message);

        related_info.reverse();
        diagnostic.related_information = Some(related_info);
        related_info = Vec::with_capacity(2);

        diags.push(diagnostic);
    }

    diags.reverse();
    diags
}

impl Backend {
    fn get_semantic_tokens(&self, tree: &zig::Ast<'_>) -> Vec<SemanticToken> {
        use zig::ast::*;
        use zig::*;

        struct SemanticVisitor<'a> {
            backend: &'a Backend,
            tokens: Vec<SemanticToken>,
            previous: Option<(TokenIndex, ast::Location)>,
        }

        impl SemanticVisitor<'_> {
            fn new(backend: &Backend) -> SemanticVisitor {
                SemanticVisitor {
                    backend,
                    tokens: Vec::new(),
                    previous: None,
                }
            }

            fn push_token(&mut self, tree: &Ast, token_index: TokenIndex, token_type: u32) {
                let start_offset = match &self.previous {
                    Some((previous_index, previous_loc)) => {
                        debug_assert!(*previous_index < token_index);
                        previous_loc.line_start
                    }
                    None => 0,
                };

                let token_loc = tree.token_location(start_offset as u32, token_index);
                let token_slice = tree.token_slice(token_index);
                let token_len = token_slice.len();

                let mut delta_line = token_loc.line;
                let mut delta_column = token_loc.column;

                if delta_line == 0 {
                    if let Some((_, previous_loc)) = &self.previous {
                        delta_column -= previous_loc.column;
                    }
                }

                self.previous = Some((token_index, token_loc));

                self.tokens.push(SemanticToken {
                    delta_line: delta_line as u32,
                    delta_start: delta_column as u32,
                    length: token_len as u32,
                    token_type,
                    token_modifiers_bitset: 0,
                });
            }
        }

        impl Visitor for SemanticVisitor<'_> {
            fn visit(&mut self, tree: &Ast, node: &Node) -> bool {
                match node.tag {
                    node::Tag::GlobalVarDecl
                    | node::Tag::LocalVarDecl
                    | node::Tag::SimpleVarDecl
                    | node::Tag::AlignedVarDecl => 'blk: {
                        if node.data.rhs == 0 {
                            break 'blk;
                        }

                        let name_token = node.main_token + 1;
                        let init_node = tree.node(node.data.rhs);
                        let init_token = tree.token_tag(init_node.main_token);

                        match init_node.tag {
                            node::Tag::ContainerDecl
                            | node::Tag::ContainerDeclTrailing
                            | node::Tag::ContainerDeclTwo
                            | node::Tag::ContainerDeclTwoTrailing
                            | node::Tag::ContainerDeclArg
                            | node::Tag::ContainerDeclArgTrailing => match init_token {
                                token::Tag::KeywordStruct
                                | token::Tag::KeywordUnion
                                | token::Tag::KeywordOpaque
                                | token::Tag::KeywordEnum => {
                                    self.push_token(tree, name_token, semantic_tokens::TYPE);
                                }
                                _ => {}
                            },
                            _ => {}
                        }
                    }
                    _ => {}
                }
                true
            }
        }

        let mut visitor = SemanticVisitor::new(self);
        tree.accept(&mut visitor);
        visitor.tokens
    }
}
