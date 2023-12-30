#![allow(dead_code)]
#![allow(unreachable_code)]
#![allow(unused_assignments)]
#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(unused_variables)]

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[derive(Debug)]
struct Backend {
    client: Client,
    semantic_tokens_map: Arc<RwLock<HashMap<Url, Vec<SemanticToken>>>>,
}

impl Backend {
    fn new(client: Client) -> Backend {
        Backend {
            client,
            semantic_tokens_map: Arc::new(RwLock::new(HashMap::new())),
        }
    }
}

macro_rules! semantic_tokens_types {
    ($($name:ident),*) => {
        fn semantic_token_types() -> Vec<SemanticTokenType> {
            let types = vec![$(SemanticTokenType::$name),*];
            $(assert_eq!(SemanticTokenType::$name, types[semantic_tokens::$name as usize]);)*
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
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                position_encoding: None, // TODO: handle position encoding
                text_document_sync: Some(TextDocumentSyncKind::FULL.into()),
                // document_symbol_provider: Some(OneOf::Left(true)),
                // folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
                // completion_provider: Some(CompletionOptions::default()),
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
        assert_eq!(content_changes.len(), 1);
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
}

impl Backend {
    async fn parse_text(&self, uri: Url, version: i32, text: &str) {
        let tree = zig::Ast::parse(text.as_bytes(), zig::ast::Mode::Zig);

        let semantic_tokens = self.get_semantic_tokens(&tree);
        {
            let mut map = self.semantic_tokens_map.write().await;
            map.insert(uri.clone(), semantic_tokens);
        }

        let diags = get_diagnostics(&uri, &tree);
        self.client
            .publish_diagnostics(uri.clone(), diags, Some(version))
            .await;
    }
}

fn get_diagnostics(uri: &Url, tree: &zig::Ast<'_>) -> Vec<Diagnostic> {
    let mut diags = Vec::with_capacity(tree.errors.len());
    let mut related_info = Vec::with_capacity(2);

    for error in tree.errors.iter().rev() {
        let loc = tree.token_location(0, error.token);
        let line = loc.line as u32;
        let character = loc.column as u32 + tree.error_offset(error);
        let start = Position { line, character };
        let range = Range { start, end: start };
        let message = error.message(tree).into_owned();

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
                        assert!(*previous_index < token_index);
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
