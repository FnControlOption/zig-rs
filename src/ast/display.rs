use super::*;

impl Ast<'_> {
    pub fn display(&self) -> Display {
        Display::new(self)
    }
}

pub struct Display<'a, 'b> {
    pub tree: &'a Ast<'b>,
    pub node: &'a Node,
}

impl<'a, 'b> Display<'a, 'b> {
    pub fn new(tree: &'a Ast<'b>) -> Self {
        Self {
            tree,
            node: tree.node(0),
        }
    }
}

impl std::fmt::Display for Display<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut visitor = DumpVisitor::new();
        visitor.accept(self.tree, self.node);
        visitor.finish().dump(f)
    }
}

struct NodeInfo {
    lines: Vec<String>,
    children: Vec<NodeInfo>,
}

impl NodeInfo {
    fn new() -> Self {
        Self {
            lines: Vec::new(),
            children: Vec::new(),
        }
    }

    fn dump(self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.dump_recursive(&mut String::new(), "", "", f)
    }

    fn dump_recursive(
        self,
        prefix: &mut String,
        pointer: &str,
        preadd: &str,
        f: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        let last_line = self.lines.len() - 1;
        for (index, mut line) in self.lines.into_iter().enumerate() {
            if index == 0 {
                writeln!(f, "{prefix}{pointer}* {line}")?;
            } else if index == last_line && self.children.is_empty() {
                writeln!(f, "{prefix}{preadd}└ {line}")?;
            } else {
                writeln!(f, "{prefix}{preadd}│ {line}")?;
            }
        }

        if self.children.is_empty() {
            return Ok(());
        }

        let prefix_len = prefix.len();
        prefix.push_str(preadd);

        let last_child = self.children.len() - 1;
        for (index, child) in self.children.into_iter().enumerate() {
            if index == last_child {
                child.dump_recursive(prefix, "└── ", "    ", f)?;
            } else {
                child.dump_recursive(prefix, "├── ", "│   ", f)?;
            }
        }

        prefix.truncate(prefix_len);
        Ok(())
    }
}

struct DumpVisitor {
    stack: Vec<NodeInfo>,
    current: NodeInfo,
}

impl DumpVisitor {
    fn new() -> Self {
        Self {
            stack: Vec::new(),
            current: NodeInfo::new(),
        }
    }

    fn visit_field(&mut self, mut f: impl FnMut(&mut Self)) {
        let container = std::mem::replace(&mut self.current, NodeInfo::new());
        self.stack.push(container);
        f(self);
        let field = std::mem::replace(&mut self.current, self.stack.pop().unwrap());
        self.current.children.push(field);
    }

    fn finish(self) -> NodeInfo {
        let mut stack = self.stack;
        let mut current = self.current;
        while let Some(container) = stack.pop() {
            let field = std::mem::replace(&mut current, container);
            current.children.push(field);
        }
        current
    }
}

macro_rules! dump {
    ($visitor:ident, $($arg:tt)*) => {{
        $visitor.current.lines.push(format!($($arg)*));
    }};
}

impl Visitor for DumpVisitor {
    fn visit(&mut self, tree: &Ast, node: &Node) -> bool {
        let ntag = node.tag;
        if self.stack.is_empty() && ntag == node::Tag::Root {
            let mode = tree.mode;
            dump!(self, "{ntag:?} ({mode:?})");
        } else {
            let ttag = tree.token_tag(node.main_token);
            let start = tree.token_start(node.main_token);
            dump!(self, "{ntag:?} @ source[{start}] / {ttag:?}");
        }
        true
    }

    fn accept_token(&mut self, tree: &Ast, index: TokenIndex) {
        self.visit_field(|this| {
            if index == 0 {
                dump!(this, "omitted token");
            } else {
                let tag = tree.token_tag(index);
                let start = tree.token_start(index);
                dump!(this, "token[{index}] @ source[{start}] / {tag:?}");
            }
        });
    }

    fn accept_child(&mut self, tree: &Ast, index: node::Index) {
        self.visit_field(|this| {
            if index == 0 {
                dump!(this, "omitted node");
            } else {
                dump!(this, "node[{index}]");
                this.accept(tree, tree.node(index));
            }
        });
    }

    fn accept_extra_child(&mut self, tree: &Ast, index: node::Index) {
        self.visit_field(|this| {
            let node_index = tree.extra_data(index);
            dump!(this, "extra[{index}] -> node[{node_index}]");
            this.accept(tree, tree.node(node_index));
        });
    }

    fn accept_extra_children(&mut self, tree: &Ast, range: std::ops::Range<node::Index>) {
        dump!(self, "extra[{range:?}]");
        for index in range {
            self.accept_extra_child(tree, index);
        }
    }
}
