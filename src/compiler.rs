use crate::errors::SourceError;
use crate::parser::{AstNode, NodeId};
use crate::typechecker::TypeId;

#[derive(Debug)]
pub struct Compiler {
    pub span_start: Vec<usize>,
    pub span_end: Vec<usize>,
    pub ast_nodes: Vec<AstNode>,
    pub node_types: Vec<TypeId>,

    pub source: Vec<u8>,

    pub file_offsets: Vec<(String, usize, usize)>, // fname, start, end

    pub errors: Vec<SourceError>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            span_start: vec![],
            span_end: vec![],
            ast_nodes: vec![],
            node_types: vec![],

            source: vec![],

            file_offsets: vec![],

            errors: vec![],
        }
    }

    pub fn print(&self) {
        if self.ast_nodes.is_empty() {
            println!("<empty>");
        } else {
            self.print_helper(&NodeId(self.ast_nodes.len() - 1), 0)
        }

        println!("{:?}", self.node_types);
    }

    fn print_helper(&self, node_id: &NodeId, indent: usize) {
        for _ in 0..indent {
            print!(" ")
        }

        match &self.ast_nodes[node_id.0] {
            AstNode::Let {
                variable_name,
                ty,
                initializer,
                is_mutable,
            } => {
                println!(
                    "Let ({}, {}, mutable: {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0], is_mutable
                );
                self.print_helper(variable_name, indent + 2);
                if let Some(ty) = ty {
                    self.print_helper(ty, indent + 2);
                }
                self.print_helper(initializer, indent + 2);
            }
            AstNode::Param { name, ty } => {
                println!(
                    "Param ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(name, indent + 2);
                if let Some(ty) = ty {
                    self.print_helper(ty, indent + 2);
                }
            }
            // AstNode::Closure { params, block } => {
            //     println!(
            //         "Closure ({}, {}):",
            //         self.span_start[node_id.0], self.span_end[node_id.0],
            //     );
            //     self.print_helper(params, indent + 2);
            //     self.print_helper(block, indent + 2);
            // }
            AstNode::Fun {
                name,
                params,
                return_ty,
                block,
            } => {
                println!("Fun:",);
                self.print_helper(name, indent + 2);
                self.print_helper(params, indent + 2);
                if let Some(return_ty) = return_ty {
                    self.print_helper(return_ty, indent + 2);
                }
                self.print_helper(block, indent + 2);
            }
            AstNode::Struct { name, fields } => {
                println!("Struct:");
                self.print_helper(name, indent + 2);
                for field in fields {
                    self.print_helper(&field.0, indent + 2);
                    self.print_helper(&field.1, indent + 2);
                }
            }
            AstNode::Block(nodes) => {
                println!(
                    "Block ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                for node in nodes {
                    self.print_helper(node, indent + 2);
                }
            }
            AstNode::New(allocation_type, node) => {
                println!(
                    "New {:?} ({}, {}):",
                    allocation_type, self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(node, indent + 2);
            }
            AstNode::NamedValue { name, value } => {
                println!(
                    "NamedValue ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(name, indent + 2);
                self.print_helper(value, indent + 2);
            }
            AstNode::Params(nodes) => {
                print!(
                    "Params ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                if nodes.is_empty() {
                    println!(" <empty>");
                } else {
                    println!();
                }

                for node in nodes {
                    self.print_helper(node, indent + 2);
                }
            }
            AstNode::Call { head, args } => {
                println!(
                    "Call ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(head, indent + 2);

                for arg in args {
                    self.print_helper(arg, indent + 2);
                }
            }
            AstNode::BinaryOp { lhs, op, rhs } => {
                println!(
                    "BinaryOp ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );

                self.print_helper(lhs, indent + 2);
                self.print_helper(op, indent + 2);
                self.print_helper(rhs, indent + 2)
            }
            AstNode::Range { lhs, rhs } => {
                println!(
                    "Range ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );

                self.print_helper(lhs, indent + 2);
                self.print_helper(rhs, indent + 2)
            }
            AstNode::MemberAccess { target, field } => {
                println!(
                    "MemberAccess ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );

                self.print_helper(target, indent + 2);
                self.print_helper(field, indent + 2)
            }
            AstNode::If {
                condition,
                then_block,
                else_expression,
            } => {
                println!(
                    "If ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(condition, indent + 2);
                self.print_helper(then_block, indent + 2);
                if let Some(else_expression) = else_expression {
                    self.print_helper(else_expression, indent + 2)
                }
            }
            AstNode::While { condition, block } => {
                println!(
                    "While ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(condition, indent + 2);
                self.print_helper(block, indent + 2);
            }
            x => {
                println!(
                    "{:?} ({}, {})",
                    x, self.span_start[node_id.0], self.span_end[node_id.0],
                )
            }
        }
    }

    // line number, line start, line_end
    pub fn line_extents(
        &self,
        span_position: usize,
        file_span_start: usize,
        file_span_end: usize,
    ) -> (usize, usize, usize) {
        let contents = &self.source;

        let line_number = contents[0..span_position].split(|x| *x == b'\n').count();

        let mut line_start = span_position;
        while line_start > file_span_start && contents[line_start] != b'\n' {
            line_start -= 1;
        }
        if span_position != line_start {
            line_start += 1;
        }

        let mut line_end = span_position;
        while line_end < file_span_end && contents[line_end] != b'\n' {
            line_end += 1;
        }

        (line_number, line_start, line_end)
    }

    pub fn print_error(&self, error: &SourceError) {
        let SourceError { node_id, message } = error;

        let span_start = self.span_start[node_id.0];
        let span_end = self.span_end[node_id.0];

        let mut filename = "unknown".to_string();
        let mut file_span_start = 0;
        let mut file_span_end = 0;

        for (fname, file_start, file_end) in &self.file_offsets {
            if span_start >= *file_start && span_start < *file_end {
                filename = fname.clone();
                file_span_start = *file_start;
                file_span_end = *file_end;
                break;
            }
        }

        let (line_number, line_start, line_end) =
            self.line_extents(span_start, file_span_start, file_span_end);

        let line_number_width = format!("{}", line_number).len();

        let max_number_width = if (line_end + 1) < file_span_end {
            let (next_line_number, _, _) =
                self.line_extents(line_end + 1, file_span_start, file_span_end);
            format!("{}", next_line_number).len()
        } else {
            line_number_width
        };

        for _ in 0..(max_number_width + 2) {
            print!("─");
        }
        println!(
            "┬─ \x1b[0;36m{}:{}:{}\x1b[0m",
            filename,
            line_number,
            span_start - line_start + 1
        );

        // Previous line in the source code, if available
        if line_start > (file_span_start + 1) {
            let (prev_line_number, prev_line_start, prev_line_end) =
                self.line_extents(line_start - 1, file_span_start, file_span_end);
            let prev_line_number_str = format!("{}", prev_line_number);

            for _ in 0..(max_number_width - prev_line_number_str.len()) {
                print!(" ")
            }

            println!(
                " {} │ {}",
                prev_line_number_str,
                String::from_utf8_lossy(&self.source[prev_line_start..prev_line_end])
            );
        }

        // Line being highlighted
        for _ in 0..(max_number_width - line_number_width) {
            print!(" ")
        }

        println!(
            " {} │ {}",
            line_number,
            String::from_utf8_lossy(&self.source[line_start..line_end])
        );

        for _ in 0..(line_number_width + 2) {
            print!(" ");
        }
        print!("│");
        for _ in 0..(span_start - line_start + 1) {
            print!(" ");
        }

        print!("\x1b[0;31m");
        for _ in span_start..span_end {
            print!("▰");
        }
        println!(" error: {}", message);
        print!("\x1b[0m");

        // Next line after error, for context
        if (line_end + 1) < file_span_end {
            let (next_line_number, next_line_start, next_line_end) =
                self.line_extents(line_end + 1, file_span_start, file_span_end);

            println!(
                " {} │ {}",
                next_line_number,
                String::from_utf8_lossy(&self.source[next_line_start..next_line_end])
            );
        }

        for _ in 0..(max_number_width + 2) {
            print!("─");
        }
        println!("┴─");
    }

    pub fn add_file(&mut self, fname: &str, contents: &[u8]) {
        let span_offset = self.source.len();

        self.file_offsets
            .push((fname.to_string(), span_offset, span_offset + contents.len()));

        self.source.extend_from_slice(contents);
    }

    pub fn span_offset(&self) -> usize {
        self.source.len()
    }

    pub fn node_id_offset(&self) -> usize {
        self.ast_nodes.len()
    }
}
