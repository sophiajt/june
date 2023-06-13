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

    pub fn print_error(&self, error: &SourceError) {
        let SourceError { node_id, message } = error;

        let span_start = self.span_start[node_id.0];
        let span_end = self.span_end[node_id.0];

        let contents = &self.source;

        let line_number = contents[0..span_start].split(|x| *x == b'\n').count();

        let mut line_start = span_start;
        while line_start > 0 && contents[line_start] != b'\n' {
            line_start -= 1;
        }
        line_start += 1;

        let mut line_end = span_end;
        while line_end < contents.len() && contents[line_end] != b'\n' {
            line_end += 1;
        }

        println!("line: {}", line_number);
        println!(
            "{}",
            String::from_utf8_lossy(&contents[line_start..line_end])
        );
        for _ in line_start..span_start {
            print!(" ");
        }
        for _ in span_start..span_end {
            print!("-");
        }
        println!(" {}", message);
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
