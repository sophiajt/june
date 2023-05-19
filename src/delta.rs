use crate::parser::{AstNode, NodeId};

#[derive(Debug)]
pub struct EngineDelta<'a> {
    pub node_id_offset: usize,
    pub span_start: Vec<usize>,
    pub span_end: Vec<usize>,
    pub ast_nodes: Vec<AstNode>,
    pub contents: &'a [u8],
}

impl<'a> EngineDelta<'a> {
    pub fn new(node_id_offset: usize, contents: &'a [u8]) -> Self {
        Self {
            node_id_offset,
            span_start: vec![],
            span_end: vec![],
            ast_nodes: vec![],
            contents,
        }
    }

    pub fn print(&self) {
        if self.ast_nodes.is_empty() {
            println!("<empty>");
        } else {
            self.print_helper(&NodeId(self.ast_nodes.len() - 1), 0)
        }
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
                block,
            } => {
                println!("Fun:",);
                self.print_helper(name, indent + 2);
                self.print_helper(params, indent + 2);
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
            AstNode::New(node) => {
                println!(
                    "New ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
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
            x => {
                println!(
                    "{:?} ({}, {})",
                    x, self.span_start[node_id.0], self.span_end[node_id.0],
                )
            }
        }
    }
}
