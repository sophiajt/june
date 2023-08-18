use std::collections::HashMap;

use crate::errors::SourceError;
use crate::lifetime_checker::AllocationLifetime;
use crate::parser::{AstNode, Block, NodeId};
use crate::typechecker::{FunId, Function, Type, TypeId, VarId, Variable, STRING_TYPE_ID};

#[derive(Debug)]
pub struct Compiler {
    // Core information, indexed by NodeId
    pub span_start: Vec<usize>,
    pub span_end: Vec<usize>,
    pub ast_nodes: Vec<AstNode>,
    pub node_types: Vec<TypeId>,
    pub node_lifetimes: Vec<AllocationLifetime>,

    // Blocks, indexed by BlockId
    pub blocks: Vec<Block>,

    pub source: Vec<u8>,

    pub file_offsets: Vec<(String, usize, usize)>, // fname, start, end

    // Definitions
    pub variables: Vec<Variable>,
    pub functions: Vec<Function>,
    pub types: Vec<Type>,

    // Use/def
    pub fun_resolution: HashMap<NodeId, FunId>,
    pub var_resolution: HashMap<NodeId, VarId>,
    pub type_resolution: HashMap<NodeId, TypeId>,

    pub errors: Vec<SourceError>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            span_start: vec![],
            span_end: vec![],
            ast_nodes: vec![],
            node_types: vec![],
            node_lifetimes: vec![],

            blocks: vec![],

            source: vec![],

            file_offsets: vec![],

            variables: vec![],
            functions: vec![],
            types: vec![],

            fun_resolution: HashMap::new(),
            var_resolution: HashMap::new(),
            type_resolution: HashMap::new(),

            errors: vec![],
        }
    }

    pub fn print(&self) {
        if self.ast_nodes.is_empty() {
            println!("<empty>");
        } else {
            self.print_helper(&NodeId(self.ast_nodes.len() - 1), 0)
        }

        println!("Nodes:");
        for (node_id, node) in self.ast_nodes.iter().enumerate() {
            println!(
                "  {}: {:?} ({:?}) @ ({:?})",
                node_id, node, self.node_types[node_id], self.node_lifetimes[node_id]
            );
        }

        println!("Variables:");
        for (var_id, var) in self.variables.iter().enumerate() {
            println!("  {}: {:?}", var_id, var);
        }

        println!("Functions:");
        for (fun_id, fun) in self.functions.iter().enumerate() {
            println!("  {}: {:?}", fun_id, fun);
        }

        println!("Types:");
        for (type_id, ty) in self.types.iter().skip(STRING_TYPE_ID.0 + 1).enumerate() {
            println!("  {}: {:?}", type_id + STRING_TYPE_ID.0 + 1, ty);
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
                    "Let ({}, {}, mutable: {})[{}]:",
                    self.span_start[node_id.0], self.span_end[node_id.0], is_mutable, node_id.0
                );
                self.print_helper(variable_name, indent + 2);
                if let Some(ty) = ty {
                    self.print_helper(ty, indent + 2);
                }
                self.print_helper(initializer, indent + 2);
            }
            AstNode::Param {
                name,
                ty,
                is_mutable,
            } => {
                println!(
                    "Param ({}, {}, mut: {})[{}]:",
                    self.span_start[node_id.0], self.span_end[node_id.0], is_mutable, node_id.0
                );
                self.print_helper(name, indent + 2);
                self.print_helper(ty, indent + 2);
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
                println!("Fun:[{}]", node_id.0);
                self.print_helper(name, indent + 2);
                self.print_helper(params, indent + 2);
                if let Some(return_ty) = return_ty {
                    self.print_helper(return_ty, indent + 2);
                }
                self.print_helper(block, indent + 2);
            }
            AstNode::Struct { name, fields } => {
                println!("Struct:[{}]", node_id.0);
                self.print_helper(name, indent + 2);
                for field in fields {
                    self.print_helper(&field.0, indent + 2);
                    self.print_helper(&field.1, indent + 2);
                }
            }
            AstNode::Block(block_id) => {
                println!(
                    "Block ({}, {}):[{}]",
                    self.span_start[node_id.0], self.span_end[node_id.0], node_id.0
                );
                for node in &self.blocks[block_id.0].nodes {
                    self.print_helper(node, indent + 2);
                }
            }
            AstNode::New(allocation_type, node) => {
                println!(
                    "New {:?} ({}, {}):[{}]",
                    allocation_type,
                    self.span_start[node_id.0],
                    self.span_end[node_id.0],
                    node_id.0
                );
                self.print_helper(node, indent + 2);
            }
            AstNode::NamedValue { name, value } => {
                println!(
                    "NamedValue ({}, {}):[{}]",
                    self.span_start[node_id.0], self.span_end[node_id.0], node_id.0
                );
                self.print_helper(name, indent + 2);
                self.print_helper(value, indent + 2);
            }
            AstNode::Params(nodes) => {
                print!(
                    "Params ({}, {}):[{}]",
                    self.span_start[node_id.0], self.span_end[node_id.0], node_id.0
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
                    "Call ({}, {}):[{}]",
                    self.span_start[node_id.0], self.span_end[node_id.0], node_id.0
                );
                self.print_helper(head, indent + 2);

                for arg in args {
                    self.print_helper(arg, indent + 2);
                }
            }
            AstNode::BinaryOp { lhs, op, rhs } => {
                println!(
                    "BinaryOp ({}, {}):[{}]",
                    self.span_start[node_id.0], self.span_end[node_id.0], node_id.0
                );

                self.print_helper(lhs, indent + 2);
                self.print_helper(op, indent + 2);
                self.print_helper(rhs, indent + 2)
            }
            AstNode::Range { lhs, rhs } => {
                println!(
                    "Range ({}, {}):[{}]",
                    self.span_start[node_id.0], self.span_end[node_id.0], node_id.0
                );

                self.print_helper(lhs, indent + 2);
                self.print_helper(rhs, indent + 2)
            }
            AstNode::MemberAccess { target, field } => {
                println!(
                    "MemberAccess ({}, {}):[{}]",
                    self.span_start[node_id.0], self.span_end[node_id.0], node_id.0
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
                    "If ({}, {}):[{}]",
                    self.span_start[node_id.0], self.span_end[node_id.0], node_id.0
                );
                self.print_helper(condition, indent + 2);
                self.print_helper(then_block, indent + 2);
                if let Some(else_expression) = else_expression {
                    self.print_helper(else_expression, indent + 2)
                }
            }
            AstNode::While { condition, block } => {
                println!(
                    "While ({}, {}):[{}]",
                    self.span_start[node_id.0], self.span_end[node_id.0], node_id.0
                );
                self.print_helper(condition, indent + 2);
                self.print_helper(block, indent + 2);
            }
            x => {
                println!(
                    "{:?} ({}, {}) [{}]",
                    x, self.span_start[node_id.0], self.span_end[node_id.0], node_id.0
                )
            }
        }
    }

    pub fn has_main(&self) -> bool {
        for f in &self.functions {
            let name = self.get_source(f.name);

            if name == b"main" {
                return true;
            }
        }

        false
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
        if line_start > file_span_start && contents[line_start] == b'\n' {
            line_start -= 1;
        }

        while line_start > file_span_start && contents[line_start] != b'\n' {
            line_start -= 1;
        }
        if contents[line_start] == b'\n' {
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
            eprint!("─");
        }
        eprintln!(
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
                eprint!(" ")
            }

            eprintln!(
                " {} │ {}",
                prev_line_number_str,
                String::from_utf8_lossy(&self.source[prev_line_start..prev_line_end])
            );
        }

        // Line being highlighted
        for _ in 0..(max_number_width - line_number_width) {
            eprint!(" ")
        }

        eprintln!(
            " {} │ {}",
            line_number,
            String::from_utf8_lossy(&self.source[line_start..line_end])
        );

        for _ in 0..(max_number_width + 2) {
            eprint!(" ");
        }
        eprint!("│");
        for _ in 0..(span_start - line_start + 1) {
            eprint!(" ");
        }

        eprint!("\x1b[0;31m");
        for _ in span_start..span_end {
            eprint!("▰");
        }
        eprintln!(" error: {}", message);
        eprint!("\x1b[0m");

        // Next line after error, for context
        if (line_end + 1) < file_span_end {
            let (next_line_number, next_line_start, next_line_end) =
                self.line_extents(line_end + 1, file_span_start, file_span_end);

            eprintln!(
                " {} │ {}",
                next_line_number,
                String::from_utf8_lossy(&self.source[next_line_start..next_line_end])
            );
        }

        for _ in 0..(max_number_width + 2) {
            eprint!("─");
        }
        eprintln!("┴─");
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

    pub fn get_source(&self, node_id: NodeId) -> &[u8] {
        &self.source[self.span_start[node_id.0]..self.span_end[node_id.0]]
    }

    pub fn find_pointer_to(&self, type_id: TypeId) -> Option<TypeId> {
        for (found_type_id, ty) in self.types.iter().enumerate() {
            if matches!(ty, Type::Pointer(_, type_id2) if &type_id == type_id2) {
                return Some(TypeId(found_type_id));
            }
        }

        None
    }

    pub fn is_copyable_type(&self, type_id: TypeId) -> bool {
        // FIXME: we should probably clean up the struct/pointer thing a bit
        !matches!(self.types[type_id.0], Type::Struct(..) | Type::Pointer(..))
    }
}
