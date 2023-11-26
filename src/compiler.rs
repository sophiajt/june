use std::collections::HashMap;

use crate::errors::{Severity, SourceError};
use crate::lifetime_checker::AllocationLifetime;
use crate::parser::{AstNode, Block, NodeId};
use crate::typechecker::{FunId, Function, Type, TypeId, VarId, Variable, STRING_TYPE_ID};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CaseOffset(pub usize);

#[derive(Debug)]
pub enum CallTarget {
    Function(FunId),
    EnumConstructor(TypeId, CaseOffset),
}

#[derive(Debug)]
pub struct Compiler {
    // Core information, indexed by NodeId
    pub span_start: Vec<usize>,
    pub span_end: Vec<usize>,
    ast_nodes: Vec<AstNode>,
    node_types: Vec<TypeId>,
    node_lifetimes: Vec<AllocationLifetime>,

    // Blocks, indexed by BlockId
    pub blocks: Vec<Block>,

    pub source: Vec<u8>,

    pub file_offsets: Vec<(String, usize, usize)>, // fname, start, end

    // Definitions:
    // indexed by VarId
    pub variables: Vec<Variable>,
    // indexed by FunId
    pub functions: Vec<Function>,
    // indexed by TypeId
    types: Vec<Type>,

    // Use/def
    pub call_resolution: HashMap<NodeId, CallTarget>,
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

            call_resolution: HashMap::new(),
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
        println!("num nodes: {}", self.ast_nodes.len());
        println!("{:?}", self.ast_nodes.last());
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

        println!("Call resolution");
        println!("{:?}", self.call_resolution);
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
                lifetime_annotations,
                return_ty,
                block,
            } => {
                println!("Fun:[{}]", node_id.0);
                self.print_helper(name, indent + 2);
                self.print_helper(params, indent + 2);
                for lifetime_annotation in lifetime_annotations {
                    self.print_helper(lifetime_annotation, indent + 2);
                }
                if let Some(return_ty) = return_ty {
                    self.print_helper(return_ty, indent + 2);
                }
                self.print_helper(block, indent + 2);
            }
            AstNode::Struct {
                typename: name,
                fields,
                methods,
                explicit_no_alloc,
            } => {
                println!(
                    "Struct:[{}] explicitly not an allocator: {}",
                    node_id.0, explicit_no_alloc
                );
                self.print_helper(name, indent + 2);
                for field in fields {
                    self.print_helper(field, indent + 2);
                }
                for method in methods {
                    self.print_helper(method, indent + 2);
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
            AstNode::New(allocation_type, required_lifetime, node) => {
                println!(
                    "New {:?} of {:?} ({}, {}):[{}]",
                    allocation_type,
                    required_lifetime,
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
            AstNode::MethodCall { target, call } => {
                println!(
                    "MethodCall ({}, {}):[{}]",
                    self.span_start[node_id.0], self.span_end[node_id.0], node_id.0
                );
                self.print_helper(target, indent + 2);
                self.print_helper(call, indent + 2);
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
        let SourceError {
            node_id,
            message,
            severity,
        } = error;

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

        match severity {
            Severity::Error => {
                eprint!("\x1b[0;31m");
                for _ in span_start..span_end {
                    eprint!("▰");
                }
                eprintln!(" error: {}", message);
            }
            Severity::Note => {
                eprint!("\x1b[0;34m");
                for _ in span_start..span_end {
                    eprint!("=");
                }
                eprintln!(" note: {}", message);
            }
        }
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

    pub fn get_node(&self, node_id: NodeId) -> &AstNode {
        &self.ast_nodes[node_id.0]
    }

    pub fn get_node_mut(&mut self, node_id: NodeId) -> &mut AstNode {
        &mut self.ast_nodes[node_id.0]
    }

    pub fn push_node(&mut self, ast_node: AstNode) -> NodeId {
        self.ast_nodes.push(ast_node);

        NodeId(self.ast_nodes.len() - 1)
    }

    pub fn resize_node_types(&mut self, size: usize, type_id: TypeId) {
        self.node_types.resize(size, type_id)
    }

    pub fn get_node_type(&self, node_id: NodeId) -> TypeId {
        self.node_types[node_id.0]
    }

    pub fn set_node_type(&mut self, node_id: NodeId, type_id: TypeId) {
        self.node_types[node_id.0] = type_id;
    }

    pub fn resize_node_lifetimes(&mut self, size: usize, allocation_lifetime: AllocationLifetime) {
        self.node_lifetimes.resize(size, allocation_lifetime)
    }

    pub fn get_node_lifetime(&self, node_id: NodeId) -> AllocationLifetime {
        self.node_lifetimes[node_id.0]
    }

    pub fn set_node_lifetime(&mut self, node_id: NodeId, allocation_lifetime: AllocationLifetime) {
        self.node_lifetimes[node_id.0] = allocation_lifetime;
    }

    pub fn get_type(&self, type_id: TypeId) -> &Type {
        &self.types[type_id.0]
    }

    pub fn get_type_mut(&mut self, type_id: TypeId) -> &mut Type {
        &mut self.types[type_id.0]
    }

    pub fn get_types(&self) -> &[Type] {
        &self.types
    }

    pub fn push_type(&mut self, ty: Type) -> TypeId {
        self.types.push(ty);

        TypeId(self.types.len() - 1)
    }

    pub fn find_or_create_type(&mut self, ty: Type) -> TypeId {
        for (idx, t) in self.types.iter().enumerate() {
            if &ty == t {
                return TypeId(idx);
            }
        }

        self.types.push(ty);

        TypeId(self.types.len() - 1)
    }

    pub fn get_underlying_type_id(&self, type_id: TypeId) -> TypeId {
        match self.get_type(type_id) {
            Type::Pointer {
                target: type_id, ..
            } => *type_id,
            _ => type_id,
        }
    }

    pub fn fresh_type_variable(&mut self) -> TypeId {
        self.types.push(Type::TypeVariable);

        TypeId(self.types.len() - 1)
    }

    pub fn num_ast_nodes(&self) -> usize {
        self.ast_nodes.len()
    }

    pub fn get_source(&self, node_id: NodeId) -> &[u8] {
        &self.source[self.span_start[node_id.0]..self.span_end[node_id.0]]
    }

    pub fn get_variable_name(&self, var_id: VarId) -> &[u8] {
        self.get_source(self.variables[var_id.0].name)
    }

    pub fn find_pointer_to(&self, type_id: TypeId) -> Option<TypeId> {
        for (found_type_id, ty) in self.types.iter().enumerate() {
            if matches!(ty, Type::Pointer { target: type_id2, ..} if &type_id == type_id2) {
                return Some(TypeId(found_type_id));
            }
        }

        None
    }

    pub fn is_allocator_type(&self, type_id: TypeId) -> bool {
        match &self.types[type_id.0] {
            Type::Pointer {
                target: pointer_type_id,
                ..
            } => self.is_allocator_type(*pointer_type_id),
            Type::Struct { is_allocator, .. } => *is_allocator,
            _ => false,
        }
    }

    pub fn is_copyable_type(&self, type_id: TypeId) -> bool {
        matches!(self.types[type_id.0], Type::Bool | Type::F64 | Type::I64)
    }
}
