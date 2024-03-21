use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::errors::{Severity, SourceError};
use crate::lifetime_checker::AllocationLifetime;
use crate::parser::{AstNode, Block, BlockId, NodeId, PointerType};
use crate::typechecker::{
    EnumVariant, FunId, Function, Module, ModuleId, Type, TypeId, VarId, Variable,
    C_STRING_TYPE_ID, UNKNOWN_TYPE_ID,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CaseOffset(pub usize);

#[derive(Debug, Copy, Clone)]
pub enum CallTarget {
    Function(FunId),
    EnumConstructor(TypeId, CaseOffset),
    NodeId(NodeId),
}

// Indexing into these vectors by the correct index type is enforced by the following factors
// * vectors are private and can only be accessed from within this file
// * this file does not contain any of the implementation details of the june compiler, it is purely an abstraction over the compiler's data moved_owned_values
// * the only way to interact with these vectors is through helper methods that take the required indexing type as an argument
#[derive(Debug, Default)]
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

    pub file_offsets: Vec<(PathBuf, usize, usize)>, // fname, start, end

    // Definitions:
    // indexed by VarId
    pub variables: Vec<Variable>,
    // indexed by FunId
    pub functions: Vec<Function>,
    // indexed by TypeId
    types: Vec<Type>,
    // indexed by ModuleId
    modules: Vec<Module>,

    // `modules` and `module_resolution` are populated by the typechecker
    // `module_lookup` and `module_lookup_use` are populated by the parser
    // lookup the node_id of the parsed block representing a module using it's absolute path
    pub module_lookup: HashMap<PathBuf, NodeId>,
    // lookup the id of the block (eg the entire module's loaded source) from the node_id of the path in the use statement
    pub module_lookup_use: HashMap<NodeId, NodeId>,

    // Memory reclamation
    pub exiting_blocks: HashMap<NodeId, Vec<BlockId>>,

    // Methods on types
    pub methods_on_type: HashMap<TypeId, Vec<FunId>>,
    pub virtual_methods_on_type: HashMap<TypeId, Vec<FunId>>,

    // Use/def
    pub call_resolution: HashMap<NodeId, CallTarget>,
    pub var_resolution: HashMap<NodeId, VarId>,
    pub fun_resolution: HashMap<NodeId, FunId>,
    pub type_resolution: HashMap<NodeId, TypeId>,
    // lookup the id of a Module from the id of the block (value field from module_lookup_use) in the ast
    pub module_resolution: HashMap<NodeId, ModuleId>,
    pub base_classes: HashMap<TypeId, Vec<TypeId>>,

    pub errors: Vec<SourceError>,
}

impl Compiler {
    pub fn new() -> Self {
        Default::default()
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
                "  {}: {:?} ({}) (lifetime: {:?})",
                node_id,
                node,
                self.pretty_type(self.node_types[node_id]),
                self.node_lifetimes[node_id]
            );
        }

        println!("Blocks:");
        for (block_id, block) in self.blocks.iter().enumerate() {
            println!("{}: {:?}", block_id, block);
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
        for (type_id, ty) in self.types.iter().skip(C_STRING_TYPE_ID.0 + 1).enumerate() {
            println!("  {}: {:?}", type_id + C_STRING_TYPE_ID.0 + 1, ty);
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
                type_params,
                lifetime_annotations,
                return_ty,
                initial_node_id,
                block,
                is_extern,
            } => {
                if *is_extern {
                    print!("extern ");
                }
                println!("Fun:[{}]", node_id.0);
                self.print_helper(name, indent + 2);
                if let Some(type_params) = type_params {
                    self.print_helper(type_params, indent + 2);
                }
                self.print_helper(params, indent + 2);
                for lifetime_annotation in lifetime_annotations {
                    self.print_helper(lifetime_annotation, indent + 2);
                }
                if let Some(return_ty) = return_ty {
                    self.print_helper(return_ty, indent + 2);
                }
                if let Some(initial_node_id) = initial_node_id {
                    self.print_helper(initial_node_id, indent + 2);
                }
                if let Some(block) = block {
                    self.print_helper(block, indent + 2);
                }
            }
            AstNode::Struct {
                typename: name,
                fields,
                methods,
                explicit_no_alloc,
                base_class: _,
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
                filename = fname.display().to_string();
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
                    eprint!("╍"); //▔
                }
                eprintln!(" error: {}", message);
            }
            Severity::Note => {
                eprint!("\x1b[0;34m");
                for _ in span_start..span_end {
                    eprint!("╌");
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

    pub fn add_file<P>(&mut self, fname: P)
    where
        P: AsRef<Path>,
    {
        let fname = fname.as_ref();
        let contents = std::fs::read(fname);

        let Ok(contents) = contents else {
            eprintln!("can't find {}", fname.display());
            std::process::exit(1)
        };

        let span_offset = self.source.len();

        self.file_offsets
            .push((fname.to_owned(), span_offset, span_offset + contents.len()));

        self.source.extend_from_slice(&contents);
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

    pub fn create_node(&mut self, ast_node: AstNode, span_start: usize, span_end: usize) -> NodeId {
        self.span_start.push(span_start);
        self.span_end.push(span_end);
        self.push_node(ast_node)
    }

    /// Takes a node_id to replace and a closure which takes the original node and it's new location as inputs
    /// and returns the new node to insert in the old node's location as it's output
    ///
    /// Returns the node_id of the new location for the original node
    ///
    /// # INVARIANTS
    ///
    /// * The user must maintain a 1 to 1 mapping between the vectors for nodes in the ast and the vector of types
    /// * this function swaps the nodes in the ast and the associated list of types, but leaves the type for the new node as unknown
    pub fn replace_node<F>(&mut self, node_id: NodeId, f: F) -> NodeId
    where
        F: FnOnce(&AstNode, NodeId) -> AstNode,
    {
        let node = self.get_node(node_id).clone();
        let span_start = self.span_start[node_id.0];
        let span_end = self.span_end[node_id.0];
        let new_node_id = self.create_node(node.clone(), span_start, span_end);
        if let Some(var) = self.var_resolution.remove(&node_id) {
            self.var_resolution.insert(new_node_id, var);
        }
        self.node_types.push(UNKNOWN_TYPE_ID);
        let new_node = f(&node, new_node_id);
        self.ast_nodes[node_id.0] = new_node;
        self.node_types.swap(node_id.0, new_node_id.0);
        new_node_id
    }

    pub fn resize_node_types(&mut self, size: usize, type_id: TypeId) {
        self.node_types.resize(size, type_id)
    }

    pub fn get_node_type(&self, node_id: NodeId) -> TypeId {
        self.node_types[node_id.0]
    }

    pub fn resolve_type(&self, type_id: TypeId, local_inferences: &[TypeId]) -> TypeId {
        match self.get_type(type_id) {
            Type::FunLocalTypeVar { offset } => local_inferences[*offset],
            _ => type_id,
        }
    }

    pub fn resolve_node_type(&self, node_id: NodeId, local_inferences: &[TypeId]) -> TypeId {
        match &self.types[self.node_types[node_id.0].0] {
            Type::FunLocalTypeVar { offset } => local_inferences[*offset],
            Type::RawBuffer(inner_type_id) => {
                let resolved_inner_type_id = self.resolve_type(*inner_type_id, local_inferences);
                self.find_type(Type::RawBuffer(resolved_inner_type_id))
            }
            _ => self.node_types[node_id.0],
        }
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

    pub fn find_type(&self, ty: Type) -> TypeId {
        for (idx, t) in self.types.iter().enumerate() {
            if &ty == t {
                return TypeId(idx);
            }
        }

        panic!("internal error: can't find Type as a TypeId for: {:?}", ty)
    }

    pub fn is_type_variable(&self, type_id: TypeId) -> bool {
        matches!(self.get_type(type_id), Type::TypeVariable(_))
    }

    pub fn is_generic_type(&self, type_id: TypeId, mut seen: Vec<TypeId>) -> bool {
        // The `seen` parameter is used to protect the recursion from going infinite. Once we see a type,
        // before we destructure it, we log that we have seen it so we do not check it again.
        if seen.contains(&type_id) {
            return false;
        }
        seen.push(type_id);

        match self.get_type(type_id) {
            Type::Bool
            | Type::CChar
            | Type::CExternalType(..)
            | Type::CString
            | Type::CVoidPtr
            | Type::Void
            | Type::I64
            | Type::F64
            | Type::CInt
            | Type::CSizeT => false,
            Type::TypeVariable(..) => true,
            Type::FunLocalTypeVar { .. } => true,
            Type::Unknown => true,
            Type::Range(x) => self.is_generic_type(*x, seen),
            Type::RawBuffer(x) => self.is_generic_type(*x, seen),
            Type::Fun { params, ret } => {
                params.iter().any(|x| {
                    let var = self.get_variable(x.var_id);
                    self.is_generic_type(var.ty, seen.clone())
                }) || self.is_generic_type(*ret, seen)
            }
            Type::Struct {
                generic_params,
                fields,
                ..
            } => {
                !generic_params.is_empty()
                    || fields
                        .iter()
                        .any(|x| self.is_generic_type(x.ty, seen.clone()))
            }
            Type::Enum {
                generic_params,
                variants,
            } => {
                !generic_params.is_empty()
                    || variants.iter().any(|x| match x {
                        EnumVariant::Simple { .. } => false,
                        EnumVariant::Single { param, .. } => {
                            self.is_generic_type(*param, seen.clone())
                        }
                        EnumVariant::Struct { params, .. } => params
                            .iter()
                            .any(|(_, ty)| self.is_generic_type(*ty, seen.clone())),
                    })
            }
            Type::Pointer { target, .. } => self.is_generic_type(*target, seen),
        }
    }

    pub fn get_underlying_type_id(&self, type_id: TypeId) -> TypeId {
        match self.get_type(type_id) {
            Type::Pointer {
                target: type_id, ..
            } => *type_id,
            _ => type_id,
        }
    }

    pub fn get_variable(&self, var_id: VarId) -> &Variable {
        &self.variables[var_id.0]
    }

    pub fn fresh_type_variable(&mut self, node_id: NodeId) -> TypeId {
        self.types.push(Type::TypeVariable(node_id));

        TypeId(self.types.len() - 1)
    }

    pub fn pretty_type(&self, type_id: TypeId) -> String {
        match self.get_type(type_id) {
            Type::Bool => "bool".into(),
            Type::CChar => "c_char".into(),
            Type::CExternalType(node_id) => {
                format!(
                    "extern({})",
                    String::from_utf8_lossy(self.get_source(*node_id))
                )
            }
            Type::CInt => "c_int".into(),
            Type::CSizeT => "c_size_t".into(),
            Type::CString => "c_string".into(),
            Type::CVoidPtr => "c_voidptr".into(),
            Type::Enum { .. } => {
                //FIXME: give this a name
                "enum".into()
            }
            Type::F64 => "f64".into(),
            Type::Fun { params, ret } => {
                let mut output = "fun (".to_string();
                let mut first = true;
                for param in params {
                    if !first {
                        output += ", ";
                    } else {
                        first = false;
                    }
                    output += &self.pretty_type(self.get_variable(param.var_id).ty);
                }
                output += ") -> ";
                output += &self.pretty_type(*ret);
                output
            }
            Type::I64 => "i64".into(),
            Type::Pointer {
                pointer_type,
                optional,
                target,
            } => {
                let mut output = String::new();
                if matches!(pointer_type, PointerType::Owned) {
                    output += "owned ";
                } else {
                    output += "shared ";
                }
                output += &self.pretty_type(*target);
                if *optional {
                    output += "?"
                }
                output
            }
            Type::Range(type_id) => {
                format!("range({})", self.pretty_type(*type_id))
            }
            Type::RawBuffer(type_id) => {
                format!("[{}]", self.pretty_type(*type_id))
            }
            Type::Struct {
                fields,
                generic_params,
                ..
            } => {
                // FIXME: need more info
                format!(
                    "({:?})struct({:?} generics: {:?})",
                    type_id, fields, generic_params
                )
            }
            Type::FunLocalTypeVar { offset } => format!("<local typevar: {}>", offset),
            Type::TypeVariable(node_id) => {
                format!("<{}>", String::from_utf8_lossy(self.get_source(*node_id)))
            }
            Type::Unknown => "<unknown>".into(),
            Type::Void => "void".into(),
        }
    }

    pub fn num_ast_nodes(&self) -> usize {
        self.ast_nodes.len()
    }

    pub fn get_source(&self, node_id: NodeId) -> &[u8] {
        &self.source[self.span_start[node_id.0]..self.span_end[node_id.0]]
    }

    pub fn get_source_str(&self, node_id: NodeId) -> &str {
        std::str::from_utf8(&self.source[self.span_start[node_id.0]..self.span_end[node_id.0]])
            .unwrap()
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

    pub(crate) fn get_source_path(&self, node: NodeId) -> &Path {
        let position = self.span_start[node.0];
        for &(ref file, start, end) in &self.file_offsets {
            if position >= start && position < end {
                return file.as_path();
            }
        }
        unreachable!("position should always be a valid offset into source content that's already been loaded")
    }

    pub(crate) fn path_from_use(&self, path: NodeId) -> PathBuf {
        let parent_path = self
            .get_source_path(path)
            .parent()
            .expect("source path should be the file itself and have a valid parent directory");

        let bytes = self.get_source(path);
        let ident = std::str::from_utf8(bytes).expect("source should be valid utf8");

        let mut path = parent_path.join(ident);
        path.set_extension("june");
        path
    }

    pub(crate) fn get_module(&self, module: ModuleId) -> &Module {
        self.modules
            .get(module.0)
            .unwrap_or_else(|| panic!("internal error: cannot find module for {module:?}"))
    }

    pub(crate) fn add_module(&mut self, module_block: NodeId, module: Module) -> ModuleId {
        let module_id = ModuleId(self.modules.len());
        self.modules.push(module);
        self.module_resolution.insert(module_block, module_id);
        module_id
    }
}
