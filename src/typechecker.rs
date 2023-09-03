use std::collections::HashMap;

use crate::{
    compiler::{CallTarget, CaseOffset, Compiler},
    errors::SourceError,
    parser::{AllocationType, AstNode, BlockId, NodeId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub usize);

#[derive(Debug, PartialEq)]
pub enum Type {
    Unknown,
    Void,
    I64,
    F64,
    Bool,
    Range(TypeId),
    String,
    Struct {
        fields: Vec<(Vec<u8>, TypeId)>,
        methods: Vec<FunId>,
        is_allocator: bool,
    },
    Enum {
        cases: Vec<EnumCase>,
        methods: Vec<FunId>,
    },
    Pointer(AllocationType, TypeId),
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumCase {
    Simple {
        name: Vec<u8>,
    },
    Single {
        name: Vec<u8>,
        param: TypeId,
    },
    Struct {
        name: Vec<u8>,
        params: Vec<(Vec<u8>, TypeId)>,
    },
}

#[derive(Debug)]
pub struct Variable {
    pub name: NodeId,
    pub ty: TypeId,
    pub is_mutable: bool,
    pub where_defined: NodeId,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Vec<u8>,
    pub var_id: VarId,
}

impl Param {
    pub fn new(name: Vec<u8>, var_id: VarId) -> Param {
        Param { name, var_id }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: NodeId,
    pub params: Vec<Param>,
    pub return_type: TypeId,
    pub body: NodeId,
}

pub struct Scope {
    variables: HashMap<Vec<u8>, VarId>,
    functions: HashMap<Vec<u8>, FunId>,
    types: HashMap<Vec<u8>, TypeId>,
    expected_return_type: Option<TypeId>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
            types: HashMap::new(),
            expected_return_type: None,
        }
    }
}

pub struct Typechecker {
    pub compiler: Compiler,
    pub scope: Vec<Scope>,
}

pub const UNKNOWN_TYPE_ID: TypeId = TypeId(0);
pub const VOID_TYPE_ID: TypeId = TypeId(1);
pub const I64_TYPE_ID: TypeId = TypeId(2);
pub const F64_TYPE_ID: TypeId = TypeId(3);
pub const BOOL_TYPE_ID: TypeId = TypeId(4);
pub const RANGE_I64_TYPE_ID: TypeId = TypeId(5);
pub const STRING_TYPE_ID: TypeId = TypeId(6);

impl Typechecker {
    pub fn new(mut compiler: Compiler) -> Self {
        // temporarily - let's add `println` for now, to get examples to typecheck
        compiler.variables.push(Variable {
            name: NodeId(0),
            ty: UNKNOWN_TYPE_ID,
            is_mutable: false,
            where_defined: NodeId(0),
        });
        compiler.functions.push(Function {
            name: NodeId(0),
            params: vec![Param::new(b"input".to_vec(), VarId(0))],
            body: NodeId(0),
            return_type: VOID_TYPE_ID,
        });

        compiler.types = vec![
            // hardwire in the core types before the user-defined types
            Type::Unknown,
            Type::Void,
            Type::I64,
            Type::F64,
            Type::Bool,
            Type::Range(I64_TYPE_ID),
            Type::String,
        ];

        let mut scope = vec![Scope::new()];

        scope
            .last_mut()
            .expect("internal error: couldn't access function scope")
            .functions
            .insert(b"println".to_vec(), FunId(0));

        Self { compiler, scope }
    }

    pub fn typecheck_typename(&mut self, ty: NodeId) -> TypeId {
        let name = self.compiler.get_source(ty);

        match name {
            b"i64" => I64_TYPE_ID,
            b"f64" => F64_TYPE_ID,
            b"string" => STRING_TYPE_ID,
            b"bool" => BOOL_TYPE_ID,
            b"void" => VOID_TYPE_ID,
            _ => {
                if let Some(type_id) = self.find_type_in_scope(ty) {
                    // Assume custom types are pointers
                    self.find_or_create_type(Type::Pointer(AllocationType::Normal, *type_id))
                } else {
                    self.error(
                        &format!("unknown type: '{}'", String::from_utf8_lossy(name)),
                        ty,
                    );
                    UNKNOWN_TYPE_ID
                }
            }
        }
    }

    pub fn typecheck_fun_predecl(
        &mut self,
        name: NodeId,
        params: NodeId,
        return_ty: Option<NodeId>,
        block: NodeId,
    ) -> FunId {
        let mut fun_params = vec![];
        let fun_name = self.compiler.source
            [self.compiler.span_start[name.0]..self.compiler.span_end[name.0]]
            .to_vec();

        //FIXME: remove clone?
        if let AstNode::Params(unchecked_params) = self.compiler.ast_nodes[params.0].clone() {
            for unchecked_param in unchecked_params {
                if let AstNode::Param {
                    name,
                    ty,
                    is_mutable,
                } = &self.compiler.ast_nodes[unchecked_param.0]
                {
                    let name = *name;
                    let param_name = self.compiler.get_source(name).to_vec();
                    let ty = *ty;
                    let is_mutable = *is_mutable;
                    let ty = self.typecheck_typename(ty);

                    let var_id = self.define_variable(name, ty, is_mutable, name);
                    fun_params.push(Param::new(param_name, var_id));
                } else {
                    self.error("expected function parameter", unchecked_param);
                    break;
                }
            }
        } else {
            self.error("expected function parameters", params)
        }

        let return_type = if let Some(return_ty) = return_ty {
            self.typecheck_typename(return_ty)
        } else {
            VOID_TYPE_ID
        };

        self.compiler.functions.push(Function {
            name,
            params: fun_params,
            return_type,
            body: block,
        });

        let fun_id = self.compiler.functions.len() - 1;

        self.scope
            .last_mut()
            .expect("internal error: missing function scope")
            .functions
            .insert(fun_name, FunId(fun_id));

        FunId(fun_id)
    }

    pub fn typecheck_fun(&mut self, fun_id: FunId) {
        let Function {
            params,
            body,
            return_type,
            ..
        } = self.compiler.functions[fun_id.0].clone();

        self.enter_scope();

        self.set_expected_return_type(return_type);

        for Param { name, var_id, .. } in &params {
            self.add_variable_to_scope(name.clone(), *var_id)
        }

        self.typecheck_node(body);

        // FIXME: check return type

        self.exit_scope();
    }

    pub fn typecheck_enum(
        &mut self,
        name: NodeId,
        cases: Vec<NodeId>,
        methods: Vec<NodeId>,
    ) -> TypeId {
        let enum_name = self.compiler.get_source(name).to_vec();

        let mut output_cases = vec![];

        for enum_case in cases {
            if let AstNode::EnumCase { name, payload } = &self.compiler.ast_nodes[enum_case.0] {
                let case_name = self.compiler.get_source(*name).to_vec();

                match payload {
                    Some(payload) => {
                        if payload.is_empty() {
                            self.error("missing payload in enum case", *name);
                            break;
                        }

                        match &self.compiler.ast_nodes[payload[0].0] {
                            AstNode::NamedValue { .. } => {
                                let mut fields = vec![];
                                let payload = payload.clone();
                                for item in payload {
                                    if let AstNode::NamedValue { name, value } =
                                        &self.compiler.ast_nodes[item.0]
                                    {
                                        let name = *name;
                                        let value = *value;

                                        let field_name = self.compiler.get_source(name).to_vec();

                                        let type_id = self.typecheck_typename(value);

                                        fields.push((field_name, type_id));
                                    } else {
                                        self.error(
                                            "expected 'name: type' for each field in enum case",
                                            item,
                                        );
                                    }
                                }

                                output_cases.push(EnumCase::Struct {
                                    name: case_name,
                                    params: fields,
                                });
                            }
                            AstNode::Type { .. } => {
                                let type_id = self.typecheck_typename(payload[0]);

                                output_cases.push(EnumCase::Single {
                                    name: case_name,
                                    param: type_id,
                                });
                            }
                            _ => {
                                self.error("unexpected node in enum cases", payload[0]);
                            }
                        }
                    }
                    None => {
                        output_cases.push(EnumCase::Simple { name: case_name });
                    }
                }
            } else {
                self.error("expect enum case inside of enum", enum_case)
            }
        }

        self.compiler.types.push(Type::Enum {
            cases: output_cases,
            methods: vec![],
        });

        let type_id = TypeId(self.compiler.types.len() - 1);

        self.compiler
            .types
            .push(Type::Pointer(AllocationType::Normal, type_id));

        self.add_type_to_scope(enum_name, type_id);

        if !methods.is_empty() {
            self.enter_scope();

            self.add_type_to_scope(b"self".to_vec(), type_id);

            let mut fun_ids = vec![];

            for method in methods {
                let AstNode::Fun { name, params, return_ty, block } = &self.compiler.ast_nodes[method.0] else {
                    self.error("internal error: can't find method definition during typecheck", method);
                    return VOID_TYPE_ID;
                };
                let name = *name;
                let params = *params;
                let return_ty = *return_ty;
                let block = *block;

                fun_ids.push(self.typecheck_fun_predecl(name, params, return_ty, block));
            }

            let Type::Struct {
                methods, ..
            } = &mut self.compiler.types[type_id.0] else {
                panic!("internal error: previously inserted struct can't be found");
            };

            *methods = fun_ids.clone();

            for fun_id in &fun_ids {
                self.typecheck_fun(*fun_id);
            }

            self.exit_scope();
        }

        type_id
    }

    pub fn typecheck_struct(
        &mut self,
        name: NodeId,
        fields: Vec<(NodeId, NodeId)>,
        methods: Vec<NodeId>,
        is_allocator: bool,
    ) -> TypeId {
        let struct_name = self.compiler.get_source(name).to_vec();

        let mut output_fields = vec![];

        for (field_name, field_type) in fields {
            let field_name = self.compiler.get_source(field_name).to_vec();
            let field_type = self.typecheck_typename(field_type);

            output_fields.push((field_name, field_type));
        }

        self.compiler.types.push(Type::Struct {
            fields: output_fields,
            methods: vec![],
            is_allocator,
        });

        let type_id = TypeId(self.compiler.types.len() - 1);

        self.compiler
            .types
            .push(Type::Pointer(AllocationType::Normal, type_id));

        self.add_type_to_scope(struct_name, type_id);

        if !methods.is_empty() {
            self.enter_scope();

            self.add_type_to_scope(b"self".to_vec(), type_id);

            let mut fun_ids = vec![];

            for method in methods {
                let AstNode::Fun { name, params, return_ty, block } = &self.compiler.ast_nodes[method.0] else {
                    self.error("internal error: can't find method definition during typecheck", method);
                    return VOID_TYPE_ID;
                };
                let name = *name;
                let params = *params;
                let return_ty = *return_ty;
                let block = *block;

                fun_ids.push(self.typecheck_fun_predecl(name, params, return_ty, block));
            }

            let Type::Struct {
                methods, ..
            } = &mut self.compiler.types[type_id.0] else {
                panic!("internal error: previously inserted struct can't be found");
            };

            *methods = fun_ids.clone();

            for fun_id in &fun_ids {
                self.typecheck_fun(*fun_id);
            }

            self.exit_scope();
        }

        type_id
    }

    pub fn typecheck_block(&mut self, node_id: NodeId, block_id: BlockId) {
        let mut funs = vec![];

        self.enter_scope();

        // FIXME: sad we have to clone here
        let block = self.compiler.blocks[block_id.0].clone();

        for node_id in &block.nodes {
            match &self.compiler.ast_nodes[node_id.0] {
                AstNode::Fun {
                    name,
                    params,
                    return_ty,
                    block,
                } => {
                    funs.push(self.typecheck_fun_predecl(*name, *params, *return_ty, *block));
                }

                AstNode::Struct {
                    name,
                    fields,
                    methods,
                    is_allocator,
                } => {
                    self.typecheck_struct(*name, fields.clone(), methods.clone(), *is_allocator);
                }

                AstNode::Enum {
                    name,
                    cases,
                    methods,
                } => {
                    self.typecheck_enum(*name, cases.clone(), methods.clone());
                }
                _ => {}
            }
        }

        for fun in funs {
            self.typecheck_fun(fun);
        }

        for node_id in &block.nodes {
            self.typecheck_node(*node_id);
        }
        self.compiler.node_types[node_id.0] = VOID_TYPE_ID;

        self.exit_scope()
    }

    pub fn is_type_compatible(&self, lhs: TypeId, rhs: TypeId) -> bool {
        lhs == rhs
    }

    pub fn typecheck_call_with_fun_id(
        &mut self,
        name: NodeId,
        fun_id: FunId,
        args: &[NodeId],
    ) -> TypeId {
        let Function {
            params,
            return_type,
            ..
        } = &self.compiler.functions[fun_id.0];

        let params = params.clone();
        let return_type = *return_type;

        if fun_id.0 == 0 {
            // Just for now, special-case println
            self.compiler
                .call_resolution
                .insert(name, CallTarget::Function(fun_id));
            for arg in args {
                // TODO: add name-checking
                let arg = *arg;

                self.typecheck_node(arg);
            }

            return VOID_TYPE_ID;
        }

        if args.len() != params.len() {
            self.error(
                &format!("expected {} args, found {}", params.len(), args.len()),
                name,
            );
            return return_type;
        }

        // TODO: do we want to wait until all params are checked
        // before we mark this as resolved?
        self.compiler
            .call_resolution
            .insert(name, CallTarget::Function(fun_id));

        for (arg, param) in args.iter().zip(params) {
            // TODO: add name-checking
            let arg = *arg;

            match &self.compiler.ast_nodes[arg.0] {
                AstNode::NamedValue { name, value } => {
                    let name = *name;
                    let value = *value;

                    let arg_ty = self.typecheck_node(value);

                    if !self.is_type_compatible(arg_ty, self.compiler.variables[param.var_id.0].ty)
                    {
                        // FIXME: make this a better error
                        self.error("types incompatible with function", value);
                    }

                    let arg_name = self.compiler.get_source(name);

                    if arg_name != param.name {
                        self.error(
                            &format!("expected name '{}'", String::from_utf8_lossy(&param.name)),
                            name,
                        )
                    }
                }
                _ => {
                    let arg_type = self.typecheck_node(arg);
                    let variable = &self.compiler.variables[param.var_id.0];

                    if !self.is_type_compatible(arg_type, variable.ty) {
                        // FIXME: make this a better type error
                        self.error("type mismatch for arg", arg);
                        return return_type;
                    }
                }
            }
        }

        return_type
    }

    pub fn typecheck_call(&mut self, head: NodeId, args: &[NodeId]) -> TypeId {
        if let Some(fun_id) = self.find_function_in_scope(head) {
            self.typecheck_call_with_fun_id(head, *fun_id, args)
        } else {
            self.error("unknown function", head);
            UNKNOWN_TYPE_ID
        }
    }

    pub fn typecheck_node(&mut self, node_id: NodeId) -> TypeId {
        let node_type = match &self.compiler.ast_nodes[node_id.0] {
            AstNode::Block(block_id) => {
                self.typecheck_block(node_id, *block_id);
                VOID_TYPE_ID
            }
            AstNode::Int => I64_TYPE_ID,
            AstNode::Float => F64_TYPE_ID,
            AstNode::True | AstNode::False => BOOL_TYPE_ID,
            AstNode::String => STRING_TYPE_ID,
            AstNode::Let {
                variable_name,
                initializer,
                is_mutable,
                ty,
            } => {
                let variable_name = *variable_name;
                let initializer = *initializer;
                let is_mutable = *is_mutable;
                let ty = *ty;

                let initializer_ty = self.typecheck_node(initializer);

                if let Some(ty) = ty {
                    let ty = self.typecheck_typename(ty);
                    if !self.is_type_compatible(ty, initializer_ty) {
                        self.error("initializer and given type do not match", initializer);
                    }
                }

                let var_id =
                    self.define_variable(variable_name, initializer_ty, is_mutable, node_id);

                self.compiler.var_resolution.insert(variable_name, var_id);

                VOID_TYPE_ID
            }
            AstNode::Variable => {
                let var_id = self.find_variable_in_scope(node_id);

                if let Some(var_id) = var_id {
                    let var_id = *var_id;
                    self.compiler.var_resolution.insert(node_id, var_id);

                    let variable = &self.compiler.variables[var_id.0];
                    variable.ty
                } else {
                    let name = self.compiler.get_source(node_id);

                    // Reserved name for synthetic self variables
                    if name == b"." {
                        self.error("can't find 'self' variable", node_id);
                    } else {
                        self.error("can't find variable", node_id);
                    }
                    UNKNOWN_TYPE_ID
                }
            }
            AstNode::MemberAccess { target, field } => {
                let target = *target;
                let field = *field;

                let type_id = self.typecheck_node(target);

                let type_id = match &self.compiler.types[type_id.0] {
                    Type::Pointer(_, type_id) => *type_id,
                    _ => type_id,
                };

                match &self.compiler.types[type_id.0] {
                    Type::Struct { fields, .. } => {
                        let field_name = self.compiler.get_source(field);
                        for known_field in fields {
                            if known_field.0 == field_name {
                                self.compiler.node_types[node_id.0] = known_field.1;
                                return known_field.1;
                            }
                        }
                        self.error("unknown field", field);
                        UNKNOWN_TYPE_ID
                    }
                    _ => {
                        self.error("field access on non-struct type", target);
                        UNKNOWN_TYPE_ID
                    }
                }
            }
            AstNode::MethodCall { target, call } => {
                let target = *target;
                let call = *call;

                let AstNode::Call { head, args } = &self.compiler.ast_nodes[call.0] else {
                    panic!("Internal error: method call using a non-call")
                };

                let head = *head;
                // FIXME: fix clone
                let args = args.clone();

                let name = self.compiler.get_source(head).to_vec();
                let type_id = self.typecheck_node(target);

                let type_id = match &self.compiler.types[type_id.0] {
                    Type::Pointer(_, type_id) => *type_id,
                    _ => type_id,
                };

                match &self.compiler.types[type_id.0] {
                    Type::Struct { methods, .. } => {
                        for method in methods {
                            let method_name = self
                                .compiler
                                .get_source(self.compiler.functions[method.0].name);

                            if method_name == name {
                                let type_id = self.typecheck_call_with_fun_id(head, *method, &args);
                                self.compiler.node_types[node_id.0] = type_id;
                                return type_id;
                            }
                        }
                        self.error("can't find method in struct", head);
                    }
                    _ => self.error("expected struct type for method call", target),
                }

                VOID_TYPE_ID
            }
            AstNode::BinaryOp { lhs, op, rhs } => {
                let lhs = *lhs;
                let rhs = *rhs;
                let op = *op;
                let lhs_ty = self.typecheck_node(lhs);
                let rhs_ty = self.typecheck_node(rhs);

                match &self.compiler.ast_nodes[op.0] {
                    AstNode::Plus | AstNode::Minus | AstNode::Multiply | AstNode::Divide => {
                        if lhs_ty != rhs_ty {
                            // FIXME: actually say the types
                            self.error("type mismatch during operation", op)
                        }
                        lhs_ty
                    }
                    AstNode::LessThan
                    | AstNode::LessThanOrEqual
                    | AstNode::Equal
                    | AstNode::NotEqual
                    | AstNode::GreaterThan
                    | AstNode::GreaterThanOrEqual => {
                        if lhs_ty != rhs_ty {
                            // FIXME: actually say the types
                            self.error("type mismatch during operation", op)
                        }
                        BOOL_TYPE_ID
                    }
                    AstNode::Assignment
                    | AstNode::AddAssignment
                    | AstNode::SubtractAssignment
                    | AstNode::MultiplyAssignment
                    | AstNode::DivideAssignment => {
                        let lhs_ty = self.typecheck_lvalue(lhs);

                        if lhs_ty != rhs_ty {
                            // FIXME: actually say the types
                            self.error("type mismatch during operation", op)
                        }
                        VOID_TYPE_ID
                    }
                    x => panic!("unsupported operator: {:?}", x),
                }
            }
            AstNode::Range { lhs, rhs } => {
                let lhs = *lhs;
                let rhs = *rhs;

                let lhs_type = self.typecheck_node(lhs);
                let rhs_type = self.typecheck_node(rhs);

                if lhs_type != I64_TYPE_ID {
                    self.error("expected i64 in range", lhs);
                }

                if rhs_type != I64_TYPE_ID {
                    self.error("expected i64 in range", rhs);
                }

                RANGE_I64_TYPE_ID
            }
            AstNode::NamespacedLookup { namespace, item } => {
                let namespace = *namespace;
                let item = *item;

                let type_id = self.find_type_in_scope(namespace);

                let Some(type_id) = type_id else {
                    self.error("could not find namespace", namespace);
                    return VOID_TYPE_ID;
                };

                let type_id = *type_id;

                match &self.compiler.types[type_id.0] {
                    Type::Struct { methods, .. } => {
                        let AstNode::Call { head, args } = &self.compiler.ast_nodes[item.0] else {
                            self.error("expected static method call on struct", item);
                            return VOID_TYPE_ID;
                        };

                        let head = *head;
                        let args = args.clone();

                        let call_name = self.compiler.get_source(head);

                        for method in methods {
                            let method_name = self
                                .compiler
                                .get_source(self.compiler.functions[method.0].name);
                            if method_name == call_name {
                                return self.typecheck_call_with_fun_id(head, *method, &args);
                            }
                        }
                    }
                    Type::Enum { cases, .. } => {
                        // FIXME: remove clone
                        let cases = cases.clone();

                        let output_type = self
                            .find_or_create_type(Type::Pointer(AllocationType::Normal, type_id));

                        match &self.compiler.ast_nodes[item.0] {
                            AstNode::Call { head, args } => {
                                // FIXME: remove clone
                                let head = *head;
                                let args = args.clone();
                                let case_name = self.compiler.get_source(head);

                                for (case_offset, case) in cases.iter().enumerate() {
                                    match case {
                                        EnumCase::Single { name, param } => {
                                            if name == case_name {
                                                let param = *param;
                                                if args.len() == 1 {
                                                    let arg_type_id = self.typecheck_node(args[0]);

                                                    if !self.is_type_compatible(param, arg_type_id)
                                                    {
                                                        self.error(
                                                            "incompatible types for enum case",
                                                            args[0],
                                                        );
                                                        return VOID_TYPE_ID;
                                                    }

                                                    self.compiler.call_resolution.insert(
                                                        head,
                                                        CallTarget::EnumConstructor(
                                                            type_id,
                                                            CaseOffset(case_offset),
                                                        ),
                                                    );

                                                    return output_type;
                                                } else {
                                                    self.error(format!("enum case has {} values, but should have 1", args.len()), item);
                                                    return VOID_TYPE_ID;
                                                }
                                            }
                                        }
                                        EnumCase::Struct { name, params } => {
                                            if name == case_name {
                                                if args.len() == params.len() {
                                                    for (arg, (param_name, param_type_id)) in
                                                        args.into_iter().zip(params)
                                                    {
                                                        if let AstNode::NamedValue { name, value } =
                                                            &self.compiler.ast_nodes[arg.0]
                                                        {
                                                            let name = *name;
                                                            let value = *value;

                                                            let name_contents =
                                                                self.compiler.get_source(name);

                                                            if name_contents != param_name {
                                                                self.error(
                                                                    "name mismatch in enum case",
                                                                    name,
                                                                );
                                                                return VOID_TYPE_ID;
                                                            }

                                                            let arg_type_id =
                                                                self.typecheck_node(value);

                                                            if !self.is_type_compatible(
                                                                *param_type_id,
                                                                arg_type_id,
                                                            ) {
                                                                self.error(
                                                                    "incompatible types for enum case",
                                                                    arg,
                                                                );
                                                                return VOID_TYPE_ID;
                                                            }
                                                        }
                                                    }
                                                    self.compiler.call_resolution.insert(
                                                        head,
                                                        CallTarget::EnumConstructor(
                                                            type_id,
                                                            CaseOffset(case_offset),
                                                        ),
                                                    );

                                                    return output_type;
                                                } else {
                                                    self.error(format!("enum case has {} values, but should have 1", args.len()), item);
                                                    return VOID_TYPE_ID;
                                                }
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            AstNode::Name | AstNode::Variable => {
                                let case_name = self.compiler.get_source(item);

                                for (case_offset, case) in cases.iter().enumerate() {
                                    match case {
                                        EnumCase::Simple { name } => {
                                            if name == case_name {
                                                self.compiler.call_resolution.insert(
                                                    item,
                                                    CallTarget::EnumConstructor(
                                                        type_id,
                                                        CaseOffset(case_offset),
                                                    ),
                                                );

                                                return output_type;
                                            }
                                        }
                                        _ => {}
                                    }
                                }

                                self.error("can't find matche enum case", item);
                            }
                            x => {
                                self.error(
                                    format!("expected enum case when created enum value: {:?}", x),
                                    item,
                                );
                            }
                        }
                    }
                    _ => {
                        self.error("expected struct or enum", namespace);
                    }
                }

                VOID_TYPE_ID
            }
            AstNode::Call { head, args } => {
                let head = *head;
                let args = args.clone();
                self.typecheck_call(head, &args)
            }
            AstNode::New(allocation_type, allocation_node_id) => {
                let allocation_type = *allocation_type;
                let allocation_node_id = *allocation_node_id;
                self.typecheck_allocation(allocation_type, allocation_node_id)
            }
            AstNode::NamedValue { value, .. } => self.typecheck_node(*value),
            AstNode::Return(return_expr) => {
                let return_expr = *return_expr;
                let expected_type = self.find_expected_return_type();

                if let Some(return_expr) = return_expr {
                    let expr_type = self.typecheck_node(return_expr);

                    if let Some(expected_type) = expected_type {
                        if !self.is_type_compatible(expected_type, expr_type) {
                            // FIXME: actually print the types

                            self.error(
                                format!(
                                    "incompatible type at return, found: {:?} expected: {:?}",
                                    expr_type, expected_type
                                ),
                                return_expr,
                            );
                        }
                    } else {
                        self.error("return used outside of a function", return_expr);
                    }
                } else if expected_type.is_some() {
                    self.error("return needs value", node_id)
                } else {
                    self.error("return used outside of a function", node_id);
                }

                VOID_TYPE_ID
            }
            AstNode::If {
                condition,
                then_block,
                else_expression,
            } => {
                let condition = *condition;
                let then_block = *then_block;
                let else_expression = *else_expression;

                self.typecheck_node(condition);
                self.typecheck_node(then_block);

                if self.compiler.node_types[condition.0] != BOOL_TYPE_ID {
                    self.error("condition not a boolean expression", condition);
                }

                if let Some(else_expression) = else_expression {
                    self.typecheck_node(else_expression);

                    // FIXME: add type compatibility
                    if self.compiler.node_types[then_block.0]
                        != self.compiler.node_types[else_expression.0]
                    {
                        self.error("return used outside of a function", else_expression);
                    }
                }

                self.compiler.node_types[then_block.0]
            }
            AstNode::While { condition, block } => {
                let condition = *condition;
                let block = *block;

                self.typecheck_node(condition);
                self.typecheck_node(block);

                if self.compiler.node_types[condition.0] != BOOL_TYPE_ID {
                    self.error("condition not a boolean expression", condition);
                }

                self.compiler.node_types[block.0]
            }
            AstNode::For {
                variable,
                range,
                block,
            } => {
                let variable = *variable;
                let range = *range;
                let block = *block;

                let range_type = self.typecheck_node(range);

                if matches!(self.compiler.types[range_type.0], Type::Range(I64_TYPE_ID)) {
                    self.enter_scope();

                    let var_id = self.define_variable(variable, I64_TYPE_ID, true, variable);
                    self.compiler.var_resolution.insert(variable, var_id);

                    self.typecheck_node(block);

                    self.exit_scope();
                } else {
                    self.error("expected range in for loop", range);
                }

                VOID_TYPE_ID
            }
            AstNode::Fun { .. } | AstNode::Struct { .. } | AstNode::Enum { .. } => {
                // ignore here, since we checked this in an earlier pass
                VOID_TYPE_ID
            }
            AstNode::Statement(node_id) => {
                self.typecheck_node(*node_id);
                VOID_TYPE_ID
            }
            x => {
                panic!("unsupported node: {:?}", x)
            }
        };

        self.compiler.node_types[node_id.0] = node_type;

        node_type
    }

    pub fn typecheck_lvalue(&mut self, lvalue: NodeId) -> TypeId {
        match &self.compiler.ast_nodes[lvalue.0] {
            AstNode::Variable => {
                let var_id = self.compiler.var_resolution.get(&lvalue);

                if let Some(var_id) = var_id {
                    let var = &self.compiler.variables[var_id.0];
                    let ty = var.ty;
                    if !var.is_mutable {
                        if self.compiler.get_source(lvalue) == b"." {
                            self.error("'self' variable is not mutable", lvalue);
                        } else {
                            self.error("variable is not mutable", lvalue);
                        }
                    }
                    ty
                } else {
                    self.error(
                        "internal error: variable unresolved when checking lvalue",
                        lvalue,
                    );
                    VOID_TYPE_ID
                }
            }
            AstNode::MemberAccess { target, field } => {
                let target = *target;
                let field = *field;

                let head_type_id = self.typecheck_lvalue(target);

                let field_name = self.compiler.get_source(field);

                let target_type_id = match &self.compiler.types[head_type_id.0] {
                    Type::Pointer(_, inner_type_id) => *inner_type_id,
                    _ => head_type_id,
                };

                match &self.compiler.types[target_type_id.0] {
                    Type::Struct { fields, .. } => {
                        for f in fields {
                            if f.0 == field_name {
                                return f.1;
                            }
                        }
                        self.error("could not find field", field);
                        VOID_TYPE_ID
                    }
                    x => {
                        self.error(
                            format!("field access only supported on structs, not {:?}", x),
                            field,
                        );
                        VOID_TYPE_ID
                    }
                }
            }
            _ => {
                self.error("unsupported lvalue, needs variable or field", lvalue);
                VOID_TYPE_ID
            }
        }
    }

    pub fn typecheck_allocation(
        &mut self,
        allocation_type: AllocationType,
        node_id: NodeId,
    ) -> TypeId {
        if let AstNode::Call { head, args } = &self.compiler.ast_nodes[node_id.0] {
            // FIXME: remove clone
            let head = *head;
            let args = args.clone();

            let Some(type_id) = self.find_type_in_scope(head) else {
                self.error("unknown type in allocation", head);
                return UNKNOWN_TYPE_ID
            };

            let type_id = *type_id;
            let output_type = self.find_or_create_type(Type::Pointer(allocation_type, type_id));

            'arg: for arg in args {
                let AstNode::NamedValue { name, value } = &self.compiler.ast_nodes[arg.0] else {
                    self.error("unexpected argument in allocation", arg);
                    return UNKNOWN_TYPE_ID
                };

                let name = *name;

                self.typecheck_node(*value);

                match &self.compiler.types[type_id.0] {
                    Type::Struct { fields, .. } => {
                        let field_name = self.compiler.get_source(name);
                        for known_field in fields {
                            if known_field.0 == field_name {
                                self.compiler.node_types[node_id.0] = known_field.1;
                                continue 'arg;
                            }
                        }
                        self.error("unknown field", name);
                        return UNKNOWN_TYPE_ID;
                    }
                    Type::Pointer(_, type_id) => match &self.compiler.types[type_id.0] {
                        Type::Struct { fields, .. } => {
                            let field_name = self.compiler.get_source(name);
                            for known_field in fields {
                                if known_field.0 == field_name {
                                    self.compiler.node_types[node_id.0] = known_field.1;
                                    continue 'arg;
                                }
                            }
                            self.error("unknown field", name);
                            return UNKNOWN_TYPE_ID;
                        }
                        _ => {
                            self.error("internal error: allocation of non-struct type", node_id);
                            return UNKNOWN_TYPE_ID;
                        }
                    },
                    _ => {
                        self.error("internal error: allocation of non-struct type", node_id);
                        return UNKNOWN_TYPE_ID;
                    }
                }
            }

            output_type
        } else {
            self.error("expected an allocation call", node_id);
            UNKNOWN_TYPE_ID
        }
    }

    pub fn typecheck(mut self) -> Compiler {
        let num_nodes = self.compiler.ast_nodes.len();
        self.compiler.node_types.resize(num_nodes, UNKNOWN_TYPE_ID);

        let top_level = NodeId(self.compiler.ast_nodes.len() - 1);
        self.typecheck_node(top_level);

        let top_level_type = self.compiler.node_types[top_level.0];

        // If we haven't seen a main, create one from the top-level node
        if !self.compiler.has_main() {
            // Synthesis of a fake 'main' node
            self.compiler.source.extend_from_slice(b"main");
            self.compiler.ast_nodes.push(AstNode::Name);
            self.compiler
                .span_start
                .push(self.compiler.source.len() - 4);
            self.compiler.span_end.push(self.compiler.source.len());
            let main_node = NodeId(self.compiler.ast_nodes.len() - 1);

            self.compiler.functions.push(Function {
                name: main_node,
                params: vec![],
                return_type: top_level_type,
                body: top_level,
            })
        }

        self.compiler
    }

    pub fn add_variable_to_scope(&mut self, variable_name: Vec<u8>, var_id: VarId) {
        self.scope
            .last_mut()
            .expect("internal error: missing typechecking scope")
            .variables
            .insert(variable_name, var_id);
    }

    pub fn add_type_to_scope(&mut self, type_name: Vec<u8>, type_id: TypeId) {
        self.scope
            .last_mut()
            .expect("internal error: missing typechecking scope")
            .types
            .insert(type_name, type_id);
    }

    pub fn define_variable(
        &mut self,
        name: NodeId,
        ty: TypeId,
        is_mutable: bool,
        where_defined: NodeId,
    ) -> VarId {
        let variable_name = self.compiler.get_source(name).to_vec();
        self.compiler.variables.push(Variable {
            name,
            ty,
            is_mutable,
            where_defined,
        });

        let var_id = VarId(self.compiler.variables.len() - 1);

        self.add_variable_to_scope(variable_name, var_id);

        var_id
    }

    pub fn find_variable_in_scope(&self, variable_name: NodeId) -> Option<&VarId> {
        let name = &self.compiler.source
            [self.compiler.span_start[variable_name.0]..self.compiler.span_end[variable_name.0]];

        // Expand the shorthand, and look for 'self' instead
        let name = if name == b"." { b"self" } else { name };

        for scope in self.scope.iter().rev() {
            if let Some(value) = scope.variables.get(name) {
                return Some(value);
            }
        }

        None
    }

    pub fn find_function_in_scope(&self, function_name: NodeId) -> Option<&FunId> {
        let name = &self.compiler.source
            [self.compiler.span_start[function_name.0]..self.compiler.span_end[function_name.0]];
        for scope in self.scope.iter().rev() {
            if let Some(value) = scope.functions.get(name) {
                return Some(value);
            }
        }

        None
    }

    pub fn find_type_in_scope(&self, type_name: NodeId) -> Option<&TypeId> {
        let name = &self.compiler.source
            [self.compiler.span_start[type_name.0]..self.compiler.span_end[type_name.0]];
        for scope in self.scope.iter().rev() {
            if let Some(value) = scope.types.get(name) {
                return Some(value);
            }
        }

        None
    }

    pub fn find_or_create_type(&mut self, ty: Type) -> TypeId {
        for (idx, t) in self.compiler.types.iter().enumerate() {
            if &ty == t {
                return TypeId(idx);
            }
        }

        self.compiler.types.push(ty);

        TypeId(self.compiler.types.len() - 1)
    }

    pub fn find_expected_return_type(&self) -> Option<TypeId> {
        for scope in self.scope.iter().rev() {
            if let Some(ret_type) = scope.expected_return_type {
                return Some(ret_type);
            }
        }

        None
    }

    pub fn set_expected_return_type(&mut self, expected_type: TypeId) {
        let frame = self
            .scope
            .last_mut()
            .expect("internal error: missing expected scope frame");
        frame.expected_return_type = Some(expected_type)
    }

    pub fn enter_scope(&mut self) {
        self.scope.push(Scope::new())
    }

    pub fn exit_scope(&mut self) {
        self.scope.pop();
    }

    pub fn error(&mut self, message: impl Into<String>, node_id: NodeId) {
        self.compiler.errors.push(SourceError {
            message: message.into(),

            node_id,
        });
    }
}
