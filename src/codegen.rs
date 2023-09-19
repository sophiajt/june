use crate::{
    compiler::{CallTarget, Compiler},
    lifetime_checker::AllocationLifetime,
    parser::{AstNode, NodeId},
    typechecker::{
        EnumVariant, FunId, Function, Param, Type, TypeId, TypedField, BOOL_TYPE_ID, F64_TYPE_ID,
        I64_TYPE_ID, STRING_TYPE_ID, UNKNOWN_TYPE_ID, VOID_TYPE_ID,
    },
};

pub struct Codegen {
    compiler: Compiler,
}

impl Codegen {
    pub fn new(compiler: Compiler) -> Self {
        Codegen { compiler }
    }

    pub fn codegen_typename(&self, type_id: TypeId, output: &mut Vec<u8>) {
        match self.compiler.get_type(type_id) {
            Type::Struct { .. } => {
                output.extend_from_slice(b"struct struct_");
                output.extend_from_slice(type_id.0.to_string().as_bytes());
            }
            Type::Enum { .. } => {
                output.extend_from_slice(b"struct enum_");
                output.extend_from_slice(type_id.0.to_string().as_bytes());
            }
            Type::Pointer {
                target: type_id, ..
            } => {
                self.codegen_typename(*type_id, output);
                output.push(b'*');
            }
            _ => {
                if type_id == VOID_TYPE_ID {
                    output.extend_from_slice(b"void");
                } else if type_id == I64_TYPE_ID {
                    output.extend_from_slice(b"int64_t");
                } else if type_id == F64_TYPE_ID {
                    output.extend_from_slice(b"double");
                } else if type_id == STRING_TYPE_ID {
                    output.extend_from_slice(b"char*");
                } else if type_id == BOOL_TYPE_ID {
                    output.extend_from_slice(b"bool");
                } else if type_id == UNKNOWN_TYPE_ID {
                    panic!("(unknown) type should be resolved before codegen");
                } else {
                    panic!("unknown type")
                }
            }
        }
    }

    pub fn codegen_allocator_function(
        &self,
        type_id: TypeId,
        fields: &[TypedField],
        is_allocator: bool,
        output: &mut Vec<u8>,
    ) {
        let Type::Pointer {
            target: inner_type_id,
            ..
        } = self.compiler.get_type(type_id)
        else {
            panic!("internal error: pointer to unknown type");
        };

        output.extend_from_slice(b"struct struct_");
        output.extend_from_slice(inner_type_id.0.to_string().as_bytes());
        output.extend_from_slice(b"* allocator_");
        output.extend_from_slice(inner_type_id.0.to_string().as_bytes());
        output.push(b'(');
        output.extend_from_slice(b"long allocation_id");

        for TypedField { name, ty, .. } in fields {
            output.extend_from_slice(b", ");
            self.codegen_typename(*ty, output);
            output.push(b' ');
            output.extend_from_slice(name);
        }
        output.extend_from_slice(b") {\n");

        self.codegen_typename(type_id, output);
        output.extend_from_slice(b" tmp = (");
        self.codegen_typename(type_id, output);
        output.extend_from_slice(b")allocate(allocator, sizeof(struct struct_");
        output.extend_from_slice(inner_type_id.0.to_string().as_bytes());
        output.extend_from_slice(b"), allocation_id);\n");

        if is_allocator {
            output.extend_from_slice(b"tmp->__allocation_id__ = allocation_id;\n");
        }

        for TypedField { name, .. } in fields {
            output.extend_from_slice(b"tmp->");
            output.extend_from_slice(name);
            output.extend_from_slice(b" = ");
            output.extend_from_slice(name);
            output.extend_from_slice(b";\n");
        }

        output.extend_from_slice(b"return tmp;\n}\n");
    }

    pub fn codegen_structs_and_enums(&self, output: &mut Vec<u8>) {
        for (idx, ty) in self.compiler.get_types().iter().enumerate() {
            match ty {
                Type::Struct {
                    fields,
                    is_allocator,
                    generic_params,
                    ..
                } => {
                    if !generic_params.is_empty() {
                        // Don't codegen generic functions. Instead, only codegen their instantiations
                        continue;
                    }
                    output.extend_from_slice(b"struct struct_");
                    output.extend_from_slice(idx.to_string().as_bytes());
                    output.extend_from_slice(b"{\n");
                    if *is_allocator {
                        self.codegen_typename(I64_TYPE_ID, output);
                        output.push(b' ');
                        output.extend_from_slice(b"__allocation_id__;\n");
                    }
                    for TypedField { name, ty, .. } in fields {
                        self.codegen_typename(*ty, output);
                        output.push(b' ');
                        output.extend_from_slice(name);
                        output.extend_from_slice(b";\n");
                    }

                    output.extend_from_slice(b"};\n");

                    if let Some(ptr) = self.compiler.find_pointer_to(TypeId(idx)) {
                        self.codegen_allocator_function(ptr, fields, *is_allocator, output);
                    } else {
                        panic!("internal error: can't find pointer to type")
                    }
                }
                Type::Enum {
                    generic_params,
                    variants: cases,
                    ..
                } => {
                    if !generic_params.is_empty() {
                        // Don't codegen generic functions. Instead, only codegen their instantiations
                        continue;
                    }

                    output.extend_from_slice(b"struct enum_");
                    output.extend_from_slice(idx.to_string().as_bytes());
                    output.extend_from_slice(b"{\n");
                    output.extend_from_slice(b"int arm_id;\n");
                    output.extend_from_slice(b"union {\n");
                    for case in cases {
                        match case {
                            EnumVariant::Single { name, param: arg } => {
                                self.codegen_typename(*arg, output);
                                output.push(b' ');
                                output.extend_from_slice(name);
                                output.extend_from_slice(b";\n");
                            }
                            EnumVariant::Struct { name, params: args } => {
                                // FIXME!! This will name collide because of C naming resolution
                                output.extend_from_slice(b"struct /*");
                                output.extend_from_slice(name);
                                output.extend_from_slice(b"*/ {\n");

                                for arg in args {
                                    self.codegen_typename(arg.1, output);
                                    output.push(b' ');
                                    output.extend_from_slice(&arg.0);
                                    output.extend_from_slice(b";\n");
                                }

                                output.extend_from_slice(b"};\n");
                            }
                            EnumVariant::Simple { .. } => {
                                // ignore because it is encoded into the arm_id above
                            }
                        }
                    }
                    output.extend_from_slice(b"};\n");

                    output.extend_from_slice(b"};\n");

                    for (case_offset, case) in cases.iter().enumerate() {
                        self.codegen_typename(TypeId(idx), output);
                        output.extend_from_slice(b"* enum_case_");
                        output.extend_from_slice(idx.to_string().as_bytes());
                        output.push(b'_');
                        output.extend_from_slice(case_offset.to_string().as_bytes());

                        output.extend_from_slice(b"(int allocation_id");

                        match case {
                            EnumVariant::Single { param, .. } => {
                                output.extend_from_slice(b", ");
                                self.codegen_typename(*param, output);
                                output.extend_from_slice(b" arg");
                            }
                            EnumVariant::Struct { params, .. } => {
                                for (param_name, param_type) in params {
                                    output.extend_from_slice(b", ");
                                    self.codegen_typename(*param_type, output);
                                    output.push(b' ');
                                    output.extend_from_slice(param_name);
                                }
                            }
                            EnumVariant::Simple { .. } => {}
                        }
                        output.extend_from_slice(b") {\n");

                        self.codegen_typename(TypeId(idx), output);
                        output.extend_from_slice(b"* tmp = (");
                        self.codegen_typename(TypeId(idx), output);
                        output.extend_from_slice(b"*)allocate(allocator, sizeof(struct enum_");
                        output.extend_from_slice(idx.to_string().as_bytes());
                        output.extend_from_slice(b"), allocation_id);\n");

                        output.extend_from_slice(b"tmp->arm_id = ");
                        output.extend_from_slice(case_offset.to_string().as_bytes());
                        output.extend_from_slice(b";\n");

                        match case {
                            EnumVariant::Single { name, .. } => {
                                output.extend_from_slice(b"tmp->");
                                output.extend_from_slice(name);
                                output.extend_from_slice(b" = ");
                                output.extend_from_slice(b"arg");
                                output.extend_from_slice(b";\n");
                            }
                            EnumVariant::Struct { params, .. } => {
                                for (param_name, _) in params {
                                    output.extend_from_slice(b"tmp->");
                                    output.extend_from_slice(param_name);
                                    output.extend_from_slice(b" = ");
                                    output.extend_from_slice(param_name);
                                    output.extend_from_slice(b";\n");
                                }
                            }
                            EnumVariant::Simple { .. } => {}
                        }

                        output.extend_from_slice(b"return tmp;\n");

                        output.extend_from_slice(b"}\n");
                    }
                }
                _ => {}
            }
        }
    }

    pub fn codegen_fun_signature(
        &self,
        fun_id: FunId,
        params: &[Param],
        return_type: TypeId,
        output: &mut Vec<u8>,
    ) {
        self.codegen_typename(return_type, output);
        output.push(b' ');
        output.extend_from_slice(b"/* ");
        output.extend_from_slice(
            self.compiler
                .get_source(self.compiler.functions[fun_id.0].name),
        );
        output.extend_from_slice(b" */ ");

        output.extend_from_slice(b"function_");
        output.extend_from_slice(fun_id.0.to_string().as_bytes());
        output.push(b'(');

        output.extend_from_slice(b"long allocation_id");
        for param in params {
            output.extend_from_slice(b", ");

            let variable = &self.compiler.variables[param.var_id.0];
            self.codegen_typename(variable.ty, output);
            output.push(b' ');
            output.extend_from_slice(b"variable_");
            output.extend_from_slice(param.var_id.0.to_string().as_bytes());
        }

        output.push(b')');
    }

    pub fn codegen_fun_decls(&self, output: &mut Vec<u8>) {
        for (
            idx,
            Function {
                params,
                return_type,
                ..
            },
        ) in self.compiler.functions.iter().enumerate().skip(1)
        {
            self.codegen_fun_signature(FunId(idx), params, *return_type, output);

            output.extend_from_slice(b";\n");
        }
        output.push(b'\n');

        for (
            idx,
            Function {
                params,
                return_type,
                body,
                ..
            },
        ) in self.compiler.functions.iter().enumerate().skip(1)
        {
            self.codegen_fun_signature(FunId(idx), params, *return_type, output);

            output.extend_from_slice(b"{\n");
            self.codegen_block(*body, output);
            output.extend_from_slice(b"}\n");
        }
    }

    pub fn codegen_annotation(&self, node_id: NodeId, output: &mut Vec<u8>) {
        match self.compiler.get_node_lifetime(node_id) {
            AllocationLifetime::Return => output.extend_from_slice(b"allocation_id"),
            AllocationLifetime::Param { var_id } => output
                .extend_from_slice(format!("variable_{}->__allocation_id__", var_id.0).as_bytes()),
            AllocationLifetime::Scope { level } => {
                output.extend_from_slice(format!("allocation_id + {} ", level).as_bytes())
            }
            AllocationLifetime::Unknown => {
                // panic!("found 'unknown' lifetime during codegen")
                output.extend_from_slice(b"/* UNKNOWN, */ ")
            }
        }
    }

    pub fn codegen_node(&self, node_id: NodeId, output: &mut Vec<u8>) {
        match &self.compiler.get_node(node_id) {
            AstNode::String => {
                let src = self.compiler.get_source(node_id);

                output.extend_from_slice(src);
            }
            AstNode::Int => {
                let src = self.compiler.get_source(node_id);

                output.extend_from_slice(src);
                output.extend_from_slice(b"LL");
            }
            AstNode::Float => {
                let src = self.compiler.get_source(node_id);

                output.extend_from_slice(src);
            }
            AstNode::None => {
                output.extend_from_slice(b"NULL");
            }
            AstNode::Name => {
                let src = self.compiler.get_source(node_id);

                output.extend_from_slice(src);
            }
            AstNode::Variable => {
                let src = self.compiler.get_source(node_id);

                let var_id = self
                    .compiler
                    .var_resolution
                    .get(&node_id)
                    .unwrap_or_else(|| {
                        println!("{:?}", String::from_utf8_lossy(src));
                        panic!(
                            "internal error: unresolved variable in codegen: {}",
                            node_id.0
                        )
                    });

                output.extend_from_slice(b"/* ");
                output.extend_from_slice(
                    self.compiler
                        .get_source(self.compiler.variables[var_id.0].name),
                );
                output.extend_from_slice(b" */ ");

                output.extend_from_slice(b"variable_");
                output.extend_from_slice(var_id.0.to_string().as_bytes());
            }
            AstNode::Let {
                variable_name,
                initializer,
                ..
            } => {
                let var_id = self
                    .compiler
                    .var_resolution
                    .get(variable_name)
                    .expect("internal error: unresolved variable in codegen");

                let ty = self.compiler.variables[var_id.0].ty;

                self.codegen_typename(ty, output);

                output.extend_from_slice(b" /* ");
                output.extend_from_slice(
                    self.compiler
                        .get_source(self.compiler.variables[var_id.0].name),
                );
                output.extend_from_slice(b" */ ");

                output.extend_from_slice(b" variable_");
                output.extend_from_slice(var_id.0.to_string().as_bytes());

                output.extend_from_slice(b" = ");
                self.codegen_node(*initializer, output);
            }
            AstNode::Plus => {
                output.push(b'+');
            }
            AstNode::Minus => {
                output.push(b'-');
            }
            AstNode::Multiply => {
                output.push(b'*');
            }
            AstNode::Divide => {
                output.push(b'/');
            }
            AstNode::Assignment => {
                output.push(b'=');
            }
            AstNode::LessThan => {
                output.push(b'<');
            }
            AstNode::LessThanOrEqual => {
                output.extend_from_slice(b"<=");
            }
            AstNode::Equal => {
                output.extend_from_slice(b"==");
            }
            AstNode::NotEqual => {
                output.extend_from_slice(b"!=");
            }
            AstNode::GreaterThan => {
                output.push(b'>');
            }
            AstNode::GreaterThanOrEqual => {
                output.extend_from_slice(b">=");
            }
            AstNode::AddAssignment => {
                output.extend_from_slice(b"+=");
            }
            AstNode::SubtractAssignment => {
                output.extend_from_slice(b"-=");
            }
            AstNode::MultiplyAssignment => {
                output.extend_from_slice(b"*=");
            }
            AstNode::DivideAssignment => {
                output.extend_from_slice(b"/=");
            }
            AstNode::BinaryOp { lhs, op, rhs } => {
                output.push(b'(');
                self.codegen_node(*lhs, output);
                output.push(b')');

                self.codegen_node(*op, output);

                output.push(b'(');
                self.codegen_node(*rhs, output);
                output.push(b')');
            }
            AstNode::Call { head, args } => {
                let call_target = self.compiler.call_resolution.get(head).expect(&format!(
                    "internal error: missing call resolution in codegen: {:?}",
                    head
                ));

                match call_target {
                    CallTarget::Function(fun_id) => {
                        if fun_id.0 == 0 {
                            // special case for println
                            match self.compiler.get_node_type(args[0]) {
                                STRING_TYPE_ID => {
                                    output.extend_from_slice(b"printf(\"%s\\n\", ");
                                    self.codegen_node(args[0], output);
                                }
                                I64_TYPE_ID => {
                                    output.extend_from_slice(b"printf(\"%lli\\n\", ");
                                    self.codegen_node(args[0], output);
                                }
                                F64_TYPE_ID => {
                                    output.extend_from_slice(b"printf(\"%lf\\n\", ");
                                    self.codegen_node(args[0], output);
                                }
                                BOOL_TYPE_ID => {
                                    output.extend_from_slice(b"printf(\"%s\\n\", (");
                                    self.codegen_node(args[0], output);
                                    output.extend_from_slice(br#")?"true":"false""#);
                                }
                                x => {
                                    panic!(
                                        "unknown type for printf: {:?}",
                                        self.compiler.get_type(x)
                                    );
                                }
                            }
                            output.extend_from_slice(b");\n");
                            return;
                        }

                        output.extend_from_slice(b"/* ");
                        output.extend_from_slice(
                            self.compiler
                                .get_source(self.compiler.functions[fun_id.0].name),
                        );
                        output.extend_from_slice(b" */ ");

                        output.extend_from_slice(b"function_");
                        output.extend_from_slice(fun_id.0.to_string().as_bytes());
                        output.push(b'(');

                        self.codegen_annotation(node_id, output);

                        for arg in args {
                            output.extend_from_slice(b", ");

                            self.codegen_node(*arg, output)
                        }
                        output.push(b')');
                    }
                    CallTarget::EnumConstructor(target, offset) => {
                        output.extend_from_slice(b"enum_case_");
                        output.extend_from_slice(target.0.to_string().as_bytes());
                        output.push(b'_');
                        output.extend_from_slice(offset.0.to_string().as_bytes());
                        output.push(b'(');

                        self.codegen_annotation(node_id, output);

                        for arg in args {
                            output.extend_from_slice(b", ");

                            self.codegen_node(*arg, output)
                        }
                        output.push(b')');
                    }
                }
            }
            AstNode::New(_, allocation_call) => {
                let type_id = self.compiler.get_node_type(node_id);

                let Type::Pointer {
                    target: type_id, ..
                } = self.compiler.get_type(type_id)
                else {
                    panic!(
                        "internal error: 'new' creating non-pointer type: {:?}",
                        self.compiler.get_type(type_id)
                    )
                };
                let type_id = *type_id;

                output.extend_from_slice(b"allocator_");
                output.extend_from_slice(type_id.0.to_string().as_bytes());
                output.push(b'(');

                self.codegen_annotation(node_id, output);

                if let AstNode::Call { args, .. } = self.compiler.get_node(*allocation_call) {
                    for arg in args {
                        output.extend_from_slice(b", ");

                        self.codegen_node(*arg, output)
                    }
                    output.push(b')');
                } else {
                    panic!("internal error: expected allocation call during allocation")
                }
            }
            AstNode::NamespacedLookup { item, .. } => match self.compiler.get_node(*item) {
                AstNode::Call { head, args } => {
                    let call_target = self
                        .compiler
                        .call_resolution
                        .get(head)
                        .expect("internal error: missing call resolution in codegen");

                    match call_target {
                        CallTarget::Function(fun_id) => {
                            output.extend_from_slice(b"/* ");
                            output.extend_from_slice(
                                self.compiler
                                    .get_source(self.compiler.functions[fun_id.0].name),
                            );
                            output.extend_from_slice(b" */ ");

                            output.extend_from_slice(b"function_");
                            output.extend_from_slice(fun_id.0.to_string().as_bytes());
                            output.push(b'(');

                            self.codegen_annotation(node_id, output);

                            for arg in args {
                                output.extend_from_slice(b", ");

                                self.codegen_node(*arg, output)
                            }
                            output.push(b')');
                        }
                        CallTarget::EnumConstructor(target, offset) => {
                            output.extend_from_slice(b"enum_case_");
                            output.extend_from_slice(target.0.to_string().as_bytes());
                            output.push(b'_');
                            output.extend_from_slice(offset.0.to_string().as_bytes());
                            output.push(b'(');

                            self.codegen_annotation(node_id, output);

                            for arg in args {
                                output.extend_from_slice(b", ");

                                self.codegen_node(*arg, output)
                            }
                            output.push(b')');
                        }
                    }
                }
                AstNode::Variable => {
                    let call_target = self
                        .compiler
                        .call_resolution
                        .get(item)
                        .expect("internal error: missing call resolution in codegen");

                    match call_target {
                        CallTarget::Function(fun_id) => {
                            output.extend_from_slice(b"/* ");
                            output.extend_from_slice(
                                self.compiler
                                    .get_source(self.compiler.functions[fun_id.0].name),
                            );
                            output.extend_from_slice(b" */ ");

                            output.extend_from_slice(b"function_");
                            output.extend_from_slice(fun_id.0.to_string().as_bytes());
                            output.push(b'(');

                            self.codegen_annotation(node_id, output);

                            output.push(b')');
                        }
                        CallTarget::EnumConstructor(target, offset) => {
                            output.extend_from_slice(b"enum_case_");
                            output.extend_from_slice(target.0.to_string().as_bytes());
                            output.push(b'_');
                            output.extend_from_slice(offset.0.to_string().as_bytes());
                            output.push(b'(');

                            self.codegen_annotation(node_id, output);

                            output.push(b')');
                        }
                    }
                }
                _ => {
                    panic!("unsupported namespace lookup")
                }
            },
            AstNode::NamedValue { value, .. } => {
                // FIXME: this should probably be handled cleanly via typecheck+codegen
                // rather than ignoring the name
                self.codegen_node(*value, output)
            }
            AstNode::MemberAccess { target, field } => {
                self.codegen_node(*target, output);
                output.extend_from_slice(b"->");
                self.codegen_node(*field, output);
            }
            AstNode::MethodCall { call, .. } => {
                self.codegen_node(*call, output);
            }
            AstNode::Statement(node_id) => {
                self.codegen_node(*node_id, output);
                output.extend_from_slice(b";\n");
            }
            AstNode::If {
                condition,
                then_block,
                else_expression,
            } => {
                output.extend_from_slice(b"if (");
                self.codegen_node(*condition, output);
                output.extend_from_slice(b") {");
                self.codegen_node(*then_block, output);

                if let Some(else_expression) = else_expression {
                    output.extend_from_slice(b"} else {");
                    self.codegen_node(*else_expression, output);
                }
                output.extend_from_slice(b"}");
            }
            AstNode::While { condition, block } => {
                output.extend_from_slice(b"while (");
                self.codegen_node(*condition, output);
                output.extend_from_slice(b") {");
                self.codegen_node(*block, output);
                output.extend_from_slice(b"}");
            }
            AstNode::For {
                variable,
                range,
                block,
            } => {
                output.extend_from_slice(b"for (");

                let AstNode::Range { lhs, rhs } = self.compiler.get_node(*range) else {
                    panic!("internal error: range not found for 'for'");
                };

                let var_id = self
                    .compiler
                    .var_resolution
                    .get(variable)
                    .expect("internal error: unresolved variable in codegen");

                let ty = self.compiler.variables[var_id.0].ty;

                self.codegen_typename(ty, output);

                output.extend_from_slice(b" variable_");
                output.extend_from_slice(var_id.0.to_string().as_bytes());

                output.extend_from_slice(b" = ");
                self.codegen_node(*lhs, output);

                output.extend_from_slice(b"; variable_");
                output.extend_from_slice(var_id.0.to_string().as_bytes());
                output.extend_from_slice(b" <= ");
                self.codegen_node(*rhs, output);

                output.extend_from_slice(b"; ++variable_");
                output.extend_from_slice(var_id.0.to_string().as_bytes());
                output.extend_from_slice(b") {");
                self.codegen_node(*block, output);
                output.extend_from_slice(b"}");
            }
            AstNode::Match { target, match_arms } => {
                output.extend_from_slice(b"{\n");
                let type_id = self.compiler.get_node_type(*target);
                self.codegen_typename(type_id, output);
                output.push(b' ');
                let match_var = format!("match_var_{}", target.0);

                output.extend_from_slice(match_var.as_bytes());
                output.extend_from_slice(b" = ");
                self.codegen_node(*target, output);

                output.extend_from_slice(b";\n");

                let mut first = true;

                for (match_arm, match_result) in match_arms {
                    if !first {
                        output.extend_from_slice(b"else ");
                    } else {
                        first = false
                    }
                    match self.compiler.get_node(*match_arm) {
                        AstNode::Variable => {
                            output.extend_from_slice(b"if (true) {\n");

                            let var_id = self
                                .compiler
                                .var_resolution
                                .get(match_arm)
                                .expect("internal error: unresolved variable in codegen");
                            let var_type = self.compiler.variables[var_id.0].ty;
                            self.codegen_typename(var_type, output);
                            output.extend_from_slice(b" variable_");
                            output.extend_from_slice(var_id.0.to_string().as_bytes());

                            output.extend_from_slice(b" = ");
                            output.extend_from_slice(match_var.as_bytes());
                            output.extend_from_slice(b";\n");

                            self.codegen_node(*match_result, output);

                            output.extend_from_slice(b"}\n");
                        }
                        AstNode::NamespacedLookup { item, .. } => {
                            match self.compiler.get_node(*item) {
                                AstNode::Name | AstNode::Variable => {
                                    let Some(resolution) =
                                        self.compiler.call_resolution.get(match_arm)
                                    else {
                                        panic!(
                                            "internal error: match arm unresolved at codegen time"
                                        )
                                    };

                                    match resolution {
                                        CallTarget::EnumConstructor(_, case_offset) => {
                                            output.extend_from_slice(b"if (");
                                            output.extend_from_slice(match_var.as_bytes());
                                            output.extend_from_slice(b"->arm_id == ");
                                            output.extend_from_slice(
                                                case_offset.0.to_string().as_bytes(),
                                            );
                                            output.extend_from_slice(b") ");
                                            self.codegen_node(*match_result, output);
                                        }
                                        x => {
                                            panic!("target not supported in enum codegen: {:?}", x);
                                        }
                                    }
                                }
                                AstNode::Call { args, .. } => {
                                    let Some(resolution) =
                                        self.compiler.call_resolution.get(match_arm)
                                    else {
                                        panic!(
                                            "internal error: match arm unresolved at codegen time"
                                        )
                                    };

                                    match resolution {
                                        CallTarget::EnumConstructor(enum_type_id, case_offset) => {
                                            output.extend_from_slice(b"if (");
                                            output.extend_from_slice(match_var.as_bytes());
                                            output.extend_from_slice(b"->arm_id == ");
                                            output.extend_from_slice(
                                                case_offset.0.to_string().as_bytes(),
                                            );

                                            let mut variable_assignments: Vec<u8> = vec![];

                                            for (arg_idx, arg) in args.iter().enumerate() {
                                                match self.compiler.get_node(*arg) {
                                                    AstNode::Variable => {
                                                        let var_id = self.compiler.var_resolution.get(arg).expect("internal error: unresolved variable in codegen");
                                                        let var_type =
                                                            self.compiler.variables[var_id.0].ty;
                                                        self.codegen_typename(
                                                            var_type,
                                                            &mut variable_assignments,
                                                        );
                                                        variable_assignments
                                                            .extend_from_slice(b" variable_");
                                                        variable_assignments.extend_from_slice(
                                                            var_id.0.to_string().as_bytes(),
                                                        );

                                                        variable_assignments
                                                            .extend_from_slice(b" = ");
                                                        variable_assignments.extend_from_slice(
                                                            match_var.as_bytes(),
                                                        );
                                                        variable_assignments
                                                            .extend_from_slice(b"->");

                                                        match self.compiler.get_type(*enum_type_id)
                                                        {
                                                            Type::Enum { variants, .. } => {
                                                                match &variants[case_offset.0] {
                                                                    EnumVariant::Single {
                                                                        name,
                                                                        ..
                                                                    } => {
                                                                        variable_assignments
                                                                            .extend_from_slice(
                                                                                name,
                                                                            );
                                                                    }
                                                                    EnumVariant::Struct {
                                                                        params,
                                                                        ..
                                                                    } => {
                                                                        variable_assignments
                                                                            .extend_from_slice(
                                                                                &params[arg_idx].0,
                                                                            );
                                                                    }
                                                                    _ => panic!(
                                                                        "unsupported enum variant"
                                                                    ),
                                                                }
                                                            }
                                                            _ => {
                                                                panic!("internal error: enum match on non-enum variant ast node")
                                                            }
                                                        }

                                                        variable_assignments
                                                            .extend_from_slice(b";\n");
                                                    }
                                                    _ => {
                                                        panic!("not yet supported")
                                                    }
                                                }
                                            }

                                            output.extend_from_slice(b") {\n");

                                            output.extend_from_slice(&variable_assignments);

                                            self.codegen_node(*match_result, output);
                                            output.extend_from_slice(b"}\n");
                                        }
                                        x => {
                                            panic!("target not supported in enum codegen: {:?}", x);
                                        }
                                    }
                                }
                                x => {
                                    panic!("node not supported in enum codegen: {:?}", x);
                                }
                            }
                        }
                        x => {
                            panic!("node not supported in enum codegen: {:?}", x);
                        }
                    }
                }

                output.extend_from_slice(b"}\n");
            }
            AstNode::Block(..) => {
                self.codegen_block(node_id, output);
            }
            AstNode::True => {
                output.extend_from_slice(b"true");
            }
            AstNode::False => {
                output.extend_from_slice(b"false");
            }
            AstNode::Fun { .. } | AstNode::Struct { .. } | AstNode::Enum { .. } => {
                // ignore this, as we handle it elsewhere
            }
            x => {
                panic!("unsupported node: {:?}", x)
            }
        }
    }

    pub fn codegen_block(&self, block: NodeId, output: &mut Vec<u8>) {
        if let AstNode::Block(block_id) = self.compiler.get_node(block) {
            for node_id in &self.compiler.blocks[block_id.0].nodes {
                if let AstNode::Return(return_expr) = self.compiler.get_node(*node_id) {
                    if let Some(return_expr) = return_expr {
                        self.codegen_typename(self.compiler.get_node_type(*return_expr), output);
                        output.extend_from_slice(b" return_expr = ");
                        self.codegen_node(*return_expr, output);
                        output.extend_from_slice(b";\n");
                    }
                    if let Some(scope_level) = self.compiler.blocks[block_id.0].may_locally_allocate
                    {
                        output.extend_from_slice(
                            format!("deallocate(allocator, allocation_id + {});\n", scope_level)
                                .as_bytes(),
                        );
                    }
                    if return_expr.is_some() {
                        output.extend_from_slice(b"return return_expr;\n");
                    } else {
                        output.extend_from_slice(b"return;\n");
                    }

                    return;
                }
                self.codegen_node(*node_id, output);
                output.extend_from_slice(b";\n");
            }
            if let Some(scope_level) = self.compiler.blocks[block_id.0].may_locally_allocate {
                output.extend_from_slice(
                    format!("deallocate(allocator, allocation_id + {});\n", scope_level).as_bytes(),
                );
            }
        } else {
            panic!("codegen of a block that isn't a block")
        }
    }

    pub fn codegen(self) -> Vec<u8> {
        let mut output = vec![];

        let allocator = include_str!("../allocator/allocator.c");

        output.extend_from_slice(allocator.as_bytes());

        output.extend_from_slice(b"struct Allocator *allocator;\n");

        self.codegen_structs_and_enums(&mut output);
        self.codegen_fun_decls(&mut output);

        for (idx, fun) in self.compiler.functions.iter().enumerate().skip(1) {
            let name = self.compiler.get_source(fun.name);

            if name == b"main" {
                output.extend_from_slice(b"int main() {\n");
                output.extend_from_slice(b"allocator = create_allocator(DEFAULT_PAGE_SIZE);\n");
                output.extend_from_slice(b"function_");
                output.extend_from_slice(idx.to_string().as_bytes());
                output.extend_from_slice(b"(0);\n}\n");
            }
        }

        output
    }
}
