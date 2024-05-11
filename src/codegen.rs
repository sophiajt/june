use std::collections::{HashMap, HashSet};
use std::io::Write;

use crate::{
    compiler::{CallTarget, Compiler},
    lifetime_checker::AllocationLifetime,
    parser::{AstNode, NodeId},
    typechecker::{
        EnumVariant, FunId, Function, Param, Type, TypeId, TypedField, BOOL_TYPE_ID,
        C_CHAR_TYPE_ID, C_INT_TYPE_ID, C_SIZE_T_TYPE_ID, C_STRING_TYPE_ID, C_VOID_PTR_TYPE_ID,
        F64_TYPE_ID, I64_TYPE_ID, UNKNOWN_TYPE_ID, VOID_TYPE_ID,
    },
};

pub struct Codegen {
    compiler: Compiler,
}

impl Codegen {
    pub fn new(compiler: Compiler) -> Self {
        Codegen { compiler }
    }

    pub fn codegen_typename(
        &self,
        type_id: TypeId,
        local_inferences: &[TypeId],
        output: &mut Vec<u8>,
    ) {
        match self.compiler.get_type(type_id) {
            Type::FunLocalTypeVar { offset } => {
                self.codegen_typename(local_inferences[*offset], local_inferences, output)
            }
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
                self.codegen_typename(*type_id, local_inferences, output);
                output.push(b'*');
            }
            Type::Fun { .. } => {
                output.extend_from_slice(b"fun_ty_");
                output.extend_from_slice(type_id.0.to_string().as_bytes());
            }
            Type::CExternalType(node_id) => {
                let typename = self.compiler.get_source(*node_id);
                output.extend_from_slice(typename);
            }
            Type::RawBuffer(internal_type_id) => {
                self.codegen_typename(*internal_type_id, local_inferences, output);
                output.push(b'*');
            }
            _ => {
                if type_id == VOID_TYPE_ID {
                    output.extend_from_slice(b"void");
                } else if type_id == I64_TYPE_ID {
                    output.extend_from_slice(b"int64_t");
                } else if type_id == F64_TYPE_ID {
                    output.extend_from_slice(b"double");
                } else if type_id == C_STRING_TYPE_ID {
                    output.extend_from_slice(b"const char*");
                } else if type_id == C_VOID_PTR_TYPE_ID {
                    output.extend_from_slice(b"void*");
                } else if type_id == C_INT_TYPE_ID {
                    output.extend_from_slice(b"int");
                } else if type_id == C_SIZE_T_TYPE_ID {
                    output.extend_from_slice(b"size_t");
                } else if type_id == C_CHAR_TYPE_ID {
                    output.extend_from_slice(b"char")
                } else if type_id == BOOL_TYPE_ID {
                    output.extend_from_slice(b"bool");
                } else if type_id == UNKNOWN_TYPE_ID {
                    panic!("(unknown) type should be resolved before codegen");
                } else {
                    panic!("unknown type: {:?}", type_id)
                }
            }
        }
    }

    pub fn codegen_allocator_function(
        &self,
        type_id: TypeId,
        fields: &[TypedField],
        is_allocator: bool,
        base_classes: Option<&Vec<TypeId>>,
        output: &mut Vec<u8>,
    ) {
        let base_classes = base_classes.map(|v| v.as_slice()).unwrap_or(&[]);
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

        for (idx, TypedField { name, ty, .. }) in fields.iter().enumerate() {
            output.extend_from_slice(b", ");
            self.codegen_typename(*ty, &[], output);
            output.push(b' ');
            output.extend_from_slice(format!("field_{}", idx).as_bytes());
            output.extend_from_slice(b" /* ");
            output.extend_from_slice(name);
            output.extend_from_slice(b" */ ");
        }
        for base_class in base_classes {
            let Type::Struct { fields, .. } = self.compiler.get_type(*base_class) else {
                panic!("base classes should be struct Types");
            };
            for (idx, TypedField { name, ty, .. }) in fields.iter().enumerate() {
                output.extend_from_slice(b", ");
                self.codegen_typename(*ty, &[], output);
                output.push(b' ');
                write!(output, "base_{}_field_{}", base_class.0, idx).unwrap();
                output.extend_from_slice(b" /* ");
                output.extend_from_slice(name);
                output.extend_from_slice(b" */ ");
            }
        }
        output.extend_from_slice(b") {\n");

        self.codegen_typename(type_id, &[], output);
        output.extend_from_slice(b" tmp = (");
        self.codegen_typename(type_id, &[], output);
        output.extend_from_slice(b")allocate(allocator, sizeof(struct struct_");
        output.extend_from_slice(inner_type_id.0.to_string().as_bytes());
        output.extend_from_slice(b"), allocation_id);\n");

        if is_allocator {
            output.extend_from_slice(b"tmp->__allocation_id__ = allocation_id;\n");
        }

        output.extend_from_slice(b"initializer_");
        output.extend_from_slice(inner_type_id.0.to_string().as_bytes());
        output.extend_from_slice(b"(tmp");

        for (idx, TypedField { name, .. }) in fields.iter().enumerate() {
            output.extend_from_slice(b", field_");
            output.extend_from_slice(idx.to_string().as_bytes());
            output.extend_from_slice(b" /* ");
            output.extend_from_slice(name);
            output.extend_from_slice(b" */");
        }
        for base_class in base_classes {
            let Type::Struct {
                fields,
                is_allocator: _,
                generic_params: _,
            } = self.compiler.get_type(*base_class)
            else {
                panic!("base classes should be struct Types");
            };
            for (idx, TypedField { name, .. }) in fields.iter().enumerate() {
                write!(output, ", base_{}_field_{}", base_class.0, idx).unwrap();
                output.extend_from_slice(b" /* ");
                output.extend_from_slice(name);
                output.extend_from_slice(b" */");
            }
        }
        output.extend_from_slice(b");\n");

        output.extend_from_slice(b"return tmp;\n}\n");
    }

    pub fn codegen_initializer_function(
        &self,
        type_id: TypeId,
        fields: &[TypedField],
        base_classes: Option<&Vec<TypeId>>,
        output: &mut Vec<u8>,
    ) {
        let Type::Pointer {
            target: inner_type_id,
            ..
        } = self.compiler.get_type(type_id)
        else {
            panic!("internal error: pointer to unknown type");
        };

        write!(
            output,
            "void initializer_{inner_type_id}(struct struct_{inner_type_id}* tmp",
            inner_type_id = inner_type_id.0
        )
        .unwrap();
        for (idx, TypedField { name, ty, .. }) in fields.iter().enumerate() {
            output.extend_from_slice(b", ");
            self.codegen_typename(*ty, &[], output);
            write!(output, " field_{idx} /* ").unwrap();
            output.extend_from_slice(name);
            write!(output, " */ ").unwrap();
        }
        if let Some(base_classes) = base_classes {
            for base_class in base_classes {
                let Type::Struct { fields, .. } = self.compiler.get_type(*base_class) else {
                    todo!()
                };
                for (idx, TypedField { name, ty, .. }) in fields.iter().enumerate() {
                    output.extend_from_slice(b", ");
                    self.codegen_typename(*ty, &[], output);
                    output.push(b' ');
                    output.extend_from_slice(format!("base_field_{}", idx).as_bytes());
                    output.extend_from_slice(b" /* ");
                    output.extend_from_slice(name);
                    output.extend_from_slice(b" */ ");
                }
            }
        }
        output.extend_from_slice(b") {\n");

        if let Some(base_classes) = base_classes {
            let base_class = base_classes[0];
            output.extend_from_slice(b"initializer_");
            output.extend_from_slice(base_class.0.to_string().as_bytes());
            output.extend_from_slice(b"(&tmp->baseclass");
            for base_class in base_classes {
                let Type::Struct {
                    fields,
                    is_allocator: _,
                    generic_params: _,
                } = self.compiler.get_type(*base_class)
                else {
                    todo!()
                };
                for (idx, TypedField { name, .. }) in fields.iter().enumerate() {
                    write!(output, ", base_field_{} /* ", idx).unwrap();
                    output.extend_from_slice(name);
                    output.extend_from_slice(b" */");
                }
            }
            output.extend_from_slice(b");\n");

            for (depth, base_class) in base_classes.iter().enumerate() {
                if let Some(true) = self
                    .compiler
                    .fully_satisfies_virtual_methods(type_id, *base_class)
                {
                    output.extend_from_slice(b"tmp->");
                    for _ in 0..=depth {
                        output.extend_from_slice(b"baseclass.");
                    }
                    writeln!(output, "vtable = &vtable_struct_{};", inner_type_id.0).unwrap();
                }
            }
        }

        for (idx, TypedField { name, .. }) in fields.iter().enumerate() {
            output.extend_from_slice(format!("tmp->field_{} = field_{} /* ", idx, idx).as_bytes());
            output.extend_from_slice(name);
            output.extend_from_slice(b" */ ;\n");
        }

        output.extend_from_slice(b"\n}\n");
    }

    pub fn codegen_user_predecls(&self, output: &mut Vec<u8>) {
        for (idx, ty) in self.compiler.get_types().iter().enumerate() {
            match ty {
                Type::Struct { generic_params, .. } => {
                    if !generic_params.is_empty() {
                        // Don't codegen generic functions. Instead, only codegen their instantiations
                        continue;
                    }
                    output.extend_from_slice(b"struct struct_");
                    output.extend_from_slice(idx.to_string().as_bytes());
                    output.extend_from_slice(b";\n");
                }
                Type::Enum { generic_params, .. } => {
                    if !generic_params.is_empty() {
                        // Don't codegen generic functions. Instead, only codegen their instantiations
                        continue;
                    }

                    output.extend_from_slice(b"struct enum_");
                    output.extend_from_slice(idx.to_string().as_bytes());
                    output.extend_from_slice(b";\n");
                }
                Type::Fun { params, ret } => {
                    if params.iter().any(|x| {
                        let var_id = x.var_id;
                        self.compiler
                            .is_type_variable(self.compiler.get_variable(var_id).ty)
                    }) || self.compiler.is_type_variable(*ret)
                    {
                        continue;
                    }
                    // typedef long(*funcPtr)(short, char);
                    output.extend_from_slice(b"typedef ");
                    self.codegen_typename(*ret, &[], output);
                    output.extend_from_slice(b"(*fun_ty_");
                    output.extend_from_slice(idx.to_string().as_bytes());
                    // FIXME: we may not always have an allocation_id
                    output.extend_from_slice(b")(long");
                    for param in params {
                        output.extend_from_slice(b", ");
                        let var_type_id = self.compiler.get_variable(param.var_id).ty;
                        self.codegen_typename(var_type_id, &[], output);
                    }
                    output.extend_from_slice(b");\n");
                }
                _ => {}
            }
        }
    }

    pub fn codegen_user_types(&self, output: &mut Vec<u8>) {
        for (idx, ty) in self.compiler.get_types().iter().enumerate() {
            match ty {
                Type::Struct {
                    fields,
                    is_allocator,
                    generic_params,
                } => {
                    if !generic_params.is_empty() {
                        // Don't codegen generic functions. Instead, only codegen their instantiations
                        continue;
                    }

                    let methods = self.compiler.methods_on_type(TypeId(idx));
                    let virtual_methods = self.compiler.virtual_methods_on_type(TypeId(idx));

                    if !virtual_methods.is_empty() {
                        self.codegen_vtable_decl(idx, virtual_methods, output);
                        self.codegen_virt_function_typedefs(virtual_methods, output);
                    }

                    output.extend_from_slice(b"struct struct_");
                    output.extend_from_slice(idx.to_string().as_bytes());
                    output.extend_from_slice(b"{\n");

                    if *is_allocator {
                        self.codegen_typename(I64_TYPE_ID, &[], output);
                        output.push(b' ');
                        output.extend_from_slice(b"__allocation_id__;\n");
                    }
                    let base_classes = self.compiler.base_classes.get(&TypeId(idx));
                    if let Some(baseclasses) = base_classes {
                        let ty = baseclasses[0];
                        self.codegen_typename(ty, &[], output);
                        output.extend_from_slice(b" baseclass;\n");
                    }
                    if !virtual_methods.is_empty() {
                        output.extend_from_slice(b"const vtable_");
                        output.extend_from_slice(idx.to_string().as_bytes());
                        output.extend_from_slice(b"* vtable;\n");
                    }
                    for (idx, TypedField { name, ty, .. }) in fields.iter().enumerate() {
                        self.codegen_typename(*ty, &[], output);
                        output.push(b' ');
                        output.extend_from_slice(format!("field_{} ", idx).as_bytes());
                        output.extend_from_slice(b"/* ");
                        output.extend_from_slice(name);
                        output.extend_from_slice(b"*/ ;\n");
                    }

                    output.extend_from_slice(b"};\n");

                    if let Some(base_classes) = base_classes {
                        self.codegen_vtable_method_predecls(base_classes, idx, methods, output);
                        self.codegen_vtable_instantiation(base_classes, idx, methods, output);
                    }

                    if let Some(ptr) = self.compiler.find_pointer_to(TypeId(idx)) {
                        self.codegen_initializer_function(ptr, fields, base_classes, output);
                        if !self.compiler.has_unsatisfied_virtual_methods(TypeId(idx)) {
                            self.codegen_allocator_function(
                                ptr,
                                fields,
                                *is_allocator,
                                base_classes,
                                output,
                            );
                        }
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
                    for (idx, case) in cases.iter().enumerate() {
                        match case {
                            EnumVariant::Single { name, param: arg } => {
                                self.codegen_typename(*arg, &[], output);
                                output.push(b' ');
                                output.extend_from_slice(format!("case_{} /* ", idx).as_bytes());
                                output.extend_from_slice(name);
                                output.extend_from_slice(b" */ ;\n");
                            }
                            EnumVariant::Struct { name, params: args } => {
                                // FIXME!! This will name collide because of C naming resolution
                                output.extend_from_slice(b"struct /* ");
                                output.extend_from_slice(name);
                                output.extend_from_slice(b" */ {\n");

                                for (arg_idx, arg) in args.iter().enumerate() {
                                    self.codegen_typename(arg.1, &[], output);
                                    output.push(b' ');
                                    output.extend_from_slice(
                                        format!("case_{}_{} /* ", idx, arg_idx).as_bytes(),
                                    );
                                    output.extend_from_slice(&arg.0);
                                    output.extend_from_slice(b" */ ;\n");
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
                        self.codegen_typename(TypeId(idx), &[], output);
                        output.extend_from_slice(b"* enum_case_");
                        output.extend_from_slice(idx.to_string().as_bytes());
                        output.push(b'_');
                        output.extend_from_slice(case_offset.to_string().as_bytes());

                        output.extend_from_slice(b"(int allocation_id");

                        match case {
                            EnumVariant::Single { param, .. } => {
                                output.extend_from_slice(b", ");
                                self.codegen_typename(*param, &[], output);
                                output.extend_from_slice(b" arg");
                            }
                            EnumVariant::Struct { params, .. } => {
                                for (param_idx, (param_name, param_type)) in
                                    params.iter().enumerate()
                                {
                                    output.extend_from_slice(b", ");
                                    self.codegen_typename(*param_type, &[], output);
                                    output.push(b' ');
                                    output.extend_from_slice(
                                        format!("case_{}_{} /* ", case_offset, param_idx)
                                            .as_bytes(),
                                    );
                                    output.extend_from_slice(param_name);
                                    output.extend_from_slice(b" */ ");
                                }
                            }
                            EnumVariant::Simple { .. } => {}
                        }
                        output.extend_from_slice(b") {\n");

                        self.codegen_typename(TypeId(idx), &[], output);
                        output.extend_from_slice(b"* tmp = (");
                        self.codegen_typename(TypeId(idx), &[], output);
                        output.extend_from_slice(b"*)allocate(allocator, sizeof(struct enum_");
                        output.extend_from_slice(idx.to_string().as_bytes());
                        output.extend_from_slice(b"), allocation_id);\n");

                        output.extend_from_slice(b"tmp->arm_id = ");
                        output.extend_from_slice(case_offset.to_string().as_bytes());
                        output.extend_from_slice(b";\n");

                        match case {
                            EnumVariant::Single { name, .. } => {
                                output.extend_from_slice(
                                    format!("tmp->case_{}", case_offset).as_bytes(),
                                );
                                output.extend_from_slice(b" = ");
                                output.extend_from_slice(b"arg; /* ");
                                output.extend_from_slice(name);
                                output.extend_from_slice(b" */\n");
                            }
                            EnumVariant::Struct { params, .. } => {
                                for (param_idx, (param_name, _)) in params.iter().enumerate() {
                                    output.extend_from_slice(
                                        format!("tmp->case_{}_{}", case_offset, param_idx)
                                            .as_bytes(),
                                    );
                                    output.extend_from_slice(b" = ");
                                    output.extend_from_slice(
                                        format!("case_{}_{}; /* ", case_offset, param_idx)
                                            .as_bytes(),
                                    );
                                    output.extend_from_slice(param_name);
                                    output.extend_from_slice(b" */\n");
                                }
                            }
                            EnumVariant::Simple { .. } => {}
                        }

                        output.extend_from_slice(b"return tmp;\n");

                        output.extend_from_slice(b"}\n");
                    }
                }
                Type::Fun { params, ret } => {
                    if params.iter().any(|x| {
                        let var_id = x.var_id;
                        self.compiler
                            .is_type_variable(self.compiler.get_variable(var_id).ty)
                    }) || self.compiler.is_type_variable(*ret)
                    {
                        continue;
                    }

                    // typedef long(*funcPtr)(short, char);
                    output.extend_from_slice(b"typedef ");
                    self.codegen_typename(*ret, &[], output);
                    output.extend_from_slice(b"(*fun_ty_");
                    output.extend_from_slice(idx.to_string().as_bytes());
                    // FIXME: we may not always have an allocation_id
                    output.extend_from_slice(b")(long");
                    for param in params {
                        output.extend_from_slice(b", ");
                        let var_type_id = self.compiler.get_variable(param.var_id).ty;
                        self.codegen_typename(var_type_id, &[], output);
                    }
                    output.extend_from_slice(b");\n");
                }
                Type::RawBuffer(inner_type_id) => {
                    if matches!(
                        self.compiler.get_type(*inner_type_id),
                        Type::FunLocalTypeVar { .. }
                    ) {
                        continue;
                    }
                    self.codegen_typename(*inner_type_id, &[], output);
                    output.extend_from_slice(b"* create_buffer_");
                    output.extend_from_slice(idx.to_string().as_bytes());
                    output.extend_from_slice(b"(int level, int count, ...)\n{\n");
                    self.codegen_typename(TypeId(idx), &[], output);
                    output.extend_from_slice(
                        b" output = allocate_resizeable_page_on_allocator_level(allocator, level, sizeof(",
                    );
                    self.codegen_typename(*inner_type_id, &[], output);
                    output.extend_from_slice(b") * count);\n");
                    output.extend_from_slice(b"va_list args;\n");
                    output.extend_from_slice(b"va_start(args, count);\n");
                    output.extend_from_slice(b"for (int i = 0; i < count; i++) {\n");
                    output.extend_from_slice(b"*(output + i) = va_arg(args, ");
                    self.codegen_typename(*inner_type_id, &[], output);
                    output.extend_from_slice(b");\n");
                    output.extend_from_slice(b"}\n");
                    output.extend_from_slice(b"va_end(args);\n");
                    output.extend_from_slice(b"return output;\n}\n");
                }
                _ => {}
            }
        }
    }

    fn codegen_vtable_decl(&self, type_id: usize, virtual_methods: &[FunId], output: &mut Vec<u8>) {
        output.extend_from_slice(b"struct vtable_");
        output.extend_from_slice(type_id.to_string().as_bytes());
        output.extend_from_slice(b"{\n");
        for method in virtual_methods {
            let fun = &self.compiler.functions[method.0];
            self.codegen_typename(fun.return_type, &fun.inference_vars, output);
            output.extend_from_slice(b" (*");
            output.extend_from_slice(self.compiler.get_source(fun.name));
            output.extend_from_slice(b")(");
            output.extend_from_slice(b"long allocation_id");

            for param in &fun.params {
                output.extend_from_slice(b", ");

                let variable_ty = self.compiler.get_variable(param.var_id).ty;
                self.codegen_typename(variable_ty, &fun.inference_vars, output);
                output.push(b' ');
                output.extend_from_slice(b"variable_");
                output.extend_from_slice(param.var_id.0.to_string().as_bytes());
            }
            output.extend_from_slice(b");\n");
        }
        output.extend_from_slice(b"};\n");
        output.extend_from_slice(b"typedef struct vtable_");
        output.extend_from_slice(type_id.to_string().as_bytes());
        output.extend_from_slice(b" vtable_");
        output.extend_from_slice(type_id.to_string().as_bytes());
        output.extend_from_slice(b";\n");
    }

    fn codegen_virt_function_typedefs(&self, virtual_methods: &[FunId], output: &mut Vec<u8>) {
        for method in virtual_methods {
            let fun = &self.compiler.functions[method.0];
            output.extend_from_slice(b"typedef ");
            self.codegen_typename(fun.return_type, &fun.inference_vars, output);
            output.extend_from_slice(b" (*virt_fun_ty_");
            output.extend_from_slice(method.0.to_string().as_bytes());
            output.extend_from_slice(b")(");
            output.extend_from_slice(b"long allocation_id");

            for param in &fun.params {
                output.extend_from_slice(b", ");

                let variable_ty = self.compiler.get_variable(param.var_id).ty;
                self.codegen_typename(variable_ty, &fun.inference_vars, output);
                output.push(b' ');
                output.extend_from_slice(b"variable_");
                output.extend_from_slice(param.var_id.0.to_string().as_bytes());
            }
            output.extend_from_slice(b");\n");
        }
    }

    fn codegen_vtable_instantiation(
        &self,
        base_classes: &[TypeId],
        type_id: usize,
        methods: &[FunId],
        output: &mut Vec<u8>,
    ) {
        'bases: for base_class in base_classes.iter().rev() {
            let Some(vtable_fully_satisfied) = self
                .compiler
                .fully_satisfies_virtual_methods(TypeId(type_id), *base_class)
            else {
                continue 'bases;
            };

            if !vtable_fully_satisfied {
                continue 'bases;
            }

            let virtual_methods = self.compiler.virtual_methods_on_type(*base_class);
            let virtual_methods = virtual_methods
                .iter()
                .map(|id| {
                    let node_id = self.compiler.functions[id.0].name;
                    (self.compiler.get_source_str(node_id), id)
                })
                .collect::<HashMap<_, _>>();

            output.extend_from_slice(b"static const vtable_");
            output.extend_from_slice(base_class.0.to_string().as_bytes());
            output.extend_from_slice(b" vtable_struct_");
            output.extend_from_slice(type_id.to_string().as_bytes());
            output.extend_from_slice(b" = {\n");

            for method in methods {
                let fun = &self.compiler.functions[method.0];
                let method_name = self.compiler.get_source_str(fun.name);

                let Some(virt_fun_id) = virtual_methods.get(method_name) else {
                    continue;
                };
                output.extend_from_slice(b"    .");
                output.extend_from_slice(method_name.as_bytes());
                output.extend_from_slice(b" = (virt_fun_ty_");
                output.extend_from_slice(virt_fun_id.0.to_string().as_bytes());
                output.extend_from_slice(b")function_");
                output.extend_from_slice(method.0.to_string().as_bytes());
                output.extend_from_slice(b",\n");
            }

            output.extend_from_slice(b"};");
        }
    }

    fn codegen_vtable_method_predecls(
        &self,
        base_classes: &[TypeId],
        _type_id: usize,
        methods: &[FunId],
        output: &mut Vec<u8>,
    ) {
        let mut base_virtual_method_names = HashSet::new();
        for ty in base_classes {
            let base_virtual_methods = self.compiler.virtual_methods_on_type(*ty);
            base_virtual_method_names.extend(base_virtual_methods.iter().map(|id| {
                let node_id = self.compiler.functions[id.0].name;
                self.compiler.get_source(node_id)
            }));
        }

        for method in methods {
            let Function {
                name,
                params,
                inference_vars,
                lifetime_annotations: _,
                type_params: _,
                return_node: _,
                return_type: _,
                initial_node_id: _,
                body: _,
                is_extern: _,
            } = &self.compiler.functions[method.0];
            let method_name = self.compiler.get_source(*name);

            if base_virtual_method_names.contains(method_name) {
                output.extend_from_slice(b"void /* ");
                output.extend_from_slice(method_name);
                output.extend_from_slice(b" */ function_");
                output.extend_from_slice(method.0.to_string().as_bytes());
                output.extend_from_slice(b"(long allocation_id");

                for param in params {
                    output.extend_from_slice(b", ");
                    let variable_ty = self.compiler.get_variable(param.var_id).ty;
                    self.codegen_typename(variable_ty, inference_vars, output);
                    output.push(b' ');
                    output.extend_from_slice(b"variable_");
                    output.extend_from_slice(param.var_id.0.to_string().as_bytes());
                }
                output.extend_from_slice(b");");
            }
        }
    }

    pub fn codegen_fun_signature(
        &self,
        fun_id: FunId,
        params: &[Param],
        return_type: TypeId,
        output: &mut Vec<u8>,
        is_extern_c: bool,
    ) {
        self.codegen_typename(
            return_type,
            &self.compiler.functions[fun_id.0].inference_vars,
            output,
        );
        output.push(b' ');
        if is_extern_c {
            output.extend_from_slice(
                self.compiler
                    .get_source(self.compiler.functions[fun_id.0].name),
            );
            output.push(b'(');
        } else {
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
        }

        let mut first = is_extern_c;

        for param in params {
            if !first {
                output.extend_from_slice(b", ");
            } else {
                first = false;
            }

            let variable_ty = self.compiler.get_variable(param.var_id).ty;
            self.codegen_typename(
                variable_ty,
                &self.compiler.functions[fun_id.0].inference_vars,
                output,
            );
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
                type_params,
                return_type,
                body,
                ..
            },
        ) in self.compiler.functions.iter().enumerate().skip(1)
        {
            let mut has_generics_in_signature = !type_params.is_empty();
            for param in params {
                let var = self.compiler.get_variable(param.var_id);
                has_generics_in_signature |= self.compiler.is_generic_type(var.ty, vec![]);
            }
            has_generics_in_signature |= self.compiler.is_generic_type(*return_type, vec![]);

            if !has_generics_in_signature {
                self.codegen_fun_signature(
                    FunId(idx),
                    params,
                    *return_type,
                    output,
                    body.is_none(),
                );

                output.extend_from_slice(b";\n");
            }
        }
        output.push(b'\n');

        for (
            idx,
            Function {
                params,
                type_params,
                return_type,
                body,
                inference_vars: type_vars,
                ..
            },
        ) in self.compiler.functions.iter().enumerate().skip(1)
        {
            let mut has_generics_in_signature = !type_params.is_empty();
            for param in params {
                let var = self.compiler.get_variable(param.var_id);
                has_generics_in_signature |= self.compiler.is_generic_type(var.ty, vec![]);
            }
            has_generics_in_signature |= self.compiler.is_generic_type(*return_type, vec![]);

            if !has_generics_in_signature {
                if let Some(body) = body {
                    self.codegen_fun_signature(FunId(idx), params, *return_type, output, false);

                    output.extend_from_slice(b"{\n");
                    self.codegen_block(*body, type_vars, output);
                    output.extend_from_slice(b"}\n");
                }
            }
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

    pub fn codegen_node(&self, node_id: NodeId, local_inferences: &[TypeId], output: &mut Vec<u8>) {
        match &self.compiler.get_node(node_id) {
            AstNode::CString => {
                let src = self.compiler.get_source(node_id);

                output.extend_from_slice(src);
            }
            AstNode::CChar => {
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
                if let Some(var_id) = self.compiler.var_resolution.get(&node_id) {
                    output.extend_from_slice(b"/* ");
                    output.extend_from_slice(
                        self.compiler
                            .get_source(self.compiler.get_variable(*var_id).name),
                    );
                    output.extend_from_slice(b" */ ");

                    output.extend_from_slice(b"variable_");
                    output.extend_from_slice(var_id.0.to_string().as_bytes());
                } else if let Some(fun_id) = self.compiler.fun_resolution.get(&node_id) {
                    output.extend_from_slice(b"/* ");
                    output.extend_from_slice(
                        self.compiler
                            .get_source(self.compiler.functions[fun_id.0].name),
                    );
                    output.extend_from_slice(b" */ ");

                    output.extend_from_slice(b"function_");
                    output.extend_from_slice(fun_id.0.to_string().as_bytes());
                } else {
                    let src = self.compiler.get_source(node_id);

                    output.extend_from_slice(src);
                }
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

                let ty = self.compiler.get_variable(*var_id).ty;

                self.codegen_typename(ty, local_inferences, output);

                output.extend_from_slice(b" /* ");
                output.extend_from_slice(
                    self.compiler
                        .get_source(self.compiler.get_variable(*var_id).name),
                );
                output.extend_from_slice(b" */ ");

                output.extend_from_slice(b" variable_");
                output.extend_from_slice(var_id.0.to_string().as_bytes());

                output.extend_from_slice(b" = ");
                self.codegen_node(*initializer, local_inferences, output);
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
            AstNode::And => {
                output.extend_from_slice(b"&&");
            }
            AstNode::Or => {
                output.extend_from_slice(b"||");
            }
            AstNode::BitwiseAnd => {
                output.extend_from_slice(b"&");
            }
            AstNode::BitwiseOr => {
                output.extend_from_slice(b"|");
            }
            AstNode::ShiftLeft => {
                output.extend_from_slice(b"<<");
            }
            AstNode::ShiftRight => {
                output.extend_from_slice(b">>");
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
                if matches!(self.compiler.get_node(*op), AstNode::As) {
                    output.extend_from_slice(b"((");
                    let rhs_type_id = self.compiler.get_node_type(*rhs);
                    self.codegen_typename(rhs_type_id, local_inferences, output);
                    output.push(b')');
                    self.codegen_node(*lhs, local_inferences, output);
                    output.push(b')');
                } else {
                    output.push(b'(');
                    self.codegen_node(*lhs, local_inferences, output);
                    output.push(b')');

                    self.codegen_node(*op, local_inferences, output);

                    output.push(b'(');
                    self.codegen_node(*rhs, local_inferences, output);
                    output.push(b')');
                }
            }
            AstNode::Call { head, args } => {
                // if matches!(
                //     self.compiler.get_type(self.compiler.get_node_type(*head)),
                //     Type::Fun { .. }
                // ) {
                //     // We're calling into a first class function
                //     output.push(b'(');
                //     self.codegen_node(*head, output);
                //     output.extend_from_slice(b")(");
                //     self.codegen_annotation(node_id, output);
                //     for arg in args {
                //         output.extend_from_slice(b", ");
                //         self.codegen_node(*arg, output);
                //     }
                //     output.push(b')');
                //     return;
                // }
                let call_target = self.compiler.call_resolution.get(head).unwrap_or_else(|| {
                    panic!(
                        "internal error: missing call resolution in codegen: {:?}",
                        head
                    )
                });

                match call_target {
                    CallTarget::Function(fun_id) => {
                        let fun = &self.compiler.functions[fun_id.0];
                        if fun_id.0 == 0 {
                            // special case for println
                            match self.compiler.resolve_node_type(args[0], local_inferences) {
                                C_STRING_TYPE_ID => {
                                    output.extend_from_slice(b"printf(\"%s\\n\", ");
                                    self.codegen_node(args[0], local_inferences, output);
                                }
                                I64_TYPE_ID => {
                                    output.extend_from_slice(b"printf(\"%lli\\n\", ");
                                    self.codegen_node(args[0], local_inferences, output);
                                }
                                F64_TYPE_ID => {
                                    output.extend_from_slice(b"printf(\"%lf\\n\", ");
                                    self.codegen_node(args[0], local_inferences, output);
                                }
                                BOOL_TYPE_ID => {
                                    output.extend_from_slice(b"printf(\"%s\\n\", (");
                                    self.codegen_node(args[0], local_inferences, output);
                                    output.extend_from_slice(br#")?"true":"false""#);
                                }
                                C_INT_TYPE_ID => {
                                    output.extend_from_slice(b"printf(\"%i\\n\", ");
                                    self.codegen_node(args[0], local_inferences, output);
                                }
                                C_SIZE_T_TYPE_ID => {
                                    output.extend_from_slice(b"printf(\"%li\\n\", ");
                                    self.codegen_node(args[0], local_inferences, output);
                                }
                                C_CHAR_TYPE_ID => {
                                    output.extend_from_slice(b"printf(\"%c\\n\", ");
                                    self.codegen_node(args[0], local_inferences, output);
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

                        let mut first = true;

                        if fun.body.is_some() {
                            output.extend_from_slice(b"/* ");
                            output.extend_from_slice(self.compiler.get_source(fun.name));
                            output.extend_from_slice(b" */ ");

                            output.extend_from_slice(b"function_");
                            output.extend_from_slice(fun_id.0.to_string().as_bytes());
                            output.push(b'(');

                            self.codegen_annotation(node_id, output);
                            first = false;
                        } else if fun.is_extern {
                            output.extend_from_slice(self.compiler.get_source(fun.name));
                            output.push(b'(');
                        } else {
                            let AstNode::MemberAccess { target, .. } =
                                self.compiler.get_node(*head)
                            else {
                                panic!()
                            };
                            self.codegen_node(*target, local_inferences, output);
                            output.extend_from_slice(b"->vtable->");
                            output.extend_from_slice(self.compiler.get_source(fun.name));
                            output.push(b'(');

                            self.codegen_annotation(node_id, output);
                            first = false;
                        }

                        if let AstNode::MemberAccess { target, .. } = self.compiler.get_node(*head)
                        {
                            // A bit of a codegen workaround for now. Because we aren't updating the AST during typecheck,
                            // we haven't moved the target of the method call to be the first arg. To get around that,
                            // we'll manually push it in now.
                            if !first {
                                output.extend_from_slice(b", ");
                            }
                            self.codegen_node(*target, local_inferences, output);
                            first = false;
                        }

                        for arg in args {
                            if !first {
                                output.extend_from_slice(b", ");
                            } else {
                                first = false;
                            }

                            self.codegen_node(*arg, local_inferences, output)
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

                            self.codegen_node(*arg, local_inferences, output)
                        }
                        output.push(b')');
                    }
                    CallTarget::NodeId(..) => {
                        output.push(b'(');
                        self.codegen_node(*head, local_inferences, output);
                        output.push(b')');
                        output.push(b'(');

                        self.codegen_annotation(node_id, output);

                        for arg in args {
                            output.extend_from_slice(b", ");

                            self.codegen_node(*arg, local_inferences, output)
                        }
                        output.push(b')');
                    }
                }
            }
            AstNode::New(_, _, allocation_call) => {
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

                        self.codegen_node(*arg, local_inferences, output)
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

                                self.codegen_node(*arg, local_inferences, output)
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

                                self.codegen_node(*arg, local_inferences, output)
                            }
                            output.push(b')');
                        }
                        _ => {
                            unimplemented!("namedspaced lookup of non-namedspaced value")
                        }
                    }
                }
                AstNode::Name => {
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
                        CallTarget::NodeId(target) => {
                            output.push(b'(');
                            self.codegen_node(*target, local_inferences, output);
                            output.push(b')');

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
                self.codegen_node(*value, local_inferences, output)
            }
            AstNode::Break => {
                if let Some(exiting_blocks) = self.compiler.exiting_blocks.get(&node_id) {
                    for exiting_block in exiting_blocks.iter().rev() {
                        if let Some(scope_level) =
                            self.compiler.blocks[exiting_block.0].may_locally_allocate
                        {
                            output.extend_from_slice(
                                format!(
                                    "free_allocator_level(allocator, allocation_id + {});\n",
                                    scope_level
                                )
                                .as_bytes(),
                            );
                        }
                    }
                }
                output.extend_from_slice(b"break;\n");
            }
            AstNode::MemberAccess { target, field } => {
                self.codegen_node(*target, local_inferences, output);
                output.extend_from_slice(b"->");

                let field_name = self.compiler.get_source(*field);

                let type_id = self.compiler.get_node_type(*target);
                let type_id = self.compiler.resolve_type(type_id, local_inferences);
                let type_id = self.compiler.get_underlying_type_id(type_id);

                // FIXME: we can do this because the fields are unique, but we probably want
                // the resolution to tell us which one to use
                match self.compiler.get_type(type_id) {
                    Type::Struct { fields, .. } => {
                        let mut found = false;
                        for (idx, TypedField { name, .. }) in fields.iter().enumerate() {
                            if name == field_name {
                                output.extend_from_slice(format!("field_{} /* ", idx).as_bytes());
                                output.extend_from_slice(name);
                                output.extend_from_slice(b" */ ");

                                found = true;
                            }
                        }

                        if !found {
                            panic!("internal error: field could not be codegen'd: {:?}", target);
                        }
                    }
                    x => {
                        panic!("internal error: field access on non-struct: {:?}", x)
                    }
                }
            }
            AstNode::RawBuffer(items) => {
                let type_id = self.compiler.resolve_node_type(node_id, local_inferences);
                output.extend_from_slice(b"create_buffer_");
                output.extend_from_slice(type_id.0.to_string().as_bytes());
                output.push(b'(');
                self.codegen_annotation(node_id, output);
                output.extend_from_slice(b", ");
                output.extend_from_slice(items.len().to_string().as_bytes());
                for item in items {
                    output.extend_from_slice(b", ");
                    self.codegen_node(*item, local_inferences, output);
                }
                output.push(b')');
            }
            AstNode::Index { target, index } => {
                let target = *target;
                let index = *index;

                output.extend_from_slice(b"(*((");
                self.codegen_node(target, local_inferences, output);
                output.extend_from_slice(b") + (");
                self.codegen_node(index, local_inferences, output);
                output.extend_from_slice(b")))");
            }
            AstNode::Statement(node_id) => {
                self.codegen_node(*node_id, local_inferences, output);
                output.extend_from_slice(b";\n");
            }
            AstNode::If {
                condition,
                then_block,
                else_expression,
            } => {
                output.extend_from_slice(b"if (");
                self.codegen_node(*condition, local_inferences, output);
                output.extend_from_slice(b") {");
                self.codegen_node(*then_block, local_inferences, output);

                if let Some(else_expression) = else_expression {
                    output.extend_from_slice(b"} else {");
                    self.codegen_node(*else_expression, local_inferences, output);
                }
                output.extend_from_slice(b"}");
            }
            AstNode::While { condition, block } => {
                output.extend_from_slice(b"while (");
                self.codegen_node(*condition, local_inferences, output);
                output.extend_from_slice(b") {");
                self.codegen_node(*block, local_inferences, output);
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

                let ty = self.compiler.get_variable(*var_id).ty;

                self.codegen_typename(ty, local_inferences, output);

                output.extend_from_slice(b" variable_");
                output.extend_from_slice(var_id.0.to_string().as_bytes());

                output.extend_from_slice(b" = ");
                self.codegen_node(*lhs, local_inferences, output);

                output.extend_from_slice(b"; variable_");
                output.extend_from_slice(var_id.0.to_string().as_bytes());
                output.extend_from_slice(b" <= ");
                self.codegen_node(*rhs, local_inferences, output);

                output.extend_from_slice(b"; ++variable_");
                output.extend_from_slice(var_id.0.to_string().as_bytes());
                output.extend_from_slice(b") {");
                self.codegen_node(*block, local_inferences, output);
                output.extend_from_slice(b"}");
            }
            AstNode::Defer { pointer, callback } => {
                output.extend_from_slice(b"add_resource_cleanup(allocator, ");
                self.codegen_annotation(*pointer, output);
                output.extend_from_slice(b", ");
                self.codegen_node(*pointer, local_inferences, output);
                output.extend_from_slice(b", (void (*)(long, void *))");
                self.codegen_node(*callback, local_inferences, output);
                output.extend_from_slice(b");\n");
            }
            AstNode::ResizeRawBuffer { pointer, new_size } => {
                let pointer = *pointer;
                let new_size = *new_size;

                self.codegen_node(pointer, local_inferences, output);
                output.extend_from_slice(b" = resize_page_on_allocator_level(allocator, ");
                self.codegen_annotation(pointer, output);
                output.extend_from_slice(b", ");
                self.codegen_node(pointer, local_inferences, output);
                output.extend_from_slice(b", sizeof(");

                let pointer_type_id = self.compiler.resolve_node_type(pointer, local_inferences);

                match self.compiler.get_type(pointer_type_id) {
                    Type::RawBuffer(inner_type_id) => {
                        self.codegen_typename(*inner_type_id, local_inferences, output);
                    }
                    _ => {
                        panic!("internal error: resize of non-buffer type")
                    }
                }

                output.extend_from_slice(b") * (");
                self.codegen_node(new_size, local_inferences, output);
                output.extend_from_slice(b"));\n");
            }
            AstNode::Match { target, match_arms } => {
                output.extend_from_slice(b"{\n");
                let type_id = self.compiler.get_node_type(*target);
                self.codegen_typename(type_id, local_inferences, output);
                output.push(b' ');
                let match_var = format!("match_var_{}", target.0);

                output.extend_from_slice(match_var.as_bytes());
                output.extend_from_slice(b" = ");
                self.codegen_node(*target, local_inferences, output);

                output.extend_from_slice(b";\n");

                let mut first = true;

                for (match_arm, match_result) in match_arms {
                    if !first {
                        output.extend_from_slice(b"else ");
                    } else {
                        first = false
                    }
                    match self.compiler.get_node(*match_arm) {
                        AstNode::Name => {
                            output.extend_from_slice(b"if (true) {\n");

                            let var_id = self
                                .compiler
                                .var_resolution
                                .get(match_arm)
                                .expect("internal error: unresolved variable in codegen");
                            let var_type = self.compiler.get_variable(*var_id).ty;
                            self.codegen_typename(var_type, local_inferences, output);
                            output.extend_from_slice(b" variable_");
                            output.extend_from_slice(var_id.0.to_string().as_bytes());

                            output.extend_from_slice(b" = ");
                            output.extend_from_slice(match_var.as_bytes());
                            output.extend_from_slice(b";\n");

                            self.codegen_node(*match_result, local_inferences, output);

                            output.extend_from_slice(b"}\n");
                        }
                        AstNode::NamespacedLookup { item, .. } => {
                            match self.compiler.get_node(*item) {
                                AstNode::Name => {
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
                                            output.extend_from_slice(b") {");
                                            self.codegen_node(
                                                *match_result,
                                                local_inferences,
                                                output,
                                            );
                                            output.extend_from_slice(b"}\n");
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
                                                    AstNode::Name => {
                                                        let var_id = self.compiler.var_resolution.get(arg).expect("internal error: unresolved variable in codegen");
                                                        let var_type =
                                                            self.compiler.get_variable(*var_id).ty;
                                                        self.codegen_typename(
                                                            var_type,
                                                            local_inferences,
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
                                                                        ..
                                                                    } => {
                                                                        variable_assignments
                                                                            .extend_from_slice(
                                                                                format!(
                                                                                    "case_{}",
                                                                                    case_offset.0
                                                                                )
                                                                                .as_bytes(),
                                                                            );
                                                                    }
                                                                    EnumVariant::Struct {
                                                                        ..
                                                                    } => {
                                                                        variable_assignments
                                                                            .extend_from_slice(
                                                                                format!(
                                                                                    "case_{}_{}",
                                                                                    case_offset.0,
                                                                                    arg_idx
                                                                                )
                                                                                .as_bytes(),
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

                                            self.codegen_node(
                                                *match_result,
                                                local_inferences,
                                                output,
                                            );
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
                self.codegen_block(node_id, local_inferences, output);
            }
            AstNode::UnsafeBlock(block) => self.codegen_node(*block, local_inferences, output),
            AstNode::True => {
                output.extend_from_slice(b"true");
            }
            AstNode::False => {
                output.extend_from_slice(b"false");
            }
            AstNode::Fun { .. }
            | AstNode::Struct { .. }
            | AstNode::Enum { .. }
            | AstNode::ExternType { .. } => {
                // ignore this, as we handle it elsewhere
            }
            AstNode::TypeCoercion {
                source_node,
                target_type,
            } => {
                let ty = self.compiler.get_node_type(*target_type);
                output.extend_from_slice(b"(");
                self.codegen_typename(ty, local_inferences, output);
                output.extend_from_slice(b")");
                output.extend_from_slice(b"(");
                self.codegen_node(*source_node, local_inferences, output);
                output.extend_from_slice(b")");
            }
            x => {
                panic!("unsupported node: {:?}", x)
            }
        }
    }

    pub fn codegen_block(&self, block: NodeId, local_inferences: &[TypeId], output: &mut Vec<u8>) {
        if let AstNode::Block(block_id) = self.compiler.get_node(block) {
            for node_id in &self.compiler.blocks[block_id.0].nodes {
                if let AstNode::Return(return_expr) = self.compiler.get_node(*node_id) {
                    if let Some(return_expr) = return_expr {
                        self.codegen_typename(
                            self.compiler.get_node_type(*return_expr),
                            local_inferences,
                            output,
                        );
                        output.extend_from_slice(b" return_expr = ");
                        self.codegen_node(*return_expr, local_inferences, output);
                        output.extend_from_slice(b";\n");
                    }

                    if let Some(exiting_blocks) = self.compiler.exiting_blocks.get(node_id) {
                        for exiting_block in exiting_blocks.iter().rev() {
                            if let Some(scope_level) =
                                self.compiler.blocks[exiting_block.0].may_locally_allocate
                            {
                                output.extend_from_slice(
                                    format!(
                                        "free_allocator_level(allocator, allocation_id + {});\n",
                                        scope_level
                                    )
                                    .as_bytes(),
                                );
                            }
                        }
                    }
                    if return_expr.is_some() {
                        output.extend_from_slice(b"return return_expr;\n");
                    } else {
                        output.extend_from_slice(b"return;\n");
                    }

                    return;
                }
                self.codegen_node(*node_id, local_inferences, output);
                output.extend_from_slice(b";\n");
            }
            if let Some(scope_level) = self.compiler.blocks[block_id.0].may_locally_allocate {
                output.extend_from_slice(
                    format!(
                        "free_allocator_level(allocator, allocation_id + {});\n",
                        scope_level
                    )
                    .as_bytes(),
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

        self.codegen_user_predecls(&mut output);
        self.codegen_user_types(&mut output);
        self.codegen_fun_decls(&mut output);

        for (idx, fun) in self.compiler.functions.iter().enumerate().skip(1) {
            let name = self.compiler.get_source(fun.name);

            if name == b"main" {
                output.extend_from_slice(b"int main() {\n");
                output.extend_from_slice(b"allocator = create_allocator(100);\n");
                output.extend_from_slice(b"function_");
                output.extend_from_slice(idx.to_string().as_bytes());
                output.extend_from_slice(b"(0);\n}\n");
            }
        }

        output
    }
}
