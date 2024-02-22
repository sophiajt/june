#[cfg(test)]
type TestResult = Result<(), Report>;

use color_eyre::{eyre::eyre, eyre::Report, Section, SectionExt};
use std::{path::Path, process::Command};

#[cfg(test)]
fn test_example(test_name: &str) -> TestResult {
    use std::{
        fs::{create_dir_all, File},
        io::{stdout, Write},
        path::PathBuf,
        sync::Once,
    };

    static INIT_REPORTER: Once = Once::new();
    INIT_REPORTER.call_once(|| {
        color_eyre::install().unwrap();
    });

    // Create it if it's not there
    let mut temp_dir = std::env::temp_dir();
    temp_dir.push("june_tests");

    let relative_path = PathBuf::from(test_name);

    if let Some(parent) = relative_path.parent() {
        temp_dir.push(parent);
    }

    eprintln!("temp_dir: {:?}", temp_dir);

    let _ = create_dir_all(&temp_dir);

    let test_filepath = {
        let mut test_filename = PathBuf::from(test_name);
        test_filename.set_extension("june");

        let mut test_filepath = PathBuf::from("./tests/integration");
        test_filepath.push(test_filename);

        eprintln!("test_filepath: {:?}", test_filepath);

        test_filepath
    };

    let config = parse_special_comments(&test_filepath);

    let c_output_filepath = {
        let c_output_filename = PathBuf::from(test_name);
        let mut c_output_filename = PathBuf::from(
            c_output_filename
                .file_name()
                .expect("missing test filename"),
        );
        c_output_filename.set_extension("c");

        let mut c_output_filepath = temp_dir.clone();
        c_output_filepath.push(c_output_filename);

        eprintln!("c_output_filepath: {:?}", c_output_filepath);

        c_output_filepath
    };

    let app_filepath = {
        let mut app_filepath = temp_dir.clone();

        let test_name = PathBuf::from(test_name);
        let test_name = test_name.file_name().expect("missing test filename");
        app_filepath.push(test_name);

        app_filepath
    };

    let mut command = Command::new("./target/debug/june");
    command.arg(&test_filepath);
    let log_env = match (std::env::var("RUST_LOG"), config.log_config) {
        (Ok(env), None) => Some(env),
        (Ok(env), Some(test_cfg)) => {
            let joined = env + "," + &test_cfg;
            Some(joined)
        }
        (Err(_), Some(test_cfg)) => Some(test_cfg),
        (Err(_), None) => None,
    };
    if let Some(log_env) = log_env {
        command.env("RUST_LOG", log_env);
    }

    let output = command.output()?;
    let command_err = String::from_utf8_lossy(&output.stderr);
    let command_out = String::from_utf8_lossy(&output.stdout);

    if !output.status.success() && config.expected_error.is_none() {
        Err(eyre!("June did not compile successfully"))
            .with_section(|| command_out.trim().to_string().header("Stdout:"))
            .with_section(|| command_err.trim().to_string().header("Stderr:"))?;
    }

    if let Some(expected_error) = &config.expected_error {
        println!("Checking:\n{}expected: {}\n", command_err, expected_error);

        assert!(command_err.contains(expected_error));
    }

    let app_output = if config.expected_error.is_none() {
        // Now, output our C to a file
        eprintln!("c_output_filepath: {:?}", c_output_filepath);
        let mut output_file = File::create(&c_output_filepath).unwrap();
        let _ = output_file.write_all(&output.stdout);

        // Next, compile the file
        let compiler = Command::new("clang")
            .arg(&c_output_filepath)
            .arg("-o")
            .arg(&app_filepath)
            .output()
            .unwrap();

        if !compiler.status.success() {
            let _ = stdout().write_all(&compiler.stdout);
            let _ = stdout().write_all(&compiler.stderr);
            panic!("Clang did not compile successfully");
        }

        let app = Command::new(app_filepath).output().unwrap();
        if !app.status.success() {
            panic!("App did not run successfully");
        }

        // Lastly, compare the expected output
        let app_output = app.stdout.to_vec();
        let app_output = String::from_utf8_lossy(&app_output);

        let app_output = app_output.replace("\r\n", "");
        app_output.replace('\n', "")
    } else {
        String::new()
    };

    if let Some(expected_output) = config.expected_output {
        assert_eq!(expected_output, app_output);
    }

    Ok(())
}

struct TestConfig {
    expected_output: Option<String>,
    expected_error: Option<String>,
    log_config: Option<String>,
}

fn parse_special_comments(test_filepath: &Path) -> TestConfig {
    let example_file_contents = std::fs::read_to_string(test_filepath).unwrap();
    let mut expected_output = None;
    let mut expected_error = None;
    let mut log_config = None;
    for line in example_file_contents.lines() {
        if line.starts_with("// output: ") && expected_output.is_none() {
            expected_output = line.strip_prefix("// output: ").map(ToOwned::to_owned);
        } else if line.starts_with("// error: ") && expected_error.is_none() {
            expected_error = line.strip_prefix("// error: ").map(ToOwned::to_owned);
        } else if line.starts_with("// RUST_LOG: ") && log_config.is_none() {
            log_config = line.strip_prefix("// RUST_LOG: ").map(ToOwned::to_owned);
        }
    }

    if expected_output.is_none() && expected_error.is_none() {
        panic!("missing \"output:\" or \"error:\" in test")
    }

    TestConfig {
        expected_output,
        expected_error,
        log_config,
    }
}

#[test]
fn alias_inference() -> TestResult {
    test_example("lifetime_inference/alias_inference")
}

#[test]
fn alias_inference2() -> TestResult {
    test_example("lifetime_inference/alias_inference2")
}

#[test]
fn alias_inference_error() -> TestResult {
    test_example("lifetime_inference/alias_inference_error")
}

#[test]
fn alias_return_error() -> TestResult {
    test_example("lifetime_inference/alias_return_error")
}

#[test]
fn annotation_param_compatible() -> TestResult {
    test_example("lifetime_inference/annotation_param_compatible")
}

#[test]
fn annotation_return_compatible() -> TestResult {
    test_example("lifetime_inference/annotation_return_compatible")
}

#[test]
fn annotation_return_compatible2() -> TestResult {
    test_example("lifetime_inference/annotation_return_compatible2")
}

#[test]
fn bad_condition() -> TestResult {
    test_example("parsing/bad_condition")
}

#[test]
fn boolean() -> TestResult {
    test_example("data_types/boolean")
}

#[test]
fn c_char() -> TestResult {
    test_example("data_types/c_char")
}

#[test]
fn c_string() -> TestResult {
    test_example("data_types/c_string")
}

#[test]
fn call_labeled_error() -> TestResult {
    test_example("typechecking/call_labeled_error")
}

#[test]
fn circular_linked_list() -> TestResult {
    test_example("data_structures/circular_linked_list")
}

#[test]
fn circular_linked_list_abstraction() -> TestResult {
    test_example("data_structures/circular_linked_list_abstraction")
}

#[test]
fn circular_linked_list_abstraction_shared() -> TestResult {
    test_example("data_structures/circular_linked_list_abstraction_shared")
}

#[test]
fn circular_linked_list_abstraction_shorthand() -> TestResult {
    test_example("data_structures/circular_linked_list_abstraction_shorthand")
}

#[test]
fn circular_linked_list_helper() -> TestResult {
    test_example("data_structures/circular_linked_list_helper")
}

#[test]
fn defer_simple() -> TestResult {
    test_example("defer/defer_simple")
}

#[test]
fn defer_simple_error() -> TestResult {
    test_example("defer/defer_simple_error")
}

#[test]
fn module_simple() -> TestResult {
    test_example("modules/main")
}

#[test]
fn module_collision() -> TestResult {
    test_example("modules/collisions")
}

#[test]
fn module_deep() -> TestResult {
    test_example("modules/deep")
}

#[test]
fn double() -> TestResult {
    test_example("data_types/double")
}

#[test]
fn enum_catchall() -> TestResult {
    test_example("enums/enum_catchall")
}

#[test]
fn enum_method() -> TestResult {
    test_example("enums/enum_method")
}

#[test]
fn enum_missing_arm_error() -> TestResult {
    test_example("enums/enum_missing_arm_error")
}

#[test]
fn enum_multi_arm() -> TestResult {
    test_example("enums/enum_multi_arm")
}

#[test]
fn enum_same_name() -> TestResult {
    test_example("enums/enum_same_name")
}

#[test]
fn enum_simple() -> TestResult {
    test_example("enums/enum_simple")
}

#[test]
fn enum_simple_error() -> TestResult {
    test_example("enums/enum_simple_error")
}

#[test]
fn enum_single_payload() -> TestResult {
    test_example("enums/enum_single_payload")
}

#[test]
fn enum_single_pointer_payload() -> TestResult {
    test_example("enums/enum_single_pointer_payload")
}

#[test]
fn enum_static_method() -> TestResult {
    test_example("enums/enum_static_method")
}

#[test]
fn enum_struct_payload() -> TestResult {
    test_example("enums/enum_struct_payload")
}

#[test]
fn enum_var_scope_error() -> TestResult {
    test_example("enums/enum_var_scope_error")
}

#[test]
fn enum_var_scope_error2() -> TestResult {
    test_example("enums/enum_var_scope_error2")
}

#[test]
fn enum_var_scope_error3() -> TestResult {
    test_example("enums/enum_var_scope_error3")
}

#[test]
fn escaping_local_alt_error() -> TestResult {
    test_example("lifetime_inference/escaping_local_alt_error")
}

#[test]
fn escaping_local_error() -> TestResult {
    test_example("lifetime_inference/escaping_local_error")
}

#[test]
fn escaping_local_error2() -> TestResult {
    test_example("lifetime_inference/escaping_local_error2")
}

#[test]
fn file_io_abstraction() -> TestResult {
    test_example("extern_c/file_io_abstraction")
}

#[test]
fn file_ptr() -> TestResult {
    test_example("extern_c/file_ptr")
}

#[test]
fn file_ptr_all() -> TestResult {
    test_example("extern_c/file_ptr_all")
}

#[test]
fn file_ptr_defer() -> TestResult {
    test_example("extern_c/file_ptr_defer")
}

#[test]
fn first_class_function_allocation() -> TestResult {
    test_example("first_class_functions/first_class_function_allocation")
}

#[test]
fn first_class_function_allocation2() -> TestResult {
    test_example("first_class_functions/first_class_function_allocation2")
}

#[test]
fn first_class_function_arg() -> TestResult {
    test_example("first_class_functions/first_class_function_arg")
}

#[test]
fn first_class_function_return() -> TestResult {
    test_example("first_class_functions/first_class_function_return")
}

#[test]
fn first_class_function_return_error() -> TestResult {
    test_example("first_class_functions/first_class_function_return_error")
}

#[test]
fn first_class_function_return_error2() -> TestResult {
    test_example("first_class_functions/first_class_function_return_error2")
}

#[test]
fn first_class_function_value() -> TestResult {
    test_example("first_class_functions/first_class_function_value")
}

#[test]
fn first_class_function_value_call() -> TestResult {
    test_example("first_class_functions/first_class_function_value_call")
}

#[test]
fn first_class_function_value_call_error() -> TestResult {
    test_example("first_class_functions/first_class_function_value_call_error")
}

#[test]
fn for_range() -> TestResult {
    test_example("control_flow/for_range")
}

#[test]
fn for_range_var() -> TestResult {
    test_example("control_flow/for_range_var")
}

#[test]
fn function_in_function() -> TestResult {
    test_example("typechecking/function_in_function")
}

#[test]
fn generic_enum() -> TestResult {
    test_example("generics/generic_enum")
}

#[test]
fn generic_enum2() -> TestResult {
    test_example("generics/generic_enum2")
}

#[test]
fn generic_enum_struct_case() -> TestResult {
    test_example("generics/generic_enum_struct_case")
}

#[test]
fn generic_struct() -> TestResult {
    test_example("generics/generic_struct")
}

#[test]
fn global_variable_error() -> TestResult {
    test_example("typechecking/global_variable_error")
}

#[test]
fn hello_fun_rev_order() -> TestResult {
    test_example("hello_world/hello_fun_rev_order")
}

#[test]
fn hello_fun_rev_order2() -> TestResult {
    test_example("hello_world/hello_fun_rev_order2")
}

#[test]
fn hello_fun() -> TestResult {
    test_example("hello_world/hello_fun")
}

#[test]
fn hello_main() -> TestResult {
    test_example("hello_world/hello_main")
}

#[test]
fn hello_world() -> TestResult {
    test_example("hello_world/hello_world")
}

#[test]
fn incompatible_param_error() -> TestResult {
    test_example("lifetime_inference/incompatible_param_error")
}

#[test]
fn incompatible_param_error2() -> TestResult {
    test_example("lifetime_inference/incompatible_param_error2")
}

#[test]
fn int_math_main() -> TestResult {
    test_example("math/int_math_main")
}

#[test]
fn int_math() -> TestResult {
    test_example("math/int_math")
}

#[test]
fn int_math_negative() -> TestResult {
    test_example("math/int_math_negative")
}

#[test]
fn int() -> TestResult {
    test_example("data_types/int")
}

#[test]
fn jason() -> TestResult {
    test_example("jason")
}

#[test]
fn lifetime_alloc_infer_error() -> TestResult {
    test_example("lifetime_inference/lifetime_alloc_infer_error")
}

#[test]
fn lifetime_alloc_infer_success() -> TestResult {
    test_example("lifetime_inference/lifetime_alloc_infer_success")
}

#[test]
fn lifetime_error() -> TestResult {
    test_example("lifetime_inference/lifetime_error")
}

#[test]
fn lifetime_infer_helper_function() -> TestResult {
    test_example("lifetime_inference/lifetime_infer_helper_function")
}

#[test]
fn lifetime_infer_helper_function2() -> TestResult {
    test_example("lifetime_inference/lifetime_infer_helper_function2")
}

#[test]
fn loop_() -> TestResult {
    test_example("control_flow/loop")
}

#[test]
fn loop_with_temporary() -> TestResult {
    test_example("control_flow/loop_with_temporary")
}

#[test]
fn method_and_struct_allocator() -> TestResult {
    test_example("structs/method_and_struct_allocator")
}

#[test]
fn method_immutable_self_error() -> TestResult {
    test_example("structs/method_immutable_self_error")
}

#[test]
fn method_in_method() -> TestResult {
    test_example("structs/method_in_method")
}

#[test]
fn method_mutation() -> TestResult {
    test_example("structs/method_mutation")
}

#[test]
fn method_simple() -> TestResult {
    test_example("structs/method_simple")
}

#[test]
fn move_ptr() -> TestResult {
    test_example("structs/method_simple_error")
}

#[test]
fn moved_owned_var_error() -> TestResult {
    test_example("owned_abstractions/moved_owned_var_error")
}

#[test]
fn moved_owned_var_error2() -> TestResult {
    test_example("owned_abstractions/moved_owned_var_error2")
}

#[test]
fn moved_owned_var_error3() -> TestResult {
    test_example("owned_abstractions/moved_owned_var_error3")
}

#[test]
fn moved_owned_var_into_shared_error() -> TestResult {
    test_example("owned_abstractions/moved_owned_var_into_shared_error")
}

#[test]
fn multiple_method_calls_on_self() -> TestResult {
    test_example("owned_abstractions/multiple_method_calls_on_self")
}

#[test]
fn mutability_mismatch_error() -> TestResult {
    test_example("typechecking/mutability_mismatch_error")
}

#[test]
fn owned_method_doesnt_permanently_move() -> TestResult {
    test_example("owned_abstractions/owned_method_doesnt_permanently_move")
}

#[test]
fn owned_struct() -> TestResult {
    test_example("owned_abstractions/owned_struct")
}

#[test]
fn owned_struct_private() -> TestResult {
    test_example("owned_abstractions/owned_struct_private")
}

#[test]
fn owned_struct_private2() -> TestResult {
    test_example("owned_abstractions/owned_struct_private2")
}

#[test]
fn owned_struct_error() -> TestResult {
    test_example("owned_abstractions/owned_struct_error")
}

#[test]
fn owned_struct_error2() -> TestResult {
    test_example("owned_abstractions/owned_struct_error2")
}

#[test]
fn owned_struct_error3() -> TestResult {
    test_example("owned_abstractions/owned_struct_error3")
}

#[test]
fn params_dont_leak_error() -> TestResult {
    test_example("typechecking/params_dont_leak_error")
}

#[test]
fn passthrough_error() -> TestResult {
    test_example("lifetime_inference/passthrough_error")
}

#[test]
fn passthrough_error2() -> TestResult {
    test_example("lifetime_inference/passthrough_error2")
}

#[test]
fn private_by_default() -> TestResult {
    test_example("classes/private_by_default")
}

#[test]
fn puts() -> TestResult {
    test_example("extern_c/puts")
}

#[test]
fn raw_buffer_argument() -> TestResult {
    test_example("raw_buffers/raw_buffer_argument")
}

#[test]
fn raw_buffer_assignment() -> TestResult {
    test_example("raw_buffers/raw_buffer_assignment")
}

#[test]
fn raw_buffer_assignment_error() -> TestResult {
    test_example("raw_buffers/raw_buffer_assignment_error")
}

#[test]
fn raw_buffer_defer() -> TestResult {
    test_example("raw_buffers/raw_buffer_defer")
}

#[test]
fn raw_buffer_resize() -> TestResult {
    test_example("raw_buffers/raw_buffer_resize")
}

#[test]
fn raw_buffer_return() -> TestResult {
    test_example("raw_buffers/raw_buffer_return")
}

#[test]
fn raw_buffer_simple() -> TestResult {
    test_example("raw_buffers/raw_buffer_simple")
}

#[test]
fn raw_buffer_type_inference() -> TestResult {
    test_example("typechecking/raw_buffer_type_inference")
}

#[test]
fn raw_buffer_type_inference2() -> TestResult {
    test_example("typechecking/raw_buffer_type_inference2")
}

#[test]
fn raw_buffer_with_pointers() -> TestResult {
    test_example("raw_buffers/raw_buffer_with_pointers")
}

#[test]
fn return_missing_error() -> TestResult {
    test_example("typechecking/return_missing_error")
}

#[test]
fn return_missing_value_error() -> TestResult {
    test_example("typechecking/return_missing_value_error")
}

#[test]
fn return_top_level_error() -> TestResult {
    test_example("typechecking/return_top_level_error")
}

#[test]
fn return_value() -> TestResult {
    test_example("typechecking/return_value")
}

#[test]
fn return_value_error() -> TestResult {
    test_example("typechecking/return_value_error")
}

#[test]
fn scope_struct() -> TestResult {
    test_example("lifetime_inference/scope_struct")
}

#[test]
fn scope_struct_to_upper_scope() -> TestResult {
    test_example("lifetime_inference/scope_struct_to_upper_scope")
}
#[test]
fn scope_struct_to_upper_scope2() -> TestResult {
    test_example("lifetime_inference/scope_struct_to_upper_scope2")
}

#[test]
fn static_method() -> TestResult {
    test_example("structs/static_method")
}

#[test]
fn struct_allocator() -> TestResult {
    test_example("structs/struct_allocator")
}

#[test]
fn struct_arg_count_error() -> TestResult {
    test_example("structs/struct_arg_count_error")
}

#[test]
fn struct_field_access_caller() -> TestResult {
    test_example("structs/struct_field_access_caller")
}

#[test]
fn struct_field_access_caller2() -> TestResult {
    test_example("structs/struct_field_access_caller2")
}

#[test]
fn struct_field_access_locally() -> TestResult {
    test_example("structs/struct_field_access_locally")
}

#[test]
fn struct_field_deep() -> TestResult {
    test_example("structs/struct_field_deep")
}

#[test]
fn struct_field_math_mainless() -> TestResult {
    test_example("structs/struct_field_math_mainless")
}

#[test]
fn struct_field_math() -> TestResult {
    test_example("structs/struct_field_math")
}

#[test]
fn struct_field_private_error() -> TestResult {
    test_example("structs/struct_field_private_error")
}

#[test]
fn struct_field_private_error2() -> TestResult {
    test_example("structs/struct_field_private_error2")
}

#[test]
fn struct_field_private_error3() -> TestResult {
    test_example("structs/struct_field_private_error2")
}

#[test]
fn struct_field_private() -> TestResult {
    test_example("structs/struct_field_private")
}

#[test]
fn struct_field_private2() -> TestResult {
    test_example("structs/struct_field_private2")
}

#[test]
fn struct_field_private3() -> TestResult {
    test_example("structs/struct_field_private3")
}

#[test]
fn struct_field() -> TestResult {
    test_example("structs/struct_field")
}

#[test]
fn struct_field_helper() -> TestResult {
    test_example("structs/struct_field_helper")
}

#[test]
fn struct_field_mutable_param() -> TestResult {
    test_example("structs/struct_field_mutable_param")
}

#[test]
fn struct_field_update() -> TestResult {
    test_example("structs/struct_field_update")
}

#[test]
fn struct_helper() -> TestResult {
    test_example("structs/struct_helper")
}

#[test]
fn struct_helper_deep() -> TestResult {
    test_example("structs/struct_helper_deep")
}

#[test]
fn struct_in_fun() -> TestResult {
    test_example("structs/struct_in_fun")
}

#[test]
fn struct_in_fun_error() -> TestResult {
    test_example("structs/struct_in_fun_error")
}

#[test]
fn struct_in_struct() -> TestResult {
    test_example("structs/struct_in_struct")
}

#[test]
fn struct_new_field_error() -> TestResult {
    test_example("structs/struct_new_field_error")
}

#[test]
fn struct_() -> TestResult {
    test_example("structs/struct")
}

#[test]
fn variable_and_function() -> TestResult {
    test_example("variables/variable_and_function")
}

#[test]
fn variable_bad_optional_type() -> TestResult {
    test_example("variables/variable_bad_optional_type")
}

#[test]
fn variable_mutation_error() -> TestResult {
    test_example("variables/variable_mutation_error")
}

#[test]
fn variable_mutation() -> TestResult {
    test_example("variables/variable_mutation")
}

#[test]
fn variable_mutation2() -> TestResult {
    test_example("variables/variable_mutation2")
}

#[test]
fn variable_mutation3() -> TestResult {
    test_example("variables/variable_mutation3")
}

#[test]
fn variable_simple_error() -> TestResult {
    test_example("variables/variable_simple_error")
}

#[test]
fn variable_simple_error2() -> TestResult {
    test_example("variables/variable_simple_error2")
}

#[test]
fn variable_simple() -> TestResult {
    test_example("variables/variable_simple")
}

#[test]
fn variable() -> TestResult {
    test_example("variables/variable")
}

#[test]
fn vector_of_ints() -> TestResult {
    test_example("data_structures/vector_of_ints")
}

#[test]
fn unicode() -> TestResult {
    test_example("parsing/unicode")
}

#[test]
fn unicode2() -> TestResult {
    test_example("parsing/unicode2")
}

#[test]
fn unicode3() -> TestResult {
    test_example("parsing/unicode3")
}
