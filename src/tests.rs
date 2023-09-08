#[cfg(test)]
use std::error::Error;

#[cfg(test)]
type TestResult = Result<(), Box<dyn Error>>;

#[cfg(test)]
fn test_example(test_name: &str) -> TestResult {
    use std::{
        fs::{create_dir, File},
        io::{stdout, Write},
        path::PathBuf,
        process::Command,
    };

    // Create it if it's not there
    let mut temp_dir = std::env::temp_dir();
    temp_dir.push("june_tests");
    let _ = create_dir(&temp_dir);

    let test_filepath = {
        let mut test_filename = PathBuf::from(test_name);
        test_filename.set_extension("june");

        let mut test_filepath = PathBuf::from("./tests");
        test_filepath.push(test_filename);

        test_filepath
    };

    let (expected_output, expected_error) = {
        let example_file_contents = std::fs::read_to_string(&test_filepath).unwrap();
        let first_line = example_file_contents.lines().next().unwrap().to_string();

        if first_line.starts_with("// output: ") {
            (
                Some(first_line.strip_prefix("// output: ").unwrap().to_string()),
                None,
            )
        } else if first_line.starts_with("// error: ") {
            (
                None,
                Some(first_line.strip_prefix("// error: ").unwrap().to_string()),
            )
        } else {
            panic!("missing \"output:\" or \"error:\" in test")
        }
    };

    let c_output_filepath = {
        let mut c_output_filename = PathBuf::from(test_name);
        c_output_filename.set_extension("c");

        let mut c_output_filepath = temp_dir.clone();
        c_output_filepath.push(c_output_filename);

        c_output_filepath
    };

    let app_filepath = {
        let mut app_filepath = temp_dir.clone();
        app_filepath.push(test_name);

        app_filepath
    };

    let command = Command::new("./target/debug/june")
        .arg(&test_filepath)
        .output();
    let command = command.unwrap();
    if !command.status.success() && expected_error.is_none() {
        panic!("June did not compile successfully");
    }

    if let Some(expected_error) = &expected_error {
        let command_err = String::from_utf8_lossy(&command.stderr);

        println!("Checking:\n{}expected: {}\n", command_err, expected_error);

        assert!(command_err.contains(expected_error));
    }

    let app_output = if expected_error.is_none() {
        // Now, output our C to a file
        let mut output_file = File::create(&c_output_filepath).unwrap();
        let _ = output_file.write_all(&command.stdout);

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

    if let Some(expected_output) = expected_output {
        assert_eq!(expected_output, app_output);
    }

    Ok(())
}

#[test]
fn alias_inference() -> TestResult {
    test_example("alias_inference")
}

#[test]
fn alias_inference2() -> TestResult {
    test_example("alias_inference2")
}

#[test]
fn alias_inference_error() -> TestResult {
    test_example("alias_inference_error")
}

#[test]
fn alias_return_error() -> TestResult {
    test_example("alias_return_error")
}

#[test]
fn bad_condition() -> TestResult {
    test_example("bad_condition")
}

#[test]
fn boolean() -> TestResult {
    test_example("boolean")
}

#[test]
fn call_labeled_error() -> TestResult {
    test_example("call_labeled_error")
}

#[test]
fn circular_linked_list() -> TestResult {
    test_example("circular_linked_list")
}

#[test]
fn circular_linked_list_abstraction() -> TestResult {
    test_example("circular_linked_list_abstraction")
}

#[test]
fn circular_linked_list_abstraction_shorthand() -> TestResult {
    test_example("circular_linked_list_abstraction_shorthand")
}

#[test]
fn circular_linked_list_helper() -> TestResult {
    test_example("circular_linked_list_helper")
}

#[test]
fn double() -> TestResult {
    test_example("double")
}

#[test]
fn enum_catchall() -> TestResult {
    test_example("enum_catchall")
}

#[test]
fn enum_missing_arm_error() -> TestResult {
    test_example("enum_missing_arm_error")
}

#[test]
fn enum_multi_arm() -> TestResult {
    test_example("enum_multi_arm")
}

#[test]
fn enum_simple() -> TestResult {
    test_example("enum_simple")
}

#[test]
fn enum_simple_error() -> TestResult {
    test_example("enum_simple_error")
}

#[test]
fn enum_single_payload() -> TestResult {
    test_example("enum_single_payload")
}

#[test]
fn enum_single_pointer_payload() -> TestResult {
    test_example("enum_single_pointer_payload")
}

#[test]
fn enum_struct_payload() -> TestResult {
    test_example("enum_struct_payload")
}

#[test]
fn enum_var_scope_error() -> TestResult {
    test_example("enum_var_scope_error")
}

#[test]
fn enum_var_scope_error2() -> TestResult {
    test_example("enum_var_scope_error2")
}

#[test]
fn enum_var_scope_error3() -> TestResult {
    test_example("enum_var_scope_error3")
}

#[test]
fn for_range() -> TestResult {
    test_example("for_range")
}

#[test]
fn for_range_var() -> TestResult {
    test_example("for_range_var")
}

#[test]
fn function_in_function() -> TestResult {
    test_example("function_in_function")
}

#[test]
fn generic_enum() -> TestResult {
    test_example("generic_enum")
}

#[test]
fn generic_enum2() -> TestResult {
    test_example("generic_enum2")
}

#[test]
fn generic_enum_struct_case() -> TestResult {
    test_example("generic_enum_struct_case")
}

#[test]
fn generic_struct() -> TestResult {
    test_example("generic_struct")
}

#[test]
fn hello_fun_rev_order() -> TestResult {
    test_example("hello_fun_rev_order")
}

#[test]
fn hello_fun_rev_order2() -> TestResult {
    test_example("hello_fun_rev_order2")
}

#[test]
fn hello_fun() -> TestResult {
    test_example("hello_fun")
}

#[test]
fn hello_main() -> TestResult {
    test_example("hello_main")
}

#[test]
fn hello_world() -> TestResult {
    test_example("hello_world")
}

#[test]
fn int_math_main() -> TestResult {
    test_example("int_math_main")
}

#[test]
fn int_math() -> TestResult {
    test_example("int_math")
}

#[test]
fn int() -> TestResult {
    test_example("int")
}

#[test]
fn jason() -> TestResult {
    test_example("jason")
}

#[test]
fn lifetime_alloc_infer_error() -> TestResult {
    test_example("lifetime_alloc_infer_error")
}

#[test]
fn lifetime_alloc_infer_success() -> TestResult {
    test_example("lifetime_alloc_infer_success")
}

#[test]
fn lifetime_error() -> TestResult {
    test_example("lifetime_error")
}

#[test]
fn lifetime_infer_helper_function() -> TestResult {
    test_example("lifetime_infer_helper_function")
}

#[test]
fn lifetime_infer_helper_function2() -> TestResult {
    test_example("lifetime_infer_helper_function2")
}

#[test]
fn loop_() -> TestResult {
    test_example("loop")
}

#[test]
fn loop_with_temporary() -> TestResult {
    test_example("loop_with_temporary")
}

#[test]
fn method_and_struct_allocator() -> TestResult {
    test_example("method_and_struct_allocator")
}

#[test]
fn method_immutable_self_error() -> TestResult {
    test_example("method_immutable_self_error")
}

#[test]
fn method_in_method() -> TestResult {
    test_example("method_in_method")
}

#[test]
fn method_mutation() -> TestResult {
    test_example("method_mutation")
}

#[test]
fn method_simple() -> TestResult {
    test_example("method_simple")
}

#[test]
fn method_simple_error() -> TestResult {
    test_example("method_simple_error")
}

#[test]
fn return_missing_error() -> TestResult {
    test_example("return_missing_error")
}

#[test]
fn return_missing_value_error() -> TestResult {
    test_example("return_missing_value_error")
}

#[test]
fn return_top_level_error() -> TestResult {
    test_example("return_top_level_error")
}

#[test]
fn return_value() -> TestResult {
    test_example("return_value")
}

#[test]
fn scope_struct() -> TestResult {
    test_example("scope_struct")
}

#[test]
fn scope_struct_to_upper_scope() -> TestResult {
    test_example("scope_struct_to_upper_scope")
}
#[test]
fn scope_struct_to_upper_scope2() -> TestResult {
    test_example("scope_struct_to_upper_scope2")
}

#[test]
fn static_method() -> TestResult {
    test_example("static_method")
}

#[test]
fn struct_allocator() -> TestResult {
    test_example("struct_allocator")
}

#[test]
fn struct_arg_count_error() -> TestResult {
    test_example("struct_arg_count_error")
}

#[test]
fn struct_field_access_caller() -> TestResult {
    test_example("struct_field_access_caller")
}

#[test]
fn struct_field_access_caller2() -> TestResult {
    test_example("struct_field_access_caller2")
}

#[test]
fn struct_field_access_locally() -> TestResult {
    test_example("struct_field_access_locally")
}

#[test]
fn struct_field_deep() -> TestResult {
    test_example("struct_field_deep")
}

#[test]
fn struct_field_math_mainless() -> TestResult {
    test_example("struct_field_math_mainless")
}

#[test]
fn struct_field_math() -> TestResult {
    test_example("struct_field_math")
}

#[test]
fn struct_field() -> TestResult {
    test_example("struct_field")
}

#[test]
fn struct_field_helper() -> TestResult {
    test_example("struct_field_helper")
}

#[test]
fn struct_field_mutable_param() -> TestResult {
    test_example("struct_field_mutable_param")
}

#[test]
fn struct_field_update() -> TestResult {
    test_example("struct_field_update")
}

#[test]
fn struct_helper() -> TestResult {
    test_example("struct_helper")
}

#[test]
fn struct_helper_deep() -> TestResult {
    test_example("struct_helper_deep")
}

#[test]
fn struct_in_fun() -> TestResult {
    test_example("struct_in_fun")
}

#[test]
fn struct_in_fun_error() -> TestResult {
    test_example("struct_in_fun_error")
}

#[test]
fn struct_in_struct() -> TestResult {
    test_example("struct_in_struct")
}

#[test]
fn struct_new_field_error() -> TestResult {
    test_example("struct_new_field_error")
}

#[test]
fn struct_raw() -> TestResult {
    test_example("struct_raw")
}

#[test]
fn struct_() -> TestResult {
    test_example("struct")
}

#[test]
fn variable_and_function() -> TestResult {
    test_example("variable_and_function")
}

#[test]
fn variable_bad_optional_type() -> TestResult {
    test_example("variable_bad_optional_type")
}

#[test]
fn variable_mutation_error() -> TestResult {
    test_example("variable_mutation_error")
}

#[test]
fn variable_mutation() -> TestResult {
    test_example("variable_mutation")
}

#[test]
fn variable_mutation2() -> TestResult {
    test_example("variable_mutation2")
}

#[test]
fn variable_mutation3() -> TestResult {
    test_example("variable_mutation3")
}

#[test]
fn variable_simple_error() -> TestResult {
    test_example("variable_simple_error")
}

#[test]
fn variable_simple_error2() -> TestResult {
    test_example("variable_simple_error2")
}

#[test]
fn variable_simple() -> TestResult {
    test_example("variable_simple")
}

#[test]
fn variable() -> TestResult {
    test_example("variable")
}

#[test]
fn unicode() -> TestResult {
    test_example("unicode")
}
