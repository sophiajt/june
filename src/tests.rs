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

        let mut test_filepath = PathBuf::from("./examples");
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

        println!("Checking:\n{} against: {}", command_err, expected_error);

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
fn boolean() -> TestResult {
    test_example("boolean")
}

#[test]
fn call_labeled_error() -> TestResult {
    test_example("call_labeled_error")
}

#[test]
fn double() -> TestResult {
    test_example("double")
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
fn return_top_level_error() -> TestResult {
    test_example("return_top_level_error")
}

#[test]
fn return_value() -> TestResult {
    test_example("return_value")
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
fn struct_helper() -> TestResult {
    test_example("struct_helper")
}

#[test]
fn struct_helper_deep() -> TestResult {
    test_example("struct_helper_deep")
}

#[test]
fn struct_in_struct() -> TestResult {
    test_example("struct_in_struct")
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
