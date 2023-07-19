use std::{
    error::Error,
    fs::{create_dir, File},
    io::{stdout, Write},
    path::PathBuf,
    process::Command,
};

type TestResult = Result<(), Box<dyn Error>>;

fn test_example(test_name: &str) -> TestResult {
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

    let test_compare_filepath = {
        let mut test_output_filename = PathBuf::from(test_name);
        test_output_filename.set_extension("out");

        let mut test_compare_filepath = PathBuf::from("./examples");
        test_compare_filepath.push(test_output_filename);

        test_compare_filepath
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
        .arg(test_filepath)
        .output();
    let command = command.unwrap();
    if !command.status.success() {
        panic!("June did not compile successfully");
    }

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
    let expected_output = std::fs::read(test_compare_filepath).unwrap();

    let app_output = String::from_utf8_lossy(&app_output);
    let expected_output = String::from_utf8_lossy(&expected_output);

    let app_output = app_output.replace("\r\n", "");
    let app_output = app_output.replace('\n', "");

    let expected_output = expected_output.replace("\r\n", "");
    let expected_output = expected_output.replace('\n', "");

    assert_eq!(expected_output, app_output);

    Ok(())
}

#[test]
fn boolean() -> TestResult {
    test_example("boolean")
}
