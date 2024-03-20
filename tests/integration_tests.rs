type TestResult = Result<(), Report>;

use color_eyre::{
    eyre::{self, eyre, Report, WrapErr},
    Section, SectionExt,
};
use libtest_mimic::{Arguments, Failed, Trial};
use std::{
    env,
    ffi::OsStr,
    fs, io,
    io::Write,
    path::{Path, PathBuf},
    process::Command,
    sync::Once,
};

fn main() -> eyre::Result<()> {
    let args = Arguments::from_args();
    let tests = collect_tests()?;
    libtest_mimic::run(&args, tests).exit();
}

/// Creates one test for each `.june` file in the current directory or
/// sub-directories of the current directory.
fn collect_tests() -> eyre::Result<Vec<Trial>> {
    fn visit_dir(path: &Path, tests: &mut Vec<Trial>) -> eyre::Result<()> {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let file_type = entry.file_type()?;

            // Handle files
            let path = entry.path();
            if file_type.is_file() {
                if path.extension() == Some(OsStr::new("june")) {
                    let name = path
                        .strip_prefix(env::current_dir()?)?
                        .display()
                        .to_string();

                    if parse_special_comments(&path).is_none() {
                        continue;
                    }

                    let test = Trial::test(name, move || eval_source_runner(&path));
                    tests.push(test);
                }
            } else if file_type.is_dir() {
                // Handle directories
                visit_dir(&path, tests)?;
            }
        }

        Ok(())
    }

    // We recursively look for `.june` files, starting from the current
    // directory.
    let mut tests = Vec::new();
    let current_dir = env::var("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)?
        .join("tests");
    visit_dir(&current_dir, &mut tests)?;

    Ok(tests)
}

/// testrunner adapter for libtest-mimic
pub fn eval_source_runner(fname: &Path) -> Result<(), Failed> {
    match test_example(fname) {
        Ok(()) => Ok(()),
        Err(report) => Err(format!("{report:?}"))?,
    }
}

fn test_example(test_name: &Path) -> TestResult {
    static INIT_REPORTER: Once = Once::new();
    INIT_REPORTER.call_once(|| {
        color_eyre::install().unwrap();
    });

    // Create it if it's not there
    let mut temp_dir = std::env::temp_dir();
    temp_dir.push("june_tests");

    let relative_path = PathBuf::from(test_name);

    let parent = relative_path.parent()
        .expect("all files are found by path so they should exist in a directory we know about and have a valid parent directory");
    let parent = parent.file_name().unwrap();
    temp_dir.push(parent);

    // eprintln!("temp_dir: {:?}", temp_dir);

    let _ = fs::create_dir_all(&temp_dir);

    let test_filepath = {
        let mut test_filename = PathBuf::from(test_name);
        test_filename.set_extension("june");

        let mut test_filepath = PathBuf::from("./tests/integration");
        test_filepath.push(test_filename);

        // eprintln!("test_filepath: {:?}", test_filepath);

        test_filepath
    };

    let config = parse_special_comments(&test_filepath)
        .expect("test should have an \"output:\" or \"error:\" test configuration comment");

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

        // eprintln!("c_output_filepath: {:?}", c_output_filepath);

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
        // println!("Checking:\n{}expected: {}\n", command_err, expected_error);

        assert!(command_err.contains(expected_error));
    }

    let app_output = if config.expected_error.is_none() {
        // Now, output our C to a file
        // eprintln!("c_output_filepath: {:?}", c_output_filepath);
        let mut output_file = fs::File::create(&c_output_filepath).unwrap();
        let _ = output_file.write_all(&output.stdout);

        // Next, compile the file
        let compiler = Command::new("clang")
            .arg(&c_output_filepath)
            .arg("-o")
            .arg(&app_filepath)
            .output()
            .wrap_err("Cannot execute clang")?;

        if !compiler.status.success() {
            let _ = io::stdout().write_all(&compiler.stdout);
            let _ = io::stdout().write_all(&compiler.stderr);
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

fn parse_special_comments(test_filepath: &Path) -> Option<TestConfig> {
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
        return None;
    }

    Some(TestConfig {
        expected_output,
        expected_error,
        log_config,
    })
}
