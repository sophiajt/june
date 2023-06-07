use errors::SourceError;

mod delta;
mod errors;
mod lexer;
mod parser;

fn main() {
    let debug_output = true;

    for file in std::env::args().skip(1) {
        let contents = std::fs::read_to_string(file).unwrap();
        let mut parser = parser::Parser::new(contents.as_bytes(), 0, 0);
        parser.parse();

        for error in &parser.errors.first() {
            // println!("error: {:?}", error);

            let SourceError { node_id, message } = error;
            {
                let span_start = parser.delta.span_start[node_id.0];
                let span_end = parser.delta.span_end[node_id.0];

                let contents = contents.as_bytes();

                let line_number = contents[0..span_start].split(|x| *x == b'\n').count();

                let mut line_start = span_start;
                while line_start > 0 && contents[line_start] != b'\n' {
                    line_start -= 1;
                }
                line_start += 1;

                let mut line_end = span_end;
                while line_end < contents.len() && contents[line_end] != b'\n' {
                    line_end += 1;
                }

                println!("line: {}", line_number);
                println!(
                    "{}",
                    String::from_utf8_lossy(&contents[line_start..line_end])
                );
                for _ in line_start..span_start {
                    print!(" ");
                }
                for _ in span_start..span_end {
                    print!("-");
                }
                println!(" {}", message);
            }
        }

        if !parser.errors.is_empty() {
            return;
        }

        let result = &parser.delta;

        if debug_output {
            println!();
            println!("parse result:");
            result.print();
        }
    }
}
