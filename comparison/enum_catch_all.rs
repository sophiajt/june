enum State {
    None,
    Some(i64),
    Struct { x: i64, y: i64 },
}

fn main() {
    let x = State::Struct { x: 66, y: 77 };

    match x {
        State::None => println!("None"),
        _ => println!("Some"),
    }
}
