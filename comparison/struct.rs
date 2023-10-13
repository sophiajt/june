struct Employee {
    name: String,
    age: i64,
}

fn main() {
    let employee = Employee {
        name: "Bob".into(),
        age: 123,
    };
    println!("{}", employee.age)
}
