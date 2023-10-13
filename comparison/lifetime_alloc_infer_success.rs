struct Stats {
    age: i64,
}

struct Employee<'a> {
    name: String,
    stats: &'a Stats,
}

fn helper<'a, 'b>(employee: &'a mut Employee<'b>, stats: &'b Stats) {
    employee.stats = stats;
}

fn main() {
    // Rust doesn't allow this to be created by `helper`, so we have
    // to create it from outside and pass it in
    let stats2 = Stats { age: 33 };

    let stats = Stats { age: 22 };

    let mut employee = Employee {
        name: "Sophia".into(),
        stats: &stats,
    };

    helper(&mut employee, &stats2);

    println!("{}", employee.stats.age);
}
