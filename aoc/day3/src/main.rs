use std::env;
use std::fs;



fn interpret(report: &String) -> (i32, i32) {
    let code_length = match report.lines().next() {
        None => panic!("report in wrong format."),
        Some(code) => code.chars().count() as i32
    };
    let report_length = report.lines().count() as i32;

    let mut counts = vec![0; code_length as usize];
    
    for code in report.lines() {
        for (i, c) in code.chars().enumerate() {
            counts[i] += c.to_digit(10).unwrap() as i32;
        }
    }

    let mut gamma = 0b0;
    for count in counts.iter() {
        gamma = gamma << 1;
        if 2 * count >= report_length {
            gamma += 0b1;
        }
    }

    let epsilon = (2 as i32).pow(code_length as u32) - gamma - 1;
    return (gamma, epsilon);
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let report = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");
    let (gamma, epsilon) = interpret(&report);
    println!("gamma: {}, epsilon: {}, result: {}", gamma, epsilon, gamma * epsilon)
}
