use std::env;
use std::fs;

enum Rating {
    Oxygen,
    CO2
}

fn parse(report: String) -> (Vec<i32>, usize) {
    let code_length = match report.lines().next() {
        None => panic!("report in wrong format."),
        Some(code) => code.chars().count()
    };
    return (report.lines().map(|line| i32::from_str_radix(line, 2).unwrap()).collect(), code_length);
}

fn count_bits(codes: &Vec<i32>, code_length: usize) -> Vec<i32> {
    let mut counts = vec![0; code_length];
    for code in codes.iter() {
        for i in 0 .. code_length{
            counts[i] += (code >> code_length - i - 1) & 1;
        }
    }
    return counts;
}

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

fn print_code(codes: &Vec<i32>) {
    for c in codes {
        println!("{:b} ", c);
    }
    println!("#####################################")
}

fn compute_rating(rating: Rating, codes: Vec<i32>, code_length: usize) -> i32 {
    let mut position = 0;

    fn filter_codes(codes: &Vec<i32>, position: usize, bit: i32, code_length: usize) -> Vec<i32> {
        return codes.iter().filter(|code| {
            let c = (*code >> (code_length - position - 1));
            println!("position: {}, code: {:b}, shifted: {:b}, result: {:b}, bit: {:b}", position, code, c, 1 & c, bit);
            return c & 1 == bit;
        }).map(|code| *code).collect();
    }

    fn decide_bit(rating: &Rating, codes: &Vec<i32>, counts: &Vec<i32>, position: usize) -> i32 {
        if 2 * counts[position] >= codes.len() as i32{
            return match rating {
                Rating::Oxygen => 1,
                Rating::CO2 => 0
            }
        } else {
            return match rating {
                Rating::Oxygen => 0,
                Rating::CO2 => 1
            }
        }
    
    }

    let mut c = codes;
    print_code(&c);
    while c.len() > 1 {
        let counts = count_bits(&c, code_length);
        let bit = decide_bit(&rating, &c, &counts, position);
        c = filter_codes(&c, position, bit, code_length);
        print_code(&c);
        position += 1;
    }
    return c[0];
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let report = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");
    let (codes, code_length) = parse(report);
    let counts = count_bits(&codes, code_length);
    
    let oxygen_rating = compute_rating(Rating::Oxygen, codes.clone(), code_length);
    let co2_rating = compute_rating(Rating::CO2, codes, code_length);
    println!("Oxygen: {}, CO2: {}, Health: {}", oxygen_rating, co2_rating, oxygen_rating*co2_rating);
}
