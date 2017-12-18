use std::u64;

fn checkMatch(a: u32, b: u32) -> bool {
    let base: u32 = 2;
    let mask = base.pow(16) - 1;
    let a_masked = a & mask;
    let b_masked = b & mask;
    //println!("{:32b}\n{:32b}\n\n", a, b);
    a_masked == b_masked
}

fn step(current: u32, factor: u32) -> u32 {
    let c = current as u64;
    let f = factor as u64;
    let base: u64 = c.wrapping_mul(f);
    let r = base.wrapping_rem(2147483647);
    r as u32
}

fn step2(current: u32, factor: u32, multiple: u32) -> u32 {
    let mut stepped = current;
    loop {
        stepped = step(stepped, factor);
        if stepped % multiple == 0 {
            break;
        }
    }
    stepped
}

fn countMatches(a_start: u32, b_start: u32, a_factor: u32, b_factor: u32, max: u32) -> u32 {
    let mut count = 0;
    let mut a = a_start;
    let mut b = b_start;
    for i in 0..max {
        a = step(a, a_factor);
        b = step(b, b_factor);
        if checkMatch(a, b) {
            count += 1;
        }
    }
    count
}

fn countMatches2(a_start: u32, b_start: u32, a_factor: u32, b_factor: u32, max: u32) -> u32 {
    let mut count = 0;
    let mut a = a_start;
    let mut b = b_start;
    for i in 0..max {
        a = step2(a, a_factor, 4);
        b = step2(b, b_factor, 8);
        if checkMatch(a, b) {
            count += 1;
        }
    }
    count
}

fn main() {
    //let runs = 5;
    let runs = 40000000;
    let runs2 = 5000000;
    let a_start = 618;
    let b_start = 814;

    let a_factor = 16807;
    let b_factor = 48271;

    println!("part 1");
    println!(
        "test with 5: {}, should be 1",
        countMatches(65, 8921, a_factor, b_factor, 5)
    );
    println!(
        "test with runs: {}, should be 588",
        countMatches(65, 8921, a_factor, b_factor, runs)
    );
    println!(
        "final: {}",
        countMatches(a_start, b_start, a_factor, b_factor, runs)
    );

    println!("part 2");
    println!(
        "test with 1056: {}, should be 1",
        countMatches2(65, 8921, a_factor, b_factor, 1056)
    );
    println!(
        "test with runs: {}, should be 309",
        countMatches2(65, 8921, a_factor, b_factor, runs2)
    );
    println!(
        "final: {}",
        countMatches2(a_start, b_start, a_factor, b_factor, runs2)
    );
}
