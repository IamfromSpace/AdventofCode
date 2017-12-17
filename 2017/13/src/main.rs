use std::collections::BTreeMap;
use std::fs::File;
use std::io::prelude::*;
use std::string::String;

fn parse(s: &str) -> BTreeMap<u32, u32> {
    let as_list: Vec<&str> = s.split("\n").collect();
    let mut m = BTreeMap::new();
    for x in as_list {
        let v: Vec<&str> = x.split(": ").collect();
        m.insert(
            v[0].parse().expect("couldn't parse"),
            v[1].parse().expect("couldn't parse"),
        );
    }
    let m = m;
    m
}

fn severity(m: &BTreeMap<u32, u32>) -> u32 {
    let mut acc = 0;
    for (depth, range) in m {
        if depth % ((range - 1) * 2) == 0 {
            acc += depth * range;
        }
    }
    let acc = acc;
    acc
}

fn caught(m: &BTreeMap<u32, u32>, delay: u32) -> bool {
    let mut c = false;
    for (depth, range) in m {
        if (depth + delay) % ((range - 1) * 2) == 0 {
            c = true;
            break;
        }
    }
    c
}

fn find_delay(m: &BTreeMap<u32, u32>, search_limit: u32) -> u32 {
    let mut delay = 0;
    loop {
        if delay == search_limit {
            panic!("delay exeeded {}!", search_limit)
        }
        if !caught(&m, delay) {
            break;
        }

        delay += 1;
    }
    delay
}

fn main() {
    const TEST_INPUT: &str = "0: 3\n1: 2\n4: 4\n6: 4";
    let test_parsed = parse(TEST_INPUT);
    println!("parsed: {:?}", test_parsed);
    println!(
        "test severity: {0:?}, should be {1}",
        severity(&test_parsed),
        24
    );
    println!(
        "delay by {} to win test input",
        find_delay(&test_parsed, 11)
    );

    let mut f = File::open("input.txt").expect("file not found");
    let mut input = String::new();
    f.read_to_string(&mut input)
        .expect("something went wrong reading file");
    let input = input.trim();

    let parsed = parse(&input);
    println!("parsed: {:?}", parsed);
    println!("test severity: {:?}", severity(&parsed));
    println!("delay by {} to win input", find_delay(&parsed, 10000000));
}
