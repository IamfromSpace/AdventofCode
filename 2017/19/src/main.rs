use std::fs::File;
use std::io::prelude::*;
use std::string::String;
use std::str;
use std::collections::BTreeMap;

#[derive(Clone, Debug)]
enum Inst {
    Continue(Option<char>),
    Turn,
}

fn parse(s: &str) -> BTreeMap<(usize, usize), Inst> {
    let mut path = BTreeMap::new();
    let v: Vec<&str> = s.split('\n').collect();
    for y in 0..(v.len()) {
        parse_line(v.len() - y, v[y], &mut path);
    }
    path
}

fn parse_line<'a>(
    y: usize,
    s: &str,
    path: &'a mut BTreeMap<(usize, usize), Inst>,
) -> &'a BTreeMap<(usize, usize), Inst> {
    let v = s.as_bytes();
    for x in 0..v.len() {
        match v[x] as char {
            '+' => path.insert((x, y), Inst::Turn),
            '|' => path.insert((x, y), Inst::Continue(None)),
            '-' => path.insert((x, y), Inst::Continue(None)),
            ' ' => None,
            _ => path.insert((x, y), Inst::Continue(Some(v[x] as char))),
        };
    }
    path
}

fn get_next_pos(pos: (usize, usize), heading: usize) -> (usize, usize) {
    let (x, y) = pos;
    match heading {
        0 => (x + 1, y),
        1 => (x, y + 1),
        2 => (x - 1, y),
        _ => (x, y - 1),
    }
}

fn step(
    pos: (usize, usize),
    heading: usize,
    path: &BTreeMap<(usize, usize), Inst>,
) -> Option<((usize, usize), usize, Option<char>)> {
    match path.get(&pos) {
        Some(inst) => match *inst {
            Inst::Turn => {
                let left = (heading + 1).wrapping_rem(4);
                let right = (heading + 3).wrapping_rem(4);
                match path.get(&get_next_pos(pos, left)) {
                    Some(_) => Some((get_next_pos(pos, left), left, None)),
                    None => Some((get_next_pos(pos, right), right, None)),
                }
            }
            Inst::Continue(mc) => Some((get_next_pos(pos, heading), heading, mc.clone())),
        },
        None => None,
    }
}

fn traverse(start: (usize, usize), path: &BTreeMap<(usize, usize), Inst>) -> (String, usize) {
    let mut steps = 0;
    let mut going = true;
    let mut pointer = start.clone();
    let mut heading = 3;
    let mut s = String::from("");
    while going {
        match step(pointer, heading, &path) {
            Some((p, h, mc)) => {
                steps = steps + 1;
                pointer = p;
                heading = h;
                match mc {
                    Some(c) => s.push(c),
                    None => {}
                }
            }
            None => going = false,
        }
    }
    (s, steps)
}

fn main() {
    let mut f = File::open("test.txt").expect("file not found");
    let mut test_input = String::new();
    f.read_to_string(&mut test_input)
        .expect("something went wrong reading file");
    let test_parsed = parse(&test_input);
    //println!("test parsed: {:?}", test_parsed);
    println!("pt1 test: {:?}", traverse((5, 7), &test_parsed));

    let mut f = File::open("input.txt").expect("file not found");
    let mut input = String::new();
    f.read_to_string(&mut input)
        .expect("something went wrong reading file");
    let parsed = parse(&input);
    //println!("parsed: {:?}", parsed);
    println!("pt1: {:?}", traverse((115, 202), &parsed));
}
