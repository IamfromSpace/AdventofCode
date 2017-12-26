use std::fs::File;
use std::io::prelude::*;
use std::collections::BTreeSet;
use std::collections::BTreeMap;
use std::usize;

type Parsed = BTreeMap<usize, BTreeSet<(usize, usize)>>;

fn get_and_parse(file_name: &str) -> Parsed {
    let mut f = File::open(file_name).expect("file not found");
    let mut content = String::new();
    f.read_to_string(&mut content)
        .expect("something went wrong reading file!");
    parse(&content)
}

fn set_to_index(set: &BTreeSet<(usize, usize)>, count: usize) -> usize {
    // store the count as a bool in the first bit
    let mut r = count & 1;
    for x in 0..count {
        for y in 0..count {
            if set.contains(&(x, y)) {
                r |= 1 << (y * count + x + 1)
            }
        }
    }
    r
}

fn parse(s: &str) -> Parsed {
    let v: Vec<&str> = s.trim().split('\n').collect();
    let mut m = BTreeMap::new();
    for sub in v {
        let (key, val) = parse_line(sub);
        m.insert(key, val);
    }
    m
}

fn parse_line(s: &str) -> (usize, BTreeSet<(usize, usize)>) {
    let v: Vec<&str> = s.trim().split(" => ").collect();
    let (len, from) = parse_set(v[0]);
    let (_, to) = parse_set(v[1]);
    (set_to_index(&from, len), to)
}

fn parse_set(s: &str) -> (usize, BTreeSet<(usize, usize)>) {
    let mut set = BTreeSet::new();
    let v: Vec<&str> = s.trim().split('/').collect();
    for y in 0..v.len() {
        for x in 0..v.len() {
            if v[y].chars().nth(x).unwrap() as char == '#' {
                set.insert((x, v.len() - 1 - y));
            }
        }
    }
    (v.len(), set)
}

fn flip_h(orig: &BTreeSet<(usize, usize)>, size: usize) -> BTreeSet<(usize, usize)> {
    let mut next = BTreeSet::new();
    for &(x, y) in orig {
        next.insert((size - 1 - x, y));
    }
    next
}

fn flip_v(orig: &BTreeSet<(usize, usize)>, size: usize) -> BTreeSet<(usize, usize)> {
    let mut next = BTreeSet::new();
    for &(x, y) in orig {
        next.insert((x, size - 1 - y));
    }
    next
}

fn rot_clock(orig: &BTreeSet<(usize, usize)>, size: usize) -> BTreeSet<(usize, usize)> {
    let mut next = BTreeSet::new();
    for &(x, y) in orig {
        next.insert((y, size - 1 - x));
    }
    next
}

fn split(
    set: &BTreeSet<(usize, usize)>,
    sub_size: usize,
    sub_count: usize,
) -> Vec<BTreeSet<(usize, usize)>> {
    let mut v = Vec::with_capacity(sub_count);
    for _ in 0..(sub_count * sub_count) {
        v.push(BTreeSet::new());
    }
    for &(x, y) in set {
        let i = y.wrapping_div(sub_size) * sub_count + x.wrapping_div(sub_size);
        v.get_mut(i)
            .unwrap()
            .insert((x.wrapping_rem(sub_size), y.wrapping_rem(sub_size)));
    }
    v
}

fn join(
    sets: Vec<&BTreeSet<(usize, usize)>>,
    sub_count: usize,
    sub_size: usize,
) -> BTreeSet<(usize, usize)> {
    let mut next = BTreeSet::new();
    for x in 0..sub_count {
        for y in 0..sub_count {
            for &(sx, sy) in sets[y * sub_count + x] {
                next.insert((sx + x * sub_size, sy + y * sub_size));
            }
        }
    }
    next
}

fn step(
    rules: &Parsed,
    set: &BTreeSet<(usize, usize)>,
    size: usize,
) -> (usize, BTreeSet<(usize, usize)>) {
    let sub_size = if size.wrapping_rem(2) == 0 { 2 } else { 3 };
    let sub_count = size.wrapping_div(sub_size);
    let v = split(set, sub_size, sub_count);
    let mut next_v = Vec::with_capacity(v.len());
    for sub in v {
        let sym_coords = gen_sym_coords(&sub, sub_size);
        let mut next = None;
        for c in sym_coords {
            let next_by_rule = rules.get(&c);
            if next_by_rule != None {
                next = next_by_rule;
            }
        }
        let next = next.expect("Rule lookup failed!");
        next_v.push(next);
    }
    (
        sub_count * (sub_size + 1),
        join(next_v, sub_count, sub_size + 1),
    )
}

fn pt1(rules: &Parsed, it_count: usize) -> usize {
    let (_, start) = parse_set(".#./..#/###");
    let mut set = start;
    let mut size = 3;
    for _ in 0..it_count {
        let (next_size, next_set) = step(&rules, &set, size);
        set = next_set;
        size = next_size;
    }
    set_size(&set)
}

fn set_size<T>(set: &BTreeSet<T>) -> usize {
    let mut r = 0;
    for _ in set {
        r += 1
    }
    r
}

fn gen_sym_coords(orig: &BTreeSet<(usize, usize)>, size: usize) -> Vec<usize> {
    let mut coords = Vec::with_capacity(12);
    let mut o = orig.clone();
    let mut h = flip_h(orig, size);
    let mut v = flip_v(orig, size);
    for _ in 0..4 {
        coords.push(set_to_index(&o, size));
        coords.push(set_to_index(&h, size));
        coords.push(set_to_index(&v, size));
        o = rot_clock(&o, size);
        h = rot_clock(&h, size);
        v = rot_clock(&v, size);
    }
    coords
}

fn main() {
    //let start = parse_set(".#./..#/###");
    //println!("sym_coords: {:?}", gen_sym_coords(&start.1, 3));
    let test_parsed = get_and_parse("test.txt");
    println!("test parsed: {:?}", test_parsed);
    println!("test pt1: {:?}", pt1(&test_parsed, 2));

    let parsed = get_and_parse("input.txt");
    //println!("parsed: {:?}", parsed);
    println!("pt1: {:?}", pt1(&parsed, 5));
    println!("pt1: {:?}", pt1(&parsed, 18));
}
