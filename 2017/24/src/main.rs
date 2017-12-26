#![feature(slice_patterns, advanced_slice_patterns)]
use std::fs::File;
use std::io::prelude::*;
use std::collections::BTreeSet;
use std::collections::LinkedList;

fn get_and_parse(file_name: &str) -> Parsed {
    let mut f = File::open(file_name).expect("file not found");
    let mut content = String::new();
    f.read_to_string(&mut content)
        .expect("something went wrong reading file!");
    parse(&content)
}

type Parsed = BTreeSet<(usize, usize)>;

fn parse(s: &str) -> Parsed {
    let v: Vec<&str> = s.trim().split('\n').collect();
    let mut set: BTreeSet<(usize, usize)> = BTreeSet::new();
    for sub_raw in v {
        let sub: Vec<&str> = sub_raw.split('/').collect();
        let sub0: Result<usize, _> = sub[0].parse();
        let sub1: Result<usize, _> = sub[1].parse();
        let x = sub0.expect("couldn't parse input port") as usize;
        let y = sub1.expect("couldn't parse output port") as usize;
        set.insert((x, y));
    }
    set
}

fn find_continuations<'a>(
    output: usize,
    chain: &LinkedList<(usize, usize)>,
    set: &Parsed,
) -> Vec<(usize, LinkedList<(usize, usize)>, Parsed)> {
    let mut v = Vec::new();
    for &(i, o) in set {
        if output == i {
            let mut next_set = set.clone();
            next_set.remove(&(i, o));
            let mut next_list = chain.clone();
            next_list.push_front((i, o));
            v.push((o, next_list, next_set));
        }
        if output == o {
            let mut next_set = set.clone();
            next_set.remove(&(i, o));
            let mut next_list = chain.clone();
            next_list.push_front((i, o));
            v.push((i, next_list, next_set));
        }
    }
    v
}

fn score(list: &LinkedList<(usize, usize)>) -> usize {
    let mut t = 0;
    for &(x, y) in list {
        t += x;
        t += y;
    }
    t
}

fn go(v: Vec<(usize, LinkedList<(usize, usize)>, Parsed)>) -> usize {
    let mut max = 0;
    for (o, list, set) in v {
        let next = find_continuations(o, &list, &set);
        let largest = if next.len() == 0 {
            score(&list)
        } else {
            go(next)
        };
        if largest > max {
            max = largest
        };
    }
    max
}

fn pt1(set: Parsed) -> usize {
    go(vec![(0, LinkedList::new(), set)])
}

fn go2(v: Vec<(usize, LinkedList<(usize, usize)>, Parsed)>) -> (usize, usize) {
    let mut strength = 0;
    let mut length = 0;
    for (o, list, set) in v {
        let next = find_continuations(o, &list, &set);
        let largest = if next.len() == 0 {
            (score(&list), list.len())
        } else {
            go2(next)
        };
        if largest.1 > length {
            strength = largest.0;
            length = largest.1;
        };
        if largest.1 == length && largest.0 > strength {
            strength = largest.0
        }
    }
    (strength, length)
}

fn pt2(set: Parsed) -> (usize, usize) {
    go2(vec![(0, LinkedList::new(), set)])
}

fn main() {
    let test_parsed = get_and_parse("test.txt");
    println!("test parsed: {:?}", test_parsed);
    println!("sturdiest test: {}", pt1(test_parsed.clone()));
    println!("longest then sturdiest test: {:?}", pt2(test_parsed));

    let parsed = get_and_parse("input.txt");
    println!("parsed: {:?}", parsed);
    println!("sturdiest: {}", pt1(parsed.clone()));
    println!("longest then sturdiest : {:?}", pt2(parsed));
}
