#![feature(splice)]
use std::fs::File;
use std::io::prelude::*;
use std::collections::BTreeSet;

fn get_and_parse(file_name: &str) -> Parsed {
    let mut f = File::open(file_name).expect("file not found");
    let mut content = String::new();
    f.read_to_string(&mut content)
        .expect("something went wrong reading file!");
    parse(&content)
}

type Point = (i64, i64, i64);
type Particle = (Point, Point, Point);
type Parsed = Vec<Particle>;

fn parse(s: &str) -> Parsed {
    let v: Vec<&str> = s.trim().split('\n').collect();
    let mut z = Vec::with_capacity(v.len());
    for line in v {
        z.push(parse_line(line));
    }
    z
}

fn parse_line(s: &str) -> Particle {
    let v: Vec<&str> = s.split(", ").collect();
    (parse_tuple(v[0]), parse_tuple(v[1]), parse_tuple(v[2]))
}

fn parse_tuple(s: &str) -> Point {
    let mut string = String::from(s);
    string.pop();
    let (_, back) = string.split_at(3);
    let v: Vec<&str> = back.split(",").collect();
    (
        v[0].parse().expect("couldn't parse"),
        v[1].parse().expect("couldn't parse"),
        v[2].parse().expect("couldn't parse"),
    )
}

fn adv_part(p: Particle) -> Particle {
    let ((px, py, pz), (vx, vy, vz), (ax, ay, az)) = p;
    (
        (px + vx + ax, py + vy + ay, pz + vz + az),
        (vx + ax, vy + ay, vz + az),
        (ax, ay, az),
    )
}

fn step(v: &mut Parsed) -> &Parsed {
    for i in 0..(v.len()) {
        v[i] = adv_part(v[i]);
    }
    let mut collided = BTreeSet::new();
    for i in 0..(v.len()) {
        for j in (i + 1)..(v.len()) {
            if v[i].0 == v[j].0 {
                collided.insert(i);
                collided.insert(j);
            }
        }
    }
    for i in collided.iter().rev() {
        v.remove(*i);
    }
    v
}

//Pt1 solved by manually looking for the particle with the lowest net acceleration

fn pt2(limit: usize, p: &Parsed) -> usize {
    let mut parsed = p.clone();
    for _ in 1..limit {
        step(&mut parsed);
    }
    parsed.len()
}

fn main() {
    let test_parsed = get_and_parse("test2.txt");
    println!("test parsed: {:?}", test_parsed);
    println!("pt2 test: {}", pt2(3, &test_parsed));

    let parsed = get_and_parse("input.txt");
    //println!("parsed: {:?}", parsed);
    println!("pt2: {}", pt2(1000, &parsed));
}
