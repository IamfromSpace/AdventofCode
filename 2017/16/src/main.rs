#![feature(slice_rotate)]
#![feature(match_default_bindings)]
use std::fs::File;
use std::io::prelude::*;
use std::string::String;
use std::str;
use std::u32;
use std::fmt;
use std::vec::Vec;

#[derive(Clone, Debug)]
enum Inst {
    Spin(u32),
    Exchange(u32, u32),
    Partner(u32, u32),
}

#[derive(Debug, Clone)]
struct PermSet {
    by_index: Vec<u32>,
    by_value: Vec<u32>,
}

impl PermSet {
    fn new(size: usize) -> PermSet {
        let mut by_index = Vec::with_capacity(size);
        let mut by_value = Vec::with_capacity(size);
        for i in 0..size {
            by_index.push(i as u32);
            by_value.push(i as u32);
        }
        PermSet { by_index, by_value }
    }

    fn apply(&mut self, inst: &Inst) -> &PermSet {
        match inst {
            Inst::Spin(i) => {
                let mid = self.by_index.len() - *i as usize;
                self.by_index.rotate(mid);
                self
            }
            Inst::Exchange(a, b) => {
                self.by_index.swap(*a as usize, *b as usize);
                self
            }
            Inst::Partner(a, b) => {
                self.by_value.swap(*a as usize, *b as usize);
                self
            }
        }
    }

    fn apply_vec(&mut self, insts: &Vec<Inst>) -> &PermSet {
        for inst in insts {
            self.apply(&inst);
        }
        self
    }

    fn to_insts(&self) -> Vec<Inst> {
        let size = self.by_index.len();
        let mut v = Vec::with_capacity(size * 2);
        let mut by_index = Vec::new();
        let mut by_value = Vec::new();
        for i in 0..size {
            by_index.push(i as u32);
            by_value.push(i as u32);
        }
        for i in 0..size {
            let current_value = by_index[i];
            let expected_value = self.by_index[i];
            if current_value != expected_value {
                let mut swap_index = None;
                for j in (i + 1)..size {
                    if by_index[j] == expected_value {
                        swap_index = Some(j as u32);
                        break;
                    }
                }
                let swap_index = swap_index.expect("swap index lookup failed! (by_index)");
                by_index.swap(i, swap_index as usize);
                v.push(Inst::Exchange(i as u32, swap_index));
            }
        }

        for i in 0..size {
            let current_value = by_value[i];
            let expected_value = self.by_value[i];
            if current_value != expected_value {
                let mut swap_index = None;
                for j in (i + 1)..size {
                    if by_value[j] == expected_value {
                        swap_index = Some(j as u32);
                        break;
                    }
                }
                let swap_index = swap_index.expect("swap index lookup failed! (by_value)");
                by_value.swap(i, swap_index as usize);
                v.push(Inst::Partner(i as u32, swap_index));
            }
        }
        v
    }
}

fn inst_by_power_two(original_insts: &Vec<Inst>, depth: usize, size: u32) -> Vec<Vec<Inst>> {
    let mut v = Vec::with_capacity(depth);
    let mut perm_set = PermSet::new(size as usize);
    perm_set.apply_vec(&original_insts);
    for i in 0..depth {
        v.push(perm_set.to_insts().clone());
        perm_set.apply_vec(&v[i]);
    }
    v
}

fn run_power_of_two(original_insts: &Vec<Inst>, its: u32, depth: usize, size: u32) -> Vec<Inst> {
    let v = inst_by_power_two(original_insts, depth, size);
    let mut perm_set = PermSet::new(size as usize);
    for i in 0..depth {
        if (its >> i) & 1 == 1 {
            perm_set.apply_vec(&v[i]);
        }
    }
    perm_set.to_insts()
}

#[derive(Debug)]
struct DanceFloor {
    val: Vec<u32>,
}

impl DanceFloor {
    fn new(size: usize) -> DanceFloor {
        let mut val = Vec::with_capacity(size);
        for i in 0..size {
            val.push(i as u32);
        }
        DanceFloor { val }
    }
    fn dance_move(&mut self, inst: &Inst) -> &DanceFloor {
        match inst {
            Inst::Spin(i) => self.spin(*i),
            Inst::Exchange(a, b) => self.exchange(*a, *b),
            Inst::Partner(a, b) => self.partner(*a, *b),
        }
    }
    fn spin(&mut self, amount: u32) -> &DanceFloor {
        let mid = self.val.len() as u32 - amount;
        self.val.rotate(mid as usize);
        self
    }
    fn exchange(&mut self, a: u32, b: u32) -> &DanceFloor {
        let ai = a.wrapping_rem(self.val.len() as u32) as usize;
        let bi = b.wrapping_rem(self.val.len() as u32) as usize;
        let tmp = self.val[ai];
        self.val[ai] = self.val[bi];
        self.val[bi] = tmp;
        self
    }
    fn partner(&mut self, a: u32, b: u32) -> &DanceFloor {
        let mut ai = Option::None;
        let mut bi = Option::None;
        for i in 0..self.val.len() {
            if self.val[i as usize] == a {
                ai = Option::Some(i);
            }
            if self.val[i as usize] == b {
                bi = Option::Some(i);
            }
        }
        let ai = ai.expect("could not find the index") as usize;
        let bi = bi.expect("could not find the index") as usize;
        let tmp = self.val[ai];
        self.val[ai] = self.val[bi];
        self.val[bi] = tmp;
        self
    }
    fn dance(&mut self, insts: &Vec<Inst>, count: u32) -> &DanceFloor {
        for _ in 0..count {
            for inst in insts {
                self.dance_move(&inst);
            }
        }
        self
    }
}
impl fmt::Display for DanceFloor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut cs = Vec::with_capacity(self.val.len());
        for v in &self.val {
            cs.push(*v as u8 + 97);
        }
        let s = str::from_utf8(&cs).expect("couldn't convert to string");
        write!(f, "{}", s)
    }
}

fn parse(s: &str) -> Vec<Inst> {
    let as_list: Vec<&str> = s.split(",").collect();
    let mut v = Vec::new();
    for x in as_list {
        let mut front = String::from(x);
        let back = front.split_off(1);
        v.push(match front.as_ref() {
            "s" => Inst::Spin(back.parse().expect("couldn't parse spin")),
            "x" => {
                let v: Vec<&str> = back.split("/").collect();
                Inst::Exchange(
                    v[0].parse().expect("couldn't parse exchange"),
                    v[1].parse().expect("couldn't parse exchange"),
                )
            }
            "p" => {
                let v: Vec<&str> = x.split("/").collect();
                Inst::Partner(
                    String::from(v[0]).pop().expect("couldn't parse partner") as u32 - 97,
                    String::from(v[1]).pop().expect("couldn't parse partner") as u32 - 97,
                )
            }
            _ => panic!("unexpected starting arg!"),
        })
    }
    v
}

fn main() {
    const TEST_INPUT: &str = "s1,x3/4,pe/b";
    let test_parsed = parse(TEST_INPUT);
    println!("test parsed: {:?}", test_parsed);
    println!(
        "test result 1: {}",
        DanceFloor::new(5).dance(&test_parsed, 4)
    );
    let mut test_simplified = PermSet::new(5);
    test_simplified.apply_vec(&test_parsed);
    println!("test simplified {:?}", test_simplified);
    println!(
        "test with simplification; once: {}",
        DanceFloor::new(5).dance(&test_simplified.to_insts(), 1)
    );
    test_simplified.apply_vec(&test_parsed);
    println!("test simplified {:?}", test_simplified);
    println!(
        "test with simplification; twice: {}",
        DanceFloor::new(5).dance(&test_simplified.to_insts(), 1)
    );
    let insts = run_power_of_two(&test_parsed, 1001, 30, 5);
    println!(
        "test result 2: {}, expect: {}",
        DanceFloor::new(5).dance(&insts, 1),
        DanceFloor::new(5).dance(&test_parsed, 1001)
    );
    /*
    println!(
        "test result 2: {}",
        DanceFloor::new(5).dance(&test_parsed, 1_000_000)
    );
    */

    let mut f = File::open("input.txt").expect("file not found");
    let mut input = String::new();
    f.read_to_string(&mut input)
        .expect("something went wrong reading file");
    let input = input.trim();
    let parsed = parse(&input);

    println!("result: {}", DanceFloor::new(16).dance(&parsed, 1));
    let insts = run_power_of_two(&parsed, 1_000_000_000, 31, 16);
    println!("result 2: {}", DanceFloor::new(16).dance(&insts, 1));
}
