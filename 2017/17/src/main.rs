use std::vec::Vec;
use std::usize;

fn spinlock(steps: usize, count: usize) -> (Vec<usize>, usize) {
    let mut v = Vec::with_capacity(count + 1);
    v.push(0);
    let mut p = 0;
    for i in 1..(count + 1) {
        p = (steps + p).wrapping_rem(v.len());
        v.insert(p + 1, i);
        p = p + 1;
    }
    (v, p)
}

fn pt2(steps: usize, count: usize) -> usize {
    let mut len = 1;
    let mut p = 0;
    let mut last_insert = 0;
    for i in 1..(count + 1) {
        p = (steps + p).wrapping_rem(len);
        len = len + 1;
        if p == 0 {
            last_insert = i;
        }
        p = p + 1;
    }
    last_insert
}

fn main() {
    const TEST_INPUT: usize = 3;
    let (test_vector, test_pointer) = spinlock(TEST_INPUT, 2017);
    println!("pt1 test: {}", test_vector[test_pointer + 1]);

    const INPUT: usize = 382;
    let (vector, pointer) = spinlock(INPUT, 2017);
    println!("pt1: {}", vector[pointer + 1]);

    println!("pt2: {}", pt2(INPUT, 50000000));
}
