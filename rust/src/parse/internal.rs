#![allow(dead_code)]

use std::iter::Step;

#[derive(Debug, Clone, Default)]
pub struct SymbolSet {
    ranges: Vec<SymbolRange>,
}

impl SymbolSet {
    pub fn new() -> Self {
        Self { ranges: Vec::new() }
    }

    pub fn singleton(symbol: char) -> Self {
        Self::range(symbol, Step::forward(symbol, 1))
    }

    pub fn range(start: char, end: char) -> Self {
        assert!(start <= end, "empty range");
        let ranges = vec![SymbolRange(start, Step::forward(end, 1))];
        Self { ranges }
    }

    pub fn contains(&self, symbol: char) -> bool {
        match self.ranges.binary_search_by_key(&symbol, |r| r.0) {
            Ok(_) => true,
            Err(0) => false,
            Err(i) => self
                .ranges
                .get(i - 1)
                .map(|r| r.contains(symbol))
                .unwrap_or(false),
        }
    }

    pub fn intersects(&self, other: &Self) -> bool {
        let (shorter, longer) = if self.ranges.len() < other.ranges.len() {
            (&self.ranges, &other.ranges)
        } else {
            (&other.ranges, &self.ranges)
        };

        for range in shorter {
            match longer.binary_search_by_key(&range.0, |r| r.0) {
                Ok(_) => return true,
                Err(i) => {
                    if let Some(r) = longer.get(i) {
                        if range.intersects(r) {
                            return true;
                        }
                    }

                    if let Some(r) = longer.get(i.saturating_sub(1)) {
                        if range.intersects(r) {
                            return true;
                        }
                    }
                }
            }
        }

        false
    }

    pub fn union(&self, other: &Self) -> Self {
        let ranges = merge_ranges(&self.ranges, &other.ranges);
        Self { ranges }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
struct SymbolRange(char, char);

impl SymbolRange {
    fn contains(&self, c: char) -> bool {
        c >= self.0 && c < self.1
    }

    fn intersects(&self, other: &Self) -> bool {
        self.0 < other.1 && self.1 > other.0
    }

    fn intersects_or_contiguous(&self, other: &Self) -> bool {
        self.0 <= other.1 && self.1 >= other.0
    }

    fn merge(&self, other: &Self) -> Self {
        assert!(self.intersects_or_contiguous(other));
        Self(self.0.min(other.0), self.1.max(other.1))
    }
}

fn push_range(ranges: &mut Vec<SymbolRange>, range: SymbolRange) {
    println!("Pushing range {:?}, last range {:?}", range, ranges.last());
    if let Some(last) = ranges.pop() {
        if last.intersects_or_contiguous(&range) {
            ranges.push(last.merge(&range));
        } else {
            ranges.push(last);
            ranges.push(range);
        }
    } else {
        ranges.push(range);
    }
}

fn merge_ranges(a: &[SymbolRange], b: &[SymbolRange]) -> Vec<SymbolRange> {
    let mut iter_a = a.iter().copied();
    let mut iter_b = b.iter().copied();

    let mut a = iter_a.next();
    let mut b = iter_b.next();
    let mut ranges = Vec::new();

    loop {
        println!("Iteration start: a = {:?}, b = {:?}", a, b);
        match (a, b) {
            (None, None) => break,
            (Some(r), None) | (None, Some(r)) => {
                println!("One iterator exhausted, pushing remaining ranges");
                push_range(&mut ranges, r);
                for a in iter_a {
                    push_range(&mut ranges, a);
                }
                for b in iter_b {
                    push_range(&mut ranges, b);
                }
                break;
            }
            (Some(r_a), Some(r_b)) => {
                if r_a.0 <= r_b.0 {
                    println!("a has lower start");
                    push_range(&mut ranges, r_a);
                    a = iter_a.next();
                } else {
                    println!("b has lower start");
                    push_range(&mut ranges, r_b);
                    b = iter_b.next();
                }
            }
        }
    }

    println!("Iterators exhausted, returning {:?}", ranges);
    ranges
}

#[cfg(test)]
mod test {
    use super::*;

    fn range(start: char, end: char) -> SymbolRange {
        SymbolRange(start, Step::forward(end, 1))
    }

    #[test]
    fn merge() {
        println!("==== MERGING: empty ranges");
        assert_eq!(merge_ranges(&[], &[]), &[]);

        println!("==== MERGING: single range, left");
        assert_eq!(merge_ranges(&[range('a', 'z')], &[]), &[range('a', 'z')]);

        println!("==== MERGING: single range, right");
        assert_eq!(merge_ranges(&[], &[range('a', 'z')]), &[range('a', 'z')]);

        println!("==== MERGING: contiguous ranges, left lower");
        assert_eq!(
            merge_ranges(&[range('a', 'c')], &[range('d', 'z')]),
            &[range('a', 'z')],
        );

        println!("==== MERGING: overlapping ranges, left lower");
        assert_eq!(
            merge_ranges(&[range('a', 'd')], &[range('c', 'z')]),
            &[range('a', 'z')],
        );

        println!("==== MERGING: contiguous ranges, right lower");
        assert_eq!(
            merge_ranges(&[range('f', 'z')], &[range('a', 'e')]),
            &[range('a', 'z')],
        );

        println!("==== MERGING: overlapping ranges, right lower");
        assert_eq!(
            merge_ranges(&[range('f', 'z')], &[range('a', 't')]),
            &[range('a', 'z')],
        );

        println!("==== MERGING: disjoint ranges, left lower");
        assert_eq!(
            merge_ranges(&[range('a', 'g')], &[range('k', 'z')]),
            &[range('a', 'g'), range('k', 'z')],
        );

        println!("==== MERGING: disjoint ranges, right lower");
        assert_eq!(
            merge_ranges(&[range('k', 'z')], &[range('a', 'd')]),
            &[range('a', 'd'), range('k', 'z')],
        );

        println!("==== MERGING: contiguous ranges, right plugs gap");
        assert_eq!(
            merge_ranges(&[range('a', 'd'), range('k', 'z')], &[range('e', 'j')]),
            &[range('a', 'z')],
        );

        println!("==== MERGING: contiguous ranges, left plugs gap");
        assert_eq!(
            merge_ranges(&[range('d', 'f')], &[range('a', 'c'), range('g', 'z')]),
            &[range('a', 'z')],
        );

        println!("==== MERGING: overlapping ranges, gap plugged");
        assert_eq!(
            merge_ranges(&[range('a', 'c'), range('g', 'z')], &[range('c', 't')]),
            &[range('a', 'z')],
        );

        println!("==== MERGING: mixed overlapping and disjoint");
        assert_eq!(
            merge_ranges(
                &[range('a', 'h'), range('q', 'z')],
                &[range('d', 'm'), range('p', 't')],
            ),
            &[range('a', 'm'), range('p', 'z')],
        );
    }

    #[test]
    fn intersects() {
        println!("==== INTERSECTS: single range, equal sets");
        let a = SymbolSet::range('a', 'z');
        assert!(a.intersects(&a));

        println!("==== INTERSECTS: multiple ranges, equal sets");
        let a = SymbolSet::range('a', 'g').union(&SymbolSet::range('t', 'z'));
        assert!(a.intersects(&a));

        println!("==== INTERSECTS: equal range in unequal sets");
        let a = SymbolSet::range('a', 'g');
        let b = SymbolSet::range('k', 'z').union(&a);
        assert!(a.intersects(&b));
        assert!(b.intersects(&a));

        println!("==== INTERSECTS: overlapping range");
        let a = SymbolSet::range('a', 'j');
        let b = SymbolSet::range('g', 'z');
        assert!(a.intersects(&b));
        assert!(b.intersects(&a));

        println!("==== INTERSECTS: overlapping range among many ranges");
        let a = SymbolSet::singleton('f');
        let b = SymbolSet::singleton('a')
            .union(&SymbolSet::range('e', 'g'))
            .union(&SymbolSet::range('l', 'z'));
        assert!(a.intersects(&b));
        assert!(b.intersects(&a));

        println!("==== INTERSECTS: tangential range among many");
        let a = SymbolSet::range('h', 'k');
        let b = SymbolSet::range('a', 'd')
            .union(&SymbolSet::range('k', 'p'))
            .union(&SymbolSet::range('s', 'z'));
        assert!(a.intersects(&b));
        assert!(b.intersects(&a));

        println!("==== DOES NOT INTERSECT: empty sets");
        let a = SymbolSet::new();
        assert!(!a.intersects(&a));

        println!("==== DOES NOT INTERSECT: empty and nonempty sets");
        let a = SymbolSet::new();
        let b = SymbolSet::range('a', 'z');
        assert!(!a.intersects(&b));
        assert!(!b.intersects(&a));

        println!("==== DOES NOT INTERSECT: disjoint single ranges");
        let a = SymbolSet::range('a', 'k');
        let b = SymbolSet::range('q', 'z');
        assert!(!a.intersects(&b));
        assert!(!b.intersects(&a));

        println!("==== DOES NOT INTERSECT: disjoint sets, interleaved contiguous ranges");
        let a = SymbolSet::range('a', 'd').union(&SymbolSet::range('q', 'u'));
        let b = SymbolSet::range('e', 'p').union(&SymbolSet::range('v', 'z'));
        assert!(!a.intersects(&b));
        assert!(!b.intersects(&a));
    }
}
