use core::{
    mem::MaybeUninit,
    ops::{Index, IndexMut, Range},
};

/// A no-std vector type around an array, with push/pop functionality
// Invariants:
// 1. len is always less than CAPACITY
// 2. all elements of array < len are initialized
pub struct StackVec<T, const CAPACITY: usize> {
    len: usize,
    array: [MaybeUninit<T>; CAPACITY],
}

impl<T, const CAPACITY: usize> StackVec<T, CAPACITY> {
    pub fn new() -> Self {
        Self {
            len: 0,
            array: MaybeUninit::uninit().transpose(),
        }
    }
    /// Push `element` onto the vec. Returns None if the CAPACITY was reached.
    pub fn push(&mut self, element: T) -> Option<()> {
        // let compiler insert bounds check
        *self.array.get_mut(self.len)? = MaybeUninit::new(element);
        self.len += 1;

        Some(())
    }
    /// Pop the last element from the vec. Returns None if the vec is empty.
    pub fn pop(&mut self) -> Option<T> {
        if self.is_empty() {
            return None;
        }
        self.len -= 1;

        // SAFETY:
        // according to Invariant 1. self.len is always less than CAPACITY, and never negative due to being an unsigned integer
        let element = unsafe { self.array.get_unchecked_mut(self.len) };

        // SAFETY:
        // `element` is element self.len - 1 and according to Invariant 2. all elements < self.len are initialized
        Some(unsafe { element.assume_init_read() })
    }
}

impl<T, const CAPACITY: usize> Default for StackVec<T, CAPACITY> {
    fn default() -> Self {
        Self::new()
    }
}

pub trait Len {
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
impl<T, const CAPACITY: usize> Len for StackVec<T, CAPACITY> {
    fn len(&self) -> usize {
        self.len
    }
}

/// # SAFETY
/// The implementor has to ensure that any index performed with an `index` for which `in_bounds()` returned true is valid.
unsafe trait BoundsCheck<Idx> {
    fn in_bounds(&self, index: Idx) -> bool;
}
unsafe impl<T> BoundsCheck<usize> for T
where
    T: Len,
{
    fn in_bounds(&self, index: usize) -> bool {
        index < self.len()
    }
}
unsafe impl<T> BoundsCheck<Range<usize>> for T
where
    T: Len,
{
    fn in_bounds(&self, index: Range<usize>) -> bool {
        index.start <= self.len() && index.end <= self.len() && index.start <= index.end
    }
}

pub trait GetUnchecked<Idx>: Len {
    type Output: ?Sized;
    /// Get a reference to the element at `index`, without any bounds checks.
    /// # SAFETY
    /// The caller is responsible for ensuring that `index` is in bounds.
    unsafe fn get_unchecked(&self, index: Idx) -> &Self::Output;
    /// Get a mutable reference to the element at `index`, without any bounds checks.
    /// # SAFETY
    /// The caller is responsible for ensuring, that `index` is in bounds.
    unsafe fn get_unchecked_mut(&mut self, index: Idx) -> &mut Self::Output;
}

impl<T, const CAPACITY: usize> GetUnchecked<usize> for StackVec<T, CAPACITY> {
    type Output = T;
    /// Get a reference to the element at `index`, without any bounds checks.
    /// # SAFETY
    /// The caller is responsible for ensuring that `index` is < self.len()
    unsafe fn get_unchecked(&self, index: usize) -> &T {
        // SAFETY:
        // The caller ensured that index < self.len, and according to Invariant 1. self.len is always less than CAPACITY
        let element = unsafe { self.array.get_unchecked(index) };

        // SAFETY:
        // The caller ensured that index < self.len, and according to Invariant 2. all elements < self.len are initialized
        unsafe { element.assume_init_ref() }
    }
    /// Get a mutable reference to the element at `index`, without any bounds checks.
    /// # SAFETY
    /// The caller is responsible for ensuring that `index` is < self.len()
    unsafe fn get_unchecked_mut(&mut self, index: usize) -> &mut T {
        // SAFETY:
        // The caller ensured that index < self.len, and according to Invariant 1. self.len is always less than CAPACITY
        let element = unsafe { self.array.get_unchecked_mut(index) };

        // SAFETY:
        // The caller ensured that index < self.len, and according to Invariant 2. all elements < self.len are initialized
        unsafe { element.assume_init_mut() }
    }
}

impl<T, const CAPACITY: usize> GetUnchecked<Range<usize>> for StackVec<T, CAPACITY> {
    type Output = [T];
    unsafe fn get_unchecked(&self, index: Range<usize>) -> &Self::Output {
        // SAFETY:
        // The caller ensured that index < self.len, and according to Invariant 1. self.len is always less than CAPACITY
        let element = unsafe { self.array.get_unchecked(index) };

        // SAFETY:
        // The caller ensured that index < self.len, and according to Invariant 2. all elements < self.len are initialized
        unsafe { element.assume_init_ref() }
    }
    unsafe fn get_unchecked_mut(&mut self, index: Range<usize>) -> &mut Self::Output {
        // SAFETY:
        // The caller ensured that index < self.len, and according to Invariant 1. self.len is always less than CAPACITY
        let element = unsafe { self.array.get_unchecked_mut(index) };

        // SAFETY:
        // The caller ensured that index < self.len, and according to Invariant 2. all elements < self.len are initialized
        unsafe { element.assume_init_mut() }
    }
}

trait Get<Idx>: GetUnchecked<Idx> + BoundsCheck<Idx>
where
    Idx: Clone,
{
    /// Get a reference to the element at `index` if `index` is < self.len()
    fn get(&self, index: Idx) -> Option<&Self::Output> {
        // SAFETY:
        // We only run get_unchecked after checking that index < self.len
        (self.in_bounds(index.clone())).then(|| unsafe { self.get_unchecked(index) })
    }
    /// Get a mutable reference to the element at `index` if `index` is < self.len()
    fn get_mut(&mut self, index: Idx) -> Option<&mut Self::Output> {
        // SAFETY:
        // We only run get_unchecked_mut after checking that index < self.len
        (self.in_bounds(index.clone())).then(|| unsafe { self.get_unchecked_mut(index) })
    }
}
impl<T, const CAPACITY: usize> Get<usize> for StackVec<T, CAPACITY> {}
impl<T, const CAPACITY: usize> Get<Range<usize>> for StackVec<T, CAPACITY> {}

impl<T, const CAPACITY: usize, Idx> Index<Idx> for StackVec<T, CAPACITY>
where
    StackVec<T, CAPACITY>: Get<Idx>,
    Idx: Clone,
{
    type Output = <StackVec<T, CAPACITY> as GetUnchecked<Idx>>::Output;
    fn index(&self, index: Idx) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<T, const CAPACITY: usize, Idx> IndexMut<Idx> for StackVec<T, CAPACITY>
where
    StackVec<T, CAPACITY>: Get<Idx>,
    Idx: Clone,
{
    fn index_mut(&mut self, index: Idx) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}

/// This module was originally written by generative AI, but manually revised and vetted.
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_is_empty() {
        let mut sv: StackVec<i32, 4> = StackVec::new();
        assert_eq!(sv.len(), 0);
        assert!(sv.is_empty());
        assert!(sv.pop().is_none());
    }

    #[test]
    fn push_pop_basic() {
        let mut sv: StackVec<i32, 4> = StackVec::new();
        assert_eq!(sv.push(10), Some(()));
        assert_eq!(sv.push(20), Some(()));
        assert_eq!(sv.len(), 2);

        assert_eq!(sv.pop(), Some(20));
        assert_eq!(sv.pop(), Some(10));
        assert!(sv.pop().is_none());
    }

    #[test]
    fn push_until_capacity() {
        let mut sv: StackVec<i32, 2> = StackVec::new();
        assert_eq!(sv.push(1), Some(()));
        assert_eq!(sv.push(2), Some(()));
        // capacity reached
        assert_eq!(sv.push(3), None);
        assert_eq!(sv.len(), 2);
    }

    #[test]
    fn index_and_index_mut() {
        let mut sv: StackVec<i32, 4> = StackVec::new();
        sv.push(5).unwrap();
        assert_eq!(sv[0], 5);

        {
            let r: &mut i32 = &mut sv[0];
            *r = 42;
        }
        assert_eq!(sv[0], 42);
    }

    #[test]
    fn range_index_and_empty_range() {
        let mut sv: StackVec<i32, 4> = StackVec::new();
        sv.push(1).unwrap();
        sv.push(2).unwrap();
        sv.push(3).unwrap();

        let slice: &[i32] = &sv[0..2];
        assert_eq!(slice, &[1, 2]);

        // empty slice at end should be allowed
        let empty: &[i32] = &sv[3..3];
        assert_eq!(empty.len(), 0);
    }

    #[test]
    #[expect(clippy::reversed_empty_ranges)]
    fn get_none_on_invalid_indices_and_ranges() {
        let mut sv: StackVec<i32, 4> = StackVec::new();
        sv.push(1).unwrap();
        sv.push(2).unwrap();

        // usize out of bounds
        assert!(sv.get(10usize).is_none());

        // inverted range
        assert!(sv.get(2..1).is_none());

        // end out of bounds
        assert!(sv.get(3..5).is_none());
    }

    #[test]
    #[should_panic]
    fn index_out_of_bounds_panics() {
        let sv: StackVec<i32, 4> = StackVec::new();
        // Index implementation unwraps Option returned by get -> panic expected
        let _ = sv[0];
    }

    #[test]
    fn get_mut_modifies_element() {
        let mut sv: StackVec<i32, 4> = StackVec::new();
        sv.push(7).unwrap();
        {
            let r = sv.get_mut(0).unwrap();
            *r = 99;
        }
        assert_eq!(sv[0], 99);
    }

    #[test]
    #[expect(clippy::reversed_empty_ranges)]
    fn range_edge_cases_for_in_bounds() {
        let mut sv: StackVec<i32, 4> = StackVec::new();
        sv.push(10).unwrap();
        sv.push(20).unwrap();

        // valid full range up to len() (exclusive end)
        let len = sv.len();
        assert!(sv.in_bounds(0..len));
        // start == end == len is allowed (empty)
        assert!(sv.in_bounds(len..len));
        // start > end is invalid
        assert!(!sv.in_bounds(2..1));
        // end > len invalid
        assert!(!sv.in_bounds(0..(len + 1)));
    }
}
