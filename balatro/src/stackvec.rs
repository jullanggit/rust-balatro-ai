use core::{
    mem::MaybeUninit,
    ops::{Deref, DerefMut, Index, IndexMut, Range},
    ptr,
};

/// A no-std vector type around an array, with push/pop functionality
// Invariants:
// 1. len is always less than CAPACITY
// 2. all elements of array < len are initialized
#[derive(Debug)]
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
    #[must_use = "Should handle error"]
    pub fn push(&mut self, element: T) -> Option<()> {
        // let compiler insert bounds check
        *self.array.get_mut(self.len)? = MaybeUninit::new(element);
        self.len += 1;

        Some(())
    }
    /// Pop the last element from the vec. Returns None if the vec is empty.
    #[must_use = "Should handle error"]
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
    /// Removes and returns the element at `index`, shifting all subsequent elements to the left. Returns none if index is out of bounds.
    #[must_use = "Should handle error"]
    pub fn remove(&mut self, index: usize) -> Option<T> {
        if !self.in_bounds(index) {
            return None;
        }
        let base = self.array.as_mut_ptr();

        // SAFETY
        // - `removed` is a &mut T and is thus properly aligned and initialized
        // - we will overwrite the location in the next step, so it will not be double-dropped
        let value = unsafe { ptr::read(base.add(index) as *mut T) };

        // shift everything down
        unsafe {
            ptr::copy(base.add(index + 1), base.add(index), self.len - index - 1);
        };

        self.len -= 1;

        Some(value)
    }
    /// Moves the element at index `current` to index `new`, keeping all other element in order. Returns None if `current` is the same as `new` or either of them are out of bounds.
    pub fn relocate(&mut self, current: usize, new: usize) -> Option<()> {
        if current == new || !self.in_bounds(current) || !self.in_bounds(new) {
            return None;
        };

        let base = self.array.as_mut_ptr();

        // SAFETY
        // - `removed` is a &mut T and is thus properly aligned and initialized
        // - we will overwrite the location in the next step, so it will not be double-dropped
        let value = unsafe { (base.add(current) as *mut T).read() };

        // shift down
        if current < new {
            unsafe { ptr::copy(base.add(current + 1), base.add(current), new - current) };
        // shift up
        } else {
            unsafe { ptr::copy(base.add(new), base.add(new + 1), current - new) };
        }

        self.array[new] = MaybeUninit::new(value);

        Some(())
    }
}
impl<T, const CAPACITY: usize> PartialEq for StackVec<T, CAPACITY>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.deref() == other.deref()
    }
}

impl<T, const CAPACITY: usize> Default for StackVec<T, CAPACITY> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, const CAPACITY: usize> Clone for StackVec<T, CAPACITY>
where
    T: Copy,
{
    fn clone(&self) -> Self {
        Self {
            len: self.len,
            array: self.array,
        }
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

impl<T, const CAPACITY: usize> Deref for StackVec<T, CAPACITY> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        // SAFETY
        // According to Invariant 1. self.len is always in bounds
        unsafe { self.get_unchecked(0..self.len) }
    }
}
impl<T, const CAPACITY: usize> DerefMut for StackVec<T, CAPACITY> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY
        // According to Invariant 1. self.len is always in bounds
        unsafe { self.get_unchecked_mut(0..self.len) }
    }
}

impl<T, const CAPACITY: usize> FromIterator<T> for StackVec<T, CAPACITY> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut out = Self::new();

        for element in iter {
            if out.push(element).is_none() {
                break;
            };
        }

        out
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

    #[test]
    fn remove_middle_shifts_and_len_decreases() {
        let mut sv: StackVec<i32, 8> = StackVec::new();
        sv.push(10);
        sv.push(20);
        sv.push(30);
        sv.push(40);
        assert_eq!(sv.len(), 4);

        // remove element "20" at index 1
        assert_eq!(sv.remove(1), Some(20));

        assert_eq!(sv.len(), 3);
        assert_eq!(sv[0], 10);
        assert_eq!(sv[1], 30);
        assert_eq!(sv[2], 40);
    }

    #[test]
    fn remove_last_is_ok_and_no_shift_needed() {
        let mut sv: StackVec<i32, 8> = StackVec::new();
        sv.push(1);
        sv.push(2);
        sv.push(3);
        assert_eq!(sv.len(), 3);

        // remove last (index 2)
        assert_eq!(sv.remove(2), Some(3));

        assert_eq!(sv.len(), 2);
        assert_eq!(sv[0], 1);
        assert_eq!(sv[1], 2);

        // removing last repeatedly empties the vec
        assert_eq!(sv.remove(1), Some(2));
        assert_eq!(sv.remove(0), Some(1));
        assert!(sv.is_empty());
        assert!(sv.pop().is_none());
    }

    #[test]
    fn remove_first_until_empty() {
        let mut sv: StackVec<i32, 8> = StackVec::new();
        for i in 1..=5 {
            sv.push(i).unwrap();
        }
        assert_eq!(sv.len(), 5);

        // remove index 0 repeatedly; remaining elements shift left each time
        assert_eq!(sv.remove(0), Some(1));

        assert_eq!(sv.remove(0), Some(2));

        assert_eq!(sv.remove(0), Some(3));

        assert_eq!(sv.remove(0), Some(4));

        assert_eq!(sv.remove(0), Some(5));
        assert!(sv.is_empty());
    }

    #[test]
    fn remove_out_of_bounds_and_at_len_returns_none_and_no_change() {
        let mut sv: StackVec<i32, 4> = StackVec::new();
        sv.push(7).unwrap();
        sv.push(8).unwrap();
        let before = sv.clone();

        // index == len is out of bounds for removal; should return None (no panic)
        let len = sv.len();
        assert_eq!(sv.remove(len), None);

        // > len is also out of bounds
        assert_eq!(sv.remove(len + 1), None);

        // state unchanged
        assert_eq!(sv.len(), before.len());
        assert_eq!(sv[0], before[0]);
        assert_eq!(sv[1], before[1]);
    }

    #[test]
    fn remove_on_empty_is_none() {
        let mut sv: StackVec<i32, 0> = StackVec::new();
        assert_eq!(sv.len(), 0);
        assert_eq!(sv.remove(0), None);

        let mut sv2: StackVec<i32, 4> = StackVec::new();
        assert_eq!(sv2.remove(0), None);
    }

    #[test]
    fn remove_preserves_invariants() {
        let mut sv: StackVec<i32, 8> = StackVec::new();
        sv.push(1);
        sv.push(2);
        sv.push(3);
        sv.push(4);
        assert_eq!(sv.remove(1), Some(2));
        assert_eq!(sv.len(), 3);

        // in-bounds indexing still works
        assert_eq!(sv[0], 1);
        assert_eq!(sv[1], 3);
        assert_eq!(sv[2], 4);

        // out-of-bounds access remains guarded via .get()
        assert!(sv.get(3usize).is_none());
        assert!(sv.get(3..4).is_none());
    }
    #[test]
    fn relocate_forward_and_backward() {
        let mut sv: StackVec<i32, 8> = StackVec::new();
        for i in 0..5 {
            sv.push(i).unwrap(); // [0,1,2,3,4]
        }

        // move 1 (index 1) to after 3 (index 3)
        assert_eq!(sv.relocate(1, 3), Some(()));
        assert_eq!(sv.deref(), &[0, 2, 3, 1, 4]);

        // move 3 (index 2) back to index 0
        assert_eq!(sv.relocate(2, 0), Some(()));
        assert_eq!(sv.deref(), &[3, 0, 2, 1, 4]);
    }

    #[test]
    fn relocate_first_to_last_and_last_to_first() {
        let mut sv: StackVec<i32, 8> = StackVec::new();
        for i in 1..=4 {
            sv.push(i).unwrap(); // [1,2,3,4]
        }

        // move first to last
        assert_eq!(sv.relocate(0, 3), Some(()));
        assert_eq!(sv.deref(), &[2, 3, 4, 1]);

        // move last back to first
        assert_eq!(sv.relocate(3, 0), Some(()));
        assert_eq!(sv.deref(), &[1, 2, 3, 4]);
    }

    #[test]
    fn relocate_invalid_indices_and_noop() {
        let mut sv: StackVec<i32, 4> = StackVec::new();
        sv.push(10).unwrap();
        sv.push(20).unwrap();

        // noop when current == new
        assert_eq!(sv.relocate(1, 1), None);
        assert_eq!(sv.deref(), &[10, 20]);

        // out of bounds
        assert_eq!(sv.relocate(0, 5), None);
        assert_eq!(sv.relocate(5, 0), None);
    }

    #[test]
    fn relocate_preserves_len_and_contents() {
        let mut sv: StackVec<i32, 8> = StackVec::new();
        for i in 0..6 {
            sv.push(i).unwrap();
        }
        let len_before = sv.len();
        let mut elems_before: StackVec<_, 8> = sv.clone();

        // perform a bunch of relocations
        sv.relocate(0, 5);
        sv.relocate(4, 1);
        sv.relocate(2, 3);

        // len unchanged
        assert_eq!(sv.len(), len_before);

        // still the same set of elements, just permuted
        let mut elems_after: StackVec<_, _> = sv.clone();
        elems_before.sort();
        elems_after.sort();
        assert_eq!(elems_before, elems_after);
    }
}
