use std::marker::PhantomData;
use std::ops;

#[derive(Debug, Clone, Default)]
pub struct IndexVec<I, V> {
    values: Vec<V>,
    _phantom: PhantomData<I>,
}

impl<I, V> IndexVec<I, V>
where
    I: From<usize> + Into<usize> + Copy,
{
    pub const fn new() -> IndexVec<I, V> {
        IndexVec {
            values: Vec::new(),
            _phantom: PhantomData,
        }
    }

    pub fn push(&mut self, value: V) -> I {
        let index = I::from(self.values.len());
        self.values.push(value);
        index
    }

    pub fn insert(&mut self, index: I, value: V)
    where
        I: Eq,
    {
        debug_assert!(index == I::from(self.values.len()));
        self.values.push(value);
    }

    pub fn push_with<F>(&mut self, f: F) -> I
    where
        F: FnOnce(I) -> V,
    {
        let index = I::from(self.values.len());
        self.values.push(f(index));
        index
    }

    #[inline]
    pub fn get(&self, index: I) -> Option<&V> {
        self.values.get(index.into())
    }

    #[inline]
    pub const fn len(&self) -> usize {
        self.values.len()
    }

    #[inline]
    pub fn index(&self) -> I {
        I::from(self.values.len())
    }
}

impl<I, V> ops::Index<I> for IndexVec<I, V>
where
    I: Into<usize> + Copy,
{
    type Output = V;

    fn index(&self, index: I) -> &Self::Output {
        &self.values[index.into()]
    }
}

impl<I, V> ops::IndexMut<I> for IndexVec<I, V>
where
    I: Into<usize> + Copy,
{
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.values[index.into()]
    }
}

impl<I, V> IntoIterator for IndexVec<I, V>
where
    I: Into<usize> + Copy,
{
    type Item = V;
    type IntoIter = std::vec::IntoIter<V>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.into_iter()
    }
}

impl<'a, I, V> IntoIterator for &'a IndexVec<I, V>
where
    I: Into<usize> + Copy,
{
    type Item = &'a V;
    type IntoIter = std::slice::Iter<'a, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.iter()
    }
}

impl<'a, I, V> IntoIterator for &'a mut IndexVec<I, V>
where
    I: Into<usize> + Copy,
{
    type Item = &'a mut V;
    type IntoIter = std::slice::IterMut<'a, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.iter_mut()
    }
}
