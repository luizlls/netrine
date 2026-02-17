use hashbrown::HashMap;
use hashbrown::hash_map::Entry;
use std::hash::Hash;
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
    pub fn new() -> IndexVec<I, V> {
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

    pub fn push_with<F>(&mut self, f: F) -> I
    where
        F: FnOnce(I) -> V,
    {
        let index = I::from(self.values.len());
        self.values.push(f(index));
        index
    }

    pub fn get(&self, index: I) -> Option<&V> {
        self.values.get(index.into())
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

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

#[derive(Debug, Default)]
pub struct IndexMap<K, I, V> {
    values: IndexVec<I, V>,
    entries: HashMap<K, I>,
}

impl<K, I, V> IndexMap<K, I, V>
where
    K: Eq + Hash,
    I: From<usize> + Into<usize> + Copy,
{
    pub fn new() -> IndexMap<K, I, V> {
        IndexMap {
            values: IndexVec::new(),
            entries: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: K, value: V) -> I {
        match self.entries.entry(key) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let index = self.values.push(value);
                entry.insert(index);
                index
            }
        }
    }

    pub fn insert_with<F>(&mut self, key: K, f: F) -> I
    where
        F: FnOnce(I) -> V,
    {
        match self.entries.entry(key) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let index = self.values.push_with(f);
                entry.insert(index);
                index
            }
        }
    }

    pub fn get(&self, key: K) -> Option<&V> {
        self.entries.get(&key).map(|&index| &self.values[index])
    }

    pub fn get_by_key(&self, key: K) -> Option<&V> {
        self.get(key)
    }

    pub fn get_by_id(&self, index: I) -> Option<&V> {
        self.values.get(index)
    }

    pub fn id(&self, key: K) -> Option<I> {
        self.entries.get(&key).cloned()
    }
}

impl<K, I, V> ops::Index<I> for IndexMap<K, I, V>
where
    I: Into<usize> + Copy,
{
    type Output = V;

    fn index(&self, index: I) -> &Self::Output {
        &self.values[index]
    }
}

impl<K, I, V> ops::IndexMut<I> for IndexMap<K, I, V>
where
    I: Into<usize> + Copy,
{
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.values[index]
    }
}

impl<K, I, V> IntoIterator for IndexMap<K, I, V>
where
    I: Into<usize> + Copy,
{
    type Item = V;
    type IntoIter = std::vec::IntoIter<V>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, Copy, PartialEq)]
    struct Index(u32);

    impl From<usize> for Index {
        fn from(value: usize) -> Self {
            Index(value as u32)
        }
    }

    impl Into<usize> for Index {
        fn into(self) -> usize {
            self.0 as usize
        }
    }

    #[test]
    fn test_index_map() {
        let mut map: IndexMap<&str, Index, i32> = IndexMap::new();
        let index1 = map.insert("foo", 1);
        assert_eq!(map.get("foo"), Some(&1));

        let _index2 = map.insert("bar", 2);
        assert_eq!(map.get("bar"), Some(&2));

        let index3 = map.insert("foo", 3);
        assert_eq!(index1, index3);
        assert_eq!(map[index3], 3);
    }
}
