use super::index_vec::IndexVec;
use hashbrown::HashMap;
use hashbrown::hash_map::Entry;
use std::hash::Hash;
use std::ops;

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

    #[inline]
    pub fn get(&self, key: K) -> Option<&V> {
        self.entries.get(&key).map(|&index| &self.values[index])
    }

    #[inline]
    pub fn id(&self, key: K) -> Option<I> {
        self.entries.get(&key).cloned()
    }

    #[inline]
    pub fn contains(&self, key: K) -> bool {
        self.entries.contains_key(&key)
    }

    #[inline]
    pub fn values(self) -> IndexVec<I, V> {
        self.values
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
