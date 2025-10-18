use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::hash::Hash;
use std::ops::Index;

#[derive(Debug, Clone)]
pub struct IndexMap<K, V, I>
where
    K: Hash + Eq + Clone,
    I: From<usize> + Into<usize> + Copy,
{
    indices: HashMap<K, I>,
    entries: Vec<V>,
}

impl<K, V, I> IndexMap<K, V, I>
where
    K: Hash + Eq + Clone,
    I: From<usize> + Into<usize> + Copy,
{
    pub fn new() -> IndexMap<K, V, I> {
        IndexMap {
            indices: HashMap::new(),
            entries: Vec::new(),
        }
    }

    pub fn insert(&mut self, key: K, value: V) -> I {
        match self.indices.entry(key) {
            Entry::Vacant(entry) => {
                let index = I::from(self.entries.len());
                entry.insert(index);
                self.entries.push(value);
                index
            }
            Entry::Occupied(entry) => {
                let index = *entry.get();
                self.entries[index.into()] = value;
                index
            }
        }
    }

    pub fn get(&self, index: I) -> Option<&V> {
        self.entries.get(index.into())
    }

    pub fn get_by_key(&self, key: &K) -> Option<&V> {
        self.indices
            .get(key)
            .map(|&index| &self.entries[index.into()])
    }
}

impl<K, V, I> Index<I> for IndexMap<K, V, I>
where
    K: Hash + Eq + Clone,
    I: From<usize> + Into<usize> + Copy,
{
    type Output = V;

    fn index(&self, index: I) -> &Self::Output {
        &self.entries[index.into()]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct Test(String);

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    struct TestId(u32);

    impl From<usize> for TestId {
        fn from(value: usize) -> TestId {
            TestId(value as u32)
        }
    }

    impl Into<usize> for TestId {
        fn into(self) -> usize {
            self.0 as usize
        }
    }

    #[test]
    fn basic_usage() {
        let mut map = IndexMap::<String, Test, TestId>::new();
        let val = Test("Value".into());

        let id = map.insert("key".into(), val.clone());

        assert_eq!(map.get(id), Some(&val));
        assert_eq!(map[id], val);
        assert_eq!(map.get(TestId(0)), Some(&val));

        assert_eq!(map.get_by_key(&"key".into()), Some(&val));
    }

    #[test]
    fn modify_values() {
        let mut map = IndexMap::<String, Test, TestId>::new();
        let old_val = Test("Old Value".into());

        let old_id = map.insert("key".into(), old_val.clone());
        assert_eq!(map.get(old_id), Some(&old_val));
        assert_eq!(map.get_by_key(&"key".into()), Some(&old_val));

        let new_val = Test("New Value".into());

        let new_id = map.insert("key".into(), new_val.clone());

        assert_eq!(old_id, new_id);
        assert_eq!(map.get(new_id), Some(&new_val));
        assert_eq!(map.get(old_id), Some(&new_val));
        assert_eq!(map.get_by_key(&"key".into()), Some(&new_val));
    }
}
