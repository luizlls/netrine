macro_rules! entity_id {
    ($name:ident, $size:ty) => {
        #[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, Default)]
        pub struct $name($size);

        impl $name {
            pub fn new(id: $size) -> $name {
                $name(id)
            }

            pub fn id(self) -> $size {
                self.0
            }
        }

        impl From<usize> for $name {
            fn from(value: usize) -> $name {
                $name(value as $size)
            }
        }

        impl From<$name> for usize {
            fn from(value: $name) -> usize {
                value.0 as usize
            }
        }
    };
}

pub(crate) use entity_id;
