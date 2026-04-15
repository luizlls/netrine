macro_rules! entity_id {
    ($name:ident, $size:ty) => {
        #[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, Default)]
        pub struct $name($size);

        impl $name {
            pub const fn new(id: $size) -> $name {
                $name(id)
            }

            pub const fn id(self) -> $size {
                self.0
            }

            pub const fn prev(self) -> $name {
                $name(self.0.saturating_sub(1))
            }

            pub const fn next(self) -> $name {
                $name(self.0.saturating_add(1))
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

        impl From<$name> for u32 {
            fn from(value: $name) -> u32 {
                value.0 as u32
            }
        }

        impl From<u32> for $name {
            fn from(value: u32) -> $name {
                $name(value as $size)
            }
        }
    };
}

pub(crate) use entity_id;
