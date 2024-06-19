use crate::syntax;
use crate::error::{error, Error, Result};

use super::node::*;

struct Lower {
    block: Block,
}
