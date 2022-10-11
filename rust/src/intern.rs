use std::borrow::Borrow;
use std::collections::HashSet;
use std::fmt::{self, Display, Formatter};
use std::ops::Deref;
use std::sync::{Arc, Mutex};

use lazy_static::lazy_static;

/// An interned, immutable string.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Text(Arc<String>);

lazy_static! {
    static ref TEXTS: Mutex<HashSet<Text>> = Mutex::new(HashSet::new());
    static ref SCRATCH: Mutex<String> = Mutex::new(String::new());
}

impl Display for Text {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Deref for Text {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Borrow<str> for Text {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl From<&str> for Text {
    fn from(contents: &str) -> Self {
        Self::new(contents)
    }
}

impl Text {
    pub fn new(value: &str) -> Self {
        let mut texts = TEXTS.lock().unwrap();

        texts
            .get_or_insert_with(value, |val| Text(Arc::new(String::from(val))))
            .clone()
    }

    pub fn append(&self, suffix: &str) -> Self {
        let mut scratch = SCRATCH.lock().unwrap();
        scratch.clear();
        scratch.push_str(self);
        scratch.push_str(suffix);

        Self::new(&scratch)
    }
}
