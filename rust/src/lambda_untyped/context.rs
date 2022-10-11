use std::convert::Infallible;

use crate::intern::Text;

pub trait Context {
    type Error;

    fn extend<F, T>(&mut self, name: Text, f: F) -> Result<T, Self::Error>
    where
        F: FnOnce(&mut Self) -> Result<T, Self::Error>;
}

impl Context for () {
    type Error = Infallible;

    fn extend<F, T>(&mut self, _name: Text, f: F) -> Result<T, Self::Error>
    where
        F: FnOnce(&mut Self) -> Result<T, Self::Error>,
    {
        f(self)
    }
}

/// A naming context to reconstruct the names of variables.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalContext {
    names: Vec<Text>,
}

impl Default for GlobalContext {
    fn default() -> Self {
        Self::new()
    }
}

impl GlobalContext {
    pub fn new() -> Self {
        Self { names: Vec::new() }
    }

    pub fn len(&self) -> usize {
        self.names.len()
    }

    pub fn is_empty(&self) -> bool {
        self.names.is_empty()
    }

    pub fn get(&self, index: u32) -> Option<Text> {
        if self.is_empty() {
            None
        } else {
            let max_idx = self.len() - 1;
            if index as usize > max_idx {
                None
            } else {
                Some(self.names[max_idx - index as usize].clone())
            }
        }
    }

    pub fn push(&mut self, name: Text) {
        self.names.push(name);
    }

    pub fn context(&self) -> NamingContext {
        NamingContext {
            context: self,
            additional: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamingContext<'a> {
    context: &'a GlobalContext,
    additional: Vec<Text>,
}

impl Context for NamingContext<'_> {
    type Error = BindingError;

    fn extend<F, T>(&mut self, name: Text, f: F) -> Result<T, Self::Error>
    where
        F: FnOnce(&mut Self) -> Result<T, Self::Error>,
    {
        self.additional.push(name);
        let res = f(self);
        self.additional.pop();
        res
    }
}

impl NamingContext<'_> {
    pub fn get(&self, index: u32) -> Option<Text> {
        let index = index as usize;
        let adtl_len = self.additional.len();
        if index < adtl_len {
            let max_idx = adtl_len - 1;
            Some(self.additional[max_idx - index].clone())
        } else {
            self.context.get((index - adtl_len) as u32)
        }
    }

    pub fn index_of(&self, name: &str) -> Option<u32> {
        self.iter().position(|n| &*n == name).map(|i| i as u32)
    }

    pub fn iter(&self) -> impl Iterator<Item = Text> + '_ {
        self.additional
            .iter()
            .rev()
            .chain(self.context.names.iter().rev())
            .cloned()
    }

    pub fn fresh(&self, base: &str) -> Text {
        let mut name = Text::new(base);

        while self.iter().any(|n| n == name) {
            name = name.append("'");
        }

        name
    }
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum BindingError {
    #[error("variable {:?}", &*name)]
    UnboundName { name: Text },

    #[error("index {index} is not associated with a name")]
    UnboundIndex { index: u32 },
}
