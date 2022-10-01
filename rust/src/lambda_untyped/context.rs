use crate::intern::Text;

/// A naming context to reconstruct the names of variables.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamingContext {
    names: Vec<Text>,
}

impl NamingContext {
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
}
