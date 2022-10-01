use std::sync::Arc;

use crate::intern::Text;

use ConstLabel::*;
use Term::*;
use TermKind::*;
use UnaryLabel::*;

pub mod context;

/// Label for constant values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConstLabel {
    True,
    False,
    Zero,
}

/// Label for unary terms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryLabel {
    Succ,
    Pred,
    IsZero,
}

/// A variable identified by its name.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NamedVar {
    pub name: Text,
}

/// A variable identified by its de Bruijn index.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct IndexVar {
    pub index: u32,
}

/// A reference-counted shared term.
pub type ArcTerm<V> = Arc<Term<V>>;

pub type NamedTerm = Term<NamedVar>;

pub type IndexTerm = Term<IndexVar>;

/// A lambda calculus term.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term<V> {
    Var {
        value: V,
    },

    Abstr {
        name: Text,
        body: ArcTerm<V>,
    },

    App {
        left: ArcTerm<V>,
        right: ArcTerm<V>,
    },

    Const {
        label: ConstLabel,
    },

    Unary {
        label: UnaryLabel,
        arg: ArcTerm<V>,
    },

    IfThenElse {
        cond: ArcTerm<V>,
        if_case: ArcTerm<V>,
        else_case: ArcTerm<V>,
    },
}

/// Kinds of terms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TermKind {
    BoolValue,
    NumValue,
    AbstrValue,
    NonValue,
}

impl<V> Term<V> {
    pub fn classify(&self) -> TermKind {
        match self {
            Abstr { .. } => AbstrValue,
            Const {
                label: True | False,
            } => BoolValue,
            Const { label: Zero } => NumValue,
            Unary { label: Succ, arg } if arg.is_num_value() => NumValue,
            _ => NonValue,
        }
    }

    pub fn is_value(&self) -> bool {
        match self {
            Abstr { .. } => true,
            Const {
                label: True | False | Zero,
            } => true,
            Unary { label: Succ, arg } if arg.classify() == NumValue => true,
            _ => false,
        }
    }

    pub fn is_bool_value(&self) -> bool {
        matches!(
            self,
            Const {
                label: True | False
            }
        )
    }

    pub fn is_num_value(&self) -> bool {
        match self {
            Const { label: Zero } => true,
            Unary { label: Succ, arg } if arg.is_num_value() => true,
            _ => false,
        }
    }
}

impl IndexTerm {
    pub fn shift(self: Arc<Self>, amount: i32) -> Arc<Self> {
        self.shift_internal(amount, 0)
    }

    fn shift_internal(self: Arc<Self>, amount: i32, cutoff: u32) -> Arc<Self> {
        match &*self {
            Var { value } => {
                if value.index < cutoff {
                    self
                } else {
                    Arc::new(Var {
                        value: IndexVar {
                            index: value.index.wrapping_add_signed(amount),
                        },
                    })
                }
            }

            Abstr { name, body } => Arc::new(Abstr {
                name: name.clone(),
                body: body.clone().shift_internal(amount, cutoff + 1),
            }),

            App { left, right } => Arc::new(App {
                left: left.clone().shift_internal(amount, cutoff),
                right: right.clone().shift_internal(amount, cutoff),
            }),

            Unary { label, arg } => Arc::new(Unary {
                label: *label,
                arg: arg.clone().shift_internal(amount, cutoff),
            }),

            IfThenElse {
                cond,
                if_case,
                else_case,
            } => Arc::new(IfThenElse {
                cond: cond.clone().shift_internal(amount, cutoff),
                if_case: if_case.clone().shift_internal(amount, cutoff),
                else_case: else_case.clone().shift_internal(amount, cutoff),
            }),

            _ => self,
        }
    }
}
