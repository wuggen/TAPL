use std::fmt::{self, Display, Formatter};

use crate::intern::Text;

use super::context::Context;

/// Value kinds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueKind {
    Bool,
    Num,
    Abstraction,
}

pub trait Classify {
    fn classify(&self) -> Option<ValueKind>;

    fn is_bool_value(&self) -> bool {
        self.classify() == Some(ValueKind::Bool)
    }

    fn is_num_value(&self) -> bool {
        self.classify() == Some(ValueKind::Num)
    }

    fn is_abstraction_value(&self) -> bool {
        self.classify() == Some(ValueKind::Abstraction)
    }

    fn is_value(&self) -> bool {
        self.classify().is_some()
    }
}

/// Constant value labels.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConstLabel {
    True,
    False,
    Zero,
}

impl Display for ConstLabel {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ConstLabel::True => write!(f, "true"),
            ConstLabel::False => write!(f, "false"),
            ConstLabel::Zero => write!(f, "0"),
        }
    }
}

impl Classify for ConstLabel {
    fn classify(&self) -> Option<ValueKind> {
        match self {
            ConstLabel::True | ConstLabel::False => Some(ValueKind::Bool),
            ConstLabel::Zero => Some(ValueKind::Num),
        }
    }
}

/// Labels for syntactic unary operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryLabel {
    Succ,
    Pred,
    IsZero,
}

impl Display for UnaryLabel {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            UnaryLabel::Succ => write!(f, "succ"),
            UnaryLabel::Pred => write!(f, "pred"),
            UnaryLabel::IsZero => write!(f, "iszero"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term<V> {
    Var {
        name: V,
    },

    Binding {
        name: Text,
        body: Box<Term<V>>,
    },

    App {
        left: Box<Term<V>>,
        right: Box<Term<V>>,
    },

    Const {
        label: ConstLabel,
    },

    Unary {
        label: UnaryLabel,
        arg: Box<Term<V>>,
    },

    IfThenElse {
        cond: Box<Term<V>>,
        if_case: Box<Term<V>>,
        else_case: Box<Term<V>>,
    },
}

pub type NamedTerm = Term<Text>;
pub type IndexTerm = Term<u32>;

impl<V: Display> Display for Term<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        impl<V> Term<V> {
            fn is_atom(&self) -> bool {
                matches!(self, Term::Var { .. }) || self.is_num_value()
            }

            fn num_value(&self) -> u32 {
                match self {
                    Term::Const {
                        label: ConstLabel::Zero,
                    } => 0,
                    Term::Unary {
                        label: UnaryLabel::Succ,
                        arg,
                    } => 1 + arg.num_value(),
                    _ => panic!("term is not a numeric value"),
                }
            }
        }

        match self {
            Term::Var { name } => write!(f, "{}", name),
            Term::Binding { name, body } => write!(f, "\\{}.{}", name, body),

            Term::App { left, right } => {
                if matches!(&**left, Term::App { .. }) || left.is_atom() {
                    write!(f, "{}", left)?;
                } else {
                    write!(f, "({})", left)?;
                }

                if right.is_atom() {
                    write!(f, " {}", right)
                } else {
                    write!(f, " ({})", right)
                }
            }

            t if t.is_num_value() => write!(f, "{}", t.num_value()),

            Term::Const { label } => write!(f, "{}", label),

            Term::Unary { label, arg } => {
                write!(f, "{}", label)?;
                if arg.is_atom() {
                    write!(f, " {}", arg)
                } else {
                    write!(f, " ({})", arg)
                }
            }

            Term::IfThenElse {
                cond,
                if_case,
                else_case,
            } => {
                write!(f, "if {} then {} else {}", cond, if_case, else_case)
            }
        }
    }
}

impl<V> Classify for Term<V> {
    fn classify(&self) -> Option<ValueKind> {
        match self {
            Term::Binding { .. } => Some(ValueKind::Abstraction),
            Term::Const { label } => label.classify(),
            Term::Unary {
                label: UnaryLabel::Succ,
                arg,
            } if arg.is_num_value() => Some(ValueKind::Num),
            _ => None,
        }
    }

    fn is_bool_value(&self) -> bool {
        matches!(
            self,
            Term::Const {
                label: ConstLabel::True | ConstLabel::False
            }
        )
    }

    fn is_num_value(&self) -> bool {
        match self {
            Term::Const {
                label: ConstLabel::Zero,
            } => true,
            Term::Unary {
                label: UnaryLabel::Succ,
                arg,
            } => arg.is_num_value(),
            _ => false,
        }
    }

    fn is_abstraction_value(&self) -> bool {
        matches!(self, Term::Binding { .. })
    }
}

impl<V> Term<V> {
    pub fn var<U>(name: U) -> Self
    where
        U: Into<V>,
    {
        Self::Var { name: name.into() }
    }

    pub fn binding<U>(name: U, body: Self) -> Self
    where
        U: Into<Text>,
    {
        Self::Binding {
            name: name.into(),
            body: Box::new(body),
        }
    }

    pub fn app(left: Self, right: Self) -> Self {
        Self::App {
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    pub fn const_(label: ConstLabel) -> Self {
        Self::Const { label }
    }

    pub fn true_() -> Self {
        Self::const_(ConstLabel::True)
    }

    pub fn false_() -> Self {
        Self::const_(ConstLabel::False)
    }

    pub fn zero() -> Self {
        Self::const_(ConstLabel::Zero)
    }

    pub fn unary(label: UnaryLabel, arg: Self) -> Self {
        Self::Unary {
            label,
            arg: Box::new(arg),
        }
    }

    pub fn succ(arg: Self) -> Self {
        Self::unary(UnaryLabel::Succ, arg)
    }

    pub fn pred(arg: Self) -> Self {
        Self::unary(UnaryLabel::Pred, arg)
    }

    pub fn iszero(arg: Self) -> Self {
        Self::unary(UnaryLabel::IsZero, arg)
    }

    pub fn ite(cond: Self, if_case: Self, else_case: Self) -> Self {
        Self::IfThenElse {
            cond: Box::new(cond),
            if_case: Box::new(if_case),
            else_case: Box::new(else_case),
        }
    }

    pub fn num(mut value: u32) -> Self {
        let mut res = Self::zero();
        while value > 0 {
            value -= 1;
            res = Self::succ(res);
        }
        res
    }

    pub fn map_context<C, U, F>(&self, context: &mut C, fvar: &mut F) -> Result<Term<U>, C::Error>
    where
        C: Context,
        F: FnMut(&V, &mut C) -> Result<Term<U>, C::Error>,
    {
        match self {
            Term::Var { name } => fvar(name, context),

            Term::Binding { name, body } => context.extend(name.clone(), |context| {
                let body = Box::new(body.map_context(context, fvar)?);
                Ok(Term::Binding {
                    name: name.clone(),
                    body,
                })
            }),

            Term::App { left, right } => {
                let left = Box::new(left.map_context(context, fvar)?);
                let right = Box::new(right.map_context(context, fvar)?);
                Ok(Term::App { left, right })
            }

            Term::Const { label } => Ok(Term::Const { label: *label }),

            Term::Unary { label, arg } => {
                let arg = Box::new(arg.map_context(context, fvar)?);
                Ok(Term::Unary { label: *label, arg })
            }

            Term::IfThenElse {
                cond,
                if_case,
                else_case,
            } => {
                let cond = Box::new(cond.map_context(context, fvar)?);
                let if_case = Box::new(if_case.map_context(context, fvar)?);
                let else_case = Box::new(else_case.map_context(context, fvar)?);

                Ok(Term::IfThenElse {
                    cond,
                    if_case,
                    else_case,
                })
            }
        }
    }

    pub fn map<U, F>(&self, mut fvar: F) -> Term<U>
    where
        F: FnMut(&V) -> Term<U>,
    {
        self.map_context(&mut (), &mut |v, _ctx| Ok(fvar(v)))
            .unwrap()
    }
}

impl IndexTerm {
    pub fn shift(&self, amount: i32) -> Self {
        self.shift_internal(amount, 0)
    }

    fn shift_internal(&self, amount: i32, cutoff: u32) -> Self {
        struct ShiftCtx {
            amount: i32,
            cutoff: u32,
        }

        impl Context for ShiftCtx {
            type Error = std::convert::Infallible;

            fn extend<F, T>(&mut self, _name: Text, f: F) -> Result<T, Self::Error>
            where
                F: FnOnce(&mut Self) -> Result<T, Self::Error>,
            {
                self.cutoff += 1;
                let res = f(self);
                self.cutoff -= 1;
                res
            }
        }

        self.map_context(&mut ShiftCtx { amount, cutoff }, &mut |v, ctx| {
            if *v < ctx.cutoff {
                Ok(Term::Var { name: *v })
            } else {
                Ok(Term::Var {
                    name: v.wrapping_add_signed(ctx.amount),
                })
            }
        })
        .unwrap()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn term_display() {
        use NamedTerm as T;

        let id = T::binding("x", T::var("x"));
        assert_eq!(id.to_string(), r#"\x.x"#);

        let f = T::binding("x", T::app(T::var("x"), T::var("x")));
        let omega = T::app(f.clone(), f);
        assert_eq!(omega.to_string(), r#"(\x.x x) (\x.x x)"#);

        let f = T::binding("x", T::app(T::var("g"), T::app(T::var("x"), T::var("x"))));
        let y = T::binding("g", T::app(f.clone(), f));
        assert_eq!(y.to_string(), r#"\g.(\x.g (x x)) (\x.g (x x))"#);
    }
}
