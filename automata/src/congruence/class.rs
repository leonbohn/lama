use itertools::Itertools;

use crate::{prelude::*, Void};

/// Represents a congruence class, which is in essence simply a non-empty sequence of symbols
/// for the underlying alphabet.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Class<S>(pub Vec<S>);

impl<S: Show> Show for Class<S> {
    fn show(&self) -> String {
        format!("[{}]", self.0.iter().map(|s| s.show()).join(""))
    }

    fn show_collection<'a, I>(iter: I) -> String
    where
        Self: 'a,
        I: IntoIterator<Item = &'a Self>,
        I::IntoIter: DoubleEndedIterator,
    {
        todo!()
    }
}

impl<A: Alphabet, Q: Clone, C: Clone> Indexes<RightCongruence<A, Q, C>> for Class<A::Symbol> {
    #[inline(always)]
    fn to_index(
        &self,
        ts: &RightCongruence<A, Q, C>,
    ) -> Option<<RightCongruence<A, Q, C> as TransitionSystem>::StateIndex> {
        ts.class_to_index(self).or(ts.reached_state_index(self))
    }
}
impl<'a, A: Alphabet, Q: Clone, C: Clone> Indexes<RightCongruence<A, Q, C>>
    for &'a Class<A::Symbol>
{
    #[inline(always)]
    fn to_index(
        &self,
        ts: &RightCongruence<A, Q, C>,
    ) -> Option<<RightCongruence<A, Q, C> as TransitionSystem>::StateIndex> {
        Class::to_index(self, ts)
    }
}

impl<S> Class<S> {
    /// Creates an instance of the empty class
    pub fn epsilon() -> Self {
        Self(vec![])
    }

    /// Takes in a single symbol and returns a class containing only that symbol.
    pub fn singleton(sym: S) -> Self {
        Self(vec![sym])
    }

    /// Turns this class into a string, using the given alphabet to convert symbols to strings.
    pub fn mr_to_string(&self) -> String
    where
        S: Symbol,
    {
        if self.is_empty() {
            "ε".to_string()
        } else {
            self.0.iter().map(|sym| sym.show()).join("")
        }
    }
}

impl<S> FromIterator<S> for Class<S> {
    fn from_iter<T: IntoIterator<Item = S>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<S: Symbol> std::fmt::Display for Class<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            if self.0.is_empty() {
                "ε".to_string()
            } else {
                self.0.iter().map(|sym| sym.show()).join("")
            }
        )
    }
}
impl<S> HasLength for Class<S> {
    type Length = FiniteLength;

    fn length(&self) -> Self::Length {
        FiniteLength(self.0.len())
    }
}

impl<S: Symbol> LinearWord<S> for Class<S> {
    fn nth(&self, position: usize) -> Option<S> {
        self.0.get(position).cloned()
    }
}
impl<S: Symbol> FiniteWord<S> for Class<S> {
    type Symbols<'this> = std::iter::Cloned<std::slice::Iter<'this, S>>
    where
        Self: 'this,
        S: 'this;

    fn symbols(&self) -> Self::Symbols<'_> {
        self.0.iter().cloned()
    }

    fn to_vec(&self) -> Vec<S> {
        self.0.clone()
    }

    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<S> std::ops::Deref for Class<S> {
    type Target = Vec<S>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<S> std::ops::DerefMut for Class<S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl<S> Default for Class<S> {
    fn default() -> Self {
        Self(vec![])
    }
}
impl<S> From<Vec<S>> for Class<S> {
    fn from(value: Vec<S>) -> Self {
        Self(value)
    }
}
impl From<&str> for Class<char> {
    fn from(value: &str) -> Self {
        Self(value.chars().collect())
    }
}
impl<S: std::fmt::Debug> std::fmt::Debug for Class<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0.iter().map(|sym| format!("{:?}", sym)).join("")
        )
    }
}

impl<S: Ord> Ord for Class<S> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0
            .len()
            .cmp(&other.0.len())
            .then_with(|| self.0.cmp(&other.0))
    }
}
impl<S: Ord> PartialOrd for Class<S> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// A colored class is a [`Class`] which additionally has an associated color.
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct ColoredClass<S: Symbol, Q = Void> {
    pub(crate) class: Class<S>,
    pub(crate) color: Q,
}

impl<A: Alphabet, Q: Clone, C: Clone> Indexes<RightCongruence<A, Q, C>>
    for ColoredClass<A::Symbol, Q>
{
    #[inline(always)]
    fn to_index(
        &self,
        ts: &RightCongruence<A, Q, C>,
    ) -> Option<<RightCongruence<A, Q, C> as TransitionSystem>::StateIndex> {
        self.class.to_index(ts)
    }
}
impl<'a, A: Alphabet, Q: Clone, C: Clone> Indexes<RightCongruence<A, Q, C>>
    for &'a ColoredClass<A::Symbol, Q>
{
    #[inline(always)]
    fn to_index(
        &self,
        ts: &RightCongruence<A, Q, C>,
    ) -> Option<<RightCongruence<A, Q, C> as TransitionSystem>::StateIndex> {
        self.class.to_index(ts)
    }
}

impl<S: Symbol, Q: std::fmt::Debug> std::fmt::Display for ColoredClass<S, Q> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{} | {:?}]", self.class, self.color)
    }
}

impl<S: Symbol, J: Into<Class<S>>> From<J> for ColoredClass<S, Void> {
    fn from(value: J) -> Self {
        Self {
            class: value.into(),
            color: Void,
        }
    }
}

impl<S: Symbol, W: FiniteWord<S>, Q: Clone> From<(W, Q)> for ColoredClass<S, Q> {
    fn from(value: (W, Q)) -> Self {
        Self {
            class: value.0.to_vec().into(),
            color: value.1,
        }
    }
}

impl<S: Symbol, Q: Clone> ColoredClass<S, Q> {
    /// Creates a new colored class from the given class and color.
    pub fn new<X: Into<Class<S>>>(class: X, color: Q) -> Self {
        Self {
            class: class.into(),
            color,
        }
    }

    /// Returns a reference to the underlying class.
    pub fn class(&self) -> &Class<S> {
        &self.class
    }

    /// Consumes `self` and returns a [`ColoredClass`] with the same color but the given `class`.
    pub fn reclass<X: Into<Class<S>>>(self, class: X) -> ColoredClass<S, Q> {
        ColoredClass {
            class: class.into(),
            color: self.color,
        }
    }

    /// Consumes `self` and returns a [`ColoredClass`] with the same class but the given `color`.
    pub fn recolor<D: Clone>(self, color: D) -> ColoredClass<S, D> {
        ColoredClass {
            class: self.class,
            color,
        }
    }

    /// Returns a reference to the stored color.
    pub fn color(&self) -> &Q {
        &self.color
    }
}
