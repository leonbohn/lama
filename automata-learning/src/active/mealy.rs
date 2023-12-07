use automata::prelude::*;

use super::LStarHypothesis;

impl<C: Color> LStarHypothesis for MooreMachine<Simple, C> {
    type Color = C;
}

impl<C: Color> LStarHypothesis for MealyMachine<Simple, C> {
    type Color = C;
}
