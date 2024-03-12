use super::OmegaSample;

pub trait SproutLearner {
    type Aut;

    fn sprout(sample: OmegaSample) -> Self::Aut;
}
