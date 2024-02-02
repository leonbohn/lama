use std::fmt::Debug;

use crate::{prelude::*, ts::Quotient};

mod partition_refinement;
pub use partition_refinement::{
    mealy_greatest_bisimulation, mealy_partition_refinement, moore_greatest_bisimulation,
    moore_partition_refinement,
};
