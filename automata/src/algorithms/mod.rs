use std::fmt::Debug;

use crate::{prelude::*, ts::Quotient};

mod partition_refinement;
pub use partition_refinement::{mealy_partition_refinement, moore_partition_refinement};
