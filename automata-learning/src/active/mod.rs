mod lstar;

mod oracle;

/// Defines everything that is necessary for running active learning algorithms. For now this focuses mainly on observation tables for Angluin style active learners.
pub mod table;

pub use oracle::Oracle;
