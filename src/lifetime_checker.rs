#[derive(Debug, PartialEq, Clone, Copy)]
pub enum AllocationLifetime {
    Local,
    Caller,
}
