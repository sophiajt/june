use crate::parser::NodeId;

#[derive(Debug)]
pub enum Severity {
    Error,
    Note,
    Warning,
}

#[derive(Debug)]
pub struct SourceError {
    pub message: String,
    pub node_id: NodeId,
    pub severity: Severity,
}
