use crate::parser::NodeId;

#[derive(Debug)]
pub struct SourceError {
    pub message: String,
    pub node_id: NodeId,
}
