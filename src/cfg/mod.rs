use super::{Source, Span};

struct SourceInfo<'input> {
    source: &'input Source,
    span: Span,
}

struct Statement<'input> {
    source_info: SourceInfo<'input>,
    kind: StatementKind,
}

enum StatementKind {}

struct Terminator<'input> {
    source_info: SourceInfo<'input>,
    kind: TerminatorKind,
}

enum TerminatorKind {}

struct Block<'input> {
    statements: Vec<Statement<'input>>,
    terminator: Terminator<'input>,
}

struct Cfg<'input> {
    blocks: Vec<Block<'input>>,
}
