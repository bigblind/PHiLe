#[derive(Clone, Copy, Debug)]
struct Location {
    line: usize,
    col: usize,
}

#[derive(Clone, Copy, Debug)]
struct Range {
    begin: Location,
    end: Location,
}

enum NodeValue {
    StructDecl,
    ClassDecl,
    EnumDecl,
    FunctionDecl,
}

struct Node {
    range: Range,
    value: NodeValue,
}
