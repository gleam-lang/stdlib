import gleam/dynamic

pub type GleamPanic {
  GleamPanic(
    message: String,
    file: String,
    module: String,
    function: String,
    line: Int,
    kind: PanicKind,
  )
}

pub type PanicKind {
  Todo
  Panic
  LetAssert(
    start: Int,
    end: Int,
    pattern_start: Int,
    pattern_end: Int,
    value: dynamic.Dynamic,
  )
  Assert(start: Int, end: Int, expression_start: Int, kind: AssertKind)
}

pub type AssertKind {
  BinaryOperator(
    operator: String,
    left: AssertedExpression,
    right: AssertedExpression,
  )
  FunctionCall(arguments: List(AssertedExpression))
  OtherExpression(expression: AssertedExpression)
}

pub type AssertedExpression {
  AssertedExpression(start: Int, end: Int, kind: ExpressionKind)
}

pub type ExpressionKind {
  Literal(value: dynamic.Dynamic)
  Expression(value: dynamic.Dynamic)
  Unevaluated
}

@external(erlang, "gleeunit_gleam_panic_ffi", "from_dynamic")
@external(javascript, "./gleeunit_gleam_panic_ffi.mjs", "from_dynamic")
pub fn from_dynamic(data: dynamic.Dynamic) -> Result(GleamPanic, Nil)
