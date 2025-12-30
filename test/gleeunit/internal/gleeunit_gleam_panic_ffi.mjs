import { Ok, Error, Empty, NonEmpty } from "../../gleam.mjs";
import {
  GleamPanic,
  Todo,
  Panic,
  LetAssert,
  Assert,
  BinaryOperator,
  FunctionCall,
  OtherExpression,
  AssertedExpression,
  Literal,
  Expression,
  Unevaluated,
} from "./gleam_panic.mjs";

export function from_dynamic(error) {
  if (!(error instanceof globalThis.Error) || !error.gleam_error) {
    return new Error(undefined);
  }

  if (error.gleam_error === "todo") {
    return wrap(error, new Todo());
  }

  if (error.gleam_error === "panic") {
    return wrap(error, new Panic());
  }

  if (error.gleam_error === "let_assert") {
    let kind = new LetAssert(
      error.start,
      error.end,
      error.pattern_start,
      error.pattern_end,
      error.value,
    );
    return wrap(error, kind);
  }

  if (error.gleam_error === "assert") {
    let kind = new Assert(
      error.start,
      error.end,
      error.expression_start,
      assert_kind(error),
    );
    return wrap(error, kind);
  }

  return new Error(undefined);
}

function assert_kind(error) {
  if (error.kind == "binary_operator") {
    return new BinaryOperator(
      error.operator,
      expression(error.left),
      expression(error.right),
    );
  }

  if (error.kind == "function_call") {
    let list = new Empty();
    let i = error.arguments.length;
    while (i--) {
      list = new NonEmpty(expression(error.arguments[i]), list);
    }
    return new FunctionCall(list);
  }

  return new OtherExpression(expression(error.expression));
}

function expression(data) {
  const expression = new AssertedExpression(data.start, data.end, undefined);
  if (data.kind == "literal") {
    expression.kind = new Literal(data.value);
  } else if (data.kind == "expression") {
    expression.kind = new Expression(data.value);
  } else {
    expression.kind = new Unevaluated();
  }
  return expression;
}

function wrap(e, kind) {
  return new Ok(
    new GleamPanic(e.message, e.file, e.module, e.function, e.line, kind),
  );
}
