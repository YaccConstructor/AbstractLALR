module FlowExpression 
type Expression = 
    | Var of string
    | Value of string
    | Union of Expression*Expression
    | Concat of Expression*Expression

type FlowExpression = FlowExpression of string * Expression

type Call = Call of FlowExpression*int