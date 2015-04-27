module FlowExpression 
type Expression = 
    | Var of string
    | Value of Parser.token
    | Union of Expression*Expression
    | Concat of Expression*Expression

type FlowExpression = FlowExpression of string * Expression

type Call = Call of FlowExpression*int

let getExpression (Call(FlowExpression(a,b), s)) = b
let getState (Call(FlowExpression(a,b), s)) = s
let getName (Call(FlowExpression(a,b), s)) = a

let (+.) (a:Expression) (b:Expression) =
    Concat(a, b) 

let (++) (a:Expression) (b:Expression) =
    Union(a, b) 

let EqualCall a b = 
    (getName a = getName b && getState a = getState b)
