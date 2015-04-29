module FlowExpression 
type Expression<'token> = 
    | Var of string
    | Value of 'token
    | Union of Expression<'token>*Expression<'token>
    | Concat of Expression<'token>*Expression<'token>

type FlowExpression<'token> = FlowExpression of string * Expression<'token>

type Call<'token> = Call of FlowExpression<'token>*int

let getExpression (Call(FlowExpression(a,b), s)) = b
let getState (Call(FlowExpression(a,b), s)) = s
let getName (Call(FlowExpression(a,b), s)) = a

let (+.) (a:Expression<_>) (b:Expression<_>) =
    Concat(a, b) 

let (++) (a:Expression<_>) (b:Expression<_>) =
    Union(a, b) 

let EqualCall a b = 
    (getName a = getName b && getState a = getState b)
