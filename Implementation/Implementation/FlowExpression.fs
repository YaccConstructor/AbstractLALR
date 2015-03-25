module FlowExpression 
type ExprTree = 
    | Var of string
    | Value of string
    | Union of ExprTree*ExprTree
    | Cncat of ExprTree*ExprTree

