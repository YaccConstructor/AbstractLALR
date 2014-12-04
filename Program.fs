type ExprTree = 
     | Var of string
     | Value of string
     | Union of ExprTree*ExprTree
     | Cncat of ExprTree*ExprTree

type Expression = Expr of string * ExprTree

[<EntryPoint>]
let main argv = 
    let exampleExpression =  Union(Var("a"), Var("b"))
    let expressionWithName = Expr("A", exampleExpression)

    0
