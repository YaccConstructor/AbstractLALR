type ExprTree = 
     | Var of string
     | Value of string
     | Union of ExprTree*ExprTree
     | Cncat of ExprTree*ExprTree

type Expression = Expr of string * ExprTree

let printTree x = 
    let rec order node =
        let union = "Union of"
        let cncat = "Cncat of" 
        match node with
        | Var(value) -> printf "%s " value
        | Value(value) -> printf "%s " value
        | Union(a, b) -> 
             printf "%s " union
             order(a)
             order(b)
        | Cncat(a, b) -> 
             printf "%s " cncat
             order(a)
             order(b)
        
    order x
    
[<EntryPoint>]
let main argv = 
        let e = Union(Var("a"), Cncat(Var("b"), Var("c")))
        printTree e
        0
