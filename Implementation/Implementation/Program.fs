(*This algorithm has the following output:
    - goto controller (function)
    - flow-equation schemes 
    - initial call X0(s0)

For implementation this algorithm we need following data structures
    - W - list of calls 
    - F - dynamically generated call graph, consisting of arcs of form, X(s) -> X'(s')
    - Cashe: Call -> P(ParseStack) - dynamic array, mapping calls to sets of parse-stack segments
      (ParseStack - trees, whose nodes are ParseStack and one node is marked - bottom and another one node - top)
      **There is a unique entrym Cache[X[s]] in the cache array <==> X(s), appears in F

Definition:
Let Σ name the states in the parser’s goto-controller
Flow equation, Xi = Ei, denotes the function, Xi : Σ → P( Σ* )

*)

open AbsStack 
open FlowExpression
open System.Collections.Generic

let isFinalState t = false
let goto a b = 42

let rec reduce state (p:AbstractStack) = 
    let t = p.top
    let T = 1
    let R = new HashSet<AbstractStack>()
    if isFinalState t then
        let newTops = p.predecessor t
        if newTops.Count = 0 then
            R.Add(AbstractStack(goto state t)) |> ignore
        else 
            let poppedStack = new HashSet<AbstractStack>()
            for s' in newTops do
                let p' = p.Clone
                p'.top <- s'
                poppedStack.Add(p') |> ignore
            for p' in poppedStack do
                R.Add(p' + AbstractStack(goto p'.top T)) |> ignore
        //this we optimize in future
        let res = new HashSet<HashSet<AbstractStack>>()
        for p'' in R do
            res.Add (reduce state p'') |> ignore 
        let result = new HashSet<AbstractStack>()

        for r in res do 
            for i in r do
                result.Add(i) |> ignore
        result
         
     else 
        let result = new HashSet<AbstractStack>()
        result.Add p |> ignore
        result
        
let algo = 
 // implementation 
 0



            
    

