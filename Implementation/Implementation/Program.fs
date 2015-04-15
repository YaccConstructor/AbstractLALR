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

open QuickGraph
open QuickGraph.Algorithms
open QuickGraph.Collections

let isFinalState t = false
let goto a b = 42

let rec reduce state (p: AbstractStack) = 
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

//global dict of equation
let FlowEquations =  new Dictionary<string, Expression>()

FlowEquations.Add("X0", Value("a")) |> ignore
FlowEquations.Add("R" , Value("[")) |> ignore  
FlowEquations.Add("X1", Union(Var("X0"), Var("X2"))) |> ignore   
FlowEquations.Add("X2", Concat(Concat(Value("["), Var("X1")), Var("X1"))) |> ignore   
FlowEquations.Add("X3", Var("X1")) |> ignore
     

let algo X0 =
    let W = new List<Call>()
    W.Add(X0)
    let F = new AdjacencyGraph<Call, Edge<Call>>();
    F.AddVertex(X0) |> ignore
    let Cache = new Dictionary<Call, HashSet<AbstractStack>>()
    

    let rec compute (c:Call) state (X:Expression) =
        match X with 
        | Var(a) -> 
            let CurFlowEq = FlowExpression(a, Var(a))

            if Cache.ContainsKey(Call(CurFlowEq, state)) then
                Cache.Add(Call(CurFlowEq, state), null)
                F.AddVerticesAndEdge(Edge(Call(CurFlowEq, state), c)) |> ignore
                W.Add(Call(CurFlowEq, state)) |> ignore

            if F.ContainsEdge(Edge(c, Call(CurFlowEq, state)))
                then 
                    let t = Cache.[Call(CurFlowEq, state)]
                    let Result = new HashSet<AbstractStack>()

                    for e in t do
                        e.fold
                        Result.Add e |> ignore
                    Result
                else 
                    let Result = new HashSet<AbstractStack>()
                    Result.UnionWith Cache.[Call(CurFlowEq, state)] |> ignore
                    Result

        |Value(t) -> 
            reduce state (AbstractStack(goto state t))
        | Union(E1,E2) -> 
           let res = (compute c state E1)
           res.UnionWith(compute c state E2)
           res
        | Concat(E1, E2) ->
            let P = new HashSet<AbstractStack>()
           
            let continueOperator (c:Call) (p: AbstractStack) (E: Expression) =
                let P = new HashSet<AbstractStack>()
                let CalcContE = compute c p.topState E
                for p' in CalcContE do
                    P.Add(p + p') |> ignore
                P

            let CalcE1 = compute c state E1

            for p in CalcE1 do
                P.UnionWith (continueOperator c p E2) |> ignore

            let Result = new HashSet<AbstractStack>()
            for p'' in P do 
                Result.UnionWith (reduce state p'')
            Result
   
    let getEq (Call(FlowExpression(a,b), s)) = 
        b
    let getSt (Call(FlowExpression(a,b), s)) = 
        s

    while W.Count <> 0 do
        let call = W.[0]
        W.RemoveAt(0) 
        let X = getEq call
        let P = compute call (getSt call) X
        if not(P.IsSubsetOf Cache.[call]) then 
            Cache.[call].UnionWith P
            for e in F.Edges do
                if (e.Target = call) then
                    W.Add e.Source
  
    0

                

            
            
            

             
    


            
    

