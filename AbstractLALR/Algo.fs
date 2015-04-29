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

module AbstractLALR

open AbsStack 
open FlowExpression
open System.Collections.Generic
open System.IO
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
open ParserHelper
open QuickGraph
open QuickGraph.Algorithms
open QuickGraph.Collections


type result = Accept | Error

// a - source 
// b - target
let findCallInGraph (gr:AdjacencyGraph<Call<_>, Edge<Call<_>>>) (a: Call<_>) (b: Call<_>) =
    let mutable flag = false
    for e in gr.Edges do
        if (EqualCall e.Source a) && (EqualCall e.Target b) then 
            flag <- true
    flag


let printFlowGraph (gr:AdjacencyGraph<Call<_>, Edge<Call<_>>>) (filename: string) = 
    use file = new StreamWriter(filename)
    file.WriteLine("digraph FlowGraph {\nrankdir=RL;")
    let CallToString call = 
        getName call + "(s" + string (call |> getState)  + ")"
    for e in gr.Edges do
        let temp = "\""+ CallToString e.Source + "\" -> \"" + CallToString e.Target + "\""
        file.WriteLine("    " + temp) 
    file.WriteLine("}") 
                               
let isSubsetOf (Q: HashSet<AbstractStack>) (B: HashSet<AbstractStack>) =
    let mutable mainFlag = true
    for b in B do
        let mutable flag = false
        for a in Q do
            if a.Equals(b) then
                flag <- true
        mainFlag <- (mainFlag && flag)
    mainFlag


//global dict of equations


let algo X0 (flowEquations: Dictionary<string, Expression<_>>) (tables: CleverParseTable<_>)  =
    let Cache = new Dictionary<Call<_>, HashSet<AbstractStack>>()
    let W = new List<Call<_>>()
    W.Add(X0)
    let F = new AdjacencyGraph<Call<_>, Edge<Call<_>>>();
    F.AddVertex(X0) |> ignore
    let tr = new HashSet<AbstractStack>()
    Cache.Add (X0, tr) |> ignore

    
    let rec reduce state (p: AbstractStack) = 

        let isReadyForReduce state (p: AbstractStack)  = 
            match (actionKind (tables.action state)) = reduceFlag with
                | true -> 
                    let n = tables.reductionSymbolCount state
                    if p.size >= n then 
                        true
                    else 
                        false

                | false -> false
    
        let t = p.topState
        let R = new HashSet<AbstractStack>()

        match isReadyForReduce t p with
            | true ->      
                let n = tables.reductionSymbolCount t
                for i in [1..n] do
                  p.pop

                let nTop = p.top               
                let newTops = p.predecessor nTop
                if newTops.Count = 0 then
                   let newGotoState = tables.gotoNonTerminal state t 
                   R.Add(AbstractStack(newGotoState)) |> ignore
                else 
                   let poppedStack = new HashSet<AbstractStack>()
                   for s' in newTops do
                       let p' = p.Clone
                       p'.top <- s'
                       poppedStack.Add(p') |> ignore

                   for p' in poppedStack do
                       let newGotoState = tables.gotoNonTerminal p'.topState state 
                       R.Add(p' + AbstractStack(newGotoState)) |> ignore

                let result = new HashSet<AbstractStack>()
                for p'' in R do
                    result.UnionWith (reduce state p'') |> ignore 
                result
         
            | false -> 
                let result = new HashSet<AbstractStack>()
                result.Add p |> ignore
                result
     

    let rec compute (c:Call<_>) state (X:Expression<_>) =
        match X with 
        | Var(a) -> 
            let CurFlowEq = FlowExpression(a, flowEquations.[a])
            if not (findCallInGraph F (Call(CurFlowEq, state)) c) then 
                F.AddVerticesAndEdge(Edge(Call(CurFlowEq, state), c)) |> ignore

            if not(Cache.ContainsKey(Call(CurFlowEq, state))) then
                let temp = new HashSet<AbstractStack>()
                Cache.Add(Call(CurFlowEq, state), temp)
                W.Add(Call(CurFlowEq, state)) |> ignore

            if findCallInGraph F c (Call(CurFlowEq, state)) then 

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

        | Value(t) -> 
            reduce state (AbstractStack(tables.gotoTerminal state t))

        | Union(E1,E2) -> 
           let res = (compute c state E1)
           res.UnionWith(compute c state E2)
           res

        | Concat(E1, E2) ->
            let P = new HashSet<AbstractStack>()
           
            let continueOperator (c:Call<_>) (p: AbstractStack) (E: Expression<_>) =
                let P = new HashSet<AbstractStack>()
                let CalcContE = compute c p.topState E
                for p' in CalcContE do
                    P.Add(p+p') |> ignore
                P

            let CalcE1 = compute c state E1

            for p in CalcE1 do
                P.UnionWith (continueOperator c p E2) |> ignore

            let Result = new HashSet<AbstractStack>()
            for p'' in P do 
                Result.UnionWith (reduce state p'')

            Result
   
    use file = new StreamWriter("WorklistLog.txt")
    while W.Count <> 0 do
        let call = W.[0]
        file.WriteLine (getName call + "(" + (string (getState call)) + ")")
        W.RemoveAt(0) 
        let X = getExpression call
        let P = compute call (getState call) X
        if  not(isSubsetOf Cache.[call] P) then  
              Cache.[call].UnionWith P
              for e in F.Edges do
                if (e.Source = call) then
                   W.Add e.Target
        
    printFlowGraph F "graph.gv"
    
            
            

    let checkResult = 
        let mutable flag = true
        if Cache.ContainsKey(X0) then
            for e in Cache.[X0] do
               
               if not (tables.isAccept e.topState) then
                   flag <- false
        flag
    
    match checkResult with
        |true -> Accept
        |false -> Error

    
(*let FlowEquations1 =  new Dictionary<string, Expression<ParserLAR.token>>()

FlowEquations1.Add("X0", Value(ParserLAR.A)) |> ignore
FlowEquations1.Add("R" , Value(ParserLAR.R)) |> ignore  
FlowEquations1.Add("X1", Var("X0") ++ Var("X2")) |> ignore   
FlowEquations1.Add("X2", Value(ParserLAR.L) +. (Var("X1") +. Var("R"))) |> ignore   
FlowEquations1.Add("X3", Var("X1")) |> ignore

let x0 =  Call(FlowExpression("X3", FlowEquations1.["X3"]), 0)
printfn "%A" (algo x0 FlowEquations1 (CleverParseTable<ParserLAR.token>(ParserLAR.tables())))


let FlowEquations2 =  new Dictionary<string, Expression<ParserAR.token>>()

FlowEquations2.Add("X0", Value(ParserAR.A)) |> ignore
FlowEquations2.Add("R" , Value(ParserAR.R)) |> ignore  
FlowEquations2.Add("X1", Var("X0") ++ Var("X2")) |> ignore   
FlowEquations2.Add("X2", (Var("X1") +. Var("R"))) |> ignore   
FlowEquations2.Add("X3", Var("X1")) |> ignore

let x02 =  Call(FlowExpression("X3", FlowEquations2.["X3"]), 0)
printfn "%A" (algo x02 FlowEquations2 (CleverParseTable<ParserAR.token>(ParserAR.tables())))



*)
