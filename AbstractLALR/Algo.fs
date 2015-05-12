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

let rec reduce (tables: CleverParseTable<_>) state (p: AbstractStack)  last = 
        
    let isReadyForReduce state (p: AbstractStack) last  =     
        match tables.isReduce state last with
            | true -> 
                let n = tables.reductionSymbolCount state last
                if p.size >= n then 
                    true
                else 
                    false

            | false -> false
            
        
    let t = p.topState
        
    let R = new HashSet<AbstractStack>()

    match isReadyForReduce t p last with
        | true ->    
            let n = tables.reductionSymbolCount t last
            // printfn "Reduce - %A %A" n state 
            //p.print
            //printfn "\n"
            for i = 1 to n - 1 do
                p.pop
                
            let nTop = p.top               
            let newTops = p.predecessor nTop
                  
            if newTops.Count = 0 then
                let newGotoState = tables.gotoNonTerminal state t last
                R.Add(AbstractStack(newGotoState)) |> ignore
            else 
                let poppedStack = new HashSet<AbstractStack>()
                for s' in newTops do
                    let p' = p.Clone
                    p'.top <- s'
                    poppedStack.Add(p') |> ignore
                for p' in poppedStack do                                           
                       
                    let newGotoState = tables.gotoNonTerminal p'.topState t last
                    R.Add(p' + AbstractStack(newGotoState)) |> ignore

            let result = new HashSet<AbstractStack>()
            for p'' in R do
                result.UnionWith (reduce tables state p'' last) |> ignore 
            result
         
        | false -> 
            // printfn "Curr - %A" state 
            // p.print
            // printfn "\n"
            let result = new HashSet<AbstractStack>()
            result.Add p |> ignore
            result

//global dict of equations


let algo X0 (flowEquations: Dictionary<string, Expression<_>>) (tables: CleverParseTable<_>)  =
    let Cache = new Dictionary<Call<_>, HashSet<AbstractStack>>()
    let W = new List<Call<_>>()
    W.Add(X0)
    let F = new AdjacencyGraph<Call<_>, Edge<Call<_>>>();
    F.AddVertex(X0) |> ignore
    let tr = new HashSet<AbstractStack>()
    Cache.Add (X0, tr) |> ignore

    
     
    let rec compute (c:Call<_>) state (X:Expression<_>) =
        match X with 
        | Var(a) -> 
            let CurFlowEq = FlowExpression(a, flowEquations.[a])
            if not (findCallInGraph F (Call(CurFlowEq, state)) c) then 
                F.AddVerticesAndEdge(Edge(Call(CurFlowEq, state), c)) |> ignore
            if not(Cache.ContainsKey(Call(CurFlowEq, state))) then
                let temp = new HashSet<AbstractStack>()
                //temp.Add() |> ignore
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
            reduce tables state (AbstractStack(tables.gotoTerminal state t)) false
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
                    P.Add(p + p') |> ignore
                P

            let CalcE1 = compute c state E1

            for p in CalcE1 do
                P.UnionWith (continueOperator c p E2) |> ignore

            let Result = new HashSet<AbstractStack>()
            for p'' in P do 
                Result.UnionWith (reduce tables state p'' false)
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
               //printfn "%A" e.topState
               if not (tables.isAccept e.topState) then
                   flag <- false
        flag
   
    match checkResult with
        |true -> Accept
        |false -> Error
    
