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
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
open ParserHelper
open Parser
open QuickGraph
open QuickGraph.Algorithms
open QuickGraph.Collections

let EqualCall a b = 
    (getName a = getName b && getState a = getState b)

let findInGr (gr:AdjacencyGraph<Call, Edge<Call>>) (a: Call) (b: Call) =
    let mutable flag = false
    for e in gr.Edges do
        if (EqualCall e.Source a) && (EqualCall e.Target b) then 
            flag <- true
    flag


let printFlowGraph (gr:AdjacencyGraph<Call, Edge<Call>>) = 
    let mutable str = "digraph FlowGraph {\nrankdir=RL;\n"
    let CallToString call = 
        getName call + "(s" + string (call |> getState)  + ")"
    for e in gr.Edges do
        let temp = "\""+ CallToString e.Source + "\" -> \"" + CallToString e.Target + "\""
        str <- str + temp + "\n"
    str <- str + "\n}" 
    str
                               
                

let rec reduce state (p: AbstractStack) = 
    let t = p.topState
    let action = int (tables().immediateActions.[t])
   
    let R = new HashSet<AbstractStack>()

    if ((actionKind action) = reduceFlag) then
        let prod = actionValue action 
        let n = int (tables().reductionSymbolCounts.[prod])
        if p.size >= n then 
            for i in [1..n] do
              p.pop
            let t = p.top               
            let newTops = p.predecessor t
            if newTops.Count = 0 then
               let newGotoState = gotoTable.Read(int (tables().productionToNonTerminalTable.[prod]), state)
               R.Add(AbstractStack(newGotoState)) |> ignore
            else 
               let poppedStack = new HashSet<AbstractStack>()
               for s' in newTops do
                   let p' = p.Clone
                   p'.top <- s'
                   poppedStack.Add(p') |> ignore
               for p' in poppedStack do
                   let newGotoState = gotoTable.Read(int (tables().productionToNonTerminalTable.[prod]), p'.topState)
                   R.Add(p' + AbstractStack(newGotoState)) |> ignore

        //this we optimize in future
            let res = new HashSet<HashSet<AbstractStack>>()
            for p'' in R do
                res.Add (reduce state p'') |> ignore 
            let result = new HashSet<AbstractStack>()

            for r in  res do 
                for i in r do
                    result.Add(i) |> ignore
            result
         
        else 
            let result = new HashSet<AbstractStack>()
            result.Add p |> ignore
            result
    else 
        let result = new HashSet<AbstractStack>()
        result.Add p |> ignore
        result

//global dict of equation
let FlowEquations =  new Dictionary<string, Expression>()

FlowEquations.Add("X0", Value(A)) |> ignore
FlowEquations.Add("R" , Value(R)) |> ignore  
FlowEquations.Add("X1", Union(Var("X0"), Var("X2"))) |> ignore   
FlowEquations.Add("X2", Concat(Value(L), Concat(Var("X1"), Var("R")))) |> ignore   
FlowEquations.Add("X3", Var("X1")) |> ignore

    
let Cache = new Dictionary<Call, HashSet<AbstractStack>>()
let algo X0 =
    let W = new List<Call>()
    W.Add(X0)
    let F = new AdjacencyGraph<Call, Edge<Call>>();
    F.AddVertex(X0) |> ignore
    let temp = new HashSet<AbstractStack>()
    Cache.Add (X0, temp) |> ignore

    let rec compute (c:Call) state (X:Expression) =
        match X with 
        | Var(a) -> 
            let CurFlowEq = FlowExpression(a, FlowEquations.[a])
            if not (findInGr F (Call(CurFlowEq, state)) c ) then 
                    F.AddVerticesAndEdge(Edge(Call(CurFlowEq, state), c)) |> ignore

            if not(Cache.ContainsKey(Call(CurFlowEq, state))) then
                let temp = new HashSet<AbstractStack>()

            
                Cache.Add(Call(CurFlowEq, state), temp)
                W.Add(Call(CurFlowEq, state)) |> ignore

            if findInGr F c (Call(CurFlowEq, state)) then 
                printfn "%A" (printFlowGraph F)

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
            let tag = tables().tagOfToken t                      
            let action = actionTable.Read(state,tag)
            let newState = actionValue action
            reduce state (AbstractStack(newState))

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
                    
                    P.Add(p+p') |> ignore
                P

            let CalcE1 = compute c state E1

            for p in CalcE1 do
                P.UnionWith (continueOperator c p E2) |> ignore

            let Result = new HashSet<AbstractStack>()
            for p'' in P do 
                Result.UnionWith (reduce state p'')
            printfn ""

            Result
   
    
    while W.Count <> 0 do
        let call = W.[0]
        printfn "%A" (getName call, getState call)
        W.RemoveAt(0) 
        let X = getExpression call
        let P = compute call (getState call) X
        if  not(P.IsSubsetOf Cache.[call]) then  
              Cache.[call].UnionWith P

              for e in F.Edges do
                if (e.Source = call) then
                   W.Add e.Target

    printfn "\n%A\n\n" (printFlowGraph F)

    0

let x0 =  Call(FlowExpression("X2", FlowEquations.["X2"]),0)
algo x0 |> ignore

if Cache.ContainsKey(x0) then
    for e in Cache.[x0] do
        printfn "%A" e.topState   //printfn ""
        e.print


(*
let action = 
        let immediateAction = int (tables().immediateActions.[currState])
        if not (immediateAction = anyMarker) then
            // Action has been pre-determined, no need to lookahead 
            // Expecting it to be a Reduce action on a non-fakeStartNonTerminal ? 
            immediateAction
        else
            let tag = 
                tables().tagOfToken L
                                    
            // Printf.printf "state %d\n" state  
            actionTable.Read(currState,tag)

printfn "%A - anymarker" (actionKind action = anyMarker)
printfn "%A - shift" (actionKind action = shiftFlag)
printfn "%A - reduce" (actionKind action = reduceFlag)
printfn "%A - error" (actionKind action = errorFlag)
printfn "%A - accept" (actionKind action = acceptFlag)

let newGotoState1 = actionValue action
                                        
printfn "%A" (newGotoState1)

let kind = actionKind action 
if  kind = reduceFlag then
    let prod = actionValue action                                     
    let reduction = tables().reductions.[prod]                                                             
    let n = int (tables().reductionSymbolCounts.[prod])
    let newGotoState = gotoTable.Read(int (tables().productionToNonTerminalTable.[prod]), 3)
    prntfn "%A" (newGotoState)
    *)


//let test = (AbstractStack(1) + AbstractStack(2)) + AbstractStack(3)
//for e in test.predecessor (1,3) do 
  // printfn "%A" e
//test.pop
//test.print
//let tag = tables().tagOfToken A                      
//let action2 = actionTable.Read(0,tag)
//let currState = actionValue action2
(*let action = 
        let immediateAction = int (tables().immediateActions.[currState])
        if not (immediateAction = anyMarker) then
            // Action has been pre-determined, no need to lookahead 
            // Expecting it to be a Reduce action on a non-fakeStartNonTerminal ? 
            immediateAction
        else
            let tag = 
                tables().tagOfToken L
                                    
            // Printf.printf "state %d\n" state  
            actionTable.Read(currState,tag)           

let kind = actionKind action 
if  kind = reduceFlag then
    let prod = actionValue action                                     
    let reduction = tables().reductions.[prod]                                                             
    
    let newGotoState = gotoTable.Read(int (tables().productionToNonTerminalTable.[prod]), 0)
    printfn "%A" (newGotoState)           

             
    *)


            
    

