open QuickGraph
open QuickGraph.Collections
open System.Collections.Generic

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

**)

type FlowExpression = 
     | Var of string
     | Value of string
     | Union of FlowExpression * FlowExpression
     | Cncat of FlowExpression * FlowExpression

type Call = Call of FlowExpression * int // int - ParseState
type ParseStack = class end // fix later (Parse Stack is a tripple (Graph, bottom, top))

let X0 = Value("a")
let s0 = 0
let startCall = Call(X0, 0)

//Initialize
let W = [startCall]
let F = new AdjacencyGraph<Call, Edge<Call>();
let Cache = new Dictionary<Call, ParseStack>()


let Algo = 
    //initialization
    F.AddVertex(startCall) 
    //--------------


    (* body of algorithm
    let reduce:ParseStack*ParseStack -> ParseStack 
    let compute: ParseState*FlowExpression -> P(ParseStack)
    X(s) := head(W ); W := tail(W );
    let X = E be the flow equation that matches X(s)
    P := computeX(s) (s, E );
    if P !⊆ Cache[X(s)]
        then Cache[X(s)] := Cache[X(s)] ∪ P ;
            forall X'(s') such that X(s) → X′(s′) ∈ F
            W := W + [X′(s′)];
    *)
    
    0
// now I must implemented ParseStack and function reduce and calculate   