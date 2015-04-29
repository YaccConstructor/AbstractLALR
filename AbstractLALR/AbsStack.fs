module AbsStack 

open QuickGraph
open QuickGraph.Algorithms
open QuickGraph.Collections
open System
open System.Collections.Generic
    
type vertex = int*int // index, target(ParseState)

type AbstractStack(_g:AdjacencyGraph<vertex, Edge<vertex>>, state:int) =
        
    let mutable g = _g
    let mutable Top = None 
    let mutable Bot = None
    let mutable Folded = false
    do 
        if(state <> -1) then
            g.AddVertex(1,state) |> ignore
            Top <- Some (1,state)
            Bot <- Some (1,state)


    new(state:int) = AbstractStack(new AdjacencyGraph<vertex, Edge<vertex>>(), state)
    //create empty stack
    new() = AbstractStack(new AdjacencyGraph<vertex, Edge<vertex>>(), -1) 
    member this.top with get() = (if (Top.IsSome) then 
                                        Top.Value 
                                    else (-1,-1)) 
                                    and set(v) = Top <- Some(v)
    member this.topState with get() = if (Top.IsSome) then 
                                        snd Top.Value
                                       else -1
                                    
    member this.bot with get() = (if (Bot.IsSome) then 
                                        Bot.Value 
                                    else (-1,-1)) 
                                    and set(v) = Bot <- Some(v)
    member this.folded with get() = Folded
    member this.graph with get() = g and set(v) = g <- v
        
    member this.isEmpty = this.graph.VertexCount = 0

    member this.fold =
        if (not Folded) then
            Folded <- true
            let setNodes = new HashSet<int>()
            for v in this.graph.Vertices do
                snd v |> setNodes.Add  |> ignore
            
            let setEdges = new HashSet<int*int>()
            for e in this.graph.Edges do
                    (snd e.Source, snd e.Target) |> setEdges.Add  |> ignore
               

            let newGr = new AdjacencyGraph<vertex, Edge<vertex>>()
            for v in setNodes do 
                (v, v) |> newGr.AddVertex |> ignore

            for (x,y) in setEdges do 
                Edge ((x, x), (y , y)) |> newGr.AddEdge  |> ignore

            this.graph <- newGr
        //labeled
            this.bot <- (snd this.bot, snd this.bot)
            this.top <- (snd this.top, snd this.top)

    //union operator
    static member (+) (a:AbstractStack, b:AbstractStack) = 
         
        let c = new AbstractStack()
        if (a.isEmpty && b.isEmpty) then
            c 
        else if (a.isEmpty) then
                b 
        else if (b.isEmpty) then
                a 
        else
            let maxLabel (g:AdjacencyGraph<vertex, Edge<vertex>>) t =
                let mutable maxLabel = 0
                for (a,b) in g.Vertices do
                    if (b = t)  then 
                        maxLabel <- a
                maxLabel
                    
            let tempBGraph = b.graph.Clone()
            for v in b.graph.Vertices do 
                if a.graph.ContainsVertex v then
                    tempBGraph.AddVertex (maxLabel a.graph (snd v) + 1, snd v) |> ignore
                    tempBGraph.RemoveVertex v |> ignore


            for e in b.graph.Edges do
                if a.graph.ContainsVertex e.Source then
                    let newSource = (maxLabel a.graph (snd e.Source) + 1, snd e.Source)
                    tempBGraph.AddEdge (Edge(newSource, e.Target)) |> ignore

                if a.graph.ContainsVertex e.Target then 
                    let newTarget = (maxLabel a.graph (fst e.Target) + 1, snd e.Target)
                    tempBGraph.AddEdge (Edge(e.Source, newTarget)) |> ignore
                        
                                       
            a.graph.Vertices |> c.graph.AddVertexRange |> ignore
            tempBGraph.Vertices |> c.graph.AddVertexRange |> ignore

            a.graph.Edges |> c.graph.AddEdgeRange |> ignore
            tempBGraph.Edges |> c.graph.AddEdgeRange |> ignore

                
            let newBbot = 
                if a.graph.ContainsVertex b.bot then
                    (maxLabel a.graph (snd b.bot) + 1, snd b.bot)
                else b.bot
            let newBtop = 
                if a.graph.ContainsVertex b.top then
                    (maxLabel a.graph (snd b.top) + 1, snd b.top)
                else b.top
                            
            Edge(newBbot, a.top) |> c.graph.AddEdge  |> ignore

            c.bot <- a.bot
            c.top <- newBtop
            c 
                

    member this.predecessor state = 
        let pred = new HashSet<vertex>()
        for e in this.graph.Edges do
                if (e.Source = state) then
                    pred.Add e.Target |> ignore
        pred
    member this.size = this.graph.VertexCount

    member this.pop =
        let mutable pred = (-1,-1)
        for e in this.graph.Edges do
            if (e.Source = this.top) then
                pred <- e.Target
     
        this.top <- pred   
        
    member this.Clone = 
        let c = new AbstractStack(this.graph.Clone(), -1)
        c.top = this.top |> ignore
        c.bot = this.bot |> ignore
        c

    member this.print = 
        this.graph.Vertices |> printfn "Vertices: %A"
        this.graph.Edges |> printfn "Edges: %A"
        this.top |> printfn "Top: %A"
        this.bot |> printfn "Bot: %A"

    member this.Equals (a:AbstractStack) = 
        let findInGr (gr:AdjacencyGraph<int*int, Edge<int*int>>) a b =
            let mutable flag = false
            for e in gr.Edges do
                if (e.Source = a) && (e.Target = b) then 
                    flag <- true
            flag

        let mutable flag = true
        for v in this.graph.Vertices do
            if not (a.graph.ContainsVertex v) then
                flag <- false 
        let mutable flag1 = true
        for v in a.graph.Vertices do
            if not (this.graph.ContainsVertex v) then
                flag1 <- false 

        let mutable flag2 = true
        for e in this.graph.Edges do
            if not (findInGr a.graph e.Source e.Target) then
                flag2 <- false 
        let mutable flag3 = true
        for e in a.graph.Edges do
            if not (findInGr this.graph e.Source e.Target) then
                flag3 <- false 
        
        (flag && flag1 && flag3 && flag2)

                
