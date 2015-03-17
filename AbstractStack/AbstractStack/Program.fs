open QuickGraph
open QuickGraph.Algorithms
open QuickGraph.Collections
open System
open System.Collections.Generic


module AbsStack =
    
    type vertex = int*int // index, target(ParseState)

    type AbsStack() =
        
        member val g = new AdjacencyGraph<vertex, Edge<vertex>>() with get, set
        member val top = (1,1) with get, set//fixed later
        member val bot = (1,1) with get, set//fixed later
      
        member this.fold =  
            let setNodes = new HashSet<int>()
            for v in this.g.Vertices do
                snd v |> setNodes.Add  |> ignore

            let setEdges = new HashSet<int*int>()
            for e in this.g.Edges do
                if (snd e.Source <> snd e.Target) then 
                   (snd e.Source, snd e.Target) |> setEdges.Add  |> ignore

            let newGr = new AdjacencyGraph<vertex, Edge<vertex>>()
            for v in setNodes do 
                (v, v) |> newGr.AddVertex |> ignore

            for (x,y) in setEdges do 
                Edge ((x, x), (y , y)) |> newGr.AddEdge  |> ignore

            this.g <- newGr
            //labeled
            this.bot <- (fst this.bot, fst this.bot)
            this.top <- (fst this.top, fst this.bot)

        //union operator
        static member (+) (a:AbsStack, b:AbsStack) = 
            let c = new AbsStack()
            a.g.Edges |> c.g.AddVerticesAndEdgeRange |> ignore
            b.g.Edges |> c.g.AddVerticesAndEdgeRange |> ignore

            Edge(b.bot, a.top) |> c.g.AddEdge  |> ignore

            c.bot <- a.bot
            c.top <- b.top
            c







            
    

