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
      
        member a.fold =  
            let setNodes = new HashSet<int>()
            for v in a.g.Vertices do
            setNodes.Add (snd v) |> ignore

            let setEdges = new HashSet<int*int>()
            for e in a.g.Edges do
                if (snd e.Source <> snd e.Target) then setEdges.Add( (snd e.Source), (snd e.Target)) |> ignore

            let newGr = new AdjacencyGraph<vertex, Edge<vertex>>()
            for v in setNodes do 
                newGr.AddVertex((v, v)) |> ignore

            for e in setEdges do 
                newGr.AddEdge(Edge ( (fst e, fst e) , (snd e , snd e))  )|> ignore

            a.g <- newGr
            //labeled
            a.bot <- (fst a.bot, fst a.bot)
            a.top <- (fst a.top, fst a.bot)

        //unuon operator
        static member (+) (a:AbsStack, b:AbsStack) = 
            let c = new AbsStack()
            for v in a.g.Vertices do
                c.g.AddVertex(v) |> ignore

            for v in b.g.Vertices do
                c.g.AddVertex(v) |> ignore

            for e in a.g.Edges do
                c.g.AddEdge(e) |> ignore

            for e in b.g.Vertices do
                c.g.AddVertex(e) |> ignore

            c.g.AddEdge (Edge((b.bot), (a.top))) |> ignore
            c.bot <- a.bot
            c.top <- b.top
            c







            
    

