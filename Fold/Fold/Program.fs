open QuickGraph
open QuickGraph.Algorithms
open QuickGraph.Collections
open System
open System.Collections.Generic


let fold (gr: AdjacencyGraph<int*int, Edge<int*int>>) = 
    let setNodes = new HashSet<int>()
    for v in gr.Vertices do
        setNodes.Add (snd v) |> ignore

    let setEdges = new HashSet<int*int>()
    for e in gr.Edges do
        if (snd e.Source <> snd e.Target) then setEdges.Add( (snd e.Source), (snd e.Target)) |> ignore

    let newGr = new AdjacencyGraph<int*int, Edge<int*int>>()
    for v in setNodes do 
        newGr.AddVertex((v,v)) |> ignore

    for e in setEdges do 
        newGr.AddEdge(Edge ( (fst e, fst e) , (snd e, snd e))) |> ignore

    newGr





let test  =
 
    let g = new AdjacencyGraph<int*int, Edge<int*int>>()
    g.AddVertex (1,1)  |> ignore
    g.AddVertex (2,1) |> ignore
    g.AddVertex (3,1) |> ignore
    
    
    g.AddEdge (Edge((1,1), (2,1)))|> ignore
    g.AddEdge (Edge((2,1), (3,1))) |> ignore
    

    g.AddVertex (6,2) |> ignore
    g.AddVertex (7,2) |> ignore
    
    g.AddEdge (Edge( (2,1), (6,2) )) |> ignore
    g.AddEdge (Edge( (3,1), (7,2) )) |> ignore

    g.AddVertex (6,3) |> ignore
    
    g.AddEdge (Edge( (1,1), (6,3) )) |> ignore

    let foldedGr = fold g
    for v in foldedGr.Vertices do
        v |> printfn "%A"

    for e in foldedGr.Edges do
        printfn "%A %A" e.Source e.Target
   
    

test

