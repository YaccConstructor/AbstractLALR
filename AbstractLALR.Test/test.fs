open AbstractLALR
open ParserHelper

open ParserLAR
open ParserAR
open FlowExpression
open NUnit.Framework
open System.Collections.Generic

[<TestFixture>]
type ``AbstractLALR parser tests`` () =
    [<Test>]
    member this.``The correct bracket sequence`` ()=
        let FlowEquations1 =  new Dictionary<string, Expression<ParserLAR.token>>()

        FlowEquations1.Add("X0", Value(ParserLAR.A)) |> ignore
        FlowEquations1.Add("R" , Value(ParserLAR.R)) |> ignore  
        FlowEquations1.Add("X1", Var("X0") ++ Var("X2")) |> ignore   
        FlowEquations1.Add("X2", Value(ParserLAR.L) +. (Var("X1") +. Var("R"))) |> ignore   
        FlowEquations1.Add("X3", Var("X1")) |> ignore

        let x0 =  Call(FlowExpression("X3", FlowEquations1.["X3"]), 0)
        let res = algo x0 FlowEquations1 (CleverParseTable<ParserLAR.token>(ParserLAR.tables()))
        Assert.AreEqual (res, Accept)
    [<Test>]
    member this.``The correct bracket sequence without left bracket`` () = 
        let FlowEquations1 =  new Dictionary<string, Expression<ParserAR.token>>()

        FlowEquations1.Add("X0", Value(ParserAR.A)) |> ignore
        FlowEquations1.Add("R" , Value(ParserAR.R)) |> ignore  
        FlowEquations1.Add("X1", Var("X0") ++ Var("X2")) |> ignore   
        FlowEquations1.Add("X2", (Var("X1") +. Var("R"))) |> ignore   
        FlowEquations1.Add("X3", Var("X1")) |> ignore

        let x0 =  Call(FlowExpression("X3", FlowEquations1.["X3"]), 0)
        let res = algo x0 FlowEquations1 (CleverParseTable<ParserAR.token>(ParserAR.tables()))
        Assert.AreEqual (res, Accept)
       



let t = new ``AbstractLALR parser tests`` () 
t.``The correct bracket sequence``() 
t.``The correct bracket sequence without left bracket``()

