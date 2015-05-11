open AbstractLALR
open ParserHelper

open ParserLAR
open ParserAR
open AbsStack
open ParserEvenCountBracket
open FlowExpression
open NUnit.Framework
open System.Collections.Generic
open Microsoft.FSharp.Text.Lexing
open System.Text

[<TestFixture>]
type ``AbstractLALR parser tests`` () =
    [<Test>]
    member this.``[a]-matching test`` ()=
        let FlowEquations1 =  new Dictionary<string, Expression<ParserLAR.token>>()

        FlowEquations1.Add("X0", Value(ParserLAR.A)) |> ignore
        FlowEquations1.Add("R" , Value(ParserLAR.R)) |> ignore  
        FlowEquations1.Add("X2", Value(ParserLAR.L) +. (Var("X0") +. Var("R"))) |> ignore   
        FlowEquations1.Add("X3", Var("X2")) |> ignore

        let x0 =  Call(FlowExpression("X3", FlowEquations1.["X3"]), 0)
        let res = algo x0 FlowEquations1 (CleverParseTable<ParserLAR.token>(ParserLAR.tables()))
        Assert.AreEqual (res, Accept)

    [<Test>]
    member this.``The correct bracket sequence`` ()=
        let FlowEquations1 =  new Dictionary<string, Expression<ParserLAR.token>>()

        FlowEquations1.Add("X0", Value(ParserLAR.A)) |> ignore
        FlowEquations1.Add("R" , Value(ParserLAR.R)) |> ignore  
        FlowEquations1.Add("X1", Var("X0") ++ Var("X2")) |> ignore   
        FlowEquations1.Add("X2", Value(ParserLAR.L) +. (Var("X1") +. Var("R"))) |> ignore   
        FlowEquations1.Add("X3", Var("X2")) |> ignore

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
    
    [<Test>]
    member this.AbstractStack () =
        let x = (AbstractStack(1) + AbstractStack(2) + AbstractStack(3)) + (AbstractStack(1) + AbstractStack(2) + AbstractStack(3))
        Assert.AreEqual (x.size, 6)
    
    
    [<Test>]
    member this.``The an even number of brackets: Example (b)(b) or (b)(b)(b)(b)``  () = 
         //a Shift-Reduce conflict which algorithm is not able to resolve on their own
         let FlowEquations1 =  new Dictionary<string, Expression<ParserEvenCountBracket.token>>()

         FlowEquations1.Add("X0", Value(ParserEvenCountBracket.L) +. Value(ParserEvenCountBracket.B) +. Value(ParserEvenCountBracket.R))
         FlowEquations1.Add("X1", Var("X0") +. Var("X0"))

         let x0 =  Call(FlowExpression("X1", FlowEquations1.["X1"]), 0)
         let res = algo x0 FlowEquations1 (CleverParseTable<ParserEvenCountBracket.token>(ParserEvenCountBracket.tables()))
         let mutable ans = Accept
         
         try
            //just yacc for control
            let text = "(B)(B)"
            let lexBuffer = LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(text))
            ParserEvenCountBracket.S Lexer.token lexBuffer |> ignore
         with e -> 
            ans <- Error
         
         Assert.AreEqual (res, ans) 
    [<Test>]
    member this.``The an even number of brackets: Example (b)(b) or (b)(b)(b)(b) Magic EOF``  () = 
         //a Shift-Reduce conflict which algorithm is not able to resolve on their own
         let FlowEquations1 =  new Dictionary<string, Expression<ParserEvenCountBracketMagicEOF.token>>()

         FlowEquations1.Add("X0", Value(ParserEvenCountBracketMagicEOF.L) +. Value(ParserEvenCountBracketMagicEOF.B) +. Value(ParserEvenCountBracketMagicEOF.R))
         FlowEquations1.Add("X1", Var("X0") +. Var("X0"))

         let x0 =  Call(FlowExpression("X1", FlowEquations1.["X1"]), 0)
         let res = algo x0 FlowEquations1 (CleverParseTable<ParserEvenCountBracketMagicEOF.token>(ParserEvenCountBracketMagicEOF.tables()))
         let mutable ans = Accept
         //If the parser to add a special character EOF, then yacc starts correctly resolve the conflict. But our algorithm not 
         //Our algo if LR(0), but fsyacc analyzer is LALR(1)      
         try
            //just yacc for control
            let text = "(B)(B)"
            let lexBuffer = LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(text))
            ParserEvenCountBracketMagicEOF.S LexerMagic.token lexBuffer |> ignore
         with e -> 
            ans <- Error
         
         Assert.AreNotEqual (res, ans) 
    [<Test>]
    member this.``Simple Calc`` ()=
        let FlowEquations1 =  new Dictionary<string, Expression<ParserSimpleCalc.token>>()

        FlowEquations1.Add("A", Value(ParserSimpleCalc.A)) |> ignore
        FlowEquations1.Add("M" , Value(ParserSimpleCalc.MINUS)) |> ignore 
        FlowEquations1.Add("P" , Value(ParserSimpleCalc.PLUS)) |> ignore 
        FlowEquations1.Add("X1" , Var("A") +. Var("M") +. Var("A")) |> ignore 
        FlowEquations1.Add("X2", Var("A") +. Var("P") +. Var("A")) |> ignore
        FlowEquations1.Add("X3", Var("X2") +. Var("P")) |> ignore


        let x0 =  Call(FlowExpression("X1", FlowEquations1.["X1"]), 0)
        let res = algo x0 FlowEquations1 (CleverParseTable<ParserSimpleCalc.token>(ParserSimpleCalc.tables()))
        Assert.AreEqual (res, Accept)

        let x1 =  Call(FlowExpression("X2", FlowEquations1.["X2"]), 0)
        let res1 = algo x1 FlowEquations1 (CleverParseTable<ParserSimpleCalc.token>(ParserSimpleCalc.tables()))
        Assert.AreEqual (res1, Accept)

        
        let x1 =  Call(FlowExpression("X3", FlowEquations1.["X3"]), 0)
        let res1 = algo x1 FlowEquations1 (CleverParseTable<ParserSimpleCalc.token>(ParserSimpleCalc.tables()))
        Assert.AreEqual (res1, Error)

    [<Test>]
    member this.``Full Calc`` ()=
        let FlowEquations1 =  new Dictionary<string, Expression<ParserCalc.token>>()

        FlowEquations1.Add("N", Value(ParserCalc.NUM)) |> ignore
        FlowEquations1.Add("M" , Value(ParserCalc.MINUS)) |> ignore 
        FlowEquations1.Add("P" , Value(ParserCalc.PLUS)) |> ignore 
        FlowEquations1.Add("L" , Value(ParserCalc.L)) |> ignore 
        FlowEquations1.Add("R" , Value(ParserCalc.R)) |> ignore 
        FlowEquations1.Add("ID" , Value(ParserCalc.ID)) |> ignore 

        FlowEquations1.Add("X1" , Var("N") +. Var("M") +. Var("N")) |> ignore 
        let x0 =  Call(FlowExpression("X1", FlowEquations1.["X1"]), 0)
        let res = algo x0 FlowEquations1 (CleverParseTable<ParserCalc.token>(ParserCalc.tables()))
        Assert.AreEqual (res, Accept)
        
        FlowEquations1.Add("X2", Var("N") +. Var("P") +. Var("N")) |> ignore
        let x1 =  Call(FlowExpression("X2", FlowEquations1.["X2"]), 0)
        let res1 = algo x1 FlowEquations1 (CleverParseTable<ParserCalc.token>(ParserCalc.tables()))
        Assert.AreEqual (res1, Accept)

        FlowEquations1.Add("X3", Var("X2") +. Var("P")) |> ignore
        let x1 =  Call(FlowExpression("X3", FlowEquations1.["X3"]), 0)
        let res1 = algo x1 FlowEquations1 (CleverParseTable<ParserCalc.token>(ParserCalc.tables()))
        Assert.AreEqual (res1, Error)

        FlowEquations1.Add("X4", Var("N") +. Var("P") +. Var("N") +. Var("P") +. Var("ID") )
        let x2 =  Call(FlowExpression("X4", FlowEquations1.["X4"]), 0)
        let res2 = algo x2 FlowEquations1 (CleverParseTable<ParserCalc.token>(ParserCalc.tables()))
        Assert.AreEqual (res2, Accept)

        
        FlowEquations1.Add("X5", Var("L") +. Var("ID") +. Var("R") )
        let x2 =  Call(FlowExpression("X5", FlowEquations1.["X5"]), 0)
        let res2 = algo x2 FlowEquations1 (CleverParseTable<ParserCalc.token>(ParserCalc.tables()))
        Assert.AreEqual (res2, Accept)

        FlowEquations1.Add("X6", Var("X5") +. Var("P") +. Var("X5"))
        let x2 =  Call(FlowExpression("X6", FlowEquations1.["X6"]), 0)
        let res2 = algo x2 FlowEquations1 (CleverParseTable<ParserCalc.token>(ParserCalc.tables()))
        Assert.AreEqual (res2, Accept)

        
        FlowEquations1.Add("X7", Var("L") +. Var("X6") +. Var("R")) // ((N) + (N))
        let x2 =  Call(FlowExpression("X7", FlowEquations1.["X7"]), 0)
        let res2 = algo x2 FlowEquations1 (CleverParseTable<ParserCalc.token>(ParserCalc.tables()))
        Assert.AreEqual (res2, Accept)

        FlowEquations1.Add("X8", Var("X7") +. Var("P") +. Var("X7")) // ((N) + (N)) + ((N) + (N))
        let x2 =  Call(FlowExpression("X8", FlowEquations1.["X8"]), 0)
        let res2 = algo x2 FlowEquations1 (CleverParseTable<ParserCalc.token>(ParserCalc.tables()))
        Assert.AreEqual (res2, Accept)

        FlowEquations1.Add("X9", Var("L") +. Var("ID") +. Var("P") +. Var("N") +. Var("R")) // (ID + N)
        let x2 =  Call(FlowExpression("X9", FlowEquations1.["X9"]), 0)
        let res2 = algo x2 FlowEquations1 (CleverParseTable<ParserCalc.token>(ParserCalc.tables()))
        Assert.AreEqual (res2, Accept)

        FlowEquations1.Add("X10", Var("X9") +. Value(ParserCalc.MINUS) +. Var("X9")) // (ID + N) - (ID + num)
        let x2 =  Call(FlowExpression("X10", FlowEquations1.["X10"]), 0)
        let res2 = algo x2 FlowEquations1 (CleverParseTable<ParserCalc.token>(ParserCalc.tables()))
        Assert.AreEqual (res2, Accept)


let t = new ``AbstractLALR parser tests`` () 
t.``The correct bracket sequence``() 
t.``[a]-matching test`` ()
t.``The correct bracket sequence without left bracket``()
t.AbstractStack()
t.``The an even number of brackets: Example (b)(b) or (b)(b)(b)(b) Magic EOF``()
t.``Simple Calc``()
t.``Full Calc``()