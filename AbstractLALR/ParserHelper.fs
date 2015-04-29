module ParserHelper 

open Microsoft.FSharp.Text.Lexing

open System
open System.Collections.Generic

let anyMarker = 0xffff
let shiftFlag = 0x0000
let reduceFlag = 0x4000
let errorFlag = 0x8000
let acceptFlag = 0xc000
let actionMask = 0xc000

let actionValue action = action &&& (~~~ actionMask)                                    
let actionKind action = action &&& actionMask

type AssocTable(elemTab:uint16[], offsetTab:uint16[]) =
        let cache = new Dictionary<_,_>(2000)

        member t.readAssoc (minElemNum,maxElemNum,defaultValueOfAssoc,keyToFind) =     
            // do a binary chop on the table 
            let elemNumber : int = (minElemNum + maxElemNum)/2
            if elemNumber = maxElemNum 
            then defaultValueOfAssoc
            else 
                let x = int elemTab.[elemNumber*2]
                if keyToFind = x then 
                    int elemTab.[elemNumber*2+1]
                elif keyToFind < x then t.readAssoc (minElemNum ,elemNumber,defaultValueOfAssoc,keyToFind)
                else                    t.readAssoc (elemNumber+1,maxElemNum,defaultValueOfAssoc,keyToFind)

        member t.Read(rowNumber ,keyToFind) =
        
            // First check the sparse lookaside table
            // Performance note: without this lookaside table the binary chop in readAssoc
            // takes up around 10% of of parsing time 
            // for parsing intensive samples such as the bootstrapped F# compiler.
            //
            // Note: using a .NET Dictionary for this int -> int table looks like it could be sub-optimal.
            // Some other better sparse lookup table may be better.
            let mutable res = 0 
            let cacheKey = (rowNumber <<< 16) ||| keyToFind
            let ok = cache.TryGetValue(cacheKey, &res) 
            if ok then res 
            else
                let headOfTable = int offsetTab.[rowNumber]
                let firstElemNumber = headOfTable + 1           
                let numberOfElementsInAssoc = int elemTab.[headOfTable*2]
                let defaultValueOfAssoc = int elemTab.[headOfTable*2+1]          
                let res = t.readAssoc (firstElemNumber,(firstElemNumber+numberOfElementsInAssoc),defaultValueOfAssoc,keyToFind)
                cache.[cacheKey] <- res
                res

        // Read all entries in the association table
        // Used during error recovery to find all valid entries in the table
        member x.ReadAll(n) =       
            let headOfTable = int offsetTab.[n]
            let firstElemNumber = headOfTable + 1           
            let numberOfElementsInAssoc = int32 elemTab.[headOfTable*2]           
            let defaultValueOfAssoc = int elemTab.[headOfTable*2+1]          
            [ for i in firstElemNumber .. (firstElemNumber+numberOfElementsInAssoc-1) -> 
                (int elemTab.[i*2], int elemTab.[i*2+1]) ], defaultValueOfAssoc

    type IdxToIdxListTable(elemTab:uint16[], offsetTab:uint16[]) =

        // Read all entries in a row of the table
        member x.ReadAll(n) =       
            let headOfTable = int offsetTab.[n]
            let firstElemNumber = headOfTable + 1           
            let numberOfElements = int32 elemTab.[headOfTable]           
            [ for i in firstElemNumber .. (firstElemNumber+numberOfElements-1) -> int elemTab.[i] ]


type CleverParseTable<'T>(tables: Microsoft.FSharp.Text.Parsing.Tables<'T>) =
    let gotoTable = new AssocTable(tables.gotos, tables.sparseGotoTableRowOffsets)
    let actionTable = new AssocTable(tables.actionTableElements, tables.actionTableRowOffsets)

    member this.gotoTerminal state terminal = 
        let tag = tables.tagOfToken terminal                      
        let action = actionTable.Read(state,tag)
        let newState = actionValue action
        newState

    member this.action state = int (tables.immediateActions.[state])
        
    member this.production state = 
        let prod = actionValue (this.action state)
        prod

    member this.reductionSymbolCount state =
        let n = int (tables.reductionSymbolCounts.[this.production state])
        n

    member this.gotoNonTerminal state t = gotoTable.Read(int (tables.productionToNonTerminalTable.[(this.production t)]), state)

    member this.isAccept state = 
            let immediateAction = int (tables.immediateActions.[state])
            if immediateAction <> anyMarker then
                let kind = actionKind (immediateAction)
                if kind = acceptFlag then
                    true
                else false
            else 
                let mutable flag = false

                let action = actionTable.Read(state, tables.endOfInputTag )
                if (actionKind action) = acceptFlag then
                    flag <- true
                flag  

