// Implementation file for parser generated by fsyacc
module ParserEvenCountBracket
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 "ParserEvenCountBracket.fsy"


# 9 "ParserEvenCountBracket.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | B
  | R
  | L
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_B
    | TOKEN_R
    | TOKEN_L
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startS
    | NONTERM_S

// This function maps tokens to integers indexes
let tagOfToken (t:token) = 
  match t with
  | B  -> 0 
  | R  -> 1 
  | L  -> 2 

// This function maps integers indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_B 
  | 1 -> TOKEN_R 
  | 2 -> TOKEN_L 
  | 5 -> TOKEN_end_of_input
  | 3 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startS 
    | 1 -> NONTERM_S 
    | 2 -> NONTERM_S 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 5 
let _fsyacc_tagOfErrorTerminal = 3

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | B  -> "B" 
  | R  -> "R" 
  | L  -> "L" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | B  -> (null : System.Object) 
  | R  -> (null : System.Object) 
  | L  -> (null : System.Object) 
let _fsyacc_gotos = [| 0us; 65535us; 2us; 65535us; 0us; 1us; 7us; 8us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 2us; 1us; 2us; 2us; 1us; 2us; 2us; 1us; 2us; 2us; 1us; 2us; 2us; 1us; 2us; 2us; 1us; 2us; 1us; 2us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 7us; 10us; 13us; 16us; 19us; 22us; |]
let _fsyacc_action_rows = 9
let _fsyacc_actionTableElements = [|1us; 32768us; 2us; 2us; 0us; 49152us; 1us; 32768us; 0us; 3us; 1us; 32768us; 1us; 4us; 1us; 32768us; 2us; 5us; 1us; 32768us; 0us; 6us; 1us; 32768us; 1us; 7us; 1us; 16385us; 2us; 2us; 0us; 16386us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 2us; 3us; 5us; 7us; 9us; 11us; 13us; 15us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 6us; 7us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 1us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16386us; |]
let _fsyacc_reductions ()  =    [| 
# 79 "ParserEvenCountBracket.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startS));
# 88 "ParserEvenCountBracket.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 14 "ParserEvenCountBracket.fsy"
                                      1
                   )
# 14 "ParserEvenCountBracket.fsy"
                 : int));
# 98 "ParserEvenCountBracket.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _7 = (let data = parseState.GetInput(7) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 14 "ParserEvenCountBracket.fsy"
                                                           1
                   )
# 14 "ParserEvenCountBracket.fsy"
                 : int));
|]
# 110 "ParserEvenCountBracket.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 6;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let S lexer lexbuf : int =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
