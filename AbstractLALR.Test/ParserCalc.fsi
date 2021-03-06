// Signature file for parser generated by fsyacc
module ParserCalc
type token = 
  | L
  | R
  | MINUS
  | PLUS
  | NUM
  | ID
type tokenId = 
    | TOKEN_L
    | TOKEN_R
    | TOKEN_MINUS
    | TOKEN_PLUS
    | TOKEN_NUM
    | TOKEN_ID
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startS
    | NONTERM_S
    | NONTERM_OP
    | NONTERM_T
/// This function maps integers indexes to symbolic token ids
val tagOfToken: token -> int

/// This function maps integers indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val S : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (int) 
