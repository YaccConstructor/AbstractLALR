fsyacc.exe ParserAR.fsy -o ParserAR.fs --module ParserAR
fsyacc.exe ParserLAR.fsy -o ParserLAR.fs --module ParserLAR
fsyacc.exe ParserEvenCountBracket.fsy -o ParserEvenCountBracket.fs --module ParserEvenCountBracket  

fsyacc.exe ParserEvenCountBracketMagicEOF.fsy -o ParserEvenCountBracketMagicEOF.fs --module ParserEvenCountBracketMagicEOF -v 

fsyacc.exe ParserCalc.fsy -o ParserCalc.fs --module ParserCalc
fsyacc.exe ParserSimpleCalc.fsy -o ParserSimpleCalc.fs --module ParserSimpleCalc 
fsyacc.exe ParserIfElse.fsy -o ParserIfElse.fs --module ParserIfElse 


