README:
===============================================================================
MAKEFILE
made a basic makefile to run the program, type make. 
Makefile needs to be ammended for submission. 

===============================================================================
CUSTOM DATA TYPES: 
concerning custom data types, there are two ways to do it here. 
type ASTTypeDenoter = TypeDenoter
data TypeDenoter =
    OrdinaryTypeDenoter ASTTypeIdentifier |
    ArrayTypeDenoter 

In this situation, within our code, we are going to have something like:
x <- parseTypeIdentifier
return (OrdinaryTypeDenoter x)

because we are not parsing tokens. 

If we parse tokens, do it this way:
type ASTSign = Sign
data Sign =
    SignPlus |
    SignMinus
    deriving(Show)

parseTokenPlus
return SignPlus

===============================================================================
PARSERS NOT DIRECTLY TAKEN FROM BNF. 
type ASTAssignmentStatementHead was added by me, not directly translated from BNF. 
Helped me to implement parser for parseAssignmentStatement. 