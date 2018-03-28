TODO:
1. Parsers to create AST
	- Create parser for empty statement. 


2. Creating CST

3. Pretty printing


===========================================================================
RANDOM TIPS
1. MAKEFILE
made a basic makefile to run the program, type make. 
Makefile needs to be ammended for submission. 

2. CUSTOM DATA TYPES: 
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


3. PARSERS NOT DIRECTLY TAKEN FROM BNF. 
type ASTAssignmentStatementHead was added by me, not directly translated from BNF. 

4. When PazParserBNF open in your code editor, set the language to Haskell for relatively useful highlighting.

===========================================================================

Helped me to implement parser for parseAssignmentStatement. 