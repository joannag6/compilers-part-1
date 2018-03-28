TODO:
1. Parsers to create AST
	- Create parser for empty statement. 
	- procedure_statement, empty_statement and identifier under variable_access
	  have special rules in the comment of PazParserBNF.txt that cannot be described 
	  just by using BNF notation. Need to find a way to implement it.
	- Currently to run it, we type make to build everything. Then we type PazParserTest <pazprogramname.paz.
	  Need to find a way to change this to match the spec. 

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
Helped me to implement parser for parseAssignmentStatement. 

4. When PazParserBNF open in your code editor, set the language to Haskell for relatively useful highlighting.

===========================================================================
