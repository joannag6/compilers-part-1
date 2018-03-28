program hello;

begin
        writeln('Hello, world!')
end.


do variable_access
then do factor


README:
made a basic makefile to run the program, type make. 


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