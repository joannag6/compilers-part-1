module PazParser where

import Debug.Trace (trace)
import Text.Parsec (
    Parsec,
    SourcePos,
    choice,
    eof,
    optionMaybe,
    optional,
    parse,
    tokenPrim,
    try
    )
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (void)
import Control.Applicative (many) -- get <|> from here too if needed
import PazLexer (
    ASTLexicalToken,
    LexicalToken(..),
    ASTCharacterString,
    ASTIdentifier,
    ASTUnsignedInteger,
    ASTUnsignedReal
    )

-- define a parser which parses an incoming stream of ASTLexicalToken,
-- noting that ASTLexicalToken is a pair of SourcePos and LexicalToken
type Parser = Parsec [ASTLexicalToken] ()

-- since we are not processing characters we must define "satisfy" ourself,
-- tokenPrim takes a show function, an update function, and test function
satisfy ::
     -- the following type constraint is some kind of Haskell gobbledygook,
     -- to make it work we'd have to turn on the FlexibleContents feature:
     --(Stream s m ASTLexicalToken) =>
     (LexicalToken -> Bool) -> Parser LexicalToken
satisfy f =
     tokenPrim
         (\(_, x) -> show x)
         updatePosASTLexicalToken
         (\(_, x) -> if f x then Just x else Nothing)

-- updating the source position is simply extracing it from the coming
-- ASTLexicalToken in the input stream, if there is no next parseToken then
-- use current (i.e. last) parseToken, I am sure there would be a better way
updatePosASTLexicalToken ::
    SourcePos -> ASTLexicalToken -> [ASTLexicalToken] -> SourcePos
updatePosASTLexicalToken _ _ ((pos, _) : _) = pos
updatePosASTLexicalToken _ (pos, _) [] = pos

parseTokenEof :: Parser ()
parseTokenEof = eof

-- lexical tokens section
-- these are basically wrappers that check for each kind of lexical parseToken
-- already recognized by PazLexer.hs and then regurgitate it, those
-- starting with "parseToken" throw away the ASTLexicalToken, those starting
-- with "parse" unwrap the ASTLexicalToken and then the LexicalToken

parseTokenLeftParenthesis :: Parser ()
parseTokenLeftParenthesis =
    void (
        satisfy (
            \x ->
                case x of
                    LTLeftParenthesis -> True
                    otherwise -> False
            )
        )
    
parseTokenRightParenthesis :: Parser ()
parseTokenRightParenthesis =
    void (
        satisfy (
            \x ->
                case x of
                    LTRightParenthesis -> True
                    otherwise -> False
            )
        )
    
parseTokenTimes :: Parser ()
parseTokenTimes =
    void (
        satisfy (
            \x ->
                case x of
                    LTTimes -> True
                    otherwise -> False
            )
        )
    
parseTokenPlus :: Parser ()
parseTokenPlus =
    void (
        satisfy (
            \x ->
                case x of
                    LTPlus -> True
                    otherwise -> False
            )
        )
    
parseTokenComma :: Parser ()
parseTokenComma =
    void (
        satisfy (
            \x ->
                case x of
                    LTComma -> True
                    otherwise -> False
            )
        )
    
parseTokenMinus :: Parser ()
parseTokenMinus =
    void (
        satisfy (
            \x ->
                case x of
                    LTMinus -> True
                    otherwise -> False
            )
        )
    
parseTokenEllipsis :: Parser ()
parseTokenEllipsis =
    void (
        satisfy (
            \x ->
                case x of
                    LTEllipsis -> True
                    otherwise -> False
            )
        )
    
parseTokenDot :: Parser ()
parseTokenDot =
    void (
        satisfy (
            \x ->
                case x of
                    LTDot -> True
                    otherwise -> False
            )
        )
    
parseTokenDivideBy :: Parser ()
parseTokenDivideBy =
    void (
        satisfy (
            \x ->
                case x of
                    LTDivideBy -> True
                    otherwise -> False
            )
        )
    
parseTokenAssign :: Parser ()
parseTokenAssign =
    void (
        satisfy (
            \x ->
                case x of
                    LTAssign -> True
                    otherwise -> False
            )
        )
    
parseTokenColon :: Parser ()
parseTokenColon =
    void (
        satisfy (
            \x ->
                case x of
                    LTColon -> True
                    otherwise -> False
            )
        )
    
parseTokenSemicolon :: Parser ()
parseTokenSemicolon =
    void (
        satisfy (
            \x ->
                case x of
                    LTSemicolon -> True
                    otherwise -> False
            )
        )
    
parseTokenLessThanOrEqual :: Parser ()
parseTokenLessThanOrEqual =
    void (
        satisfy (
            \x ->
                case x of
                    LTLessThanOrEqual -> True
                    otherwise -> False
            )
        )
    
parseTokenNotEqual :: Parser ()
parseTokenNotEqual =
    void (
        satisfy (
            \x ->
                case x of
                    LTNotEqual -> True
                    otherwise -> False
            )
        )
    
parseTokenLessThan :: Parser ()
parseTokenLessThan =
    void (
        satisfy (
            \x ->
                case x of
                    LTLessThan -> True
                    otherwise -> False
            )
        )
    
parseTokenEqual :: Parser ()
parseTokenEqual =
    void (
        satisfy (
            \x ->
                case x of
                    LTEqual -> True
                    otherwise -> False
            )
        )
    
parseTokenGreaterThanOrEqual :: Parser ()
parseTokenGreaterThanOrEqual =
    void (
        satisfy (
            \x ->
                case x of
                    LTGreaterThanOrEqual -> True
                    otherwise -> False
            )
        )
    
parseTokenGreaterThan :: Parser ()
parseTokenGreaterThan =
    void (
        satisfy (
            \x ->
                case x of
                    LTGreaterThan -> True
                    otherwise -> False
            )
        )
    
parseTokenLeftBracket :: Parser ()
parseTokenLeftBracket =
    void (
        satisfy (
            \x ->
                case x of
                    LTLeftBracket -> True
                    otherwise -> False
            )
        )
    
parseTokenRightBracket :: Parser ()
parseTokenRightBracket =
    void (
        satisfy (
            \x ->
                case x of
                    LTRightBracket -> True
                    otherwise -> False
            )
        )
    
parseTokenAnd :: Parser ()
parseTokenAnd =
    void (
        satisfy (
            \x ->
                case x of
                    LTAnd -> True
                    otherwise -> False
            )
        )

parseTokenArray :: Parser ()
parseTokenArray =
    void (
        satisfy (
            \x ->
                case x of
                    LTArray -> True
                    otherwise -> False
            )
        )

parseTokenBegin :: Parser ()
parseTokenBegin =
    void (
        satisfy (
            \x ->
                case x of
                    LTBegin -> True
                    otherwise -> False
            )
        )

parseTokenBoolean :: Parser ()
parseTokenBoolean =
    void (
        satisfy (
            \x ->
                case x of
                    LTBoolean -> True
                    otherwise -> False
            )
        )

parseTokenDiv :: Parser ()
parseTokenDiv =
    void (
        satisfy (
            \x ->
                case x of
                    LTDiv -> True
                    otherwise -> False
            )
        )

parseTokenDo :: Parser ()
parseTokenDo =
    void (
        satisfy (
            \x ->
                case x of
                    LTDo -> True
                    otherwise -> False
            )
        )

parseTokenDownTo :: Parser ()
parseTokenDownTo =
    void (
        satisfy (
            \x ->
                case x of
                    LTDownTo -> True
                    otherwise -> False
            )
        )

parseTokenElse :: Parser ()
parseTokenElse =
    void (
        satisfy (
            \x ->
                case x of
                    LTElse -> True
                    otherwise -> False
            )
        )

parseTokenEnd :: Parser ()
parseTokenEnd =
    void (
        satisfy (
            \x ->
                case x of
                    LTEnd -> True
                    otherwise -> False
            )
        )

parseTokenFor :: Parser ()
parseTokenFor =
    void (
        satisfy (
            \x ->
                case x of
                    LTFor -> True
                    otherwise -> False
            )
        )

parseTokenFunction :: Parser ()
parseTokenFunction =
    void (
        satisfy (
            \x ->
                case x of
                    LTFunction -> True
                    otherwise -> False
            )
        )

parseTokenIf :: Parser ()
parseTokenIf =
    void (
        satisfy (
            \x ->
                case x of
                    LTIf -> True
                    otherwise -> False
            )
        )

parseTokenInteger :: Parser ()
parseTokenInteger =
    void (
        satisfy (
            \x ->
                case x of
                    LTInteger -> True
                    otherwise -> False
            )
        )

parseTokenNot :: Parser ()
parseTokenNot =
    void (
        satisfy (
            \x ->
                case x of
                    LTNot -> True
                    otherwise -> False
            )
        )

parseTokenOf :: Parser ()
parseTokenOf =
    void (
        satisfy (
            \x ->
                case x of
                    LTOf -> True
                    otherwise -> False
            )
        )

parseTokenOr :: Parser ()
parseTokenOr =
    void (
        satisfy (
            \x ->
                case x of
                    LTOr -> True
                    otherwise -> False
            )
        )

parseTokenProcedure :: Parser ()
parseTokenProcedure =
    void (
        satisfy (
            \x ->
                case x of
                    LTProcedure -> True
                    otherwise -> False
            )
        )

parseTokenProgram :: Parser ()
parseTokenProgram =
    void (
        satisfy (
            \x ->
                case x of
                    LTProgram -> True
                    otherwise -> False
            )
        )

parseTokenReal :: Parser ()
parseTokenReal =
    void (
        satisfy (
            \x ->
                case x of
                    LTReal -> True
                    otherwise -> False
            )
        )

parseTokenThen :: Parser ()
parseTokenThen =
    void (
        satisfy (
            \x ->
                case x of
                    LTThen -> True
                    otherwise -> False
            )
        )

parseTokenTo :: Parser ()
parseTokenTo =
    void (
        satisfy (
            \x ->
                case x of
                    LTTo -> True
                    otherwise -> False
            )
        )

parseTokenVar :: Parser ()
parseTokenVar =
    void (
        satisfy (
            \x ->
                case x of
                    LTVar -> True
                    otherwise -> False
            )
        )

parseTokenWhile :: Parser ()
parseTokenWhile =
    void (
        satisfy (
            \x ->
                case x of
                    LTWhile -> True
                    otherwise -> False
            )
        )

parseCharacterString :: Parser ASTCharacterString
parseCharacterString =
    do
        (LTCharacterString x) <-
            satisfy(   
                \x ->
                    case x of
                        LTCharacterString _ -> True
                        otherwise -> False
                )
        return x

parseIdentifier :: Parser ASTIdentifier
parseIdentifier =
    do
        (LTIdentifier x) <-
            satisfy (
                \x ->
                    case x of
                        LTIdentifier _ -> True
                        otherwise -> False
                )
        return x

parseUnsignedInteger :: Parser ASTUnsignedInteger
parseUnsignedInteger =
    do
        (LTUnsignedInteger x) <-
            satisfy (
                \x ->
                    case x of
                        LTUnsignedInteger _ -> True
                        otherwise -> False
                )
        return x

parseUnsignedReal :: Parser ASTUnsignedReal
parseUnsignedReal =
    do
        (LTUnsignedReal x) <-
            satisfy (
                \x ->
                    case x of
                        LTUnsignedReal _ -> True
                        otherwise -> False
                )
        return x



----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-- end of lexical tokens section
 
type ASTStartSymbol = ASTProgram
parseStartSymbol :: Parser ASTStartSymbol
parseStartSymbol =
    trace
        "parseStartSymbol"
        (
            do
                x0 <-
                    parseProgram
                parseTokenEof
                return x0
            )

type ASTProgram = (ASTIdentifier, ASTVariableDeclarationPart, ASTProcedureDeclarationPart, ASTCompoundStatement)
parseProgram :: Parser ASTProgram
parseProgram =
    trace
        "parseProgram"
        (
            do
                parseTokenProgram
                x0 <-
                    parseIdentifier
                parseTokenSemicolon
                x1 <-
                    parseVariableDeclarationPart
                x2 <-
                    parseProcedureDeclarationPart
                x3 <-
                    parseCompoundStatement
                parseTokenDot
                return (x0, x1, x2, x3)
            )

type ASTProcedureDeclarationPart = [ASTProcedureDeclaration]
parseProcedureDeclarationPart :: Parser ASTProcedureDeclarationPart
parseProcedureDeclarationPart =
    trace
        "parseProcedureDeclarationPart"
        (
            many (
                try (
                    do
                        x0 <-
                            parseProcedureDeclaration
                        parseTokenSemicolon
                        return x0
                    )
                )
            )
----------------------------------------------------------------------------
-- your code starts here

--corresponds to statement in PazParserBNF.txt
-- TODO: Needs empty statement.
-- statement
--  : assignment_statement
--  | procedure_statement -- must go after assignment_statement
--  | compound_statement
--  | if_statement
--  | while_statement
--  | for_statement
--  | empty_statement     -- must go at the end. 
type ASTStatement = Statement
data Statement = 
    AssignmentStatementStatement ASTAssignmentStatement |
    ProcedureStatementStatement ASTProcedureStatement |
    CompoundStatementStatement ASTCompoundStatement |
    IfStatementStatement ASTIfStatement |
    WhileStatementStatement ASTWhileStatement |
    ForStatementStatement ASTForStatement
    deriving(Show)
 

parseStatement :: Parser ASTStatement
parseStatement =
    trace
        "parseStatement"
        (
            choice
                    [
                        try (
                            do
                                x <-
                                    parseAssignmentStatement
                                return (AssignmentStatementStatement x)
                            ),
                        try (
                            do
                                x <-
                                    parseProcedureStatement
                                return (ProcedureStatementStatement x)
                            ),
                        try (
                            do
                                x <-
                                    parseCompoundStatement
                                return (CompoundStatementStatement x)
                            ),
                        try (
                            do
                                x <-
                                    parseIfStatement
                                return (IfStatementStatement x)
                            ),
                        try (
                            do
                                x <-
                                    parseWhileStatement
                                return (WhileStatementStatement x)
                            ),
                        do
                            x <-
                                parseForStatement
                            return (ForStatementStatement x)
                        ]
        )

-- Corresponds to assignment_statement
-- assignment_statement : (variable_access | identifier ) ASSIGN expression
type ASTAssignmentStatement = (ASTAssignmentStatementHead, ASTExpression) 
parseAssignmentStatement :: Parser ASTAssignmentStatement
parseAssignmentStatement =
    trace
        "parseAssignmentStatement"
        (
            do
                x0<-parseAssignmentStatementHead
                parseTokenAssign
                x1<-parseExpression
                return(x0,x1)
        )
-- TODO: i wrote this extra type to help me create the parser for assigment_statement
-- there probably is a way to make assigment_statment parser work without this
-- ugly thing here. 
type ASTAssignmentStatementHead = AssignmentStatementHead 
data AssignmentStatementHead = 
    VariableAccessASH ASTVariableAccess |
    IdentifierASH ASTIdentifier
    deriving(Show)
parseAssignmentStatementHead :: Parser ASTAssignmentStatementHead
parseAssignmentStatementHead = 
    trace
        "parseAssignmentStatementHead"
        (
            choice
                [
                    try
                    (
                        do
                            x<-parseVariableAccess
                            return (VariableAccessASH x)
                    ),
                do
                    x<-parseIdentifier
                    return (IdentifierASH x)
                ]
        )

-- Correpsonds to procedure_statement
-- procedure_statement : identifier [actual_parameter list]
type ASTProcedureStatement = (ASTIdentifier, Maybe ASTActualParameterList )
parseProcedureStatement :: Parser ASTProcedureStatement
parseProcedureStatement =
    trace
        "parseProcedureStatement"
        (
            do
                x0 <-parseIdentifier
                x1 <- 
                    optionMaybe
                    (
                        try
                        (
                            do
                                parseActualParameterList
                        )
                    )
                return (x0,x1)
        )

-- Corresponds to actual_parameter_list
--actual_parameter_list
--    : LEFT_PARENTHESIS expression {COMMA expression} RIGHT_PARENTHESIS
type ASTActualParameterList = (ASTExpression, [ASTExpression])
parseActualParameterList :: Parser ASTActualParameterList
parseActualParameterList = 
    trace 
        "parseActualParameterList"
        (
            do
                parseTokenLeftParenthesis
                x0 <- parseExpression
                x1 <- 
                    many
                    (
                        try
                        (
                            do
                                parseTokenComma
                                x2 <- parseExpression
                                return x2
                        )
                    )
                parseTokenRightParenthesis
                return (x0,x1)
        ) 

-- Corresponds to compound_statement.
--compound_statement
--    : BEGIN statement_sequence END
type ASTCompoundStatement = (ASTStatementSequence)
parseCompoundStatement :: Parser ASTCompoundStatement
parseCompoundStatement =
    trace
        "parseCompoundStatement"
        (
            do
                parseTokenBegin
                x <- parseStatementSequence
                parseTokenEnd
                return x
            )

-- Corresponds to statement_sequence
--statement_sequence
--    : statement {SEMICOLON statement}
type ASTStatementSequence = (ASTStatement, [ASTStatement])
parseStatementSequence :: Parser ASTStatementSequence
parseStatementSequence = 
    trace
        "parseStatementSequence"
        (
            do
                x0 <- parseStatement
                x1 <-
                    many
                    (
                        try
                        (
                            do
                                parseTokenSemicolon
                                x2 <- parseStatement
                                return x2
                        )
                    )
                return (x0,x1)
        )

-- Corresponds to if_statement.
--if_statement
--    : IF expression THEN statement [ELSE statement]    
type ASTIfStatement = (ASTExpression, ASTStatement, (Maybe ASTStatement))
parseIfStatement :: Parser ASTIfStatement
parseIfStatement =
    trace
        "parseIfStatement"
        (
            do
                parseTokenIf
                x0 <- parseExpression
                parseTokenThen
                x1 <- parseStatement
                x2 <-
                    optionMaybe
                    (
                        try
                        (
                            do
                                parseTokenElse
                                parseStatement
                        )
                    )
                return (x0,x1,x2)
        )

-- Corresponds to while_statement
-- while_statement
--    : WHILE expression DO statement
type ASTWhileStatement = (ASTExpression, ASTStatement)
parseWhileStatement :: Parser ASTWhileStatement
parseWhileStatement = 
    trace
        "parseWhileStatement"
        (
            do
                parseTokenWhile
                x0 <- parseExpression
                parseTokenDo
                x1 <- parseStatement
                return (x0,x1)
        )

-- Corresponds to for_statement.
-- FOR identifier ASSIGN expression (TO|DOWN_TO) expression DO statement
type ASTForStatement = (ASTIdentifier, ASTExpression, ASTExpression, ASTStatement)
parseForStatement :: Parser ASTForStatement
parseForStatement = 
    trace
        "parseForStatement"
        (
            do
                parseTokenFor
                x0 <- parseIdentifier
                parseTokenAssign
                x1 <- parseExpression
                choice
                    [
                        try
                        (
                        do 
                            parseTokenTo
                        ),
                        do
                            parseTokenDownTo
                        
                    ]
                x2 <- parseExpression
                parseTokenDo
                x3 <- parseStatement
                return(x0,x1,x2,x3)


        )
 
-- Expressions section.
--expression: simple expression [relational_operator simple_expression]
type ASTExpression = (ASTSimpleExpression, Maybe (ASTRelationalOperator, ASTSimpleExpression))
parseExpression :: Parser ASTExpression
parseExpression = 
    trace
        "parseExpression"
        (
            do
                x0<-parseSimpleExpression
                x1<-
                    optionMaybe
                    (
                        try
                        (
                            do
                                x2<-parseRelationalOperator
                                x3<-parseSimpleExpression
                                return(x2,x3)
                        )
                    )
                return (x0,x1)
        )

--relational_operator
--    : EQUAL
--    | NOT_EQUAL
--    | LESS_THAN
--    | GREATER_THAN
--    | LESS_THAN_OR_EQUAL
--    | GREATER_THAN_OR_EQUAL
--    ;
type ASTRelationalOperator = RelationalOperator
data RelationalOperator =
    RelationOpEqual |
    RelationOpNotEqual |
    RelationOpLessThan |
    RelationOpGreaterThan |
    RelationOpLessThanOrEqual |
    RelationOpGreaterThanOrEqual
    deriving(Show)
parseRelationalOperator :: Parser ASTRelationalOperator
parseRelationalOperator =
    trace
        "parseRelationalOperator"
        (
            choice
                [
                    try (
                        do
                            parseTokenEqual
                            return RelationOpEqual
                        ),
                    try (
                        do
                            parseTokenNotEqual
                            return RelationOpNotEqual
                        ),
                    try (
                        do
                            parseTokenLessThan
                            return RelationOpLessThan
                        ),
                    try (
                        do
                            parseTokenGreaterThan
                            return RelationOpGreaterThan
                        ),
                    try (
                        do
                            parseTokenLessThanOrEqual
                            return RelationOpLessThanOrEqual
                        ),
                    do
                        parseTokenGreaterThanOrEqual
                        return RelationOpGreaterThanOrEqual
                    ]
            )


--simple_expression
--    : [sign] term {adding_operator term}
type ASTSimpleExpression = ((Maybe (ASTSign)), ASTTerm, [(ASTAddingOperator, ASTTerm)])
parseSimpleExpression :: Parser ASTSimpleExpression
parseSimpleExpression = 
    trace
        "parseSimpleExpression"
        (
            do
                x0 <- 
                    optionMaybe
                    (
                        try
                        (
                            do
                            
                                parseSign
                            
                        )
                    )
                x1 <- parseTerm
                x2 <- 
                    many
                    (
                        try
                        (
                            do
                                x3 <- parseAddingOperator
                                x4 <- parseTerm
                                return (x3,x4)
                        )
                    )
                return (x0,x1,x2)
        )

-- Corresponds to adding operators. 
--adding_operator
--    : PLUS
--    | MINUS
--    | OR
--    ;
type ASTAddingOperator = AddingOperator
data AddingOperator = 
    AddOpPlus |
    AddOpMinus |
    AddOpOr
    deriving(Show)

parseAddingOperator :: Parser ASTAddingOperator
parseAddingOperator =
    trace
        "parseAddingOperator"
        (
            choice
                [
                    try (
                        do
                            parseTokenPlus
                            return AddOpPlus
                        ),
                    try (
                        do
                            parseTokenMinus
                            return AddOpPlus
                        ),
                    
                    do
                        parseTokenOr
                        return AddOpOr
                    ]
            )

--term
--    : factor {multiplying_operator factor}
type ASTTerm = (ASTFactor, [(ASTMultOperator, ASTFactor)])
parseTerm :: Parser ASTTerm
parseTerm =
    trace
        "parseTerm"
        (
            do
                x0 <-
                    parseFactor
                x1 <-
                    many (
                        try (
                            do
                                x2 <- parseMultOperator
                                x3 <- parseFactor
                                return (x2,x3)
                            )
                        )
                return (x0, x1)
            )

-- Corresponds to multiplying_operator. 
type ASTMultOperator = MultOperator
data MultOperator = 
    MultOpTimes |
    MultOpDivideBy |
    MultOpDiv |
    MultOpAnd
    deriving(Show)

--multiplying_operator
--    : TIMES
--    | DIVIDE_BY
--    | DIV
--    | AND
--    ;
parseMultOperator :: Parser ASTMultOperator
parseMultOperator =
    trace
        "parseMultOperator"
        (
            choice
                [
                    try (
                        do
                            parseTokenTimes
                            return MultOpTimes
                        ),
                    try (
                        do
                            parseTokenDivideBy                     
                            return MultOpDivideBy
                        ),
                    try (
                        do
                            parseTokenDiv
                            return MultOpDiv
                        ),
                    do
                        parseTokenAnd
                        return MultOpAnd
                    ]
            )

-- Code corresponds to factor.
--factor
--    : unsigned_constant
--    | variable_access
--    | LEFT_PARENTHESIS expression RIGHT_PARENTHESIS
--    | NOT factor
--    ;
type ASTFactor = Factor
data Factor = 
    UnsignedConstantFactor ASTUnsignedConstant |
    VariableAccessFactor ASTVariableAccess |
    ExpressionFactor ASTExpression |
    FactorFactor Factor
    deriving(Show)
parseFactor :: Parser ASTFactor
parseFactor =
    trace
        "parseFactor"
        (
            choice
                [
                    -- unsigned_constant
                    try (
                        do
                            x<-parseUnsignedConstant
                            return (UnsignedConstantFactor x)
                        ),
                    
                    -- variable_access
                    try (
                        do
                            x<-parseVariableAccess
                            return (VariableAccessFactor x)
                        ),
                    
                    -- LEFT_PARENTHESIS expression RIGHT_PARENTHESIS
                    try (
                        do
                            parseTokenLeftParenthesis
                            x <- parseExpression
                            parseTokenRightParenthesis
                            return (ExpressionFactor x)
                        ),
                    do
                        -- check this as well. 
                        x <- parseFactor
                        return (FactorFactor x) 
                    ]
            )

--unsigned_constant
--    : unsigned_number
--    | character_string
--    ;
type ASTUnsignedConstant = UnsignedConstant
data UnsignedConstant =
    UnsignedNumberConstant ASTUnsignedNumber |
    CharacterStringConstant ASTCharacterString
    deriving(Show)

parseUnsignedConstant :: Parser ASTUnsignedConstant
parseUnsignedConstant =
    trace
        "parseUnsignedConstant"
        (
            choice
                [
                    try (
                        do
                            x <- parseUnsignedNumber
                            return (UnsignedNumberConstant x)
                        ),
                    do
                        x <- parseCharacterString
                        return (CharacterStringConstant x)
                    ]
            ) 

--unsigned_number
--    : unsigned_integer
--    | unsigned_real
--    ;
type ASTUnsignedNumber = UnsignedNumber
data UnsignedNumber =
    UnsignedIntegerNumber ASTUnsignedInteger |
    UnsignedIntegerReal ASTUnsignedReal
    deriving(Show)

-- Correpsonds to unsigned number.
parseUnsignedNumber :: Parser ASTUnsignedNumber
parseUnsignedNumber =
    trace
        "parseUnsignedNumber"
        (
            choice
                [
                    try (
                        do
                            x <- parseUnsignedInteger
                            return (UnsignedIntegerNumber x)
                        ),
                    do
                        x <- parseUnsignedReal
                        return (UnsignedIntegerReal x)
                    ]
            )

-- Corresponds to variable_access.
--variable_access
--    : indexed_variable
--    | identifier -- must be after indexed_variable
type ASTVariableAccess = VariableAccess
data VariableAccess =
    IndexedVariableVariableAccess ASTIndexedVariable |
    IdenfierVariableAccess ASTIdentifier
    deriving(Show)


parseVariableAccess :: Parser ASTVariableAccess
parseVariableAccess =
    trace
        "parseVariableAccess"
        (
            choice
                [
                    -- indexed_variable
                    try (
                        do
                            x<-parseIndexedVariable
                            return (IndexedVariableVariableAccess x)
                        ),
                    
                    -- TODO: somehow got to make this after indexed_variable
                    do
                        x<-parseIdentifier
                        return (IdenfierVariableAccess x)
                    
                    ]
            )

-- indexed variable: identifier LEFT_BRACKET expression RIGHT_BRACKET
type ASTIndexedVariable = (ASTIdentifier, ASTExpression)
parseIndexedVariable :: Parser ASTIndexedVariable
parseIndexedVariable = 
    trace
        "parseIndexedVariable"
        (
            do
                x0 <- 
                    parseIdentifier
                parseTokenLeftParenthesis
                x1 <-
                    parseExpression
                parseTokenRightParenthesis 
                return (x0,x1)
        )




-- your code ends here

type ASTProcedureDeclaration = (ASTIdentifier, (Maybe ASTFormalParameterList), ASTVariableDeclarationPart, ASTCompoundStatement)
parseProcedureDeclaration :: Parser ASTProcedureDeclaration
parseProcedureDeclaration =
    trace
        "parseProcedureDeclaration"
        (
            do
                parseTokenProcedure
                x0 <-
                    parseIdentifier
                x1 <-
                    optionMaybe (
                        try (
                            parseFormalParameterList
                            )
                        )
                parseTokenSemicolon
                x2 <-
                    parseVariableDeclarationPart
                x3 <-
                    parseCompoundStatement
                return (x0, x1, x2, x3)
            )

type ASTFormalParameterList = (ASTFormalParameterSection, [ASTFormalParameterSection])
parseFormalParameterList :: Parser ASTFormalParameterList
parseFormalParameterList =
    trace
        "parseFormalParameterList"
        (
            do
                parseTokenLeftParenthesis
                x0 <-
                    parseFormalParameterSection
                x1 <-
                    many (
                        try (
                            do
                                parseTokenSemicolon
                                x0 <-
                                    parseFormalParameterSection
                                return x0
                            )
                        )
                parseTokenRightParenthesis
                return (x0, x1)
            )

-- presence of "var" keyword should have type "Maybe ()" according to the
-- usual system, but the generator cannot handle this because it is stupid,
-- so we manually put in a type of "Bool" which is "True" if "var" present
type ASTFormalParameterSection = (Bool, ASTIdentifierList, ASTTypeDenoter)
parseFormalParameterSection :: Parser ASTFormalParameterSection
parseFormalParameterSection =
    trace
        "parseFormalParameterSection"
        (
            do
                x0 <-
                    optionMaybe (
                        try (
                            parseTokenVar
                            )
                        )
                x1 <-
                    parseIdentifierList
                parseTokenColon
                x2 <-
                    parseTypeDenoter
                return (
                    case x0 of
                        Just _ -> (True, x1, x2)
                        _ -> (False, x1, x2)
                    )
            )

type ASTIdentifierList = (ASTIdentifier, [ASTIdentifier])
parseIdentifierList :: Parser ASTIdentifierList
parseIdentifierList =
    trace
        "parseIdentifierList"
        (
            do
                x0 <-
                    parseIdentifier
                x1 <-
                    many (
                        try (
                            do
                                parseTokenComma
                                x0 <-
                                    parseIdentifier
                                return x0
                            )
                        )
                return (x0, x1)
            )

type ASTVariableDeclarationPart = (Maybe (ASTVariableDeclaration, [ASTVariableDeclaration]))
parseVariableDeclarationPart :: Parser ASTVariableDeclarationPart
parseVariableDeclarationPart =
    trace
        "parseVariableDeclarationPart"
        (
            optionMaybe (
                try (
                    do
                        parseTokenVar
                        x0 <-
                            parseVariableDeclaration
                        parseTokenSemicolon
                        x1 <-
                            many (
                                try (
                                    do
                                        x0 <-
                                            parseVariableDeclaration
                                        parseTokenSemicolon
                                        return x0
                                    )
                                )
                        return (x0, x1)
                    )
                )
            )

type ASTVariableDeclaration = (ASTIdentifierList, ASTTypeDenoter)
parseVariableDeclaration :: Parser ASTVariableDeclaration
parseVariableDeclaration =
    trace
        "parseVariableDeclaration"
        (
            do
                x0 <-
                    parseIdentifierList
                parseTokenColon
                x1 <-
                    parseTypeDenoter
                return (x0, x1)
            )

type ASTTypeDenoter = TypeDenoter
data TypeDenoter =
    OrdinaryTypeDenoter ASTTypeIdentifier |
    ArrayTypeDenoter ASTArrayType
    deriving(Show)
parseTypeDenoter :: Parser ASTTypeDenoter
parseTypeDenoter =
    trace
        "parseTypeDenoter"
        (
            choice
                [
                    try (
                        do
                            x <-
                                parseTypeIdentifier
                            return (OrdinaryTypeDenoter x)
                        ),
                    do
                        x <-
                            parseArrayType
                        return (ArrayTypeDenoter x)
                    ]
            )

type ASTTypeIdentifier = TypeIdentifier
data TypeIdentifier =
    IntegerTypeIdentifier |
    RealTypeIdentifier |
    BooleanTypeIdentifier
    deriving(Show)
parseTypeIdentifier :: Parser ASTTypeIdentifier
parseTypeIdentifier =
    trace
        "parseTypeIdentifier"
        (
            choice
                [
                    try (
                        do
                            parseTokenInteger
                            return IntegerTypeIdentifier
                        ),
                    try (
                        do
                            parseTokenReal
                            return RealTypeIdentifier
                        ),
                    do
                        parseTokenBoolean
                        return BooleanTypeIdentifier
                    ]
            )

type ASTArrayType = (ASTSubrangeType, ASTTypeIdentifier)
parseArrayType :: Parser ASTArrayType
parseArrayType =
    trace
        "parseArrayType"
        (
            do
                parseTokenArray
                parseTokenLeftBracket
                x0 <-
                    parseSubrangeType
                parseTokenRightBracket
                parseTokenOf
                x1 <-
                    parseTypeIdentifier
                return (x0, x1)
            )

type ASTSubrangeType = (ASTConstant, ASTConstant)
parseSubrangeType :: Parser ASTSubrangeType
parseSubrangeType =
    trace
        "parseSubrangeType"
        (
            do
                x0 <-
                    parseConstant
                parseTokenEllipsis
                x1 <-
                    parseConstant
                return (x0, x1)
            )

type ASTConstant = ((Maybe ASTSign), ASTUnsignedInteger)
parseConstant :: Parser ASTConstant
parseConstant =
    trace
        "parseConstant"
        (
            do
                x0 <-
                    optionMaybe (
                        try (
                            parseSign
                            )
                        )
                x1 <-
                    parseUnsignedInteger
                return (x0, x1)
            )

type ASTSign = Sign
data Sign =
    SignPlus |
    SignMinus
    deriving(Show)
parseSign :: Parser ASTSign
parseSign =
    trace
        "parseSign"
        (
            choice
                [
                    try (
                        do
                            parseTokenPlus
                            return SignPlus
                        ),
                    do
                        parseTokenMinus
                        return SignMinus
                    ]
            )


