import Debug.Trace (trace)
import Text.Parsec (parse)
import PazLexer
import PazParser
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.Environment

import Text.Printf



die :: String -> IO ()
die err = do
    hPutStrLn stderr err
    exitFailure

main :: IO ()
main = do
    progname <- getProgName
    args <- getArgs
    case args of
        [a] -> do
                putStrLn "Sorry, cannot generate code yet"
                exitWith (ExitFailure 1)
        ["-p", a] -> doParsing a
        otherwise -> do
                        putStrLn ("Usage: " ++ progname ++ " [-p] sourcefile")
                        exitWith (ExitFailure 1)

doParsing :: String -> IO ()
doParsing filename = do
    input <- readFile (filename)
    case
        trace
             "*** Lexical analysis"
             (parse PazLexer.parseStartSymbol "(stdin)" input)
        of
        Left error ->
            die ("Lexical error:\n" ++ show error)
        Right tokens ->
            case
                trace
                    "*** Syntax analysis"
                    (parse PazParser.parseStartSymbol "(stdin)" tokens)
                of
                Left error ->
                    die ("Syntax error:\n" ++ show error)
                Right ast ->
                    --putStrLn (show ast)
                    prettyPrint ast

--------------------------------------------------------------------------------
-- ASTVariableDeclarationPart
--------------------------------------------------------------------------------
ppSign :: PazParser.ASTSign -> String
ppSign s =
  case s of
    PazParser.SignPlus -> "+"
    PazParser.SignMinus -> "-"

ppConstant :: PazParser.ASTConstant -> String
ppConstant ((Nothing), n) = n
ppConstant ((Just sign), n) = (ppSign sign) ++ n

ppSubrange :: PazParser.ASTSubrangeType -> String
ppSubrange (start, end) = "[" ++ (ppConstant start) ++ ".." ++ (ppConstant end) ++ "]"

ppArrayType :: PazParser.ASTArrayType -> String
ppArrayType (range, ti) = (ppSubrange range) ++ " of " ++ (ppTypeIdentifier ti)

ppTypeIdentifier :: PazParser.ASTTypeIdentifier -> String
ppTypeIdentifier ti =
  case ti of
    IntegerTypeIdentifier -> "integer"
    RealTypeIdentifier -> "real"
    BooleanTypeIdentifier -> "boolean"

ppTypeDenoter :: PazParser.ASTTypeDenoter -> String
ppTypeDenoter td =
  case td of
    OrdinaryTypeDenoter t -> (ppTypeIdentifier t)
    ArrayTypeDenoter t -> "array " ++ (ppArrayType t)

ppIdentifier :: PazLexer.ASTIdentifier -> String
ppIdentifier i = i

ppIdentifierList :: PazParser.ASTIdentifierList -> String
ppIdentifierList (var, []) = (ppIdentifier var)
ppIdentifierList (var, (v:vs)) = (ppIdentifier var) ++ ppIdentifierList (v, vs)

ppVariableDec :: PazParser.ASTVariableDeclaration -> IO ()
ppVariableDec (identifiers, types) = do
  printf "    %s: %s;\n" (ppIdentifierList identifiers) (ppTypeDenoter types)

ppVariableDecList :: [PazParser.ASTVariableDeclaration] -> IO ()
ppVariableDecList [] = return () -- do nothing
ppVariableDecList (v:vs) = do
  ppVariableDec v
  ppVariableDecList vs

ppVariableDecPart :: PazParser.ASTVariableDeclarationPart -> IO ()
ppVariableDecPart Nothing = return () -- do nothing
ppVariableDecPart (Just variables) = do
  printf "\nvar\n"
  ppVariableDecList variables

--------------------------------------------------------------------------------
-- ASTProcedureDeclarationPart
--------------------------------------------------------------------------------
-- (Bool, ASTIdentifierList, ASTTypeDenoter)
ppParamSection :: PazParser.ASTFormalParameterSection -> String
ppParamSection (False, idlist, td) = (ppIdentifierList idlist) ++ ": " ++ (ppTypeDenoter td)
ppParamSection (True, idlist, td) = "var " ++ (ppIdentifierList idlist) ++ ": " ++ (ppTypeDenoter td)

ppFormalParamList :: PazParser.ASTFormalParameterList -> String
ppFormalParamList [] = ""
ppFormalParamList [x] = (ppParamSection x)
ppFormalParamList (x:xs) = (ppParamSection x) ++ "; " ++ (ppFormalParamList xs)

ppProcedureDec :: PazParser.ASTProcedureDeclaration -> IO ()
ppProcedureDec (ident, (Nothing), vardecpart, compound) = do
  printf ("procedure " ++ (ppIdentifier ident) ++ ";")
  ppVariableDecPart vardecpart
  ppCompound compound 4
  putStr ";\n\n"
ppProcedureDec (ident, (Just paramlist), vardecpart, compound) = do
  printf ("procedure " ++ (ppIdentifier ident) ++ "(" ++ (ppFormalParamList paramlist) ++ ");")
  ppVariableDecPart vardecpart
  ppCompound compound 4
  putStr ";\n\n"


ppProcedureDecPart :: [PazParser.ASTProcedureDeclaration] -> IO ()
ppProcedureDecPart [] = return () -- do nothing
ppProcedureDecPart (p:ps) = do
  (ppProcedureDec p)
  (ppProcedureDecPart ps)
  -- ppProcedureDec v
  -- ppProcedureDecList vs

--------------------------------------------------------------------------------
-- ASTCompoundStatement
--------------------------------------------------------------------------------
ppIndent :: Int -> IO ()
ppIndent n = putStr (concat (replicate n " "))

data PrevSign =
  AddOp |
  MulOp |
  NotOp |
  Empty
  deriving (Eq, Show)

ppUnsignedReal :: PazLexer.ASTUnsignedReal -> IO ()
ppUnsignedReal _ = putStr "REALNUM"

ppUnsignedNum :: PazParser.ASTUnsignedNumber -> IO ()
ppUnsignedNum n =
  case n of
    UnsignedInteger i -> putStr (show i)-- print i
    UnsignedReal r -> ppUnsignedReal r

ppUnsignedConstant :: PazParser.ASTUnsignedConstant -> IO ()
ppUnsignedConstant c =
  case c of
    UnsignedNumberConstant n -> ppUnsignedNum n
    CharacterStringConstant s -> putStr ("'" ++ s ++ "'")

ppAddingOperator :: PazParser.ASTAddingOperator -> IO ()
ppAddingOperator a =
  case a of
    AddOpPlus -> putStr " + "
    AddOpMinus -> putStr " - "
    AddOpOr -> putStr " or "

ppMultOperator :: PazParser.ASTMultOperator -> IO ()
ppMultOperator m =
  case m of
    MultOpTimes -> putStr " * "
    MultOpDivideBy -> putStr " / "
    MultOpDiv -> putStr " div "
    MultOpAnd -> putStr " and "

ppFactor :: PazParser.ASTFactor -> PrevSign -> IO ()
ppFactor f prev =
  case f of
    UnsignedConstantFactor cf -> ppUnsignedConstant cf
    VariableAccessFactor va -> ppVariableAccess va
    ExpressionFactor ef -> ppExpression ef prev -- TODO need to add parans
    FactorFactor ff -> do
      putStr "not "
      ppFactor ff NotOp -- TODO add parans if non-single expression

ppFactorList :: [(ASTMultOperator, ASTFactor)] -> IO ()
ppFactorList [] = return ()
ppFactorList ((mulop, factor):fs) = do
  ppMultOperator mulop
  ppFactor factor MulOp
  ppFactorList fs

-- (ASTFactor, [(ASTMultOperator, ASTFactor)])
ppTerm :: PazParser.ASTTerm -> PrevSign -> IO ()
ppTerm (factor, []) prev = do
  ppFactor factor prev
ppTerm (factor, factors) prev = do
  ppFactor factor MulOp
  ppFactorList factors

-- [(ASTAddingOperator, ASTTerm)]
ppTermList :: [(ASTAddingOperator, ASTTerm)] -> IO ()
ppTermList [] = return ()
ppTermList ((addop, term):ts) = do
  ppAddingOperator addop
  ppTerm term AddOp
  ppTermList ts

-- ((Maybe (ASTSign)), ASTTerm, [(ASTAddingOperator, ASTTerm)])
ppSimpleExpression :: PazParser.ASTSimpleExpression -> PrevSign -> IO ()
ppSimpleExpression ((Nothing), term, []) prev = do
  if prev == Empty || prev == AddOp
    then do
      ppTerm term prev
    else do
      putStr "("
      ppTerm term Empty
      putStr ")"
ppSimpleExpression ((Just sign), term, []) prev = do
  putStr (ppSign sign)
  if prev == Empty || prev == AddOp
    then do
      ppTerm term prev
    else do
      putStr "("
      ppTerm term Empty
      putStr ")"
ppSimpleExpression ((Nothing), term, terms) prev = do
  -- putStr (show prev)
  if prev == Empty || prev == AddOp
    then do
      ppTerm term AddOp
      ppTermList terms
    else do
      putStr "("
      ppTerm term Empty
      ppTermList terms
      putStr ")"
ppSimpleExpression ((Just sign), term, terms) prev = do
  -- putStr (show prev)
  if prev == Empty || prev == AddOp
    then do
      putStr (ppSign sign)
      ppTerm term AddOp
      ppTermList terms
    else do
      putStr (ppSign sign)
      putStr "("
      ppTerm term Empty
      ppTermList terms
      putStr ")"

ppRelOperator :: PazParser.ASTRelationalOperator -> IO ()
ppRelOperator r =
  case r of
    ROEqual -> putStr " = "
    RONotEqual -> putStr " <> "
    ROLessThan -> putStr " < "
    ROGreaterThan -> putStr " > "
    ROLessThanOrEqual -> putStr " <= "
    ROGreaterThanOrEqual -> putStr " >= "

-- (ASTSimpleExpression, Maybe (ASTRelationalOperator, ASTSimpleExpression))
ppExpression :: PazParser.ASTExpression -> PrevSign -> IO ()
ppExpression (simple, Nothing) prev =
  if prev == Empty
    then do
      ppSimpleExpression simple prev
    else do
      putStr "("
      ppSimpleExpression simple Empty
      putStr ")"
ppExpression (simple, (Just (relop, simple2))) prev = do
  -- putStr (show prev)
  if prev == Empty
    then do
      ppSimpleExpression simple prev
      ppRelOperator relop
      ppSimpleExpression simple2 prev
    else do
      putStr "("
      ppSimpleExpression simple Empty
      ppRelOperator relop
      ppSimpleExpression simple2 Empty
      putStr ")"

-- [ASTExpression]
ppActualParamList :: PazParser.ASTActualParameterList -> IO ()
ppActualParamList [] = return ()
ppActualParamList [x] = ppExpression x Empty
ppActualParamList (x:xs) = do
  ppExpression x Empty
  putStr ", "
  ppActualParamList xs

-- (ASTIdentifier, ASTExpression)
ppIndexedVariable :: PazParser.ASTIndexedVariable -> IO ()
ppIndexedVariable (ident, expression) = do
  putStr (ppIdentifier ident)
  ppExpression expression Empty

ppVariableAccess :: PazParser.ASTVariableAccess -> IO ()
ppVariableAccess v =
  case v of
    IndexedVariableVariableAccess va -> ppIndexedVariable va
    IdenfierVariableAccess i -> putStr (ppIdentifier i)

-- (ASTIdentifier, Maybe ASTActualParameterList )
ppProcedureStmt :: PazParser.ASTProcedureStatement -> Int -> IO ()
ppProcedureStmt (i, Nothing) indent = do
  ppIndent indent
  putStr (ppIdentifier i)
ppProcedureStmt (i, (Just params)) indent = do
  ppIndent indent
  putStr (ppIdentifier i)
  putStr "("
  ppActualParamList params
  putStr ")"

-- (Variable, ASTExpression) -- Variable only used in this statement
ppAssignmentStmt :: PazParser.ASTAssignmentStatement -> Int -> IO ()
ppAssignmentStmt (variable, expression) indent = do
  ppIndent indent
  case variable of
    VariableAcessAssignmentStatement va ->  ppVariableAccess va
    IdentifierAssignmentStatement i -> putStr (ppIdentifier i)
  putStr " := "
  ppExpression expression Empty

-- (ASTExpression, ASTStatement, (Maybe ASTStatement))
ppIfStmt :: PazParser.ASTIfStatement -> Int -> IO ()
ppIfStmt (expression, thenstmt, (Nothing)) indent = do
  ppIndent indent
  putStr "if "
  ppExpression expression Empty
  putStr " then\n"
  ppStatement thenstmt (indent+4)
ppIfStmt (expression, thenstmt, (Just elsestmt)) indent = do
  ppIndent indent
  putStr "if "
  ppExpression expression Empty
  putStr " then\n"
  ppStatement thenstmt (indent+4)
  putStr "\n"
  ppIndent indent
  putStr "else\n"
  ppStatement elsestmt (indent+4)

-- (ASTExpression, ASTStatement)
ppWhileStmt :: PazParser.ASTWhileStatement -> Int -> IO ()
ppWhileStmt (expression, statement) indent = do
  ppIndent indent
  putStr "while "
  ppExpression expression Empty
  putStr " do\n"
  ppStatement statement (indent+4)

-- (ASTIdentifier, ASTExpression, ASTExpression, ASTStatement)
ppForStmt :: PazParser.ASTForStatement -> Int -> IO ()
ppForStmt (ident, expr1, expr2, stmt) indent = do
  ppIndent indent
  putStr "for "
  putStr (ppIdentifier ident)
  putStr " := "
  ppExpression expr1 Empty
  putStr " to "
  ppExpression expr2 Empty
  putStr " do\n"
  ppStatement stmt (indent+4)

ppStatement :: PazParser.ASTStatement -> Int -> IO ()
ppStatement s indent = do
  case s of
    AssignmentStatement as -> ppAssignmentStmt as indent
    ProcedureStatement ps -> ppProcedureStmt ps indent
    CompoundStatement cs -> ppCompound cs indent
    IfStatement is -> ppIfStmt is indent
    WhileStatement ws -> ppWhileStmt ws indent
    ForStatement fs -> ppForStmt fs indent
    EmptyStatement -> putStr ";\n"

-- [ASTStatement]   TODO - add ";\n" before each statement if not first one
ppStatementSequence :: PazParser.ASTStatementSequence -> Int -> IO ()
ppStatementSequence [] _ = return ()
ppStatementSequence [x] indent = do
  ppStatement x indent
ppStatementSequence (x:xs) indent = do
  ppStatement x indent
  putStr ";\n"
  ppStatementSequence xs indent

-- (ASTStatementSequence)
ppCompound :: PazParser.ASTCompoundStatement -> Int -> IO ()
ppCompound (c) indent = do
  ppIndent (indent-4)
  putStr "begin\n" -- check ordering !! only used in Compound Statements
  ppStatementSequence c indent
  putStr "\n"
  ppIndent (indent-4)
  putStr "end" -- check ordering !! only used in Compound Statements

-- TODO TODO TODO indents and new lines

--------------------------------------------------------------------------------
-- Top level pretty print
--------------------------------------------------------------------------------
prettyPrint :: PazParser.ASTStartSymbol -> IO ()
-- mapping ASTs to Paz programs.

-- ASTVariableDeclarationPart, ASTProcedureDeclarationPart, ASTCompoundStatement
-- prettyPrint ast = putStrLn "I'm a placeholder" -- TODO(joanna): put pretty printing here
prettyPrint (programname, variables, procedures, compound) = do
  printf "program %s;" programname
  putStr "\n"

  -- can print ASTIdentifier,
  ppVariableDecPart variables -- can be Nothing or list of ((ASTIdentifier, [ASTIdentifier]), ASTTypeDenoter -- array or normal type)
  putStr "\n"

  ppProcedureDecPart procedures
  ppCompound compound 4
  putStr ".\n"
