module PazPrettyPrinter where

import Debug.Trace (trace)
import PazLexer
import System.Environment
import Text.Printf
import PazParser 

indentSpacing = 4

-- Maps ASTs to Pretty Printed Paz programs.
prettyPrint :: PazParser.ASTStartSymbol -> IO ()
prettyPrint (programname, variables, procedures, compound) = do
  printf "program %s;\n" programname
  ppProgramVariableDecPart variables
  ppProcedureDecPart procedures
  ppCompound compound indentSpacing
  putStr ".\n"

--------------------------------------------------------------------------------
-- Variable Declaration Part
--------------------------------------------------------------------------------
-- Pretty Print Signs (from PazParser)
ppSign :: PazParser.ASTSign -> String
ppSign s =
  case s of
    PazParser.SignPlus -> "+"
    PazParser.SignMinus -> "-"

-- Pretty Print Signs (from PazLexer)
ppLexSign :: PazLexer.ASTSign -> String
ppLexSign s =
  case s of
    PazLexer.SignPlus -> "+"
    PazLexer.SignMinus -> "-"

-- Pretty Print Constants
ppConstant :: PazParser.ASTConstant -> String
ppConstant ((Nothing), n) = n
ppConstant ((Just sign), n) = (ppSign sign) ++ n

-- Pretty Print Array Subranges
ppSubrange :: PazParser.ASTSubrangeType -> String
ppSubrange (start, end) =
  "[" ++ (ppConstant start) ++ ".." ++ (ppConstant end) ++ "]"

-- Pretty Print Array Types
ppArrayType :: PazParser.ASTArrayType -> String
ppArrayType (range, ti) = (ppSubrange range) ++ " of " ++ (ppTypeIdentifier ti)

-- Pretty Print Type Identifiers
ppTypeIdentifier :: PazParser.ASTTypeIdentifier -> String
ppTypeIdentifier ti =
  case ti of
    IntegerTypeIdentifier -> "integer"
    RealTypeIdentifier -> "real"
    BooleanTypeIdentifier -> "boolean"

-- Pretty Print Type Denoters
ppTypeDenoter :: PazParser.ASTTypeDenoter -> String
ppTypeDenoter td =
  case td of
    OrdinaryTypeDenoter t -> (ppTypeIdentifier t)
    ArrayTypeDenoter t -> "array" ++ (ppArrayType t)

-- Pretty Print Identifiers
ppIdentifier :: PazLexer.ASTIdentifier -> String
ppIdentifier i = i

-- Pretty Print Identifier Lists
ppIdentifierList :: PazParser.ASTIdentifierList -> String
ppIdentifierList (var, []) = (ppIdentifier var)
ppIdentifierList (var, (v:vs)) = (ppIdentifier var) ++ ppIdentifierList (v, vs)

-- Pretty Print Variable Declaration List
ppVariableDecList :: [PazParser.ASTVariableDeclaration] -> IO ()
ppVariableDecList [] = return ()
ppVariableDecList [(identifiers, types)] = do
  printf "    %s: %s;" (ppIdentifierList identifiers) (ppTypeDenoter types)
ppVariableDecList ((identifiers, types):vs) = do
  printf "    %s: %s;\n" (ppIdentifierList identifiers) (ppTypeDenoter types)
  ppVariableDecList vs

-- Pretty Print Variable Declaration Part
ppVariableDecPart :: PazParser.ASTVariableDeclarationPart -> IO ()
ppVariableDecPart Nothing = return ()
ppVariableDecPart (Just variables) = do
  printf "\nvar\n"
  ppVariableDecList variables

-- Pretty Print Program Variable Declaration Part
ppProgramVariableDecPart :: PazParser.ASTVariableDeclarationPart -> IO ()
ppProgramVariableDecPart Nothing = return ()
ppProgramVariableDecPart vardecpart = do
  ppVariableDecPart vardecpart
  putStr "\n"


--------------------------------------------------------------------------------
-- Procedure Declaration Part
--------------------------------------------------------------------------------
-- Pretty Print Parameters
ppParamSection :: PazParser.ASTFormalParameterSection -> String
ppParamSection (False, idlist, td) =
  (ppIdentifierList idlist) ++ ": " ++ (ppTypeDenoter td)
ppParamSection (True, idlist, td) =
  "var " ++ (ppIdentifierList idlist) ++ ": " ++ (ppTypeDenoter td)

-- Pretty Print Formal Parameters for Procedure Declarations
ppFormalParamList :: PazParser.ASTFormalParameterList -> String
ppFormalParamList [] = ""
ppFormalParamList [x] = (ppParamSection x)
ppFormalParamList (x:xs) = (ppParamSection x) ++ "; " ++ (ppFormalParamList xs)

-- Pretty Print Procedure Declarations
ppProcedureDec :: PazParser.ASTProcedureDeclaration -> IO ()
ppProcedureDec (ident, (Nothing), vardecpart, compound) = do
  printf ("procedure " ++ (ppIdentifier ident) ++ ";")
  ppVariableDecPart vardecpart
  ppCompound compound indentSpacing
  putStr ";\n"
ppProcedureDec (ident, (Just paramlist), vardecpart, compound) = do
  putStr ("procedure " ++
    (ppIdentifier ident) ++ "(" ++ (ppFormalParamList paramlist) ++ ");")
  ppVariableDecPart vardecpart
  ppCompound compound indentSpacing
  putStr ";\n"

-- Pretty Print Procedure Declaration Part
ppProcedureDecPart :: [PazParser.ASTProcedureDeclaration] -> IO ()
ppProcedureDecPart [] = return ()
ppProcedureDecPart (p:ps) = do
  putStr "\n"
  ppProcedureDec p
  ppProcedureDecPart ps
  
{-ppProcedureDecSequence :: [PazParser.ASTProcedureDeclaration] -> IO ()
ppProcedureDecSequence [] = return ()
ppProcedureDecSequence (p:ps) = do-}

--------------------------------------------------------------------------------
-- ASTCompoundStatement
--------------------------------------------------------------------------------
data PrevSign =
  AddOp |
  MulOp |
  NotOp |
  Empty
  deriving (Eq, Show)

-- Pretty Print Indent Spaces
ppIndent :: Int -> IO ()
ppIndent n = putStr (concat (replicate n " "))

-- Pretty Print Scale Factors
ppScale :: PazLexer.ASTScaleFactor -> String
ppScale (Nothing, digits) = digits
ppScale ((Just sign), digits) = (ppLexSign sign) ++ digits

-- Pretty Print Unsigned Real Numbers
ppUnsignedReal :: PazLexer.ASTUnsignedReal -> IO ()
ppUnsignedReal (digits, (Nothing), (Nothing)) =
  putStr digits
ppUnsignedReal (digits, (Just decimals), (Nothing)) =
  putStr (digits ++ "." ++ decimals)
ppUnsignedReal (digits, (Nothing), (Just scale)) =
  putStr (digits ++ "e" ++ (ppScale scale))
ppUnsignedReal (digits, (Just decimals), (Just scale)) =
  putStr (digits ++ "." ++ decimals ++ "e" ++ (ppScale scale))

-- Pretty Print Unsigned Numbers
ppUnsignedNum :: PazParser.ASTUnsignedNumber -> IO ()
ppUnsignedNum n =
  case n of
    UnsignedInteger i -> putStr (show i)
    UnsignedReal r -> ppUnsignedReal r

-- Pretty Print Unsigned Constants
ppUnsignedConstant :: PazParser.ASTUnsignedConstant -> IO ()
ppUnsignedConstant c =
  case c of
    UnsignedNumberConstant n -> ppUnsignedNum n
    CharacterStringConstant s -> putStr ("'" ++ s ++ "'")

-- Pretty Print Adding Operators
ppAddingOperator :: PazParser.ASTAddingOperator -> IO ()
ppAddingOperator a =
  case a of
    AddOpPlus -> putStr " + "
    AddOpMinus -> putStr " - "
    AddOpOr -> putStr " or "

-- Pretty Print Multiplying Operators
ppMultOperator :: PazParser.ASTMultOperator -> IO ()
ppMultOperator m =
  case m of
    MultOpTimes -> putStr " * "
    MultOpDivideBy -> putStr " / "
    MultOpDiv -> putStr " div "
    MultOpAnd -> putStr " and "

-- Pretty Print Factors
ppFactor :: PazParser.ASTFactor -> PrevSign -> IO ()
ppFactor f prev =
  case f of
    UnsignedConstantFactor cf -> ppUnsignedConstant cf
    VariableAccessFactor va -> ppVariableAccess va
    ExpressionFactor ef -> do
      ppExpression ef prev
    FactorFactor ff -> do
      putStr "not "
      ppFactor ff NotOp
 
ppFactorList :: [(ASTMultOperator, ASTFactor)] -> IO ()
ppFactorList [] = return ()
ppFactorList ((mulop, factor):fs) = do
  ppMultOperator mulop
  ppFactor factor MulOp
  ppFactorList fs

-- Pretty Print Terms
ppTerm :: PazParser.ASTTerm -> PrevSign -> IO ()
ppTerm (factor, []) prev = do
  ppFactor factor Empty
ppTerm (factor, factors) prev = do
  ppFactor factor MulOp
  ppFactorList factors

-- Pretty Print Term Lists
ppTermList :: [(ASTAddingOperator, ASTTerm)] -> IO ()
ppTermList [] = return ()
ppTermList ((addop, term):ts) = do
  ppAddingOperator addop
  ppTerm term AddOp
  ppTermList ts


-- Pretty Print Simple Expressions
ppSimpleExpression :: PazParser.ASTSimpleExpression -> PrevSign -> IO ()
-- TODO: for our paren issues, this gets called for all the boolean ones, i think. 
ppSimpleExpression ((Nothing), term, []) prev = do
  ppTerm term Empty


-- maybe delete
ppSimpleExpression ((Just sign), (factor, []), []) prev = do
  putStr (ppSign sign)
  case sign of
    PazParser.SignMinus -> do
      -- TODO it is here.
      case factor of
        ExpressionFactor ef -> do
          -- TODO HERE: it now does -(y) for all y,
          -- the problem is, if y looks like (2*3*4), we do not want
          -- the paran.
          -- If y looks like (2 + 3 + 4), we want the paran.
          -- Best I could figure out is, if it is a list of terms, 
          -- it will look like 2+3+4 and then we shd put in the paran.
          -- else, we do not want to put in paran.
          putStr "("
          ppTerm (factor, []) Empty  
          putStr ")"
        _ -> do 
          ppTerm (factor, []) Empty
          
    _ -> do
      ppTerm (factor, []) Empty    

-- NOt sure if this is necessary TODO
ppSimpleExpression ((Just sign), term, []) prev = do
 
  putStr (ppSign sign)
  ppTerm term Empty


ppSimpleExpression ((Nothing), term, terms) prev = do
  if prev == Empty || prev == AddOp -- PROBLEM
    then do
      ppTerm term AddOp
      ppTermList terms
    else do
      putStr "(" -- PROBLEM
      ppTerm term AddOp
      ppTermList terms
      putStr ")"
ppSimpleExpression ((Just sign), term, terms) prev = do
  if prev == Empty || prev == AddOp 
    then do
      putStr (ppSign sign)
      ppTerm term AddOp
      ppTermList terms
    else do
      putStr (ppSign sign)
      putStr "(" -- PROBLEM
      ppTerm term AddOp
      ppTermList terms
      putStr ")"

-- Pretty Print Relational Operators
ppRelOperator :: PazParser.ASTRelationalOperator -> IO ()
ppRelOperator r =
  case r of
    ROEqual -> putStr " = "
    RONotEqual -> putStr " <> "
    ROLessThan -> putStr " < "
    ROGreaterThan -> putStr " > "
    ROLessThanOrEqual -> putStr " <= "
    ROGreaterThanOrEqual -> putStr " >= "

-- Pretty Print Expressions
ppExpression :: PazParser.ASTExpression -> PrevSign -> IO ()
ppExpression (simple, Nothing) prev = do
  if prev == Empty
    then do
      ppSimpleExpression simple prev
    else do
      putStr "(" --PROBLEM
      ppSimpleExpression simple Empty
      putStr ")" -- PROBLEM
ppExpression (simple, (Just (relop, simple2))) prev = do

  if prev == Empty
    then do
      ppSimpleExpression simple prev
      ppRelOperator relop
      ppSimpleExpression simple2 prev
    else do
      putStr "("    -- PROBLEM
      ppSimpleExpression simple Empty
      ppRelOperator relop
      ppSimpleExpression simple2 Empty
      putStr ")"     --PROBLEM

-- Pretty Print Actual Parameters for Procedure Statements
ppActualParamList :: PazParser.ASTActualParameterList -> IO ()
ppActualParamList [] = return ()
ppActualParamList [x] = ppExpression x Empty
ppActualParamList (x:xs) = do
  ppExpression x Empty
  putStr ", "
  ppActualParamList xs

-- Pretty Print Indexed Variables
ppIndexedVariable :: PazParser.ASTIndexedVariable -> IO ()
ppIndexedVariable (ident, expression) = do
  putStr (ppIdentifier ident)
  putStr "["
  ppExpression expression Empty
  putStr "]"

-- Pretty Print Accessed Variables
ppVariableAccess :: PazParser.ASTVariableAccess -> IO ()
ppVariableAccess v =
  case v of
    IndexedVariableVariableAccess va -> ppIndexedVariable va
    IdenfierVariableAccess i -> putStr (ppIdentifier i)

-- Pretty Print Procedure Statements
ppProcedureStmt :: PazParser.ASTProcedureStatement -> Int -> IO ()
ppProcedureStmt (i, Nothing) indent = do
  putStr "\n"
  ppIndent indent
  putStr (ppIdentifier i)
ppProcedureStmt (i, (Just params)) indent = do
  putStr "\n"
  ppIndent indent
  putStr (ppIdentifier i)
  putStr "("
  ppActualParamList params
  putStr ")"

-- Pretty Print Assignment Statements
ppAssignmentStmt :: PazParser.ASTAssignmentStatement -> Int -> IO ()
ppAssignmentStmt (variable, expression) indent = do
  putStr "\n"
  ppIndent indent
  case variable of
    VariableAcessAssignmentStatement va ->  ppVariableAccess va
    IdentifierAssignmentStatement i -> putStr (ppIdentifier i)
  putStr " := "
  ppExpression expression Empty

-- Pretty Print If Statements
ppIfStmt :: PazParser.ASTIfStatement -> Int -> IO ()
ppIfStmt (expression, thenstmt, (Nothing)) indent = do
  putStr "\n"
  ppIndent indent
  putStr "if "
  ppExpression expression Empty
  putStr " then"
  ppStatement thenstmt (indent+indentSpacing)

ppIfStmt (expression, thenstmt, (Just elsestmt)) indent = do
  putStr "\n"
  ppIndent indent
  putStr "if "
  ppExpression expression Empty
  putStr " then"
  ppStatement thenstmt (indent+indentSpacing)
  putStr "\n"
  ppIndent indent
  putStr "else"
  ppStatement elsestmt (indent+indentSpacing)

-- Pretty Print While Statements
ppWhileStmt :: PazParser.ASTWhileStatement -> Int -> IO ()
ppWhileStmt (expression, statement) indent = do
  putStr "\n"
  ppIndent indent
  putStr "while "
  ppExpression expression Empty
  putStr " do"
  ppStatement statement (indent+indentSpacing)

-- Pretty Print For Statements
ppForStmt :: PazParser.ASTForStatement -> Int -> IO ()
ppForStmt (ident, expr1, expr2, stmt) indent = do
  putStr "\n"
  ppIndent indent
  putStr "for "
  putStr (ppIdentifier ident)
  putStr " := "
  ppExpression expr1 Empty
  putStr " to "
  ppExpression expr2 Empty
  putStr " do"
  ppStatement stmt (indent+indentSpacing)

-- Pretty Print Statements
ppStatement :: PazParser.ASTStatement -> Int -> IO ()
ppStatement s indent = do
  case s of
    AssignmentStatement assigs -> ppAssignmentStmt assigs indent
    ProcedureStatement ps -> ppProcedureStmt ps indent
    CompoundStatement cs -> ppCompound cs indent
    IfStatement is -> ppIfStmt is indent
    WhileStatement ws -> ppWhileStmt ws indent
    ForStatement fs -> ppForStmt fs indent
    EmptyStatement -> putStr "\n"

-- Pretty Print Statement Sequences
ppStatementSequence :: PazParser.ASTStatementSequence -> Int -> IO ()
ppStatementSequence [] _ = return ()
ppStatementSequence [x] indent = do
  ppStatement x indent
ppStatementSequence (x:xs) indent = do
  ppStatement x indent
  putStr ";"
  ppStatementSequence xs indent

-- Pretty Print Compound Statements
ppCompound :: PazParser.ASTCompoundStatement -> Int -> IO ()
ppCompound (c) indent = do
  putStr "\n"
  ppIndent (indent-indentSpacing)
  putStr "begin"
  ppStatementSequence c indent
  putStr "\n"
  ppIndent (indent-indentSpacing)
  putStr "end"
