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
  printf "var\n"
  ppVariableDecList variables

--------------------------------------------------------------------------------
-- ASTProcedureDeclarationPart
--------------------------------------------------------------------------------
-- (Bool, ASTIdentifierList, ASTTypeDenoter)
ppParamSection :: PazParser.ASTFormalParameterSection -> String
ppParamSection (False, idlist, td) = (ppIdentifierList idlist) ++ ": " ++ (ppTypeDenoter td)
ppParamSection (True, idlist, td) = "var " ++ (ppIdentifierList idlist) ++ ": " ++ (ppTypeDenoter td)

ppParamList :: PazParser.ASTFormalParameterList -> String
ppParamList [] = ""
ppParamList [x] = (ppParamSection x)
ppParamList (x:xs) = (ppParamSection x) ++ "; " ++ (ppParamList xs)

ppProcedureDec :: PazParser.ASTProcedureDeclaration -> IO ()
ppProcedureDec (ident, (Nothing), vardecpart, compound) = do
  printf ((ppIdentifier ident) ++ ";\n")
ppProcedureDec (ident, (Just paramlist), vardecpart, compound) = do
  printf ((ppIdentifier ident) ++ "(" ++ (ppParamList paramlist) ++ ");\n")


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
ppCompound :: PazParser.ASTCompoundStatement -> IO ()
ppCompound c = do
  putStr "begin\n" -- check ordering !! only used in Compound Statements
  print c
  putStr "end\n\n" -- check ordering !! only used in Compound Statements

--------------------------------------------------------------------------------
-- Top level pretty print
--------------------------------------------------------------------------------
prettyPrint :: PazParser.ASTStartSymbol -> IO ()
-- mapping ASTs to Paz programs.

-- ASTVariableDeclarationPart, ASTProcedureDeclarationPart, ASTCompoundStatement
-- prettyPrint ast = putStrLn "I'm a placeholder" -- TODO(joanna): put pretty printing here
prettyPrint (programname, variables, procedures, compound) = do
  printf "program %s;" programname
  putStr "\n\n"

  -- can print ASTIdentifier,
  ppVariableDecPart variables -- can be Nothing or list of ((ASTIdentifier, [ASTIdentifier]), ASTTypeDenoter -- array or normal type)
  putStr "\n"

  putStr "procedure "
  ppProcedureDecPart procedures
  ppCompound compound
