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
----------------------------------------------------------------------------
prettyPrintIdentifierList :: PazParser.ASTIdentifierList -> IO ()
prettyPrintIdentifierList ()

prettyPrintVariableDec :: PazParser.ASTVariableDeclaration -> IO ()
prettyPrintVariableDec (identifiers, types) = do
  printf "    %s: %s;\n" (show identifiers) (show identifiers)

prettyPrintVariableDecList :: [PazParser.ASTVariableDeclaration] -> IO ()
prettyPrintVariableDecList [] = return () -- do nothing
prettyPrintVariableDecList (v:vs) = do
  prettyPrintVariableDec v
  prettyPrintVariableDecList vs

prettyPrintVariableDecPart :: PazParser.ASTVariableDeclarationPart -> IO ()
prettyPrintVariableDecPart Nothing = return () -- do nothing
prettyPrintVariableDecPart (Just variables) = do
  printf "var\n"
  prettyPrintVariableDecList variables

prettyPrint :: PazParser.ASTStartSymbol -> IO ()
-- mapping ASTs to Paz programs.

-- ASTVariableDeclarationPart, ASTProcedureDeclarationPart, ASTCompoundStatement
-- prettyPrint ast = putStrLn "I'm a placeholder" -- TODO(joanna): put pretty printing here
prettyPrint (programname, variables, procedures, compound) = do
  printf "program %s;" programname
  putStr "\n\n"

  -- can print ASTIdentifier,
  prettyPrintVariableDecPart variables -- can be Nothing or list of ((ASTIdentifier, [ASTIdentifier]), ASTTypeDenoter -- array or normal type)
  print procedures
  print compound
