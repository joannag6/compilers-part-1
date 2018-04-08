import Debug.Trace (trace)
import Text.Parsec (parse)
import PazLexer
import PazParser
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.Environment

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
                        putStrLn ("Usage: " ++ progname ++ " [-p] filename\n\n")
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

prettyPrint :: PazParser.ASTStartSymbol -> IO ()
prettyPrint ast = putStrLn "I'm a placeholder"

