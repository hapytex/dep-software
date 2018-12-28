{-# LANGUAGE GADTs #-}
-- | A package that links all submodules together into a program.
module Dep(main) where

import Control.Monad(liftM,void)

import Data.ByteString(hPut)
import Data.Char(toLower,toUpper)
import Data.EnumSet(T,fromEnum,fromEnums,toEnums)
import Data.Function(on)
import Data.List(find)
import Data.Maybe(isNothing,isJust,fromJust)
import Data.Text(strip,pack,unpack)
import Data.Time.Clock(utctDay,getCurrentTime)
import Data.Time.Calendar(toGregorian)

import Graphics.UI.Gtk(mainGUI,widgetShowAll,onDestroy,mainQuit,containerAdd)

import System.Console.ANSI(ConsoleIntensity(BoldIntensity,NormalIntensity),SGR(SetConsoleIntensity),setSGR)
import System.Console.GetOpt(ArgDescr(NoArg,ReqArg),ArgOrder(Permute),getOpt,OptDescr(Option),usageInfo)
import System.Console.Terminal.Size(size,Window(width))
import System.Environment(getArgs)
import System.IO(Handle,hClose,hFlush,hPutStrLn,IOMode(WriteMode),openFile,stdout)

import Dep.Algorithms.Comb(synthetize)
import Dep.Algorithms.Seq(concatReduceFSM,graySequence)
import Dep.Algorithms.Cpu(CiscProgram)
import Dep.Cisc.Parser()
-- import Dep.Ui(clearAndExit,runMinimizeFsm,runShowKarnaugh,runSynthetize)
import Dep.Gui.Utils(initDepGui,depWindow)
import Dep.Gui.Utils.SchematicsViewer(schematicsViewerNew)
import Dep.Tables.Parser() -- used to import readability of datastructures
import Dep.Utils(similarity,argmin)
import Dep.Structures as Str

-- TODO: replace with imports
runShowKarnaugh :: String -> String -> IO ()
runShowKarnaugh = undefined

runSynthetize :: String -> String -> IO ()
runSynthetize = undefined

runMinimizeFsm :: String -> String -> IO ()
runMinimizeFsm = undefined

clearAndExit :: IO a
clearAndExit = undefined
-- END

-- | The list of input/output datatypes the program supports.
data IOType = IOInt  -- ^ An integral number.
            | IOCT   -- ^ A combinatorial table.
            | IOKC   -- ^ A Karnaugh card.
            | IOEC   -- ^ An electronics circuit.
            | IOMST  -- ^ A finite state machine, any flavor.
            | IOMoST -- ^ A finite state machine, Moore flavor.
            | IOMyST -- ^ A finite state machine, Mealy flavor.
            | IOMET  -- ^ An encoded finite state machine, any flavor.
            | IOMoET -- ^ An encoded finite state machine, Moore flavor.
            | IOMyET -- ^ An encoded finite state machine, Mealy flavor.
            | IOBS   -- ^ A bitstring.
            | IOBSs  -- ^ A sequence of bitstrings.
            | IOCisc -- ^ A CISC program.
            | IOBin  -- ^ A binary stream.
            deriving (Bounded,Enum,Eq,Ord,Read)

data Command = Cmnd {info :: Description, function :: Driver -> String -> String -> Handle -> IO (), interactiveFunction :: Maybe (String -> String -> IO())}

data CmndFun q i o where
    CmndQ :: (Read q, Read i, Printable o) => (q -> i -> o) -> CmndFun q i o    --command with query
    CmndN :: (Read i, Printable o) => (i -> o) -> CmndFun String i o            --command without query

-- | A description for a given functionality for the dep program.
data Description = Desc {
    name :: String -- ^ The name of the function, used to call the function.
   ,excerpt :: String -- ^ The excerpt of the function, used to shortly describe the functionality.
   ,input :: IOType -- ^ The type of input of the function, for instance a combinatorial table.
   ,output :: IOType -- ^ The type of output of the function, for instance a binary stream.
}

-- The options that can be passed to the program in order to perform the correct functionality. Some are mandatory.
data ProgramOptions = Options {
    optShowVersion :: Bool -- ^ An option indicating whether the version of the program should be shown.
   ,optShowHelp    :: Bool -- ^ An option indicating whether the help manual should be shown.
   ,fullDocument   :: Bool -- ^ An option indicating whether a standalone document should be generated.
   ,outputDriver   :: Driver -- ^ The driver that is called, determining the output format.
   ,query          :: String -- ^ The query with which the proper function is called.
   ,interactive    :: Bool -- A boolean indicating whether interactive mode is required.
   ,outputFile     :: Maybe String -- ^ The name of the output file. If Nothing, redirects to stdout.
}

-- | The name of each input/output type.
instance Show IOType where
    show IOInt = "integer"
    show IOBS = "bitstring"
    show IOBSs = "Sequence of bitstrings"
    show IOCT = "combinatorial table"
    show IOKC = "Karnaugh card"
    show IOEC = "electronic circuit"
    show IOMoST = "Moore state table"
    show IOMyST = "Mealy state table"
    show IOMST  = "State table"
    show IOMoET = "Moore encoding table"
    show IOMyET = "Mealy encoding table"
    show IOMET  = "Encoding table"
    show IOCisc = "CISC Assembler source code"
    show IOBin = "Binary"

-- | The description of each input/output type.
instance Describeable IOType where
    description IOInt = "An integral number."
    description IOBS = "A sequence of bits, potentially a bit can be a don't care."
    description IOBSs = "A sequence of bitstrings. Potentially a bit can be a don't care."
    description IOCT = "A table consists out of two columns. Each entry contains a bitstring."
    description IOCisc = "Assembler source code for the CISC processor. You can build/simulate programs with the compiler/interpreter."
    description IOBin = "A sequence of zeros and ones. You better use I/O redirection to load/save it to a file."
    description _ = ""

-- | The list of supported commands. Each command contains a description as well as the proper functions to provide the described functionality.
commands :: [Command]
commands = [
        Cmnd (Desc "expand" "Takes as input a combinatorial table and expands the combinatorial table (don't care expansion)." IOCT IOCT) (toProg (expand :: CombTable -> CombTable)) Nothing,
        Cmnd (Desc "reduce" "Takes as input a combinatorial table and reduces it by placing don't cares at the input side." IOCT IOCT) (toProg (reduce :: CombTable -> CombTable)) Nothing,
        Cmnd (Desc "lookup" "Takes as input a combinatorial table and as query a bit sequence (optionally with don't cares)." IOCT IOBS) (toProgQ (btlookupThJust :: BitThSeq -> CombTable -> BitThSeq)) Nothing,
        Cmnd (Desc "showKarnaugh" "Takes as input a combinatorial table and prints the corresponding Karnaugh card(s)." IOCT IOKC) (toProg KC) (Just runShowKarnaugh),
        Cmnd (Desc "synthetize" "Generate a sum-of-products for the given combinatorial table." IOCT IOKC) (toProg synthetize) (Just runSynthetize),
        Cmnd (Desc "minimize-fsm" "Minimize the given finite state machine to a finite state machine with a minimum amount of states. This commands works on both Moore and Mealy machines." IOMST IOMST) (toProg (concatReduceFSM :: FSM String BitThSeq BitThSeq -> FSM String BitThSeq BitThSeq)) (Just runMinimizeFsm),
        Cmnd (Desc "generate-gray" "Generate for the given number, the sequence of Gray codes with the given number of bits." IOInt IOBSs) (toProg graySequence) Nothing,
        Cmnd (Desc "compile-cisc" "Compile the CISC assembler-language input to CISC machine code." IOCisc IOBin) (toProgB (id :: CiscProgram -> CiscProgram)) Nothing
    ]

-- | A function that maps the input/output types on a set of supported drivers. By default, only the ASCII driver is supported.
supportedDrivers :: IOType -- ^ The given I/O type to determine the supported output drivers from.
    -> T Int Driver -- ^ The resulting set of output drivers.
supportedDrivers IOCT = Data.EnumSet.fromEnums [ASCII,SVG,LaTeX]
supportedDrivers IOKC = Data.EnumSet.fromEnum ASCII
supportedDrivers IOEC = Data.EnumSet.fromEnum ASCII
supportedDrivers _ = Data.EnumSet.fromEnum ASCII

-- | Convert the given function into an IO program that takes as input a driver, query and input.
toProg :: (Read a, Printable b) => (a -> b) -> Driver -> String -> String -> Handle -> IO ()
toProg f d _ ip h = do
    cw <- consoleWidth
    hPutStrLn h $ Str.print d cw $ f $ read ip

toProgS :: (Read a) => (a -> String) -> Driver -> String -> String -> Handle -> IO ()
toProgS f _ _ ip h = hPutStrLn h $ f $ read ip

toProgQ :: (Read a, Read q, Printable b) => (q -> a -> b) -> Driver -> String -> String -> Handle -> IO ()
toProgQ f d q ip h = do
    cw <- consoleWidth
    hPutStrLn h $ Str.print d cw $ f (read q) $ read ip

toProgB :: (Read a, Serializeable b) => (a -> b) -> Driver -> String -> String -> Handle -> IO ()
toProgB f _ _ ip h = hPut h $ Str.serializeByteString $ f $ read ip

defaultOptions :: ProgramOptions
defaultOptions = Options {
        optShowVersion = False,
        optShowHelp    = False,
        outputDriver = ASCII,
        fullDocument = False,
        query = [],
        interactive = False,
        outputFile = Nothing
    }

getOutputHandle :: ProgramOptions -> IO Handle
getOutputHandle p | Just fn <- outputFile p = openFile fn WriteMode
                  | otherwise = return stdout

options :: [OptDescr (ProgramOptions -> ProgramOptions)]
options =
    [ Option "v" ["version"]
        (NoArg (\opts -> opts { optShowVersion = True }))
        "Show version number."
    , Option "h?" ["help"]
        (NoArg (\opts -> opts { optShowHelp = True }))
        "Show this help sequence."
    , Option [] ["ascii"]
        (NoArg (\opts -> opts { outputDriver = ASCII }))
        "Print output using the ASCII driver."
    , Option [] ["svg"]
        (NoArg (\opts -> opts { outputDriver = SVG }))
        "Print output using the HTML/SVG driver."
    , Option [] ["html"]
        (NoArg (\opts -> opts { outputDriver = SVG }))
        "Print output using the HTML/SVG driver."
    , Option [] ["latex"]
        (NoArg (\opts -> opts { outputDriver = LaTeX }))
        "Print output using the LaTeX driver."
    , Option "s" ["standalone"]
        (NoArg (\opts -> opts { fullDocument = True }))
        "Create a standalone document (only when using the HTML/SVG or LaTeX driver)."
    , Option "o" ["output"]
        (ReqArg (\q opts -> opts {outputFile = Just q}) "file")
        "Write the output to the specified file instead of STDOUT."
    , Option "q" ["query"]
        (ReqArg (\q opts -> opts { query = q }) "query")
        "Provide a query context (for the specified command)."
    , Option "i" ["interactive"]
        (NoArg (\opts -> opts { interactive = True }))
        "The program will - if possible for the given task - use an text-based interactive user interface. The terminal in which the program runs should support the ANSI terminal standard."
    ]

compilerOpts :: [String] -> IO (ProgramOptions, [String])
compilerOpts argv =
  case getOpt Permute options argv of
     (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
         where header = "Usage: dep command [OPTIONS...]"

stripTitle :: String -> String
stripTitle = unpack.strip.pack

stripContent :: String -> String
stripContent x = ' ':' ':(unpack.strip.pack) x

printHeader :: String -> IO ()
printHeader t = do
    ic <- isConsole
    if ic then do
        setSGR [SetConsoleIntensity BoldIntensity]
        putStrLn $ map toUpper $ stripTitle t
        setSGR [SetConsoleIntensity NormalIntensity]
    else putStrLn $ map toUpper $ stripTitle t

printChapter :: String -> String -> IO ()
printChapter t c = do
    printHeader t
    putStrLn $ stripContent c
    putStrLn []

printChapterList :: String -> (a -> IO ()) -> [a] -> IO ()
printChapterList t f vs = do
    printHeader t
    putStrLn []
    mapM_ f vs
    putStrLn []

showExcerpt :: Command -> IO()
showExcerpt (Cmnd d _ _) = do
    ic <- isConsole
    if ic then do
        setSGR [SetConsoleIntensity BoldIntensity]
        putStrLn $ "  "++name d
        setSGR [SetConsoleIntensity NormalIntensity]
        putStrLn $ "    "++excerpt d
    else do
        putStrLn $ "  "++name d
        putStrLn $ "    "++excerpt d

-- | Get the current date according the Gergorian calendar represented as a tuple containing the year, month and day.
date :: IO (Integer,Int,Int) -- ^ An I/O operation returning a tuple contain the year, month and day.
date = fmap (toGregorian . utctDay) getCurrentTime

header :: IO ()
header = putStrLn $ unlines [" ____             ",
                             "|  _ \\  ___ _ __  ",
                             "| | | |/ _ \\ '_ \\ ",
                             "| |_| |  __/ |_) |",
                             "|____/ \\___| .__/ ",
                             "           |_|"]

copyright :: IO ()
copyright = do
    (yr,_,_) <- date
    printChapter ca $ (++) cb (if yr > 2015 then '-':show yr++cc else cc)
    where ca = "copyright"
          cb = "dep is copyright (c) 2015"
          cc = " by Willem M. A. Van Onsem"

printNegation :: Bool -> String
printNegation False = " not"
printNegation True = ""


showFunctionHelp :: String -> Command -> Description -> IO ()
showFunctionHelp s c d = do
    printChapter "name"  $ "dep "++s++" - "++excerpt d
    printChapter "input" $ namedescribe $ input d
    printChapter "output" $ namedescribe (output d)++"\n\n  Supported output formats:\n"++unlines (map (\x -> "    - "++show x) $ toEnums $ supportedDrivers $ output d)
    printChapter "interactive" $ "Interactive mode is"++printNegation (isJust $ interactiveFunction c)++" supported."
    copyright

processCommand :: String -> Maybe Command -> ProgramOptions -> String -> IO ()
processCommand s Nothing _ _ = error ("Command \""++s++"\" unknown. Perhaps you meant \""++argmin (on similarity maplow s) cns++"\"?")
        where cns = map (name.info) commands
              maplow = map toLower
processCommand s (Just c) po inp | optShowHelp po = showFunctionHelp s c d
                                 | not $ interactive po = do
                                     h <- getOutputHandle po
                                     function c (outputDriver po) (query po) inp h
                                     hFlush h
                                     hClose h
                                 | isNothing ic = error ("No interactive mode supported for command \""++s++"\".")
                                 | otherwise = void (fic (query po) inp >> clearAndExit)
    where d = info c
          ic = interactiveFunction c
          fic = fromJust ic

isConsole :: IO Bool
isConsole = fmap isJust size

consoleWidth :: IO (Maybe Int)
consoleWidth = fmap (fmap width) size
--consoleWidth = liftM f (size :: IO (Maybe (Window Int)))
--    where f :: Maybe (Window Int) -> Maybe Int
--          f (Just wind) = width wind
--          f Nothing = Nothing

fetchInput :: [String] -> IO String
fetchInput (_:fn:_) = readFile fn
fetchInput _ = getContents

-- | The main entry of the program: it reads the parameters and from stdin and calls the appropriate function.

--{-
main :: IO ()
main = do
    s <- initDepGui
    w <- depWindow s "Schematics Simulator"
    sv <- schematicsViewerNew
    w `containerAdd` sv
    onDestroy w mainQuit
    widgetShowAll w
    mainGUI
--}
{-
main :: IO ()
main = do
    args <- getArgs
    (po,er) <- compilerOpts args
    if length er <= 0
    then do
        header
        printChapter "name" "dep - digital electronics synthesis and analysis"
        printChapter "synopsis" "dep [COMMAND] [OPTIONS] [FILE]\n  dep [COMMAND] [OPTIONS] --interactive FILE\n  dep [COMMAND] --help"
        printChapter "description" "A companion program for the book \"Digitale Elektronica en Processoren\" by Willem M. A. Van Onsem."
        printChapterList "commands" showExcerpt commands
        printChapter "options" $ usageInfo "" options
        printChapter "input" "By default, the program reads from STDIN, if a FILE is provided, it reads from the file.\n  In case interactive mode is used, the program cannot read from STDIN (since it reads keystrokes), in that case you should provide a FILE."
        copyright
    else do
        inp <- fetchInput er
        processCommand (head er) (find ((==) (head er) . name . info) commands) po inp
--}
