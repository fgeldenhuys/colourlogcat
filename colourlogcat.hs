import Control.Applicative ((<$>), (<*>))
import qualified Control.Exception as CE
import Control.Monad (when)
import Control.Monad.State.Lazy
import Data.Time
import qualified Data.Map as Map
import Debug.Trace
import Text.ParserCombinators.Parsec (Parser, (<|>), (<?>), between, char, choice,
                                      count, digit, endBy, lookAhead, many,
                                      many1, manyTill, noneOf, notFollowedBy, oneOf,
                                      parse, space, string, try, upper)
import Text.Printf
import System.Console.ANSI
import System.IO
import System.IO.Error

data LogLevel = Verbose | Debug | Info | Warn | Error | Fatal

instance Show LogLevel where
  show Verbose = "V"
  show Debug = "D"
  show Info = "I"
  show Warn = "W"
  show Error = "E"
  show Fatal = "F"

logLevelSGR :: LogLevel -> IO ()
logLevelSGR Verbose = do setSGR [SetColor Foreground Vivid White]
                         setSGR [SetColor Background Dull Blue]
logLevelSGR Debug = do setSGR [SetColor Foreground Vivid Black]
                       setSGR [SetColor Background Dull Blue]
logLevelSGR Info = do setSGR [SetColor Foreground Vivid White]
                      setSGR [SetColor Background Dull Blue]
logLevelSGR Warn = do setSGR [SetColor Foreground Dull Black]
                      setSGR [SetColor Background Dull Yellow]
logLevelSGR Error = do setSGR [SetColor Foreground Dull Black]
                       setSGR [SetColor Background Dull Red]
logLevelSGR Fatal = do setSGR [SetColor Foreground Dull Black]
                       setSGR [SetColor Background Dull Magenta]

data LogFragment = Text String |
                   Emphasized String |
                   BlockBrackets String |
                   TimeStamp String
                   deriving Show

data LogEntry = LogEntry { getLevel :: LogLevel
                         , getTag :: String
                         , getPid :: Int
                         , getMessage :: [LogFragment]
                         }
              | LogComment { getText :: String }
              | LogError { getError :: String
                         , getRaw :: String
                         }
              deriving Show

styleOptions :: [IO ()]
styleOptions = make <$> bg <*> fg
  where
    bg = [(Dull, Black), (Vivid, Green)]
    fg = [ (Dull, Blue), (Dull, Magenta), (Dull, Green), (Dull, Red)
         , (Dull, Yellow), (Dull, Cyan), (Vivid, Red), (Vivid, Magenta)]
    make b f = do
      setSGR [SetColor Background (fst b) (snd b)]
      setSGR [SetColor Foreground (fst f) (snd f)]

presetTagStyles :: [(String, IO ())]
presetTagStyles = [ ("System.err", setSGR [SetColor Foreground Dull Black] >> setSGR [SetColor Background Dull Red])
                  , ("System.out", setSGR [SetColor Foreground Dull Black] >> setSGR [SetColor Background Dull Green])
                  , ("dalvikvm", setSGR [SetColor Foreground Vivid Black] >> setSGR [SetColor Background Dull Yellow])
                  , ("dalvikvm-heap", setSGR [SetColor Foreground Vivid Black] >> setSGR [SetColor Background Dull Yellow])
                  , ("ActivityManager", setSGR [SetColor Foreground Vivid Black] >> setSGR [SetColor Background Dull Green])
                  ]

maxTagWidth :: Int
maxTagWidth = 18

tagWidthReduceAt :: Int
tagWidthReduceAt = 10

showPauseTime :: NominalDiffTime
showPauseTime = 5

data LogState = LogState { getStyleCycle :: [IO ()]
                         , getTagStyles :: Map.Map String (IO ())
                         , getTagWidth :: Int
                         , getTagWidthInertia :: Int
                         , getLastTime :: UTCTime
                         }

traceShow' :: Show a => a -> a
traceShow' x = traceShow x x

main :: IO ()
main = do
  hSetEncoding stdin char8
  printColors
  let styleCycle = cycle styleOptions
  let tagStyles = Map.fromList presetTagStyles
  now <- getCurrentTime
  process LogState { getStyleCycle = styleCycle
                   , getTagStyles = tagStyles
                   , getTagWidth = 9
                   , getTagWidthInertia = 0
                   , getLastTime = now
                   }

printColors :: IO ()
printColors = do
  let colors = combine <$> [("Dull", Dull), ("Vivid", Vivid)] <*>
               [("Black", Black), ("Red", Red), ("Green", Green)
               , ("Yellow", Yellow), ("Blue", Blue), ("Magenta", Magenta)
               , ("Cyan", Cyan), ("White", White)]
  mapM_ printColor colors
  return ()
  where
    combine a b = (fst a ++ " " ++ fst b, (snd a, snd b))
    printColor pair = do
      let (text, c) = pair
      setSGR [SetColor Foreground (fst c) (snd c)]
      putStrLn text

process :: LogState -> IO ()
process initState = do
  now <- getCurrentTime
  let deltaTime = diffUTCTime now (getLastTime initState)
  when (deltaTime > showPauseTime) $ do
    setSGR [SetColor Foreground Vivid Magenta]
    putStrLn $ printf "\t --- %s --- " (show deltaTime)
    setSGR [Reset]
  line <- CE.handle handleInputError getLine
  let log = parseLine line
  nextState <- case log of
    LogEntry {} -> execStateT (printLog log) initState
    LogComment text -> do
      setSGR [SetColor Background Dull Blue]
      setSGR [SetColor Foreground Dull Black]
      putStrLn $ text ++ " "
      setSGR [Reset]
      return initState
    LogError err raw -> do
      setSGR [SetColor Background Vivid Red]
      setSGR [SetColor Foreground Vivid Black]
      putStrLn err
      setSGR [SetColor Background Dull Black]
      setSGR [SetColor Foreground Dull Red]
      print raw
      setSGR [Reset]
      return initState
  process nextState { getLastTime = now }
  where
    handleInputError e =
      if isEOFError e
        then error "End Of File"
        else return $ "-- " ++ (show (e :: IOError))

printLog :: LogEntry -> StateT LogState IO ()
printLog log = do
  -- Print log level
  liftIO . logLevelSGR $ getLevel log
  liftIO . putStr $ printf " %1s " $ show (getLevel log)
  liftIO $ setSGR [Reset]
  -- Print package tag
  tagStyle <- state $ runState $ getTagStyle (getTag log)
  state $ runState $ adjustTagWidth (getTag log)
  liftIO tagStyle
  tagWidth <- liftM getTagWidth get
  liftIO . putStr $ printf " %s " $ boxString (getTag log) tagWidth
  liftIO $ setSGR [Reset]
  -- Print message
  liftIO $ mapM printFragment (getMessage log)
  liftIO $ setSGR [Reset]
  liftIO $ putStrLn ""
  return ()

printFragment :: LogFragment -> IO ()
printFragment fragment =
  case fragment of
    Text text -> putStr text
    Emphasized text -> do
      setSGR [SetColor Foreground Dull White]
      putStr text
      setSGR [Reset]
    BlockBrackets text -> do
      setSGR [SetColor Foreground Dull Cyan]
      putStr text
      setSGR [Reset]
    TimeStamp text -> do
      setSGR [SetColor Foreground Vivid Green]
      putStr text
      setSGR [Reset]

getTagStyle :: String -> State LogState (IO ())
getTagStyle tag = do
  state <- get
  let currentTagStyles = getTagStyles state
  case Map.lookup tag currentTagStyles of
    Just style -> return style
    Nothing -> do
      let styleCycle = getStyleCycle state
          newStyle = head styleCycle
          newStyleCycle = tail styleCycle
          newTagStyles = Map.insert tag newStyle currentTagStyles
      put $ state { getStyleCycle = newStyleCycle
                  , getTagStyles = newTagStyles
                  }
      return newStyle

adjustTagWidth :: String -> State LogState ()
adjustTagWidth tag =
  do state <- get
     let n = length tag
         tw = getTagWidth state
         twi = getTagWidthInertia state
     put $ adjust state n tw twi
  where adjust state n tw twi
          | n > tw && tw < maxTagWidth =
            state { getTagWidth = tw + 1, getTagWidthInertia = 0 }
          | n > tw = state
          | n < tw && twi >= tagWidthReduceAt =
            state { getTagWidth = tw - 1, getTagWidthInertia = 0 }
          | n < tw = state { getTagWidthInertia = twi + 1 }
          | n == tw = state { getTagWidthInertia = 0 }

boxString :: String -> Int -> String
boxString src width
  | length src > width = reduce
  | length src == width = src
  | otherwise = pad
  where
    reduce = take width src
    pad = replicate (width - length src) ' ' ++ src

parseLine :: String -> LogEntry
parseLine line = case parse parseLogEntry "(unknown)" line of
  Left e -> LogError (show e) line
  Right x -> x

parseLogEntry :: Parser LogEntry
parseLogEntry = try logComment <|> do
  level <- logLevel
  tag <- tagName
  pid <- processId
  message <- messageLine
  return $ LogEntry level tag pid message

logComment :: Parser LogEntry
logComment = do
  lookAhead (char '-')
  text <- many1 (noneOf "\n\r") <?> "comment text"
  many (oneOf "\n\r")
  return $ LogComment text

logLevel :: Parser LogLevel
logLevel = do
  level <- oneOf "DIWEF" <?> "Valid log level character"
  char '/' <?> "Slash after log level"
  return $ levelChar level
  where levelChar 'V' = Verbose
        levelChar 'D' = Debug
        levelChar 'I' = Info
        levelChar 'W' = Warn
        levelChar 'E' = Error
        levelChar 'F' = Fatal

processId :: Parser Int
processId = do
  pid <- between (char '(') (char ')') (many (noneOf "()"))
         <?> "Process id in brackets"
  return (read pid :: Int)

tagName :: Parser String
tagName = do
  many space
  tag <- many (noneOf "( ")
  many space
  return tag

messageLine :: Parser [LogFragment]
messageLine = do
    oneOf ":" <?> "Colon after process id"
    many (try timeStampFragment
          <|> try emphasizedFragment
          <|> try blockBracketsFragment
          <|> try textFragment
          <|> paddingFragment
          <?> "Unknown message fragment")
    where
      timeStampFragment = do
        tab <- many (oneOf " \t")
        date <- count 6 digit
        char '.'
        time <- count 6 digit
        char '.'
        millis <- count 3 digit
        lookAhead $ oneOf " \t\n\r"
        return $ TimeStamp $ printf "%s%s.%s.%s" tab date time millis
      emphasizedFragment = do
        tab <- many (oneOf " \t")
        start <- upper
        text <- many1 (upper <|> digit <|> oneOf "_+-*!#$,.:/")
        lookAhead $ oneOf " \t\n\r"
        return $ Emphasized (tab ++ [start] ++ text)
      blockBracketsFragment = do
        tab <- many (oneOf " \t") <?> "whitespace"
        char '[' <?> "open block bracket"
        text <- many1 (noneOf "]\n\r") <?> "block bracket text"
        char ']' <?> "close block bracket"
        return $ BlockBrackets $ printf "%s[%s]" tab text
      textFragment = do
        tab <- many (oneOf " \t") <?> "whitespace"
        text <- many1 (noneOf " \t\n\r") <?> "message text"
        return $ Text (tab ++ text)
      paddingFragment = do
        many1 (oneOf " \t\n\r")
        return $ Text ""
