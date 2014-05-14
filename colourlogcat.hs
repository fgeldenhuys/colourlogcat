import Control.Monad
import Text.ParserCombinators.Parsec
import Text.Printf
import System.Console.ANSI
import qualified Data.Map as Map

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
logLevelSGR Debug = do setSGR [SetColor Foreground Vivid Yellow]
                       setSGR [SetColor Background Dull Blue]
logLevelSGR Info = do setSGR [SetColor Foreground Vivid Cyan]
                      setSGR [SetColor Background Dull Blue]
logLevelSGR Warn = do setSGR [SetColor Foreground Dull Black]
                      setSGR [SetColor Background Dull Yellow]
logLevelSGR Error = do setSGR [SetColor Foreground Dull Black]
                       setSGR [SetColor Background Dull Red]
logLevelSGR Fatal = do setSGR [SetColor Foreground Dull Black]
                       setSGR [SetColor Background Dull Magenta]

data LogEntry = LogEntry { getLevel :: LogLevel
                         , getTag :: String
                         , getPid :: Int
                         , getMessage :: String
                         } deriving (Show)

styleOptions :: [IO ()]
styleOptions = [ setSGR [SetColor Foreground Dull Blue] >> setSGR [SetColor Background Dull Black]
               , setSGR [SetColor Foreground Dull Cyan] >> setSGR [SetColor Background Dull Black]
               , setSGR [SetColor Foreground Dull Magenta] >> setSGR [SetColor Background Dull Black]
               , setSGR [SetColor Foreground Dull White] >> setSGR [SetColor Background Dull Black]
               , setSGR [SetColor Foreground Dull Yellow] >> setSGR [SetColor Background Dull Black]
               , setSGR [SetColor Foreground Vivid Cyan] >> setSGR [SetColor Background Dull Black]
               , setSGR [SetColor Foreground Vivid Magenta] >> setSGR [SetColor Background Dull Black]
               , setSGR [SetColor Foreground Vivid White] >> setSGR [SetColor Background Dull Black]
               , setSGR [SetColor Foreground Vivid Yellow] >> setSGR [SetColor Background Dull Black]
               ]

data LogState = LogState { getStyleCycle :: [IO ()]
                         , getTagStyles :: Map.Map String (IO ())
                         }

main :: IO ()
main = do
  let styleCycle = cycle styleOptions
  process LogState {getStyleCycle = styleCycle, getTagStyles = Map.empty}

process :: LogState -> IO ()
process initState = do
  line <- getLine
  let log = parseLine line
  nextState <- case log of
    Just entry -> printLog initState entry
    Nothing -> do
      print line
      return initState
  process nextState

printLog :: LogState -> LogEntry -> IO LogState
printLog initState log = do
  -- Print log level
  logLevelSGR $ getLevel log
  putStr $ printf " %1s " $ show (getLevel log)
  setSGR [Reset]
  -- Print package tag
  (tagStyle, nextState) <- getTagStyle initState (getTag log)
  tagStyle
  putStr $ printf " %s " $ getTag log
  setSGR [Reset]
  -- Print message
  putStrLn $ printf " %s" $ getMessage log
  return nextState

getTagStyle :: LogState -> String -> IO (IO (), LogState)
getTagStyle initState tag = do
  let currentTagStyles = getTagStyles initState
  case Map.lookup tag currentTagStyles of
    Just style -> return (style, initState)
    Nothing -> do
      let styleCycle = getStyleCycle initState
      let newStyle = head styleCycle
      let newStyleCycle = tail styleCycle
      let newTagStyles = Map.insert tag newStyle currentTagStyles
      return (newStyle, initState { getStyleCycle = newStyleCycle
                                  , getTagStyles = newTagStyles
                                  })

parseLine :: String -> Maybe LogEntry
parseLine line = case parse logLine "(unknown)" line of
  Right (x:_) -> Just x
  Right [] -> Nothing
  Left _ -> Nothing

logLine = endBy body eol

body = do
  level <- logLevel
  package <- many (noneOf "(")
  pid <- processId
  message <- messageLine
  return $ LogEntry level package pid message

eol = try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"

logLevel = do
  level <- oneOf "DIWEF"
  char '/'
  return $ levelChar level
  where levelChar 'D' = Debug
        levelChar 'I' = Info
        levelChar 'W' = Warn
        levelChar 'E' = Error
        levelChar 'F' = Fatal

processId = do
  pid <- between (char '(') (char ')') (many (noneOf "()"))
  return (read pid :: Int)

messageLine = do
  oneOf ":"
  many space
  many (noneOf "\n\r")