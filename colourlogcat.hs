import Control.Applicative ((<$>), (<*>))
import Control.Monad.State.Lazy
import Debug.Trace
import Text.ParserCombinators.Parsec (Parser, (<|>), between, char, choice, digit, endBy, lookAhead, many,
                                      many1, manyTill, noneOf, notFollowedBy, oneOf,
                                      parse, space, string, try, upper)
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

data LogFragment = Text String | Emphasized String deriving Show

data LogEntry = LogEntry { getLevel :: LogLevel
                         , getTag :: String
                         , getPid :: Int
                         , getMessage :: [LogFragment]
                         } deriving Show

styleOptions :: [IO ()]
styleOptions = make <$> bg <*> fg
  where
    bg = [(Dull, Black), (Vivid, Green)]
    fg = [(Dull, Red), (Dull, Green), (Dull, Yellow), (Dull, Blue), (Dull, Magenta), (Dull, Cyan), (Vivid, Red), (Vivid, Magenta)]
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

data LogState = LogState { getStyleCycle :: [IO ()]
                         , getTagStyles :: Map.Map String (IO ())
                         , getTagWidth :: Int
                         , getTagWidthInertia :: Int
                         }

traceShow' :: Show a => a -> a
traceShow' x = traceShow x x

main :: IO ()
main = do
  -- printColors
  let styleCycle = cycle styleOptions
  let tagStyles = Map.fromList presetTagStyles
  process LogState { getStyleCycle = styleCycle
                   , getTagStyles = tagStyles
                   , getTagWidth = 9
                   , getTagWidthInertia = 0
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
  line <- getLine
  let log = parseLine line
  nextState <- case log of
    Just entry -> execStateT (printLog entry) initState
    Nothing -> do
      putStrLn line
      return initState
  process nextState

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
printFragment fragment = do
  case fragment of
    Text text -> putStr text
    Emphasized text -> do
      setSGR [SetColor Foreground Dull White]
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

parseLine :: String -> Maybe LogEntry
parseLine line = case parse logLine "(unknown)" line of
  Right (x:_) -> Just x
  Right [] -> Nothing
  Left _ -> Nothing

logLine = endBy body eol

body = do
  level <- logLevel
  tag <- tagName
  pid <- processId
  message <- messageLine
  return $ LogEntry level tag pid message

eol = try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"

logLevel = do
  level <- oneOf "DIWEF"
  char '/'
  return $ levelChar level
  where levelChar 'V' = Verbose
        levelChar 'D' = Debug
        levelChar 'I' = Info
        levelChar 'W' = Warn
        levelChar 'E' = Error
        levelChar 'F' = Fatal

processId = do
  pid <- between (char '(') (char ')') (many (noneOf "()"))
  return (read pid :: Int)

tagName :: Parser String
tagName = do
  many space
  tag <- many (noneOf "( ")
  many space
  return tag

messageLine :: Parser [LogFragment]
messageLine = do
    oneOf ":"
    many $ try emphasizedFragment <|> textFragment
    where
      emphasizedFragment = do
        tab <- many (oneOf " \t")
        start <- upper
        text <- many1 (upper <|> digit <|> oneOf "_+-*!#$,.:/")
        lookAhead $ oneOf " \t\n\r"
        return $ Emphasized (tab ++ [start] ++ text)
      textFragment = do
        tab <- many (oneOf " \t")
        text <- many1 (noneOf " \t\n\r")
        return $ Text (tab ++ text)
