
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Directory
  ( listDirectory
  , doesDirectoryExist
  , doesFileExist
  , removeDirectoryRecursive
    )
import Text.Parsec (ParsecT)
import qualified Text.Parsec as P
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import Data.Either (rights)
import System.Process (system)
import System.Exit (ExitCode (..))
import Control.Applicative ((<|>))
import Control.Monad (unless, void, forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Set as S
import qualified Data.Map as M

word_parser :: Monad m => ParsecT Text u m Word
word_parser = read <$> P.many1 P.digit

-- LTS 6 0 = lts-6.0
data LTSSnapshot = LTS Word Word deriving (Eq, Ord)

instance Show LTSSnapshot where
  show (LTS major minor) = "lts-" ++ show major ++ "." ++ show minor

lts_parser :: Monad m => ParsecT Text u m LTSSnapshot
lts_parser = do
  _ <- P.string "lts-"
  major <- word_parser
  _ <- P.char '.'
  minor <- word_parser
  pure $ LTS major minor

ltsSelect :: [LTSSnapshot] -> [LTSSnapshot]
ltsSelect = fmap (uncurry LTS)
          . M.toList
          . foldr (\(LTS major minor) -> M.insertWith max major minor) M.empty

-- Nightly 2016 5 29 = nightly-2016-05-29
data NightlySnapshot = Nightly Word Word Word deriving (Eq, Ord)

instance Show NightlySnapshot where
  show (Nightly y m d) = "nightly-" ++ show y ++ "-" ++ show m ++ "-" ++ show d

nightly_parser :: Monad m => ParsecT Text u m NightlySnapshot
nightly_parser = do
  _ <- P.string "nightly-"
  year <- word_parser
  _ <- P.char '-'
  month <- word_parser
  _ <- P.char '-'
  day <- word_parser
  pure $ Nightly year month day

getAllSnapshotsWith :: FilePath -> P.Parsec Text () a -> IO [a]
getAllSnapshotsWith fp parser = do
  b <- doesDirectoryExist fp
  unless b $ void $ system $ "git clone --depth=1 git@github.com:fpco/" ++ fp ++ ".git"
  fs <- listDirectory fp
  removeDirectoryRecursive fp
  pure $ rights $ fmap (P.parse parser fp . pack) fs

getAllLTS :: IO [LTSSnapshot]
getAllLTS = getAllSnapshotsWith "lts-haskell" lts_parser

getAllNightlys :: IO [NightlySnapshot]
getAllNightlys = getAllSnapshotsWith "stackage-nightly" nightly_parser

type StackageBuilder = StateT ([LTSSnapshot],[NightlySnapshot]) IO

runStackageBuilder :: StackageBuilder a -> IO a
runStackageBuilder builder = evalStateT builder ([],[])

askAllLTS :: StackageBuilder [LTSSnapshot]
askAllLTS = do
  (ltslist,nightlylist) <- get
  case ltslist of
    [] -> do lputStrLn "* Downloading LTS information..."
             alllts <- liftIO getAllLTS
             put (alllts,nightlylist)
             pure alllts
    _ -> pure ltslist

askAllNightlys :: StackageBuilder [NightlySnapshot]
askAllNightlys = do
  (ltslist,nightlylist) <- get
  case nightlylist of
    [] -> do lputStrLn "* Downloading Nightly information..."
             allnightlys <- liftIO getAllNightlys
             put (ltslist,allnightlys)
             pure allnightlys
    _ -> pure nightlylist

latestLTS :: StackageBuilder LTSSnapshot
latestLTS = maximum <$> askAllLTS

latestNightly :: StackageBuilder NightlySnapshot
latestNightly = maximum <$> askAllNightlys

ltsRange :: LTSSnapshot -> LTSSnapshot -> StackageBuilder [LTSSnapshot]
ltsRange lts1 lts2 =
  filter (\lts -> lts1 <= lts && lts <= lts2) <$> askAllLTS

nightlyRange :: NightlySnapshot -> NightlySnapshot -> StackageBuilder [NightlySnapshot]
nightlyRange nightly1 nightly2 =
  filter (\nightly -> nightly1 <= nightly && nightly <= nightly2) <$> askAllNightlys

data StackageSnapshot =
    LTSSnapshot     LTSSnapshot
  | NightlySnapshot NightlySnapshot
    deriving (Eq, Ord)

instance Show StackageSnapshot where
  show (LTSSnapshot lts) = show lts
  show (NightlySnapshot nightly) = show nightly

defaultFile :: StackageSnapshot -> String
defaultFile snapshot = unlines
  [ "resolver: " ++ show snapshot
  , "packages: [\".\"]"
    ]

writeDefaultFile :: StackageSnapshot -> IO ()
writeDefaultFile = writeFile "stack.yaml" . defaultFile

buildWith :: StackageSnapshot -> IO ExitCode
buildWith snapshot = system $
     "stack build --install-ghc --resolver " ++ show snapshot
  ++ " --ghc-options -fforce-recomp"
type SnapshotSet = S.Set StackageSnapshot

type Parser = ParsecT Text () StackageBuilder

parseLTS :: Parser LTSSnapshot
parseLTS = P.try (P.string "latest" >> lift latestLTS)
       <|> lts_parser

parseNightly :: Parser NightlySnapshot
parseNightly = P.try (P.string "latest" >> lift latestNightly)
           <|> nightly_parser

spaced :: Parser a -> Parser a
spaced p = P.skipMany (P.char ' ') *> p <* P.skipMany (P.char ' ')

parseRange :: Parser a -> Parser b -> Parser (a,b)
parseRange p1 p2 = do
  x1 <- p1
  _ <- spaced $ P.char '~'
  x2 <- p2
  pure (x1,x2)

line_parser :: (a -> StackageSnapshot)
            -> (a -> a -> StackageBuilder [a])
            -> ([a] -> [a])
            -> Parser a
            -> String
            -> Parser SnapshotSet
line_parser f g h p str = do
  _ <- P.string str
  c <- P.anyChar
  case c of
    ':' -> fmap (S.singleton . f) $ spaced p
    '-' -> do _ <- P.string "range"
              c' <- P.anyChar
              case c' of
                ':' -> fmap (S.fromList . fmap f) $ spaced (parseRange p p) >>= lift . uncurry g
                '-' -> do _ <- P.string "selected:"
                          fmap (S.fromList . fmap f . h) $ spaced (parseRange p p) >>= lift . uncurry g
                _ -> P.unexpected [c']
    _ -> P.unexpected [c]

snapshotset_parser :: Parser SnapshotSet
snapshotset_parser = fmap S.unions $ P.many $ P.choice
  [ line_parser     LTSSnapshot     ltsRange ltsSelect parseLTS     "lts"
      <* P.many (P.char ' ') <* P.optional P.endOfLine
  , line_parser NightlySnapshot nightlyRange id        parseNightly "nightly"
      <* P.many (P.char ' ') <* P.optional P.endOfLine
    ]

lputStrLn :: MonadIO m => String -> m ()
lputStrLn = liftIO . putStrLn

main :: IO ()
main = runStackageBuilder $ do
  lputStrLn "* stackage-multibuild"
  lputStrLn "* Printing stack version..."
  _ <- liftIO $ system "stack --version"
  lputStrLn "* Parsing snapshot set..."
  let fp = "stackage-multibuild.yaml"
  t <- liftIO $ T.readFile fp
  eset <- P.runParserT snapshotset_parser () fp t
  case eset of
    Left err -> liftIO $ print err
    Right set -> forM_ set $ \snapshot -> do
      lputStrLn $ "* Using snapshot: " ++ show snapshot
      b <- liftIO $ doesFileExist "stack.yaml"
      unless b $ do
        lputStrLn "* No stack.yaml file found, writing default file."
        latestLTS >>= liftIO . writeDefaultFile . LTSSnapshot
      code <- liftIO $ buildWith snapshot
      case code of
        ExitSuccess -> pure ()
        _ -> fail $ "Snapshot " ++ show snapshot ++ " failed to build"
