{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Paths_follower (version)

import Web.Twitter
import Web.Twitter.Monad
import Web.Twitter.Types
import Web.Twitter.Fetch
import Data.Time
import Data.Time.Format
import Data.Version (showVersion)
import Control.Monad
import System.Locale
import System.Directory
import System.Console.CmdArgs
import qualified System.IO.Strict as S
import qualified System.FilePath as Fp
import Text.PrettyPrint.ANSI.Leijen hiding (list)

data Argument = Argument {follow :: Maybe String,
                          no_follow :: Maybe String,
                          list :: Bool}
              deriving (Show, Data, Typeable)

type State = [(UserName, StatusId)]

instance Eq Status where
  x == y = (statusId x) == (statusId y)

instance Ord Status where
  compare x y = compare xt yt
    where
      xt :: UTCTime
      xt = parseStatusDate $ statusCreated x
      yt :: UTCTime
      yt = parseStatusDate $ statusCreated y

parseStatusDate :: (ParseTime a) => DateString -> a
parseStatusDate dstr = case (parseTime defaultTimeLocale (dateTimeFmt defaultTimeLocale) dstr) of
  Just t -> t
  Nothing -> error "Cannot parse time!"

getTweetsBy :: (UserName, StatusId) -> TM [Status]
getTweetsBy (name,since) = getUserTimeline (Just name) Nothing (Just since)

formatTweets :: [Status] -> Doc
formatTweets = (<$> empty) . vsep . map formatTweet

shortenTime :: DateString -> String
shortenTime dstr = formatTime defaultTimeLocale "%F %R %Z" tweetTime
  where
    tweetTime :: UTCTime
    tweetTime = parseStatusDate dstr

formatTweet :: Status -> Doc
formatTweet tweet = (bold $ langle <>
                     (blue $ text username) <>
                     rangle) <+>
                    (align $ (foldr (</>) empty) $ (map text) $ words tweettext) <$>
                    (text "--") <+> lparen <> (red $ text tweetdate) <> rparen
                     
  where
    username = userScreenName (statusUser tweet)
    tweetdate = shortenTime $ statusCreated tweet
    tweettext = statusText tweet

notReply :: Status -> Bool
notReply tweet = case (statusInReplyTo tweet) of
  Just _ -> False
  Nothing -> True

merge :: (Ord a) => [[a]] -> [a]
merge = foldr merge' []
    where
      merge' [] [] = []
      merge' [] (x:xs) = x:xs
      merge' (x:xs) [] = x:xs
      merge' (x:xs) (y:ys) = case (compare x y) of
        LT -> y : (merge' (x:xs) ys)
        EQ -> y : x : (merge' xs ys)
        GT -> x : (merge' xs (y:ys))

followAll :: State -> IO State
followAll us = do results <- mapM (runTM nullAuthUser . getTweetsBy) us
                  putDoc $ formatTweets $ filter notReply $ merge results
                  return $ updateState us results
  where
    updateState :: State -> [[Status]] -> State
    updateState [] [] = []
    updateState (p:ps) (t:ts) = case t of
      [] -> p : (updateState ps ts)
      (x:xs) -> ((fst p), (statusId x)) : (updateState ps ts)
    updateState _ _ = error "This should not happen"

stalk :: UserName -> IO (Maybe StatusId)
stalk un = do tweets <- runTM nullAuthUser $ getUserTimeline (Just un) Nothing Nothing
              case tweets of
                [] -> return Nothing
                (x:xs) -> do putDoc $ formatTweets tweets
                             return $ Just $ statusId x

addFollow :: State -> UserName -> IO State
addFollow curr un = do result <- stalk un
                       return $ case result of
                         Just state -> (un, state) : curr
                         Nothing -> curr

rmFollow :: State -> UserName -> IO State
rmFollow curr un = return $ filter ((/= un) . fst) curr

saveState :: (Show a) => a -> IO ()
saveState st = do confDir <- getAppUserDataDirectory appName
                  createDirectoryIfMissing True confDir
                  writeFile (confDir Fp.</> stateFileName) (show st)

loadState :: (Read a) => IO (Maybe a)
loadState = do confDir <- getAppUserDataDirectory appName
               let confFile = confDir Fp.</> stateFileName
               exists <- doesFileExist confFile
               case exists of
                 True ->  do contents <- S.readFile confFile
                             return $ Just $ read contents
                 False -> return Nothing

stateFileName = "state"
appName = "follower"

arguments = Argument { follow = def &= typ "SCREEN_NAME" &= help "Follow the user with the given screen name",
                       no_follow = def &= typ "SCREEN_NAME" &= help "Stop following the user with the given screen name",
                       list = def &= help "Display the current follow list"
                     } &= program appName &= summary (appName ++ " " ++ (showVersion version) ++ " " ++ "Copyright R. Emre Başar 2011 - ...")

argumentsGiven :: Argument -> Bool
argumentsGiven args = (follow args /= Nothing) || (no_follow args /= Nothing) || (list args)

applyArgs :: Argument -> State -> IO State
applyArgs args cs = if (argumentsGiven args) then
                      processArguments args cs
                    else
                      followAll cs

listFollowed state = putDoc $ (vsep $ map (text . fst) state) <$> empty

processArguments :: Argument -> State -> IO State
processArguments args state = do newState <- case (follow args) of
                                                  Just name -> addFollow state name
                                                  Nothing -> return state
                                 newState <- case (no_follow args) of
                                                  Just name -> rmFollow newState name
                                                  Nothing -> return newState
                                 when (list args) $ listFollowed newState
                                 return newState

main :: IO ()
main = do args <- cmdArgs arguments
          currentState <- loadState
          case currentState of
            Just st -> do latestState <- applyArgs args st
                          saveState latestState
            Nothing -> putStrLn "You are not following anyone. Try adding some people with --follow"          
