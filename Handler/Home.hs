{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home
  ( getHomeR
  , postHomeR
  )
where

import Import
import Handler.Helper
import Yesod.Auth
import Data.Maybe
import Data.Text (pack)

data EntryRequest = EntryRequest {
                      title :: Text
                    , url   :: Text
                     }
entryForm :: Form EntryRequest
entryForm = renderDivs  $ EntryRequest
  <$> areq textField "Title" Nothing
  <*> areq urlField  "Url"   Nothing

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

cssClassVoteForEntry :: Key backend (EntryGeneric backend)
                        -> [Entity (VoteGeneric backend)]
                        -> Text
cssClassVoteForEntry entryId votes =
  case (filter voteForId votes) of
    [] -> "" :: Text
    (Entity _ vote):_ -> case voteValue vote of
                                  1    -> " yeah_voted"
                                  (-1) -> " neah_voted"
                                  _    -> ""
  where
    voteForId (Entity _ vote) = voteEntry vote == entryId


humanReadableOld :: UTCTime -> Entry -> Text
humanReadableOld currentTime entry =
  -- pack $ show $ entryCreated entry
  showDuration duration
  where
  duration = diffUTCTime currentTime (entryCreated entry)
  second, minute, hour, day, year :: NominalDiffTime
  second = fromIntegral (1 :: Int)
  minute = (fromIntegral ( 60 :: Int))  * second
  hour   = (fromIntegral ( 60 :: Int))  * minute
  day    = (fromIntegral ( 24 :: Int))  * hour
  year   = (fromIntegral (365 :: Int)) * day
  seconds,minutes,hours,days,years :: NominalDiffTime -> NominalDiffTime
  seconds t = t / second
  minutes t = t / minute
  hours   t = t / hour
  days    t = t / day
  years   t = t / year
  showTime t = show (floor t :: Integer)
  showDuration t
    | t < minute  = "1m"
    | t < hour = pack $ (showTime $ minutes t) ++ " minutes ago"
    | t < day  = pack $ (showTime $ hours   t) ++ " hours ago"
    | t < year = pack $ (showTime $ days    t) ++ " days ago"
    | otherwise       = pack $ (showTime $ years t) ++ " years and " 
                            ++ (show ((floor $ days t) `mod` 365 :: Integer)) ++ " days ago"


-- the name getHomeR is for
-- handle the request GET on the resource HomeR
getHomeR :: Handler RepHtml
getHomeR = do
  -- We get the current user id (return Nothing if not logged in)
  currentUserId <- maybeAuthId
  currentTime <- liftIO getCurrentTime

  -- Generate a couple
  -- widget => some HTML containing the form labels and input fields
  -- enctype => the encoding
  (widget,enctype) <- generateFormPost entryForm

  -- We get the list of entries sorted by entry title
  (entries,votes) <- runDB $ do
    entries <- selectList [] [Desc EntryCreated, LimitTo 25]
    allVotesOfCurrentUser   <- case currentUserId of
                  Nothing   -> return []
                  Just user -> selectList [VoteCreator ==. user][]
    return ( entries
           , map fst $ joinTables voteEntry allVotesOfCurrentUser entries)

  -- We return some HTML (not full)
  defaultLayout $ do
    aDomId <- lift newIdent
    setTitle "Inmanis"
    $(widgetFile "homepage")


-- When we receive a post request on HomeR resource (/ path)
-- We create a new resource
postHomeR :: Handler RepHtml
postHomeR = do
  userId <- maybeAuthId
  case userId of
    Nothing -> errorPage "You're not logged"
    Just currentUserId -> do -- errorPage "You're logged"
      ((res,_),_) <- runFormPost entryForm
      case res of
        FormSuccess personRequest -> do
          time <- liftIO getCurrentTime
          let newEntry = Entry currentUserId (title personRequest) (url personRequest) 0 0 0 time
          entryId <- runDB $ insert newEntry
          setMessage $ toHtml (title personRequest)
          redirect $ EntryR entryId
        _ -> errorPage "Please correct your entry form"


