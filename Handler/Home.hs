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

cssClassVoteForEntry entryId votes =
  case (filter voteForId votes) of
    [] -> "" :: Text
    (Entity voteId vote):vs -> case voteValue vote of
                                  1    -> " yeah_voted"
                                  (-1) -> " neah_voted"
                                  _    -> ""
  where
    voteForId (Entity voteId vote) = voteEntry vote == entryId


humanReadableOld :: UTCTime -> Entry -> Text
humanReadableOld currentTime entry =
  -- pack $ show $ entryCreated entry
  showDuration duration
  where
  duration = diffUTCTime currentTime (entryCreated entry)
  second, minute, hour, day, year :: NominalDiffTime
  second = fromIntegral 1
  minute = (fromIntegral 60)  * second
  hour   = (fromIntegral 60)  * minute
  day    = (fromIntegral 24)  * hour
  year   = (fromIntegral 365) * day
  seconds,minutes,hours,days,years :: NominalDiffTime -> NominalDiffTime
  seconds duration = duration / second
  minutes duration = duration / minute
  hours   duration = duration / hour
  days    duration = duration / day
  years   duration = duration / year
  showDuration duration
    | duration < minute  = "1m"
    | duration < hour = pack $ (show $ floor $ minutes duration) ++ " minutes ago"
    | duration < day  = pack $ (show $ floor $ hours   duration) ++ " hours ago"
    | duration < year = pack $ (show $ floor $ days    duration) ++ " days ago"
    | otherwise       = pack $ (show $ (floor $ years   duration)) ++ " years and " ++ (show ((floor $ days duration) `mod` 365)) ++ " days ago"




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


