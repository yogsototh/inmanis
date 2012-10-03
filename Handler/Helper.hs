module Handler.Helper
  ( errorPage
  , errorPageJson
  , showId
  , joinTables
  , humanReadableRelativeTime
  , testLogged
  , score
  )
where

import Prelude
import Import
import Database.Persist.Store
import Data.Maybe
import Data.Text (pack)
import qualified Data.Map as M
import Yesod.Auth

-- | test if the user is logged
testLogged ::  (UserId -> Handler RepHtmlJson) -> Handler RepHtmlJson
testLogged v = do
  maybeUserId <- maybeAuthId
  case maybeUserId of
    Nothing -> errorPageJson "You're not logged"
    Just currentUserId -> (v currentUserId)

-- Show the id number from an entry
-- Can be used inside HTML
showId :: Key backend entity -> String
showId k = showInt64 $ unKey k
  where showInt64 (PersistInt64 i) = show i
        showInt64 _ = "unknow"

errorPage :: Text -> Handler RepHtml
errorPage errorText = do
  let errorMessage = toHtml errorText
  defaultLayout $ do
    setTitle errorMessage
    $(widgetFile "error")

errorPageJson :: Text -> Handler RepHtmlJson
errorPageJson errorText = do
  defaultLayoutJson widget json
  where
    errorMessage = toHtml errorText
    widget = do
      setTitle errorMessage
      $(widgetFile "error")
    json = object ["msg" .= errorText]



--- AUTHOR pbrisbin
-- |
--
-- My solution to the N+1 problem:
--
-- > runDB $ do
-- >     posts <- selectList [] []
-- >     users <- selectList [] []
-- >
-- >     let records = joinTables postUser posts users
-- >
-- >     forM records $ \(post,user) -> do
-- >         --
-- >         -- ...
-- >         --
--
joinTables :: (a -> Key (PersistEntityBackend b) b)
           -> [Entity a]
           -> [Entity b]
           -> [(Entity a, Entity b)]
joinTables f as bs = catMaybes . for as $ \a -> fmap (\b -> (a,b)) $ lookupRelation f a bs

joinTables3 :: (a -> Key (PersistEntityBackend b) b)
            -> (a -> Key (PersistEntityBackend c) c)
            -> [Entity a]
            -> [Entity b]
            -> [Entity c]
            -> [(Entity a, Entity b, Entity c)]
joinTables3 f g as bs cs = catMaybes . for as $ \a ->
    case (lookupRelation f a bs, lookupRelation g a cs) of
        (Just b, Just c) -> Just (a,b,c)
        _                -> Nothing

lookupRelation :: (a -> Key (PersistEntityBackend b) b) -> Entity a -> [Entity b] -> Maybe (Entity b)
lookupRelation f a bs = let k  = f $ entityVal a
                            vs = M.fromList $ map (\(Entity k' v) -> (k',v)) bs
                        in fmap (\v -> Entity k v) $ M.lookup k vs

for ::  [a] -> (a -> b) -> [b]
for xs f = map f xs


humanReadableRelativeTime :: UTCTime -> UTCTime -> Text
humanReadableRelativeTime currentTime createdTime =
  -- pack $ show $ entryCreated entry
  showDuration duration
  where
  duration = diffUTCTime currentTime createdTime
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
    | t < second  = "Just now"
    | t < minute  = pack $ (showTime $ seconds t) ++ " seconds ago"
    | t < hour = pack $ (showTime $ minutes t) ++ " minutes ago"
    | t < day  = pack $ (showTime $ hours   t) ++ " hours ago"
    | t < year = pack $ (showTime $ days    t) ++ " days ago"
    | t < 2 * year = pack $ (showTime $ days    t) ++ " days ago"
                            ++ (show ((floor $ days t) `rem` 365 :: Integer)) ++ " days ago"
    | otherwise       = pack $ (showTime $ years t) ++ " years ago"

score :: Int -> Int -> UTCTime -> Double
score upvotes downvotes created =
 (log (max (abs votesdiff) 1)) + sig * (age / timeinfluence)
    where
      hour = 3600
      day = 24*hour
      year = 365*day
      timeinfluence = year
      age :: Double
      age = (realToFrac $ diffUTCTime created oldDay)
      votesdiff = fromIntegral (upvotes - downvotes)
      sig = if votesdiff>0 then 1 else if votesdiff<0 then -1 else 0
      oldDay = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 0 }
                    , utctDayTime = 0 }
