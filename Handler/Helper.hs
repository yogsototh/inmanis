module Handler.Helper 
  ( errorPage
  , errorPageJson
  , showId )
where

import Import
import Database.Persist.Store

-- Show the id number from an entry
-- Can be used inside HTML
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
