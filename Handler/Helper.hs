module Handler.Helper 
  ( errorPage
  , errorPageJson )
where

import Import

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
