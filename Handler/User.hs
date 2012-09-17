module Handler.User (
  getUserR
, postUserR
, deleteUserR
) where

import Import
import Handler.Helper

getUserR :: UserId -> Handler RepHtml
getUserR userId = do
  maybeUser <- runDB $ get userId
  case maybeUser of
    Nothing -> errorPage "This user doesn't exists"
    Just user -> do
      defaultLayout $ do
          setTitle $ "Inmanis"
          [whamlet|$newline always
            <h2> #{userIdent user}|]

postUserR :: UserId -> Handler RepHtml
postUserR _ = error "Not yet implemented: postUserR"

deleteUserR :: UserId -> Handler RepHtml
deleteUserR _ = error "Not yet implemented: deleteUserR"
