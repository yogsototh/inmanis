module Handler.User (
  getUserR
, postUserR
, deleteUserR
) where

import Import
import Handler.Helper
import Data.Maybe

getUserR :: Text -> Handler RepHtml
getUserR nickname = do
  maybeUser <- runDB $ getBy $ UniqueIdent $ nickname
  case maybeUser of
    Nothing -> errorPage "This user doesn't exists"
    Just (Entity _ user) -> do
      defaultLayout $ do
          setTitle $ "Inmanis"
          [whamlet|$newline always
            <h2>
              $if isNothing $ userRealName user
                #{userNickname user}
              $else
                #{fromJust $ userRealName user}
            <ul>
              $if isNothing $ userRealName user
              $else
                <li> Real Name: '#{fromJust $ userRealName user}'
              <li> Nickname: '#{userNickname user}'
              <li> UUID: '#{userIdent user}'|]

postUserR :: Text -> Handler RepHtml
postUserR _ = error "Not yet implemented: postUserR"

deleteUserR :: Text -> Handler RepHtml
deleteUserR _ = error "Not yet implemented: deleteUserR"
