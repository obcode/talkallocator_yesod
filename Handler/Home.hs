{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost talkForm
    let handlerName = "getHomeR" :: Text
    availableTalks <- runDB $ selectList [TalkSpeaker ==. Nothing] [Asc TalkTitle]
    talksWithSpeaker <- runDB $ selectList [TalkSpeaker !=. Nothing] [Asc TalkTitle]
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "TalkAllocator!"
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, _), _) <- runFormPost talkForm
    let handlerName = "postHomeR" :: Text
    case result of
      FormSuccess talk -> do
        talkid <- runDB $ insert talk
        setMessage "Talk added"
      _ -> do
        setMessage "Talk could not be added"
    redirect HomeR

talkForm :: Form Talk
talkForm = renderBootstrap $ flip Talk Nothing
    <$> areq textField "Title" Nothing

getDelR :: Handler RepHtml
getDelR = do
    (formWidget, formEnctype) <- generateFormPost deleteForm
    let handlerName = "getHomeR" :: Text
    talks <- runDB $ selectList [] [Asc TalkTitle]
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "TalkAllocator!"
        $(widgetFile "delpage")

deleteForm :: Form Text
deleteForm = renderBootstrap $ id
    <$> areq textField "Talks" Nothing

postDelR :: Handler RepHtml
postDelR = redirect HomeR
