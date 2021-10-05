{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Text (Text)
import Monomer
import TextShow

data AppModel = AppModel {
	_clickCount :: Int
	} deriving (Eq, Show)

data AppEvent = AppInit | AppIncrease
	deriving (Eq, Show)

makeLenses 'AppModel

buildUI :: WidgetEnv AppModel AppEvent -> AppModel -> WidgetNode AppModel AppEvent
buildUI wenv model = vstack [
	label "Hello world",
	spacer,
	hstack [
		label $ "Click count: " <> showt (model ^. clickCount),
		spacer,
		button "Increase count" AppIncrease
		]
	] `styleBasic` [padding 10]

handleEvent :: WidgetEnv AppModel AppEvent -> WidgetNode AppModel AppEvent -> AppModel -> AppEvent -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
	AppInit -> []
	AppIncrease -> [ Model (model & clickCount +~ 1) ]

main :: IO ()
main = do
	startApp (AppModel 0) handleEvent buildUI [
		appWindowTitle "Hello World",
		appTheme darkTheme,
		appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
		appInitEvent AppInit ]