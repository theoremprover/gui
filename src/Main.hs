{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Monomer
import TextShow

--import qualified Monomer.Lens as L

newtype AppModel = AppModel {
  _clickCount :: Int
} deriving (Eq, Show)
initialModel = AppModel 1

--data AppEvent = AppInitialModel | AppIncrease
data AppEvent = AppIncrease
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
--  AppInitialModel -> []
  AppIncrease -> [ Model (model & clickCount +~ 1) ]

main :: IO ()
main = do
	startApp initialModel handleEvent buildUI [
		appWindowTitle "Hello world",
		appTheme darkTheme,
		appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
--		appInitEvent AppInitialModel
		]
