{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow
import Data.Tree

--import qualified Monomer.Lens as L

type MyTree = Tree String
type TreeLoc = [Int]

newtype AppModel = AppModel {
  myTree :: MyTree
} deriving (Eq, Show)

data AppEvent = AppInitialModel | AppAddTree TreeLoc MyTree | AppRemoveTree TreeLoc
  deriving (Eq, Show)

initialTree = Node "root" [
	Node "Node 1" [],
	Node "Node 2" [
		Node "Node 2a" [],
		Node "Node 2b" [] ]
	]

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
  AppInitialModel -> []
  AppAddTree loc tree -> [ Model (model & clickCount +~ 1) ]

main :: IO ()
main = do
	startApp (AppModel 0) handleEvent buildUI [
		appWindowTitle "Hello world",
		appTheme darkTheme,
		appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
		appInitEvent AppInit
		]
