{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Monomer
import TextShow

--import qualified Monomer.Lens as L

import Data.Tree
import Data.Tree.Lens

newtype AppModel = AppModel {
	_clickCount :: Int
} deriving (Eq, Show)

initialModel = AppModel 0

data AppEvent = AppInit | AppIncrease
	deriving (Eq, Show)

makeLenses 'AppModel

{-
newtype AppModel = AppModel {
	_myTree :: Tree String
} deriving (Eq, Show)
initialTree :: Tree String
initialTree = Node "root" [
	Node "node 1" [Node "node 1a" [], Node "node 1b" []],
	Node "node 2" []
	]
initialModel = AppModel initialTree

data TreeCfg = TreeCfg {
	fontSize :: Int
	} deriving (Eq,Show)
instance Default TreeCfg where
	def = TreeCfg {
		fontSize = 10
		}

data (Eq a,Show a) => TreeState a = TreeState {
	_treeS :: Tree a
	} deriving (Eq,Show)

makeLenses ''TreeState


tree :: WidgetNode s e
tree = tree_ def

tree_ :: [CanvasCfg] -> Tree a -> WidgetNode (Tree a) e
tree_ configs initialtree = defaultWidgetNode "tree" newWidget where
	config = mconcat configs
	newWidget = makeTree config initialtree

makeTree :: CanvasCfg -> TreeState -> Widget s e
makeTree cfg state = widget where
	widget = createSingle state def {
	...
	}

--data AppEvent = AppInitialModel | AppIncrease
data AppEvent = AppIncrease
	deriving (Eq, Show)
-}

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
  AppInit     -> []
  AppIncrease -> [ Model (model & clickCount +~ 1) ]

main :: IO ()
main = do
	startApp initialModel handleEvent buildUI [
		appWindowTitle "Hello world",
		appTheme darkTheme,
		appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
		appInitEvent AppInit
		]
