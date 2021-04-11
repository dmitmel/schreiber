import           Control.Monad        (void)

import           Brick.AttrMap
import qualified Brick.Main           as Brick
import           Brick.Types
import           Brick.Widgets.Border
import           Brick.Widgets.Core
import qualified Graphics.Vty         as Vty

import           Brick.Widgets.Tabs

data State n = State
    { stateCenterTabs :: Tabs n
    , stateBottomTabs :: Tabs n
    , stateLeftTabs   :: Tabs n
    , stateRightTabs  :: Tabs n
    }

main :: IO ()
main = do
    config <- Vty.standardIOConfig
    vty <- Vty.mkVty config
    Vty.setMode (Vty.outputIface vty) Vty.Mouse True
    void $ Brick.customMain (return vty) Nothing app createState

createState :: State Int
createState = State
    { stateCenterTabs = Tabs
        { tabsList        = [createTab "hello.bf" 0, createTab "world.bfng" 1]
        , tabsBarPosition = TopTabsBar
        , tabsCurrentTab  = -1
        }
    , stateBottomTabs = Tabs
        { tabsList        = [createTab "Commands" 2, createTab "Output" 3, createTab "Debug" 4]
        , tabsBarPosition = BottomTabsBar
        , tabsCurrentTab  = -1
        }
    , stateLeftTabs = Tabs
        { tabsList        = [createTab "Project" 5, createTab "Structure" 6, createTab "Warnings" 7]
        , tabsBarPosition = LeftTabsBar
        , tabsCurrentTab  = -1
        }
    , stateRightTabs = Tabs
        { tabsList        = [createTab "Documentation" 8, createTab "Build" 9, createTab "Find & Replace" 10]
        , tabsBarPosition = RightTabsBar
        , tabsCurrentTab  = -1
        }
    }

createTab :: String -> n -> Tab n
createTab name tID =
    Tab
    { tabID = tID
    , tabName = name
    , tabContent = vBox $ map (str . ("Line " ++) . show) [0..200 :: Int]
    }

app :: Brick.App (State Int) e Int
app =
    Brick.App
    { Brick.appDraw = drawUI
    , Brick.appStartEvent = return
    , Brick.appHandleEvent = handleEvent
    , Brick.appAttrMap = const $ attrMap Vty.defAttr [(selectedTabAttr, Vty.defAttr `Vty.withStyle` Vty.bold)]
    , Brick.appChooseCursor = Brick.neverShowCursor
    }

drawUI :: State Int -> [Widget Int]
drawUI (State centerTabs bottomTabs leftTabs rightTabs) = [border $ hBox [renderTabs leftTabs, vBorder, vBox [renderTabs centerTabs, hBorder, renderTabs bottomTabs], vBorder, renderTabs rightTabs]]

handleEvent :: State Int -> BrickEvent Int e -> EventM Int (Next (State Int))
handleEvent state event =
    case event of
        -- MouseDown newInt _ _ _ -> Brick.continue $ state {tabsBarCurrentTab = newInt}
        -- VtyEvent (Vty.EvKey Vty.KUp _) -> do
        --     Brick.viewportScroll currentTab `Brick.vScrollBy` (-1)
        --     Brick.continue state
        -- VtyEvent (Vty.EvKey Vty.KDown _) -> do
        --     Brick.viewportScroll currentTab `Brick.vScrollBy` 1
        --     Brick.continue state
        VtyEvent (Vty.EvKey Vty.KRight _) -> Brick.continue $ state
            { stateCenterTabs = changeTabBy (stateCenterTabs state) 1
            , stateBottomTabs = changeTabBy (stateBottomTabs state) 1
            , stateLeftTabs = changeTabBy (stateLeftTabs state) 1
            , stateRightTabs = changeTabBy (stateRightTabs state) 1
            }
        VtyEvent (Vty.EvKey Vty.KLeft _) -> Brick.continue $ state
            { stateCenterTabs = changeTabBy (stateCenterTabs state) (-1)
            , stateBottomTabs = changeTabBy (stateBottomTabs state) (-1)
            , stateLeftTabs = changeTabBy (stateLeftTabs state) (-1)
            , stateRightTabs = changeTabBy (stateRightTabs state) (-1)
            }
        VtyEvent (Vty.EvKey Vty.KEsc _) -> Brick.halt state
        _                               -> Brick.continue state
  where
    changeTabBy tabs scroll = tabs { tabsCurrentTab = (tabsCurrentTab tabs + scroll) `mod` length (tabsList tabs) }
