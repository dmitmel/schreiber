{-# LANGUAGE OverloadedStrings #-}

module Brick.Widgets.Tabs
    ( Tab(..)
    , Tabs(..)
    , TabsBarPosition(..)
    , selectedTabAttr
    , renderTabs
    ) where

import           Data.Maybe                 (isJust)
import           Data.Monoid                ((<>))

import           Brick.AttrMap
import           Brick.Types
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center       (center)
import           Brick.Widgets.Core

data Tab n = Tab
    { tabID      :: n
    , tabName    :: String
    , tabContent :: Widget n
    }

data TabsBarPosition = TopTabsBar | BottomTabsBar | LeftTabsBar | RightTabsBar
    deriving (Show, Eq, Ord, Enum, Bounded)

data Tabs n = Tabs
    { tabsList        :: [Tab n]
    , tabsBarPosition :: TabsBarPosition
    , tabsCurrentTab  :: n
    }

tabAttr :: AttrName
tabAttr = "tab"

selectedTabAttr :: AttrName
selectedTabAttr = tabAttr <> "selected"

renderTabs :: (Ord n, Show n) => Tabs n -> Widget n
renderTabs tabs@(Tabs tabsList barPosition currentTabID) =
    let mSlicedTabs = sliceBy (\(Tab tID _ _) -> tID == currentTabID) tabsList
        mCurrentTab = (\(_, currentTab, _) -> currentTab) <$> mSlicedTabs
        widgetSize  = if isJust mCurrentTab then Fixed else Greedy
    in Widget widgetSize widgetSize $ do
        borderStyle <- ctxBorderStyle <$> getContext
        render $ case barPosition of
            TopTabsBar ->
                vBox [hHeader tabs, hBorderUnderTabs borderStyle tabs mSlicedTabs, renderTabContent mCurrentTab]
            BottomTabsBar ->
                vBox [renderTabContent mCurrentTab, hBorderUnderTabs borderStyle tabs mSlicedTabs, hHeader tabs]
            LeftTabsBar ->
                hBox [vHeader tabs, vBorderUnderTabs borderStyle tabs mSlicedTabs, renderTabContent mCurrentTab]
            RightTabsBar ->
                hBox [renderTabContent mCurrentTab, vBorderUnderTabs borderStyle tabs mSlicedTabs, vHeader tabs]

sliceBy :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
sliceBy _ [] = Nothing
sliceBy f (x:xs)
    | f x       = Just ([], x, xs)
    | otherwise = (\(before, y, after) -> (x:before, y, after)) <$> sliceBy f xs

hHeader :: Eq n => Tabs n -> Widget n
hHeader (Tabs [] _ _) = str " "
hHeader (Tabs tabsList _ currentTabID) =
    vLimit 1 $ hBox $ map (\tab -> renderTab tab <+> vBorder) tabsList
  where
    renderTab (Tab tID name _)
        | tID == currentTabID = clickable tID $ withAttr selectedTabAttr $ str name
        | otherwise           = clickable tID $ withAttr tabAttr $ str name

vHeader :: Eq n => Tabs n -> Widget n
vHeader (Tabs [] _ _) = str " "
vHeader (Tabs tabsList _ currentTabID) =
    hLimit 1 $ vBox $ map (\tab -> renderTab tab <=> hBorder) tabsList
  where
    renderTab (Tab tID name _)
        | tID == currentTabID = clickable tID $ withAttr selectedTabAttr $ vStr name
        | otherwise           = clickable tID $ withAttr tabAttr $ vStr name

    vStr = vBox . map (\char -> str [char])

renderTabContent :: (Show n, Ord n) => Maybe (Tab n) -> Widget n
renderTabContent (Just (Tab _ _ tContent)) = tContent
renderTabContent Nothing                   = center $ str "No tabs are opened"

hBorderUnderTabs :: BorderStyle -> Tabs n -> Maybe ([Tab n], Tab n, [Tab n]) -> Widget n
hBorderUnderTabs borderStyle (Tabs tabsList barPosition _) Nothing =
    hBox (map (\tab -> hBorderUnderTab tab <+> separator) tabsList) <+> hBorder
  where
    separator = case barPosition of
        TopTabsBar    -> str [bsIntersectB borderStyle]
        BottomTabsBar -> str [bsIntersectT borderStyle]
        _             -> error "Brick.Widgets.Tabs.hBorderUnderTabs: expected horizontal bar position"
hBorderUnderTabs borderStyle (Tabs _ barPosition _) (Just (leftTabs, Tab _ currentName _, rightTabs))
    | null leftTabs =
        translateByX (textWidth currentName) rightSeparator <+>
        rightBorder <+> hBorder
    | otherwise =
        leftBorder <+> leftSeparator <+>
        translateByX (textWidth currentName) rightSeparator <+>
        rightBorder <+> hBorder
  where
    separator = case barPosition of
        TopTabsBar    -> str [bsIntersectB borderStyle]
        BottomTabsBar -> str [bsIntersectT borderStyle]
        _             -> error "Brick.Widgets.Tabs.hBorderUnderTabs: expected horizontal bar position"
    leftSeparator = case barPosition of
        TopTabsBar    -> str [bsCornerBR borderStyle]
        BottomTabsBar -> str [bsCornerTR borderStyle]
        _             -> error "Brick.Widgets.Tabs.hBorderUnderTabs: expected horizontal bar position"
    rightSeparator = case barPosition of
        TopTabsBar    -> str [bsCornerBL borderStyle]
        BottomTabsBar -> str [bsCornerTL borderStyle]
        _             -> error "Brick.Widgets.Tabs.hBorderUnderTabs: expected horizontal bar position"
    leftBorder     = foldl1 (\leftTab rightTab -> leftTab <+> separator <+> rightTab) $ map hBorderUnderTab leftTabs
    rightBorder    = hBox $ map (\tab -> hBorderUnderTab tab <+> separator) rightTabs

hBorderUnderTab :: Tab n -> Widget n
hBorderUnderTab (Tab _ name _) = hLimit (textWidth name) hBorder

vBorderUnderTabs :: BorderStyle -> Tabs n -> Maybe ([Tab n], Tab n, [Tab n]) -> Widget n
vBorderUnderTabs borderStyle (Tabs tabsList barPosition _) Nothing =
    vBox (map (\tab -> vBorderUnderTab tab <=> separator) tabsList) <=> vBorder
  where
    separator = case barPosition of
        LeftTabsBar  -> str [bsIntersectR borderStyle]
        RightTabsBar -> str [bsIntersectL borderStyle]
        _             -> error "Brick.Widgets.Tabs.hBorderUnderTabs: expected vertical bar position"
vBorderUnderTabs borderStyle (Tabs _ barPosition _) (Just (leftTabs, Tab _ currentName _, rightTabs))
    | null leftTabs =
        translateByY (textWidth currentName) rightSeparator <=>
        rightBorder <=> vBorder
    | otherwise =
        leftBorder <=> leftSeparator <=>
        translateByY (textWidth currentName) rightSeparator <=>
        rightBorder <=> vBorder
  where
    separator = case barPosition of
        LeftTabsBar  -> str [bsIntersectR borderStyle]
        RightTabsBar -> str [bsIntersectL borderStyle]
        _             -> error "Brick.Widgets.Tabs.hBorderUnderTabs: expected vertical bar position"
    leftSeparator = case barPosition of
        LeftTabsBar  -> str [bsCornerBR borderStyle]
        RightTabsBar -> str [bsCornerBL borderStyle]
        _             -> error "Brick.Widgets.Tabs.hBorderUnderTabs: expected vertical bar position"
    rightSeparator = case barPosition of
        LeftTabsBar  -> str [bsCornerTR borderStyle]
        RightTabsBar -> str [bsCornerTL borderStyle]
        _             -> error "Brick.Widgets.Tabs.hBorderUnderTabs: expected vertical bar position"
    leftBorder     = foldl1 (\leftTab rightTab -> leftTab <=> separator <=> rightTab) $ map vBorderUnderTab leftTabs
    rightBorder    = vBox $ map (\tab -> vBorderUnderTab tab <=> separator) rightTabs

vBorderUnderTab :: Tab n -> Widget n
vBorderUnderTab (Tab _ name _) = vLimit (textWidth name) vBorder

translateByX :: Int -> Widget n -> Widget n
translateByX x = translateBy (Location (x, 0))

translateByY :: Int -> Widget n -> Widget n
translateByY y = translateBy (Location (0, y))
