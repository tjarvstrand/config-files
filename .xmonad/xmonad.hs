import XMonad
import qualified XMonad.StackSet as W

import XMonad.Layout.Gaps
import XMonad.Config.Gnome
import XMonad.Util.EZConfig
import XMonad.ManageHook
import XMonad.Actions.CycleWS
import XMonad.Actions.Volume
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders

import XMonad.Actions.CycleWindows

myLayout = tiled ||| Mirror tiled ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
      tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
      nmaster = 1

    -- Default proportion of screen occupied by master pane
      ratio   = 2/3

    -- Percent of screen to increment by when resizing panes
      delta   = 3/100

myManageHook :: [ManageHook]
myManageHook =
  [ resource =? "Do" --> doIgnore
  , isFullscreen --> doFullFloat
  , className =? "Unity-2d-panel" --> doIgnore
  , className =? "Unity-2d-launcher" --> doFloat ]

main = xmonad $ gnomeConfig
  { modMask = mod4Mask
  , terminal = "urxvt"
  , borderWidth = 2
  , manageHook = manageHook gnomeConfig <+> composeAll myManageHook
  , logHook = ewmhDesktopsLogHook >> setWMName "LG3D" -- java workaround
  , layoutHook = gaps [(U, 24)] myLayout
  }
  `additionalKeysP`
    [ ("M1-<Tab>",  windows W.focusDown)
    , ("M1-<Space>", spawn "gnome-do")
    , ("M1-<Up>", raiseVolume 4 >> return ())
    , ("M1-<Down>", lowerVolume 4 >> return ())
    ]