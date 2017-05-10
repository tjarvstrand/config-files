import XMonad
import qualified XMonad.StackSet as W

import XMonad.Layout.Gaps
import XMonad.Config.Xfce
import XMonad.Util.EZConfig
import XMonad.ManageHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWindows

myLayout = smartBorders tiled |||
           -- Mirror tiled |||
           noBorders Full
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
  -- , isFullscreen --> doFullFloat
  -- , isDialog --> doFloat
  , className =? "vlc" --> doFullFloat
  , className =? "Xfce4-panel" --> doIgnore
  , className =? "Xfce4-notifyd" --> doIgnore
  , manageDocks]

myLogHook :: [X ()]
myLogHook =
 [ ewmhDesktopsLogHook >> setWMName "LG3D"  -- java workaround
 ]

main = xmonad $ xfceConfig
  { modMask = mod4Mask
  , terminal = "urxvt"
  , borderWidth = 2
  , manageHook = manageHook xfceConfig <+> composeAll myManageHook
  , handleEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook
  , logHook = composeAll myLogHook
  , layoutHook = lessBorders OnlyFloat $ avoidStruts $ myLayout
  , startupHook = do spawn "/bin/bash ~/.xmonadrc"
  }
  `additionalKeysP`
    [ ("M1-<Tab>",  windows W.focusDown)
    , ("M-w",  kill)
    , ("C-<Return>", spawn "dmenu_run_history -i -l 5")
    ]