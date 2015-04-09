import XMonad
import XMonad.Actions.UpdatePointer
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Script
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import Data.Ratio ((%))
import System.IO

import qualified XMonad.StackSet as W

myLayout = avoidStruts (Tall 1 (3/100) (34/55)) ||| -- approx of golden ratio ;-)
    noBorders (fullscreenFull Full)
    -- spiral (6/7)

-- use 'xprop' to get the class information from a window
myManageHook = composeAll
    [ className =? "Gimp"           --> doFloat
    , className =? "Gimp-2.6"       --> doFloat
    , className =? "Msgcompose"     --> doCenterFloat
    , className =? "Ekiga"          --> doFloat
    , className =? "Skype"          --> doFloat
    , title     =? "Volume Control" --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    ]

myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]

main = xmonad =<< xmobar myConfig
 
myConfig = gnomeConfig {
    manageHook    = manageDocks <+> myManageHook -- make sure to include myManageHook definition from above
                                <+> manageHook defaultConfig
    , borderWidth = 1
    , layoutHook  = smartBorders $ myLayout
    , startupHook = execScriptHook "startup"
    , logHook     = updatePointer (Relative 0.5 0.5)
    , modMask     = mod4Mask     -- Rebind Mod to the Windows key
    , terminal    = "terminator"
    } `additionalKeys`
    [ ((mod4Mask, xK_a), spawn "pavucontrol")
    , ((mod4Mask, xK_p), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((mod4Mask, xK_v), spawn "gvim")
    , ((mod4Mask, xK_f), spawn "pcmanfm")
    , ((mod4Mask, xK_c), spawn "google-chrome")
    , ((mod4Mask, xK_z), spawn "xscreensaver-command -lock")
    , ((mod1Mask, xK_Tab), windows W.focusDown)
    ]
