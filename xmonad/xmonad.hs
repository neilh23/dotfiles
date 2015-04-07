import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Script
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

import qualified XMonad.StackSet as W
 
myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Gimp-2.6"      --> doFloat
    , className =? "Ekiga"      --> doFloat
    , title =? "ALSA Mixer"	--> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    ]

main = xmonad =<< xmobar myConfig
 
myConfig = gnomeConfig {
    manageHook = manageDocks <+> myManageHook -- make sure to include myManageHook definition from above
                             <+> manageHook defaultConfig
    , layoutHook = avoidStruts  $  layoutHook defaultConfig
    , startupHook = execScriptHook "startup"
    , modMask = mod4Mask     -- Rebind Mod to the Windows key
    , terminal = "terminator"
    } `additionalKeys`
    [ ((mod4Mask, xK_a), spawn "alsamixergui")
    , ((mod4Mask, xK_p), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((mod4Mask, xK_v), spawn "gvim")
    , ((mod4Mask, xK_f), spawn "nautilus")
    , ((mod4Mask, xK_c), spawn "google-chrome")
    , ((mod1Mask, xK_Tab), windows W.focusDown)
    ]
