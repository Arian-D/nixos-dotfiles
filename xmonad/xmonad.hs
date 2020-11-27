import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.XSelection (promptSelection)
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import System.IO

main = xmonad $ def
  { borderWidth = 3
  , modMask = mod4Mask
  , terminal = "alacritty"
  , focusedBorderColor = "#0000ff"
  , manageHook = manageDocks <+> manageHook def
  } `additionalKeys`
  [ ((mod4Mask, xK_p), spawn "rofi -show drun") -- Rofi > dmenu
  , ((mod4Mask, xK_backslash), promptSelection "mpv") -- Watch clipboard
  ]

test x = do
  w <- x
  return w
