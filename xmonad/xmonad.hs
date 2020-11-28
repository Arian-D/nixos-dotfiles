import XMonad
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.XSelection (promptSelection)
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed (shrinkText, tabbed)
import XMonad.Layout.Accordion -- (Accordion)
import System.IO
import qualified Data.Map as M

main = xmonad $ docks def
  { borderWidth = 2
  , modMask = mod4Mask
  , terminal = "alacritty"
  , focusedBorderColor = "#0000ff"
  , layoutHook =
    let gap = 10
        border = Border gap gap gap gap
    in
    avoidStruts $ (spacingRaw True -- Smart border
      border -- Screen border
      False
      border -- Window border
      False) $ layoutHook def
  , manageHook = manageDocks <+> manageHook def
  , keys = myKeys <+> keys def
  }

-- Key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
  [ ((modm, xK_F12), xmonadPrompt def)
  , ((modm, xK_F3), shellPrompt def)
  , ((modm, xK_backslash), promptSelection "mpv") -- Watch clipboard
  , ((modm, xK_p), spawn "rofi -show drun") -- Rofi > dmenu
  , ((modm .|. shiftMask, xK_g), toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled)
  ] 
