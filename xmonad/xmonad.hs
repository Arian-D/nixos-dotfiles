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
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import System.IO
import qualified Data.Map as M
import System.Environment (lookupEnv)
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
  home <- getHomeDirectory
  setWallpaper $ home </> "wallpaper"
  xmonad $ fullscreenSupport $  docks def
    { borderWidth = 2
    , modMask = mod4Mask
    , terminal = "alacritty"
    , focusedBorderColor = "#0000ff"
    -- , layoutHook =
    --   let gap = 10
    --       border = Border gap gap gap gap
    --   in
    --     avoidStruts $ (spacingRaw False-- Smart border
    --                    border -- Screen border
    --                    True
    --                    border -- Window border
    --                    True) $ layoutHook def
    , layoutHook = avoidStruts (
        Full |||
        Tall 1 (3/100) (1/2)) |||
        noBorders (fullscreenFull Full)
                   
    , manageHook = manageDocks <+> manageHook def
    , keys = myKeys <+> keys def
    }

-- Key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
  [ ((modm, xK_F12), xmonadPrompt def)
  , ((modm, xK_F3), shellPrompt def)
  , ((modm, xK_backslash), promptSelection "mpv") -- Watch clipboard
  , ((modm, xK_p), spawn "rofi -show drun") -- Rofi > dmenu
  , ((modm .|. shiftMask, xK_g),
     toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled)
  ]

setWallpaper :: FilePath -> IO ()
setWallpaper path = do
  isThere <- doesFileExist path 
  if isThere
    then spawn . unwords $ ["feh", "--bg-fill", path]
    else return ()
