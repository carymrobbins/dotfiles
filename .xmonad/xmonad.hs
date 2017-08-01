import           XMonad
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run

myModKey = mod4Mask

-- 4 monitors
-- myScreenKeys = zip [xK_w, xK_e, xK_r, xK_d] [3,1,2,0]

-- 3 monitors
myScreenKeys = zip [xK_w, xK_e, xK_r, xK_d] [1,0,2,0]

myKeys =
  [((m .|. myModKey, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- myScreenKeys
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)] ]

main = do
  xmobarProc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ def
    { terminal = "terminator"
    , modMask = myModKey
    , borderWidth = 1
    , focusedBorderColor = "#268bd2"
    , handleEventHook = def <+> docksEventHook
    , startupHook = setWMName "LG3D"
    , manageHook = def <+> manageDocks
    , layoutHook = avoidStruts $ layoutHook def
    , logHook =
        updatePointer (0.5, 0.5) (0, 0) -- Exact center of window
        <+> dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn xmobarProc }
    } `additionalKeys` myKeys
