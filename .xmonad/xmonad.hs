import           XMonad
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run

myModKey = mod4Mask

myScreenKeys = zip [xK_w, xK_e, xK_r, xK_d] [3,1,2,0]

myLayout = tiled ||| Mirror tiled ||| Full
  where
  -- default tiling algorithm partitions the screen into two panes
  tiled   = Tall nmaster delta ratio

  -- The default number of windows in the master pane
  nmaster = 1

  -- Default proportion of screen occupied by master pane
  ratio   = 1/2

  -- Percent of screen to increment by when resizing panes
  delta   = 3/100

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
    , handleEventHook = def <+> docksEventHook
    , startupHook = setWMName "LG3D"
    , manageHook = def <+> manageDocks
    , layoutHook = avoidStruts myLayout
    , logHook =
        updatePointer (0.5, 0.5) (0, 0) -- Exact center of window
        <+> dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn xmobarProc }
    } `additionalKeys` myKeys
