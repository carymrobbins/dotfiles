import           XMonad
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.SetWMName
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run

myModKey = mod1Mask

myScreenKeys = zip [xK_w, xK_e, xK_r, xK_d] [0,1,2,0]

myKeys =
  [((m .|. myModKey, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- screenKeys
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)] ]

main = do
  xmonad $ def
    { terminal = "terminator"
    , modMask = mod4Mask
    , borderWidth = 1
    , startupHook = setWMName "LG3D"
    , logHook = updatePointer (0.5, 0.5) (0, 0) -- Exact center of window
    } `additionalKeys` myKeys
