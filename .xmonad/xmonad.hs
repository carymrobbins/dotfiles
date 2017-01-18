import XMonad
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Util.Run

main = do
  xmonad def
    { terminal = "terminator"
    , modMask = mod4Mask
    , borderWidth = 1
    , startupHook = setWMName "LG3D"
    , logHook = updatePointer (0.5, 0.5) (0, 0) -- Exact center of window
    }
