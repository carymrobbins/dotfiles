import XMonad
import XMonad.Hooks.SetWMName

main = do
  xmonad defaultConfig
    { terminal = "terminator"
    , modMask = mod4Mask
    , borderWidth = 1
    , startupHook = setWMName "LG3D"
    }
