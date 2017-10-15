import           XMonad
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run

myModKey = mod4Mask

-- 4 monitors
-- myScreenKeys = zip [xK_w, xK_e, xK_r, xK_d] [3,1,2,0]

-- 3 monitors
myScreenKeys = zip [xK_w, xK_e, xK_r, xK_d] [1,0,2,0]

myKeys =
  -- Set screen keys
  [
    ((m .|. myModKey, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- myScreenKeys
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ] ++
  -- Scratchpads
  [ ((shiftMask .|. myModKey, xK_h), scratchHamster)
  ]
  where
  scratchHamster = namedScratchpadAction myScratchpads "hamster"

myScratchpads =
  [ NS "hamster" spawnHamster findHamster manageHamster
  ]
  where
  -- Avoid spawning multiple instances of hamster
  spawnHamster = "bash -c 'killall hamster 2>/dev/null || true && hamster'"
  findHamster = className =? "Hamster"
  manageHamster = customFloating $ W.RationalRect h w t l
    where
    h = 0.3
    w = 0.3
    t = (1 - h) / 2
    l = (1 - w) / 1.5

myManageHook =
  def
  <+> manageDocks
  <+> namedScratchpadManageHook myScratchpads
  -- Float that annoying slack call popup
  <+> (title =? "Slack Call Minipanel" --> doFloat)
  -- Float amethyst games, so long as their app name is 'Amethyst'
  <+> (appName =? "Amethyst" --> doFloat)

main = do
  xmobarProc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ def
    { terminal = "terminator"
    , modMask = myModKey
    , borderWidth = 1
    , focusedBorderColor = "#268bd2"
    , handleEventHook = def <+> docksEventHook
    , startupHook = setWMName "LG3D"
    , manageHook = myManageHook
    , layoutHook = avoidStruts $ layoutHook def
    , logHook =
        updatePointer (0.5, 0.5) (0, 0) -- Exact center of window
        <+> dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn xmobarProc }
    } `additionalKeys` myKeys
