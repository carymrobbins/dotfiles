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

-- 2 monitors
-- myScreenKeys = zip [xK_w, xK_e, xK_r, xK_d] [0,0,1,0]

myKeys =
  -- Set screen keys
  [
    ((m .|. myModKey, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- myScreenKeys
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ] ++
  -- Scratchpads
  [ ((shiftMask .|. myModKey, xK_h), scratch "hamster")
  , ((shiftMask .|. myModKey, xK_t), scratch "trello")
  , ((shiftMask .|. myModKey, xK_v), scratch "vim-cheatsheet")
  , ((shiftMask .|. myModKey, xK_m), scratch "thunderbird")
  ]
  where
  scratch = namedScratchpadAction myScratchpads

myScratchpads =
  [ NS "hamster"
       -- Avoid spawning multiple instances of hamster
       "bash -c 'killall hamster 2>/dev/null || true && hamster'"
       (className =? "Hamster")
       (customFloating $ centeredRect (1/3) (2/3))

  , NS "trello"
       "trello"
       (className =? "Trello")
       (customFloating $ centeredRect (2/3) (2/3))

  , NS "vim-cheatsheet"
       "terminator -T vim-cheatsheet -x 'vim ~/dotfiles/notes/vim-cheatsheet.md'"
       (title =? "vim-cheatsheet")
       (customFloating $ centeredRect (1/3) (2/3))
  , NS "thunderbird"
       "bash -c 'env GTK_THEME=Adwaita:light thunderbird'"
       (className =? "Thunderbird")
       (customFloating $ centeredRect (13/14) (13/14))
  ]

-- | Rectangle centered on screen with specified width and height, sizes
--   relative to the screen size.
centeredRect w h = W.RationalRect ((1-w)/2) ((1-h)/2) w h

myManageHook =
  def
  <+> manageDocks
  <+> namedScratchpadManageHook myScratchpads
  <+> specificWindowManageHooks

specificWindowManageHooks =
  foldMap (--> doFloat) floatQueries
  <+> foldMap (--> doIgnore) ignoreQueries
  where
  floatQueries =
    [ appName =? "Amethyst"
    , className =? "Nm-connection-editor"
    , title =? "Terminator Preferences"
    , title =? "Quit GIMP"
    , className =? "Blueman-manager"
    , className =? "Pavucontrol"
    , className =? "Remmina"
    , windowRole =? "gimp-dock"
    , windowRole =? "gimp-toolbox"
    , title =? "Install user style"
    , windowRole =? "autoconfig" -- thunderbird config windows
    ]

  ignoreQueries =
    [ title =? "Slack Call Minipanel"
    ]

windowRole = stringProperty "WM_WINDOW_ROLE"

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
