import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.EZConfig
import           XMonad.Util.Run

myModKey = mod4Mask

-- 4 monitors
-- myScreenKeys = zip [xK_w, xK_e, xK_r, xK_d] [3,1,2,0]

-- 3 monitors
myScreenKeys = zip [xK_w, xK_e, xK_r, xK_d] [1,0,2,0]

-- 2 monitors
-- myScreenKeys = zip [xK_w, xK_e, xK_r, xK_d] [1,0,1,0]

myKeys =
  -- Set screen keys
  [
    ((m .|. myModKey, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- myScreenKeys
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]

  -- Switch to NSP workspace
  &  ((myModKey, xK_0), windows $ W.greedyView "NSP")

  -- Scratchpads
  & ((shiftMask .|. myModKey, xK_h), scratch "hamster")
  & ((shiftMask .|. myModKey, xK_t), scratch "trello")
  & ((shiftMask .|. myModKey, xK_v), scratch "vim-cheatsheet")
  & ((shiftMask .|. myModKey, xK_m), scratch "thunderbird")

  -- Cycle through invisible workspaces, allows selecting the NSP workspace
  & ((myModKey, xK_n), moveTo Next HiddenNonEmptyWS)

  -- Move mouse to center of focused window
  & ((myModKey, xK_m), centerMouse)

  -- Sticky
  -- TODO: Not quite perfect yet...
  -- & ((              myModKey, xK_s), windows $ copy "1")
  -- & ((shiftMask .|. myModKey, xK_s), killAllOtherCopies)
  where
  scratch = namedScratchpadAction myScratchpads
  centerMouse = updatePointer (0.5, 0.5) (0, 0) -- Exact center of window
  (&) = flip (:)

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
  <+> defaultWorkspaces
  <+> specificWindowManageHooks

defaultWorkspaces =
  def
  -- IntelliJ, except the "exit" dialog should just appear on the active window
  <+> (className =? "jetbrains-idea" <&&> title /=? "Confirm Exit" --> doShift "2")
  -- Browsers
  <+> (className =? "Google-chrome" --> doShift "3")
  -- Chat windows
  <+> (className =? "Slack"    --> doShift "4")
  <+> (title     =? "Gitter"   --> doShift "4")
  <+> (className =? "Hexchat"  --> doShift "4")

-- | Query for not-equal, negation of =?
(/=?) :: Eq a => Query a -> a -> Query Bool
q /=? x = fmap (/= x) q

specificWindowManageHooks =
  def
  <+> foldMap (--> doFloat) floatQueries
  <+> foldMap (--> doIgnore) ignoreQueries
  where
  floatQueries =
    [ appName =? "Amethyst"
    , className =? "Nm-connection-editor"
    , title =? "Terminator Preferences"
    , title =? "Quit GIMP"
    , className =? "Blueman-manager"
    -- This is always hard to find, ends up mostly off screen
    -- , className =? "Pavucontrol"
    , className =? "Remmina"
    , windowRole =? "gimp-dock"
    , windowRole =? "gimp-toolbox"
    , title =? "Install user style"
    , windowRole =? "autoconfig" -- thunderbird config windows
    ]

  ignoreQueries =
    [ title =? "Slack Call Minipanel"
    -- Help IntelliJ's autocomplete popup to appear on the top of the IDE
    , appName =? "sun-awt-X11-XWindowPeer" <&&> className =? "jetbrains-idea"
    ]

myLogHooks xmobarProc =
  def
  <+> xmobarLog
  where
  xmobarLog = dynamicLogWithPP xmobarPP
    { ppOutput  = hPutStrLn xmobarProc
    , ppCurrent = xmobarColor myBlue "" . wrap "[" "]"
    -- I don't really need to see the layout name or active window title in xmobar
    , ppTitle = const ""
    -- , ppLayout = const ""
    }

myLayoutHook =
  avoidStruts $ layouts
  where
  addTopBar = noFrillsDeco shrinkText topBarTheme
  addGaps = smartSpacing 5
  tall = Tall 1 (3/100) (1/2)

  -- Pass function `f` so we can `Mirror` before we add the top bar.
  tiled f = addTopBar $ f $ addGaps $ tall
  full    = addTopBar $ Full

  layouts =
    tiled id
    ||| tiled Mirror
    ||| full

topBarTheme = def
  { fontName = "xft:Noto:style=Bold:pixelsize=10:hinting=true"
  , activeColor = myBlue
  , activeBorderColor = myBlue
  , inactiveColor = myInactiveColor
  , inactiveBorderColor = myInactiveColor
  , decoHeight = 15
  }

myBlue = "#268bd2"
myInactiveColor = "#dddddd"

windowRole = stringProperty "WM_WINDOW_ROLE"

main = do
  xmobarProc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ def
    { terminal = "bash -c 'term || terminator'"
    , modMask = myModKey
    , borderWidth = 0
    , focusedBorderColor = myBlue
    , handleEventHook = def <+> docksEventHook
    , startupHook = setWMName "LG3D"
    , manageHook = myManageHook
    , layoutHook = myLayoutHook
    , logHook = myLogHooks xmobarProc
    } `additionalKeys` myKeys
