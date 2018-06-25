{-# LANGUAGE TypeApplications #-}
import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.UpdatePointer
import qualified XMonad.DBus as D
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Named
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.EZConfig
import           XMonad.Util.Run

import           Control.Monad
import           Control.Monad.Extra (whenM)
import           Data.Coerce
import           Data.IORef
import           System.IO.Unsafe

useXMobar = False

myModKey = mod4Mask

-- 4 monitors
-- myScreenKeys = zip [xK_w, xK_e, xK_r, xK_d] [3,1,2,0]

-- 3 monitors
myScreenKeys = zip [xK_w, xK_e, xK_r, xK_d] [1,0,2,0]

-- 2 monitors
-- myScreenKeys = zip [xK_w, xK_e, xK_r, xK_d] [1,0,1,0]

myWorkspaces@
  [ wsTerm,   wsDev,    wsBrowse, wsChat,   ws5,   ws6, ws7, ws8, ws9 ] =
  [ prompt,   rocket,   earth,    chats,    html5, "6", "7", "8", "9" ]
  where
  prompt = "\xf120"
  wrench = "\xf0ad"
  rocket = "\xf135"
  earth  = "\xf0ac"
  chats  = "\xf086"
  html5  = "\xf13b"

myKeys =
  -- Set screen keys
  [
    (
      (m .|. myModKey, key),
      -- Focus workspace, possibly refocusing the mouse.
      screenWorkspace sc >>= flip whenJust (windows . f) >> refocusMouse
    ) | (key, sc) <- myScreenKeys
      , (f, m)    <- [(W.view, 0), (W.shift, shiftMask)]
  ]

  -- Override the existing restart xmonad key binding.
  & ((myModKey, xK_q), restartXMonad)

  -- Focus next/prev window, possibly refocusing the mouse.
  & ((myModKey, xK_j), windows W.focusDown >> refocusMouse)
  & ((myModKey, xK_k), windows W.focusUp   >> refocusMouse)

  -- Switch to NSP workspace
  &  ((myModKey, xK_0), windows $ W.view "NSP")
  &  ((shiftMask .|. myModKey, xK_0), windows $ W.shift "NSP")

  -- Scratchpads
  & ((shiftMask .|. myModKey, xK_h), scratch "hamster")
  -- & ((shiftMask .|. myModKey, xK_t), scratch "trello")
  & ((shiftMask .|. myModKey, xK_t), scratch "toggl")
  & ((shiftMask .|. myModKey, xK_v), scratch "vim-cheatsheet")
  & ((shiftMask .|. myModKey, xK_m), scratch "thunderbird")

  -- Cycle through workspaces
  & ((myModKey,               xK_n), moveTo Next NonEmptyWS)
  & ((shiftMask .|. myModKey, xK_n), moveTo Prev NonEmptyWS)

  -- Toggle mouse-follows-focus(-like) behavior
  & ((myModKey, xK_m), toggleMouseFollowsFocus >> refocusMouse)

  -- Sticky
  & ((              myModKey, xK_s), windows $ copyToAll)
  & ((shiftMask .|. myModKey, xK_s), killAllOtherCopies)

  -- Float to medium-ish window
  & ((shiftMask .|. myModKey, xK_equal), floatScreenWidth $ const (1/3))
  -- Float to small-ish window
  & ((shiftMask .|. myModKey, xK_minus), floatScreenWidth $ const (1/5))
  -- Increase/decrease floating window size by 5%
  & ((              myModKey, xK_equal), floatScreenWidth $       (+ 0.05))
  & ((              myModKey, xK_minus), floatScreenWidth $ subtract 0.05)
  where
  (&) = flip (:)
  scratch = namedScratchpadAction myScratchpads

  refocusMouse =
    whenM (coerce <$> shouldRefocusMouse) $
      updatePointer (0.5, 0.5) (0, 0) -- Exact center of window

  shouldRefocusMouse = XS.get @MouseFollowsFocus
  toggleMouseFollowsFocus = XS.modify (MouseFollowsFocus . not . coerce)

  -- Float the focused window, resizing relative to its current width
  -- (which is relative to the screen size.
  floatScreenWidth f = withFocused $ \wid -> do
    (_, W.RationalRect x y w _) <- floatLocation wid
    windows (W.float wid (W.RationalRect x y (f w) (f w)))

  -- Augmented restart XMonad command. This restarts polybar after we restart XMonad
  -- to ensure that the `avoidStruts` in the layout hook can properly detect it and
  -- adjust the window layouts accordingly.
  -- Also this will use notify-send instead of xmessage for error messages. It also
  -- reports compilation errors instead of silently failing to restart.
  -- Adapted logic from -
  -- https://www.stackage.org/haddock/lts-11.14/xmonad-0.13/src/XMonad.Config.html#keys
  restartXMonad = spawn $ unlines
    [ "if type xmonad; then"
    , "  out=$(xmonad --recompile 2>&1)"
    , "  if [ $? -ne 0 ]; then"
    , "    term  XMonad \"$out\""
    , "  else"
    , "    xmonad --restart &&"
    , "      sleep 0.1 &&"
    , "      systemctl --user restart polybar"
    , "  fi"
    , "else"
    , "  notify-send XMonad \"xmonad not in \\$PATH: $PATH\""
    , "fi"
    ]

newtype MouseFollowsFocus = MouseFollowsFocus Bool
  deriving (Typeable, Read, Show)

instance ExtensionClass MouseFollowsFocus where
  initialValue = MouseFollowsFocus True
  extensionType = PersistentExtension

myScratchpads =
  [ NS "hamster"
       -- Avoid spawning multiple instances of hamster
       "bash -c 'killall hamster 2>/dev/null || true && hamster'"
       (className =? "Hamster")
       (customFloating $ centeredRect (1/3) (2/3))

  , NS "toggl"
       -- Avoid spawning multiple instances of hamster
       "bash -c 'killall TogglDesktop 3>/dev/null || true && toggldesktop'"
       (className =? "Toggl Desktop")
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
       nonFloating
       --(customFloating $ centeredRect (13/14) (13/14))
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
  <+> (className =? "jetbrains-idea"    <&&> title /=? "Confirm Exit" --> doShift wsDev)
  <+> (className =? "jetbrains-idea-ce" <&&> title /=? "Confirm Exit" --> doShift wsDev)
  -- Browsers
  <+> (className =? "Google-chrome" --> doShift wsBrowse)
  -- Chat windows
  <+> (className =? "Slack"    --> doShift wsChat)
  <+> (title     =? "Gitter"   --> doShift wsChat)
  <+> (className =? "Hexchat"  --> doShift wsChat)

-- | Query for not-equal, negation of =?
(/=?) :: Eq a => Query a -> a -> Query Bool
q /=? x = fmap (/= x) q

specificWindowManageHooks =
  def
  <+> foldMap (--> doFloat) floatQueries
  <+> foldMap (--> doIgnore) ignoreQueries
  <+> foldMap (--> doShift "NSP") hideQueries
  where
  floatQueries =
    [ appName =? "Amethyst"
    , className =? "Nm-connection-editor"
    , title =? "Terminator Preferences"
    -- , title =? "Quit GIMP"
    , className =? "Blueman-manager"
    -- This is always hard to find, ends up mostly off screen
    -- , className =? "Pavucontrol"
    -- , className =? "Remmina"
    , windowRole =? "gimp-dock"
    , windowRole =? "gimp-toolbox"
    , title =? "Install user style"
    , windowRole =? "autoconfig" -- thunderbird config windows
    , windowRole =? "gimp-action-search-dialog"
    , className =? "Unity" <&&> title =? "Starting Unity..."
    ]

  ignoreQueries =
    [ title =? "Slack Call Minipanel"
    -- Help IntelliJ's autocomplete popup to appear on the top of the IDE
    , appName =? "sun-awt-X11-XWindowPeer" <&&> className =? "jetbrains-idea"
    ]

  hideQueries =
    [ className =? "Unity" <&&> title =? "Starting Unity..."
    ]

myLogHooks maybeXMobarProc dbus = def <+> ppLog
  where
  ppLog = maybe polybarLog xmobarLog maybeXMobarProc

  polybarLog = dynamicLogWithPP def
    { ppOutput = D.send dbus
    , ppVisible = wrap " " " "
    , ppHidden  = hideNSP
    , ppCurrent = wrap ("%{F" <> myBlue <> "} ") " %{F-}"
    -- , ppCurrent = wrap ("%{u" <> myBlue <> " +u} ") " %{-u}"
    , ppUrgent  = wrap "%{F#f00}" "%{F-}"
    , ppSep     = " | "
      -- No need for title since I use top bars/tabs
    , ppTitle = const ""
    }

  xmobarLog xmobarProc = dynamicLogWithPP xmobarPP
    { ppOutput  = hPutStrLn xmobarProc
    , ppCurrent = xmobarColor myBlue "" . wrap "[" "]"
      -- No need for title since I use top bars/tabs
    , ppTitle = const ""
    }

  hideNSP wId = if wId == "NSP" then "" else wrap " " " " wId

myLayoutHook =
  (avoidStruts $ two ||| three ||| full) ||| fullNoBar
  where
  two       = named twoName       $ addBarGaps $ withDims Tall
  three     = named threeName     $ addBarGaps $ withDims ThreeColMid
  full      = named fullName      $ tabbedAlways shrinkText myTopBarTheme
  fullNoBar = named fullNoBarName Full

  polyName s    = "%{F#555}" <> s <> "%{F-}"
  twoName       = polyName "\xe131"
  threeName     = polyName "\xe134"
  fullName      = polyName "\xe0b7"
  fullNoBarName = polyName "\xe130"

  addBarGaps = addTopBar . addGaps
  addTopBar = noFrillsDeco shrinkText myTopBarTheme
  addGaps = smartSpacing 5
  withDims f = f nmaster delta ratio
  nmaster = 1
  delta   = 3/100
  ratio   = 1/2

myTopBarTheme = def
  { fontName = "xft:Noto:style=Bold:pixelsize=10:hinting=true"
  , activeTextColor = activeFG
  , activeColor = activeBG
  , activeBorderColor = activeBG
  , inactiveColor = inactiveBG
  , inactiveBorderColor = inactiveBG
  , decoHeight = 15
  }
  where
  activeBG   = "#68c987"
  activeFG   = "#112d1a"
  inactiveBG = "#dddddd"

myBlue       = "#268bd2"

windowRole = stringProperty "WM_WINDOW_ROLE"

main = do
  dbus <- D.connect
  D.requestAccess dbus
  maybeXMobarProc <-
    if useXMobar then
      Just <$> spawnPipe "xmobar ~/.xmonad/xmobarrc"
    else
      return Nothing
  xmonad $ def
    { terminal = "bash -c 'term || terminator'"
    , modMask = myModKey
    , borderWidth = 0
    , handleEventHook = def <+> docksEventHook
    , startupHook = setWMName "LG3D"
    , manageHook = myManageHook
    , layoutHook = myLayoutHook
    , logHook = myLogHooks maybeXMobarProc dbus
    , workspaces = myWorkspaces
    } `additionalKeys` myKeys
