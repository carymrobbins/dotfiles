#!/usr/bin/env stack
{- stack
  --resolver lts-18.27
  --install-ghc runghc
  --package karabiner-config-0.0.0.0
-}

-- This script simply outputs the JSON used to configure Karabiner-Elements
-- Tou must redirect the output it into the appropriate file, e.g.
--    ~/dotfiles/.config/karabiner/assets/complex_modifications/linux.json
-- There is a wrapper script that handles this for us
--    ~/dotfiles/.config/karabiner/update

{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall #-}

import Data.Functor
import Karabiner.Config

main :: IO ()
main =  mkMain root

root :: Root
root = Root "Linux Compat" [rule]
  where
  rule = Rule "Various remappings to make it feel like linux" $ mempty
    <> generalRemaps
    <> terminalRemaps
    <> intellijRemaps
    <> slackRemaps
    <> notTerminalOrIntelliJRemaps
    <> skhdRemaps
    <> chunkwmRemaps
    -- <> vimArrowsRemaps
    -- <> moveWindowToWorkspaceRemaps
    -- <> switchWorkspaceRemaps

  generalRemaps =
    [ -- Spotlight
      Command |+| P !> RightCommand |+| Spacebar
    ]

  terminalRemaps = concat
    [
      -- Tmux: Switch windows with Ctrl+<number>
      numbers >>= \n -> [ Option |+| n !> tmuxPrefix |-> singleKey n ]
    , [
        -- Tmux: Next window
        Option |+| N !> tmuxPrefix |-> singleKey N
        -- Tmux: Previous window
      , Option |+| P !> tmuxPrefix |-> singleKey P
        -- Copy
      , [Control, Shift] |+| C !> RightCommand |+| C
        -- Paste
      , [Control, Shift] |+| V !> RightCommand |+| V
      ]

      -- Tmux: Use option+hjkl for moving between panes
    -- , [H, J, K, L] <&> \hjkl ->
    --     Option |+| hjkl !> tmuxPrefix |-> singleKey hjkl
    ] ?? [macterm, iterm]

  -- The Option + Key mappings in IntelliJ don't work consistently due
  -- to Mac's preference to insert unicode chars instead.
  -- These mappings override that behavior.
  intellijRemaps = concat
    [ [ -- Jump to Super method
        Option |+| U !> RightCommand |+| U
        -- Preferences
      , [Control, Option] |+| S !> RightCommand |+| Comma
        -- Project Structure
      , [Shift, Control, Option] |+| S !> RightCommand |+| Semicolon
      ]

      -- Tmux-ish: Use option+hjkl for moving between panes
      -- This works because I already have some bindings in IntelliJ for
      -- tmuxPrefix + hjkl
    , [H, J, K, L] <&> \hjkl ->
        Option |+| hjkl !> tmuxPrefix |-> singleKey hjkl
    ] ?? [intellij]

  slackRemaps =
    (numbers >>= \n -> [ Control |+| n !> RightCommand |+| n ])
    ?? [slack]

  -- These bindings don't work well with iterm2 or intellij due
  -- to clashes with vim, tmux, etc, so excluding them with ??!
  notTerminalOrIntelliJRemaps =
    (
      -- Useful for moving to tab n of an app, e.g. browser.
      (map (\n -> Option |+| n !> RightCommand |+| n) numbers)
      <>
      [ -- New tab
        Control |+| T !> RightCommand |+| T
        -- Open closed tab
      , [Control, Shift] |+| T !> [RightCommand, RightShift] |+| T
        -- Close tab
      , Control |+| W !> RightCommand |+| W
        -- Copy
      , Control |+| C !> RightCommand |+| C
        -- Cut
      , Control |+| X !> RightCommand |+| X
        -- Paste
      , Control |+| V !> RightCommand |+| V
        -- Paste without formatting
      , [Control, Shift] |+| V !> [RightCommand, RightShift] |+| V
        -- Select all
      , Control |+| A !> RightCommand |+| A
        -- Find
      , Control |+| F !> RightCommand |+| F
        -- Reload
      , Control |+| R !> RightCommand |+| R
        -- Force reload
      , [Shift, Control] |+| R !> [RightShift, RightCommand] |+| R
        -- Undo
      , Control |+| Z !> RightCommand |+| Z
        -- Redo
      , Control |+| Y !> RightCommand |+| Y
        -- Highlight address
      , Control |+| L !> RightCommand |+| L
        -- Previous tab
      , [Option, Shift] |+| OpenBracket  !> [RightCommand, RightShift] |+| OpenBracket
        -- Next tab
      , [Option, Shift] |+| CloseBracket !> [RightCommand, RightShift] |+| CloseBracket
        -- Back / Forward
      , Option |+| LeftArrow  !> RightCommand |+| LeftArrow
      , Option |+| RightArrow !> RightCommand |+| RightArrow
        -- Delete previous word
      , Control |+| Backspace !> RightOption |+| Backspace
        -- Chrome: Private tab
      , [Control, Shift] |+| N !> [RightCommand, RightShift] |+| N
        -- Firefox: Private tab
      , [Control, Shift] |+| P !> [RightCommand, RightShift] |+| P
        -- Slack: Go to conversation
      , Control |+| K !> RightCommand |+| K
      , Control |+| ReturnOrEnter !> RightCommand |+| ReturnOrEnter
        -- Print
      , Control |+| P !> RightCommand |+| P

      , Control |+| Zero !> RightCommand |+| Zero
      ]
    ) ??! [macterm, iterm, intellij]

  skhdRemaps = numbers >>= \n ->
    [ -- Mapped in ~/.skhdrc
      Command |+| n !> chunkMod |+| n
    ]

  -- Key maps to use simulate Xmonad.
  chunkwmRemaps =
    [
      -- Focus monitor 1
      Command |+| W !> chunkMod |+| W
      -- Focus monitor 2
    , Command |+| E !> chunkMod |+| E
      -- Focus monitor 3
    , Command |+| R !> chunkMod |+| R
      -- Move window to monitor 1
    , [Shift, Command] |+| W !> chunkShiftMod |+| W
      -- Move window to monitor 2
    , [Shift, Command] |+| E !> chunkShiftMod |+| E
      -- Move window to monitor 3
    , [Shift, Command] |+| R !> chunkShiftMod |+| R
      -- Focus previous window
    , Command |+| J !> chunkMod |+| J
      -- Focus next window
    , Command |+| K !> chunkMod |+| K
      -- Move window to previous
    , [Shift, Command] |+| J !> chunkShiftMod |+| J
      -- Move window to next
    , [Shift, Command] |+| K !> chunkShiftMod |+| K

    -- Disable these so we don't break Slack huddle mute
    --  -- Default (bsp) layout
    --, Command |+| Spacebar !> chunkMod |+| Spacebar
    --  -- Full (monocle) layout
    --, [Shift, Command] |+| Spacebar !> chunkShiftMod |+| Spacebar

      -- Toggle float current window
    , Command |+| T !> chunkMod |+| T
      -- Grow region
    , Command |+| H !> chunkMod |+| H
      -- Shrink region
    , Command |+| L !> chunkMod |+| L
    ]

  -- switchWorkspaceRemaps = numbers >>= \n ->
  --   [ -- Use Command+number instead of Control+number for switching workspaces.
  --     Command          |+| n !> RightControl  |+| n
  --   ]

  -- moveWindowToWorkspaceRemaps = numbers >>= \n ->
  --   [ -- Use Shift+Command+number to move window to workspace via chunkwm
  --     [Shift, Command] |+| n !> chunkShiftMod |+| n
  --   ]

  -- vimArrowsRemaps =
  --   [ -- Remap Option+h/j/k/l to arrow keys
  --     Option |+| H !> LeftArrow
  --   , Option |+| J !> DownArrow
  --   , Option |+| K !> UpArrow
  --   , Option |+| L !> RightArrow
  --     -- Remap Shift+Option+h/j/k/l to shift+arrow keys
  --     -- Useful when highlighting with vim arrows
  --   , [Shift, Option] |+| H !> RightShift |+| LeftArrow
  --   , [Shift, Option] |+| J !> RightShift |+| DownArrow
  --   , [Shift, Option] |+| K !> RightShift |+| UpArrow
  --   , [Shift, Option] |+| L !> RightShift |+| RightArrow
  --   ]

  -- My ~/.chunkwmrc uses these keys as "mod" keys
  chunkMod = [RightControl, RightOption, RightCommand]
  chunkShiftMod = RightShift : chunkMod

  tmuxPrefix = RightControl |+| A

  -- Patterns for binding keys only for certain apps.
  iterm = litPat "com.googlecode.iterm2"
  macterm = litPat "com.apple.Terminal"
  -- chrome = litPat "com.google.Chrome"
  intellij = "com.jetbrains.intellij.*"
  slack = litPat "com.tinyspeck.slackmacgap"
