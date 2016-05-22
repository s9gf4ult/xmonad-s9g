module XMonad.Lens where

import Control.Lens
import XMonad.Core
import XMonad.StackSet

-- = Core

makeLensesFor
  [(x, "_" ++ x)
  | x <- [ "normalBorderColor"
         , "focusedBorderColor"
         , "terminal"
         , "layoutHook"
         , "manageHook"
         , "handleEventHook"
         , "workspaces"
         , "modMask"
         , "keys"
         , "mouseBindings"
         , "borderWidth"
         , "logHook"
         , "startupHook"
         , "focusFollowsMouse"
         , "clickJustFocuses"
         , "clientMask"
         , "rootMask"
         , "handleExtraArgs"]]
  ''XConfig

makeLensesFor
  [(x, "_" ++ x)
  | x <- [ "display"
         , "config"
         , "theRoot"
         , "normalBorder"
         , "focusedBorder"
         , "keyActions"
         , "buttonActions"
         , "mouseFocused"
         , "mousePosition"
         , "currentEvent" ]]
  ''XConf

makeLensesFor
  [(x, "_" ++ x)
  | x <- [ "windowset"
         , "mapped"
         , "waitingUnmap"
         , "dragging"
         , "numberlockMask"
         , "extensibleState" ]]
  ''XState

-- = StackSet

makeLensesFor
  [(x, "_" ++ x)
  | x <- [ "current"
         , "visible"
         , "hidden"
         , "floating" ]]
  ''StackSet

makeLensesFor
  [(x, "_" ++ x)
  | x <- [ "workspace"
         , "screen"
         , "screenDetail" ]]
  ''Screen

makeLensesFor
  [(x, "_" ++ x)
  | x <- [ "tag"
         , "layout"
         , "stack" ]]
  ''Workspace

xpos :: Lens' RationalRect Rational
xpos = lens g s
  where
    g (RationalRect x _ _ _) = x
    s (RationalRect _ y w h) x = RationalRect x y w h

ypos :: Lens' RationalRect Rational
ypos = lens g s
  where
    g (RationalRect _ y _ _) = y
    s (RationalRect x _ w h) y = RationalRect x y w h

width :: Lens' RationalRect Rational
width = lens g s
  where
    g (RationalRect _ _ w _) = w
    s (RationalRect x y _ h) w = RationalRect x y w h

height :: Lens' RationalRect Rational
height = lens g s
  where
    g (RationalRect _ _ _ h) = h
    s (RationalRect x y w _) h = RationalRect x y w h

makeLensesFor
  [(x, "_" ++ x)
  | x <- [ "focus"
         , "up"
         , "down" ]]
  ''Stack
