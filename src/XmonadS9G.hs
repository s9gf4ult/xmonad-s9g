module XmonadS9G
  ( launch
  ) where

import Control.Lens
import Data.Monoid
import System.Exit
import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Lens

import qualified Data.Map        as M
import qualified XMonad.StackSet as W

launch :: IO ()
launch = xmonad $ configure $ ewmh $ def

configure :: XConfig l -> XConfig _
configure
  = set _terminal "xterm"
  . set _focusFollowsMouse True
  . set _clickJustFocuses  True
  . set _borderWidth 1
  . set _modMask  myModMask
  . set _workspaces  myWorkspaces
  . set _normalBorderColor  myNormalBorderColor
  . set _focusedBorderColor  myFocusedBorderColor
  . set _keys myKeys
  . set _mouseBindings  myMouseBindings
  . set _layoutHook  myLayout
  . over _manageHook  configureManageHook
  . over _handleEventHook  (<> fullscreenEventHook)
  -- . over _logHook  myLogHook
  -- . over _startupHook  myStartupHook

myModMask       = mod4Mask

myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

myKeys
  :: XConfig Layout
  -> M.Map (ButtonMask,KeySym) (X ())
myKeys conf =
  let modm = conf ^. _modMask
  in M.fromList $

     -- launch a terminal
     [ ((modm .|. shiftMask, xK_Return), spawn $ conf ^. _terminal)

     -- launch dmenu
     , ((modm,               xK_o     ), spawn "gmrun")

     -- close focused window
     , ((modm .|. shiftMask, xK_c     ), kill)

      -- Rotate through the available layout algorithms
     , ((modm,               xK_space ), sendMessage NextLayout)

     --  Reset the layouts on the current workspace to default
     , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

     -- Resize viewed windows to the correct size
     , ((modm,               xK_n     ), refresh)

     -- Move focus to the next window
     , ((modm,               xK_j     ), windows W.focusDown)

     -- Move focus to the previous window
     , ((modm,               xK_k     ), windows W.focusUp  )

     -- Move focus to the master window
     , ((modm,               xK_m     ), windows W.focusMaster  )

     -- Swap the focused window and the master window
     , ((modm,               xK_Return), windows W.swapMaster)

     -- Swap the focused window with the next window
     , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

     -- Swap the focused window with the previous window
     , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

     -- Shrink the master area
     , ((modm,               xK_h     ), sendMessage Shrink)

     -- Expand the master area
     , ((modm,               xK_l     ), sendMessage Expand)

     -- Push window back into tiling
     , ((modm,               xK_t     ), withFocused $ windows . W.sink)

     -- Increment the number of windows in the master area
     , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

     -- Deincrement the number of windows in the master area
     , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

     -- Toggle the status bar gap
     -- Use this binding with avoidStruts from Hooks.ManageDocks.
     -- See also the statusBar function from Hooks.DynamicLog.
     --
     -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

     -- Quit xmonad
     , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

     -- Restart xmonad
     , ((modm              , xK_q     ), spawn "xmonad-s9g --restart")

     ]
     ++

     --
     -- mod-[1..9], Switch to workspace N
     -- mod-shift-[1..9], Move client to workspace N
     --
     [((m .|. modm, k), windows $ f i)
         | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
     ++

     --
     -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
     -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
     --
     [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
         | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myLayout = tiled ||| Mirror tiled ||| noBorders Full
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

-- configureManageHook :: ManageHook -> ManageHook
configureManageHook hooks = composeAll
  [ hooks
  , className =? "MPlayer"        --> doFloat
  , className =? "Gimp"           --> doFloat
  , resource  =? "desktop_window" --> doIgnore
  , resource  =? "kdesktop"       --> doIgnore
  ]
