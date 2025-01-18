{-# LANGUAGE CPP #-}
-- xmonad config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config

import System.Process
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Util.Run (safeSpawn)
import System.IO
import System.Info
import System.Exit
import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicLog (xmobar)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.Minimize
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Decoration
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Util.Run(runProcessWithInput)
import XMonad.Util.Run(spawnPipe, unsafeSpawn, safeSpawn)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D
#ifdef STACK 
import XMonad.Actions.Minimize
#endif
import XMonad.Layout.Minimize
import qualified XMonad.Layout.BoringWindows as BW

import XMonad.Layout.ToggleLayouts
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.DynamicWorkspaceOrder as WD
import qualified Data.Map        as M
import qualified XMonad.Layout.BoringWindows
import Debug.Trace (traceShow)
import System.Environment (getEnvironment)
import Language.Haskell.TH.Syntax (runIO)


------------------------------------------------------------------------
-- Terminal
-- The preferred terminal program, which is used in a binding below and certain
-- by contrib modules.
--

myTerminal :: String
myTerminal = do
  if os == "freebsd"
    then "xterm"
    else "if which konsole > /dev/null ; then /usr/bin/konsole; else xterm; fi"
--    else "if which gnome-terminal > /dev/null ; then /usr/bin/gnome-terminal; else xterm; fi"

myBrowser :: String
myBrowser = do
  if os == "freebsd"
    then "firefox"
    else "if which firefox > /dev/null ; then firefox; else if which google-chrome > /dev/null ; then google-chrome ; else chromium-browser; fi; fi;"

-- The command to lock the screen or show the screensaver.
myScreensaver = "xwd | convert xwd:- capture_objects.png"

-- The command to take a selective screenshot, where you select
-- what you'd like to capture on the screen.
mySelectScreenshot = "select-screenshot"

-- The command to take a fullscreen screenshot.
myScreenshot = "screenshot"

-- The command to use as a launcher, to launch commands that don't have
-- preset keybindings.
myLauncher = "dmenu_run -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -nb '#000000' -nf '#FFFFFF' -sb '#7C7C7C' -sf '#CEFFAC'"
--     $(yeganesh -x -- -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -nb '#000000' -nf '#FFFFFF' -sb '#7C7C7C' -sf '#CEFFAC')"


------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces = ["1:term","2:code","3:code","4:web"] -- ,"5:web","6:vm","7:media"] ++ map show [8..9]


------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [
--      className =? "Chromium"       --> doShift "5:web"
--    , className =? "google-chrome"  --> doShift "5:web"
--    ,
      resource  =? "desktop_window" --> doIgnore
    , className =? "Galculator"     --> doFloat
    , className =? "Steam"          --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "gpicview"       --> doFloat
    , className =? "MPlayer"        --> doFloat
    , title     =? "Microsoft Teams Notification" --> doSideFloat NE
--  , className =? "VirtualBox"     --> doShift "6:vm"
    , className =? "Xchat"          --> doShift "7:media"
    , className =? "stalonetray"    --> doIgnore
    , isFullscreen --> (doF W.focusDown <+> doFullFloat  )]



------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout =  minimize ( avoidStruts (
    toggleLayouts Full (
                  ResizableTall 1 (10/100) (2/3) [] |||
                  tabbed shrinkText tabConfig
                  )
    ))



-- spiral (6/7) |||
--    ThreeColMid 1 (3/100) (1/2) |||
--
--    Mirror (Tall 1 (3/100) (1/2)) |||

------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the ir_black theme.
--
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6ff"

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = def {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}

-- Color of current window title in xmobar.
xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#CEFFAC"

-- Width of the window border in pixels.
myBorderWidth = 1


------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask
altModMask = mod1Mask


startdefaultinws :: X ()
startdefaultinws = do
    current <- gets (W.currentTag . windowset)
    if current == "5:web"
     then ( unsafeSpawn myBrowser )
     else ( unsafeSpawn myTerminal )

-- groupBy :: String -> Char -> [String]
-- groupBy str delim = let (start, end) = break (== delim) str
--                   in start : if null end then [] else groupBy (tail end) delim

startgpanel :: X ()
startgpanel = do
     gp <- liftIO $ runProcessWithInput  "pidof" ["gnome-panel"] ""
     if (length gp) > 0
      then unsafeSpawn (traceShow gp $ "killall xmobar; killall gnome-panel")
      -- https://askubuntu.com/questions/1097737/gnome-panel-applet-indicator-applet-complete-is-missing-icons/1097759#1097759
      else unsafeSpawn (traceShow gp $ "systemctl --user start indicator-power.service; systemctl --user start indicator-keyboard.service;systemctl --user start indicator-sound.service;systemctl --user start indicator-datetime.service;systemctl --user start indicator-session.service;systemctl --user start indicator-application.service;systemctl --user start indicator-bluetooth.service;systemctl --user start indicator-messages.service;systemctl --user start indicator-printers.service; sleep 0.5; gnome-panel  " )

startmobar :: X ()
startmobar = do
     gp <- liftIO $ runProcessWithInput  "pidof" ["xmobar"] ""
     if (length gp) > 0
      then spawn "killall xmobar"
      else spawn "xmobar"


myRestart = "pkill -KILL xmobar && xmonad --recompile && xmonad --restart"

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --

  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [

    -- Ubuntu keybinding :

-- startdefaultinws

     ((altModMask .|. controlMask, xK_Return), startdefaultinws )

   , ((altModMask .|. controlMask, xK_g), unsafeSpawn myBrowser )


   -- cycle workspaces
   --, ((altModMask .|. controlMask, xK_Left),
   --  prevWS)
   --, ((altModMask .|. controlMask, xK_Right),
   --  nextWS)
   , ((altModMask .|. controlMask, xK_Left),
     WD.moveTo Prev HiddenNonEmptyWS)
   , ((altModMask .|. controlMask, xK_Right),
     WD.moveTo Next HiddenNonEmptyWS)

   -- cycle screen focus
  , ((modMask .|. controlMask , xK_Right),
     nextScreen)
  , ((modMask .|. controlMask , xK_Left),
     prevScreen)



   , ((myModMask .|. shiftMask, xK_Left),
     windowSwap L False)
   , ((myModMask .|. shiftMask, xK_Right),
     windowSwap R False)
   , ((myModMask .|. shiftMask, xK_Up),
     windowSwap U False)
   , ((myModMask .|. shiftMask, xK_Down),
     windowSwap D False)


   -- Directional navigation of windows
   , ((myModMask,                 xK_Right), windowGo R False)
   , ((myModMask,                 xK_Left ), windowGo L False)
   , ((myModMask,                 xK_Up   ), windowGo U False)
   , ((myModMask,                 xK_Down ), windowGo D False)

   , ((myModMask, xK_b), sendMessage ToggleStruts)

   , ((myModMask.|. shiftMask, xK_b), spawn "xdotool windowraise `xdotool search --all --name xmobar`")

   -- sendMessage ToggleStruts -- startmobar
   , ((modMask .|. shiftMask, xK_g), startgpanel)
   --, ((myModMask, xK_g), startgpanel)

-- spawn "gnome-panel"
--   , ((myModMask .|. shiftMask, xK_g), spawn "killall gnome-panel")


-- spawn $ XMonad.terminal conf


  -- Lock the screen using command specified by myScreensaver.
  , ((modMask .|. controlMask, xK_l),
     spawn myScreensaver)

  -- Spawn the launcher using command specified by myLauncher.
  -- Use this to launch programs without a key binding.
  , ((modMask, xK_p),
     spawn myLauncher)

  -- Take a selective screenshot using the command specified by mySelectScreenshot.
  , ((modMask .|. shiftMask, xK_p),
     spawn "sleep 0.5; d=`date +\"%H%M%S\"`;scrot \"/tmp/%Y-%m-%d_${d}_\\$wx\\$h_scrot.png\" -e 'mv $f ~/Pictures/'")

     -- spawn mySelectScreenshot)

  -- Take a full screenshot using the command specified by myScreenshot.
  , ((modMask .|. controlMask .|. shiftMask, xK_p),
     spawn myScreenshot)

  -- Mute volume.
  , ((modMask .|. controlMask, xK_m),
     spawn "amixer -q set Master toggle; amixer -q set Speaker on; amixer -q set Headphone on")

  -- Decrease volume.
  , ((modMask .|. controlMask, xK_j),
     spawn "amixer -q set Master 10%-")

  -- Increase volume.
  , ((modMask .|. controlMask, xK_k),
     spawn "amixer -q set Master 10%+")

  -- , ((modMask, xK_g),           moveTo Next HiddenNonEmptyWS)

#ifdef STACK 
  -- minimize/maximize
  , ((modMask,               xK_BackSpace     ), withFocused minimizeWindow)
  , ((modMask .|. shiftMask, xK_BackSpace     ), withLastMinimized maximizeWindowAndFocus)
#endif
  
  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Close focused window.
  , ((modMask .|. shiftMask, xK_c),
     kill)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
     setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)

  -- Move focus to the next window.
  , ((modMask, xK_Tab),
     windows W.focusDown)

  -- start rofi.
  , ((modMask .|. shiftMask, xK_Tab),
     spawn "rofi -show-icons -show window")
  
  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusUp)

  -- Move focus to the previous window.
  , ((modMask, xK_k),
     windows W.focusDown  )

  -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )

  -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
     windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp    )

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)

  , ((modMask .|. shiftMask .|. altModMask, xK_Left),
     sendMessage Shrink)
  , ((modMask .|. shiftMask .|. altModMask, xK_Right),
     sendMessage Expand)
  , ((modMask .|. shiftMask .|. altModMask, xK_Up),
     sendMessage MirrorExpand)
  , ((modMask .|. shiftMask .|. altModMask, xK_Down),
     sendMessage MirrorShrink)

  -- Switch to Full layout.
  , ((modMask .|. shiftMask, xK_l),
      sendMessage (Toggle "Full") )

  -- Push window back into tiling.
  , ((modMask, xK_t),
     withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
     sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap.
  -- TODO: update this binding with avoidStruts, ((modMask, xK_b),

  -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_q),
     --if os == "freebsd"
       -- then
                io (exitWith ExitSuccess)
       --else spawn "/usr/bin/gnome-session-quit  --logout --no-prompt"
    )

  -- , ((altModMask .|. controlMask, xK_q), io (exitWith ExitSuccess) )


  -- , ((modMask .|. shiftMask, xK_q), spawn "/usr/bin/gnome-session-quit  --logout --no-prompt")
  --     io (exitWith ExitSuccess))


  -- Restart xmonad.
  -- xdotool windowraise `xdotool search --all --name xmobar`
  , ((modMask, xK_q), spawn "xmonad --recompile; xmonad --restart")

     -- spawn "pkill -KILL xmobar && xmonad --recompile && xmonad --restart")
     -- restart "xmonad" True)
     -- spawn myRestart
     -- spawn "pkill -KILL xmobar && xmonad --recompile && xmonad --restart"
     -- restart "xmonad" True
  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--


------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.

myStartupHook = do
   spawn "xdotool windowraise `xdotool search --all --name xmobar`"


------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
-- https://bbs.archlinux.org/viewtopic.php?id=238674
--
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $ defaults { --ewmh def  { -- 
        logHook = ( dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
          , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
          , ppSep = "   "
        } ) <+> ewmhDesktopsLogHook,
        manageHook = manageDocks <+> myManageHook
      , startupHook = startupHook defaults >> setWMName "LG3D"
}

------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = gnomeConfig {- defaultConfig -} {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = avoidStruts $ smartBorders $ myLayout ,
    manageHook         = myManageHook ,

    startupHook        = startupHook gnomeConfig  >> myStartupHook <+> ewmhDesktopsStartup,
   
    handleEventHook    = docksEventHook <+> ewmhDesktopsEventHook <+> minimizeEventHook

}
