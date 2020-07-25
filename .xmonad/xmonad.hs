-- IMPORTS
import XMonad
import Data.Monoid
import System.Exit
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run

import qualified XMonad.StackSet as W
import qualified Data.Map as M

xmobarPath = "/home/mcard/.xmonad/xmobarrc.hs"

myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_p      ), spawn "dmenu_run -fn 'Hasklig-13'")
    , ((modm,               xK_o      ), spawn "emacs --with-profile default")
    , ((modm,               xK_i      ), spawn "emacs --with-profile doom")
    , ((modm .|. shiftMask, xK_x      ), spawn "emacs /home/mcard/.xmonad/xmonad.hs") -- Open this file
    , ((modm .|. shiftMask, xK_o      ), spawn "emacs /home/mcard/regmacs/.emacs.d/init.el") -- Open default init file
    , ((modm .|. shiftMask, xK_i      ), spawn "emacs --with-profile doom /home/mcard/doomacs/.emacs.d/init.el") -- Open doom init file
    , ((modm,               xK_b      ), spawn "google-chrome")
    , ((modm .|. shiftMask, xK_equal  ), spawn "killall xmobar")
    , ((modm,               xK_f      ), spawn "nemo")
    , ((modm,               xK_s      ), spawn "cinnamon-settings")
    , ((modm .|. shiftMask, xK_n      ), spawn "nitrogen --restore")
    , ((modm .|. shiftMask, xK_m      ), spawn "lxappearance")
    , ((0                 , 0x1008FF11), spawn "/home/mcard/.bin/changevol.sh -10%") -- Vol down
    , ((0                 , 0x1008FF13), spawn "/home/mcard/.bin/changevol.sh +10%") -- Vol up
    , ((0                 , 0x1008FF12), spawn "/home/mcard/.bin/mutevol.sh")        -- Vol mute
    , ((modm .|. shiftMask, xK_c      ), kill)
    , ((modm,               xK_space  ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space  ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n      ), refresh)
    , ((modm,               xK_Tab    ), windows W.focusDown)
    , ((modm,               xK_j      ), windows W.focusDown)
    , ((modm,               xK_k      ), windows W.focusUp  )
    , ((modm,               xK_m      ), windows W.focusMaster)
    , ((modm,               xK_Return ), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j      ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k      ), windows W.swapUp    )
    , ((modm,               xK_h      ), sendMessage Shrink)
    , ((modm,               xK_l      ), sendMessage Expand)
    , ((modm,               xK_t      ), withFocused $ windows . W.sink)
    , ((modm              , xK_comma  ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period ), sendMessage (IncMasterN (-1)))
    , ((modm .|. shiftMask, xK_q      ), io exitSuccess)
    , ((modm              , xK_q      ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm .|. shiftMask, xK_slash  ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

myWorkspaces :: [String]
myWorkspaces = ["trm","edt","www","sys","msc"] ++ map show [6..9]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Hooks
layHook = avoidStruts (tiled ||| Full)
    where tiled = Tall nmaster delta ratio
          nmaster = 1 -- Num of windows in master
          ratio = 1/2 -- How much of screen is master
          delta = 3/100 -- Percent increment when adjusting size

manHook = composeAll
          [ className =? "MPlayer" --> doFloat
          , className =? "Gimp"    --> doFloat
          , resource  =? "desktop_window" --> doIgnore
          , resource  =? "kdesktop" --> doIgnore ]

eveHook = mempty

lgHook x1 x2 = dynamicLogWithPP xmobarPP
                  { ppOutput = \x -> hPutStrLn x1 x >> hPutStrLn x2 x
                  , ppCurrent = wrap "<" ">"
                  , ppVisible = xmobarColor "#FF9800" ""
                  , ppHidden = wrap "*" "*"
                  , ppHiddenNoWindows = xmobarColor "#666666" ""
                  , ppTitle = xmobarColor "#ffffff" "" . shorten 25
                  , ppSep = "<fc=#666666> | </fc>"
                  , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
                  , ppExtras = [windowCount]
                  , ppOrder = id
                  }

startHook = do spawn "/home/mcard/.bin/.xmonad-session-rc"

main = do
  xmproc0 <- spawnPipe $ "xmobar -x 0 " ++ xmobarPath
  xmproc1 <- spawnPipe $ "xmobar -x 1 " ++ xmobarPath
  xmonad $ docks def {
             -- Basics
               modMask = mod4Mask
             , terminal = "gnome-terminal"
             , focusFollowsMouse = True
             , clickJustFocuses = False
             , workspaces = myWorkspaces
             , normalBorderColor = "#dddddd"
             , focusedBorderColor = "#FF0000"
             , borderWidth = 2

             -- Bindings
             , keys = myKeys
             , mouseBindings = myMouseBindings

             -- Hooks and Layouts

             , layoutHook = layHook
             , manageHook = manHook
             , handleEventHook = eveHook
             , logHook = lgHook xmproc0 xmproc1
             , startupHook = startHook
         }

help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:", "", "-- launching and killing programs", "mod-Shift-Enter  Launch xterminal", "mod-p            Launch dmenu", "mod-Shift-p      Launch gmrun", "mod-Shift-c      Close/kill the focused window","mod-Space        Rotate through the available layout algorithms","mod-Shift-Space  Reset the layouts on the current workSpace to default", "mod-n            Resize/refresh viewed windows to the correct size", "","-- move focus up or down the window stack","mod-Tab        Move focus to the next window", "mod-Shift-Tab  Move focus to the previous window","mod-j          Move focus to the next window","mod-k          Move focus to the previous window","mod-m          Move focus to the master window","","-- modifying the window order","mod-Return   Swap the focused window and the master window","mod-Shift-j  Swap the focused window with the next window","mod-Shift-k  Swap the focused window with the previous window","","-- resizing the master/slave ratio","mod-h  Shrink the master area","mod-l  Expand the master area","","-- floating layer support","mod-t  Push window back into tiling; unfloat and re-tile it","","-- increase or decrease number of windows in the master area","mod-comma  (mod-,)   Increment the number of windows in the master area","mod-period (mod-.)   Deincrement the number of windows in the master area","","-- quit, or restart","mod-Shift-q  Quit xmonad","mod-q        Restart xmonad","mod-[1..9]   Switch to workSpace N","","-- Workspaces & screens","mod-Shift-[1..9]   Move client to workspace N","mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3","mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3","","-- Mouse bindings: default actions bound to mouse events","mod-button1  Set the window to floating mode and move by dragging","mod-button2  Raise the window to the top of the stack","mod-button3  Set the window to floating mode and resize by dragging"]
