-- IMPORTS
import           Control.Arrow                       (first)
import           Data.Char
import qualified Data.Map                            as M
import           Data.Monoid
import           Data.Tuple
import           System.Exit
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Accordion
import           XMonad.Layout.Circle
import           XMonad.Layout.GridVariants          (Grid (Grid))
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LimitWindows          (decreaseLimit,
                                                      increaseLimit,
                                                      limitWindows)
import           XMonad.Layout.MultiToggle           (EOT (EOT), mkToggle,
                                                      single, (??))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed               (Rename (Replace), renamed)
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import qualified XMonad.Layout.ToggleLayouts         as T (ToggleLayout (Toggle),
                                                           toggleLayouts)
import           XMonad.Prompt
import           XMonad.Prompt.Input
import qualified XMonad.StackSet                     as W
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run

--
-- Local vars
--
myWorkspaces :: [String]
myWorkspaces = tots ++ rest
    where tots = ["trm","edt","www","sch","dsc","vid"]
          rest = map show $ [(length tots)+1..9]

alt :: KeyMask
alt = mod1Mask

mod :: KeyMask
mod = mod4Mask

myFont :: String
myFont = "Source Code Pro:size=13"

-- apps
browser :: String
browser = "firefox"

fileman :: String
fileman = "pcmanfm"

term :: String
term = "/home/mcard/.local/share/cargo/bin/alacritty"

-- basic colors
white    = "#ffffff"
altwhite = "#888888"
black    = "#000000"
bg       = "#181818"
altbg    = "#282828"

-- theme colors
princ  = "#cc8c3c"
secon  = "#ffdd33"
focol  = "#8b3622"
active = "#7b4032"
alert  = "#f43841"
cgood  = "#3774b5"

-- Paths
xmobarPath :: String
xmobarPath = "/home/mcard/.config/xmonad/xmobarrc.hs"

--
-- STARTUP
--
startHook :: X ()
startHook = spawn "nm-applet" >> mempty
            -- spawn "~/.bin/openers/ranger.sh" >>
            -- spawn "~/.bin/i3init.sh" >>

--
-- KEYBINDS
--

myKeys conf@XConfig {XMonad.modMask = mod} = M.fromList $
    [ -- Workspace movement
      ((alt, xK_Tab), nextWS)
    , ((alt .|. shf, xK_Tab), prevWS)
    -- focus movement
    , ((mod, xK_h), sendMessage Shrink)
    , ((mod, xK_j), windows W.focusDown)
    , ((mod .|. shf, xK_j), windows W.swapDown)
    , ((mod, xK_k), windows W.focusUp)
    , ((mod .|. shf, xK_k), windows W.swapUp)
    , ((mod, xK_l), sendMessage Expand)
    , ((mod, xK_m), windows W.focusMaster)
    -- misc
    , ((mod, xK_d), withFocused toggleFloat)
    , ((mod, xK_Tab), windows W.focusDown)
    , ((mod .|. shf, xK_Tab), windows W.focusUp)
    -- execs
    , ((mod, xK_o), spawn "emacs")
    , ((mod .|. shf, xK_o), spawn "~/.bin/emacs.sh")
    , ((mod, xK_p), spawn "dmenu_run")
    , ((mod .|. shf, xK_f), spawn $ fileman)
    , ((mod, xK_Return), spawn term)
    , ((mod, xK_z), spawn "~/.bin/i3lock.sh")
    , ((mod, xK_b), spawn $ browser)
    , ((mod, xK_Print), spawn "scrot -s")
    -- window modification
    , ((mod .|. shf, xK_c), kill)
    -- workspace
    , ((mod, xK_space), sendMessage NextLayout)
    , ((mod .|. shf, xK_space), setLayout $ XMonad.layoutHook conf)
    , ((mod, xK_n), refresh)
    , ((mod, xK_t), withFocused $ windows . W.sink)
    , ((mod, xK_comma), sendMessage (IncMasterN 1))
    , ((mod, xK_period), sendMessage (IncMasterN (-1)))
    , ((mod .|. shf, xK_q), io exitSuccess)
    , ((mod, xK_q), spawn "xmonad --recompile; xmonad --restart")

      -- MISC
    , ((0, 0x1008FF11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
    , ((0, 0x1008FF13), spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
    , ((0, 0x1008FF12), spawn "pactl set-sink-mute   @DEFAULT_SINK@ toggle")

      -- Scratchpads
    , ((mod .|. shf, xK_Return), namedScratchpadAction scratchpads "dropterm")
    , ((0, xK_F2), namedScratchpadAction scratchpads "Notepad")
    , ((mod, xK_f), namedScratchpadAction scratchpads "Ranger")
    , ((0, xK_F4), namedScratchpadAction scratchpads "Schedule")
    ]
    ++
    [((mo .|. mod, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, mo) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((mo .|. mod, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, mo) <- [(W.view, 0), (W.shift, shiftMask)]]
    where shf = shiftMask
          toggleFloat w = windows (\s -> if M.member w (W.floating s)
                 then W.sink w s
                 else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))



myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
    [ ((modm, button1),
      \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    , ((modm, button2),
      \w -> focus w >> windows W.shiftMaster)
    , ((modm, button3),
      \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

-- Scratchpads
scratchpads :: [NamedScratchpad]
scratchpads = [
 -- format :: NS <name> <command> <query> <hook>
   NS "dropterm" (term ++ " --class dropterm -t dropterm")
       (appName =? "dropterm")
       (customFloating $ W.RationalRect (1/12) (1/6) (5/6) (2/3))

 , NS "Ranger" (term ++" --class Ranger -t Ranger -e ranger")
       (appName =? "Ranger")
       (customFloating $ W.RationalRect (1/12) (1/6) (5/6) (2/3))

 , NS "Notepad" "emacs -T notepad \
      \--eval='(unless (boundp 'server-process) (server-start))'"
      (title =? "notepad")
      (customFloating  $ W.RationalRect (1/12) (1/6) (5/6) (2/3))

 , NS "Schedule" "feh ~/Pictures/schedule.png --title 'Schedule'"
      (title =? "Schedule")
      (customFloating  $ W.RationalRect (3/14) (1/6) (4/7) (13/20))
 ]



-- Layouts

mySpacing :: Integer -> l a
          -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tiled :: Tall a
tiled = Tall 1 (3/100) (1/2)

floats = renamed [Replace "Float"] $ limitWindows 20 simplestFloat

grid = renamed [Replace "Grid"]
           $ limitWindows 12 $ mySpacing 5 $ mkToggle (single MIRROR) $ Grid (16/10)

monocleBare = noBorders $ monocle

monocle  = renamed [Replace "Monocle"]
           $ limitWindows 20 Full

tabs = renamed [Replace "Tabs"]
       $ tabbed shrinkText $
         def { fontName            = myFont
             , activeColor         = focol
             , inactiveColor       = bg
             , activeBorderColor   = focol
             , inactiveBorderColor = bg
             , activeTextColor     = white
             , inactiveTextColor   = white
             }


layouts = avoidStruts $ onWorkspace "float" simplestFloat $
          grid
   -- ||| Accordion
   -- ||| noBorders Circle
      ||| floats
      ||| noBorders tabs
      ||| monocleBare
   -- ||| tiled
   -- ||| Full


-- Misc.

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Hooks

manHook :: Query (Endo WindowSet)
manHook = composeAll
          [ className =? "MPlayer" --> doFloat
          , className =? "Gimp"    --> doFloat
          , resource =? "desktop_window" --> doIgnore
          , resource =? "kdesktop" --> doIgnore
          , className =? "Test Window" --> doFloat
          , appName =? term  --> doShift (myWorkspaces !! 0) -- Terminal in 1
          , appName =? browser --> doShift (myWorkspaces !! 2) -- browser in 3
          , appName =? fileman  --> doShift (myWorkspaces !! 3) -- File Man. in 4
          , appName =? "vlc"   --> doShift (myWorkspaces !! 4) -- Video in 5
          , appName =? "discord" --> doShift (myWorkspaces !! 5) -- Discord in 6
          , appName =? "lxappearance" --> doShift (myWorkspaces !! 8) <+> doFloat  -- lxappearance in 9
          ]

eveHook :: Event -> X All
eveHook = mempty

lgHook x1 x2 = dynamicLogWithPP xmobarPP
                  { ppOutput = \x -> hPutStrLn x1 x >> hPutStrLn x2 x
                  , ppCurrent = wrap "<" ">"
                  , ppVisible = xmobarColor "#FF9800" ""
                  , ppHidden = wrap "*" "*"
                  , ppHiddenNoWindows = (xmobarColor "#666666" "" .  wrap "[" "]")
                  , ppTitle = xmobarColor "#ffffff" "" . shorten 25
                  , ppSep = "<fc=#666666> | </fc>"
                  , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
                  , ppExtras = [windowCount]
                  , ppOrder = id
                  }

main :: IO ()
main = do
  xmproc0 <- spawnPipe $ "xmobar -x 0 " ++ xmobarPath
  xmproc1 <- spawnPipe $ "xmobar -x 1 " ++ xmobarPath
  xmonad $ docks def {
             -- Basics
               modMask = mod4Mask
             , terminal = term
             , focusFollowsMouse = True
             , clickJustFocuses = False
             , workspaces = myWorkspaces
             , normalBorderColor = bg
             , focusedBorderColor = focol
             , borderWidth = 2

             -- Bindings
             , keys = myKeys
             -- , mouseBindings = myMouseBindings

             -- Hooks and Layouts

             , layoutHook = layouts
             , manageHook = manHook <+> namedScratchpadManageHook scratchpads
             , handleEventHook = eveHook
             , logHook = lgHook xmproc0 xmproc1
             , startupHook = startHook
         }

--EOF
