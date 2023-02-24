-- IMPORTS
import           Control.Arrow (first)
import           Data.Char
import qualified Data.Map as M
import           Data.Monoid
import           Data.Tuple
import           GHC.IO.Handle.Types
import           System.Exit
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.FloatKeys
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Accordion
import           XMonad.Layout.Circle
import           XMonad.Layout.GridVariants (Grid (Grid))
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LimitWindows
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
    (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed (Rename (Replace), renamed)
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import qualified XMonad.Layout.ToggleLayouts as T (ToggleLayout (Toggle),
                                                                toggleLayouts)
import           XMonad.Prompt
import           XMonad.Prompt.Input
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import           XMonad.Util.WorkspaceCompare

--
-- Local vars
--
myWS :: [String]
myWS = tots ++ rest 
    where tots = ["trm","edt","www","sch","dsc","vid"]
          rest = map show $ [(length tots)+1..9]

alt :: KeyMask
alt = mod1Mask

mod :: KeyMask
mod = mod4Mask

myFont :: String
myFont = "Source Code Pro:size=13"

type GridType = ModifiedLayout Rename (ModifiedLayout LimitWindows (ModifiedLayout Spacing (MultiToggle (HCons StdTransformers EOT) Grid)))

-- apps
browser :: String
browser = "chromium"

fileman :: String
fileman = "pcmanfm"

term :: String
term = "kitty"

-- basic colors
white    = "#ffffff"
altwhite = "#888888"
black    = "#000000"
bg       = "#181818"
altbg    = "#282828"

-- theme colors
-- 'gruber' colors
princ  = "#cc8c3c"
secon  = "#ffdd33"
focol  = "#8b3622"
blue = altbg
vis    = "#ff56cb"
active = "#7b4032"
inactive = bg
alert  = "#f43841"
cgood  = "#3774b5"

-- Paths
xmobarPath :: String
xmobarPath = "/home/mcard/.config/xmonad/xmobarrc.hs"

dec :: Num a => a
dec = 10

inc :: Num a => a
inc = -10

l :: ChangeDim
l = (inc,   0)
d :: ChangeDim
d = (  0, dec)
u :: ChangeDim
u = (  0, inc)
r :: ChangeDim
r = (dec,   0)

non :: G
non = (0,0)

--
-- STARTUP
--
startHook :: X ()
startHook = do
  spawn "nm-applet"
  spawn "picom"
  -- spawn "/home/mcard/.bin/cisbg.sh"
  
--
-- KEYBINDS
--
myKeys conf@XConfig {XMonad.modMask = mod} = M.fromList $
    [
    -- Movement & General WM stuff 
    --- Workspace movement
      ((alt, xK_Tab), nextWS)
    , ((alt .|. shf, xK_Tab), prevWS)
    
    --- Focus movement
    , ((mod, xK_j), windows W.focusDown)
    , ((mod .|. shf, xK_j), windows W.swapDown)
    , ((mod, xK_k), windows W.focusUp)
    , ((mod .|. shf, xK_k), windows W.swapUp)
    , ((mod, xK_d), withFocused toggleFloat)
    , ((mod, xK_Tab), windows W.focusDown)
    , ((mod .|. shf, xK_Tab), windows W.focusUp)
    , ((mod, xK_l), windows W.focusUp)
    , ((mod, xK_h), windows W.focusDown)
      
    --- Move floating windows
    , ((mod, xK_n),      withFocused $ keysMoveWindow l) -- left
    , ((mod, xK_m),      withFocused $ keysMoveWindow d) -- down
    , ((mod, xK_comma),  withFocused $ keysMoveWindow u) -- up
    , ((mod, xK_period), withFocused $ keysMoveWindow r) -- right
    
    --- Resize Floating Windows
    , ((mod .|. shf, xK_n),      withFocused $ keysResizeWindow l non)
    , ((mod .|. shf, xK_m),      withFocused $ keysResizeWindow d non)
    , ((mod .|. shf, xK_comma),  withFocused $ keysResizeWindow u non)
    , ((mod .|. shf, xK_period), withFocused $ keysResizeWindow r non)
      
    --- Kill window
    , ((mod .|. shf, xK_c), kill)
    , ((mod .|. shf, xK_p), spawn "~/.bin/truekill.sh")
      
    --- Change WS layout
    , ((mod, xK_space), sendMessage NextLayout)
    , ((mod .|. shf, xK_space), setLayout $ XMonad.layoutHook conf)
    , ((mod, xK_t), withFocused $ windows . W.sink) -- set window to non-floating

    -- execs
    , ((mod, xK_b), spawn $ browser)
    , ((mod, xK_i), spawn "thunderbird")
    , ((mod, xK_o), spawn "emacs-29.0.60")
    , ((mod, xK_p), spawn "dmenu_run")
    , ((mod, xK_z), spawn "~/.bin/i3lock.sh")
    , ((mod, xK_Print), spawn "scrot -s")
    , ((mod, xK_Return), spawn term)
    , ((mod .|. shf, xK_f), spawn $ fileman)
    -- Exit, recompule, etc
    , ((mod .|. shf, xK_q), io exitSuccess)
    , ((mod, xK_q), spawn "xmonad --recompile && xmonad --restart")
    -- MISC
    , ((0, 0x1008FF11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ((0, 0x1008FF13), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ((0, 0x1008FF12), spawn "pactl set-sink-mute   @DEFAULT_SINK@ toggle")

      -- Scratchpads
    , ((mod .|. shf, xK_b), namedScratchpadAction scratchpads "Books")
    , ((mod .|. shf, xK_Return), namedScratchpadAction scratchpads "dropterm")
    , ((mod, xK_f), namedScratchpadAction scratchpads "Ranger")
    , ((mod, xK_v), namedScratchpadAction scratchpads "Discord")
    , ((mod, xK_c), namedScratchpadAction scratchpads "Slack")
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


myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
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
    NS "dropterm" (term ++ " --class dropterm --title dropterm")
       (className =? "dropterm")
       (customFloating $ W.RationalRect (1/4) (1/6) (1/2) (2/3))

  , NS "Ranger" (term ++ " --class Ranger --title Ranger -e ranger")
       (className =? "Ranger")
       (customFloating $ W.RationalRect (1/4) (1/6) (1/2) (2/3))

  , NS "Books" (term ++ " --class Books --title Books -e ranger --cmd 'set column_ratios 0' ~/school/.misc/Books")
       (className =? "Books")
       (customFloating $ W.RationalRect (1/4) (1/6) (1/2) (2/3))

  , NS "Notepad" "emacs -T notepad \
                 \ --eval='(unless (boundp 'server-process) (server-start))'"
       (title =? "notepad")
       (customFloating $ W.RationalRect (1/12) (1/6) (5/6) (2/3))
       
  , NS "Discord" "discord-canary"
       (className =? "discord")
       (customFloating $ W.RationalRect (1/4) (1/6) (1/2) (2/3))

  , NS "Slack" "slack"
       (className =? "Slack")
       (customFloating $ W.RationalRect (1/4) (1/6) (1/2) (2/3))
  ]

-- Layouts
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tiled :: ModifiedLayout Rename Tall a
tiled = renamed [Replace "Tile"] $ Tall 1 (3/100) (1/2)

grid :: GridType a
grid = renamed [Replace "Grid"] $ limitWindows 12 $
       mySpacing 5 $ mkToggle (single MIRROR) $ Grid (16/10)

tabConfig :: Theme
tabConfig = def { fontName            = "xft:Source Code Pro"
                , activeColor         = focol
                , inactiveColor       = inactive
                , activeBorderColor   = focol
                , inactiveBorderColor = inactive
                , activeTextColor     = white
                , inactiveTextColor   = white
                , activeBorderWidth = 10
                , inactiveBorderWidth = 10
                }

layouts = as (grid ||| tiled) ||| noBorders Full
    where as = avoidStruts

-- Misc.
windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Hooks
manHook :: Query (Endo WindowSet)
manHook = composeAll $
          [ className =? "Gimp"           --> doFloat
          , className =? "Test Window"    --> doFloat
          , className =? "Matplotlib"     --> doFloat
          , className =? "tk"             --> doFloat
          , className =? "Tk"             --> doFloat
          , resource  =? "desktop_window" --> doIgnore
          , resource  =? "kdesktop"       --> doIgnore
          , appName   =? browser          --> doShift (myWS !! 2)
          , appName   =? fileman          --> doShift (myWS !! 3)
          , className =? "discord"        --> doFloat
          , className =? "slack"          --> doFloat
          , className =? "Ranger"         --> doShift "NS"
          , className =? "dropterm"       --> doShift "NS"
          , className =? "discord"        --> doShift "NS"
          , className =? "slack"          --> doShift "NS"
          ]

eveHook :: Event -> X All
eveHook = mempty

lgHook :: String -> String -> String -> String -> String -> String
       -> String -> String -> String -> Handle -> Handle -> X()
lgHook c1 c2 c3 c4 c5 c6 c7 c8 c9 x1 x2
    = dynamicLogWithPP xmobarPP
      { ppOutput  = \x -> hPutStrLn x1 x
                       >> hPutStrLn x2 x
      , ppCurrent = xmobarColor c1 c2 . sp
      , ppVisible = xmobarColor c3 c4 . sp
      , ppHidden  = xmobarColor c5 c6 . sp
      , ppHiddenNoWindows = xmobarColor c7 c8 . sp
      , ppTitle = xmobarColor c9 "" . shorten 25
      , ppSep = "<fc=#666666> | </fc>"
      , ppWsSep = ""
      , ppUrgent = xmobarColor white alert . sp
      , ppExtras = [windowCount]
      , ppSort = (mkWsSort getWsCompare')
      , ppOrder = id
      }
    where sp = wrap " " " "
          getWsCompare' :: X WorkspaceCompare
          getWsCompare' = do
            wsIndex <- getWsIndex
            return $ \a b -> f (wsIndex a) (wsIndex b) `mappend` compare a b
                where
                  f Nothing Nothing   = EQ
                  f (Just _) Nothing  = LT
                  f Nothing (Just _)  = GT
                  f (Just x) (Just y) = compare x y

regLogHook :: Handle -> Handle -> X()
regLogHook = lgHook white focol altwhite vis altwhite active altwhite blue white 

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
             , workspaces = myWS
             , normalBorderColor = inactive
             , focusedBorderColor = focol
             , borderWidth = 4
             -- Bindings
             , keys = myKeys
             , mouseBindings = myMouseBindings
             -- Hooks and Layouts
             , layoutHook = layouts 
             , manageHook = manHook <+> (namedScratchpadManageHook scratchpads)
             , handleEventHook = eveHook
             , logHook = regLogHook xmproc0 xmproc1
             , startupHook = startHook
         }
         
--EOF
