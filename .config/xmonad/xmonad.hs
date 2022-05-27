-- IMPORTS
import           Control.Arrow (first)
import           Data.Char
import qualified Data.Map as M
import           Data.Monoid
import           Data.Tuple
import           System.Exit
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.FloatKeys
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Layout.Accordion
import           XMonad.Layout.Circle
import           XMonad.Layout.GridVariants (Grid (Grid))
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LimitWindows          (decreaseLimit,
                                                      increaseLimit,
                                                      limitWindows)
import           XMonad.Layout.MultiToggle           (EOT (EOT), mkToggle,
                                                      single, (??))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed (Rename (Replace), renamed)
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import qualified XMonad.Layout.ToggleLayouts         as T (ToggleLayout (Toggle),
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
myWS = tots ++ rest ++ ["NSP"]
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
term = "kitty"

dmenuOpts :: Bool -> String
dmenuOpts gay = if gay then " -fc #ffdd33" else ""

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
active = "#7b4032"
inactive = bg
alert  = "#f43841"
cgood  = "#3774b5"

-- gay gay gay colors
gaypink1 = "#C479A2"
gaypink2 = "#EDA5CD"
gaypurpl = "#D6C7E8"
gaywhite = "#FFFFFF"
gayltblu = "#9AC7E8"
gaydrblu = "#6D82D1"
gayfocol  = "#f7a8b8"
gaysecon  = "#f37d95"
gayblue = "#55cdfc"
gayinactive = "#55cdfc"

-- Paths
xmobarPath :: String
xmobarPath = "/home/mcard/.config/xmonad/xmobarrc.hs"

gayxmobarPath :: String
gayxmobarPath = "/home/mcard/.config/xmonad/gayxmobarrc.hs"

dec :: Num a => a
dec = 10

l = (-dec,   0)
d = (   0, dec)
u = (   0,-dec)
r = ( dec,   0)

--
-- STARTUP
--
startHook :: X ()
startHook = spawn "nm-applet"
            -- >> spawn "~/.bin/i3init.sh"

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
    --- Resize in tiled mode
    , ((mod, xK_h), sendMessage Shrink)
    , ((mod, xK_l), sendMessage Expand)
    --- Move floating windows
    , ((mod, xK_n),      withFocused $ keysMoveWindow l) -- left
    , ((mod, xK_m),      withFocused $ keysMoveWindow d) -- down
    , ((mod, xK_comma),  withFocused $ keysMoveWindow u) -- up
    , ((mod, xK_period), withFocused $ keysMoveWindow r) -- right
    --- Resize Floating Windows
    , ((mod .|. shf, xK_n),      withFocused $ keysResizeWindow l (0,0))
    , ((mod .|. shf, xK_m),      withFocused $ keysResizeWindow d (0,0))
    , ((mod .|. shf, xK_comma),  withFocused $ keysResizeWindow u (0,0))
    , ((mod .|. shf, xK_period), withFocused $ keysResizeWindow r (0,0))
    --- Kill window
    , ((mod .|. shf, xK_c), kill)
    , ((mod .|. shf, xK_p), spawn "~/.bin/truekill.sh")
    --- Change WS layout
    , ((mod, xK_space), sendMessage NextLayout)
    , ((mod .|. shf, xK_space), setLayout $ XMonad.layoutHook conf)
    , ((mod, xK_t), withFocused $ windows . W.sink) -- set window to non-floating
    -- execs
    --- apps
    , ((mod, xK_o), spawn "emacs")
    , ((mod, xK_v), spawn "st -e vim")
    , ((mod, xK_p), spawn "dmenu_run")
    , ((mod .|. shf, xK_f), spawn $ fileman)
    , ((mod, xK_Return), spawn term)
    , ((mod, xK_b), spawn $ browser)
    , ((mod, xK_Print), spawn "scrot -s")
    --- scripts
    , ((mod .|. shf, xK_o), spawn "~/.bin/emacs.sh")
    , ((mod, xK_z), spawn "~/.bin/i3lock.sh")
    , ((mod .|. shf, xK_b), spawn "~/.bin/books.sh")
    -- Exit, recompule, etc
    , ((mod .|. shf, xK_q), io exitSuccess)
    , ((mod, xK_q), spawn "xmonad --recompile; xmonad --restart")
    -- MISC
    , ((0, 0x1008FF11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ((0, 0x1008FF13), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ((0, 0x1008FF12), spawn "pactl set-sink-mute   @DEFAULT_SINK@ toggle")
    , ((0, xK_F12), spawn "~/.bin/egg.sh")

      -- Scratchpads
    , ((mod .|. shf, xK_Return), namedScratchpadAction scratchpads "dropterm")
    , ((mod, xK_f), namedScratchpadAction scratchpads "Ranger")
    , ((0, xK_F2), namedScratchpadAction scratchpads "Notepad")
    , ((0, xK_F4), namedScratchpadAction scratchpads "Schedule")
    , ((0, xK_F5), namedScratchpadAction scratchpads "vimwindow")
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
-- scratchpads = []
scratchpads = [
  -- format :: NS <name> <command> <query> <hook>
    NS "dropte1rm" (term ++ " --class dropterm --title dropterm")
       (className =? "dropterm")
       (customFloating $ W.RationalRect (1/4) (1/6) (1/2) (2/3))

  , NS "Ranger" (term ++ " --class Ranger --title Ranger -e ranger")
       (className =? "Ranger")
       (customFloating $ W.RationalRect (1/4) (1/6) (1/2) (2/3))

  , NS "Notepad" "emacs -T notepad \
                 \ --eval='(unless (boundp 'server-process) (server-start))'"
       (title =? "notepad")
       (customFloating $ W.RationalRect (1/12) (1/6) (5/6) (2/3))

  , NS "Schedule" "feh ~/Pictures/schedule.png --title 'Schedule'"
       (title =? "Schedule")
       (customFloating $ W.RationalRect (1/4) (1/6) (1/2) (2/3))

  , NS "vimwindow" (term ++ " --class vim -e vim")
       (className =? "vim")
       (customFloating $ W.RationalRect (1/4) (1/6) (1/2) (2/3))
  ]



-- Layouts

mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tiled = Tall 1 (3/100) (1/2)

--floats = renamed [Replace "Float"] $ limitWindows 20 simplestFloat

grid = renamed [Replace "Grid"] $ limitWindows 12 $
       mySpacing 5 $ mkToggle (single MIRROR) $ Grid (16/10)

tabConfig = def { fontName            = "Source_Code_Pro"
                , activeColor         = focol
                , inactiveColor       = inactive
                , activeBorderColor   = focol
                , inactiveBorderColor = inactive
                , activeTextColor     = white
                , inactiveTextColor   = white
                }

layouts =  onWorkspace "sch" simplestFloat $
           (as grid) ||| noBorders Full -- ||| noBorders tabs ||| floats
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
          , resource  =? "desktop_window" --> doIgnore
          , resource  =? "kdesktop"       --> doIgnore
          , appName   =? browser          --> doShift (myWS !! 2)
          , appName   =? fileman          --> doShift (myWS !! 3)
          , appName   =? "discord"        --> doShift (myWS !! 4)
          -- , appName   =? "vlc"            --> doShift (myWS !! 5)
          , className =? "Ranger"         --> doShift "NS"
          , className =? "dropterm"       --> doShift "NS"
          ]

eveHook :: Event -> X All
eveHook = mempty

lgHook c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 x1 = dynamicLogWithPP xmobarPP
                  { ppOutput  = hPutStrLn x1
                  , ppCurrent = xmobarColor c1 c2 . sp
                  , ppVisible = xmobarColor c3 c4
                  , ppHidden  = xmobarColor c5 c6 . sp
                  , ppHiddenNoWindows = xmobarColor c7 c8 . sp
                  , ppTitle = xmobarColor c9 "" . shorten 25
                  , ppSep = " | "
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

regLogHook = lgHook "#666666" white focol bg active altwhite active altwhite blue white 

gayLogHook = lgHook "#000000" altbg gaypink2 altbg gaypink1 altbg gaypurpl altbg gayltblu bg

main :: IO ()
main = do
  xmproc <- spawnPipe $ "xmobar -x 0 " ++ if gay then gayxmobarPath else xmobarPath
  xmonad $ docks def {
             -- Basics
               modMask = mod4Mask
             , terminal = term
             , focusFollowsMouse = True
             , clickJustFocuses = False
             , workspaces = myWS
             , normalBorderColor = inactive
             , focusedBorderColor = if gay then gayfocol else focol
             , borderWidth = 2
             -- Bindings
             , keys = myKeys
             , mouseBindings = myMouseBindings
             -- Hooks and Layouts
             , layoutHook = layouts 
             , manageHook = manHook <+> (namedScratchpadManageHook scratchpads)
             , handleEventHook = eveHook
             , logHook = ifGayLogHook xmproc 
             , startupHook = startHook
         }
      where gay = False
            ifGayLogHook = if gay then gayLogHook else regLogHook
         
--EOF
