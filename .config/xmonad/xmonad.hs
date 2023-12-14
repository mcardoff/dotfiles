-- IMPORTS
import qualified Data.Map as M
import           Data.Monoid
import           GHC.IO.Handle.Types
import           System.Exit
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.FloatKeys
import           XMonad.Actions.GridSelect

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

windowCount :: String -> X (Maybe String)
windowCount str = gets $ Just . (xmobarColor black str) . wrap " " " " . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

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

-- theme colors
-- 'gruber' colors
gruberBg = "#181818"
gruberBg1 = "#282828"
gruberBg2 = "#585858"
gruberFg = "#ffffff"
gruberFg1 = "#888888"
gruberCyan = "#70c0b1"
gruberGreen = "#73c936"
gruberYellow = "#ffdd33"
gruberBrown = "#cc8c3c"
gruberQuartz = "#95a99f"
gruberNiagara = "#96a6c8"
gruberWisteria = "#9e95c7"
gruberDarkRed = "#7b4032"
gruberDarkRed1 = "#8b3622"
gruberDarkRed2 = "#8b2222"
gruberRed  = "#f43841"

-- Paths
xmobarPath :: String
xmobarPath = "/home/mcard/.config/xmonad/xmobarrc.hs"

--
-- STARTUP 
--
startup :: X ()
startup = do
  spawn "nm-applet" -- Network Manager
  spawn "picom" -- compositor
  spawn "twmnd" -- notification daemon
  spawn "/home/mcard/.local/scripts/cisbg.sh" -- background
  
--
-- KEYBINDS
--
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
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
    , ((mod, xK_Tab), windows W.focusDown)
    , ((mod .|. shf, xK_Tab), windows W.focusUp)
    , ((mod, xK_l), windows W.focusUp)
    , ((mod, xK_h), windows W.focusDown)
    --- Move floating windows
    , ((mod, xK_d), withFocused toggleFloat) -- toggle floating
    , ((mod, xK_n),      withFocused $ keysMoveWindow l) -- left
    , ((mod, xK_m),      withFocused $ keysMoveWindow d) -- down
    , ((mod, xK_comma),  withFocused $ keysMoveWindow u) -- up
    , ((mod, xK_period), withFocused $ keysMoveWindow r) -- right
    --- Resize Floating Windows
    , ((mod .|. shf, xK_n),      withFocused $ keysResizeWindow l non)
    , ((mod .|. shf, xK_m),      withFocused $ keysResizeWindow d non)
    , ((mod .|. shf, xK_comma),  withFocused $ keysResizeWindow u non)
    , ((mod .|. shf, xK_period), withFocused $ keysResizeWindow r non)
    , ((mod, xK_Left),  withFocused $ keysResizeWindow l non)
    , ((mod, xK_Down),  withFocused $ keysResizeWindow d non)
    , ((mod, xK_Up),    withFocused $ keysResizeWindow u non)
    , ((mod, xK_Right), withFocused $ keysResizeWindow r non)
    --- Kill window
    , ((mod .|. shf, xK_c), kill)
    , ((mod .|. shf, xK_p), spawn "~/.local/scripts/truekill.sh")      
    --- Change WS layout
    , ((mod, xK_space), sendMessage NextLayout)
    , ((mod .|. shf, xK_space), setLayout $ XMonad.layoutHook conf)
    -- execs
    , ((mod, xK_b), spawn $ browser)
    , ((mod, xK_o), spawn "emacs-29.0.60")
    , ((mod, xK_p), spawn "dmenu_run")
    , ((mod, xK_z), spawn "~/.local/scripts/i3lock.sh")
    , ((mod, xK_Insert), spawn "~/.local/scripts/secret.sh")
    , ((mod, xK_Print), spawn "scrot -s")
    , ((mod, xK_Return), spawn term)
    -- , ((mod .|. shf, xK_f), spawn $ fileman)
    -- Exit, recompile, etc
    , ((mod .|. shf, xK_q), io exitSuccess)
    , ((mod, xK_q), spawn "xmonad --recompile && xmonad --restart")
    -- MISC
    , ((0, 0x1008FF11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ((0, 0x1008FF13), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ((0, 0x1008FF12), spawn "pactl set-sink-mute   @DEFAULT_SINK@ toggle")
    , ((mod, 0xff50), spawn "xrandr --auto")
    -- gridselect
    ,  ((mod, xK_c), runSelectedAction gridConfig chatAppsGrid)
    -- Scratchpads
    , ((mod .|. shf, xK_o), namedScratchpadAction scratchpads "Notepad")
    , ((mod .|. shf, xK_Return), namedScratchpadAction scratchpads "dropterm")
    , ((mod, xK_f), namedScratchpadAction scratchpads "Ranger")
    , ((mod, xK_s), namedScratchpadAction scratchpads "Books")
    -- , ((mod, xK_t), namedScratchpadAction scratchpads "Mattermost")
    -- , ((mod, xK_y), namedScratchpadAction scratchpads "Discord")
    -- , ((mod, xK_u), namedScratchpadAction scratchpads "Slack")
    -- , ((mod, xK_i), namedScratchpadAction scratchpads "Skype")
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
          alt = mod1Mask
          toggleFloat w = windows (\s -> if M.member w (W.floating s)
                 then W.sink w s
                 else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))
          dec = 10
          inc = -dec
          l = (inc,   0)
          d = (  0, dec)
          u = (  0, inc)
          r = (dec,   0)
          non = (0,0)


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
  -- format for W.RationalRect ((1-w) / 2) ((1-h)/2) w h
    NS "dropterm" (term ++ " --class dropterm --title dropterm")
       (className =? "dropterm")
       (customFloating $ easyrr (2/3) (2/3))
  , NS "Ranger" (term ++ " --class Ranger --title Ranger -e ranger")
       (className =? "Ranger")
       (customFloating $ easyrr (2/3) (2/3))
  , NS "Books" (term ++ " --class Books --title Books -e ranger --cmd 'set column_ratios 0' ~/school/.misc/Books")
       (className =? "Books")
       (customFloating $ easyrr (5/6) (2/3))
  , NS "Notepad" "emacs-29.0.60 -T notepad --eval=\"(unless (boundp 'server-process) (server-start))\""
       (title =? "notepad")
       (customFloating $ easyrr (5/6) (2/3))
  , NS "Discord" "discord-canary"
       (className =? "discord")
       (customFloating $ easyrr (1/2) (2/3))
  , NS "Slack" "slack"
       (className =? "Slack")
       (customFloating $ easyrr (5/6) (2/3))       
  , NS "Mattermost" "mattermost-desktop"
       (className =? "Mattermost")
       (customFloating $ easyrr (5/6) (2/3))
  , NS "Skype" "skypeforlinux"
       (className =? "Skype")
       (customFloating $ easyrr (5/6) (2/3))
  ]
    where easyrr w h = W.RationalRect ((1-w)/2) ((1-h)/2) w h

-- GridSelect
gridNav :: TwoD a (Maybe a)
gridNav = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
 where navKeyMap = M.fromList [
          ((0,xK_Escape), cancel)
         ,((0,xK_Return), select)
         ,((0,xK_slash) , substringSearch gridNav)
         ,((0,xK_Left)  , move (-1,0)  >> gridNav)
         ,((0,xK_h)     , move (-1,0)  >> gridNav)
         ,((0,xK_Right) , move (1,0)   >> gridNav)
         ,((0,xK_l)     , move (1,0)   >> gridNav)
         ,((0,xK_Down)  , move (0,1)   >> gridNav)
         ,((0,xK_j)     , move (0,1)   >> gridNav)
         ,((0,xK_Up)    , move (0,-1)  >> gridNav)
         ,((0,xK_k)     , move (0,-1)  >> gridNav)
         ,((0,xK_y)     , move (-1,-1) >> gridNav)
         ,((0,xK_i)     , move (1,-1)  >> gridNav)
         ,((0,xK_n)     , move (-1,1)  >> gridNav)
         ,((0,xK_m)     , move (1,-1)  >> gridNav)
         ,((0,xK_space) , setPos (0,0) >> gridNav)
         ]
       -- The navigation handler ignores unknown key symbols
       navDefaultHandler = const gridNav

-- gridSelect menu layout
gridConfig :: GSConfig (X())
gridConfig = def
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_navigate    = gridNav
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = "xft:Source Code Pro"
    }

-- list of chat apps to open in grid:
chatAppsGrid :: [(String, X ())]
chatAppsGrid = map (\ s-> (s, namedScratchpadAction scratchpads s)) $
               ["Slack", "Discord", "Skype", "Mattermost"]



-- Layouts
tiled :: ModifiedLayout Rename Tall a
tiled = renamed [Replace "Tile"] $ Tall 1 (3/100) (1/2)

type GridType = ModifiedLayout Rename (ModifiedLayout LimitWindows (ModifiedLayout Spacing (MultiToggle (HCons StdTransformers EOT) Grid)))
grid :: GridType a
grid = renamed [Replace "Grid"] $ limitWindows 12 $
       mySpacing 5 $ mkToggle (single MIRROR) $ Grid (16/10)
           where mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- tabConfig :: Theme
-- tabConfig = def { fontName            = "xft:Source Code Pro"
--                 , activeColor         = gruberDarkRed1
--                 , inactiveColor       = gruberBg
--                 , activeBorderColor   = gruberDarkRed1
--                 , inactiveBorderColor = gruberBg
--                 , activeTextColor     = white
--                 , inactiveTextColor   = white
--                 , activeBorderWidth = 10
--                 , inactiveBorderWidth = 10
--                 }

layouts = as (grid ||| tiled) ||| noBorders Full
    where as = avoidStruts

-- Hooks
manageHook' :: Query (Endo WindowSet)
manageHook' = composeAll $
          [ className =? "Gimp"           --> doFloat
          , className =? "Test Window"    --> doFloat
          , className =? "Matplotlib"     --> doFloat
          , className =? "tk"             --> doFloat
          , className =? "Tk"             --> doFloat
          , className =? "slack"          --> doFloat <+> (doShift "NS")
          , className =? "Mattermost"     --> doFloat <+> (doShift "NS")
          , className =? "discord"        --> doFloat <+> (doShift "NS")
          , className =? "Ranger"         --> doShift "NS"
          , className =? "dropterm"       --> doShift "NS"
          , appName   =? fileman          --> doShift (myWS !! 3)
          , appName   =? browser          --> doShift (myWS !! 2)
          , resource  =? "desktop_window" --> doIgnore
          , resource  =? "kdesktop"       --> doIgnore
          ]

scratchpadManageHook :: ManageHook
scratchpadManageHook = namedScratchpadManageHook scratchpads

logHookDef :: String -> String -> String -> String -> String -> String 
           -> String -> String -> String -> String -> Handle -> Handle -> X()
logHookDef c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 x1 x2
    = dynamicLogWithPP $ filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP
      { ppOutput  = \x -> hPutStrLn x1 x >> hPutStrLn x2 x
      , ppCurrent = xmobarColor c1 c2 . sp
      , ppVisible = xmobarColor c3 c4 . sp
      , ppHidden  = xmobarColor c5 c6 . sp
      , ppHiddenNoWindows = xmobarColor c7 c8 . sp
      , ppTitle = xmobarColor black c9 . sp . shorten 25 
      , ppSep = " "
      , ppWsSep = ""
      , ppUrgent = xmobarColor black gruberRed . sp
      , ppExtras = [windowCount c10]
      , ppSort = (mkWsSort getWsCompare')
      , ppOrder = \(ws:_:t:wc:_) -> [ws,t,wc]
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
regLogHook = logHookDef gruberBg gruberDarkRed1 gruberBg gruberYellow gruberBg gruberBrown gruberBg2 gruberBg1 gruberGreen gruberCyan

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
             , normalBorderColor = gruberDarkRed2
             , focusedBorderColor = gruberDarkRed1
             , borderWidth = 4
             -- Bindings
             , keys = myKeys
             , mouseBindings = myMouseBindings
             -- Hooks and Layouts
             , layoutHook = layouts 
             , manageHook = manageHook' <+> scratchpadManageHook
             , handleEventHook = mempty
             , logHook = regLogHook xmproc0 xmproc1
             , startupHook = startup
         }
         
--EOF
