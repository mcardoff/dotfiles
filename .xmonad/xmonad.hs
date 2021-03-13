-- IMPORTS
import XMonad
import Data.Char
import Data.Monoid
import Data.Tuple
import System.Exit
import Control.Arrow (first)
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

import XMonad.Layout.SimplestFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.Accordion
import XMonad.Layout.Circle
import XMonad.Layout.PerWorkspace
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

import XMonad.Util.Run
import XMonad.Util.EZConfig

import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- Colors

red :: String
red = "#f43841"

orange :: String
orange = "#CC8C3C"

green :: String
green = "#73c936"

bgGray :: String
bgGray = "#1A1A1A"

fgGray :: String
fgGray = "#666666"

lightGray :: String
lightGray = "#95A99F"

-- Strings for passing through

xmobarPath :: String
xmobarPath = "/home/mcard/.xmonad/xmobarrc.hs"

editor :: String
editor = "emacs"

editorDoom :: String
editorDoom = "emacs --with-profile doom "

regularConfig :: String
regularConfig = " /home/mcard/regmacs/.emacs.d/init.el"

doomConfig :: String
doomConfig = " /home/mcard/doomacs/.emacs.d/init.el"

browser :: String
browser = "google-chrome"

myFont :: String
myFont = "Hasklig:size=13"

myTerm :: String
myTerm = "~/.cargo/bin/alacritty"

myFile :: String
myFile = "nemo"

dmenuOptions :: String
dmenuOptions = "-fn '" ++ myFont ++ "' -p '>' -sb '#cc8c3c' -sf '#FAFAFA' -nb '#282828' -nf '#FAFAFA' -c -l 20"

xmonadConfig :: String
xmonadConfig = editor ++ " /home/mcard/.xmonad/xmonad.hs"

-- Keys

alt :: KeyMask
alt = mod1Mask

myKeys conf@XConfig {XMonad.modMask = sup} = M.fromList $
    [ -- SPAWNING
      ((sup .|. shf, xK_Return), spawn $ myTerm)
    , ((sup .|. shf, xK_x), spawn $ xmonadConfig)
    , ((sup, xK_b), spawn $ browser)
    , ((sup, xK_o), spawn $ editor)
    , ((sup, xK_i), spawn $ editorDoom)
    , ((sup .|. shf, xK_o), spawn $ editor ++ regularConfig)
    , ((sup .|. shf, xK_i), spawn $ editorDoom ++ doomConfig)
    , ((sup, xK_p), spawn $ "dmenu_run " ++ dmenuOptions)
    , ((sup .|. shf, xK_equal), spawn $ "killall xmobar")
    , ((sup, xK_f), spawn $ myTerm ++ " -e ranger")
    , ((sup .|. shf, xK_f), spawn $ myFile)
    , ((sup, xK_s), spawn $ "cinnamon-settings")
    , ((sup .|. shf, xK_n), spawn $ "nitrogen --restore")
    , ((sup .|. shf, xK_m), spawn $ "lxappearance")

      -- PROMPTS
    , ((sup,xK_c), calcPrompt myXPConfig "qalc")
    -- , ((sup,xK_z), customBasicPrompt "hoogle --count=1" myXPConfig "hoogle")

      -- TEST
--    , ((sup,xK_n), updateLayout "1" (Just tabbed))

      -- XMONAD DEFAULTS
    , ((sup .|. shf, xK_c), kill)
    , ((sup, xK_space), sendMessage NextLayout)
    , ((sup .|. shf, xK_space), setLayout $ XMonad.layoutHook conf)
    , ((sup, xK_n), refresh)
    , ((sup, xK_Tab), windows W.focusDown)
    , ((sup, xK_j), windows W.focusDown)
    , ((sup, xK_k), windows W.focusUp)
    , ((sup, xK_m), windows W.focusMaster)
    , ((sup, xK_Return), windows W.swapMaster)
    , ((sup .|. shf, xK_j), windows W.swapDown)
    , ((sup .|. shf, xK_k), windows W.swapUp)
    , ((sup, xK_h), sendMessage Shrink)
    , ((sup, xK_l), sendMessage Expand)
    , ((sup, xK_t), withFocused $ windows . W.sink)
    , ((sup, xK_comma), sendMessage (IncMasterN 1))
    , ((sup, xK_period), sendMessage (IncMasterN (-1)))
    , ((sup .|. shf, xK_q), io exitSuccess)
    , ((sup, xK_q), spawn "xmonad --recompile; xmonad --restart")

      -- MISC
    , ((0, 0x1008FF11), spawn "/home/mcard/.bin/changevol.sh -10%") -- Vol down
    , ((0, 0x1008FF13), spawn "/home/mcard/.bin/changevol.sh +10%") -- Vol up
    , ((0, 0x1008FF12), spawn "/home/mcard/.bin/mutevol.sh")        -- Vol mute

    ]
    ++
    [((mo .|. sup, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, mo) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((mo .|. sup, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, mo) <- [(W.view, 0), (W.shift, shiftMask)]]
        where shf = shiftMask


myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
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
             , activeColor         = "#cc8c3c"
             , inactiveColor       = "#181818"
             , activeBorderColor   = "#cc8c3c"
             , inactiveBorderColor = "#181818"
             , activeTextColor     = "#ffffff"
             , inactiveTextColor   = "#ffffff"
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

myWorkspaces :: [String]
myWorkspaces = tots ++ rest
    where tots = ["trm","edt","www","sys","vid","dsc"]
          rest = map show $ [(length tots)+1..9]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myXPConfig :: XPConfig
myXPConfig = def {
               -- font = "xft:Hasklig:pixelsize=14"
               font = "xft:Hasklig:size=13"
             , bgColor = "#282828"
             , fgColor =  "#CC8C3C"
             , bgHLight = "#CC8C3C"
             , fgHLight = "FAFAFA"
             , borderColor = "#CC8C3C"
             , promptBorderWidth = 2
             , height = 30
             , position = CenteredAt (1/2) (1/2)
             , maxComplRows = Just 20
               }

calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans =
    inputPrompt c (trim ans) ?+ \input ->
        liftIO(runProcessWithInput "qalc" [input] "") >>= calcPrompt c
    where
        trim  = f . f
        f = reverse . dropWhile isSpace

-- customBasicPrompt :: String -> XPConfig -> String -> X ()
-- customBasicPrompt pr c an =
--     inputPrompt c (trim an) ?+ \input ->
--         liftIO(runProcessWithInput pr [input] "") >>= customBasicPrompt pr c
--     where
--         trim  = f . f
--         f = reverse . dropWhile isSpace

-- Hooks

manHook :: Query (Endo WindowSet)
manHook = composeAll
          [ className =? "MPlayer" --> doFloat
          , className =? "Gimp"    --> doFloat
          , resource =? "desktop_window" --> doIgnore
          , resource =? "kdesktop" --> doIgnore
          , className =? "Test Window" --> doFloat
          , appName =? myTerm  --> doShift (myWorkspaces !! 0) -- Terminal in 1
          , appName =? editor  --> doShift (myWorkspaces !! 1) -- Emacs in 2
          , appName =? browser --> doShift (myWorkspaces !! 2) -- Chrome in 3
          , appName =? myFile  --> doShift (myWorkspaces !! 3) -- File Man. in 4
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

startHook :: X ()
startHook = spawn "/home/mcard/.bin/.xmonad-session-rc"

main :: IO ()
main = do
  xmproc0 <- spawnPipe $ "xmobar -x 0 " ++ xmobarPath
  xmproc1 <- spawnPipe $ "xmobar -x 1 " ++ xmobarPath
  xmonad $ docks def {
             -- Basics
               modMask = mod4Mask
             , terminal = myTerm
             , focusFollowsMouse = True
             , clickJustFocuses = False
             , workspaces = myWorkspaces
             , normalBorderColor = lightGray
             , focusedBorderColor = orange
             , borderWidth = 2

             -- Bindings
             , keys = myKeys
             , mouseBindings = myMouseBindings

             -- Hooks and Layouts

             , layoutHook = layouts
             , manageHook = manHook
             , handleEventHook = eveHook
             , logHook = lgHook xmproc0 xmproc1
             , startupHook = startHook
         }

--EOF
