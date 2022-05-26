Config { font = "xft:Source Code Pro:pixelsize=14:antialias=true:hinting=true"
       , additionalFonts = []
       , bgColor = "#666666"
       , fgColor = "#181818"
       , position = TopW R 100
       , lowerOnStart = True
       , hideOnStart = False
       , iconRoot = "/home/mcard/.config/xmonad/gayxpm/"
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ Run Date "<icon=calendar.xpm/> %d %b \
                               \<icon=clock.xpm/> %H:%M:%S" "date" 10
                    , Run Com "/home/mcard/.bin/sound.sh" [] "sound" 10
                    , Run Com "/home/mcard/.bin/essid.sh" [] "netName" 10
                    , Run Com "/home/mcard/.bin/gaywireless.sh" [] "wifi" 10
                    , Run Com "/home/mcard/.bin/battery.sh" [] "batt" 10
                    , Run Com "/home/mcard/.bin/mathematicians.sh" [] "math" 300
                    , Run Com "/home/mcard/.bin/eqns.sh" [] "eqn" 300
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " <icon=haskell.xpm/> %StdinReader%\
                    \   %math%   }\
                    \{   %eqn%   \
                    \<fc=#73c936>%sound%</fc>   \
                    \%wifi%   \
                    \%batt%   \
                    \%date% "
       }
