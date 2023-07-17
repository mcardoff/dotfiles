Config { font = "xft:Source Code Pro:pixelsize=14:antialias=true:hinting=true"
       , additionalFonts = []
       , bgColor = "#181818"
       , fgColor = "#FFFFFF"
       , position = TopW R 100
       , lowerOnStart = True
       , hideOnStart = False
       , iconRoot = "/home/mcard/.config/xmonad/xpm/"
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ Run Date "<icon=calendar.xpm/> %d %b \
                               \<icon=clock.xpm/> %H:%M:%S" "date" 10
                    , Run Com "/home/mcard/.local/scripts/sound.sh" [] "sound" 10
                    , Run Com "/home/mcard/.local/scripts/essid.sh" [] "netName" 10
                    , Run Com "/home/mcard/.local/scripts/wireless.sh" [] "wifi" 10
                    , Run Com "/home/mcard/.local/scripts/battery.sh" [] "batt" 10
                    , Run Com "/home/mcard/.local/scripts/mathematicians.sh" [] "math" 300
                    , Run Com "/home/mcard/.local/scripts/eqns.sh" [] "eqn" 300
                       -- Check for pacman updates (script found in .local/bin)
                    , Run Com ".local/bin/pacupdate" [] "pacupdate" 36000
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " <icon=haskell.xpm/> <fc=#FFFFFF>%StdinReader%</fc>\
                    \ <fc=#666666>|</fc> %math% <fc=#666666>|</fc> }\
                    \{ <fc=#666666>|</fc> %eqn% <fc=#666666>|</fc> \
                    \<fc=#73c936>%sound%</fc> <fc=#666666>|</fc> \
                    \%wifi% <fc=#666666>|</fc> \
                    \%batt% <fc=#666666>|</fc> \
                    \%date% "
       }
