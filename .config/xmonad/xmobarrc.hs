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
       , commands =
           [ Run Date "<icon=calendar.xpm/> <box type=Bottom width=2 mb=2 color=#73c936><fc=#73c936> %d %b </fc></box> \
                      \<icon=clock.xpm/> <box type=Bottom width=2 mb=2 color=#ffdd33><fc=#ffdd33> %H:%M:%S </fc></box>" "date" 10
           , Run Com "/home/mcard/.local/scripts/sound.sh" [] "sound" 10
           , Run Com "/home/mcard/.local/scripts/essid.sh" [] "netName" 10
           , Run Com "/home/mcard/.local/scripts/wireless.sh" [] "wifi" 10
           , Run Com "/home/mcard/.local/scripts/battery.sh" [] "batt" 10
           , Run Com "/home/mcard/.local/scripts/mathematicians.sh" [] "math" 300
           , Run Com "/home/mcard/.local/scripts/eqns.sh" [] "eqn" 300
           , Run Com "/home/mcard/.local/scripts/pacupdate.sh" [] "pacupdate" 300
           , Run StdinReader
           ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " <icon=haskell.xpm/> <fc=#FFFFFF>%StdinReader%</fc> }\
                    \{%pacupdate% \
                    \%sound% \
                    \%wifi% \
                    \%batt% \
                    \%date% "
       }
