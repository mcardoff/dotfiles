Config { font = "xft:Source Code Pro:pixelsize=14:antialias=true:hinting=true"
       , additionalFonts = []
       , bgColor = "#181818"
       , fgColor = "#FFFFFF"
       , position = TopW C 99
       , lowerOnStart = False
       , hideOnStart = False
       , iconRoot = "/home/mcard/.config/xmonad/xpm/"
       , allDesktops = True
       , overrideRedirect = True
       , commands =
           [ Run Date " <icon=calendar.xpm/>  <fc=#181818,#9e95c7> %d %b </fc> \
                      \ <icon=clock.xpm/>  <fc=#181818,#e075cb> %H:%M:%S </fc>" "date" 10
           , Run Com "/home/mcard/.local/scripts/sound.sh" [] "sound" 10
           , Run Com "/home/mcard/.local/scripts/essid.sh" [] "netName" 10
           , Run Com "/home/mcard/.local/scripts/wireless.sh" [] "wifi" 10
           , Run Com "/home/mcard/.local/scripts/battery.sh" [] "batt" 10
           , Run Com "/home/mcard/.local/scripts/mathematicians.sh" [] "math" 300
           , Run Com "/home/mcard/.local/scripts/eqns.sh" [] "eqn" 300
           , Run Com "/home/mcard/.local/scripts/pacupdate.sh" [] "pacupdate" 300
           , Run DiskU [("/", "<icon=disk.xpm/> <fc=#181818,#cc8c3c> <free> free </fc>")] [] 60
           , Run StdinReader
           ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " <icon=haskell.xpm/> <fc=#FFFFFF>%StdinReader%</fc> } \
                     \ {%pacupdate% %disku% %sound% %wifi% %batt% %date% "
       }
