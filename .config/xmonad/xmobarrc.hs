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
       , commands = [ Run Weather "KORD" ["-t","<station>: <tempC>C",
                                          "-L","18","-H","25",
                                          "--normal","green",
                                          "--high","red",
                                          "--low","lightblue"] 36000
                    , Run Cpu ["-L","3","-H","50", "--normal","green","--high","red"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Date "<icon=calendar.xpm/> %d %b \
                               \<icon=clock.xpm/> %H:%M:%S" "date" 10
                    , Run Com "uname" ["-r"] "" 36000
                    , Run Com "/home/mcard/.bin/sound.sh" [] "sound" 10
                    , Run Com "/home/mcard/.bin/essid.sh" [] "netName" 10
                    , Run Com "/home/mcard/.bin/wireless.sh" [] "wifi" 10
                    , Run Com "/home/mcard/.bin/battery.sh" [] "batt" 10
                    , Run Com "/home/mcard/.bin/mathematicians.sh" [] "math" 300
                    , Run Com "/home/mcard/.bin/eqns.sh" [] "eqn" 300
                    , Run StdinReader
                    , Run Battery [
                            "--template" , "<icon=battery.xpm/> <acstatus>"
                          , "--Low"      , "10"        -- units: %
                          , "--High"     , "80"        -- units: %
                          , "--low"      , "#c73c3f"
                          , "--normal"   , "#cc8c3c"
                          , "--high"     , "#73c936"
                          , "--"
                          -- battery specific options
                          -- discharging status
                          , "-o", "<left>% (<timeleft>)"
                          -- AC "on" status
                          , "-O", "<fc=#cc8c3c>Charging</fc>"
                          -- charged status
                          , "-i", "<fc=#73c936>Charged</fc>"
                          -- , "-c", ""
                          ] 50
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " <icon=haskell.xpm/> <fc=#ffffff>%StdinReader%</fc>\
                    \ <fc=#666666>|</fc> %math% <fc=#666666>|</fc> }\
                    \{ <fc=#666666>|</fc> %eqn% <fc=#666666>|</fc> \
                    \<fc=#73c936>%sound%</fc> <fc=#666666>|</fc> \
                    \%wifi% <fc=#666666>|</fc> \
                    \%batt% <fc=#666666>|</fc> \
                    \%date% "
       }
