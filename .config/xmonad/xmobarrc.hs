Config { font = "xft:Source Code Pro:pixelsize=14:antialias=true:hinting=true"
       , additionalFonts = []
       , bgColor = "#181818"
       , fgColor = "#8B3622"
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
                    , Run Date " %d %b %H:%M:%S" "date" 10
                    , Run Com "uname" ["-r"] "" 36000
                    , Run Com "/home/mcard/.bin/sound.sh" [] "sound" 30
                    , Run Com "/home/mcard/.bin/essid.sh" [] "netName" 30
                    , Run Com "/home/mcard/.bin/wireless.sh" [] "wifi" 30
                    , Run Com "/home/mcard/.bin/mathematicians.sh" [] "math" 300
                    , Run Com "/home/mcard/.bin/eqns.sh" [] "eqn" 300
                    , Run UnsafeStdinReader
                    , Run Battery [ "--template" , "<fc=#8b3622>Batt</fc> <acstatus>"
                             , "--Low"      , "10"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--low"      , "#8b3622"
                             , "--normal"   , "#cc8c3c"
                             , "--high"     , "#ffdd33"
                             , "--"
                                -- battery specific options
                                -- discharging status
                                , "-o"	    , "<left>% (<timeleft>)"
                                -- AC "on" status
                                , "-O"	    , "<fc=#8b3622>Charging</fc>"
                                -- charged status
                                , "-i"	    , "<fc=#73c936>Charged</fc>"
                             ] 50
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "<icon=haskell.xpm/>%UnsafeStdinReader%<fc=#666666>|</fc>\
                    \ %math% \
                    \ <fc=#666666>|</fc> }\
                    \{ <fc=#666666>|</fc>\
                    \ %eqn% \
                    \ <fc=#666666>|</fc>\
                    \ <fc=#FFFFFF>Vol</fc> [<fc=#73c936>%sound%</fc>] \
                    \<fc=#666666>|</fc>\
                    \ <fc=#FFFFFF>%netName%</fc>\
                    \ <fc=#FFFFFF>[<fc=#f43841>%wifi%</fc>]</fc? <fc=#666666>|</fc> %battery% <fc=#666666>|</fc><fc=#8b3622>%date%</fc> "
       }
