Config { font = "xft:Hasklig:pixelsize=16:antialias=true:hinting=true"
       , additionalFonts = []
       , bgColor = "#1A1A1A"
       , fgColor = "#cc8c3c"
       , position = Top
       , lowerOnStart = True
       , hideOnStart = False
       , iconRoot = "/home/mcard/.xmonad/xpm/"
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
                    , Run Date " %_d %b %H:%M:%S" "date" 10
		    , Run Com "uname" ["-r"] "" 36000
		    , Run Com "/home/mcard/.bin/sound.sh" [] "sound" 30
		    , Run Com "/home/mcard/.bin/essid.sh" [] "netName" 30
		    , Run Com "/home/mcard/.bin/wireless.sh" [] "wifi" 30
		    , Run UnsafeStdinReader
		    , Run Battery [ "--template" , "<fc=#cc8c3c>Batt</fc> <acstatus>"
                             , "--Low"      , "10"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--low"      , "#c73c3f"
                             , "--normal"   , "#cc8c3c"
                             , "--high"     , "#73c936"
                             , "--"
			     -- battery specific options
                                -- discharging status
                                , "-o"	    , "<left>% (<timeleft>)"
                                -- AC "on" status
                                , "-O"	    , "<fc=#cc8c3c>Charging</fc>"
                                -- charged status
                                , "-i"	    , "<fc=#73c936>Charged</fc>"
                             ] 50
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "<icon=haskell.xpm/> %UnsafeStdinReader% <fc=#666666>|</fc>} { <fc=#cc8c3c>Vol</fc> [<fc=#73c936>%sound%</fc>] <fc=#666666>|</fc> <fc=#cc8c3c>%netName%</fc> [<fc=#f43841>%wifi%</fc>] <fc=#666666>|</fc> %battery% <fc=#666666>|</fc><fc=#cc8c3c>%date%</fc> "
       }
