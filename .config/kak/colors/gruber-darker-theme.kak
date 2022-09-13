##
## gruber-darker-theme.kak by me
##

evaluate-commands %sh{
    white="rgb:FFFFFF"
    black="rgb:000000"
    green="rgb:73c936"
    yellow="rgb:ffdd33"
    red="rgb:f43841"
    orange="rgb:cc8c3c"
    niagara="rgb:96a6c8"
    wisteria="rgb:9e95c7"
    quartz="rgb:95a99f"

    bg="rgb:181818"
    bg1="rgb:282828"
    bg2="rgb:453d41"
    bg3="rgb:484848"
    bg4="rgb:52494e"

    fg="rgb:e4e4ef"
    fg1="rgb:f4f4ff"
    fg2="rgb:f5f5ff"

    echo "
        # code
        face global value      ${white}
        face global type       ${quartz}
        face global variable   ${fg1}
        face global module     ${yellow}
        face global function   ${yellow}
        face global identifier ${niagara}
        face global string     ${green}
        face global keyword    ${yellow}+b
        face global operator   ${white}
        face global attribute  ${white}
        face global comment    ${orange}
        face global meta       ${fg}
        face global builtin    ${yellow}

        # text
        face global title  ${niagara}+b
        face global header ${white}
        face global bold   ${white}+b
        face global italic ${fg}+i
        face global mono   ${white},${bg1}
        face global block  ${white},${bg1}
        face global link   ${niagara}+u
        face global bullet ${yellow}
        face global list   ${white}

        # kakoune UI
        face global Default              ${white},${bg}
        face global PrimarySelection     ${white},"rgb:484848"
        face global SecondarySelection   ${bg},${orange}
        face global PrimaryCursor        ${bg},${yellow}+b
        face global SecondaryCursor      ${bg},${yellow}+b
        face global PrimaryCursorEol     ${bg},${yellow}+b
        face global SecondaryCursorEol   ${bg},${yellow}+b
        face global MatchingChar         ${yellow},${bg1}
        face global Search               ${yellow},${bg1}
        face global Whitespace           ${fg},${bg1}
        face global BufferPadding        ${bg}
        face global LineNumbers          ${bg4},${bg}
        face global LineNumberCursor     ${bg4},${bg}
        face global MenuForeground       ${bg},${yellow}
        face global MenuBackground       ${white},${bg}
        face global MenuInfo             ${white},${bg}
        face global Information          ${white},${bg}
        face global Error                ${bg},${red}
        face global StatusLine           ${white},${bg1}
        face global StatusLineMode       ${white},${bg1}
        face global StatusLineInfo       ${green},${bg1}
        face global StatusLineValue      ${white},${bg1}
        face global StatusCursor         ${yellow},${bg1}
        face global Prompt               ${quartz},${bg1}
"
}
