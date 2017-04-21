-- xmobar config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config

-- This is setup for dual 1920x1080 monitors, with the right monitor as primary
Config {
    font = "xft:Fixed-8",
    bgColor = "#000000",
    fgColor = "#ffffff",
    -- position = Static { xpos = 0, ypos = 0, height = 16 }, --width = 2560,
    position = Top,
    -- overrideRedirect = False,
    -- lowerOnStart = False,
    commands = [
        Run Date "%a %b %_d %l:%M" "date" 10,
        Run StdinReader
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader% }{  <fc=#FFFFCC>%date%</fc>  "
}
