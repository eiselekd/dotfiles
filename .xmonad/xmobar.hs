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
        Run Battery        [ "--template" , "Batt: <acstatus>"
	, "--Low"      , "10"        -- units: %
	, "--High"     , "80"        -- units: %
	, "--low"      , "darkred"
	, "--normal"   , "darkorange"
	, "--high"     , "darkgreen"

	, "--" -- battery specific options
	-- discharging status
	, "-o"	, "<left>% (<timeleft>)"
	-- AC "on" status
	, "-O"	, "<fc=#dAA520>Charging</fc>"
	-- charged status
	, "-i"	, "<fc=#006000>Charged</fc>"
	] 50,

        Run StdinReader
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader% }{ %battery% <fc=#FFFFCC>%date%</fc>  "
}
