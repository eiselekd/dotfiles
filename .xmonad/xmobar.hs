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
    -- lowerOnStart = False,
    commands = [
        Run MultiCpu ["-t","Cpu: <total0> <total1> <total2> <total3>","-L","30","-H","60","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC","-w","3"] 10,
        Run Memory ["-t","Mem: <usedratio>%","-H","8192","-L","4096","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10,
        Run Swap ["-t","Swap: <usedratio>%","-H","1024","-L","512","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10,
        Run Network "wlp4s0" ["-t","Net: <rx>, <tx>","-H","200","-L","10","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10,
        Run Network "enp0s31f6" ["-t","Net: <rx>, <tx>","-H","200","-L","10","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10,
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
    template = "%StdinReader% }{ %battery% %multicpu%   %memory%   %swap%   %wlp4s0% %enp0s31f6%  <fc=#FFFFCC>%date%</fc>  "
}