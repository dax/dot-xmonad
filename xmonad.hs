import XMonad
import System.IO
import System.Exit
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.CopyWindow
import System.IO
import qualified XMonad.StackSet as W
import XMonad.Util.Scratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Accordion
import XMonad.Layout.ResizableTile
import XMonad.Layout.DragPane
import XMonad.Layout.LayoutCombinators hiding ((|||))
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout
import XMonad.Actions.CycleWS
import XMonad.Layout.Reflect
import Data.Ratio ((%))
import XMonad.Prompt
import XMonad.Prompt.Ssh
import XMonad.Prompt.Email
import XMonad.Prompt.Man
import XMonad.Prompt.Theme
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.Prompt.RunOrRaise
import XMonad.Layout.NoBorders
import XMonad.Actions.NoBorders
import XMonad.Layout.SimpleDecoration
import XMonad.Config.Gnome
import XMonad.Actions.CycleWS
import XMonad.Util.EZConfig
import qualified Data.Map as M
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Themes
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.SetWMName
import Data.Monoid

myWorkspaces = ["1:im","2:web","3:dev","4:calendar","5:music","6:video","7:download","8:log","9:email"]

-- find infos with xprop
myManageHook = composeAll
    [ className =? "Gimp"      --> doShift "9:email"
    , className =? "Vncviewer" --> doFloat
    , className =? "MPlayer" --> doShift "6:video"
    , className =? "xine" --> doShift "6:video"
    , className =? "Gajim.py"  --> doShift "1:im"
    , className =? "psi"  --> doShift "1:im"
    , className =? "Pidgin"  --> doShift "1:im"
    , className =? "Empathy"   --> doShift "1:im"
    , className =? "Firefox"   --> doShift "2:web"
    , className =? "browser"   --> doShift "2:web"
    , className =? "Navigator" --> doShift "2:web"
    , (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
    , className =? "Shiretoko"   --> doShift "2:web"
    , (className =? "Shiretoko" <&&> resource =? "Dialog") --> doFloat
    , className =? "Chromium-browser" --> doShift "2:web"
    , className =? "Chromium" --> doShift "2:web"
    , stringProperty "WM_WINDOW_ROLE" =? "browser" --> doShift "2:web"
    , className =? "Uzbl-core"   --> doShift "2:web"
    , title =? "Gnus"     --> doShift "9:email"
--    , (className =? "Emacs" <&&> className /=? "Gnus") --> doShift "3:dev"
    , className =? "Rhythmbox" --> doShift "5:music"
    , className =? "Amarok"    --> doShift "5:music"
    , className =? "Banshee"   --> doShift "5:music"
    , className =? "Transmission" --> doShift "7:download"
    , className =? "Thunar"    --> doShift "8:file"
    , className =? "stalonetray" --> doIgnore
    , resource =? "synapse" --> (do
                                  w <- ask
                                  liftX $ toggleBorder w
                                  doCenterFloat)
    , className =? "Flashplayerdebugger" --> doShift "8:log"
    , title =? "OCTO Technology - Calendar" --> doShift "4:calendar"
    , title =? "messages_watcher" --> doShift "8:log"
    , title =? "kernel_watcher" --> doShift "8:log"
    , title =? "syslog_watcher" --> doShift "8:log"
    , title =? "xorg_watcher" --> doShift "8:log"
    , isFullscreen --> doFullFloat
    , isSplash --> doFloat
    ]
    where copyToWss ids win = map (copyWindow win) ids
          isSplash = (isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH")
                     <||> (isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG")

-- XMobar integration
-- xmobarLogHook xmproc = dynamicLogWithPP $ xmobarPP
--                         { ppOutput = hPutStrLn xmproc
--                         , ppTitle = xmobarColor "green" "" . shorten 100
--                         }

myLogHook pp = do
    ewmhDesktopsLogHook
    dynamicLogWithPP pp >> updatePointer (Relative 0.5 0.5)

xmonadAppletLogHook = myLogHook $ defaultPP {
                   ppOutput   = \ str -> do
                     let str'  = "<span>" ++ str ++
                                 "</span>"
                     spawn("dbus-send --session --type=signal /org/xmonad/Log org.xmonad.Log.Update string:'" ++ str' ++ "'")
                     return ()
                 , ppTitle    = pangoColor "#2B7598" . shorten 50
                 , ppCurrent  = pangoColor "#2B7598" . wrap "[" "]"
                 , ppVisible  = pangoColor "#2B7598" . wrap "(" ")"
                 , ppHidden   = wrap " " " "
                 , ppUrgent   = pangoColor "red"
                 }

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
 where
  left  = "<span foreground=\"" ++ fg ++ "\">"
  right = "</span>"

myFixedFont = "xft\
         \:Inconsolata\
         \:pixelsize=12\
         \:weight=medium\
         \:width=semicondensed\
         \:dpi=96\
         \:hinting=true\
         \:hintstyle=hintslight\
         \:antialias=true\
         \:rgba=rgb\
         \:lcdfilter=lcdlight"

myFont = "xft\
         \:Sans\
         \:pixelsize=12\
         \:weight=regular\
         \:width=normal\
         \:dpi=96\
         \:hinting=true\
         \:hintstyle=hintslight\
         \:antialias=true\
         \:rgba=rgb\
         \:lcdfilter=lcdlight"

myPromptConfig = defaultXPConfig {
                   font = myFixedFont,
                   bgColor = "#343434",
                   fgColor = "#FFFFFF",
                   borderColor = "#343434",
                   fgHLight = "#FFFFFF",
                   bgHLight = "#2B7598",
                   promptBorderWidth = 0
                 }

myTheme = defaultTheme {
            fontName = myFont,
            activeColor = "#343434",
            activeTextColor = "#2B7598",
            activeBorderColor = "#2B7598",
            inactiveColor = "#343434",
            inactiveTextColor = "#FFFFFF",
            inactiveBorderColor = "#343434"
          }

myEventHook = mempty -- ewmhDesktopsEventHook
myStartupHook = setWMName "LG3D" -- Hack for Java applications

main = do
    gnomeRegister
    xmonad
	$ gnomeConfig
        { workspaces = myWorkspaces
        , terminal = "x-terminal-emulator"
        , manageHook = manageDocks <+> myManageHook -- make sure to include myManageHook definition from above
                        <+> manageHook gnomeConfig
        , layoutHook = onWorkspace "9:email" gimpLayout $
                     onWorkspace "1:im" imLayout $
                     onWorkspace "2:web" webLayout $
                     onWorkspace "6:video" videoLayout $
                     onWorkspace "8:log" logLayout $
                     avoidStruts $ layoutHook gnomeConfig
        , logHook = xmonadAppletLogHook -- xmobarLogHook xmproc
        , startupHook = myStartupHook
        , handleEventHook = myEventHook
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , focusedBorderColor = "#2B7598"
        }
        `additionalKeysP` myKeys
        where
          gimpLayout = avoidStruts $ Tall 1 (3/100) (1/2) ||| (withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") $ tabbed shrinkText myTheme)
          videoLayout = (avoidStruts $ Tall 1 (3/100) (1/2)) ||| smartBorders (tabbed shrinkText myTheme)
          logLayout = simpleDeco shrinkText myTheme $ avoidStruts $ Grid ||| Full ||| Accordion ||| Tall 1 (3/100) (1/2)
          imLayout = avoidStruts $ withIM (1%6) (Or (Or (Or (ClassName "psi") (Role "roster")) (Role "buddy_list")) (Role "contact_list")) (tabbed shrinkText myTheme) ||| withIM (1%6) (Or (Or (Or (ClassName "psi") (Role "roster")) (Role "buddy_list")) (Role "contact_list")) (Tall 1 (3/100) (1/2))
          webLayout = avoidStruts $ tabbed shrinkText myTheme ||| Tall 1 (3/100) (1/2) ||| Accordion ||| Grid

myKeys =
        [ ("M-l",   spawn "gnome-screensaver-command -l")
        , ("M-S-q", spawn "gnome-session-quit --power-off")
        , ("M-S-l", spawn "gnome-session-quit --logout")
        -- , ("M-C-q",       io (exitWith ExitSuccess))
        , ("M-h",   sendMessage Shrink)
        , ("M-j",   sendMessage Expand)
        -- moving workspaces
        , ("M-<Left>",    prevWS )
        , ("M-<Right>",   nextWS )
        , ("M-S-<Left>",  shiftToPrev )
        , ("M-S-<Right>", shiftToNext )
        , ("M-C-<Up>",    spawn "disper -c" )
        , ("M-C-<Left>",  spawn "disper -e -t left" )
        , ("M-C-<Right>", spawn "disper -e -t right" )
        , ("M-C-<Down>",  spawn "disper -s" )
        , ("M-k",         kill)
        , ("M-b", withFocused toggleBorder)
        , ("M-s",         spawn "toggle_transparency" )
        , ("M-S-s",       spawn "toggle_transparency current" )
        , ("M-m",         manPrompt myPromptConfig )
        , ("M-C-t",       themePrompt myPromptConfig )
        , ("M-C-g",       windowPromptGoto myPromptConfig )
        , ("M-C-b",       windowPromptBring myPromptConfig )
        , ("M-C-x",       xmonadPrompt myPromptConfig )
        , ("M-x",         runOrRaisePrompt myPromptConfig )
        , ("M-S-f",       spawn "firefox" )
        , ("M-S-b",       spawn "gnome-www-browser" )
        , ("M-S-e",       spawn "e" )
        , ("M-S-g",       spawn "gnus" )
        , ("M-S-r",       spawn "emacsclient -n -e '(make-remember-frame)'" )
        , ("M-S-m",       spawn "emacs" )
        , ("M-S-n",       spawn "nautilus --no-desktop ~/" )
        , ("M-S-u",       spawn "uzbl-browser" )
        , ("M-S-c",       spawn "chromium" )
        , ("M-S-p",       spawn "scrot")
        , ("M-S-h",       spawn "x-terminal-emulator -e htop")
        , ("M-S-i",       spawn "x-terminal-emulator -e irssi")
        , ("M-S-s",       spawn "x-terminal-emulator -e start_eclimd")
        , ("M-S-t",       spawn "x-terminal-emulator -e tmuxclient")
        , ("M-C-a",       spawn "keepass --auto-type")
        ]
        ++ -- important since ff. is a list itself, can't just put inside above list
        [(otherModMasks ++ "M-" ++ [key], action tag)
         | (tag, key)  <- zip myWorkspaces "123456789"
         , (otherModMasks, action) <- [ ("", windows . W.view) -- was W.greedyView
                                      , ("S-", windows . W.shift)]
        ]
