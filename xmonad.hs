import qualified Data.Map as M
import System.Exit (exitSuccess)
import XMonad
import XMonad.Actions.WithAll (sinkAll)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers (isDialog)
import XMonad.Hooks.StatusBar
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.StackSet (swapMaster)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Loggers
import XMonad.Util.Ungrab (unGrab)

-- import XMonad.Hooks.StatusBar.PP
-- import XMonad.Layout.ThreeColumns
-- import XMonad.ManageHook
main :: IO ()
main =
  xmonad .
  ewmhFullscreen .
  ewmh . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) toggleStrutsKey $
  myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig {modMask = modMask} = (modMask .|. shiftMask, xK_b)

myConfig =
  def
    { keys = myKeys
    , layoutHook = myLayout
    , manageHook = myManageHook
    , modMask = modMask -- rebind mod to the super key
    , startupHook = myStartupHook
    , terminal = "st"
    } `additionalKeys`
  [ ((modMask, xK_b), spawn "brave")
  , ((modMask, xK_s), unGrab >> spawn ("scrot " ++ screenshotPath))
  , ( (modMask .|. shiftMask, xK_s)
    , unGrab >> spawn ("scrot -s " ++ screenshotPath))
  , ((modMask, xK_f), sendMessage $ Toggle FULL)
  ]
  where
    modMask = mod4Mask
    screenshotPath = "~/pictures/screenshots/'%Y-%m-%dT%H:%M:%S_$wx$h.png'"

myLayout = mkToggle (single FULL) (tiled ||| Mirror tiled)
  where
    tiled = Tall nmaster delta ratio
    -- threeColMid = ThreeColMid nmaster delta ratio
    nmaster = 1 -- default number of masters
    ratio = 1 / 2 -- default ratio occupied by master
    delta = 3 / 100 -- default ratio of screen to increment by when resizing

-- run xprop command on window
-- the second string in WM_CLASS is the class name
myManageHook :: ManageHook
myManageHook = mconcat [className =? "Gimp" --> doFloat, isDialog --> doFloat]

myStartupHook = do
  spawn "~/.fehbg"

myKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeys config@(XConfig {modMask = modMask}) =
  M.fromList
    [ ((modMask, xK_Return), spawn $ terminal config)
    , ((modMask, xK_r), spawn $ terminal config ++ " -- lf")
    , ((modMask .|. shiftMask, xK_Return), windows swapMaster)
    , ((modMask, xK_q), kill)
    , ((modMask .|. shiftMask, xK_c), io exitSuccess)
    , ( (modMask, xK_c)
      , spawn
          "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi" -- %! Restart xmonad
       )
    , ((modMask .|. shiftMask, xK_t), sinkAll)
    , ((modMask, xK_d), spawn "dmenu_run")
    ] <>
  foldr
    M.delete
    (keys def config)
    [(modMask .|. shiftMask, xK_q), (modMask, xK_p)]

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " * "
    , ppTitleSanitize = xmobarStrip
    , ppCurrent = wrap " " "" . xmobarBorder "Top" "#8de9fd" 2
    , ppHidden = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    , ppOrder = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow
    ppWindow :: String -> String
    ppWindow =
      xmobarRaw .
      (\w ->
         if null w
           then "untitled"
           else w) .
      shorten 30
    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
