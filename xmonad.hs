{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NamedWildCards        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}

import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.String.Interpolate             (__i)
import           System.Exit                         (exitSuccess)
import           XMonad
import           XMonad.Actions.WithAll              (sinkAll)
import           XMonad.Hooks.DynamicLog
  ( PP (..)
  , shorten
  , wrap
  , xmobarBorder
  , xmobarColor
  , xmobarRaw
  , xmobarStrip
  )
import           XMonad.Hooks.EwmhDesktops           (ewmh, ewmhFullscreen)
import           XMonad.Hooks.ManageHelpers          (isDialog)
import           XMonad.Hooks.StatusBar              (statusBarProp, withEasySB)
import           XMonad.Layout.MultiToggle
  ( Toggle (..)
  , mkToggle
  , single
  )
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL))
import           XMonad.StackSet                     (swapMaster)
import           XMonad.Util.Loggers                 (logTitles)
import           XMonad.Util.Ungrab                  (unGrab)

main :: IO ()
main =
  xmonad .
  ewmhFullscreen .
  ewmh . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) toggleBarKey $
  myConfig
  where
    toggleBarKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleBarKey XConfig {modMask} = (modMask .|. shiftMask, xK_b)

myConfig :: XConfig _layout
myConfig =
  def
    { keys = myKeys
    , layoutHook = myLayout
    , manageHook = myManageHook
    , modMask = mod4Mask -- rebind mod to the super key
    , startupHook = myStartupHook
    , terminal = "st"
    }

myLayout :: _layout a
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

myStartupHook :: X ()
myStartupHook = do
  spawn "~/.fehbg"

myKeys :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
myKeys config@(XConfig {modMask, terminal}) =
  Map.fromList
    [ ((modMask .|. shiftMask, xK_Return), windows swapMaster)
    , ((modMask .|. shiftMask, xK_c), io exitSuccess)
    , ( (modMask .|. shiftMask, xK_s)
      , unGrab >> spawn ("scrot -s " <> screenshotPath))
    , ((modMask .|. shiftMask, xK_t), sinkAll)
    , ((modMask, xK_Return), spawn terminal)
    , ((modMask, xK_b), spawn "brave")
    , ((modMask, xK_c), spawn restartXmonad)
    , ((modMask, xK_d), spawn "dmenu_run")
    , ((modMask, xK_f), sendMessage $ Toggle FULL)
    , ((modMask, xK_q), kill)
    , ((modMask, xK_r), spawn $ terminal <> " -- lf")
    , ((modMask, xK_s), unGrab >> spawn ("scrot " <> screenshotPath))
    ] <>
  foldr
    Map.delete
    (keys def config)
    [(modMask .|. shiftMask, xK_q), (modMask, xK_p)]
  where
    screenshotPath = "~/pictures/screenshots/'%Y-%m-%dT%H:%M:%S_$wx$h.png'"
    restartXmonad =
      [__i|
      if type xmonad; then
        xmonad --recompile && xmonad --restart
      else
        xmessage 'xmonad not in $PATH: '"$PATH"
      fi
    |]

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " * "
    , ppTitleSanitize = xmobarStrip
    , ppCurrent = wrap " " "" . xmobarBorder "Top" "#8de9fd" 2
    , ppHidden = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    , ppOrder =
        \case
          [ws, l, _, wins] -> [ws, l, wins]
          xs               -> xs
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
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
