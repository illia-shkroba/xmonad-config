{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NamedWildCards        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ViewPatterns          #-}

import           Control.Concurrent.MVar
  ( MVar
  , modifyMVar_
  , newMVar
  )
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.String.Interpolate             (__i, i)
import           Data.Time.Calendar                  (Day, toGregorian)
import           Data.Time.Clock
  ( UTCTime (utctDay)
  , getCurrentTime
  )
import qualified System.Directory                    as Directory
import           System.Exit                         (exitSuccess)
import qualified System.FilePath                     as FilePath
import           System.FilePath                     ((<.>), (</>))
import           XMonad
import           XMonad.Actions.MostRecentlyUsed
  ( configureMRU
  , mostRecentlyUsed
  )
import           XMonad.Actions.WindowGo             (raiseMaybe, runOrRaise)
import           XMonad.Actions.WithAll              (killOthers, sinkAll)
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
import           XMonad.Hooks.ManageHelpers
  ( Side (NC)
  , doCenterFloat
  , doSideFloat
  , isDialog
  )
import           XMonad.Hooks.StatusBar              (statusBarProp, withEasySB)
import           XMonad.Layout.MultiToggle
  ( Toggle (..)
  , mkToggle
  , single
  )
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import           XMonad.Layout.NoBorders             (smartBorders)
import           XMonad.Layout.Spacing
  ( decScreenSpacing
  , decWindowSpacing
  , incScreenSpacing
  , incWindowSpacing
  , setScreenWindowSpacing
  , smartSpacingWithEdge
  , toggleScreenSpacingEnabled
  , toggleSmartSpacing
  , toggleWindowSpacingEnabled
  )
import           XMonad.StackSet                     (sink, swapMaster)
import           XMonad.Util.Loggers                 (logTitles)
import           XMonad.Util.Run
  ( runProcessWithInputAndWait
  , seconds
  )
import           XMonad.Util.SpawnOnce               (spawnOnce)

main :: IO ()
main = do
  state <- newMVar State {screenkeyEnabled = False}
  xmonad
    . configureMRU
    . ewmhFullscreen
    . ewmh
    . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) toggleBarKey
    $ myConfig state
  where
    toggleBarKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleBarKey XConfig {modMask} = (modMask .|. shiftMask, xK_b)

myConfig :: MVar State -> XConfig _layout
myConfig state =
  def
    { borderWidth = 1
    , clickJustFocuses = True
    , focusFollowsMouse = True
    , focusedBorderColor = "#89b4fa"
    , keys = myKeys state
    , layoutHook = myLayout
    , manageHook = myManageHook
    , modMask = mod4Mask -- rebind mod to the super key
    , normalBorderColor = "#050508"
    , startupHook = myStartupHook
    , terminal = "st"
    , workspaces = workspaces
    }
  where
    workspaces :: [WorkspaceId]
    workspaces = show <$> [1 .. 9 :: Int]

myKeys :: MVar State -> XConfig Layout -> Map (ButtonMask, KeySym) (X ())
myKeys state config@(XConfig {modMask, terminal}) =
  Map.fromList
    [ ((mod1Mask, xK_Tab), mostRecentlyUsed [xK_Alt_L, xK_Alt_R] xK_Tab)
    , ((modMask .|. shiftMask, xK_c), io exitSuccess)
    , ((modMask .|. shiftMask, xK_equal), setScreenWindowSpacing 5)
    , ((modMask .|. shiftMask, xK_g), toggleSmartSpacing)
    , ((modMask .|. shiftMask, xK_m), windows swapMaster)
    , ((modMask .|. shiftMask, xK_p), spawn "pfilemenu -l 10 -i")
    ,
      ( (modMask .|. shiftMask, xK_s)
      , do
          path <- screenshotPath
          createParentDirectory path
          unGrab >> spawn [i|scrot -s #{path}|]
      )
    , ((modMask .|. shiftMask, xK_slash), helpCommand)
    , ((modMask .|. shiftMask, xK_space), setLayout $ layoutHook config)
    , ((modMask .|. shiftMask, xK_h), decScreenSpacing 5 >> decWindowSpacing 5)
    , ((modMask .|. shiftMask, xK_l), incScreenSpacing 5 >> incWindowSpacing 5)
    , ((modMask .|. shiftMask, xK_semicolon), spawn [i|#{terminal} -- tmux attach|])
    , ((modMask .|. shiftMask, xK_t), sinkAll)
    , ((modMask, xK_Return), spawn terminal)
    , ((modMask, xK_b), runOrRaise "qutebrowser" (className =? "qutebrowser"))
    , ((modMask, xK_c), spawn restartXmonad)
    , ((modMask, xK_d), spawn "dmenu_run")
    ,
      ( (modMask, xK_e)
      , liftIO . modifyMVar_ state $ \State {screenkeyEnabled} ->
          if screenkeyEnabled
            then State {screenkeyEnabled = False} <$ spawn "killall screenkey"
            else
              State {screenkeyEnabled = True}
                <$ runProcessWithInputAndWait
                  "screenkey"
                  ["-t", "0.5", "-s", "small", "--opacity", "0.3"]
                  ""
                  (seconds 3)
      )
    , ((modMask, xK_f), sendMessage $ Toggle NBFULL)
    , ((modMask, xK_g), toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled)
    ,
      ( (modMask, xK_n)
      , raiseMaybe
          (spawn [i|#{terminal} -c ncmpcpp -- ncmpcpp|])
          (className =? "ncmpcpp")
      )
    , ((modMask, xK_o), killOthers)
    , ((modMask, xK_p), spawn "passmenu")
    , ((modMask, xK_q), kill)
    ,
      ( (modMask, xK_s)
      , do
          path <- screenshotPath
          createParentDirectory path
          unGrab >> spawn [i|scrot #{path}|]
      )
    , ((modMask, xK_semicolon), spawn [i|#{terminal} -- tmux|])
    , ((modMask, xK_t), withFocused $ windows . sink)
    ,
      ( (modMask, xK_v)
      , do
          path <- today >>= journalPath
          spawn [i|#{terminal} -- nvim #{path}|]
      )
    ,
      ( (modMask, xK_y)
      , spawn "FZF_TERMINAL='st -c clipmenu-fzfmenu' CM_LAUNCHER=fzfclipmenu clipmenu"
      )
    , ((noModMask .|. shiftMask, xK_F10), spawn "wallpaper --random")
    , ((noModMask, xK_F10), spawn "wallpaper --pick")
    ,
      ( (noModMask, xK_F11)
      , spawn "brightnessctl -m set 10%- | cut -d ',' -f 4 | dzen2 -p 1"
      )
    ,
      ( (noModMask, xK_F12)
      , spawn "brightnessctl -m set 10%+ | cut -d ',' -f 4 | dzen2 -p 1"
      )
    ,
      ( (noModMask, xK_F2)
      , spawn [i|wpctl set-volume "$(wpctl-get-default-sink)" 0.05-|]
      )
    ,
      ( (noModMask, xK_F3)
      , spawn [i|wpctl set-volume "$(wpctl-get-default-sink)" 0.05+|]
      )
    ,
      ( (noModMask, xK_F4)
      , spawn [i|wpctl set-mute "$(wpctl-get-default-sink)" toggle|]
      )
    ]
    <> foldr
      Map.delete
      (keys def config)
      [(modMask .|. shiftMask, xK_q), (modMask, xK_p)]
  where
    screenshotPath :: MonadIO m => m FilePath
    screenshotPath = liftIO $ do
      home <- Directory.getHomeDirectory
      pure $
        home
          </> "media"
          </> "pictures"
          </> "screenshots"
          </> "'%Y-%m-%dT%H:%M:%S_$wx$h.png'"

    createParentDirectory :: MonadIO m => FilePath -> m ()
    createParentDirectory path = liftIO $ do
      absolutePath <- Directory.makeAbsolute path
      Directory.createDirectoryIfMissing True . parent $ absolutePath

    parent :: FilePath -> FilePath
    parent = FilePath.dropFileName . FilePath.dropTrailingPathSeparator

    restartXmonad =
      [__i|
      if type xmonad-x86_64-linux; then
        xmonad-x86_64-linux --recompile && xmonad-x86_64-linux --restart
      else
        xmessage 'xmonad-x86_64-linux not in $PATH: '"$PATH"
      fi
    |]

    today :: MonadIO m => m Day
    today = liftIO $ utctDay <$> getCurrentTime

    journalPath :: MonadIO m => Day -> m FilePath
    journalPath (toGregorian -> (year, month, day)) = liftIO $ do
      home <- Directory.getHomeDirectory
      pure $
        home
          </> "notes"
          </> "journal"
          </> show year
          </> show month
          </> show day
          <.> "md"

helpCommand :: X ()
helpCommand = xmessage help

help :: String
help =
  [__i|
    The modifier key used is 'Super'. Defined keybindings:

    -- launching and killing programs
    mod-Enter        Launch st
    mod-;            Launch tmux
    mod-:            Launch tmux attach
    mod-d            Launch dmenu
    mod-q            Close/kill the focused window
    mod-b            Launch qutebrowser
    mod-n            Launch ncmpcpp
    mod-p            Launch passmenu
    mod-y            Launch clipmenu
    mod-e            Toggle screenkey
    mod-s            Take a screenshot
    mod-f            Toggle fullscreen
    mod-Space        Rotate through the available layout algorithms
    mod-Shift-Space  Reset the layouts on the current workSpace to default
    mod-Shift-/      Show this help message
    mod-Shift-s      Take a screenshot of a selected area

    -- move focus up or down the window stack
    mod-Tab        Move focus to the next window
    mod-Shift-Tab  Move focus to the previous window
    mod-j          Move focus to the next window
    mod-k          Move focus to the previous window
    mod-m          Move focus to the master window

    -- modifying the window order
    mod-Shift-Return   Swap the focused window and the master window
    mod-Shift-j        Swap the focused window with the next window
    mod-Shift-k        Swap the focused window with the previous window

    -- resizing the master/slave ratio
    mod-h  Shrink the master area
    mod-l  Expand the master area

    -- floating layer support
    mod-t        Push window back into tiling; unfloat and re-tile it
    mod-Shift-t  Push all windows back into tiling; unfloat and re-tile them

    -- increase or decrease number of windows in the master area
    mod-comma  (mod-,)   Increment the number of windows in the master area
    mod-period (mod-.)   Deincrement the number of windows in the master area

    -- quit, or restart
    mod-Shift-c  Quit xmonad
    mod-c        Restart xmonad

    -- Workspaces & screens
    mod-[1..9]        Switch to workSpace N
    mod-Shift-[1..9]  Move client to workspace N
    mod-{w,e}         Switch to physical/Xinerama screens 1, or 2
    mod-Shift-{w,e}   Move client to screen 1, or 2

    -- Gaps
    mod-g        Toggle gaps
    mod-G        Toggle smart gaps (removes/adds gaps when single window is open)
    mod-Shift-=  Restore gaps size
    mod-Shift-h  Decrease gaps size
    mod-Shift-l  Increase gaps size

    -- Most Recently Used
    alt-Tab  Move focus to the most recently used window

    -- Mouse bindings: default actions bound to mouse events
    mod-button1  Set the window to floating mode and move by dragging
    mod-button2  Raise the window to the top of the stack
    mod-button3  Set the window to floating mode and resize by dragging

    -- Volume
    F2  Decrease volume of a default sink
    F3  Increase volume of a default sink
    F4  Mute a default sink

    -- Wallpaper
    F10        Pick a wallpaper from $XDG_DATA_HOME/wallpapers/
    Shift-F10  Pick a random wallpaper from $XDG_DATA_HOME/wallpapers/

    -- Brightness
    F11  Decrease brightness by 10%
    F12  Increase brightness by 10%
|]

myLayout :: _layout Window
myLayout =
  smartBorders . smartSpacingWithEdge 5 . mkToggle (single NBFULL) $
    tiled ||| Mirror tiled
  where
    tiled = Tall nmaster delta ratio
    -- threeColMid = ThreeColMid nmaster delta ratio
    nmaster = 1 -- default number of masters
    ratio = 1 / 2 -- default ratio occupied by master
    delta = 3 / 100 -- default ratio of screen to increment by when resizing

-- run xprop command on window
-- the second string in WM_CLASS is the class name
myManageHook :: ManageHook
myManageHook =
  mconcat
    [ className =? "Gimp" --> doFloat
    , className =? "ncmpcpp" --> doCenterFloat
    , className =? "qutebrowser-fzfmenu" --> doSideFloat NC
    , className =? "clipmenu-fzfmenu" --> doSideFloat NC
    , isDialog --> doFloat
    ]

myStartupHook :: X ()
myStartupHook = do
  -- `spawnOnce` should be used instead of `spawn` in the `startupHook`.
  -- `spawnOnce` ensures that a command is executed only once, even when
  -- `xmonad` is reloaded.
  spawnOnce "picom &"
  spawnOnce "toggle-language"
  spawnOnce "wallpaper --load"

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
      xmobarRaw
        . ( \w ->
              if null w
                then "untitled"
                else w
          )
        . shorten 30
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#89b4fa" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f9e2af" ""
    red = xmobarColor "#f38ba8" ""
    lowWhite = xmobarColor "#bbbbbb" ""

data State
  = State
      { screenkeyEnabled :: Bool
      }
