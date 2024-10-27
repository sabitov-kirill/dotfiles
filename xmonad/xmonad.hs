{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import qualified Data.Map                       as M

import           System.Exit                    (exitSuccess)
import           XMonad
import           XMonad.Actions.NoBorders
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops      (ewmh, ewmhFullscreen)
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.Place             (inBounds, placeHook,
                                                 underMouse)
import           XMonad.Hooks.StatusBar         (defToggleStrutsKey,
                                                 statusBarProp, withEasySB)
import           XMonad.Layout                  ()
import           XMonad.Layout.AutoMaster
import           XMonad.Layout.CenteredIfSingle (centeredIfSingle)
import           XMonad.Layout.CenterMainFluid  (CenterMainFluid (CenterMainFluid))
import           XMonad.Layout.Combo
import           XMonad.Layout.ComboP
import           XMonad.Layout.Grid
import           XMonad.Layout.Reflect
import           XMonad.Layout.Spacing          (spacingWithEdge, spacing)
import           XMonad.Layout.Spiral           ()
import           XMonad.Layout.Tabbed           (shrinkText, tabbed)
import           XMonad.Layout.ThreeColumns     (ThreeCol (ThreeCol))
import           XMonad.Layout.ToggleLayouts
import           XMonad.Layout.TwoPane
import           XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet                as W
import           XMonad.StackSet                as W (focusDown, focusMaster,
                                                      focusUp, greedyView,
                                                      shift, sink, swapDown,
                                                      swapMaster, swapUp, RationalRect (RationalRect))
import           XMonad.Util.EZConfig           (additionalKeysP)
import           XMonad.Util.Loggers            (logTitles)
import           XMonad.Util.SpawnOnce          (spawnOnce)
import XMonad.Util.WindowProperties
import XMonad.Hooks.ManageHelpers (doFullFloat, doRectFloat)
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (named)

toggleFullscreen :: X ()
toggleFullscreen =
    withWindowSet $ \ws ->
    withFocused $ \w -> do
        let fullRect = W.RationalRect 0 0 1 1
        let isFullFloat = w `M.lookup` W.floating ws == Just fullRect
        windows $ if isFullFloat then W.sink w else W.float w fullRect

main :: IO ()
main = (xmonad . ewmhFullscreen . ewmh . xmonadXmobarProp) xmonadConfig

accentColor = "#A0CFA2"
accentColorBright = "#7CDF7F"
inactiveColor = "#434343"

-- XMobar configuration
xmonadXmobarProp = withEasySB (statusBarProp "xmobar" (pure xmobarPP)) defToggleStrutsKey
  where
    -- toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)

    xmobarPP :: PP
    xmobarPP = def
      { ppSep             = lowWhite " | "
      , ppTitleSanitize   = xmobarStrip
      , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
      , ppHidden          = white . wrap " " ""
      , ppHiddenNoWindows = lowWhite . wrap " " ""
      , ppUrgent          = magenta . wrap (magenta "!") (magenta "!")
      , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
      , ppExtras          = [logTitles formatFocused formatUnfocused]
      }
    formatFocused   = magenta . ppWindow
    formatUnfocused _ = ""

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 70

    lowWhite, magenta, white :: String -> String
    magenta  = xmobarColor accentColorBright ""
    white    = xmobarColor "#f8f8f2" ""
    lowWhite = xmobarColor "#888888" ""

-- Xmonad configuration
xmonadConfig =
  def
    { modMask = mod4Mask,
      layoutHook = xmonadLayoutHook,
      manageHook = xmonadManageHook,
      startupHook = xmonadStartupHook,
      keys = xmonadKeys,
      terminal = "kitty",
      borderWidth = 2,
      normalBorderColor = inactiveColor,
      focusedBorderColor = accentColor
    } `additionalKeysP` xmonadAdditionalKeys
    where

      -- Strartup hook
      xmonadStartupHook :: X ()
      xmonadStartupHook = do
          spawnOnce "picom"
          spawnOnce "xsetroot -solid \"#212121\""
          -- spawnOnce $ "feh --bg-fill " ++ wallpaper
      wallpaper = "~/pictures/wallpapers/gray.png"

      -- Windows manage hook
      xmonadManageHook = placeHook (inBounds (underMouse (0.5, 0.5)))
        <+> composeAll
          [ className =? "Pavucontrol" --> doFloat
          , className =? "Xdg-desktop-portal-gtk" --> doFloat
          , className =? "org.gnome.Nautilus" --> doFloat
          , propertyToQuery (And (ClassName "TelegramDesktop") (Title "Media viewer")) --> doRectFloat (RationalRect 0 0 1 1)
          ]
        <+> manageHook def

      -- Layouts
      xmonadLayouts =
        centered
        ||| twoPaneNamed
        ||| Grid
        where
          centered = named "Centered" (CenterMainFluid 2 (3/100) (1/2))
          twoPaneNamed = named "Two Pane (Coding)" twoPane
          twoPane = combineTwoP (TwoPane (3/100) 0.33) (Tall 2 0 (1/2)) (Mirror $ Tall 1 0 (1/2)) bigPaneWindowClasses
          bigPaneWindowClasses = ClassName "jetbrains-idea" `Or` ClassName "jetbrains-clion" `Or` ClassName "jetbrains-pycharm" `Or` ClassName "Code"

      xmonadLayoutHook =
        avoidStruts $
        spacing 9 $
        smartBorders $
        windowNavigation $
        centeredIfSingle 0.5 0.65 xmonadLayouts

      -- Keys configuration
      xmonadKeys conf@(XConfig {modMask = modMask}) =
        M.fromList $
          [ ((modMask .|. shiftMask, xK_t), spawn $ XMonad.terminal conf), -- %! Launch terminal
            ((modMask, xK_q), kill), -- %! Close the focused window
            ((modMask .|. shiftMask, xK_space), sendMessage NextLayout), -- %! Rotate through the available layout algorithms
            ((modMask, xK_n), refresh), -- %! Resize viewed windows to the correct size

            -- move focus up or down the window stack
            ((modMask, xK_Tab), windows W.focusDown), -- %! Move focus to the next window
            ((modMask .|. shiftMask, xK_Tab), windows W.focusUp), -- %! Move focus to the previous window
            ((modMask, xK_j), windows W.focusDown), -- %! Move focus to the next window
            ((modMask, xK_k), windows W.focusUp), -- %! Move focus to the previous window
            ((modMask, xK_m), windows W.focusMaster), -- %! Move focus to the master window

            -- modifying the window order
            ((modMask, xK_Return), windows W.swapMaster), -- %! Swap the focused window and the master window
            ((modMask .|. shiftMask, xK_j), windows W.swapDown), -- %! Swap the focused window with the next window
            ((modMask .|. shiftMask, xK_k), windows W.swapUp), -- %! Swap the focused window with the previous window
            ((modMask .|. controlMask .|. shiftMask, xK_Right), sendMessage $ Move R)
          , ((modMask .|. controlMask .|. shiftMask, xK_Left ), sendMessage $ Move L)
          , ((modMask .|. controlMask .|. shiftMask, xK_Up   ), sendMessage $ Move U)
          , ((modMask .|. controlMask .|. shiftMask, xK_Down ), sendMessage $ Move D)
          , ((modMask .|. controlMask .|. shiftMask, xK_s    ), sendMessage SwapWindow),
            ((modMask, xK_f), toggleFullscreen),

            -- resizing the master/slave ratio
            ((modMask, xK_h), sendMessage Shrink), -- %! Shrink the master area
            ((modMask, xK_l), sendMessage Expand), -- %! Expand the master area

            -- floating layer support
            ((modMask, xK_t), withFocused $ windows . W.sink), -- %! Push window back into tiling

            -- increase or decrease number of windows in the master area
            ((modMask, xK_comma), sendMessage (IncMasterN 1)), -- %! Increment the number of windows in the master area
            ((modMask, xK_period), sendMessage (IncMasterN (-1))), -- %! Deincrement the number of windows in the master area

            -- quit, or restart
            ((controlMask .|. shiftMask, xK_Delete), io exitSuccess), -- %! Quit xmonad
            ((modMask .|. shiftMask, xK_r), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
          ]
            ++
            -- mod-[1..9] %! Switch to workspace N
            -- mod-shift-[1..9] %! Move client to workspace N
            [ ((m .|. modMask, k), windows $ f i)
              | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
                (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
            ]

      xmonadAdditionalKeys =
        [ ("M-S-s", unGrab *> spawn "flameshot gui"),
          ("M-S-g", spawn "google-chrome"),
          ("M-x", spawn "rofi -show drun")
        ]
