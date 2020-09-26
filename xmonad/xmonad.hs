{-# LANGUAGE
    DeriveDataTypeable,
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    NoMonomorphismRestriction,
    PatternGuards,
    ScopedTypeVariables,
    TypeSynonymInstances,
    UndecidableInstances
#-}
{-# OPTIONS_GHC -W -fwarn-unused-imports -fno-warn-missing-signatures #-}

import qualified Data.Map as M
import qualified Data.List as L
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.IndependentScreens
import XMonad.Layout.MultiColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Renamed
import XMonad.Layout.Reflect
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare


main :: IO ()
main = do
  trayerProc <- spawnPipe "trayer --edge top --align right --SetDockType true --SetPartialStrut true --widthtype percent --width 10 --heighttype pixel --height 18 --transparent true --alpha 1000 --tint 0x000000 --padding 0"
  xmproc <- spawnPipe "/usr/bin/xmobar -x 4 ~/.xmobarrc"
  xmonad $ ewmh $ withNavigation2DConfig def . docks $ defaultConfig
    { manageHook = myManageHook <+> manageHook defaultConfig
    -- , layoutHook = avoidStruts  $  layoutHook defaultConfig
    , layoutHook = myLayouts
    -- If updatePointer stops working, check if xmobar is running at all. It's
    -- probably not and you need to recompile xmobar.
    , logHook = dynamicLogWithPP xmobarPP
                  { ppOutput = hPutStrLn xmproc
                  , ppTitle = xmobarColor "green" ""
                  , ppLayout = xmobarColor "orange" ""
                  , ppSort = getSortByTag
                  , ppHidden = const ""
                  } >> updatePointer (0.5,0.5) (0.2,0.2)
    , modMask = mod4Mask
    , terminal = "urxvtc"
    , focusFollowsMouse = True
    , borderWidth = 1
    , normalBorderColor = "#ff0000"
    , focusedBorderColor = "#0088ff"
    , workspaces = myWorkspaces
    , keys = \c -> myKeys c `M.union` keys defaultConfig c
    }

myManageHook = composeAll . concat $
   [ [ fmap ( c `L.isInfixOf`) className --> doCenterFloat | c <- myMatchAnywhereFloatsClass ]
   , [ fmap ( c `L.isInfixOf`) title --> doCenterFloat | c <- myMatchAnywhereFloatsTitle ]
   , [ manageDocks ]
   ]
  where
   myMatchAnywhereFloatsClass = []
   myMatchAnywhereFloatsTitle = ["Copying", "Moving", "Deleting"]

myWorkspaces = withScreens 4 ["`", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "=", "<-"]

myLayouts = avoidStruts
            $ mkToggle (REFLECTX ?? EOT)
            $ mkToggle (REFLECTY ?? EOT)
            $ mkToggle (NBFULL ?? EOT)
            $ mkToggle (MIRROR ?? TABBED ?? FULL ?? EOT)
            $ Tall 2 (100/1000) (1/2)
            ||| ThreeCol 2 (10/100) (1/3)
            ||| multiCol [2] 3 0.1 0.4


myTabConfig = defaultTheme { inactiveBorderColor = "#ff0000"
                           , activeBorderColor = "#0088ff"
                           , activeColor = "#000000"
                           , inactiveColor = "#000000"
                           , decoHeight = 18
                           }

data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
  transform _ x k = k (tabbed shrinkText myTabConfig) (const x)


myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
  [ ((modm,               xK_t), sendMessage $ Toggle MIRROR)
  , ((modm,               xK_f), sendMessage $ Toggle TABBED)
  , ((modm .|. shiftMask, xK_f), sendMessage $ Toggle NBFULL)
  , ((modm,               xK_x), sendMessage $ Toggle REFLECTX)
  , ((modm,               xK_y), sendMessage $ Toggle REFLECTY)
  , ((modm .|. shiftMask, xK_t), withFocused $ windows . W.sink)
  , ((modm,               xK_q), spawn "pkill trayer; xmonad --recompile && xmonad --restart")
  , ((modm,               xK_i), spawn "em_paste.sh")
  , ((modm,               xK_Right), windowGo R False)
  , ((modm,               xK_Left ), windowGo L False)
  , ((modm,               xK_Up   ), windowGo U False)
  , ((modm,               xK_Down ), windowGo D False)
  , ((modm .|. shiftMask, xK_Right), windowSwap R False)
  , ((modm .|. shiftMask, xK_Left ), windowSwap L False)
  , ((modm .|. shiftMask, xK_Up   ), windowSwap U False)
  , ((modm .|. shiftMask, xK_Down ), windowSwap D False)
  , ((noModMask, xF86XK_AudioRaiseVolume), spawn "adjust_volume.sh +5%")
  , ((noModMask, xF86XK_AudioLowerVolume), spawn "adjust_volume.sh -5%")
  , ((noModMask, xF86XK_AudioMute), spawn "mute_volume.sh")
--  , ((noModMask, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5%")
--  , ((noModMask, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5%")
  ]
  ++ [((m .|. modm, k), windows $ onCurrentScreen f i)
     | (i, k) <- zip (workspaces' conf) [xK_grave, xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0, xK_minus, xK_equal, xK_BackSpace]
     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
     ]
  ++ [((modm .|. mask, key), f def sc)
     | (key, sc) <- zip [xK_w, xK_e, xK_s, xK_d] [0..]
     , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]
     ]
