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
import qualified XMonad.StackSet as W
import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Grid
import XMonad.Layout.IndependentScreens
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.MultiToggle
import XMonad.Layout.Renamed
import XMonad.Layout.Reflect
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare


main :: IO ()
main = do
  trayerProc <- spawnPipe "trayer --edge top --align right --SetDockType true --SetPartialStrut true --widthtype percent --width 10 --heighttype pixel --height 18 --transparent true --alpha 1000 --tint 0x000000 --padding 0"
  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
  xmonad $ defaultConfig
    { manageHook = myManageHook <+> manageHook defaultConfig
    -- , layoutHook = avoidStruts  $  layoutHook defaultConfig
    , layoutHook = myLayouts
    , logHook = dynamicLogWithPP xmobarPP
                  { ppOutput = hPutStrLn xmproc
                  , ppTitle = xmobarColor "green" ""
                  , ppLayout = xmobarColor "orange" ""
                  , ppSort = getSortByTag
                  , ppHidden = const ""
                  } >> updatePointer (TowardsCentre 0.2 0.2)
    , modMask = mod4Mask
    , terminal = "SHELL=/usr/bin/fish urxvtc"
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
    myMatchAnywhereFloatsTitle = ["Copying"]

myWorkspaces = withScreens 4 ["`", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "=", "<-"]

myLayouts = avoidStruts
            $ mkToggle (REFLECTX ?? REFLECTY ?? MIRROR ?? TABBED ?? FULL ?? EOT)
            $ Tall 1 (3/1000) (1/2)
            ||| ThreeCol 1 (3/100) (1/2)
            ||| renamed [Replace "ThreeColMid"] (ThreeColMid 1 (3/100) (1/2))
            ||| Grid
            ||| spiral (6/7)

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
  M.fromList
  $ [ ((modm,               xK_t), sendMessage $ Toggle MIRROR)
    , ((modm,               xK_f), sendMessage $ Toggle TABBED)
    , ((modm .|. shiftMask, xK_f), sendMessage $ Toggle FULL)
    , ((modm,               xK_x), sendMessage $ Toggle REFLECTX)
    , ((modm,               xK_y), sendMessage $ Toggle REFLECTY)
    , ((modm .|. shiftMask, xK_t), withFocused $ windows . W.sink)
    , ((modm,               xK_q), spawn "pkill trayer; xmonad --recompile && xmonad --restart")
    ]
  ++ [((m .|. modm, k), windows $ onCurrentScreen f i)
     | (i, k) <- zip (workspaces' conf) [xK_grave, xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0, xK_minus, xK_equal, xK_BackSpace]
     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
     ]
  ++ [((modm .|. mask, key), f sc)
     | (key, sc) <- zip [xK_w, xK_e, xK_s, xK_d] [0..]
     , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]
     ]
