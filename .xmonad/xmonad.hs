-- vi: et ts=2 sw=2 
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import System.IO
import XMonad.Config.Desktop
import XMonad.Util.Paste
import XMonad.Hooks.EwmhDesktops

main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- xmobar related config
myBar = "xmobar"
myPP = xmobarPP { ppCurrent = xmobarColor "#ffffff" "" . wrap "<" ">" }
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myModMask = mod4Mask
myAdditionalKeysP = 
  [ ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 3%+")
  , ("<XF86AudioLowerVolume>", spawn "amixer sset Master 3%-")
  , ("<XF86AudioMute>",        spawn "amixer sset Master toggle")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10") 
  , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10") 
  , ("M-<Tab>", spawn "setxkbmap us && xmodmap ~/.Xmodmap")
  ]

myConfig = desktopConfig
  { modMask = myModMask
  , terminal = "urxvt"
  , borderWidth=3
  , handleEventHook = fullscreenEventHook -- for chrome fullscreen
  -- , ((0, xK_Insert), pasteSelection)
  } `additionalKeysP` myAdditionalKeysP
