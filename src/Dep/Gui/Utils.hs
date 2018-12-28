module Dep.Gui.Utils(DepGui,initDepGui,depWindow) where

import Graphics.UI.Gtk(initGUI,Window,windowNew,set,windowIcon,AttrOp((:=)),windowTitle,windowDefaultWidth,windowDefaultHeight)
import Graphics.UI.Gtk.Gdk.Pixbuf(Pixbuf,pixbufNewFromFile)

newtype DepGui = DepGui { depIcon :: Pixbuf }

initDepGui :: IO DepGui
initDepGui = do
    initGUI
    icon <- pixbufNewFromFile "imagestore/buffer.svg"
    return $ DepGui icon

depWindow :: DepGui -> String -> IO Window
depWindow dg tl = do
    wi <- windowNew
    set wi [windowIcon := Just (depIcon dg), windowTitle := ("DEP - " ++ tl), windowDefaultWidth := 1024, windowDefaultHeight := 800]
    return wi
