module Dep.Gui.Utils.SchematicsViewer(schematicsViewerNew) where

import Control.Monad.Trans(liftIO)

import Graphics.UI.Gtk(DrawingArea,drawingAreaNew,exposeEvent,on,eventWindow,widgetGetDrawWindow,renderWithDrawable)
import Graphics.UI.Gtk.Gdk.Drawable(drawLines)
import Graphics.UI.Gtk.Gdk.EventM(EventM,EExpose)
import Graphics.UI.Gtk.Gdk.GC(gcNew,gcSetValues,newGCValues,GCValues(foreground,capStyle,lineWidth,joinStyle),Color(Color),CapStyle(CapRound),JoinStyle(JoinRound))
import Graphics.Rendering.Cairo(setSourceRGBA,moveTo,lineTo,curveTo,closePath,fillPreserve,stroke,Render,translate,save,restore)

schematicsViewerNew :: IO DrawingArea
schematicsViewerNew = do
    da <- drawingAreaNew
    da `on` exposeEvent $ drawSchematics da
    return da

drawSchematics :: DrawingArea -> EventM EExpose Bool
drawSchematics da = do
    win <- eventWindow
    liftIO $ do
        dw <- widgetGetDrawWindow da
        renderWithDrawable dw $ do
            -- Clock
            translate 30.0 30.0
            save
            paintClock
            restore
            -- AND gate
            {-
            moveTo 30.0 5.0
            lineTo 30.0 45.0
            lineTo 50.0 45.0
            curveTo 61.7441 45.0 70.47619 36.0 70.47619 25.0
            curveTo 70.47619 14.0 61.7441 5.0 50.47619 5.0
            closePath
            paintOuterGate
            -}
            -- OR gate
            {-
            moveTo 7.625 44.5
            lineTo 9.625 46.9375
            curveTo 9.625 46.9375 15.28125 53.93755 15.28125 64.5
            curveTo 15.28125 75.0625 9.625 82.0625 9.625 82.0625
            lineTo 7.625 84.5
            lineTo 10.78125 84.5
            lineTo 24.78125 84.5
            curveTo 27.189326 84.5 32.47095 84.524514 38.40625 82.09375
            curveTo 44.341551 79.662986 50.942786 74.750484 56.09375 65.21875
            lineTo 54.78125 64.5
            lineTo 56.09375 63.78125
            curveTo 45.790637 44.71559 29.537824 44.5 24.78125 44.5
            closePath
            paintOuterGate
            -}
        return True

paintClock :: Render ()
paintClock = do
    moveTo 0.0 0.0
    lineTo 40.0 0.0
    lineTo 40.0 40.0
    lineTo 00.0 40.0
    closePath
    moveTo 10.0 30.0
    lineTo 15.0 30.0
    lineTo 15.0 10.0
    lineTo 20.0 10.0
    lineTo 20.0 30.0
    lineTo 25.0 30.0
    lineTo 25.0 10.0
    lineTo 30.0 10.0
    setSourceRGBA 0.0 0.0 0.0 1.0 -- stroke color
    stroke

paintOuterGate :: Render()
paintOuterGate = do
    setSourceRGBA 1.0 0.5 0.0 1.0 -- fill color
    fillPreserve
    setSourceRGBA 0.0 0.0 0.0 1.0 -- stroke color
    stroke
