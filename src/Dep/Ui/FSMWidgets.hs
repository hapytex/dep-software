module Dep.Ui.FSMWidgets (
    ) where

import Control.Monad((>=>))

import Data.IORef(readIORef)

import Dep.Utils(selectBool)
import Dep.Ui.Utils(EdgeEmit(..),EdgeWidget(..),defaultRenderEdges,branchW,branchH,branchWinit,RenderEmitter,propagateShiftSetCurrentPosition,linC)

import Graphics.Vty.Attributes(Attr(..))
import Graphics.Vty.Prelude(DisplayRegion)
import Graphics.Vty.Image(Image,string,char,imageWidth,imageHeight,crop,(<|>),(<->))
import Graphics.Vty.Input.Events(Key(..),Modifier)
import Graphics.Vty.Widgets.All(Widget(),newWidget,WidgetImpl(..),RenderContext(..),getNormalAttr,getState,updateWidgetState,addToFocusGroup)
import Graphics.Vty.Widgets.Core(handleKeyEvent,render,getCursorPosition,getCurrentSize,setCurrentPosition,growHorizontal,growVertical,relayFocusEvents,updateWidgetState,(<~~),(<~))
import Graphics.Vty.Widgets.Box(IndividualPolicy(..),BoxError(..))
import Graphics.Vty.Widgets.Util(plusWidth,plusHeight,withWidth,withHeight)

