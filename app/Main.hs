{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Main where

import qualified GI.Gtk as Gtk
import qualified GI.GLib as GLib
import Data.GI.Base
import GI.Cairo as Cairo

import qualified Data.Text as T

import Data.Functor.Compose
import Data.IORef

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async.Timer as Timer
import Lib

main :: IO ()
main = do
    Gtk.init Nothing
    win <- new Gtk.Window [ #title := "Conway"]

    on win #destroy Gtk.mainQuit


    button <- new Gtk.Button [ #label := "Start"]
    on button #clicked (set button [
            #sensitive := False,
            #label := "Started"])

    cellGrid <- createRandomGrid 50 50
    grid <- widgetsForCells cellGrid

    forkIO $ gameLoop grid cellGrid

    #add win grid

    #showAll win

    Gtk.main

gameLoop :: Gtk.Grid -> Grid -> IO ()
gameLoop uiGrid grid = do
    gridRef <- newIORef grid
    let conf = (Timer.setInitDelay  0 (Timer.setInterval 250 Timer.defaultConf))

    Timer.withAsyncTimer conf $ \ timer -> do
                    forever $ do
                        Timer.wait timer
                        currentGrid <- readIORef gridRef
                        let newGrid = transition currentGrid
                        writeIORef gridRef newGrid
                        GLib.idleAdd GLib.PRIORITY_DEFAULT $ updateGrid uiGrid newGrid  >> return False




pane :: IO Gtk.Grid
pane = new Gtk.Grid []

kill widget = do
    sc <- Gtk.widgetGetStyleContext widget
    Gtk.styleContextRemoveClass sc "alive"

singleCellAttach grid widget left top = do
    Gtk.gridAttach grid widget left top 1 1 

widgetsForCells :: Grid -> IO Gtk.Grid
widgetsForCells (Grid grid) = do
     gtkGrid <- pane
     sequence_ $ Compose (stuffs gtkGrid)
     return gtkGrid

    where
        stuffs :: Gtk.Grid -> [[IO ()]] 
        stuffs pane = bimapIndexed grid (createAndPlaceCell pane)
        createAndPlaceCell :: Gtk.Grid -> Int -> Int -> Cell -> IO ()
        createAndPlaceCell gtkGrid x y cell = do
                widget <- cellWidget cell
                singleCellAttach gtkGrid widget (fromIntegral x) (fromIntegral y)

-- This only works if the style provider is added to every label, not sure why?
-- but we should add a class to the widget
cellWidget cell = do
    lbl <- new Gtk.Label [#label := ""]
    sc <- Gtk.widgetGetStyleContext lbl
    Gtk.widgetSetSizeRequest lbl 20 20
    Gtk.styleContextAddClass sc (classForCell cell)
    css <- Gtk.cssProviderNew
    Gtk.cssProviderLoadFromPath css "app/style.css"
    Gtk.styleContextAddProvider sc css 0
    return lbl

classForCell :: Cell -> T.Text
classForCell Alive = "alive"
classForCell Dead = ""

updateGrid :: Gtk.Grid -> Grid -> IO ()
updateGrid uiGrid (Grid grid) = sequence_ $ Compose updatedCells
    where
        updatedCells = bimapIndexed grid (stuffs uiGrid)
        stuffs :: Gtk.Grid -> Int -> Int -> Cell -> IO ()
        stuffs grid x y cell = do
            child <- Gtk.gridGetChildAt grid (fromIntegral x) (fromIntegral y)
            maybe (pure ()) (updateCell cell) child


updateCell :: Cell -> Gtk.Widget -> IO ()
updateCell Alive widget = do
    sc <- Gtk.widgetGetStyleContext widget
    Gtk.styleContextAddClass sc "alive"
updateCell Dead widget = do
    sc <- Gtk.widgetGetStyleContext widget
    Gtk.styleContextRemoveClass sc "alive"