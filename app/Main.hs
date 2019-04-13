{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Main where

import qualified GI.Gtk as Gtk
import qualified GI.GLib as GLib
import Data.GI.Base (new, on, set, AttrOp((:=)))

import Data.Functor.Compose (Compose(..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Control.Monad (void, forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async.Timer as Timer

import Lib

main :: IO ()
main = do
    Gtk.init Nothing
    win <- new Gtk.Window [ #title := "Conway"]
    on win #destroy Gtk.mainQuit

    cellGrid <- createRandomGrid 50 50
    uiGrid <- createUIGrid cellGrid

    forkIO $ gameLoop uiGrid cellGrid

    #add win uiGrid
    #showAll win
    Gtk.main

tickDelay :: Int
tickDelay = 500

gameLoop :: Gtk.Grid -> Grid -> IO ()
gameLoop uiGrid gridState = do
    gridRef <- newIORef gridState
    let conf = Timer.setInitDelay 0 (Timer.setInterval tickDelay Timer.defaultConf)

    Timer.withAsyncTimer conf $ \timer ->
                    forever $ do
                        Timer.wait timer
                        tick gridRef

    where
        tick :: IORef Grid -> IO ()
        tick gridRef = do
            currentGrid <- readIORef gridRef
            let newGrid = transition currentGrid
            writeIORef gridRef newGrid
            void $ GLib.idleAdd GLib.PRIORITY_DEFAULT (renderGrid uiGrid newGrid >> pure False)

        renderGrid :: Gtk.Grid -> Grid -> IO ()
        renderGrid uiGrid = forallCells (renderCellAtPosition uiGrid)

        renderCellAtPosition :: Gtk.Grid -> Int -> Int -> Cell -> IO ()
        renderCellAtPosition uiGrid x y cell = do
            child <- Gtk.gridGetChildAt uiGrid (fromIntegral x) (fromIntegral y)
            case child of
                Nothing -> pure ()
                (Just uiCell) -> renderCell cell uiCell

        renderCell :: Gtk.IsWidget a => Cell -> a -> IO ()
        renderCell aliveOrDead widget = do
            sc <- Gtk.widgetGetStyleContext widget
            case aliveOrDead of
                Alive -> Gtk.styleContextAddClass sc "alive"
                Dead -> Gtk.styleContextRemoveClass sc "alive"


createUIGrid :: Grid -> IO Gtk.Grid
createUIGrid grid = do
    gtkGrid <- new Gtk.Grid []
    forallCells (createAndPlaceCell gtkGrid) grid
    pure gtkGrid

    where
        createAndPlaceCell :: Gtk.Grid -> Int -> Int -> Cell -> IO ()
        createAndPlaceCell gtkGrid x y cell = do
            widget <- cellWidget cell
            Gtk.gridAttach gtkGrid widget (fromIntegral x) (fromIntegral y) 1 1

        cellWidget :: Cell -> IO Gtk.Label
        cellWidget cellState = do
            uiCell <- new Gtk.Label [#label := ""]
            Gtk.widgetSetSizeRequest uiCell 20 20
            sc <- Gtk.widgetGetStyleContext uiCell
            css <- Gtk.cssProviderNew
            Gtk.styleContextAddProvider sc css 0
            Gtk.cssProviderLoadFromPath css "app/style.css"
            pure uiCell
