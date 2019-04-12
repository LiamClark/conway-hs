{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Main where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import GI.Cairo as Cairo

import qualified Data.Text as T

import Data.Functor.Compose

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

    cellGrid <- createRandomGrid 10 10

    grid <- widgetsForCells cellGrid
    #add win grid

    #showAll win

    Gtk.main

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
