{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Main where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import GI.Cairo as Cairo

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
    grid <- pane
    #add win grid
    area1 <- drawArea'
    area2 <- drawArea'
    area3 <- drawArea'

    singleCellAttach grid area1 0 0
    singleCellAttach grid area2 0 1
    singleCellAttach grid area3 1 0

    #showAll win

    Gtk.main

pane :: IO Gtk.Grid
pane = new Gtk.Grid []

kill widget = do
    sc <- Gtk.widgetGetStyleContext widget
    Gtk.styleContextRemoveClass sc "alive"

singleCellAttach grid widget left top = do
    Gtk.gridAttach grid widget left top 1 1 


-- This only works if the style provider is added to every label, not sure why?
-- but we should add a class to the widget
drawArea' = do
    lbl <- new Gtk.Label [#label := ""]
    sc <- Gtk.widgetGetStyleContext lbl
    Gtk.widgetSetSizeRequest lbl 20 20
    Gtk.styleContextAddClass sc "alive"
    css <- Gtk.cssProviderNew
    Gtk.cssProviderLoadFromPath css "app/style.css"
    Gtk.styleContextAddProvider sc css 0
    return lbl
