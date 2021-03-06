module Main where

import Chonquest

import Data.Maybe

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

ok_clicked = mainQuit

setup_treeview :: GladeXML -> IO ()
setup_treeview xml = do
    treeview <- xmlGetWidget xml 
        castToTreeView "player_treeview"
    list <- listStoreNew 
        [Player 
            {p_name="Player 1", number=1,p_type=Human}]
    treeViewSetModel treeview list
    treeViewSetHeadersVisible treeview True

    name_col <- treeViewColumnNew
    treeViewColumnSetTitle name_col "Name"
    renderer <- cellRendererTextNew
    cellLayoutPackStart name_col renderer False
    cellLayoutSetAttributes name_col renderer list
        $ \play -> [cellText := p_name play]
    treeViewAppendColumn treeview name_col

    type_list <- listStoreNew [Human, EasyComputer, MediumComputer, HardComputer]

    type_col <- treeViewColumnNew
    treeViewColumnSetClickable type_col True
    renderer <- cellRendererComboNew
    set renderer [cellComboTextModel := 
        (type_list, makeColumnIdString 1),
        cellMode := (toEnum 2)]
    
    treeViewColumnSetTitle type_col "Type"
    cellLayoutPackStart type_col renderer False
    cellLayoutSetAttributes type_col renderer list
        $ \play -> [cellText := show (p_type play)]
    treeViewAppendColumn treeview type_col
    return ()

setup_owner_combobox :: GladeXML -> IO ()
setup_owner_combobox xml = do
    box <- xmlGetWidget xml 
        castToComboBox "owner_combobox"
    list <- comboBoxSetModelText box
    listStoreAppend list (show Vacant)
    comboBoxSetActive box 0

add_player = return ()

remove_player = return ()

main :: IO ()
main = do
    initGUI
    mxml <- xmlNew "data/chonquest.glade"
    xml <- return $ fromJust mxml
    dialog <- xmlGetWidget xml castToDialog 
        "new_dialog"
    ok_button <- xmlGetWidget xml castToButton 
        "ok_button"
    cancel_button <- xmlGetWidget xml castToButton 
        "cancel_button"
    add_button <- xmlGetWidget xml castToButton
        "add_player_button"
    remove_button <- xmlGetWidget xml castToButton
        "remove_player_button"

    onClicked ok_button ok_clicked
    onClicked cancel_button mainQuit
    onClicked add_button add_player
    onClicked remove_button remove_player

    onDestroy dialog mainQuit

    setup_treeview xml
    setup_owner_combobox xml

    widgetShowAll dialog
    mainGUI
