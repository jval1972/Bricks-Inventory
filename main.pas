//------------------------------------------------------------------------------
//
//  BrickInventory: A tool for managing your brick collection
//  Copyright (C) 2014-2023 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//    Main Form
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, OleCtrls, SHDocVw, Readhtml, FramView, Htmlview, ExtCtrls, Masks,
  StdCtrls, Buttons, bi_docwriter, bi_db, bi_hash, bi_orders, Menus, jpeg,
  pngimage, bi_lugbulk2017, ComCtrls, bi_delphi, AppEvnts;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    OutputMemo: TMemo;
    Splitter1: TSplitter;
    Bevel1: TBevel;
    Timer1: TTimer;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Set1: TMenuItem;
    Missingformultiplesets1: TMenuItem;
    Printpreview1: TMenuItem;
    N1: TMenuItem;
    Panel2: TPanel;
    btn_back: TSpeedButton;
    btn_fwd: TSpeedButton;
    btn_home: TSpeedButton;
    btn_printpreview: TSpeedButton;
    Compare2sets1: TMenuItem;
    ools1: TMenuItem;
    Mosaic1: TMenuItem;
    Piece1: TMenuItem;
    N2: TMenuItem;
    Sets1: TMenuItem;
    btn_save: TSpeedButton;
    SetstobuyNew1: TMenuItem;
    btn_Copy: TSpeedButton;
    SetstobuyUsed1: TMenuItem;
    N4: TMenuItem;
    UsedPiecesbelow30euroKgr1: TMenuItem;
    UsedPiecesbelow20euroKgr1: TMenuItem;
    UsedPiecesbelow15euroKgr1: TMenuItem;
    UsedPiecesbelow10euroKgr1: TMenuItem;
    UsedPiecesbelow25euroKgr1: TMenuItem;
    CheckBox1: TCheckBox;
    N5: TMenuItem;
    Missingfromstoragebins1: TMenuItem;
    Export1: TMenuItem;
    Priceguide1: TMenuItem;
    Partoutguide1: TMenuItem;
    Import1: TMenuItem;
    BricklinkOrder1: TMenuItem;
    OpenDialog1: TOpenDialog;
    N6: TMenuItem;
    LUGBULKSuggestions1: TMenuItem;
    AddressEdit: TEdit;
    SpeedButton1: TSpeedButton;
    Missingfordismandaledsets1: TMenuItem;
    Usedsetstobuiltfromscratch1: TMenuItem;
    N8: TMenuItem;
    Weightqueries1: TMenuItem;
    Batchlink1: TMenuItem;
    S1: TMenuItem;
    Reloadcache1: TMenuItem;
    CheckCacheHashEfficiency1: TMenuItem;
    Queries1: TMenuItem;
    Platequeries1: TMenuItem;
    Bricks1x1: TMenuItem;
    Bricks2x1: TMenuItem;
    N9: TMenuItem;
    Plates1x1: TMenuItem;
    Plates2x1: TMenuItem;
    N10: TMenuItem;
    Plates6x1: TMenuItem;
    Plates8x1: TMenuItem;
    N11: TMenuItem;
    iles1x1: TMenuItem;
    iles2x1: TMenuItem;
    Database1: TMenuItem;
    Set2: TMenuItem;
    N12: TMenuItem;
    echnicBricks1x1: TMenuItem;
    LugBulks1: TMenuItem;
    LugBulk20171: TMenuItem;
    N20171: TMenuItem;
    N13: TMenuItem;
    LugBulk2017CheapBricks1: TMenuItem;
    LugBulk2017CheapSlopes1: TMenuItem;
    LugBulk2017CheapPlates1: TMenuItem;
    LugBulk2017CheapTiles1: TMenuItem;
    LugBulk2017CheapInvertedSlopes1: TMenuItem;
    Specialbricks1x1: TMenuItem;
    N14: TMenuItem;
    SaveToRebrickableDialog1: TSaveDialog;
    Rebrickableparts1: TMenuItem;
    N15: TMenuItem;
    Setswithunknownreleaseyear1: TMenuItem;
    N20181: TMenuItem;
    LugBulk2018CheapInvertedSlopes1: TMenuItem;
    LugBulk2018CheapSlopes1: TMenuItem;
    LugBulk2018CheapTiles1: TMenuItem;
    LugBulk2018CheapPlates1: TMenuItem;
    LugBulk2018CheapBricks1: TMenuItem;
    N16: TMenuItem;
    LugBulk2018CheapParts1: TMenuItem;
    N17: TMenuItem;
    Showorderinformation1: TMenuItem;
    Rebrickablesets1: TMenuItem;
    Rebrickablepartsofbuildedsets1: TMenuItem;
    N18: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Rebrickablepartsofbuildedmocs1: TMenuItem;
    MosaicMenuItem1: TMenuItem;
    PlateMosaic1: TMenuItem;
    TileMosaic1: TMenuItem;
    SnotBricks1: TMenuItem;
    N19: TMenuItem;
    UsedPiecesabove1000euroKgr1: TMenuItem;
    UsedPiecesabove5000euroKgr1: TMenuItem;
    UsedPiecesabove10000euroKgr1: TMenuItem;
    N20: TMenuItem;
    NewPiecesbelow10euroKgr1: TMenuItem;
    NewPiecesbelow15euroKgr1: TMenuItem;
    NewPiecesbelow20euroKgr1: TMenuItem;
    NewPiecesbelow25euroKgr1: TMenuItem;
    NewPiecesbelow30euroKgr1: TMenuItem;
    N21: TMenuItem;
    NewPiecesabove1000euroKgr1: TMenuItem;
    NewPiecesabove5000euroKgr1: TMenuItem;
    NewPiecesabove10000euroKgr1: TMenuItem;
    Pricequeries1: TMenuItem;
    Newcheaperthanused1: TMenuItem;
    Slopes18degrees1: TMenuItem;
    N23: TMenuItem;
    Newpieceswithpricegraterthan50euro1: TMenuItem;
    Usedpieceswithpricegraterthan50euro1: TMenuItem;
    MostexpensivelotsofmyNEWparts1: TMenuItem;
    MostexpensivelotsofmyUSEDparts1: TMenuItem;
    Slopequeries1: TMenuItem;
    Slopes33degrees1: TMenuItem;
    Slopes45degrees1: TMenuItem;
    Slopes65degrees1: TMenuItem;
    Slopes75degrees1: TMenuItem;
    N25: TMenuItem;
    InvertedSlopes33degrees1: TMenuItem;
    InvertedSlopes45degrees1: TMenuItem;
    InvertedSlopes75degrees1: TMenuItem;
    Checkstoragebinsreport1: TMenuItem;
    ProgressBar1: TProgressBar;
    Storage1: TMenuItem;
    Correntunknowncategoriesofmyinventory1: TMenuItem;
    Inventory1: TMenuItem;
    Clearimagecache1: TMenuItem;
    Piecequeries1: TMenuItem;
    Piecesfirstappearedinyear1: TMenuItem;
    Piecesdiscontinuedatyear1: TMenuItem;
    N26: TMenuItem;
    FirstappearedExcludingvariations1: TMenuItem;
    DiscontinuedAllpartsExcl1: TMenuItem;
    Rarepartsofmyinventory1: TMenuItem;
    Appersin1set1: TMenuItem;
    Appearsin2sets1: TMenuItem;
    Appearsin3sets1: TMenuItem;
    N27: TMenuItem;
    MostexpensiveofmypartsNEW1: TMenuItem;
    MostexpensiveofmypartsUSED1: TMenuItem;
    Sets2: TMenuItem;
    N3: TMenuItem;
    N7: TMenuItem;
    SetstobuyforminifigsNEW1: TMenuItem;
    SetstobuyforminifigsUSED1: TMenuItem;
    N22: TMenuItem;
    FirstappearedMinifigureparts1: TMenuItem;
    DiscontinuedMinifigureparts1: TMenuItem;
    Piece2: TMenuItem;
    Rebrickablecsv1: TMenuItem;
    OpenDialog2: TOpenDialog;
    Newpieceswithpricegreaterthan50eurosoldandavailable1: TMenuItem;
    Usedpieceswithpricegreaterthan50eurosoldandavailable1: TMenuItem;
    N28: TMenuItem;
    Prise1: TMenuItem;
    Exportunknownpieces1: TMenuItem;
    SaveDialog1: TSaveDialog;
    ClearStubEntries1: TMenuItem;
    Reorganize1: TMenuItem;
    Update1: TMenuItem;
    Sets3: TMenuItem;
    Partnamesrebrickable1: TMenuItem;
    Partswithoutknowncolors1: TMenuItem;
    Newismuchmoreexpensivethanused1: TMenuItem;
    Newpartsbricklinkcom1: TMenuItem;
    ColorQueries1: TMenuItem;
    Pieceswithmorethan30colors1: TMenuItem;
    Nameswithbothpartandsetcolorindexes1: TMenuItem;
    ExportInventory1: TMenuItem;
    N29: TMenuItem;
    Allitems1: TMenuItem;
    Multipagedisplay1: TMenuItem;
    UsemultipleCPUcores1: TMenuItem;
    N30: TMenuItem;
    N31: TMenuItem;
    Options1: TMenuItem;
    IdleTimer: TTimer;
    btn_CatalogHome: TSpeedButton;
    FindDialog: TFindDialog;
    N32: TMenuItem;
    Edit1: TMenuItem;
    HtmlCopy1: TMenuItem;
    HtmlFind1: TMenuItem;
    HtmlSelectAll1: TMenuItem;
    TabControl1: TTabControl;
    HTML: THTMLViewer;
    PopupMenu1: TPopupMenu;
    CloseTab1: TMenuItem;
    N34: TMenuItem;
    HtmlCopy2: TMenuItem;
    HtmlFind2: TMenuItem;
    HtmlSelectAll2: TMenuItem;
    Openlinkinnewtab1: TMenuItem;
    N35: TMenuItem;
    PopupMenu2: TPopupMenu;
    CloseTab2: TMenuItem;
    Openlink1: TMenuItem;
    N33: TMenuItem;
    Setsbynumpieces1: TMenuItem;
    Setsbynumlots1: TMenuItem;
    Datequeries1: TMenuItem;
    Pieceswithoutupdatethelast10days1: TMenuItem;
    Pieceswithoutupdatethelast30days1: TMenuItem;
    Pieceswithoutupdatethelast90days1: TMenuItem;
    Pieceswithoutupdatethelast365days1: TMenuItem;
    N36: TMenuItem;
    Pieceswithoutupdateinrange1: TMenuItem;
    Expensivepartsofmyinventory1: TMenuItem;
    N24: TMenuItem;
    Biglotsofmyinventory1: TMenuItem;
    Lotswith10ormoreparts1: TMenuItem;
    Lotswith50ormoreparts1: TMenuItem;
    Lotswith100ormoreparts1: TMenuItem;
    Lotswith500ormoreparts1: TMenuItem;
    Lotswith1000ormoreparts1: TMenuItem;
    N20191: TMenuItem;
    LugBulk2019CheapParts1: TMenuItem;
    N37: TMenuItem;
    LugBulk2019CheapBricks1: TMenuItem;
    LugBulk2019CheapPlates1: TMenuItem;
    LugBulk2019CheapTiles1: TMenuItem;
    LugBulk2019CheapSlopes1: TMenuItem;
    LugBulk2019CheapInvertedSlopes1: TMenuItem;
    ExportPartsColors1: TMenuItem;
    SaveDialogPriceGuide: TSaveDialog;
    SaveDialogPartOut: TSaveDialog;
    SaveDialogDatabase: TSaveDialog;
    SaveDialogPartsColors: TSaveDialog;
    MarketQueriesSets1: TMenuItem;
    MostitemssoldNew1: TMenuItem;
    MostitemssoldUsed1: TMenuItem;
    MostitemssoldNewUsed1: TMenuItem;
    N38: TMenuItem;
    MoreitemsavailableNew1: TMenuItem;
    MoreitemsavailableUsed1: TMenuItem;
    MoreitemsavailableNewUsed1: TMenuItem;
    MostsetssoldNew1: TMenuItem;
    MostsetssoldUsed1: TMenuItem;
    MostsetssoldNewUsed1: TMenuItem;
    N40: TMenuItem;
    MoresetsavailableNew1: TMenuItem;
    MoresetsavailableUsed1: TMenuItem;
    MoresetsavailableNewUsed1: TMenuItem;
    Parts1: TMenuItem;
    Marketqueries2: TMenuItem;
    Minifigs1: TMenuItem;
    MostminifigssoldNew1: TMenuItem;
    MostminifigssoldUsed1: TMenuItem;
    MostminifigssoldNewUsed1: TMenuItem;
    N39: TMenuItem;
    MoreminifigsavailableNew1: TMenuItem;
    MoreminifigsavailableUsed1: TMenuItem;
    MoreminifigsavailableNewUsed1: TMenuItem;
    MoldQueries1: TMenuItem;
    Moldsdiscontinuedatyear1: TMenuItem;
    MoldsFirstAppeared1: TMenuItem;
    CollectionValue1: TMenuItem;
    N41: TMenuItem;
    NewTab1: TMenuItem;
    N42: TMenuItem;
    Lugbulk2017items1: TMenuItem;
    N43: TMenuItem;
    Lugbulk2018items1: TMenuItem;
    N44: TMenuItem;
    Lugbulk2019items1: TMenuItem;
    N45: TMenuItem;
    Comparesets1: TMenuItem;
    Pieceswithunknownweight1: TMenuItem;
    N46: TMenuItem;
    SpeedButton2: TSpeedButton;
    MinifiguresIcanbuild1: TMenuItem;
    N47: TMenuItem;
    OpenItemPU1: TMenuItem;
    N48: TMenuItem;
    Instructionswithunknownweight1: TMenuItem;
    OriginalBoxeswithunknownweight1: TMenuItem;
    N20201: TMenuItem;
    Lugbulk2020items1: TMenuItem;
    N49: TMenuItem;
    LugBulk2020CheapParts1: TMenuItem;
    N50: TMenuItem;
    LugBulk2020CheapBricks1: TMenuItem;
    LugBulk2020CheapPlates1: TMenuItem;
    LugBulk2020CheapTiles1: TMenuItem;
    LugBulk2020CheapSlopes1: TMenuItem;
    LugBulk2020CheapInvertedSlopes1: TMenuItem;
    N20211: TMenuItem;
    Lugbulk2021items1: TMenuItem;
    N51: TMenuItem;
    LugBulk2021CheapParts1: TMenuItem;
    N52: TMenuItem;
    LugBulk2020CheapBricks2: TMenuItem;
    LugBulk2021CheapPlates1: TMenuItem;
    LugBulk2021CheapTiles1: TMenuItem;
    LugBulk2021CheapSlopes1: TMenuItem;
    LugBulk2021CheapInvertedSlopes1: TMenuItem;
    OpenDialog3: TOpenDialog;
    UpdatePriceGuideDisklist1: TMenuItem;
    N53: TMenuItem;
    SaveLugbulkBLCostDialog: TSaveDialog;
    N54: TMenuItem;
    SaveLugbulk2021database1: TMenuItem;
    N55: TMenuItem;
    Readlist1: TMenuItem;
    N56: TMenuItem;
    Crawlerfile1: TMenuItem;
    SaveDialogCrawlerInv: TSaveDialog;
    Plates3x1: TMenuItem;
    Plates16x1: TMenuItem;
    iles3x1: TMenuItem;
    iles4x1: TMenuItem;
    iles6x1: TMenuItem;
    iles8x1: TMenuItem;
    Bricks4x1: TMenuItem;
    Bricks8x1: TMenuItem;
    Brickqueries1: TMenuItem;
    AllBricks1: TMenuItem;
    N57: TMenuItem;
    ilesqueries1: TMenuItem;
    Alltiles1: TMenuItem;
    Allplates1: TMenuItem;
    N58: TMenuItem;
    Commoninventoryof2sets1: TMenuItem;
    Other1: TMenuItem;
    Jumpers1: TMenuItem;
    Connectors1: TMenuItem;
    Doorrail1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure HTMLImageRequest(Sender: TObject; const SRC: String; var Stream: TMemoryStream);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HTMLHotSpotClick(Sender: TObject; const SRC: String;
      var Handled: Boolean);
    procedure HTMLMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure HTMLMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Exit1Click(Sender: TObject);
    procedure Set1Click(Sender: TObject);
    procedure Missingformultiplesets1Click(Sender: TObject);
    procedure Printpreview1Click(Sender: TObject);
    procedure btn_backClick(Sender: TObject);
    procedure btn_fwdClick(Sender: TObject);
    procedure btn_homeClick(Sender: TObject);
    procedure Compare2sets1Click(Sender: TObject);
    procedure Mosaic1Click(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure Piece1Click(Sender: TObject);
    procedure Sets1Click(Sender: TObject);
    procedure btn_saveClick(Sender: TObject);
    procedure SetstobuyNew1Click(Sender: TObject);
    procedure btn_CopyClick(Sender: TObject);
    procedure SetstobuyUsed1Click(Sender: TObject);
    procedure UsedPiecesbelow30euroKgr1Click(Sender: TObject);
    procedure UsedPiecesbelow10euroKgr1Click(Sender: TObject);
    procedure UsedPiecesbelow15euroKgr1Click(Sender: TObject);
    procedure UsedPiecesbelow20euroKgr1Click(Sender: TObject);
    procedure UsedPiecesbelow25euroKgr1Click(Sender: TObject);
    procedure Missingfromstoragebins1Click(Sender: TObject);
    procedure Priceguide1Click(Sender: TObject);
    procedure Partoutguide1Click(Sender: TObject);
    procedure BricklinkOrder1Click(Sender: TObject);
    procedure LUGBULKSuggestions1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Missingfordismandaledsets1Click(Sender: TObject);
    procedure Usedsetstobuiltfromscratch1Click(Sender: TObject);
    procedure Batchlink1Click(Sender: TObject);
    procedure Reloadcache1Click(Sender: TObject);
    procedure CheckCacheHashEfficiency1Click(Sender: TObject);
    procedure Bricks1x1Click(Sender: TObject);
    procedure Bricks2x1Click(Sender: TObject);
    procedure Plates1x1Click(Sender: TObject);
    procedure Plates2x1Click(Sender: TObject);
    procedure N10Click(Sender: TObject);
    procedure Plates6x1Click(Sender: TObject);
    procedure Plates8x1Click(Sender: TObject);
    procedure Tiles1x1Click(Sender: TObject);
    procedure Tiles2x1Click(Sender: TObject);
    procedure Set2Click(Sender: TObject);
    procedure TechnicBricks1x1Click(Sender: TObject);
    procedure LugBulk20171Click(Sender: TObject);
    procedure LugBulk2017CheapBricks1Click(Sender: TObject);
    procedure LugBulk2017CheapSlopes1Click(Sender: TObject);
    procedure LugBulk2017CheapInvertedSlopes1Click(Sender: TObject);
    procedure LugBulk2017CheapPlates1Click(Sender: TObject);
    procedure LugBulk2017CheapTiles1Click(Sender: TObject);
    procedure Specialbricks1x1Click(Sender: TObject);
    procedure Rebrickableparts1Click(Sender: TObject);
    procedure Setswithunknownreleaseyear1Click(Sender: TObject);
    procedure LugBulk2018CheapParts1Click(Sender: TObject);
    procedure LugBulk2018CheapBricks1Click(Sender: TObject);
    procedure LugBulk2018CheapPlates1Click(Sender: TObject);
    procedure LugBulk2018CheapTiles1Click(Sender: TObject);
    procedure LugBulk2018CheapSlopes1Click(Sender: TObject);
    procedure LugBulk2018CheapInvertedSlopes1Click(Sender: TObject);
    procedure S1Click(Sender: TObject);
    procedure Showorderinformation1Click(Sender: TObject);
    procedure Rebrickablesets1Click(Sender: TObject);
    procedure Rebrickablepartsofbuildedsets1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Rebrickablepartsofbuildedmocs1Click(Sender: TObject);
    procedure PlateMosaic1Click(Sender: TObject);
    procedure TileMosaic1Click(Sender: TObject);
    procedure SnotBricks1Click(Sender: TObject);
    procedure UsedPiecesabove1000euroKgr1Click(Sender: TObject);
    procedure UsedPiecesabove5000euroKgr1Click(Sender: TObject);
    procedure UsedPiecesabove10000euroKgr1Click(Sender: TObject);
    procedure NewPiecesbelow10euroKgr1Click(Sender: TObject);
    procedure NewPiecesbelow15euroKgr1Click(Sender: TObject);
    procedure NewPiecesbelow20euroKgr1Click(Sender: TObject);
    procedure NewPiecesbelow25euroKgr1Click(Sender: TObject);
    procedure NewPiecesbelow30euroKgr1Click(Sender: TObject);
    procedure NewPiecesabove1000euroKgr1Click(Sender: TObject);
    procedure NewPiecesabove5000euroKgr1Click(Sender: TObject);
    procedure NewPiecesabove10000euroKgr1Click(Sender: TObject);
    procedure Newcheaperthanused1Click(Sender: TObject);
    procedure Slopes18degrees1Click(Sender: TObject);
    procedure Slopes33degrees1Click(Sender: TObject);
    procedure Slopes45degrees1Click(Sender: TObject);
    procedure Slopes65degrees1Click(Sender: TObject);
    procedure Slopes75degrees1Click(Sender: TObject);
    procedure Newpieceswithpricegraterthan50euro1Click(Sender: TObject);
    procedure Usedpieceswithpricegraterthan50euro1Click(Sender: TObject);
    procedure MostexpensivelotsofmyNEWparts1Click(Sender: TObject);
    procedure MostexpensivelotsofmyUSEDparts1Click(Sender: TObject);
    procedure InvertedSlopes33degrees1Click(Sender: TObject);
    procedure InvertedSlopes45degrees1Click(Sender: TObject);
    procedure InvertedSlopes75degrees1Click(Sender: TObject);
    procedure Checkstoragebinsreport1Click(Sender: TObject);
    procedure HTMLProcessing(Sender: TObject; ProcessingOn: Boolean);
    procedure HTMLProgress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Integer);
    procedure Storage1Click(Sender: TObject);
    procedure Correntunknowncategoriesofmyinventory1Click(Sender: TObject);
    procedure Clearimagecache1Click(Sender: TObject);
    procedure Piecesfirstappearedinyear1Click(Sender: TObject);
    procedure Piecesdiscontinuedatyear1Click(Sender: TObject);
    procedure FirstappearedExcludingvariations1Click(Sender: TObject);
    procedure DiscontinuedAllpartsExcl1Click(Sender: TObject);
    procedure Appersin1set1Click(Sender: TObject);
    procedure Appearsin2sets1Click(Sender: TObject);
    procedure Appearsin3sets1Click(Sender: TObject);
    procedure MostexpensiveofmypartsNEW1Click(Sender: TObject);
    procedure MostexpensiveofmypartsUSED1Click(Sender: TObject);
    procedure SetstobuyforminifigsNEW1Click(Sender: TObject);
    procedure SetstobuyforminifigsUSED1Click(Sender: TObject);
    procedure FirstappearedMinifigureparts1Click(Sender: TObject);
    procedure DiscontinuedMinifigureparts1Click(Sender: TObject);
    procedure Piece2Click(Sender: TObject);
    procedure Rebrickablecsv1Click(Sender: TObject);
    procedure Newpieceswithpricegreaterthan50eurosoldandavailable1Click(
      Sender: TObject);
    procedure Usedpieceswithpricegreaterthan50eurosoldandavailable1Click(
      Sender: TObject);
    procedure Exportunknownpieces1Click(Sender: TObject);
    procedure ClearStubEntries1Click(Sender: TObject);
    procedure Reorganize1Click(Sender: TObject);
    procedure Sets3Click(Sender: TObject);
    procedure Partnamesrebrickable1Click(Sender: TObject);
    procedure Partswithoutknowncolors1Click(Sender: TObject);
    procedure Newismuchmoreexpensivethanused1Click(Sender: TObject);
    procedure Newpartsbricklinkcom1Click(Sender: TObject);
    procedure Pieceswithmorethan30colors1Click(Sender: TObject);
    procedure Nameswithbothpartandsetcolorindexes1Click(Sender: TObject);
    procedure Allitems1Click(Sender: TObject);
    procedure Multipagedisplay1Click(Sender: TObject);
    procedure UsemultipleCPUcores1Click(Sender: TObject);
    procedure Options1Click(Sender: TObject);
    procedure IdleTimerTimer(Sender: TObject);
    procedure btn_CatalogHomeClick(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure HtmlFindClick(Sender: TObject);
    procedure HtmlCopyClick(Sender: TObject);
    procedure HtmlSelectAllClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure CloseTabClick(Sender: TObject);
    procedure TabControl1Changing(Sender: TObject;
      var AllowChange: Boolean);
    procedure TabControl1Change(Sender: TObject);
    procedure HTMLRightClick(Sender: TObject;
      Parameters: TRightClickParameters);
    procedure Openlinkinnewtab1Click(Sender: TObject);
    procedure Openlink1Click(Sender: TObject);
    procedure LugBulks1Click(Sender: TObject);
    procedure Setsbynumpieces1Click(Sender: TObject);
    procedure Setsbynumlots1Click(Sender: TObject);
    procedure ShowSetsLotsBetween(const a, b: integer);
    procedure ShowSetsPartsBetween(const a, b: integer);
    procedure Pieceswithoutupdatethelast10days1Click(Sender: TObject);
    procedure Pieceswithoutupdatethelast30days1Click(Sender: TObject);
    procedure Pieceswithoutupdatethelast90days1Click(Sender: TObject);
    procedure Pieceswithoutupdatethelast365days1Click(Sender: TObject);
    procedure Pieceswithoutupdateinrange1Click(Sender: TObject);
    procedure Lotswith10ormoreparts1Click(Sender: TObject);
    procedure Lotswith50ormoreparts1Click(Sender: TObject);
    procedure Lotswith100ormoreparts1Click(Sender: TObject);
    procedure Lotswith500ormoreparts1Click(Sender: TObject);
    procedure Lotswith1000ormoreparts1Click(Sender: TObject);
    procedure LugBulk2019CheapParts1Click(Sender: TObject);
    procedure LugBulk2019CheapBricks1Click(Sender: TObject);
    procedure LugBulk2019CheapPlates1Click(Sender: TObject);
    procedure LugBulk2019CheapTiles1Click(Sender: TObject);
    procedure LugBulk2019CheapSlopes1Click(Sender: TObject);
    procedure LugBulk2019CheapInvertedSlopes1Click(Sender: TObject);
    procedure ExportPartsColors1Click(Sender: TObject);
    procedure MostitemssoldNew1Click(Sender: TObject);
    procedure MostitemssoldUsed1Click(Sender: TObject);
    procedure MostitemssoldNewUsed1Click(Sender: TObject);
    procedure MoreitemsavailableNew1Click(Sender: TObject);
    procedure MoreitemsavailableUsed1Click(Sender: TObject);
    procedure MoreitemsavailableNewUsed1Click(Sender: TObject);
    procedure MostsetssoldNew1Click(Sender: TObject);
    procedure MostsetssoldUsed1Click(Sender: TObject);
    procedure MostsetssoldNewUsed1Click(Sender: TObject);
    procedure MoresetsavailableNew1Click(Sender: TObject);
    procedure MoresetsavailableUsed1Click(Sender: TObject);
    procedure MoresetsavailableNewUsed1Click(Sender: TObject);
    procedure MostminifigssoldNew1Click(Sender: TObject);
    procedure MostminifigssoldUsed1Click(Sender: TObject);
    procedure MostminifigssoldNewUsed1Click(Sender: TObject);
    procedure MoreminifigsavailableNew1Click(Sender: TObject);
    procedure MoreminifigsavailableUsed1Click(Sender: TObject);
    procedure MoreminifigsavailableNewUsed1Click(Sender: TObject);
    procedure MoldsFirstAppeared1Click(Sender: TObject);
    procedure Moldsdiscontinuedatyear1Click(Sender: TObject);
    procedure CollectionValue1Click(Sender: TObject);
    procedure NewTab1Click(Sender: TObject);
    procedure Lugbulk2017items1Click(Sender: TObject);
    procedure Lugbulk2018items1Click(Sender: TObject);
    procedure Lugbulk2019items1Click(Sender: TObject);
    procedure Comparesets1Click(Sender: TObject);
    procedure Pieceswithunknownweight1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure MinifiguresIcanbuild1Click(Sender: TObject);
    procedure OpenItemPU1Click(Sender: TObject);
    procedure Instructionswithunknownweight1Click(Sender: TObject);
    procedure OriginalBoxeswithunknownweight1Click(Sender: TObject);
    procedure Lugbulk2020items1Click(Sender: TObject);
    procedure LugBulk2020CheapParts1Click(Sender: TObject);
    procedure LugBulk2020CheapBricks1Click(Sender: TObject);
    procedure LugBulk2020CheapPlates1Click(Sender: TObject);
    procedure LugBulk2020CheapTiles1Click(Sender: TObject);
    procedure LugBulk2020CheapInvertedSlopes1Click(Sender: TObject);
    procedure LugBulk2020CheapSlopes1Click(Sender: TObject);
    procedure Lugbulk2021items1Click(Sender: TObject);
    procedure LugBulk2021CheapParts1Click(Sender: TObject);
    procedure LugBulk2020CheapBricks2Click(Sender: TObject);
    procedure LugBulk2021CheapPlates1Click(Sender: TObject);
    procedure LugBulk2021CheapTiles1Click(Sender: TObject);
    procedure LugBulk2021CheapSlopes1Click(Sender: TObject);
    procedure LugBulk2021CheapInvertedSlopes1Click(Sender: TObject);
    procedure UpdatePriceGuideDisklist1Click(Sender: TObject);
    procedure SaveLugbulk2021database1Click(Sender: TObject);
    procedure Readlist1Click(Sender: TObject);
    procedure Crawlerfile1Click(Sender: TObject);
    procedure Plates3x1Click(Sender: TObject);
    procedure Plates16x1Click(Sender: TObject);
    procedure Tiles3x1Click(Sender: TObject);
    procedure Tiles4x1Click(Sender: TObject);
    procedure Tiles6x1Click(Sender: TObject);
    procedure Tiles8x1Click(Sender: TObject);
    procedure Bricks4x1Click(Sender: TObject);
    procedure Bricks8x1Click(Sender: TObject);
    procedure AllBricks1Click(Sender: TObject);
    procedure Alltiles1Click(Sender: TObject);
    procedure Allplates1Click(Sender: TObject);
    procedure Commoninventoryof2sets1Click(Sender: TObject);
    procedure Jumpers1Click(Sender: TObject);
    procedure Connectors1Click(Sender: TObject);
    procedure Doorrail1Click(Sender: TObject);
  private
    { Private declarations }
    streams: TStringList;
    imagerequests: THashStringList;
    initialized: boolean;
    document: TDocument;
    entries: TStringList;
    entriesHash: THashTable;
    goback, gofwd: TStringList;
    progress_string: string;
    Comparesetslist: TStringList;
    Missingformultiplesetslist: TStringList;
    Missingfordismandaledsetslist: TStringList;
    lastset: string;
    dismantaledsetsinv: TBrickInventory;
    diskmirror: string;
    storagebinsupdatetime: double;
    thumbnailcache: array[0..127] of TStringList;
    thumbnailfilesexist: array[0..127] of TStringList;
    newtabUrl: string;
    function CheckAA(const AA, fromAA, toAA: integer): boolean;
    procedure Navigate(const akey: string; const pg: integer);
    procedure DrawColorCell(const cc: integer; const width: integer);
    procedure DrawColorCells(const colors: TDNumberList; const width: integer);
    procedure DrawColorCells2(const colors: TDNumberList; const width: integer);
    procedure doShowLengthQueryColor(inv: TBrickInventory; const id: string; const color: integer; inflst: TStringList);
    procedure doShowLengthQueryColorSlopes(inv: TBrickInventory; const id: string; const color: integer; inflst: TStringList);
    procedure doShowDimentionsQueryColor(inv: TBrickInventory; const id: string; const color: integer; inflst: TStringList);
    procedure doShowLengthQuery(inv: TBrickInventory; const id: string);
    procedure doShowLengthQuerySlopes(inv: TBrickInventory; const id: string);
    procedure doShowDimentionsQuery(inv: TBrickInventory; const id: string);
    procedure dbloadprogress(const s: string; d : Double);
    procedure ShowLooseParts(inv: TBrickInventory; colormask: Integer = -1; partmask: string = ''; cat: Integer = -1;
      const fromAA: integer = -1; const toAA: integer = MAXINT);
    procedure ShowSetInventory(const setid: string; const lite: Boolean = False);
    procedure ShowSetPartsStorage(const setid: string; const ppreview: boolean);
    procedure DrawInventoryPartsStorage(const psinv: TBrickInventory; const ppreview: boolean);
    procedure PreviewInventoryTable(inv: TBrickInventory);
    procedure PreviewSetInventory(const setid: string);
    procedure ShowColors;
    procedure ShowTags;
    procedure ShowTag(const tag: string);
    procedure ShowTagInv(const tag: string);
    procedure ShowCategoryColors(const cat: integer);
    function HtmlDrawInvImgLink(const pcs: string; const color: integer;
      const pi: TPieceInfo): string;
    function HtmlDrawInvCode(const pci: TPieceColorInfo; const extras1, extras2: string): string;
    procedure ShowCategories;
    procedure ShowPiece(pcs: string; const year: integer = -1);
    procedure DoAddNewSetAsPiece(const pcs: string; const desc: string);
    procedure AddNewSetAsPiece(const pcs: string; const desc: string);
    procedure RefreshUnKnownPiecesCategory(const limit: integer);
    procedure RefreshUnKnownSetsCategoryAndWeight(const limit: integer);
    procedure RefreshUnKnownMinifigCategory(const limit: integer);
    procedure RefreshUnKnownInventoryPiecesCategory;
    procedure RefreshUnKnownPiecesWeight(const limit: integer);
    procedure RefreshUnKnownMinifigWeight(const limit: integer);
    function GetAPieceColor(pcs: string): integer;
    procedure DrawMoldList(const tit: string; const lst: TStringList; const splitcolorflags: boolean;
      const donumlinks: boolean; const doctit: string = '');
    procedure DrawMoldListCatalog(const tit: string; const lst: TStringList; const year: integer; const catid: integer; const typ: string);
    procedure ShowCatalogList(const ltyp: string; const year: integer; const catid1: integer; const doall: boolean);
    procedure DrawPieceList(const tit: string; const lst: TStringList; const sortorder: integer = 0;
      const extratit: string = ''; const doctit: string = '');
    procedure DrawPieceListSet(const tit: string; const settit: string; const lst: TStringList);
    procedure DrawPieceListLugbulk(const tit: string; const lst: TStringList);
    procedure DrawPieceListLugbulkKnownCost(const tit: string; const lb: TLugBulk2017; const year: integer;
      const over: double; const dobrickorederinfo: boolean; const catid: Integer = -1);
    procedure DrawSetAlternatePieceList(const tit: string; const lst: TStringList);
    procedure PiecesWithDaysToUpdate(const x: integer);
    procedure PiecesWithDaysToUpdateRange(const ax1, ax2: integer);
    procedure PiecesUnknownWeight;
    procedure InstructionsUnknownWeight;
    procedure BoxesUnknownWeight;
    procedure UsedPiecesbeloweuroKgr(const x: integer);
    procedure UsedPiecesaboveeuroKgr(const x: integer);
    procedure NewPiecesbeloweuroKgr(const x: integer);
    procedure NewPiecesaboveeuroKgr(const x: integer);
    procedure NewPiecesPriceAbove(const x: double);
    procedure UsedPiecesPriceAbove(const x: double);
    procedure NewPiecesPriceAboveEvaluated(const x: double);
    procedure UsedPiecesPriceAboveEvaluated(const x: double);
    procedure PriceGuideQry(const n: integer; const typ: string; const dosoldnew, dosoldused, doavailnew, doavailused: boolean);
    procedure NewPiecesCheaperUsed;
    procedure NewPiecesMuchMoreExpensiveThanUsed(const factor: double);
    procedure PiecesDiscontinuedAtYear(const y: integer);
    procedure PiecesDiscontinuedAtYearExcludingVariations(const y: integer);
    procedure PiecesDiscontinuedAtYear_Minifigure(const y: integer);
    procedure PiecesNewAtYear(const y: integer);
    procedure PiecesNewAtYearExcludingVariations(const y: integer);
    procedure PiecesNewAtYear_Minifigure(const y: integer);
    procedure MoldsYearQry(const y: integer; const firstappeared: boolean);
    procedure ShowPieceCInventory(const pcs: string; const color: integer);
    procedure ShowColorPiece(const apcs: string; const color: integer; const ayear: integer = -1; const doshowsets: boolean = True);
    procedure ShowInstructions(const apcs: string);
    procedure ShowSetsICanBuild(const pct: double; const dosets, dofigs: boolean);
    procedure ShowSetsAtYear(const year: integer);
    procedure ShowSetsAtUnknownYear;
    procedure UpdateUnknownYearFromDisk(const qry: string);
    procedure UpdateAllPartsUnknownYear;
    procedure UpdateAllItemsUnknownDesc;
    procedure UpdatePartFromDisk(const spart: string);
    procedure UpdateSetAssetsFromBricklink(const s: string);
    procedure ShowSetsDataCompare(const tit: string; const setlist: TStringList);
    procedure ShowSetsForPartOutNew(const minyear, minavailablelots: integer;
                                    const mindemand, mincostmultiplier: double);
    procedure ShowSetsForPartOutUsed(const minyear, minavailablelots: integer;
                                    const mindemand, mincostmultiplier: double);
    procedure ShowSetsForPartInUsed(const posost: integer);
    procedure ShowSetsForPartOutWithMiniFigsNew(const minyear, minavailablelots, minminifignum: integer;
                                                const mindemand, minpartscostmultiplier, minminifigscostmultiplier, minpartoutmultiplier: double);
    procedure ShowSetsForPartOutWithMiniFigsUsed(const minyear, minavailablelots, minminifignum: integer;
                                                const mindemand, minpartscostmultiplier, minminifigscostmultiplier, minpartoutmultiplier: double);
    procedure ShowSetStatsByNumPieces;
    procedure ShowSetStatsByNumPieces2(const x: integer);
    procedure ShowSetStatsByNumLots;
    procedure ShowMissingFromStorageBins;
    procedure ShowCheckStorageReport;
    procedure ShowMissingToBuildSetInventory(const setid: string; const numsets: integer; legacyignore: boolean);
    procedure ShowExpensiveSetLotsNew(const setid: string; const numlots: integer);
    procedure ShowExpensiveSetLotsUsed(const setid: string; const numlots: integer);
    procedure ShowExpensiveInvNew(const atitle: string; const numlots: integer);
    procedure ShowExpensiveInvUsed(const atitle: string; const numlots: integer);
    procedure ShowExpensiveInvPartsNew(const atitle: string; const numlots: integer);
    procedure ShowExpensiveInvPartsUsed(const atitle: string; const numlots: integer);
    procedure ShowBigInvLots(const lotparts: integer);
    procedure ShowMissingToBuilMultipledSets(const setids: TStringList);
    procedure ShowInventoryForMultipledSets(const setids: TStringList);
    procedure ShowStorageLocationsForMultipledSets(const setids: TStringList; const ppreview: boolean);
    procedure ShowLugbulkSuggestions(const years: string; const demand, sold: integer; const price: double);
    procedure ShowCompare2Sets(const set1, set2: string);
    procedure ShowCommon2Sets(const set1, set2: string);
    procedure ShowOrders(const seller: string = '');
    procedure ShowOrder(const orderid: string);
    procedure DrawOrderInf(const orderid: string);
    procedure ShowLugbulk(const year: string; const catid: integer  = -1);
    procedure ShowLugbulkBestPrice(const year: string; const over: double; const catid: integer  = -1);
    procedure ShowLugbulkBestPriceNoBrickOrder(const year: string; const over: double; const catid: integer  = -1);
    procedure ShowStorageBins;
    procedure ShowStorageInventory(const st: string);
    procedure ShowMyMinifigInventory(const doloose, doofsets, domocs: boolean);
    procedure ShowHomePage;
    function ShowInventorySets(const inv: TBrickInventory; const header_flash: boolean; const mocflag: integer): boolean;
    procedure ShowMySetsAndMocs;
    procedure ShowMyMocs;
    procedure ShowMyOfficialSets;
    procedure ShowMySetsPieces;
    procedure ShowMyMocsPieces;
    procedure ShowMyMinifiguresMenu;
    procedure ShowMyPiecesValue;
    procedure ShowLengthQuery(const id: string);
    procedure ShowLengthQuerySlopes(const id: string);
    procedure ShowDimentionsQuery(const id: string);
    procedure DrawNavigateBar;
    procedure DrawNavigateCatalog;
    procedure DrawHeadLine(const s: string);
    procedure DrawHeadLine2(const s1, s2: string);
    procedure DrawHeadLineN(const ll: TStringList);
    procedure DrawPartOutValueWithOutSets(inv: TBrickInventory; const setid: string = '');
    procedure DrawPartOutValue(inv: TBrickInventory; const setid: string = '');
    procedure DrawInventoryTable(inv: TBrickInventory; const lite: Boolean = False;
      const setid: string = ''; const dosort: boolean = True; const usepages: boolean = True;
      const lb: TLugBulk2017 = nil; const dosplash: boolean = true; const showreadylist: boolean = false);
    procedure DrawInventoryTableNoPages(inv: TBrickInventory; const lite: Boolean = False; const setid: string = ''; const dosort: boolean = True; const dosplash: boolean = true);
    procedure DrawInventoryTableForPartsStorageQuery(inv, qryinv: TBrickInventory; const ppreview: boolean);
    procedure DrawBrickOrderInfo(const brick: brickpool_p; const setid: string = ''; const aspan1: integer = -1; const aspan2: integer = -1; const showreadylist: boolean = false);
    procedure DrawBrickOrderInfoLite(const brick: brickpool_p; const needed: integer; const setid: string = '');
    procedure UpdateDismantaledsetsinv;
    procedure DrawPriceguide(const part: string; const color: integer = -1);
    procedure DrawPriceguideEx(const part: string; const color: integer; const at: TDateTime);
    procedure HTMLClick(const SRC1: String; var Handled: Boolean);
    procedure AdjustStreamsSize;
    procedure doHTMLClick(const SRC1: String; var Handled: Boolean);
    procedure StoreInventoryStatsRec(const piece: string; const color: string = '');
    procedure DoEditSet(const setid: string);
    procedure ShowUniquePiecesOfMyInventory(const ntimes: integer);
    procedure ShowMoldsWithNumColors(const ncolors: integer);
    procedure ShowMoldsWithMoreThanColors(const ncolors: integer);
    procedure ShowMoldsWithNumColorsBetween(const mincolors, maxcolors: integer);
    procedure ShowNameswithbothpartandsetcolorindexes;
    procedure ShowChildMolds(const basepcs: string);
    procedure ShowFamilyMolds(const basepcs: string);
    procedure ShowMoldVariations(const basepcs: string);
    procedure ShowPieceAlternates(const basepcs: string);
    procedure ShowPiecePatterns(const basepcs: string);
    procedure ShowPiecePrints(const basepcs: string);
    function MakeThumbnailImageEx(const pcs1: string; const typof: char; const ncolor: integer = -1000): string;
    function MakeThumbnailImageExCache(const pcs: string; const typof: char; const ncolor: integer = -1000): string;
    function ConvertThumbnailImageExCache(const imgfile: string): boolean;
    function MakeThumbnailImage(const pcs: string; const ncolor: integer = -1000): string;
    function MakeThumbnailImage2(const pcs: string; const ncolor: integer = -1000): string;
    function FindThumbnailImageFileName(const SRC: string): string;
    function FindThumbnailImageFileNameForHtmlReq(const SRC: string): string;
    procedure StoreTab;
    procedure RestoreTab;
    function CanOpenInNewTab(const surl: string): boolean;
    procedure ErrorBeep;
    function DoOpenUrlInNewTable(const aUrl: string): boolean;
    function GetRebrickableColorHtml(const cl: integer): string;
    function AutoCorrectUnknownPieceYears: boolean;
    procedure DoUpdateInstructionsFromNet(const sset: string);
    procedure DoUpdateInstructionsFromNetHost(const sset: string; const host: string);
    procedure DoUpdateInstructionsFromPdf(const sset: string);
    function RemoveImageFromCache(const simg: string): boolean;
    procedure DoUpdatePriceGuideFromDiskList(const fn: string; const days: integer);
    procedure SaveLugbulkDatabase(const fn: string; const year: integer);
    procedure SortInventory(const ainv: TBrickInventory);
    function taglink(const tag: string): string;
    function lugbulklinks(const pci: TPieceColorInfo): string;
    function GetSetCostDbl(const setid: string): double;
    function GetItemCostDbl(const apart: string; const acolor: integer): double;
  public
    { Public declarations }
    activebits: integer;
  end;

var
  MainForm: TMainForm;

implementation

uses
  DateUtils, ShellApi, bi_pak, bi_io, bi_system, bi_tmp, slpash, bi_utils,
  timing, searchset, searchpart, frm_multiplesets, PreviewForm, bl_orderxml,
  compare2sets, mosaicfrm, mosaicfrm_plates, mosaicfrm_tiles, editpiecefrm,
  removepiecefromstoragefrm, strutils, frm_diagrams, frm_setsforpartout_params,
  frm_selectsets, searchstorage, frm_batch, bi_globals, frm_lugbulksuggest,
  frm_editsetastext, frm_setsminifigspartout_params, editmoldfrm, frm_update1,
  frm_update2, frm_update3, frm_update4, bi_cachefile, frm_editlugbulkprice,
  frm_options, bi_multithread, bi_iterators, bi_defs, bi_crawler, bi_script,
  buildinexcludes, bi_instructions, frm_pdfinstructions, bi_data,
  bi_imagerotate, bi_readylist;

{$R *.dfm}

const
  SORT_NONE = 0;
  SORT_PRICE_NEW = 1;
  SORT_PRICE_USED = 2;
  SORT_DATE_UPDATE = 3;
  SORT_ITEMS_CINTEGER = 4;

type
  TThumbnailCacheInfo = class(TObject)
  private
    fpcs: string;
    ftypof: char;
    fncolor: integer;
  public
    constructor Create(const apcs: string; const atypof: char; const ancolor: integer);
    property pcs: string read fpcs;
    property typof: char read ftypof;
    property ncolor: integer read fncolor;
  end;

constructor TThumbnailCacheInfo.Create(const apcs: string; const atypof: char; const ancolor: integer);
begin
  fpcs := apcs;
  ftypof := atypof;
  fncolor := ancolor;
  Inherited Create;
end;

type
  TScrollPos = class(Tobject)
    x, y: integer;
    constructor Create(const ax, ay: integer);
  end;

constructor TScrollPos.Create(const ax, ay: integer);
begin
  Inherited Create;
  x := ax;
  y := ay;
end;


const
  MAXNUMTABS = 50;

type
  TTabItem = class(TComponent)
  protected
    fgoback, fgofwd: TStringList;
    fAddress: string;
    fTitle: string;
    fHtmlString: string;
    fHScroll, fVScroll: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Store(const HTML: THTMLViewer; const aaddress: string; const agoback, agofwd: TStringList);
    procedure Restore(const HTML: THTMLViewer; const aEdit: TEdit; const agoback, agofwd: TStringList);
    property goback: TStringList read fgoback;
    property gofwd: TStringList read fgofwd;
    property Title: string read fTitle write fTitle;
    property HtmlString: string read fHtmlString write fHtmlString;
    property Address: string read fAddress write fAddress;
  end;

constructor TTabItem.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  fgoback := TStringList.Create;
  fgofwd := TStringList.Create;
  fAddress := '';
  fTitle := '';
  fHtmlString := '';
  fHScroll := 0;
  fVScroll := 0;
end;

destructor TTabItem.Destroy;
begin
  FreeList(fgoback);
  FreeList(fgofwd);
  fTitle := '';
  fHtmlString := '';
  Inherited;
end;

procedure TTabItem.Store(const HTML: THTMLViewer; const aaddress: string; const agoback, agofwd: TStringList);
var
  i: integer;
begin
  fAddress := aaddress;
  for i := 0 to fgoback.Count - 1 do
    fgoback.Objects[i].Free;
  fgoback.Clear;
  for i := 0 to agoback.Count - 1 do
    fgoback.AddObject(agoback.Strings[i],
      TScrollPos.Create((agoback.Objects[i] as TScrollPos).x, (agoback.Objects[i] as TScrollPos).y));

  for i := 0 to fgofwd.Count - 1 do
    fgofwd.Objects[i].Free;
  fgofwd.Clear;
  for i := 0 to agofwd.Count - 1 do
    fgofwd.AddObject(agofwd.Strings[i],
      TScrollPos.Create((agofwd.Objects[i] as TScrollPos).x, (agofwd.Objects[i] as TScrollPos).y));

  fTitle := HTML.DocumentTitle;
  fHtmlString := HTML.DocumentSource;
  fHScroll := HTML.HScrollBarPosition;
  fVScroll := HTML.VScrollBarPosition;
end;

procedure TTabItem.Restore(const HTML: THTMLViewer; const aEdit: TEdit; const agoback, agofwd: TStringList);
var
  i: integer;
  dd: TDocument;
begin
  aEdit.Text := fAddress;
  for i := 0 to agoback.Count - 1 do
    agoback.Objects[i].Free;
  agoback.Clear;
  for i := 0 to fgoback.Count - 1 do
    agoback.AddObject(fgoback.Strings[i],
      TScrollPos.Create((fgoback.Objects[i] as TScrollPos).x, (fgoback.Objects[i] as TScrollPos).y));

  for i := 0 to agofwd.Count - 1 do
    agofwd.Objects[i].Free;
  agofwd.Clear;
  for i := 0 to fgofwd.Count - 1 do
    agofwd.AddObject(fgofwd.Strings[i],
      TScrollPos.Create((fgofwd.Objects[i] as TScrollPos).x, (fgofwd.Objects[i] as TScrollPos).y));

  dd := TDocument.Create(HTML);
  try
    dd.write(fHtmlString);
    dd.Flash;
  finally
    dd.Free;
  end;
  HTML.HScrollBarPosition := fHScroll;
  HTML.VScrollBarPosition := fVScroll;
end;

const
  TBGCOLOR = '#FFFFFF'; // Table Background color
  THBGCOLOR = '#E0E0E0'; // Table Header background color
  TFGCOLOR = '#202020'; // Table font color
  DBGCOLOR = '#F7E967'; // Div (HEADER) background color
  DFGCOLOR = '#000080'; // Div (HEADER) font color

const
  MAXOUTPUTMEMOLINES = 200;

procedure outprocmemo(const s: string);
var
  sl: TStringList;
begin
  try
    sl := TStringList.Create;
    sl.Text := MainForm.OutputMemo.Lines.Text + s;
    while sl.Count > MAXOUTPUTMEMOLINES do
      sl.Delete(0);
    MainForm.OutputMemo.Lines.Text := sl.Text;
    sl.Free;
    fprintf(stdout, s);
  except
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  s1: string;
  i: integer;
begin
//  I_InitializeIO;
//  I_InitTempFiles;
  I_Init;
  MT_Init;
  SplashForm := TSplashForm.Create(nil);
  lastset := '';
  diskmirror := '';
  outproc := outprocmemo;
  printf('Starting BrickInventory...'#13#10);
  BI_LoadDefaults(basedefault + 'bi4.ini');
  activebits := 0;
  storagebinsupdatetime := 0.0;
  newtabUrl := '';

  dismantaledsetsinv := nil;

  for i := 0 to 127 do
  begin
    thumbnailcache[i] := TStringList.Create;
    thumbnailcache[i].Sorted := True;
  end;
  for i := 0 to 127 do
  begin
    thumbnailfilesexist[i] := TStringList.Create;
    thumbnailfilesexist[i].Sorted := True;
  end;
  streams := TStringList.Create;
  imagerequests := THashStringList.Create;
  entries := TStringList.Create;
  entriesHash := THashTable.Create;
  goback := TStringList.Create;
  gofwd := TStringList.Create;

  Comparesetslist := TStringList.Create;
  s1 := basedefault + 'out\Comparesets\Comparesetslist.txt';
  if fexists(s1) then
    Comparesetslist.LoadFromFile(s1);

  Missingformultiplesetslist := TStringList.Create;
  s1 := basedefault + 'out\multisetsquery\missing_multisetsquery_sets.txt';
  if fexists(s1) then
    Missingformultiplesetslist.LoadFromFile(s1);

  Missingfordismandaledsetslist := TStringList.Create;
  s1 := basedefault + 'out\dismandaledsetsquery\missing_dismandaledsetsquery_sets.txt';
  if fexists(s1) then
    Missingfordismandaledsetslist.LoadFromFile(s1);

  document := TDocument.Create(HTML);

  orders := TOrders.Create;

  SaveDialogPriceGuide.InitialDir := basedefault;
  SaveDialogPriceGuide.Filename := basedefault + 'dbexport_priceguide.txt';

  SaveDialogPartOut.InitialDir := basedefault;
  SaveDialogPartOut.Filename := basedefault + 'dbexport_partout.txt';

  SaveDialogDatabase.InitialDir := basedefault;
  SaveDialogDatabase.Filename := basedefault + 'dbexport_database.txt';

  SaveDialogPartsColors.InitialDir := basedefault;
  SaveDialogPartsColors.Filename := basedefault + 'dbexport_parts_colors.txt';

  Application.OnIdle := nil;
  IdleTimer.Enabled := True;
end;

procedure TMainForm.Navigate(const akey: string; const pg: integer);
begin
  Screen.Cursor := crHourglass;
  try
    document.FlashMultiPageDocument(akey, pg);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.DrawColorCells(const colors: TDNumberList; const width: integer);
var
  i: integer;
  cc: integer;
begin
  document.Write('<table border=1><tr>');
  for i := 0 to colors.Count - 1 do
  begin
    cc := colors.Numbers[i];
    case cc of
      -1, 9999:
        document.Write('<td><table border=1 width=' + IntToStr(width) + ' bgcolor="#' + IntToHex(db.colors(cc).RGB, 6) + '"><tr><td><p align=center><font color=#FFFFFF><b>-</b></font></p></td></tr></table></td>');
      9996:
        document.Write('<td><table border=1 width=' + IntToStr(width) + ' bgcolor="#' + IntToHex(db.colors(cc).RGB, 6) + '"><tr><td><p align=center><font color=#FFFFFF><b>C</b></font></p></td></tr></table></td>');
      9997:
        document.Write('<td><table border=1 width=' + IntToStr(width) + ' bgcolor="#' + IntToHex(db.colors(cc).RGB, 6) + '"><tr><td><p align=center><font color=#FFFFFF><b>I</b></font></p></td></tr></table></td>');
      9998:
        document.Write('<td><table border=1 width=' + IntToStr(width) + ' bgcolor="#' + IntToHex(db.colors(cc).RGB, 6) + '"><tr><td><p align=center><font color=#FFFFFF><b>O</b></font></p></td></tr></table></td>');
      else
        document.Write('<td><table border=1 width=' + IntToStr(width) + ' bgcolor="#' + IntToHex(db.colors(cc).RGB, 6) + '"><tr><td><p align=center><font color=#' + IntToHex(db.colors(cc).RGB, 6) + '><b>.</b></font></p></td></tr></table></td>');
    end;

    if i > 0 then
      if i < colors.Count - 1 then
        if (i + 1) mod 20 = 0 then
        begin
          document.Write('</tr></table>');
          document.Write('<table border=1><tr>');
        end;
  end;
  document.Write('</tr></table>');
end;

procedure TMainForm.DrawColorCells2(const colors: TDNumberList; const width: integer);
var
  i, x: integer;
  cc: integer;
  cinfo: colorinfo_p;
begin
  document.Write('<table border=1><tr>');
  x := 0;
  for i := 0 to colors.Count - 1 do
  begin
    cc := colors.Numbers[i];
    inc(x);
    case cc of
      -1, 9999:
        document.Write('<td><table border=1 width=' + IntToStr(width) + ' bgcolor="#' + IntToHex(db.colors(cc).RGB, 6) + '"><tr><td><p align=center><font color=#FFFFFF><b>-</b></font></p></td></tr></table></td>');
      9996:
        document.Write('<td><table border=1 width=' + IntToStr(width) + ' bgcolor="#' + IntToHex(db.colors(cc).RGB, 6) + '"><tr><td><p align=center><font color=#FFFFFF><b>C</b></font></p></td></tr></table></td>');
      9997:
        document.Write('<td><table border=1 width=' + IntToStr(width) + ' bgcolor="#' + IntToHex(db.colors(cc).RGB, 6) + '"><tr><td><p align=center><font color=#FFFFFF><b>I</b></font></p></td></tr></table></td>');
      9998:
        document.Write('<td><table border=1 width=' + IntToStr(width) + ' bgcolor="#' + IntToHex(db.colors(cc).RGB, 6) + '"><tr><td><p align=center><font color=#FFFFFF><b>O</b></font></p></td></tr></table></td>');
      else
        dec(x);
    end;

    if i > 0 then
      if i < colors.Count - 1 then
        if (i + 1) mod 20 = 0 then
  end;
  if x > 0 then
  begin
    document.Write('</tr></table>');
    document.Write('<table border=1><tr>');
  end;
  for i := 0 to colors.Count - 1 do
  begin
    cc := colors.Numbers[i];
    inc(x);
    case cc of
      -1, 9996, 9997, 9998, 9999:
        dec(x);
      else
      begin
        cinfo := db.colors(cc);
        document.Write('<td><table border=1 width=' + IntToStr(width) + ' bgcolor="#' + IntToHex(cinfo.RGB, 6) + '"><tr><td><p align=center><font color=#' + IntToHex(cinfo.RGB, 6) + '><b>.</b></font></p></td></tr></table></td>');
      end;
    end;

    if x > 0 then
      if i < colors.Count - 1 then
        if x mod 20 = 0 then
        begin
          document.Write('</tr></table>');
          document.Write('<table border=1><tr>');
          x := 0;
        end;
  end;

  document.Write('</tr></table>');
end;

procedure TMainForm.DrawColorCell(const cc: integer; const width: integer);
begin
  case cc of
    -1, 9999:
      document.Write('<table border=1 width=' + IntToStr(width) + ' bgcolor="#' + IntToHex(db.colors(cc).RGB, 6) + '"><tr><td><p align=center><font color=#FFFFFF><b>-</b></font></p></td></tr></table>');
    9996:
      document.Write('<table border=1 width=' + IntToStr(width) + ' bgcolor="#' + IntToHex(db.colors(cc).RGB, 6) + '"><tr><td><p align=center><font color=#FFFFFF><b>C</b></font></p></td></tr></table>');
    9997:
      document.Write('<table border=1 width=' + IntToStr(width) + ' bgcolor="#' + IntToHex(db.colors(cc).RGB, 6) + '"><tr><td><p align=center><font color=#FFFFFF><b>I</b></font></p></td></tr></table>');
    9998:
      document.Write('<table border=1 width=' + IntToStr(width) + ' bgcolor="#' + IntToHex(db.colors(cc).RGB, 6) + '"><tr><td><p align=center><font color=#FFFFFF><b>O</b></font></p></td></tr></table>');
    else
      document.BlancColorCell(db.colors(cc).RGB, width);
  end;
end;

procedure TMainForm.DrawNavigateBar;
begin
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2><tr>');

  document.write('<td width=8%><a href="home">Home</a></td>');
  document.write('<td width=10%><a href="cataloghome">Catalog</a></td>');
  document.write('<td width=10%><a href="inv/0/C/-1">My loose parts</a></td>');
  document.write('<td width=14%><a href="mysetsandmocs">My official sets and mocs</a></td>');
  document.write('<td width=10%><a href="ShowMyMinifiguresMenu">My minifigures</a></td>');
  document.write('<td width=8%><a href="colors">Colors</a></td>');
  document.write('<td width=10%><a href="categories">Categories</a></td>');
  document.write('<td width=10%><a href="orders">Orders</a></td>');
  document.write('<td width=10%><a href="ShowStorageBins">Storage Bins</a></td>');
  document.write('<td width=10%><a href="tags">Tags</a></td>');

  document.write('</tr></table></p></div><br><br>');

end;

procedure TMainForm.DrawNavigateCatalog;
begin
  document.write('<body background="splash.jpg">');
  document.title('Catalog');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  DrawHeadLine('Bricks Inventory - Catalog');
  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');

  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>Quick links</b></th>');
  document.write('</tr>');

  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="ShowCatalogList//-1/-1">All Items</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="catalogparts">Parts</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="catalogpartsinv">Parts with inventory</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="catalogsets">Sets</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="catalogsetsnoinv">Sets without inventory</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="catalogminifigures">Minifigures</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="catalogmocs">Mocs</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="cataloginstructions">Instructions</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="catalogboxes">Original Boxes</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="cataloggears">Gears</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="catalogbooks">Books</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="catalogcatalogs">Catalogs</a></td></tr>');

  document.write('</table></p></div></body>');

  document.SaveBufferToFile(diskmirror);
  document.Flash;

end;

procedure TMainForm.DrawHeadLine(const s: string);
begin
  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + DBGCOLOR + '>');
  document.write('<td width 100%>');
  document.write('<font color=' + DFGCOLOR + '>');
  document.write('<h3 align=center>' + s + '</h3>');
  document.write('</font>');
  document.write('</td></tr></table>');
end;

procedure TMainForm.DrawHeadLine2(const s1, s2: string);
begin
  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + DBGCOLOR + '>');

  document.write('<td width 50%>');
  document.write('<font color=' + DFGCOLOR + '>');
  document.write('<h3 align=center>' + s1 + '</h3>');
  document.write('</font>');
  document.write('</td>');

  document.write('<td width 50%>');
  document.write('<font color=' + DFGCOLOR + '>');
  document.write('<h3 align=center>' + s2 + '</h3>');
  document.write('</font>');
  document.write('</td>');

  document.write('</tr></table>');
end;

procedure TMainForm.DrawHeadLineN(const ll: TStringList);
var
  i: integer;
  ww: integer;
begin
  if ll.Count = 0 then
    Exit;
  if ll.Count = 1 then
    DrawHeadLine((ll.Objects[0] as TString).text)
  else if ll.Count = 2 then
    DrawHeadLine2((ll.Objects[0] as TString).text, (ll.Objects[1] as TString).text)
  else
  begin
    ww := 100 div ll.Count;
    document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
    document.write('<tr bgcolor=' + DBGCOLOR + '>');
    for i := 0 to ll.Count - 1 do
    begin
      if i = 0 then
        document.write('<td width=' + itoa(100 - (ll.Count - 1) * ww) + '%>')
      else
        document.write('<td width=' + itoa(ww) + '%>');
      document.write('<font color=' + DFGCOLOR + '>');
      document.write('<h3 align=center>' + (ll.Objects[i] as TString).text + '</h3>');
      document.write('</font>');
      document.write('</td>');
    end;
    document.write('</tr></table>');
  end;
end;

procedure TMainForm.DrawOrderInf(const orderid: string);
var
  i, j: integer;
  tot, grantot: Double;
  eval: Double;
  curconv: Double;
  oo: IXMLORDERType;
  check: integer;
begin
  check := StrToIntDef(orderid, -1);
  if check < 0 then
    Exit;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');

  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>Date</b></th>');
  document.write('<th><b>Seller</b></th>');
  document.write('<th><b>Status</b></th>');
  document.write('<th><b>Lots</b></th>');
  document.write('<th><b>Items</b></th>');
  document.write('<th><b>Total</b></th>');
  document.write('<th><b>Gran Total</b></th>');
  document.write('<th><b>Eval</b></th>');
  document.write('</tr>');

  for i := 0 to orders.numorders - 1 do
    for j := 0 to orders.orders[i].Count - 1 do
    begin
      oo := orders.orders[i].ORDER[j];
      if oo.ORDERID <> check then
        Continue;
      document.write('<tr bgcolor=' + TBGCOLOR + '>');
      document.write('<td width=10% align=right>' + oo.ORDERDATE + '</td>');
      document.write('<td width=19%><a href=sellerorders/' + oo.SELLER + '>' + oo.SELLER + '</a></td>');
      if oo.ORDERSTATUS = 'NSS' then
        document.write('<td width=10%><font color="red">' + oo.ORDERSTATUS + '</font></td>')
      else if (oo.ORDERSTATUS = 'Canceled') or (oo.ORDERSTATUS = 'Cancelled') then
        document.write('<td width=10%><font color="red">' + oo.ORDERSTATUS + '</font></td>')
      else if (oo.ORDERSTATUS = 'Completed') or (oo.ORDERSTATUS = 'Received') then
        document.write('<td width=10%><font color="green">' + oo.ORDERSTATUS + '</font></td>')
      else
        document.write('<td width=10%>' + oo.ORDERSTATUS + '</td>');

      document.write('<td width=8% align=right>' + itoa(oo.ORDERLOTS) + '</td>');
      document.write('<td width=8% align=right>' + itoa(oo.ORDERITEMS) + '</td>');

      curconv := db.ConvertCurrency(oo.BASECURRENCYCODE);
      tot := atof(oo.ORDERTOTAL) * curconv;
      grantot := atof(oo.BASEGRANDTOTAL) * curconv;

      if curconv = 1.0 then
      begin
        document.write('<td width=15% align=right>' + Format('€ %2.3f', [tot]) + '</td>');
        document.write('<td width=15% align=right>' + Format('€ %2.3f', [grantot]) + '</td>');
      end
      else
      begin
        document.write('<td width=15% align=right>' + Format('*€ %2.3f', [tot]) + '</td>');
        document.write('<td width=15% align=right>' + Format('*€ %2.3f', [grantot]) + '</td>');
      end;
      eval := EvaluatedPrice(oo);
      document.write('<td width=15% align=right>' + Format('€ %2.3f', [eval]) + '</td>');
      document.write('</tr></table>');
      Exit;
    end;
  document.write('</table>');
end;

procedure TMainForm.ShowOrders(const seller: string = '');
var
  aa, i, j: integer;
  tot, grantot: Double;
  sum, gransum: Double;
  eval, evalsum: Double;
  curconv: Double;
  numitems, numlots: integer;
  oo: IXMLORDERType;
  orderslist: TDNumberList;
  sorder: string;
begin
  Screen.Cursor := crHourglass;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowOrders', seller);

  document.write('<body background="splash.jpg">');
  if seller = '' then
    document.title('Orders')
  else
    document.title('Orders (' + seller + ')');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  DrawHeadLine('Orders');

  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');

  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>*</b></th>');
  document.write('<th><b>OrderID</b></th>');
  document.write('<th><b>Date</b></th>');
  document.write('<th><b>Seller</b></th>');
  document.write('<th><b>Status</b></th>');
  document.write('<th><b>Lots</b></th>');
  document.write('<th><b>Items</b></th>');
  document.write('<th><b>Total</b></th>');
  document.write('<th><b>Gran Total</b></th>');
  document.write('<th><b>Eval</b></th>');
  document.write('</tr>');

  aa := 0;
  sum := 0.0;
  gransum := 0.0;
  evalsum := 0.0;
  numitems := 0;
  numlots := 0;

  ShowSplash;

  orderslist := TDNumberList.Create;
  for i := 0 to orders.numorders - 1 do
    for j := 0 to orders.orders[i].Count - 1 do
      if (seller = '') or (orders.orders[i].ORDER[j].SELLER = seller) then
        orderslist.Add(orders.orders[i].ORDER[j].ORDERID);

  orderslist.Sort;

  for i := 0 to orderslist.Count - 1 do
  begin
    SplashProgress('Working...', i / orderslist.Count);
    inc(aa);
    document.StartItemId(aa);
    sorder := itoa(orderslist.Numbers[i]);
    orders.StoreEvalHistory(basedefault + 'orders\' + sorder + '.eval', sorder);
    oo := orders.order(sorder);
    document.write('<tr bgcolor=' + TBGCOLOR + '>');
    document.write('<td width=5% align=right>' + IntToStr(aa) + '.</td>');
    document.write('<td width=10%><a href=order/' + itoa(oo.ORDERID) + '>' + itoa(oo.ORDERID) + '</a></td>');
    document.write('<td width=10% align=right>' + oo.ORDERDATE + '</td>');
    if seller = '' then
      document.write('<td width=19%><a href=sellerorders/' + oo.SELLER + '>' + oo.SELLER + '</a></td>')
    else
      document.write('<td width=19%>' + oo.SELLER + '</td>');
    if oo.ORDERSTATUS = 'NSS' then
      document.write('<td width=10%><font color="red">' + oo.ORDERSTATUS + '</font></td>')
    else if (oo.ORDERSTATUS = 'Canceled') or (oo.ORDERSTATUS = 'Cancelled') then
      document.write('<td width=10%><font color="red">' + oo.ORDERSTATUS + '</font></td>')
    else if (oo.ORDERSTATUS = 'Completed') or (oo.ORDERSTATUS = 'Received') then
      document.write('<td width=10%><font color="green">' + oo.ORDERSTATUS + '</font></td>')
    else
      document.write('<td width=10%>' + oo.ORDERSTATUS + '</td>');

    document.write('<td width=8% align=right>' + itoa(oo.ORDERLOTS) + '</td>');
    numlots := numlots + oo.ORDERLOTS;
    document.write('<td width=8% align=right>' + itoa(oo.ORDERITEMS) + '</td>');
    numitems := numitems + oo.ORDERITEMS;

    curconv := db.ConvertCurrency(oo.BASECURRENCYCODE);
    tot := atof(oo.ORDERTOTAL) * curconv;
    grantot := atof(oo.BASEGRANDTOTAL) * curconv;
    sum := sum + tot;
    gransum := gransum + grantot;

    if curconv = 1.0 then
    begin
      document.write('<td width=15% align=right>' + Format('€ %2.3f', [tot]) + '</td>');
      document.write('<td width=15% align=right>' + Format('€ %2.3f', [grantot]) + '</td>');
    end
    else
    begin
      document.write('<td width=15% align=right>' + Format('*€ %2.3f', [tot]) + '</td>');
      document.write('<td width=15% align=right>' + Format('*€ %2.3f', [grantot]) + '</td>');
    end;
    eval := EvaluatedPrice(oo);
    document.write('<td width=15% align=right>' + Format('€ %2.3f', [eval]) + '</td>');
    evalsum := evalsum + eval;
    document.write('</tr>');
  end;

  document.EndNavigateSection;

  orderslist.Free;

  document.write('<tr bgcolor=' + TBGCOLOR + '>');
  document.write('<td></td>');
  document.write('<td colspan="4">Total</td>');

  document.write('<td width=8% align=right>' + itoa(numlots) + '</td>');
  document.write('<td width=8% align=right>' + itoa(numitems) + '</td>');
  document.write('<td width=15% align=right>' + Format('€ %2.3f', [sum]) + '</td>');
  document.write('<td width=15% align=right>' + Format('€ %2.3f', [gransum]) + '</td>');
  document.write('<td width=15% align=right>' + Format('€ %2.3f', [evalsum]) + '</td>');
  document.write('</tr></table>');

  document.MarkBottomNavigateSection;

  document.write('</p></div></body>');

  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowStorageBins;
var
  storages: TStringList;
  aa, i: integer;
  eval, evalsum: Double;
  numitems, numlots: integer;
  st: string;
  inv: TBrickInventory;
  w, tw: double;
  storestats: boolean;
begin
  Screen.Cursor := crHourglass;

  if Now - storagebinsupdatetime > 0.1 then
  begin
    storestats := True;
    storagebinsupdatetime := Now;
  end
  else
    storestats := False;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowStorageBins', itoa(db.StorageBins.count));

  document.write('<body background="splash.jpg">');
  document.title('Storage Bins');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  DrawHeadLine('<a href=storage/>Storage Bins</a> <a href="diagramstorage/Storage Bins"><img src="images\diagram.png">');

  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');

  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>*</b></th>');
  document.write('<th><b>Storage</b></th>');
  document.write('<th><b>Lots</b></th>');
  document.write('<th><b>Items</b></th>');
  document.write('<th><b>Eval</b></th>');
  document.write('<th><b>Weight</b></th>');
  document.write('<th><b>Euro/Kgr</b></th>');
  document.write('</tr>');

  storages := db.StorageBins;

  ShowSplash;

  aa := 0;
  evalsum := 0.0;
  numitems := 0;
  numlots := 0;
  tw := 0.0;

  db.FetchStorageBinsCache;

  for i := 0 to storages.Count - 1 do
  begin
    SplashProgress('Working...', i / storages.Count);

    inc(aa);
    document.StartItemId(aa);
    st := storages.Strings[i];
    inv := db.InventoryForStorageBinCache(st);
    if storestats then
    begin
      inv.StoreHistoryStatsRec(basedefault + 'storage\storage_' + filenamestring(st) + '.stats');
      inv.StoreHistoryEvalRec(basedefault + 'storage\storage_' + filenamestring(st) + '.ieval');
    end;

    document.write('<tr bgcolor=' + TBGCOLOR + '>');
    document.write('<td width=5% align=right>' + IntToStr(aa) + '.</td>');
    document.write('<td width=20%><a href=storage/' + st + '>' + st + '</a></td>');
    numlots := numlots + inv.numlooseparts;
    numitems := numitems + inv.totallooseparts;
    eval := inv.EvaluatedPartOutValue_uQtyAvg.value;
    evalsum := evalsum + eval;
    w := inv.LoosePartsWeight / 1000;
    tw := tw + w;
    document.write('<td width=20% align=right>' + itoa(inv.numlooseparts) + '</td>');
    document.write('<td width=20% align=right>' + itoa(inv.totallooseparts) + '</td>');
    document.write('<td width=20% align=right>' + Format('€ %2.3f', [eval]) + '</td>');
    document.write('<td width=20% align=right>' + Format('%2.3f Kgr', [w]) + '</td>');
    document.write('<td width=20% align=right>' + Format('€ %2.3f / Kgr', [dbl_safe_div(eval, w)]) + '</td>');

    inv.Free;
    document.write('</tr>');
  end;

  document.EndNavigateSection;

  document.write('<tr bgcolor=' + TBGCOLOR + '>');
  document.write('<td>*</td>');
  document.write('<td>Total</td>');

  document.write('<td width=20% align=right>' + itoa(numlots) + '</td>');
  document.write('<td width=20% align=right>' + itoa(numitems) + '</td>');
  document.write('<td width=20% align=right>' + Format('€ %2.3f', [evalsum]) + '</td>');
  document.write('<td width=20% align=right>' + Format('%2.3f Kgr', [tw]) + '</td>');
  document.write('<td width=20% align=right>' + Format('€ %2.3f / Kgr', [dbl_safe_div(evalsum, tw)]) + '</td>');
  document.write('</tr></table>');

  document.MarkBottomNavigateSection;

  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  storages.Free;

  Screen.Cursor := crDefault;

end;

procedure TMainForm.ShowStorageInventory(const st: string);
var
  inv: TBrickInventory;
  inv_next, inv_prev: TBrickInventory;
  snext, sprev: string;
  html_next, html_prev: string;
begin
  Screen.Cursor := crHourGlass;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowStorageInventory', st);

  document.write('<body background="splash.jpg">');
  if st = '' then
    document.title('Storage Bins Inventory')
  else
    document.title('Storage "' + st + '"');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  if st = '' then
  begin
    inv := db.InventoryForAllStorageBins;
    inv.StoreHistoryStatsRec(basedefault + 'storage\storagebins.stats');
    inv.StoreHistoryEvalRec(basedefault + 'storage\storagebins.ieval');
    snext := '';
    sprev := '';
    html_next := '';
    html_prev := '';
  end
  else
  begin
    inv := db.InventoryForStorageBin(st);
    inv.StoreHistoryStatsRec(basedefault + 'storage\storage_' + filenamestring(st) + '.stats');
    inv.StoreHistoryEvalRec(basedefault + 'storage\storage_' + filenamestring(st) + '.ieval');

    snext := GetNextAlphanumeric(st);
    if snext = st then
      inv_next := nil
    else
    begin
      inv_next := db.InventoryForStorageBin(snext);
      if inv_next.totallooseparts = 0 then
      begin
        inv_next.Free;
        inv_next := nil;
      end;
    end;

    sprev := GetPrevAlphanumeric(st);
    if sprev = st then
      inv_prev := nil
    else
    begin
      inv_prev := db.InventoryForStorageBin(sprev);
      if inv_prev.totallooseparts = 0 then
      begin
        inv_prev.Free;
        inv_prev := nil;
      end;
    end;

    if inv_next = nil then
      html_next := ''
    else
    begin
      html_next := '<a href=storage/' + snext + '>next</a>';
      inv_next.Free;
    end;

    if inv_prev = nil then
      html_prev := ''
    else
    begin
      html_prev := '<a href=storage/' + sprev + '>prev</a>';
      inv_prev.Free;
    end;

  end;

  if inv = nil then
  begin
    DrawHeadLine('Can not find inventory for storage bin ' + st);
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  if st <> '' then
  begin
    inv.SaveLooseParts(basedefault + 'storage\storage_' + filenamestring(st) + '_parts.txt');
    inv.SaveSets(basedefault + 'storage\storage_' + filenamestring(st) + '_sets.txt');
    DrawHeadLine(html_prev + ' Storage Bin #' + st + ' ' + html_next + ' <a href="diagramstorage/' + filenamestring(st) + '"><img src="images\diagram.png"></a>');
  end
  else
  begin
    inv.SaveLooseParts(basedefault + 'storage\storage_all_parts.txt');
    inv.SaveSets(basedefault + 'storage\storage_all_sets.txt');
    DrawHeadLine('Storage Bins Inventory <a href="diagramstorage/Storage Bins"><img src="images\diagram.png"></a>');
  end;
  SortInventory(inv);

  DrawInventoryTable(inv);
  document.write('<br>');
  document.write('<br>');
  if ShowInventorySets(inv, False, 3) then
  begin
    document.write('<br>');
    document.write('<br>');
  end;
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  inv.Free;
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  Screen.Cursor := crDefault;
end;


procedure TMainForm.ShowMyMinifigInventory(const doloose, doofsets, domocs: boolean);
var
  inv, itmp: TBrickInventory;
  tit: string;
  i, j: integer;
begin
  Screen.Cursor := crHourGlass;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowMyMinifigInventory_', btoa(doloose) + '_' + btoa(doofsets) + '_' + btoa(domocs));

  document.write('<body background="splash.jpg">');
  tit := 'Minifigures (';
  if doloose then
    tit := tit + 'loose';
  if doofsets then
    if tit[length(tit)] <> '(' then
      tit := tit + ', sets'
    else
      tit := tit + 'sets';
  if domocs then
    if tit[length(tit)] <> '(' then
      tit := tit + ', mocs'
    else
      tit := tit + 'mocs';
    tit := tit + ')';

  document.title(tit);

  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  inv := TBrickInventory.Create;
  if doloose then
  begin
    itmp := inventory.Minifigures;
    inv.MergeWith(itmp);
    itmp.Free;
  end;
  if doofsets then
  begin
    for i := 0 to inventory.numsets - 1 do
      if inventory.sets[i].num > 0 then
        if not db.IsMoc(inventory.sets[i].setid) then
        begin
          itmp := db.GetSetInventory(inventory.sets[i].setid);
          if itmp <> nil then
          begin
            itmp := itmp.Minifigures;
            for j := 0 to inventory.sets[i].num - 1 do
              inv.MergeWith(itmp);
            itmp.Free;
          end;
        end;
  end;
  if domocs then
  begin
    for i := 0 to inventory.numsets - 1 do
      if inventory.sets[i].num > 0 then
        if db.IsMoc(inventory.sets[i].setid) then
        begin
          itmp := db.GetSetInventory(inventory.sets[i].setid);
          if itmp <> nil then
          begin
            itmp := itmp.Minifigures;
            for j := 0 to inventory.sets[i].num - 1 do
              inv.MergeWith(itmp);
            itmp.Free;
          end;
        end;
  end;

  DrawHeadLine(tit);

  //inv.SortPiecesByPartNumber;
  SortInventory(inv);

  DrawInventoryTable(inv);
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  inv.Free;
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowOrder(const orderid: string);
var
  inv: TBrickInventory;
begin
  Screen.Cursor := crHourGlass;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowOrder', orderid);

  document.write('<body background="splash.jpg">');
  document.title('Order #' + orderid);
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  inv := orders.OrderInventory(orderid);
  if inv = nil then
  begin
    DrawHeadLine('Can not find inventory for order ' + orderid);
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  orders.StoreEvalHistory(basedefault + 'orders\' + orderid + '.eval', orderid);

  inv.SaveLooseParts(basedefault + 'orders\order_' + orderid + '_parts.txt');
  inv.SaveSets(basedefault + 'orders\order_' + orderid + '_sets.txt');
  SortInventory(inv);
  DrawHeadLine('Order #' + orderid + '<a href="diagramorder/' + orderid + '"><img src="images\diagram.png"></a>');

  DrawOrderInf(orderid);
  DrawInventoryTable(inv);
  document.write('<br>');
  document.write('<br>');
  if ShowInventorySets(inv, False, 3) then
  begin
    document.write('<br>');
    document.write('<br>');
  end;
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  inv.Free;

  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowHomePage;
begin
  document.write('<body background="splash.jpg">');
  document.title('Home');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  DrawHeadLine('Bricks Inventory');
  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');

  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>Quick links</b></th>');
  document.write('</tr>');

  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="cataloghome">Catalog</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="inv/0/C/-1">My loose parts</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="mysetsandmocs">My official sets and mocs</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="myofficialsets">My official sets</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="mymocs">My mocs</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="ShowMyMinifiguresMenu">My minifigures</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="colors">Colors</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="categories">Categories</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="orders">Orders</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="ShowStorageBins">Storage Bins</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="tags">Tags</a></td></tr>');

  document.write('</table></p></div></body>');

  document.SaveBufferToFile(diskmirror);
  document.Flash;
end;

procedure TMainForm.dbloadprogress(const s: string; d : Double);
begin
  progress_string := s;
  SplashProgress(progress_string, d);
end;

function NewBlFileName(const fname: string): string;
var
  s1, s2: string;
begin
  splitstring(fname, s1, s2, '.');
  if length(s2) <> 3 then
  begin
    Result := fname;
    Exit;
  end;
  Result := db.GetBLNetPieceName(s1) + '.' + s2;
end;

function IsThumbImageLink(const src: string): boolean;
begin
  if Length(src) < 8 then
  begin
    Result := False;
    Exit;
  end;
  Result := (toupper(src[1]) = 'T') and (toupper(src[2]) = 'H') and (src[4] in ['\', '/']);
end;

procedure TMainForm.HTMLImageRequest(Sender: TObject; const SRC: String;
  var Stream: TMemoryStream);
var
  idx: integer;
  m: TMemoryStream;
  ps: TPakStream;
  stmp: string;
  trydownload: boolean;
  outfname: string;
  legocode: string;
  retrebrickable: boolean;
  jpgfilename, pngfilename: string;
  imgfound: boolean;
  pci: TPieceColorInfo;
  idx2: integer;
  retry: boolean;
  iname1, iname2: string;
  scheck: string;
  ftmp: string;
  didgear: boolean;
  jnewblname: string;
  pnewblname: string;
  curS: string;
  curM: TMemoryStream;

  function BLCOLOR1: string;
  var
    p: integer;
    n: string;
  begin
    Result := '';
    p := CharPos('\', SRC);
    if p < 1 then
      Exit;
    n := Copy(SRC, 1, p - 1);
    Result := itoa(db.colors(atoi(n)).BrickLingColor);
  end;

  function RBCOLOR1: string;
  var
    p: integer;
  begin
    Result := '';
    p := CharPos('\', SRC);
    if p < 1 then
      Exit;
    Result := Copy(SRC, 1, p - 1);
  end;


begin
  ChDir(basedefault);
  scheck := Trim(SRC);
  if scheck = '' then
    Exit;

  if Pos1('//static.bricklink', SRC) then
    Exit;

  if IsThumbImageLink(scheck) then
  begin
    if not fexists(basedefault + scheck) then
      ConvertThumbnailImageExCache(scheck);
    Exit;
  end;

  if fexists(basedefault + SRC) then
  begin
    if (RightStr(SRC, 4) = '.jpg') and (Pos('s\', scheck) = 1) then // set
    begin
      trydownload := not CheckValidImageDonwload(basedefault + 's\' + ExtractFileName(SRC));
      if not trydownload then
        Exit;
    end
    else
      Exit;
  end;
  didgear := False;
  idx := streams.IndexOf(strupper(SRC));
  idx2 := imagerequests.IndexOf(strupper(SRC));
  if idx2 < 0 then
    imagerequests.Add(strupper(SRC));
  if idx = -1 then
  begin
    ps := TPakStream.Create(SRC, pm_full);
    trydownload := (ps.IOResult <> 0) and (idx2 = -1);
    if not trydownload then
      if (RightStr(SRC, 4) = '.jpg') and (Pos('s\', scheck) = 1) then // set
        if fexists(basedefault + 's\' + ExtractFileName(SRC)) then
          trydownload := not CheckValidImageDonwload(basedefault + 's\' + ExtractFileName(SRC));
    if trydownload then
    begin
      Screen.Cursor := crHourglass;
      ps.Free;
      if (RightStr(SRC, 4) = '.jpg') and (Pos('s\', scheck) = 1)then // set
      begin
        ForceDirectories(basedefault + 's\');
        jpgfilename := ExtractFileName(SRC);
        pngfilename := ChangeFileExt(jpgfilename, '.png');
        outfname := basedefault + 's\' + ExtractFileName(SRC);
        if db.IsBook(firstword(ExtractFileName(SRC), '.')) then
        begin
          jnewblname := NewBlFileName(jpgfilename);
          pnewblname := NewBlFileName(pngfilename);
          if not DownloadFileImg('https://' + BL_NET + '/BL/' + jnewblname, outfname) then
            if not DownloadFileImg('https://' + BL_NET + '/BN/' + pnewblname, outfname) then
              if searchdownloadimg then
                if not DownloadFileImg('http://' + BL_NET + '/SL/' + jnewblname, outfname) then
                  if not DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/SL/' + jnewblname, outfname) then
                    if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/PN/0/' + pnewblname, outfname) then
                      if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/SL/' + pnewblname, outfname) then
                        if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/MN/0/' + pnewblname, outfname) then
                          if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/ON/0/' + pnewblname, outfname) then
                            if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/SN/0/' + pnewblname, outfname) then
                              if not DownloadFileImg('http://' + BL_NET + '/ML/' + jnewblname, outfname) then
                                if not DownloadFileImg('http://www.1000steine.com/brickset/images/' + jnewblname, outfname) then
                                  if not DownloadFileImg('https://images.brickset.com/sets/images/' + jnewblname, outfname) then
                                    if not DownloadFileImg('http://img.rebrickable.com/img/sets-b/' + jnewblname, outfname) then;
        end
        else if db.IsGear(firstword(ExtractFileName(SRC), '.')) then
        begin
          pnewblname := NewBlFileName(pngfilename);
          if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/GL/' + pnewblname, outfname) then
            if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/GN/0/' + pnewblname, outfname) then
              if searchdownloadimg then
                if not DownloadFileImg('https://' + BL_NET + '/BL/' + NewBlFileName(jpgfilename), outfname) then;
        end
        else if db.IsMinifigure(firstword(ExtractFileName(SRC), '.')) then
        begin
          jnewblname := NewBlFileName(jpgfilename);
          pnewblname := NewBlFileName(pngfilename);
          if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/MN/0/' + pnewblname, outfname) then
            if not DownloadFileImg('http://' + BL_NET + '/ML/' + jnewblname, outfname) then
              if searchdownloadimg then
                if not DownloadFileImg('http://' + BL_NET + '/SL/' + jnewblname, outfname) then
                  if not DownloadFileImg('https://' + BL_NET + '/BL/' + jnewblname, outfname) then
                    if not DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/SL/' + jnewblname, outfname) then
                      if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/PN/0/' + pnewblname, outfname) then
                        if not DownloadFileImg('https://' + BL_NET + '/BN/' + pnewblname, outfname) then
                          if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/SL/' + pnewblname, outfname) then
                            if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/ON/0/' + pnewblname, outfname) then
                              if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/SN/0/' + pnewblname, outfname) then
                                if not DownloadFileImg('http://www.1000steine.com/brickset/images/' + jpgfilename, outfname) then
                                  if not DownloadFileImg('https://images.brickset.com/sets/images/' + jpgfilename, outfname) then
                                    if not DownloadFileImg('http://img.rebrickable.com/img/sets-b/' + jpgfilename, outfname) then;
        end
        else if db.IsPart(firstword(ExtractFileName(SRC), '.')) then
        begin
          jnewblname := NewBlFileName(jpgfilename);
          pnewblname := NewBlFileName(pngfilename);
          if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/PN/0/' + pnewblname, outfname) then
            if not DownloadFileImg('https://' + BL_NET + '/BL/' + jnewblname, outfname) then
              if searchdownloadimg then
                if not DownloadFileImg('http://' + BL_NET + '/SL/' + jnewblname, outfname) then
                  if not DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/SL/' + jnewblname, outfname) then
                    if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/SL/' + pnewblname, outfname) then
                      if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/MN/0/' + pnewblname, outfname) then
                        if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/ON/0/' + pnewblname, outfname) then
                          if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/SN/0/' + pnewblname, outfname) then
                            if not DownloadFileImg('http://' + BL_NET + '/ML/' + jnewblname, outfname) then
                              if not DownloadFileImg('https://' + BL_NET + '/BN/' + pnewblname, outfname) then
                                  if not DownloadFileImg('http://www.1000steine.com/brickset/images/' + jpgfilename, outfname) then
                                    if not DownloadFileImg('https://images.brickset.com/sets/images/' + jpgfilename, outfname) then
                                      if not DownloadFileImg('http://img.rebrickable.com/img/sets-b/' + jpgfilename, outfname) then;
        end
        else
        begin
          jnewblname := NewBlFileName(jpgfilename);
          pnewblname := NewBlFileName(pngfilename);
          if not DownloadFileImg('http://' + BL_NET + '/SL/' + jnewblname, outfname) then
            if not DownloadFileImg('https://' + BL_NET + '/BL/' + jnewblname, outfname) then
              if not DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/SL/' + jnewblname, outfname) then
                if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/PN/0/' + pnewblname, outfname) then
                  if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/SL/' + pnewblname, outfname) then
                    if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/MN/0/' + pnewblname, outfname) then
                      if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/ON/0/' + pnewblname, outfname) then
                        if not DownloadPngFileToJpg('https://img.' + s_bricklink + '.com/ItemImage/SN/0/' + pnewblname, outfname) then
                          if not DownloadFileImg('http://' + BL_NET + '/ML/' + jnewblname, outfname) then
                            if not DownloadFileImg('https://' + BL_NET + '/BN/' + pnewblname, outfname) then
                              if searchdownloadimg then
                                if not DownloadFileImg('http://www.1000steine.com/brickset/images/' + jpgfilename, outfname) then
                                  if not DownloadFileImg('https://images.brickset.com/sets/images/' + jpgfilename, outfname) then
                                    if not DownloadFileImg('http://img.rebrickable.com/img/sets-b/' + jpgfilename, outfname) then;
        end;
        if fexists(outfname) then
          MakeThumbnailImage(firstword(ExtractFileName(SRC), '.'), -1);
      end
      else if RightStr(SRC, 4) = '.png' then
      begin
        ForceDirectories(basedefault + RBCOLOR1 + '\');
        if RBCOLOR1 = '89' then
        begin
          outfname := basedefault + '89\' + ExtractFileName(SRC);
          if not DownloadJpgFileToPNG('http://' + BL_NET + '/SL/' + NewBlFileName(ChangeFileExt(ExtractFileName(SRC), '.jpg')), outfname) then
            DownloadJpgFileToPNG('http://www.1000steine.com/brickset/images/' + ChangeFileExt(ExtractFileName(SRC), '.jpg'), outfname);
        end
        else if RBCOLOR1 = itoa(CATALOGCOLORINDEX) then
        begin
          outfname := basedefault + itoa(CATALOGCOLORINDEX) + '\' + ExtractFileName(SRC);
          if not DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/CN/0/' + NewBlFileName(ExtractFileName(SRC)), outfname) then
            if not DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/CT/0/' + ChangeFileExt(NewBlFileName(ExtractFileName(SRC)), '.t1.png'), outfname) then
              if not DownloadGIFFileToPNG('http://img.' + s_bricklink + '.com/C/' + NewBlFileName(ChangeFileExt(ExtractFileName(SRC), '.gif')), outfname) then
                DownloadJpgFileToPNG('http://img.' + s_bricklink + '.com/C/' + NewBlFileName(ChangeFileExt(ExtractFileName(SRC), '.jpg')), outfname);
        end
        else if RBCOLOR1 = itoa(INSTRUCTIONCOLORINDEX) then
        begin
          outfname := basedefault + itoa(INSTRUCTIONCOLORINDEX) + '\' + ExtractFileName(SRC);
          if not DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/IN/0/' + NewBlFileName(ExtractFileName(SRC)), outfname) then
            if not DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/IN/0/' + NewBlFileName(ChangeFileExt(ExtractFileName(SRC), '') + '-99') + '.png', outfname) then
              if not DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/IT/0/' + ChangeFileExt(NewBlFileName(ExtractFileName(SRC)), '.t1.png'), outfname) then
                if not DownloadJpgFileToPNG('http://img.' + s_bricklink + '.com/I/' + NewBlFileName(ChangeFileExt(ExtractFileName(SRC), '.jpg')), outfname) then
                begin
                  iname1 := ChangeFileExt(ExtractFileName(SRC), '');
                  if Length(iname1) > 5 then
                    if Pos('-1', iname1) = Length(iname1) - 1 then
                    begin
                      SetLength(iname1, Length(iname1) - 2);
                      iname2 := iname1;
                      iname2[Length(iname2) - 2] := '0';
                      iname2[Length(iname2) - 1] := '0';
                      iname2[Length(iname2) - 0] := '0';
                      while Length(iname2) < 5 do
                        iname2 := '0' + iname2;
                      if not DownloadJpgFileToPNG('http://media.brickinstructions.com/' + iname2 + '/' + iname1 + '/001.jpg', outfname) then
                        if not DownloadJpgFileToPNG('http://media.brickinstructions.com/' + iname2 + '/' + iname1 + '/main.jpg', outfname) then
                          if not DownloadJpgFileToPNG('http://lego.brickinstructions.com/' + iname2 + '/' + iname1 + '/001.jpg', outfname) then
                            DownloadJpgFileToPNG('http://lego.brickinstructions.com/' + iname2 + '/' + iname1 + '/main.jpg', outfname);
                    end;
                end;
        end
        else if RBCOLOR1 = itoa(BOXCOLORINDEX) then
        begin
          outfname := basedefault + itoa(BOXCOLORINDEX) + '\' + ExtractFileName(SRC);
          if not DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/ON/0/' + NewBlFileName(ExtractFileName(SRC)), outfname) then
            DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/OT/0/' + ChangeFileExt(NewBlFileName(ExtractFileName(SRC)), '.t1.png'), outfname);
        end
        else
        begin
          outfname := basedefault + RBCOLOR1 + '\' + ExtractFileName(SRC);

          retrebrickable := False;
          legocode := db.GetCodeFromPieceColor(db.RebrickablePart(firstword(ExtractFileName(SRC), '.')), atoi(RBCOLOR1));
          if legocode <> '' then
            retrebrickable := DownloadJpgFileToPNG('https://m.rebrickable.com/media/parts/elements/' + legocode + '.jpg', outfname);

          if not retrebrickable then
          begin
            imgfound := False;
            if RBCOLOR1 = '-1' then
              if JPG2PNG(basedefault + 's\' + firstword(ExtractFileName(SRC), '.') + '.jpg', outfname) then
                imgfound := True;
            if not imgfound then
              if RBCOLOR1 = '-1' then
              begin
                ftmp := basedefault + '9999\' + firstword(ExtractFileName(SRC), '.') + '.png';
                if fexists(ftmp) then
                begin
                  if CopyFile(ftmp, outfname) then
                    if fexists(outfname) then
                      imgfound := True;
                end;
              end;
            if not imgfound then
              if RBCOLOR1 = '9999' then
              begin
                ftmp := basedefault + '-1\' + firstword(ExtractFileName(SRC), '.') + '.png';
                if fexists(ftmp) then
                begin
                  if CopyFile(ftmp, outfname) then
                    if fexists(outfname) then
                      imgfound := True;
                end;
              end;

            if not imgfound then
            begin
              if (RBCOLOR1 = '-1') or (RBCOLOR1 = '9999') then
              begin
                pci := db.PieceColorInfo(firstword(ExtractFileName(SRC), '.'), -1);
                if pci <> nil then
                  if pci.parttype = TYPE_MINIFIGURE then
                    imgfound := DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/MN/0/' + NewBlFileName(ExtractFileName(SRC)), outfname);
                if not imgfound then
                  imgfound := DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/SN/0/' + NewBlFileName(ExtractFileName(SRC)), outfname);
                if not imgfound then
                begin
                  imgfound := DownloadFile('https://img.' + s_bricklink + '.com/ItemImage/GN/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.png', outfname);
                  if not imgfound then
                    imgfound := DownloadFile('https://img.' + s_bricklink + '.com/ItemImage/GL/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.png', outfname);
                  if not imgfound then
                    imgfound := DownloadFile('https://img.' + s_bricklink + '.com/ItemImage/GL/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.png', outfname);
                  didgear := True;
                end;
              end
              else
              begin
                imgfound := DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/PT/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.t1.png', outfname);
              end;
            end;

            if not imgfound then
              if searchdownloadimg then
              begin
                if not DownloadJpgFileToPNG('http://img.' + s_bricklink + '.com/P/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.jpg', outfname) then
                  if not DownloadJpgFileToPNG('http://img.' + s_bricklink + '.com/P/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(firstword(ExtractFileName(SRC), '.')) + '.jpg', outfname) then
                    if not DownloadJpgFileToPNG('http://' + BL_NET + '/P/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.jpg', outfname) then
                      if not DownloadGIFFileToPNG('http://' + BL_NET + '/P/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.gif', outfname) then
                        if not DownloadGIFFileToPNG('http://' + BL_NET + '/M/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.gif', outfname) then
                          if not DownloadJpgFileToPNG('http://' + BL_NET + '/M/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.jpg', outfname) then
                            if not DownloadGIFFileToPNG('https://' + BL_NET + '/ML/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.gif', outfname) then
                              if not DownloadJpgFileToPNG('https://' + BL_NET + '/ML/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.jpg', outfname) then
                                if not DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/BN/' + BLCOLOR1 + '/' + NewBlFileName(ExtractFileName(SRC)), outfname) then
                                  if not DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/PN/' + BLCOLOR1 + '/' + NewBlFileName(ExtractFileName(SRC)), outfname) then
                                    if not didgear then
                                    begin
                                      if not DownloadFile('https://img.' + s_bricklink + '.com/ItemImage/GN/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.png', outfname) then
                                        DownloadFile('https://img.' + s_bricklink + '.com/ItemImage/GL/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.png', outfname)
                                    end;
              end
              else
              begin
                if db.IsPart(firstword(ExtractFileName(SRC), '.')) then
                begin
                  if not DownloadJpgFileToPNG('http://img.' + s_bricklink + '.com/P/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.jpg', outfname) then
                    if not DownloadJpgFileToPNG('http://img.' + s_bricklink + '.com/P/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(firstword(ExtractFileName(SRC), '.')) + '.jpg', outfname) then
                      if not DownloadJpgFileToPNG('http://' + BL_NET + '/P/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.jpg', outfname) then
                        if not DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/PN/' + BLCOLOR1 + '/' + NewBlFileName(ExtractFileName(SRC)), outfname) then
                          if not DownloadGIFFileToPNG('http://' + BL_NET + '/P/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.gif', outfname) then;
                end
                else if db.IsMinifigure(firstword(ExtractFileName(SRC), '.')) then
                begin
                  if not DownloadGIFFileToPNG('http://' + BL_NET + '/M/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.gif', outfname) then
                    if not DownloadJpgFileToPNG('http://' + BL_NET + '/M/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.jpg', outfname) then
                      if not DownloadGIFFileToPNG('https://' + BL_NET + '/ML/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.gif', outfname) then
                        if not DownloadJpgFileToPNG('https://' + BL_NET + '/ML/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.jpg', outfname) then;
                end
                else
                begin
                  if not didgear then
                    if db.IsPossibleGear(firstword(ExtractFileName(SRC), '.')) then
                    begin
                      if not DownloadFile('https://img.' + s_bricklink + '.com/ItemImage/GN/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.png', outfname) then
                        DownloadFile('https://img.' + s_bricklink + '.com/ItemImage/GL/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.png', outfname)
                  end
                  else
                  begin
                    if db.IsPossiblePart(firstword(ExtractFileName(SRC), '.')) then
                    begin
                      if not DownloadJpgFileToPNG('http://img.' + s_bricklink + '.com/P/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.jpg', outfname) then
                        if not DownloadJpgFileToPNG('http://img.' + s_bricklink + '.com/P/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(firstword(ExtractFileName(SRC), '.')) + '.jpg', outfname) then
                          if not DownloadJpgFileToPNG('http://' + BL_NET + '/P/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.jpg', outfname) then
                            if not DownloadGIFFileToPNG('http://' + BL_NET + '/P/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.gif', outfname) then
                              if not DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/PN/' + BLCOLOR1 + '/' + NewBlFileName(ExtractFileName(SRC)), outfname) then;
                    end
                    else if db.IsPossibleMinifigure(firstword(ExtractFileName(SRC), '.')) then
                    begin
                      if not DownloadGIFFileToPNG('http://' + BL_NET + '/M/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.gif', outfname) then
                        if not DownloadJpgFileToPNG('http://' + BL_NET + '/M/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.jpg', outfname) then
                          if not DownloadGIFFileToPNG('https://' + BL_NET + '/ML/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.gif', outfname) then
                            if not DownloadJpgFileToPNG('https://' + BL_NET + '/ML/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.jpg', outfname) then;
                    end
                    else if db.IsPossibleSet(firstword(ExtractFileName(SRC), '.')) then  // Search box
                    begin
                      if not DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/BN/' + BLCOLOR1 + '/' + NewBlFileName(ExtractFileName(SRC)), outfname) then;
                    end
                    else
                    begin
                      if not DownloadJpgFileToPNG('http://img.' + s_bricklink + '.com/P/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.jpg', outfname) then
                        if not DownloadJpgFileToPNG('http://img.' + s_bricklink + '.com/P/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(firstword(ExtractFileName(SRC), '.')) + '.jpg', outfname) then
                          if not DownloadJpgFileToPNG('http://' + BL_NET + '/P/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.jpg', outfname) then
                            if not DownloadGIFFileToPNG('http://' + BL_NET + '/P/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.gif', outfname) then
                              if not DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/PN/' + BLCOLOR1 + '/' + NewBlFileName(ExtractFileName(SRC)), outfname) then
                                if not DownloadGIFFileToPNG('http://' + BL_NET + '/M/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.gif', outfname) then
                                  if not DownloadJpgFileToPNG('http://' + BL_NET + '/M/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.jpg', outfname) then
                                    if not DownloadGIFFileToPNG('https://' + BL_NET + '/ML/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.gif', outfname) then
                                      if not DownloadJpgFileToPNG('https://' + BL_NET + '/ML/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.jpg', outfname) then
                                        if not DownloadFileImg('https://img.' + s_bricklink + '.com/ItemImage/BN/' + BLCOLOR1 + '/' + NewBlFileName(ExtractFileName(SRC)), outfname) then;
                    end;
                  end;
                end
              end;
          end
        end;

        if fexists(outfname) then
          MakeThumbnailImage(firstword(ExtractFileName(SRC), '.'), atoi(RBCOLOR1));
      end;
      ps := TPakStream.Create(SRC, pm_full);
      Screen.Cursor := crDefault;
    end;

    if ps.IOResult <> 0 then
    begin
      ps.Free;

      retry := True;
      sTmp := FindThumbnailImageFileNameForHtmlReq(SRC);
      if sTmp <> '' then
      begin
        if fexists(basedefault + sTmp) then
        begin
          ps := TPakStream.Create(basedefault + sTmp, pm_full);
          if ps.IOResult <> 0 then
            ps.Free
          else
            retry := False;
        end;
      end;

      if retry then
      begin
        sTmp := findsimilarimagestring(entriesHash, SRC);
        printf('Image %s not found, retrying ... [%s]'#13#10,[SRC, sTmp]);
        if sTmp = SRC then
        begin
          Stream := nil;
          Exit;
        end;
        if sTmp = '' then
        begin
          Stream := nil;
          Exit;
        end;
        if sTmp[Length(sTmp)] = '\' then
        begin
          Stream := nil;
          Exit;
        end;

        ps := TPakStream.Create(sTmp, pm_full);
        if ps.IOResult <> 0 then
        begin
          ps.Free;
          Stream := nil;
          Exit;
        end;
      end;

    end;
    m := TMemoryStream.Create;
    m.LoadFromStream(ps);
    ps.Free;
    idx := streams.AddObject(strupper(SRC), m);
  end;
  if idx > 1 then
  begin
    curS := streams.Strings[idx];
    curM := streams.Objects[idx] as TMemoryStream;
    streams.Delete(idx);
    streams.Insert(0, curS);
    streams.Objects[0] := curM;
    idx := 0;
  end;
  Stream := streams.Objects[idx] as TMemoryStream;

{  curM: TMemoryStream;
  Streams.Exchange(0, idx);
  Stream := streams.Objects[0] as TMemoryStream;}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  IdleTimer.Enabled := False;

//  TimingForm.CrawlerTimer.Enabled := False;
  inventory.SaveLooseParts(basedefault + 'myparts.txt');
  inventory.SavePartsInventoryPriceguide(basedefault + 'myparts_priceguide.txt');
  inventory.SaveSets(basedefault + 'mysets.txt');

{  ShowSplash;
  progress_string := 'Saving Priceguide...';
  SplashProgress(progress_string, 0.05);
  db.ExportPriceGuide(basedefault + 'dbexport_priceguide.txt');
  progress_string := 'Saving Part Out guide...';
  SplashProgress(progress_string, 0.05);
  db.ExportPartOutGuide(basedefault + 'dbexport_partout.txt');
  HideSplash;}

  orders.Free;
  inventory.Free;
  db.Free;

  for i := 0 to streams.Count - 1 do
    streams.Objects[i].Free;
  streams.Free;
  imagerequests.Free;
  for i := 0 to 127 do
    FreeList(thumbnailcache[i]);
  for i := 0 to 127 do
    FreeList(thumbnailfilesexist[i]);

  try entries.Free; except end;
  try entriesHash.Free; except end;

  for i := 0 to goback.Count - 1 do
    goback.Objects[i].Free;
  goback.Free;
  for i := 0 to gofwd.Count - 1 do
    gofwd.Objects[i].Free;
  gofwd.Free;
  Comparesetslist.Free;
  Missingformultiplesetslist.Free;
  Missingfordismandaledsetslist.Free;

  if dismantaledsetsinv <> nil then
    dismantaledsetsinv.Free;

  SplashForm.Free;
  PAK_ShutDown;
  MT_ShutDown;
  I_Quit;

  document.Free;

  BI_SaveDefaults(basedefault + 'bi4.ini');
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  i: integer;
  parts: TDStringList;
  tb: TTabItem;
begin
  if initialized then
    Exit;

  ShowSplash;
  SplashProgress('Loading images...', 0.0);
  printf('PAK_InitFileSystem(): Initializing pak files.'#13#10);
  PAK_InitFileSystem;
  PAK_SetDefaultPath(basedefault);
  PAK_AddDirectory(ExtractFilePath(ParamStr(0)));
  SplashProgress('Loading images...', 0.05);
  PAK_AddFile('sets_0.zip');
  SplashProgress('Loading images...', 0.15);
  parts := findfiles('parts_*.zip');
  for i := 0 to parts.Count - 1 do
  begin
    SplashProgress('Loading images...', 0.15 + 0.85 * i / parts.Count);
    PAK_AddFile(parts.Strings[i]);
  end;
  parts.Free;
  PAK_AddFile('main.zip');
  SplashProgress('Loading images...', 1.0);
  PAK_GetEntries(entries);
  entries.Sorted := True;
  entriesHash.AssignStringList(entries);

  if not DirectoryExists(basedefault + 'out') then
    MkDir(basedefault + 'out');
  if not DirectoryExists(basedefault + 'out\navigate') then
    MkDir(basedefault + 'out\navigate');
  if not DirectoryExists(basedefault + 'db') then
    MkDir(basedefault + 'db');
  if not DirectoryExists(basedefault + 'db\instructions') then
    MkDir(basedefault + 'db\instructions');
  if not DirectoryExists(basedefault + 'db\boxes') then
    MkDir(basedefault + 'db\boxes');
  if not DirectoryExists(basedefault + 'db\setmolds') then
    MkDir(basedefault + 'db\setmolds');
  if not DirectoryExists(basedefault + 'db\books') then
    MkDir(basedefault + 'db\books');
  if not DirectoryExists(basedefault + 'db\gears') then
    MkDir(basedefault + 'db\gears');
  if not DirectoryExists(basedefault + 'db\minifigs') then
    MkDir(basedefault + 'db\minifigs');
  if not DirectoryExists(basedefault + 'db\rmolds') then
    MkDir(basedefault + 'db\rmolds');
  if not DirectoryExists(basedefault + 'db\molds') then
    MkDir(basedefault + 'db\molds');
  if not DirectoryExists(basedefault + 'db\parts') then
    MkDir(basedefault + 'db\parts');
  if not DirectoryExists(basedefault + 'db\sets') then
    MkDir(basedefault + 'db\sets');
  if not DirectoryExists(basedefault + 'db\catalogs') then
    MkDir(basedefault + 'db\catalogs');
  if not DirectoryExists(basedefault + 'thg') then
    MkDir(basedefault + 'thg');
  if not DirectoryExists(basedefault + 'thc') then
    MkDir(basedefault + 'thc');
  if not DirectoryExists(basedefault + 'tho') then
    MkDir(basedefault + 'tho');
  if not DirectoryExists(basedefault + 'thi') then
    MkDir(basedefault + 'thi');
  if not DirectoryExists(basedefault + 'th0') then
    MkDir(basedefault + 'th0');
  if not DirectoryExists(basedefault + 'mosaic') then
    MkDir(basedefault + 'mosaic');
  if not DirectoryExists(basedefault + 's') then
    MkDir(basedefault + 's');
  if not DirectoryExists(basedefault + '9997') then
    MkDir(basedefault + '9997');
  if not DirectoryExists(basedefault + 'orders') then
    MkDir(basedefault + 'orders');
  if not DirectoryExists(basedefault + 'storage') then
    MkDir(basedefault + 'storage');
  if not DirectoryExists(basedefault + 'images') then
    MkDir(basedefault + 'images');
  if not DirectoryExists(basedefault + 'cache') then
    MkDir(basedefault + 'cache');

  LugBulks1.Visible := DirectoryExists(basedefault + 'lugbulks');

  db := TSetsDatabase.Create;
  progress_string := 'Loading database...';
  db.progressfunc := dbloadprogress;
  db.InitCreate;
  db.LoadFromDisk(basedefault + 'db\db_set_pieces.txt');
  inventory := TBrickInventory.Create;
  inventory.CreateExtentedHashTable;
  if fexists(basedefault + 'myparts.txt') then
    inventory.LoadLooseParts(basedefault + 'myparts.txt');
  if fexists(basedefault + 'mysets.txt') then
    inventory.LoadSets(basedefault + 'mysets.txt');

  initialized := True;
  HideSplash;

  orders.LoadFilesDirectory(basedefault + 'orders');

  BI_CheckDefaultAssets(basedefault);

  document.savepath := basedefault + 'out\navigate\';

  AddressEdit.Text := 'home';

  goback.AddObject('home', TScrollPos.Create(0, 0));
  ShowHomePage;
  Caption := Application.Title + ' - ' + HTML.DocumentTitle;
  tb := TTabItem.Create(TabControl1);
  tb.Store(HTML, 'home', goback, gofwd);
  TabControl1.Tabs.AddObject(HTML.DocumentTitle, tb);
{  db.CrawlerPriorityPart('15714', 15);
  db.CrawlerPriorityPart('15714', 41);
  db.CrawlerPriorityPart('15714', 14);
  db.CrawlerPriorityPart('15714', 4);
  db.CrawlerPriorityPart('15714', 1);}
end;

procedure TMainForm.DrawPartOutValueWithOutSets(inv: TBrickInventory; const setid: string = '');
var
  inv2: TBrickInventory;
begin
  inv2 := inv.Clone;
  inv2.RemoveAllSets;
  DrawPartOutValue(inv2, setid);
  inv2.Free;
end;

procedure TMainForm.DrawPartOutValue(inv: TBrickInventory; const setid: string = '');

  function _poutcol(const cont: string; const percent, y1, y2: Double): string;
  var
    sp: string;
    sy1, sy2: string;
  begin
    sp := Format('%2.3f%s', [percent * 100, '%']);
    sy1 := Format('%2.3f', [y1]);
    sy2 := Format('%2.3f', [y2]);
    Result := '<td width=17%><p align="center"><b>' + cont + '</b><br></p>';
    Result := Result + '<table width=99% bgcolor=' + TBGCOLOR + ' border=2><tbody><tr align="RIGHT">';
    Result := Result + '<td width="50%">Percent:</td><td width="50%"><b>' + sp;
    Result := Result + '</b></td></tr><tr align="RIGHT"><td>Avg Price:</td><td><b>€ ' + sy1;
    Result := Result + '</b></td></tr><tr align="RIGHT"><td>Qty Avg Price:</td><td><b>€ ' + sy2;
    Result := Result + '</b></td></tr></tbody></table></td>';
  end;

begin
  document.write('<table  width=99% bgcolor=' + THBGCOLOR + ' border=2>');
  document.write('<table  width=99% bgcolor=' + THBGCOLOR + ' border=2>');
  document.write('<th>');
  document.write('<b>Inventory (Part Out) Value (%d ' +
                  decide(inv.totallooseparts = 1, 'part', 'parts') + ' in %d ' + decide(inv.numlooseparts = 1, 'lot', 'lots') + ')</b>',
                  [inv.totallooseparts, inv.numlooseparts]);
  if setid <> '' then
    document.write(' <a href=refreshset/' + setid + '><img src="images\refresh.png"></a>');
  document.write('</th></table>');
  document.write('<tr>');
  document.write('<td width=34% align="center"><b>Sold</b></td>');
  document.write('<td width=34% align="center"><b>Available</b></td>');
  document.write('<td width=34% align="center"><b>Evaluated</b></td>');
  document.write('</tr>');
  document.write('</table>');
  document.write('<table  width=99% bgcolor=' + THBGCOLOR + ' border=2>');
  document.write('<tr>');
  inv.UpdateCostValues;
  document.write(_poutcol('New', inv.SoldPartOutValue_nAvg.percentage, inv.SoldPartOutValue_nAvg.value, inv.SoldPartOutValue_nQtyAvg.value));
  document.write(_poutcol('Used', inv.SoldPartOutValue_uAvg.percentage, inv.SoldPartOutValue_uAvg.value, inv.SoldPartOutValue_uQtyAvg.value));
  document.write(_poutcol('New', inv.AvailablePartOutValue_nAvg.percentage, inv.AvailablePartOutValue_nAvg.value, inv.AvailablePartOutValue_nQtyAvg.value));
  document.write(_poutcol('Used', inv.AvailablePartOutValue_uAvg.percentage, inv.AvailablePartOutValue_uAvg.value, inv.AvailablePartOutValue_uQtyAvg.value));
  document.write(_poutcol('New', inv.EvaluatedPartOutValue_nAvg.percentage, inv.EvaluatedPartOutValue_nAvg.value, inv.EvaluatedPartOutValue_nQtyAvg.value));
  document.write(_poutcol('Used', inv.EvaluatedPartOutValue_uAvg.percentage, inv.EvaluatedPartOutValue_uAvg.value, inv.EvaluatedPartOutValue_uQtyAvg.value));
  document.write('</tr></table>');

  document.write('<table  width=99% bgcolor=' + THBGCOLOR + ' border=2>');
  document.write('<table  width=99% bgcolor=' + THBGCOLOR + ' border=2>');
  document.write('<th>');
  document.write('<b>Demand</b>');
  document.write('</th></table>');
  document.write('<tr>');
  document.write('<td width=50% align="center"><b>New</b></td>');
  document.write('<td width=50% align="center"><b>Used</b></td>');
  document.write('</tr>');
  document.write('</table>');
  document.write('<table  width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr align="RIGHT">');
  document.write('<td>%2.2f</td>', [inv.nDemand.value * 100]);
  document.write('<td>%2.2f</td>', [inv.uDemand.value * 100]);
  document.write('</tr></table>');
end;

procedure TMainForm.UpdateDismantaledsetsinv;
var
  inv2: TBrickInventory;
  i: integer;
begin
  if dismantaledsetsinv <> nil then
    dismantaledsetsinv.Free;
  dismantaledsetsinv := TBrickInventory.Create;

  for i := 0 to inventory.numsets - 1 do
    if inventory.sets[i].dismantaled > 0 then
    begin
      inv2 := db.GetSetInventory(inventory.sets[i].setid);
      dismantaledsetsinv.MergeWith(inv2);
    end;
end;

procedure TMainForm.DrawBrickOrderInfo(const brick: brickpool_p; const setid: string = '';
  const aspan1: integer = -1; const aspan2: integer = -1; const showreadylist: boolean = false);
var
  oinf: TStringList;
  oitem: TOrderItemInfo;
  i, j, p, k: integer;
  curconv: Double;
  ss1, ss2: string;
  inv, inv2: TBrickInventory;
  num: integer;
  pci: TPieceColorInfo;
  stor, stmp: string;
  stmp2, stmp3: string;
  sset: string;
  pavl: integer;
  sid: string;
  pi: TPieceInfo;
  removeforbuildsetstr: string;
  sqty1, sqty2: string;
  span1, span2: integer;
  sspan1, sspan2, sspan3: string;
  dolugbulk: boolean;
  lb: TLugbulk2017;
  lcost: double;
  snumstor, sdescstor: string;
  numstor: integer;
begin
  if not dodraworderinfo then
    Exit;

  if aspan1 > 0 then
    span1 := aspan1
  else
    span1 := 4;

  if aspan2 > 0 then
    span2 := aspan2
  else
    span2 := 3;
  if showreadylist then
    inc(span2);

  sspan1 := itoa(span1);
  sspan2 := itoa(span2);
  sspan3 := itoa(span1 + span2 + 1);

  oinf := orders.ItemInfo(brick.part, brick.color);
  if oinf <> nil then
  begin
    for i := 0 to oinf.Count - 1 do
    begin
      oitem := oinf.Objects[i] as TOrderItemInfo;
      document.write('<tr bgcolor=#EEEEEE>');

      if oitem.orderstatus = 'NSS' then
        ss1 := '<font color="red">'
      else if (oitem.orderstatus = 'Canceled') or (oitem.orderstatus = 'Cancelled') then
        ss1 := '<font color="red">'
      else if (oitem.orderstatus = 'Completed') or (oitem.orderstatus = 'Received') then
        ss1 := '<font color="green">'
      else
        ss1 := '';
      if ss1 = '' then
        ss2 := ''
      else
        ss2 := '</font>';

      document.write('<td colspan="' + sspan1 + '">%s<b><a href="order/%d">%d</a></b> %s %s [%s] %s</td>',
          [ss1,
           oitem.orderid,
           oitem.orderid,
           oitem.orderdate,
           '<a href=sellerorders/' + oitem.orderseller + '>' + oitem.orderseller + '</a>',
           oitem.orderstatus,
           ss2]);

      document.write('<td align="right">%d</td>',
          [oitem.num]);

      document.write('<td align="right" colspan="' + sspan2 + '">(%s) %s %2.3f (%2.3f)<br>%2.3f (%2.3f)',
          [oitem.condition,
           oitem.currency,
           oitem.price,
           oitem.pricetot,
           oitem.num * oitem.price,
           oitem.num * oitem.pricetot]);

      curconv := db.ConvertCurrency(oitem.currency);
      if curconv <> 1.0 then
        document.write('<br>(%s) %s %2.3f (%2.3f)<br>%2.3f (%2.3f)</td>',
          [oitem.condition,
           '*€',
           oitem.price * curconv,
           oitem.pricetot * curconv,
           oitem.num * oitem.price * curconv,
           oitem.num * oitem.pricetot * curconv])
      else
        document.write('</td>');


      if (oitem.orderstatus = 'NSS') or (oitem.orderstatus = 'Canceled') or (oitem.orderstatus = 'Cancelled') then
        document.write('</font>');

      document.write('</tr>');
    end;
  end;

  if dismantaledsetsinv <> nil then
    if dismantaledsetsinv.LoosePartCount(brick.part, brick.color) > 0 then
      for i := 0 to inventory.numsets - 1 do
        if inventory.sets[i].dismantaled > 0 then
        begin
          sid := inventory.sets[i].setid;
          inv := db.GetSetInventory(sid);
          if inv <> nil then
          begin
            inv2 := inv.Clone;
            for j := 1 to inventory.sets[i].dismantaled - 1 do
              inv2.MergeWith(inv);
            num := inv2.LoosePartCount(brick.part, brick.color);
            if num > 0 then
            begin
              document.write('<tr bgcolor=#EEEEEE>');
//              document.write('<td colspan="4"><b><a href="sinv/%s">%d x %s</a></b> - %s <img width=48px src=s\%s.jpg></td>',
              document.write('<td colspan="' + sspan1 + '"><b><a href="sinv/%s">%d x %s</a></b> - %s ' + MakeThumbnailImage2(sid, -1) + '</td>',
                [sid, inventory.sets[i].dismantaled,
                 sid, db.SetDesc(sid)]);
              document.write('<td align="right">%d</td>', [num]);

              dolugbulk := False;
              pi := db.PieceInfo(sid);
              if pi <> nil then
                if pi.category = CATEGORYLUGBULK then
                  if fexists(basedefault + 'lugbulks\' + itoa(db.SetYear(sid)) + '.txt') then
                    dolugbulk := True;

              if dolugbulk then
              begin
                lb := TLugbulk2017.Create;
                lb.LoadFromFile(basedefault + 'lugbulks\' + itoa(db.SetYear(sid)) + '.txt');
                lcost := lb.ItemCost(brick.part, brick.color);
                if lcost > 0.0 then
                  document.write('<td align="right" colspan="' + sspan2 + '">(N) EUR ' + Format('%2.3f', [lcost]) + '<br>' + Format('%2.3f', [num * lcost]) + '</td></tr>')
                else
                  document.write('<td colspan="' + sspan2 + '"><br></td></tr>');
                lb.Free;
              end
              else
                document.write('<td colspan="' + sspan2 + '"><br></td></tr>');
            end;
            inv2.Free;
          end;
        end;

//  pci := db.PieceColorInfo(brick.part, brick.color);
  pci := db.PieceColorInfo(brick);
  if pci <> nil then
  begin
    for i := 0 to pci.storage.Count - 1 do
    begin
      document.write('<tr bgcolor=#EEEEEE>');
      splitstring(pci.storage.Strings[i], stor, stmp, ':');
      // Remove from storage button for building this set
      splitstring(stmp, snumstor, sdescstor, ':');
      numstor := atoi(snumstor);
      if setid = '' then
      begin
        if numstor > 0 then
          removeforbuildsetstr :=
            ' <a href=removepiecefromstoragenum/' + brick.part + '/' + itoa(brick.color) + '/' +
                itoa(brick.num) + '/' + stor + '><img src="images\remove.png"></a>'
        else
          removeforbuildsetstr := '';
      end
      else
      begin
        if numstor > 0 then
          removeforbuildsetstr :=
            ' <a href=removepiecefromstorageset/' + brick.part + '/' + itoa(brick.color) + '/' +
                setid + '/' + stor + '><img src="images\remove.png"></a>'
        else
          removeforbuildsetstr := '';
      end;

      stmp2 := strupper(stmp);
      p := Pos('SET ', stmp2);
      if p > 0 then
      begin
        sset := '';
        j := p + 4;
        pavl := 0;
        while j <= length(stmp) do
        begin
          if IsNumericC(stmp[j]) or ((stmp[j] = '-') and (pavl < 1)) then
          begin
            if stmp[j] = '-' then
              inc(pavl);
            sset := sset + stmp[j];
          end
          else
            break;
          inc(j);
        end;
        if CharPos('-', sset) = 0 then
          sset := sset + '-1'
        else if CharPos('-', sset) = Length(sset) then
          sset := sset + '1';
        if db.SetDesc(sset) = '' then
          stmp3 := stmp
        else
        begin
          stmp3 := '';
          for k := 1 to p + 3 do
            stmp3 := stmp3 + stmp[k];
          stmp3 := stmp3 + '<b><a href=sinv/' + sset + '>' + sset + '</a></b>';
          for k := j to length(stmp) do
            stmp3 := stmp3 + stmp[k];
        end;
      end
      else
        stmp3 := stmp;

      splitstring(stmp3, sqty1, sqty2, ':');
      if atoi(sqty1) < 0 then
      begin
        stmp3 := '<font color="red">' + sqty1 + '</font>';
        if sqty2 <> '' then
          stmp3 := stmp3 + ':' + sqty2;
      end;

      document.write('<td colspan="' + sspan3 + '">%s</td></tr>',
                ['<b><a href=storage/' + stor + '>' + stor + '</a></b>:' + stmp3 + removeforbuildsetstr]);
    end;
  end;

  document.write('<tr bgcolor=' + TFGCOLOR + '><td colspan="' + sspan3 + '"></td></tr>');
end;

procedure TMainForm.DrawBrickOrderInfoLite(const brick: brickpool_p; const needed: integer; const setid: string = '');
var
  oinf: TStringList;
  oitem: TOrderItemInfo;
  i, j: integer;
  inv, inv2: TBrickInventory;
  num: integer;
  pci: TPieceColorInfo;
  stor, stmp: string;
  sfont: string;
  sid: string;
  sqty1, sqty2: string;
  snumstor, sdescstor: string;
  numstor: integer;
begin
  if not dodraworderinfolite then
    Exit;

  document.write('<td width=30%>');
  oinf := orders.ItemInfo(brick.part, brick.color);
  if oinf <> nil then
    for i := 0 to oinf.Count - 1 do
    begin
      oitem := oinf.Objects[i] as TOrderItemInfo;
      if (oitem.orderstatus <> 'NSS') and (oitem.orderstatus <> 'Canceled') and (oitem.orderstatus <> 'Cancelled') then
      begin
        if oitem.num >= needed then
          sfont := '<font color="green">'
        else
          sfont :='<font color="red">';

        document.write('<b><a href="order/%d">%d</a> (%s%d</font>x)</b><br>',
          [oitem.orderid,
           oitem.orderid,
           sfont,
           oitem.num]);
      end;
    end;

  if dismantaledsetsinv <> nil then
    if dismantaledsetsinv.LoosePartCount(brick.part, brick.color) > 0 then
      for i := 0 to inventory.numsets - 1 do
        if inventory.sets[i].dismantaled > 0 then
        begin
          sid := inventory.sets[i].setid;
          inv := db.GetSetInventory(sid);
          if inv <> nil then
            if db.PieceInfo(sid).category = CATEGORYLUGBULK then
            begin
              inv2 := inv.Clone;
              for j := 1 to inventory.sets[i].dismantaled - 1 do
                inv2.MergeWith(inv);
              num := inv2.LoosePartCount(brick.part, brick.color);
              if num > 0 then
              begin
                if num >= needed then
                  sfont := '<font color="green">'
                else
                  sfont :='<font color="red">';

                document.write('<b><a href="sinv/%s">%s</a> (%s%d</font>x)</b><br>',
                  [sid,
                   sid,
                   sfont,
                   num]);
              end;
              inv2.Free;
            end;
        end;

  pci := db.PieceColorInfo(brick);
  if pci <> nil then
  begin
    for i := 0 to pci.storage.Count - 1 do
    begin
      splitstring(pci.storage.Strings[i], stor, stmp, ':');
      // Remove from storage button for building this set
      splitstring(stmp, snumstor, sdescstor, ':');
      numstor := atoi(snumstor);
      splitstring(stmp, sqty1, sqty2, ':');
      num := atoi(sqty1);
      if num > 0 then
      begin
        if num >= needed then
          sfont := '<font color="green">'
        else
          sfont :='<font color="red">';

        document.write('<b><a href="storage/%s">%s</a> (%s%d</font>x)</b><br>',
                  [stor,
                   stor,
                   sfont,
                   num]);
      end;
    end;
  end;

  document.write('</td>');
end;

procedure TMainForm.PreviewInventoryTable(inv: TBrickInventory);
var
  aa, i: integer;
  brick: brickpool_p;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  prn, pru: double;
  prnt, prut: double;
  cl: TDNumberList;
  pl: TStringList;
  cinfo: colorinfo_p;
  num: integer;
  accurstr: string;
  totalweight: double;
  totalcostwn: double;
  totalcostwu: double;
  scolor: string;
  www: double;
begin
  UpdateDismantaledsetsinv;

  if inv = nil then
    inv := inventory;

  Screen.Cursor := crHourGlass;

  SortInventory(inv);

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Part</b></th>');
  document.write('<th>Color</th>');
  document.write('<th>Num pieces</th>');
  document.write('<th>Price New</th>');
  document.write('<th>Price Used</th>');
  document.write('</tr>');

  ShowSplash;
  SplashProgress('Working...', 0);

  brick := @inv.looseparts[0];
  aa := 0;
  prnt := 0;
  prut := 0;
  num := 0;
  accurstr := '%2.3f';
  cl := TDNumberList.Create;
  pl := TStringList.Create;
  totalweight := 0.0;
  totalcostwn := 0.0;
  totalcostwu := 0.0;
  for i := 0 to inv.numlooseparts - 1 do
  begin
    inc(aa);
    scolor := itoa(brick.Color);
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=35%><img width=100px src=' + scolor + '\' + brick.part + '.png><br><b>');
    document.write('<a href=spiece/' + brick.part + '>' + brick.part + '</a></b>');
    document.write(' - ' + db.PieceDesc(brick.part) + '</td><td width=20%>');
    DrawColorCell(brick.color, 25);
//    document.BlancColorCell(db.colors(brick.color).RGB, 25);
//    pci := db.PieceColorInfo(brick.part, brick.color);
    pci := db.PieceColorInfo(brick);
    cinfo := db.colors(brick.color);
    document.write('<a href=spiecec/' + brick.part + '/' + scolor + '>' +  cinfo.name +
      ' (' + scolor + ') (BL=' + IntToStr(cinfo.BrickLingColor) + ')' + GetRebrickableColorHtml(brick.color) + '</td>');
    document.write('<td width=10% align=right>' + IntToStr(brick.num));
    document.write('</td>');
    pi := db.PieceInfo(pci);
    if pci <> nil then
    begin
      www := db.GetItemWeight(pci.piece, pci.color, pi);
      prn := pci.EvaluatePriceNew;
      pru := pci.EvaluatePriceUsed;
      document.write('<td width=15% align=right>' + Format('€ ' + accurstr + '<br>€ ' + accurstr + '<br>€ %2.2f / Kgr', [prn, prn * brick.num, dbl_safe_div(prn, www) * 1000]) + '</td>');
      document.write('<td width=15% align=right>' + Format('€ ' + accurstr + '<br>€ ' + accurstr + '<br>€ %2.2f / Kgr', [pru, pru * brick.num, dbl_safe_div(pru, www) * 1000]) + '</td>');
      prnt := prnt + prn * brick.num;
      prut := prut + pru * brick.num;
      if www > 0.0 then
      begin
        totalweight := totalweight + www * brick.num;
        totalcostwn := totalcostwn + prn * brick.num;
        totalcostwu := totalcostwu + pru * brick.num;
      end;
    end;
    document.write('</tr>');

    if cl.IndexOf(brick.color) < 0 then
      cl.Add(brick.color);
    if pl.IndexOf(brick.part) < 0 then
      pl.Add(brick.part);
    num := num + brick.num;

    Inc(brick);
    if inv.numlooseparts < 20 then
      SplashProgress('Working...', i / (inv.numlooseparts))
    else
      if (i mod 5) = 0 then
        SplashProgress('Working...', i / (inv.numlooseparts));

  end;

  document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right><b>Total</b></td>');
  document.write('<td width=35%><b>' + IntToStr(pl.Count) + ' unique mold' + decide(pl.Count = 1, '', 's') + '</b></td>');
  document.write('<td width=20%><b>' + IntToStr(cl.Count) + ' unique color' + decide(cl.Count = 1, '', 's') + '</b></td>');

  document.write('<td width=10% align=right><b>' + IntToStr(num) + '<br>' + Format('%2.3f Kgr', [totalweight / 1000]) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f<br>€ %2.2f / Kgr', [prnt, dbl_safe_div(totalcostwn, totalweight) * 1000]) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f<br>€ %2.2f / Kgr', [prut, dbl_safe_div(totalcostwu, totalweight) * 1000]) + '</b></td>');
  document.write('</tr>');
  SplashProgress('Working...', 1);

  cl.Free;
  pl.Free;

  HideSplash;

  document.write('</tr></table>');

  Screen.Cursor := crDefault;
end;

procedure TMainForm.DrawInventoryTableNoPages(inv: TBrickInventory; const lite: Boolean = False; const setid: string = ''; const dosort: boolean = True; const dosplash: boolean = true);
begin
  DrawInventoryTable(inv, lite, setid, dosort, false, nil, dosplash);
end;

procedure TMainForm.DrawInventoryTable(inv: TBrickInventory; const lite: Boolean = False;
  const setid: string = ''; const dosort: boolean = True; const usepages: boolean = True;
  const lb: TLugBulk2017 = nil; const dosplash: boolean = true; const showreadylist: boolean = false);
var
  aa, i: integer;
  brick: brickpool_p;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  prn, pru: double;
  prnt, prut: double;
  mycost: Double;
  mycosttot: Double;
  mytotcostpieces: integer;
  cl: TDNumberList;
  pl: TStringList;
  num: integer;
  accurstr: string;
  totalweight, totalweight_ready: double;
  totalcostwn: double;
  totalcostwt: double;
  totalcostwu: double;
  scolor: string;
  lcost, lcostt: double;
  lugcostedit: string;
  cinfo: colorinfo_p;
  www: double;
  readyinv: TBrickInventory;
  rnum, rnum_tot: integer;
begin
  UpdateDismantaledsetsinv;

  if dosplash then
  begin
    ShowSplash;
    SplashProgress('Working...', 0);
  end;

  if inv = nil then
    inv := inventory;

  Screen.Cursor := crHourGlass;

  if dosort then
    SortInventory(inv);

  if not lite then
    DrawPartOutValue(inv, setid);

  if usepages then
    if not lite then
      document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Part</b></th>');
  document.write('<th>Color</th>');
  if not lite then
    document.write('<th>Demand</th>');
  document.write('<th>Num pieces</th>');
  if lite then
    document.write('<th>Price New</th>')
  else
    document.write('<th>N</th>');
  if lb <> nil then
    if not lite then
      document.write('<th>Lugbulk Price</th>');
  if not lite then
    document.write('<th>U</th>');
  if not lite then
    document.write('<th>Cost</th>');
  if showreadylist then
    document.write('<th>In Ready List</th>');
  readyinv := BI_GetReadyListInv(S_READYLIST_01);
  document.write('</tr>');

  brick := @inv.looseparts[0];
  aa := 0;
  prnt := 0;
  prut := 0;
  lcostt := 0.0;
  num := 0;
  mycosttot := 0.0;
  mytotcostpieces := 0;
  if lite then
    accurstr := '%2.3f'
  else
    accurstr := '%2.4f';
  cl := TDNumberList.Create;
  pl := TStringList.Create;
  totalweight := 0.0;
  totalweight_ready := 0.0;
  rnum_tot := 0;
  totalcostwn := 0.0;
  totalcostwt := 0.0;
  totalcostwu := 0.0;
  for i := 0 to inv.numlooseparts - 1 do
  begin
    inc(aa);
    if usepages then
      if not lite then
        document.StartItemId(aa);
    scolor := itoa(brick.Color);
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' +
      IntToStr(aa) + '.</td><td width=35%><img width=100px src=' + scolor + '\' + brick.part + '.png><br><b>');
    document.write('<a href=spiece/' + brick.part + '>' + brick.part + '</a></b>');
    document.write(' - ' + db.PieceDesc(brick.part) + '</td><td width=20%>');
    DrawColorCell(brick.color, 25);
//    document.BlancColorCell(db.colors(brick.color).RGB, 25);
//    pci := db.PieceColorInfo(brick.part, brick.color);
    pci := db.PieceColorInfo(brick);
    pi := db.PieceInfo(pci);
    if pi = nil then
      pi := db.PieceInfo(brick.part);
    cinfo := db.colors(brick.color);
    if lite or (pci = nil) then
      document.write('<a href=spiecec/' + brick.part + '/' + scolor + '>' +  cinfo.name +
        ' (' + scolor + ') (BL=' + IntToStr(cinfo.BrickLingColor) + ')' +
          GetRebrickableColorHtml(brick.color) + '<img src="images\details.png"></a>' +
          HtmlDrawInvImgLink(brick.part, brick.color, pi) + '</td>')
    else
      document.write('<a href=spiecec/' + brick.part + '/' + scolor + '>' +  cinfo.name +
        ' (' + scolor + ') (BL=' + IntToStr(cinfo.BrickLingColor) + ')' +
          GetRebrickableColorHtml(brick.color) + '<img src="images\details.png"></a>' +
          HtmlDrawInvImgLink(brick.part, brick.color, pi) +
          decide(pci.setmost = '', '', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) +
          ' times in ' + pci.setmost + '</a>') + lugbulklinks(pci) + '</td>');
    if not lite then
    begin
      if pci <> nil then
      begin
        document.write('<td width=10% align=right>');
        document.write('N=%2.3f<br>U=%2.3f</td>', [pci.nDemand, pci.uDemand]);
      end
      else
        document.write('<td width=10% align=right></td>');
    end;

    document.write('<td width=10% align=right>' + IntToStr(brick.num));
    if not lite then
    begin
      document.write('<br><a href=editpiece/' + brick.part + '/' + scolor + '><img src="images\edit.png"></a>');
      document.write('<br><a href=diagrampiece/' + brick.part + '/' + scolor + '><img src="images\diagram.png"></a>');
    end;
    document.write('</td>');

    www := 0.0;

    if pci <> nil then
    begin
      prn := pci.EvaluatePriceNew;
      pru := pci.EvaluatePriceUsed;
      www := db.GetItemWeight(pci.piece, pci.color, pi);
      document.write('<td width=' + decide(lite, '15%', '10%') + ' align=right>' + Format('€ ' + accurstr + '<br>€ ' + accurstr + '<br>€ %2.2f / Kgr', [prn, prn * brick.num, dbl_safe_div(prn, www) * 1000]) + '</td>');
      if (lb <> nil) and not lite then
      begin
        lcost := lb.ItemCost(pci.piece, pci.color);
        lugcostedit := 'EditLugbulkPrice/' + brick.part + '/' + itoa(brick.color) + '/' + itoa(lb.year);
        if lcost >= 0.0 then
        begin
          lcostt := lcostt + lcost * brick.num;
          document.write('<td width=' + decide(lite, '15%', '10%') + ' align=right>' + Format('€ ' + '<a href=' + lugcostedit + '>' + accurstr + '</a><br>€ ' + accurstr + '<br>€ %2.2f / Kgr', [lcost, lcost * brick.num, dbl_safe_div(lcost, www) * 1000]) + '</td>')
        end
        else
        begin
          lcost := 0.0;
          document.write('<td width=' + decide(lite, '15%', '10%') + ' align=right><a href=' + lugcostedit + '>-</a></td>');
        end;
      end
      else
        lcost := 0.0;

      if not lite then
        document.write('<td width=10% align=right>' + Format('€ ' + accurstr + '<br>€ ' + accurstr + '<br>€ %2.2f / Kgr', [pru, pru * brick.num, dbl_safe_div(pru, www) * 1000]) + '</td>');
      prnt := prnt + prn * brick.num;
      prut := prut + pru * brick.num;
      if www > 0.0 then
      begin
        totalweight := totalweight + www * brick.num;
        totalcostwn := totalcostwn + prn * brick.num;
        totalcostwt := totalcostwt + lcost * brick.num;
        totalcostwu := totalcostwu + pru * brick.num;
      end;
    end;
    mycost := GetItemCostDbl(brick.part, brick.color);
    if not lite then
      document.write('<td width=10% align=right>' + Format('€ %2.4f<br>€ %2.4f', [mycost, mycost * brick.num]) + '</td>');
    mycosttot := mycosttot + mycost * brick.num;
    if mycost > 0.0 then
      mytotcostpieces := mytotcostpieces + brick.num;

    if showreadylist then
    begin
      if readyinv <> nil then
      begin
        rnum := readyinv.LoosePartCount(brick.part, brick.color);
        rnum_tot := rnum_tot + rnum;
        totalweight_ready := totalweight_ready + www * rnum;
        document.write('<td width=10% align=right>' + Format('%d<br>', [rnum]));
        if rnum >= brick.num then
          document.write('<img src="images\readyall.png"></td>')
        else if rnum = 0 then
          document.write('<img src="images\readynone.png"></td>')
        else
          document.write('<img src="images\readysome.png"></td>');
      end
      else
        document.write('<td width=10% align=right>(no info)</td>');
    end;

    document.write('</tr>');

    if cl.IndexOf(brick.color) < 0 then
      cl.Add(brick.color);
    if pl.IndexOf(brick.part) < 0 then
      pl.Add(brick.part);
    num := num + brick.num;

    if lite then
      DrawBrickOrderInfo(brick, '', -1, -1, showreadylist)
    else if lb <> nil then
      DrawBrickOrderInfo(brick, setid, 4, 4, showreadylist)
    else
      DrawBrickOrderInfo(brick, setid, -1, -1, showreadylist);
    Inc(brick);
    if dosplash then
    begin
      if inv.numlooseparts < 20 then
        SplashProgress('Working...', i / (inv.numlooseparts))
      else if inv.numlooseparts < 200 then
      begin
        if (i mod 4) = 0 then
          SplashProgress('Working...', i / (inv.numlooseparts));
      end
      else
      begin
        if (i mod 10) = 0 then
          SplashProgress('Working...', i / (inv.numlooseparts));
      end;
    end;
  end;
  if usepages then
    if not lite then
      document.EndNavigateSection;

  if dosplash then
    SplashProgress('Working...', 1);

  document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right><b>Total</b></td>');
  document.write('<td width=35%><b>' + IntToStr(pl.Count) + ' unique mold' + decide(pl.Count = 1, '', 's') + '</b></td>');
  document.write('<td width=20%><b>' + IntToStr(cl.Count) + ' unique color' + decide(cl.Count = 1, '', 's') + '</b></td>');
  if not lite then
  begin
    document.write('<td width=10% align=right>');
    document.write('N=%2.3f<br>U=%2.3f</td>', [inv.nDemand.value, inv.uDemand.value]);
  end;

{  else
    document.write('<td width=10% align=right></td>');}

  document.write('<td width=10% align=right><b>' + IntToStr(num) + '<br>' + Format('%2.3f Kgr', [totalweight / 1000]) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f<br>€ %2.2f / Kgr', [prnt, dbl_safe_div(totalcostwn, totalweight) * 1000]) + '</b></td>');

  if lb <> nil then
    document.write('<td width=10% align=right><b>' + Format('€ %2.2f<br>€ %2.2f / Kgr', [lcostt, dbl_safe_div(totalcostwt, totalweight) * 1000]) + '</b></td>');

  if not lite then
  begin
    document.write('<td width=10% align=right><b>' + Format('€ %2.2f<br>€ %2.2f / Kgr', [prut,  dbl_safe_div(totalcostwu, totalweight) * 1000]) + '</b></td>');
    if num = 0 then
      document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [mycosttot]) + '</b></td>')
    else
      document.write('<td width=10% align=right><b>' + Format('€ %2.2f<br>%2.3f%s', [mycosttot, 100 * mytotcostpieces / num, '%']) + '</b></td>');
  end;

  if showreadylist then
    document.write('<td width=10% align=right><b>' + IntToStr(rnum_tot) + '<br>' + Format('%2.3f Kgr', [totalweight_ready / 1000]) + '</b></td>');

  document.write('</tr></table>');

  if usepages then
    if not lite then
      document.MarkBottomNavigateSection;

  cl.Free;
  pl.Free;

  HideSplash;

  Screen.Cursor := crDefault;
end;

procedure TMainForm.DrawInventoryTableForPartsStorageQuery(inv, qryinv: TBrickInventory; const ppreview: boolean);
var
  aa, i: integer;
  brick: brickpool_p;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  cl: TDNumberList;
  pl: TStringList;
  num, num1, num2: integer;
  accurstr: string;
  scolor: string;
  cinfo: colorinfo_p;
begin
  UpdateDismantaledsetsinv;

  if inv = nil then
    inv := inventory;

  SortInventory(inv);

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=' + itoa(decide(ppreview, 1, 2)) + '>');
  document.write('<tr bgcolor=' + decide(ppreview, TBGCOLOR, THBGCOLOR) + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Part</b></th>');
  document.write('<th>Color</th>');
  document.write('<th>Num pieces</th>');
  document.write('<th>Needed pieces</th>');
  if dodraworderinfolite then
    document.write('<th>Additional storages</th>');

  brick := @inv.looseparts[0];
  aa := 0;
  num1 := 0;
  num2 := 0;
  accurstr := '%2.4f';
  cl := TDNumberList.Create;
  pl := TStringList.Create;
  for i := 0 to inv.numlooseparts - 1 do
  begin
    inc(aa);
    scolor := itoa(brick.Color);
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' +
      IntToStr(aa) + '.</td><td width=' + itoa(decide(ppreview, 55, 35)) + '%><img width=' +
        itoa(decide(ppreview, 80, 100)) + 'px src=' + scolor + '\' + brick.part + '.png><br><b>');
    document.write('<a href=spiece/' + brick.part + '>' + brick.part + '</a></b>');
    document.write(' - ' + db.PieceDesc(brick.part) + '</td><td width=' + itoa(decide(ppreview, 20, 40)) + '%>');
    DrawColorCell(brick.color, 25);
    pci := db.PieceColorInfo(brick);
    pi := db.PieceInfo(pci);
    if pi = nil then
      pi := db.PieceInfo(brick.part);
    cinfo := db.colors(brick.color);
    if ppreview then
      document.write(
          HtmlDrawInvImgLink(brick.part, brick.color, pi) + ' ' + cinfo.name)
    else
    begin
      document.write('<a href=spiecec/' + brick.part + '/' + scolor + '>' +  cinfo.name +
        ' (' + scolor + ') (BL=' + IntToStr(cinfo.BrickLingColor) + ')' +
          GetRebrickableColorHtml(brick.color) + '<img src="images\details.png"></a>' +
          HtmlDrawInvImgLink(brick.part, brick.color, pi));
      document.write('<br><a href=editpiece/' + brick.part + '/' + scolor + '><img src="images\edit.png"></a>');
      document.write('<br><a href=diagrampiece/' + brick.part + '/' + scolor + '><img src="images\diagram.png"></a></td>');
    end;

    document.write('<td width=10% align=right>' + IntToStr(brick.num));
    document.write('</td>');
    num := qryinv.loosepartcount(brick.part, brick.color);
    num2 := num2 + num;
    document.write('<td width=10% align=right>' + IntToStr(num) + '</td>');

    DrawBrickOrderInfoLite(brick, brick.num, '');

    document.write('</tr>');

    if cl.IndexOf(brick.color) < 0 then
      cl.Add(brick.color);
    if pl.IndexOf(brick.part) < 0 then
      pl.Add(brick.part);
    num1 := num1 + brick.num;

    Inc(brick);
  end;

  document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right><b>Total</b></td>');
  document.write('<td width=35%><b>' + IntToStr(pl.Count) + ' unique mold' + decide(pl.Count = 1, '', 's') + '</b></td>');
  document.write('<td width=40%><b>' + IntToStr(cl.Count) + ' unique color' + decide(cl.Count = 1, '', 's') + '</b></td>');
  document.write('<td width=10% align=right><b>' + IntToStr(num1) + '</b></td>');
  document.write('<td width=10% align=right><b>' + IntToStr(num2) + '</b></td>');

  document.write('</tr></table>');

  cl.Free;
  pl.Free;
end;

procedure TMainForm.DrawPriceguide(const part: string; const color: integer = -1);
var
  pg: priceguide_t;
  av: availability_t;

  function _getcol(const cont, til: string; x1, x2: integer; y1, y2, y3, y4: Double): string;
  var
    sx1, sx2: string;
    sy1, sy2, sy3, sy4: string;
  begin
    sx1 := IntToStr(x1);
    sx2 := IntToStr(x2);
    sy1 := Format('%2.3f', [y1]);
    sy2 := Format('%2.3f', [y2]);
    sy3 := Format('%2.3f', [y3]);
    sy4 := Format('%2.3f', [y4]);
    Result := '<td width=25%><p align="center"><b>' + cont + '</b><br></p>';
    Result := Result + '<table width=99% bgcolor=' + TBGCOLOR + ' border=2><tbody><tr align="RIGHT">';
    Result := Result + '<td width="50%">' + til + '</td><td width="50%"><b>' + sx1;
    Result := Result + '</b></td></tr><tr align="RIGHT"><td>Total Qty:</td><td><b>' + sx2;
    Result := Result + '</b></td></tr><tr align="RIGHT"><td>Min Price:</td><td><b>€ ' + sy1;
    Result := Result + '</b></td></tr><tr align="RIGHT"><td>Avg Price:</td><td><b>€ ' + sy2;
    Result := Result + '</b></td></tr><tr align="RIGHT"><td>Qty Avg Price:</td><td><b>€ ' + sy3;
    Result := Result + '</b></td></tr><tr align="RIGHT"><td>Max Price:</td><td><b>€ ' + sy4;
    Result := Result + '</b></td></tr></tbody></table></td>';
  end;

begin
  pg := db.Priceguide(part, color);
  av := db.Availability(part, color);
  document.write('<table  width=99% bgcolor=' + THBGCOLOR + ' border=2>');
  document.write('<tr>');
  document.write('<td width=50% align="center"><b>Priceguide</b></td>');
  document.write('<td width=50% align="center"><b>Availability</b></td>');
  document.write('</tr>');
  document.write('</table>');
  document.write('<table  width=99% bgcolor=' + THBGCOLOR + ' border=2>');
  document.write('<tr>');
  document.write(_getcol('New', 'Times Sold: ', pg.nTimesSold, pg.nTotalQty, pg.nMinPrice, pg.nAvgPrice, pg.nQtyAvgPrice, pg.nMaxPrice));
  document.write(_getcol('Used', 'Times Sold: ', pg.uTimesSold, pg.uTotalQty, pg.uMinPrice, pg.uAvgPrice, pg.uQtyAvgPrice, pg.uMaxPrice));
  document.write(_getcol('New', 'Total Lots: ', av.nTotalLots, av.nTotalQty, av.nMinPrice, av.nAvgPrice, av.nQtyAvgPrice, av.nMaxPrice));
  document.write(_getcol('Used', 'Total Lots: ', av.uTotalLots, av.uTotalQty, av.uMinPrice, av.uAvgPrice, av.uQtyAvgPrice, av.uMaxPrice));
  document.write('</tr></table>');
end;

procedure TMainForm.DrawPriceguideEx(const part: string; const color: integer; const at: TDateTime);
var
  pg: priceguide_t;
  av: availability_t;
  PA: parecdate_t;

  function _getdiagram(x1, x1a: double; const chid: integer): string;
  begin
    if x1 < x1a * 0.9 then
      Result := ' <a href="diagrampieceex/' + part + '/' + itoa(color) + '/' + itoa(chid) + '"><img src="images\diagram_down.png"></a>'
    else if x1 > x1a * 1.1 then
      Result := ' <a href="diagrampieceex/' + part + '/' + itoa(color) + '/' + itoa(chid) + '"><img src="images\diagram_up.png"></a>'
    else
      Result := ' <a href="diagrampieceex/' + part + '/' + itoa(color) + '/' + itoa(chid) + '"><img src="images\diagram_flat.png"></a>';
  end;

  function _getcolex(const cont, til: string;
    x1, x2: integer; y1, y2, y3, y4: Double;
    x1a, x2a: integer; y1a, y2a, y3a, y4a: Double; const chbase: integer): string;
  var
    sx1, sx2: string;
    sy1, sy2, sy3, sy4: string;
  begin
    sx1 := IntToStr(x1);
    sx2 := IntToStr(x2);
    sy1 := Format('%2.3f', [y1]);
    sy2 := Format('%2.3f', [y2]);
    sy3 := Format('%2.3f', [y3]);
    sy4 := Format('%2.3f', [y4]);
    Result := '<td width=25%><p align="center"><b>' + cont + '</b><br></p>';
    Result := Result + '<table width=99% bgcolor=' + TBGCOLOR + ' border=2><tbody><tr align="RIGHT">';
    Result := Result + '<td width="50%">' + til + '</td><td width="50%"><b>' + sx1 + _getdiagram(x1, x1a, chbase);
    Result := Result + '</b></td></tr><tr align="RIGHT"><td>Total Qty:</td><td><b>' + sx2 + _getdiagram(x2, x2a, chbase + 1);
    Result := Result + '</b></td></tr><tr align="RIGHT"><td>Min Price:</td><td><b>€ ' + sy1 + _getdiagram(y1, y1a, chbase + 2);
    Result := Result + '</b></td></tr><tr align="RIGHT"><td>Avg Price:</td><td><b>€ ' + sy2 + _getdiagram(y2, y2a, chbase + 3);
    Result := Result + '</b></td></tr><tr align="RIGHT"><td>Qty Avg Price:</td><td><b>€ ' + sy3 + _getdiagram(y3, y3a, chbase + 4);
    Result := Result + '</b></td></tr><tr align="RIGHT"><td>Max Price:</td><td><b>€ ' + sy4 + _getdiagram(y4, y4a, chbase + 5);
    Result := Result + '</b></td></tr></tbody></table></td>';
  end;

begin
  pg := db.Priceguide(part, color);
  av := db.Availability(part, color);
  PA := db.PArecAt(part, color, at);

  document.write('<table  width=99% bgcolor=' + THBGCOLOR + ' border=2>');
  document.write('<tr>');
  document.write('<td width=50% align="center"><b>Priceguide</b></td>');
  document.write('<td width=50% align="center"><b>Availability</b></td>');
  document.write('</tr>');
  document.write('</table>');
  document.write('<table  width=99% bgcolor=' + THBGCOLOR + ' border=2>');
  document.write('<tr>');
  document.write(_getcolex('New', 'Times Sold: ',
    pg.nTimesSold, pg.nTotalQty, pg.nMinPrice, pg.nAvgPrice, pg.nQtyAvgPrice, pg.nMaxPrice,
    PA.priceguide.nTimesSold, PA.priceguide.nTotalQty, PA.priceguide.nMinPrice, PA.priceguide.nAvgPrice, PA.priceguide.nQtyAvgPrice, PA.priceguide.nMaxPrice,
    1
    )
  );
  document.write(_getcolex('Used', 'Times Sold: ',
    pg.uTimesSold, pg.uTotalQty, pg.uMinPrice, pg.uAvgPrice, pg.uQtyAvgPrice, pg.uMaxPrice,
    PA.priceguide.uTimesSold, PA.priceguide.uTotalQty, PA.priceguide.uMinPrice, PA.priceguide.uAvgPrice, PA.priceguide.uQtyAvgPrice, PA.priceguide.uMaxPrice,
    7
    )
  );
  document.write(_getcolex('New', 'Total Lots: ',
    av.nTotalLots, av.nTotalQty, av.nMinPrice, av.nAvgPrice, av.nQtyAvgPrice, av.nMaxPrice,
    PA.availability.nTotalLots, PA.availability.nTotalQty, PA.availability.nMinPrice, PA.availability.nAvgPrice, PA.availability.nQtyAvgPrice, PA.availability.nMaxPrice,
    13
    )
  );
  document.write(_getcolex('Used', 'Total Lots: ',
    av.uTotalLots, av.uTotalQty, av.uMinPrice, av.uAvgPrice, av.uQtyAvgPrice, av.uMaxPrice,
    PA.availability.uTotalLots, PA.availability.uTotalQty, PA.availability.uMinPrice, PA.availability.uAvgPrice, PA.availability.uQtyAvgPrice, PA.availability.uMaxPrice,
    19
    )
  );
  document.write('</tr></table>');
end;

function TMainForm.ShowInventorySets(const inv: TBrickInventory; const header_flash: boolean; const mocflag: integer): boolean;
var
  aa, i: integer;
  set1: set_p;
  link: string;
  sinv: TBrickInventory;
  tot: integer;
  ismoc: Boolean;
  showit: boolean;
  sid: string;
  sdesc: string;
begin
  Result := inv.numsets > 0;
  if not Result then
    if not header_flash then
      Exit;

  Screen.Cursor := crHourGlass;

  ShowSplash;
  SplashProgress('Working...', 0);

  if header_flash then
  begin
    document.write('<body background="splash.jpg">');
    document.title('Inventory Sets/Mocs');
    DrawNavigateBar;
    document.write('<div style="color:' + DFGCOLOR + '">');
    document.write('<p align=center>');
    link := '<a href=dismantleallsets>Dismantle All Sets</a> - <a href=buildallsets>Build All Sets</a>';

    tot := 0;

    // Official sets only
    if mocflag = 1 then
    begin
      for i := 0 to inv.numsets - 1 do
        if not db.IsMoc(inv.sets[i].setid) then
          if inv.sets[i].num > 0 then
          begin
            sinv := db.GetSetInventory(inv.sets[i].setid);
            if sinv <> nil then
              tot := tot + sinv.totallooseparts * inv.sets[i].num;
          end;
      DrawHeadLine('My Official Sets <br><br>' +
        'My official sets have <a href="ShowMyMocsPieces">' + IntToStr(tot) + ' parts</a><br><br>');
    end;

    // Mocs only
    if mocflag = 2 then
    begin
      for i := 0 to inv.numsets - 1 do
        if db.IsMoc(inv.sets[i].setid) then
          if inv.sets[i].num > 0 then
          begin
            sinv := db.GetSetInventory(inv.sets[i].setid);
            if sinv <> nil then
              tot := tot + sinv.totallooseparts * inv.sets[i].num;
          end;
      DrawHeadLine('My Mocs <br><br>' +
        'My builded mocs have <a href="ShowMyMocsPieces">' + IntToStr(tot) + ' parts</a><br><br>');
    end;

    // Official sets & mocs
    if mocflag = 3 then
    begin
      for i := 0 to inv.numsets - 1 do
        if inv.sets[i].num > 0 then
        begin
          sinv := db.GetSetInventory(inv.sets[i].setid);
          if sinv <> nil then
            tot := tot + sinv.totallooseparts * inv.sets[i].num;
        end;
      DrawHeadLine('My <a href="myofficialsets">Official Sets</a> and <a href="mymocs">Mocs</a> (' + IntToStr(inv.numsets) + ' lots)<br>(' + IntToStr(inv.totalsetsbuilted) + ' builted - ' + IntToStr(inv.totalsetsdismantaled) + ' dismantaled)<br><br>' +
        'My builded official sets and mocs have <a href="ShowMySetsPieces">' + IntToStr(tot) + ' parts</a><br><br>' +
        link);
    end;

  end;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  if mocflag = 2 then
    document.write('<th><b>Moc</b></th>')
  else if mocflag = 1 then
    document.write('<th><b>Set</b></th>')
  else
    document.write('<th><b>Set/Moc</b></th>');
  if header_flash then
  begin
    document.write('<th>Num Builded</th>');
    document.write('<th>Num Dismandaled</th>');
  end
  else
    document.write('<th>Num</th>');
  document.write('</tr>');

  set1 := @inv.sets[0];
  aa := 0;
  for i := 0 to inv.numsets - 1 do
  begin
    if mocflag = 3 then
      showit := True
    else
    begin
      ismoc := db.IsMoc(set1.setid);
      if mocflag = 1 then
        showit := not ismoc
      else
        showit := ismoc
    end;

    if showit then
    begin
      inc(aa);

      if inv = inventory then
      begin
        sid := Trim(set1.setid);
        inventory.StorePieceInventoryStatsRec(basedefault + 'out\' + sid + '\' + sid + '.history', sid, -1);
      end;

//      document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%><img width=64px src=s\' + set1.setid + '.jpg><br>');
      document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%>' +
                      MakeThumbnailImage2(set1.setid, -1) + '<br>');
      if db.GetSetInventory(set1.setid) <> nil then
      begin
        document.write('<a href="sinv/' + set1.setid + '">');
        sdesc := db.SetDesc(set1.setid);
        document.write('<b>' + set1.setid + '</b> - ' + sdesc)
      end
      else
      begin
        document.write('<a href="spiece/' + set1.setid + '">');
        sdesc := db.PieceDesc(set1.setid);
        document.write('<b>' + set1.setid + '</b> - ' + sdesc);
      end;

      document.write('</td><td width=15% align=right>');
      if header_flash then
      begin
        document.write(IntToStr(set1.num) + '</td>');
        document.write('<td width=15% align=right>' + IntToStr(set1.dismantaled) + '</td></tr>');
      end
      else
        document.write(IntToStr(set1.num + set1.dismantaled) + '</td></tr>');
    end;

    Inc(set1);

    if inv.numsets > 1 then
    begin
      if inv.numsets < 20 then
        SplashProgress('Working...', i / (inv.numsets - 1))
      else
        if (i mod 5) = 0 then
          SplashProgress('Working...', i / (inv.numsets - 1));
    end;

  end;

  HideSplash;

  document.write('</tr></table>');
  if header_flash then
  begin
    document.write('</p>');
    document.write('<br>');
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
  end;

  Screen.Cursor := crDefault;

end;

procedure TMainForm.ShowMySetsAndMocs;
begin
  ShowInventorySets(inventory, True, 3);
end;

procedure TMainForm.ShowMyMocs;
begin
  ShowInventorySets(inventory, True, 2);
end;

procedure TMainForm.ShowMyOfficialSets;
begin
  ShowInventorySets(inventory, True, 1);
end;

function BLColorPieceInfo(const pcs: string; const color: integer): string;
const
  CHECK1 = 'Group by Currency</A> ]</TD></TR></TABLE>';
  CHECK2 = '<img src="/images/box16Y.png" height="16" width="16" align="ABSMIDDLE" border="0"> <img src="/images/box16N.png" height="16" width="16" align="ABSMIDDLE" border="0"> Indicates whether';
var
  s: TStringList;
  stmp: string;
  stmp2: string;
  stmp3: string;
  i, p, lpos: integer;
  fname: string;
  p1, p2: integer;
begin
  Result := '';
  fname := PieceColorCacheFName(pcs, itoa(color)) + '.html';
  if fexists(fname) then
  begin
    s := TStringList.Create;
    S_LoadFromFile(s, fname);
    stmp := Utf8ToAnsi(s.Text);
    s.Free;
    p1 := Pos('<TABLE ', UpperCase(stmp));
    if p1 > 0 then
    begin
      p2 := Pos('</TABLE>', UpperCase(stmp));
      if p2 > p1 then
        Result :=
          '<table width=99% bgcolor=' + TBGCOLOR + ' border=2><tr bgcolor=' + DBGCOLOR + '><td width 100%>' +
          StringReplace(Copy(stmp, p1, p2 - p1 + 8), ' SIZE="2"', '', [rfReplaceAll, rfIgnoreCase]) + 
          '</td></tr></table><br>';
    end;
  end;

  fname := PieceColorCacheFName(pcs, itoa(color)) + '.htm';
  if not fexists(fname) then
    Exit;
  s := TStringList.Create;
  S_LoadFromFile(s, fname);
  stmp := Utf8ToAnsi(s.Text);
  s.Free;

  p := Pos('Group by Currency', stmp);
  if p <= 0 then
    Exit;
  stmp2 := Copy(stmp, p, Length(stmp) - p - 1);

  p := Pos('<TABLE ', stmp2);
  if p <= 0 then
    Exit;
  stmp3 := Copy(stmp2, p, Length(stmp2) - p - 1);

  p := Pos('Indicates whether', stmp3);
  if p <= 0 then
    p := Pos('Select Language', stmp3);
  if p <= 0 then
  begin
    Result := Result + StringReplace(stmp3, '/images/box16', 'images\box16', [rfReplaceAll, rfIgnoreCase]);
  end
  else
  begin
    stmp := Copy(stmp3, 1, p - Length(CHECK2) + 15);
    Result := Result + StringReplace(stmp, '/images/box16', 'images\box16', [rfReplaceAll, rfIgnoreCase]);
  end;

  p := 0;
  stmp := UpperCase(Result);
  for i := Length(Result) - 4 downto 7 do
  begin
    lpos := PosEx('<TR>', stmp, i);
    if lpos > 0 then
    begin
      p := lpos;
      break;
    end;
  end;
  if p > 0 then
  begin
    SetLength(Result, p - 1);
    Result := Result + '</TABLE>';
  end;
end;

procedure TMainForm.ShowSetInventory(const setid: string; const lite: Boolean = False);
var
  inv: TBrickInventory;
  missing: integer;
  missing2: integer;
  st: set_t;
  i, j: integer;
  desc: string;
  tmpsets: TStringList;
  s1, s2: string;
  swanted: string;
  sx, ss1, ss2, ss3: string;
  sf1, sf2, sf3, sf4: string;
  numbricks: integer;
  numplates: integer;
  numtiles: integer;
  numslopes: integer;
  idx: integer;
  lnk: string;
  expensizestr: string;
  addinv: TStringList;
  minifiginv: TBrickInventory;
  dolugbulk: boolean;
  lb: TLugBulk2017;
  pi: TPieceInfo;
  pidx: integer;
  year: integer;
  slink: string;
  pci: TPieceColorInfo;
begin
  Screen.Cursor := crHourGlass;

  lastset := setid;

{  if domultipagedocuments then
    if not lite then
      document.NewMultiPageDocument('ShowSetInventory', setid);}

  document.write('<body background="splash.jpg">');
  document.title('Set ' + setid);
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  inv := db.GetSetInventory(setid);
  if inv = nil then
  begin
    if Trim(setid) <> '' then
      lnk := '<a href=downloadset/' + setid + '>Try to download the inventory from ' + s_bricklink + '.com</a>'
    else
      lnk := '';
    pci := db.PieceColorInfo(setid, -1);
    if pci = nil then
      slink := setid
    else
      slink := '<a href="spiece/' + setid + '">' + setid + '</a>';
    DrawHeadLine('Can not find inventory for ' + slink + ' <a href=DoEditSet/' + setid + '><img src="images\edit.png"></a><br><br>' + lnk);
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');

    tmpsets := TStringList.Create;
    if IsValidDBMask(setid) then
    begin
      for i := 0 to db.AllSets.Count - 1 do
      begin
        desc := db.SetDesc(db.AllSets.Strings[i]);
        if MatchesMask(desc, setid) then
          tmpsets.Add(db.AllSets.Strings[i])
        else if MatchesMask(db.AllSets.Strings[i], setid) then
          tmpsets.Add(db.AllSets.Strings[i]);
        if tmpsets.Count >= 500 then
          Break;
      end;
    end;

    if tmpsets.Count = 0 then
    begin
      for i := 0 to db.AllSets.Count - 1 do
      begin
        desc := db.SetDesc(db.AllSets.Strings[i]);
        if Pos(strupper(setid), strupper(desc)) > 0 then
          tmpsets.Add(db.AllSets.Strings[i]);
      end;

      for i := 0 to db.AllSets.Count - 1 do
      begin
        desc := db.AllSets.Strings[i];
        if Pos(strupper(setid), strupper(desc)) > 0 then
          tmpsets.Add(desc);
      end;
    end;

    tmpsets.Sort;
    for i := tmpsets.Count - 1 downto 1 do
      if tmpsets.Strings[i] = tmpsets.Strings[i - 1] then
        tmpsets.Delete(i);

    for i := 0 to tmpsets.Count - 1 do
    begin
      inv := db.GetSetInventory(tmpsets.Strings[i]);
      DrawHeadLine(Format('<a href="sinv/%s">%s - %s</a><br>(%d lots, %d parts, %d sets)<br><img width=360px src=s\' + tmpsets.Strings[i] + '.jpg>',
        [tmpsets.Strings[i], tmpsets.Strings[i], db.SetDesc(tmpsets.Strings[i]), inv.numlooseparts, inv.totallooseparts, inv.totalsetsbuilted + inv.totalsetsdismantaled]));
    end;
    tmpsets.Free;
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  if not db.IsMoc(setid) then
  begin
    lnk := '<a href=downloadsetandrefresh/' + setid + '>Refresh set inventory from ' + s_bricklink + '.com</a>';
    DrawHeadLine(lnk);
  end;
  inv.DoUpdateCostValues;
  SortInventory(inv);

  s1 := basedefault + 'out\' + Trim(setid) + '\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s2 := s1 + Trim(setid) + '.txt';
  inv.SaveLoosePartsAndSets(s2);

  s2 := s1 + setid + '_priceguide.txt';
  inv.SavePartsInventoryPriceguide(s2);

  inv.StoreHistoryStatsRec(basedefault + 'out\' + Trim(setid) + '\' + Trim(setid) + '.stats');
  inv.StoreHistoryEvalRec(basedefault + 'out\' + Trim(setid) + '\' + Trim(setid) + '.ieval');
  inventory.StorePieceInventoryStatsRec(basedefault + 'out\' + Trim(setid) + '\' + Trim(setid) + '.history', Trim(setid), -1);

  swanted := s1 + setid + '_wantedlist';
  inv.SaveLoosePartsWantedListNew(swanted + '_NEW_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListNew(swanted + '_NEW_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListNew(swanted + '_NEW_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListNew(swanted + '_NEW_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListNew(swanted + '_NEW_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListNew(swanted + '_NEW_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListNew(swanted + '_NEW_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListNew(swanted + '_NEW_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListNew(swanted + '_NEW_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListNew(swanted + '_NEW_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListNew(swanted + '_NEW_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListNew(swanted + '_NEW_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListNew(swanted + '_NEW_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListNew(swanted + '_NEW_030%.xml', 0.3);

  inv.SaveLoosePartsWantedListUsed(swanted + '_USED_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListUsed(swanted + '_USED_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListUsed(swanted + '_USED_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListUsed(swanted + '_USED_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListUsed(swanted + '_USED_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListUsed(swanted + '_USED_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListUsed(swanted + '_USED_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListUsed(swanted + '_USED_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListUsed(swanted + '_USED_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListUsed(swanted + '_USED_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListUsed(swanted + '_USED_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListUsed(swanted + '_USED_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListUsed(swanted + '_USED_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListUsed(swanted + '_USED_030%.xml', 0.3);


  for j := 0 to inv.numlooseparts - 1 do
    inventory.StorePieceInventoryStatsRec(
      basedefault + 'cache\' + itoa(decide(inv.looseparts[j].color = -1, 9999, inv.looseparts[j].color)) + '\' + inv.looseparts[j].part + '.history',
      inv.looseparts[j].part,
      inv.looseparts[j].color
    );
  for j := 0 to inv.numsets - 1 do
    inventory.StorePieceInventoryStatsRec(
      basedefault + 'out\' + Trim(inv.sets[j].setid) + '\' + Trim(inv.sets[j].setid) + '.history',
      Trim(inv.sets[j].setid), -1
    );

  inventory.GetSetInfo(setid, @st);
  sf1 := '<a href=addset/' + Trim(setid) + '>+</a>';
  if st.num > 0 then
    sf2 := '<a href=removeset/' + Trim(setid) + '>-</a>'
  else
    sf2 := '';
  sf3 := '<a href=addsetdismantaled/' + Trim(setid) + '>+</a>';
  if st.dismantaled > 0 then
    sf4 := '<a href=removesetdismantaled/' + Trim(setid) + '>-</a>'
  else
    sf4 := '';
  year := db.SetYear(setid);
  ss1 := Format('Inventory for %s - %s <br>(%d lots, %d parts, %d sets)<br>You have %s%d%s builted and %s%d%s dismantaled<br><img width=360px src=s\' + setid + '.jpg>' +
      ' <a href=DoEditSet/' + setid + '><img src="images\edit.png"></a>' +
      ' <a href=PreviewSetInventory/' + setid + '><img src="images\print.png"></a>' +
      ' <a href=diagrampiece/' + setid + '/-1><img src="images\diagram.png"></a><br>' +

      '[Year: <a href=ShowSetsAtYear/%d>%d</a>]<br>',
    ['<a href=spiece/' + setid + '>' + setid + '</a>', db.SetDesc(setid), inv.numlooseparts, inv.totallooseparts, inv.totalsetsbuilted + inv.totalsetsdismantaled,
     sf2, st.num, sf1, sf4, st.dismantaled, sf3, year, year]);
  idx := db.allsets.IndexOf(Trim(setid));
  if idx > -1 then
  begin
    if idx > 0 then
      ss1 := ss1 + '<a href=sinv/' + db.allsets.Strings[idx - 1] + '>Prev(' + db.allsets.Strings[idx - 1] + ' - ' + db.SetDesc(db.allsets.Strings[idx - 1]) + ')</a>  ';
    if idx < db.allsets.Count - 1 then
      ss1 := ss1 + decide(idx > 0, ' - ', ' ') + '<a href=sinv/' + db.allsets.Strings[idx + 1] + '>Next(' + db.allsets.Strings[idx + 1] + ' - ' + db.SetDesc(db.allsets.Strings[idx + 1]) + ')</a>  ';
  end
  else
  begin
    splitstring(Trim(setid), ss2, ss3, '-');
    if ss3 <> '' then
    begin
      sx := itoa(atoi(ss2) - 1) + '-' + ss3;
      if db.SetDesc(sx) <> '' then
        ss1 := ss1 + '<a href=sinv/' + sx + '>Prev(' + sx + ' - ' + db.SetDesc(sx) + ')</a>  ';
      sx := itoa(atoi(ss2) + 1) + '-' + ss3;
      if db.SetDesc(sx) <> '' then
        ss1 := ss1 + '<a href=sinv/' + sx + '>Next(' + sx + ' - ' + db.SetDesc(sx) + ')</a>';
    end;
  end;
  DrawHeadLine(ss1);
  missing := inventory.MissingToBuildSet(setid);
  if st.num > 0 then
    DrawHeadLine('You can dismantle this set to your loose parts! <a href=dismantleset/' + Trim(setid) + '>(dismantle it!)</a>');

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + DBGCOLOR + '>');
  document.write('<td width 25%>');

  if missing = 0 then
    DrawHeadLine('You can built this set! <a href=buildset/' + Trim(setid) + '>(Built it!)</a>')
  else
    DrawHeadLine(Format('%d part' + decide(missing = 1, ' is', 's are') + ' missing to build a copy of this set (%2.2f%s) (<a href="missingtobuildset/%s">show</a>)',
      [missing, dbl_safe_div(missing * 100, inv.totallooseparts), '%', Trim(setid)]));

  document.write('</td><td width 25%>');

  missing2 := inventory.MissingToBuildSetLegacyIgnore(Trim(setid));
  DrawHeadLine(Format('%d part' + decide(missing2 <= 1, ' is', 's are') + ' missing to build a copy of this set ignoring legacy colors (%2.2f%s) (<a href="missingtobuildsetLI/%s">show</a>)',
    [missing2, dbl_safe_div(missing2 * 100, inv.totallooseparts), '%', Trim(setid)]));

  document.write('</td><td width 25%>');

  DrawHeadLine(Format('<a href="missingtobuildset/%s/2">Check to build 2 sets</a>', [Trim(setid)]));

  document.write('</td><td width 25%>');

  DrawHeadLine('<a href=sstorageloc/' + setid + '>Inventory storage locations</a>');

  document.write('</td></tr></table>');

  numbricks := inv.totalloosepartsbycategory(5);
  numplates := inv.totalloosepartsbycategory(26);
  numtiles := inv.totalloosepartsbycategory(37) + inv.totalloosepartsbycategory(39);
  numslopes := inv.totalloosepartsbycategory(31) + inv.totalloosepartsbycategory(32) + inv.totalloosepartsbycategory(33);
  expensizestr := 'Most expensive lots: <a href="ShowExpensiveSetLotsNew/' + Trim(setid) + '/10">New</a> - <a href="ShowExpensiveSetLotsUsed/' + Trim(setid) + '/10">Used</a>';

  DrawHeadLine(Format('Bricks: %d<br>Slopes: %d<br>Plates: %d<br>Tiles: %d<br>Other: %d<br>' + expensizestr, [numbricks, numslopes, numplates, numtiles, inv.totallooseparts - numbricks - numplates - numslopes - numtiles]));

  DrawPriceguide(setid);

  dolugbulk := False;
  lb := nil;
  if not lite then
  begin
    pidx := db.AllPieces.IndexOf(setid);
    if pidx >= 0 then
    begin
      pi := db.AllPieces.Objects[pidx] as TPieceInfo;
      if pi.category = CATEGORYLUGBULK then
      begin
        if fexists(basedefault + 'lugbulks\' + itoa(year) + '.txt') then
        begin
          lb := TLugBulk2017.Create;
          lb.LoadFromFile(basedefault + 'lugbulks\' + itoa(year) + '.txt');
          dolugbulk := True;
        end;
      end;
    end;
  end;

  if dolugbulk then
  begin
    DrawInventoryTable(inv, lite, setid, True, True, lb);
    lb.Free;
  end
  else
    DrawInventoryTable(inv, lite, setid);

  document.write('<br>');
  document.write('<br>');
  if ShowInventorySets(inv, False, 3) then
  begin
    document.write('<br>');
    document.write('<br>');
  end;
  document.write('</p>');

  if not lite then
  begin
    minifiginv := inv.Minifigures;
    if minifiginv.numlooseparts > 0 then
    begin
      document.write('<p>');
      DrawHeadLine('Minifigures');
      DrawInventoryTableNoPages(minifiginv);
      document.write('<br>');
      document.write('<br>');
      document.write('</p>');
    end;
    minifiginv.free;
  end;

  if fexists(basedefault + 'db\sets\' + Trim(setid) + '.alternatives.txt') then
  begin
    addinv := TStringList.Create;
    addinv.LoadFromFile(basedefault + 'db\sets\' + Trim(setid) + '.alternatives.txt');
    DrawSetAlternatePieceList('Alternate parts <a href=downloadsetaltparts/' + Trim(setid) + '><img src="images\refresh.png"></a>', addinv);
    addinv.Free;
    document.write('<br>');
    document.write('<br>');
  end
  else if not db.IsMoc(setid) then
  begin
    DrawHeadLine('<a href=downloadsetaltparts/' + Trim(setid) + '>Try to download alternate parts from ' + s_bricklink + '.com</a>');
    document.write('<br>');
    document.write('<br>');
  end;
  document.write('</div>');
  if not lite then
    document.write(BLColorPieceInfo(Trim(setid), -1));
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowSetPartsStorage(const setid: string; const ppreview: boolean);
var
  inv: TBrickInventory;
  missing: integer;
  missing2: integer;
  st: set_t;
  i: integer;
  desc: string;
  tmpsets: TStringList;
  sx, ss1, ss2, ss3: string;
  sf1, sf2, sf3, sf4: string;
  numbricks: integer;
  numplates: integer;
  numtiles: integer;
  numslopes: integer;
  idx: integer;
  lnk: string;
  expensizestr: string;
  year: integer;
  slink: string;
  pci: TPieceColorInfo;
begin
  Screen.Cursor := crHourGlass;

  document.write('<body background="splash.jpg">');
  document.title('Set ' + setid);
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  inv := db.GetSetInventory(setid);
  if inv = nil then
  begin
    if Trim(setid) <> '' then
      lnk := '<a href=downloadset/' + setid + '>Try to download the inventory from ' + s_bricklink + '.com</a>'
    else
      lnk := '';
    pci := db.PieceColorInfo(setid, -1);
    if pci = nil then
      slink := setid
    else
      slink := '<a href="spiece/' + setid + '">' + setid + '</a>';
    DrawHeadLine('Can not find inventory for ' + slink + ' <a href=DoEditSet/' + setid + '><img src="images\edit.png"></a><br><br>' + lnk);
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');

    tmpsets := TStringList.Create;
    if IsValidDBMask(setid) then
    begin
      for i := 0 to db.AllSets.Count - 1 do
      begin
        desc := db.SetDesc(db.AllSets.Strings[i]);
        if MatchesMask(desc, setid) then
          tmpsets.Add(db.AllSets.Strings[i])
        else if MatchesMask(db.AllSets.Strings[i], setid) then
          tmpsets.Add(db.AllSets.Strings[i]);
        if tmpsets.Count >= 500 then
          Break;
      end;
    end;

    if tmpsets.Count = 0 then
    begin
      for i := 0 to db.AllSets.Count - 1 do
      begin
        desc := db.SetDesc(db.AllSets.Strings[i]);
        if Pos(strupper(setid), strupper(desc)) > 0 then
          tmpsets.Add(db.AllSets.Strings[i]);
      end;

      for i := 0 to db.AllSets.Count - 1 do
      begin
        desc := db.AllSets.Strings[i];
        if Pos(strupper(setid), strupper(desc)) > 0 then
          tmpsets.Add(desc);
      end;
    end;

    tmpsets.Sort;
    for i := tmpsets.Count - 1 downto 1 do
      if tmpsets.Strings[i] = tmpsets.Strings[i - 1] then
        tmpsets.Delete(i);

    for i := 0 to tmpsets.Count - 1 do
    begin
      inv := db.GetSetInventory(tmpsets.Strings[i]);
      DrawHeadLine(Format('<a href="sstorageloc/%s">%s - %s</a><br>(%d lots, %d parts, %d sets)<br><img width=360px src=s\' + tmpsets.Strings[i] + '.jpg>',
        [tmpsets.Strings[i], tmpsets.Strings[i], db.SetDesc(tmpsets.Strings[i]), inv.numlooseparts, inv.totallooseparts, inv.totalsetsbuilted + inv.totalsetsdismantaled]));
    end;
    tmpsets.Free;
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  inventory.GetSetInfo(setid, @st);
  sf1 := '<a href=addset/' + Trim(setid) + '>+</a>';
  if st.num > 0 then
    sf2 := '<a href=removeset/' + Trim(setid) + '>-</a>'
  else
    sf2 := '';
  sf3 := '<a href=addsetdismantaled/' + Trim(setid) + '>+</a>';
  if st.dismantaled > 0 then
    sf4 := '<a href=removesetdismantaled/' + Trim(setid) + '>-</a>'
  else
    sf4 := '';
  year := db.SetYear(setid);
  if ppreview then
    ss1 := Format('Storage Location for the inventory of %s - %s <br>(%d lots, %d parts, %d sets)<br>You have %s%d%s builted and %s%d%s dismantaled<br><img width=360px src=s\' + setid + '.jpg><br>',
    ['<a href=spiece/' + setid + '>' + setid + '</a>', db.SetDesc(setid), inv.numlooseparts, inv.totallooseparts, inv.totalsetsbuilted + inv.totalsetsdismantaled,
     sf2, st.num, sf1, sf4, st.dismantaled, sf3])
  else
    ss1 := Format('Storage Location for the inventory of %s - %s <br>(%d lots, %d parts, %d sets)<br>You have %s%d%s builted and %s%d%s dismantaled<br><img width=360px src=s\' + setid + '.jpg>' +
      ' <a href=DoEditSet/' + setid + '><img src="images\edit.png"></a>' +
//      ' <a href=PreviewSetInventory/' + setid + '><img src="images\print.png"></a>' +
      ' <a href=sstoragelocpv/' + setid + '><img src="images\print.png"></a>' +

      ' <a href=diagrampiece/' + setid + '/-1><img src="images\diagram.png"></a><br>' +

      '[Year: <a href=ShowSetsAtYear/%d>%d</a>]<br>',
    ['<a href=spiece/' + setid + '>' + setid + '</a>', db.SetDesc(setid), inv.numlooseparts, inv.totallooseparts, inv.totalsetsbuilted + inv.totalsetsdismantaled,
     sf2, st.num, sf1, sf4, st.dismantaled, sf3, year, year]);
  idx := db.allsets.IndexOf(Trim(setid));
  if idx > -1 then
  begin
    if idx > 0 then
      ss1 := ss1 + '<a href=sstorageloc/' + db.allsets.Strings[idx - 1] + '>Prev(' + db.allsets.Strings[idx - 1] + ' - ' + db.SetDesc(db.allsets.Strings[idx - 1]) + ')</a>  ';
    if idx < db.allsets.Count - 1 then
      ss1 := ss1 + decide(idx > 0, ' - ', ' ') + '<a href=sstorageloc/' + db.allsets.Strings[idx + 1] + '>Next(' + db.allsets.Strings[idx + 1] + ' - ' + db.SetDesc(db.allsets.Strings[idx + 1]) + ')</a>  ';
  end
  else
  begin
    splitstring(Trim(setid), ss2, ss3, '-');
    if ss3 <> '' then
    begin
      sx := itoa(atoi(ss2) - 1) + '-' + ss3;
      if db.SetDesc(sx) <> '' then
        ss1 := ss1 + '<a href=sstorageloc/' + sx + '>Prev(' + sx + ' - ' + db.SetDesc(sx) + ')</a>  ';
      sx := itoa(atoi(ss2) + 1) + '-' + ss3;
      if db.SetDesc(sx) <> '' then
        ss1 := ss1 + '<a href=sstorageloc/' + sx + '>Next(' + sx + ' - ' + db.SetDesc(sx) + ')</a>';
    end;
  end;
  DrawHeadLine(ss1);

  missing := inventory.MissingToBuildSet(setid);
  if st.num > 0 then
    DrawHeadLine('You can dismantle this set to your loose parts! <a href=dismantleset/' + Trim(setid) + '>(dismantle it!)</a>');

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + DBGCOLOR + '>');
  document.write('<td width 34%>');

  if missing = 0 then
    DrawHeadLine('You can built this set! <a href=buildset/' + Trim(setid) + '>(Built it!)</a>')
  else
    DrawHeadLine(Format('%d part' + decide(missing = 1, ' is', 's are') + ' missing to build a copy of this set (%2.2f%s) (<a href="missingtobuildset/%s">show</a>)',
      [missing, dbl_safe_div(missing * 100, inv.totallooseparts), '%', Trim(setid)]));

  document.write('</td><td width 33%>');

  missing2 := inventory.MissingToBuildSetLegacyIgnore(Trim(setid));
  DrawHeadLine(Format('%d part' + decide(missing2 <= 1, ' is', 's are') + ' missing to build a copy of this set ignoring legacy colors (%2.2f%s) (<a href="missingtobuildsetLI/%s">show</a>)',
    [missing2, dbl_safe_div(missing2 * 100, inv.totallooseparts), '%', Trim(setid)]));

  document.write('</td><td width 33%>');

  DrawHeadLine(Format('<a href="missingtobuildset/%s/2">Check to build 2 sets</a>', [Trim(setid)]));

  document.write('</td></tr></table>');

  numbricks := inv.totalloosepartsbycategory(5);
  numplates := inv.totalloosepartsbycategory(26);
  numtiles := inv.totalloosepartsbycategory(37) + inv.totalloosepartsbycategory(39);
  numslopes := inv.totalloosepartsbycategory(31) + inv.totalloosepartsbycategory(32) + inv.totalloosepartsbycategory(33);
  expensizestr := 'Most expensive lots: <a href="ShowExpensiveSetLotsNew/' + Trim(setid) + '/10">New</a> - <a href="ShowExpensiveSetLotsUsed/' + Trim(setid) + '/10">Used</a>';

  DrawHeadLine(Format('Bricks: %d<br>Slopes: %d<br>Plates: %d<br>Tiles: %d<br>Other: %d<br>' + expensizestr, [numbricks, numslopes, numplates, numtiles, inv.totallooseparts - numbricks - numplates - numslopes - numtiles]));

  DrawPriceguide(setid);

  DrawInventoryPartsStorage(inv, ppreview);

  document.write('<br>');
  document.write('<br>');

  document.write('</p>');

  document.write('</div>');

{  if not lite then
    document.write(BLColorPieceInfo(Trim(setid), -1));}

  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.DrawInventoryPartsStorage(const psinv: TBrickInventory; const ppreview: boolean);
var
  storagelst: TStringList;
  storagecompleteparts1, storagecompleteparts2, preferredparts: TStringList;
  i, aa: Integer;
  s1, s2, stmp: string;
  pci: TPieceColorInfo;
  olddodraworderinfo: boolean;
  dbstorageinvs: TStringList;
  tmpinv, missinginv, inv, readyinv, neededfromreadyinv: TBrickInventory;

  function _numfoundatstorage(const Apart: string; const Acolor: Integer): integer;
  var
    ii: integer;
  begin
    result := 0;
    for ii := 0 to storagelst.Count - 1 do
    begin
      (storagelst.Objects[ii] as TBrickInventory).DoReorganize;
      result := result + (storagelst.Objects[ii] as TBrickInventory).LoosePartCount(Apart, Acolor);
    end;
  end;

  procedure _removefromstorage(const Apart: string; const Acolor: Integer; const qty: integer = -1);
  var
    ii: integer;
    tot: integer;
    num1: integer;
    leftqty: integer;
  begin
    tot := _numfoundatstorage(Apart, Acolor);
    if tot = 0 then
      exit;

    if qty = -1 then
    begin
      for ii := 0 to storagelst.Count - 1 do
      begin
        num1 := (storagelst.Objects[ii] as TBrickInventory).loosepartcount(Apart, Acolor);
        if num1 > 0 then
        begin
          (storagelst.Objects[ii] as TBrickInventory).RemoveLoosePart(Apart, Acolor, num1);
          (storagelst.Objects[ii] as TBrickInventory).DoReorganize;
        end;
      end;
    end
    else
    begin
      leftqty := qty;
      if leftqty > 0 then
      begin
        for ii := 0 to storagelst.Count - 1 do
        begin
          num1 := (storagelst.Objects[ii] as TBrickInventory).loosepartcount(Apart, Acolor);
          if num1 > 0 then
            if num1 < leftqty then
            begin
              (storagelst.Objects[ii] as TBrickInventory).RemoveLoosePart(Apart, Acolor, num1);
              (storagelst.Objects[ii] as TBrickInventory).DoReorganize;
              leftqty := leftqty - num1;
            end;
        end;

        if leftqty > 0 then
        begin
          for ii := 0 to storagelst.Count - 1 do
          begin
            num1 := (storagelst.Objects[ii] as TBrickInventory).loosepartcount(Apart, Acolor);
            if num1 = leftqty then
            begin
              (storagelst.Objects[ii] as TBrickInventory).RemoveLoosePart(Apart, Acolor, num1);
              (storagelst.Objects[ii] as TBrickInventory).DoReorganize;
              leftqty := 0;
              break;
            end;
          end;

          if leftqty > 0 then
          begin
            for ii := 0 to storagelst.Count - 1 do
            begin
              num1 := (storagelst.Objects[ii] as TBrickInventory).loosepartcount(Apart, Acolor);
              if num1 > leftqty then
              begin
                (storagelst.Objects[ii] as TBrickInventory).RemoveLoosePart(Apart, Acolor, leftqty);
                (storagelst.Objects[ii] as TBrickInventory).DoReorganize;
                leftqty := 0;
                break;
              end;
            end;
          end;

        end;
      end;
    end;
  end;

  function _AddToStorageList1(const s: string; const Apart: string; const Acolor: Integer; const Anum: Integer): boolean;
  var
    idx: integer;
  begin
    Result := False;

    if Anum <= 0 then
      Exit;

    idx := storagecompleteparts1.IndexOf(Apart + ',' + itoa(Acolor));
    if idx >= 0 then
      exit;

    storagecompleteparts1.Add(Apart + ',' + itoa(Acolor));

    idx := storagelst.IndexOf(s);
    if idx < 0 then
      idx := storagelst.AddObject(s, TBrickInventory.Create);
    (storagelst.Objects[idx] as TBrickInventory).AddLoosePart(Apart, Acolor, Anum);

    Result := True;
  end;

  function _AddToStorageList2(const s: string; const Apart: string; const Acolor: Integer; const Anum: Integer): boolean;
  var
    idx: integer;
  begin
    Result := False;

    if Anum <= 0 then
      Exit;

    idx := storagecompleteparts1.IndexOf(Apart + ',' + itoa(Acolor));
    if idx >= 0 then
      exit;

    idx := storagecompleteparts2.IndexOf(Apart + ',' + itoa(Acolor));
    if idx >= 0 then
      exit;

    idx := storagelst.IndexOf(s);
    if idx < 0 then
      idx := storagelst.AddObject(s, TBrickInventory.Create);
    (storagelst.Objects[idx] as TBrickInventory).AddLoosePart(Apart, Acolor, Anum);
    (storagelst.Objects[idx] as TBrickInventory).DoReorganize;

    Result := True;
  end;

  procedure FindAPartStorage(const Apci: TPieceColorInfo; const needed: integer;
    const pass: integer);
  var
    Aoinf: TStringList;
    Aoitem: TOrderItemInfo;
    ii: integer;
    Ainv: TBrickInventory;
    pi: TPieceInfo;
    num: integer;
    Asid: string;
    foundall: boolean;
    numfound: integer;
    s1, s2: string;
    oid: integer;
  begin
    if pass = 0 then
    begin
      if Apci.prefferedlocation <> '' then
      begin
        if Pos1('LUGBULK ', Apci.prefferedlocation) then
        begin
          splitstring(Apci.prefferedlocation, s1, s2, ' ');
          s2 := strtrim(s2);
          Asid := 'LUGBULK-' + s2;
          Ainv := db.GetSetInventory(Asid);
          if Ainv <> nil then
          begin
            num := Ainv.LoosePartCount(Apci.piece, Apci.color);
            if num >= needed then
            begin
              _AddToStorageList2('set:' + Asid, Apci.piece, Apci.color, MinI(num, needed));
              preferredparts.Add(Apci.piece + ',' + itoa(Apci.color));
            end;
          end;
        end
        else if Pos1('ORDER ', Apci.prefferedlocation) then
        begin
          splitstring(Apci.prefferedlocation, s1, s2, ' ');
          s2 := strtrim(s2);
          oid := atoi(s2);
          Aoinf := orders.ItemInfo(Apci.piece, Apci.color);
          if Aoinf <> nil then
            for ii := 0 to Aoinf.Count - 1 do
            begin
              Aoitem := Aoinf.Objects[ii] as TOrderItemInfo;
              if Aoitem.orderid = oid then
                if needed <= Aoitem.num then
                begin
                  _AddToStorageList2('order:' + itoa(Aoitem.orderid), Apci.piece, Apci.color, needed);
                  preferredparts.Add(Apci.piece + ',' + itoa(Apci.color));
                end;
            end;
        end;
      end;
      Exit;
    end;

    if preferredparts.IndexOf(Apci.piece + ',' + itoa(Apci.color)) >= 0 then
      Exit;

    if pass = 1 then
    begin
      for ii := dbstorageinvs.Count - 1 downto 0 do
      begin
        Ainv := dbstorageinvs.Objects[ii] as TBrickInventory;
        num := Ainv.LoosePartCount(Apci.piece, Apci.color);
        if num >= needed then
          if _AddToStorageList1('storage:' + dbstorageinvs.Strings[ii], Apci.piece, Apci.color, needed) then
            Ainv.RemoveLoosePart(Apci.piece, Apci.color, needed);
      end;
    end;

    numfound := _numfoundatstorage(Apci.piece, Apci.color);
    if numfound >= needed then
      exit;

    if pass = 2 then
    begin

      for ii := dbstorageinvs.Count - 1 downto 0 do
      begin
        Ainv := dbstorageinvs.Objects[ii] as TBrickInventory;
        num := Ainv.LoosePartCount(Apci.piece, Apci.color);
        if num > 0 then
          if _AddToStorageList2('storage:' + dbstorageinvs.Strings[ii], Apci.piece, Apci.color, MinI(num, needed)) then
          begin
            Ainv.RemoveLoosePart(Apci.piece, Apci.color, MinI(num, needed));
            numfound := numfound + num;
            if numfound >= needed then
              exit;
          end;
      end;
    end;

    if pass = 3 then
    begin
      if optlocationslugbulk and (dismantaledsetsinv <> nil) then
        if dismantaledsetsinv.LoosePartCount(Apci.piece, Apci.color) > 0 then
          for ii := 0 to inventory.numsets - 1 do
            if inventory.sets[ii].dismantaled > 0 then
            begin
              Asid := inventory.sets[ii].setid;
              pi := db.PieceInfo(Asid);
              if pi <> nil then
                if pi.category = CATEGORYLUGBULK then
                begin
                  Ainv := db.GetSetInventory(Asid);
                  num := Ainv.LoosePartCount(Apci.piece, Apci.color);
                  if num > 0 then
                  begin
                    foundall := num >= needed;
                    if foundall then
                    begin
                      _removefromstorage(Apci.piece, Apci.color);
                      _AddToStorageList2('set:' + Asid, Apci.piece, Apci.color, MinI(num, needed));
                      storagecompleteparts2.Add(Apci.piece + ',' + itoa(Apci.color));
                      exit;
                    end
                    else
                      _AddToStorageList2('set:' + Asid, Apci.piece, Apci.color, MinI(num, needed));
                  end;
                end;
            end;

      for ii := 0 to dbstorageinvs.Count - 1 do
      begin
        Ainv := dbstorageinvs.Objects[ii] as TBrickInventory;
        num := Ainv.LoosePartCount(Apci.piece, Apci.color);
        if num > 0 then
          if _AddToStorageList2('storage:' + dbstorageinvs.Strings[ii], Apci.piece, Apci.color, MinI(num, needed)) then
          begin
            Ainv.RemoveLoosePart(Apci.piece, Apci.color, MinI(num, needed));
            foundall := num >= needed;
            if foundall then
              exit;
          end;
      end;

      if optlocationsorders then
      begin
        Aoinf := orders.ItemInfo(Apci.piece, Apci.color);
        if Aoinf <> nil then
          for ii := 0 to Aoinf.Count - 1 do
          begin
            Aoitem := Aoinf.Objects[ii] as TOrderItemInfo;
            _AddToStorageList2('order:' + itoa(Aoitem.orderid), Apci.piece, Apci.color, MinI(Aoitem.num, needed));
          end;
      end;
    end;

  end;

  procedure _removeunused;
  var
    ii: integer;
  begin
    for ii := storagelst.Count - 1 downto 0 do
    begin
      (storagelst.Objects[ii] as TBrickInventory).DoReorganize;
      if (storagelst.Objects[ii] as TBrickInventory).totallooseparts = 0 then
      begin
        (storagelst.Objects[ii] as TBrickInventory).Free;
        storagelst.Delete(ii);
      end;
    end;
  end;

  procedure _checkoverflow(const Apci: TPieceColorInfo; const needed: integer);
  var
    atstorage: integer;
  begin
    atstorage := _numfoundatstorage(Apci.piece, Apci.color);
    if atstorage > needed then
      _removefromstorage(Apci.piece, Apci.color, atstorage - needed);
  end;

  procedure _trytomergestorages(const nlots: integer);
  var
    ii, jj: integer;
    stinv, stinv2, dbinv: TBrickInventory;
    st1, st2: string;
    idx: integer;
    changed: boolean;
  begin
    changed := false;
    for ii := 0 to storagelst.Count - 1 do
      if Pos1('storage:', storagelst.Strings[ii]) then
      begin
        stinv := storagelst.Objects[ii] as TBrickInventory;
        if stinv.numlooseparts = nlots then
        begin
          for jj := 0 to storagelst.Count - 1 do
            if jj <> ii then
              if Pos1('storage:', storagelst.Strings[jj]) then
              begin
                stinv2 := (storagelst.Objects[jj] as TBrickInventory);
                if stinv2.numlooseparts > 0 then
                begin
                  splitstring(storagelst.Strings[jj], st1, st2, ':');
                  idx := dbstorageinvs.IndexOf(st2);
                  if idx >= 0 then
                  begin
                    dbinv := dbstorageinvs.Objects[idx] as TBrickInventory;
                    if dbinv.CanBuildInventory(stinv) then
                    begin
                      stinv2.MergeWith(stinv);
                      stinv.Clear;
                      changed := True;
                    end;
                  end;
                end;
              end;
        end;
      end;
    if changed then
      _removeunused;
  end;

begin
  Screen.Cursor := crHourGlass;
  ShowSplash;
  SplashProgress('Working...', 0);

  inv := psinv.Clone;

  UpdateDismantaledsetsinv;

  if optlocationsreadylist then
  begin
    readyinv := BI_GetReadyListInv(S_READYLIST_01);
    inv.TryToRemoveInventory(readyinv);
    neededfromreadyinv := psinv.Clone;
    neededfromreadyinv.TryToRemoveInventory(inv);
  end
  else
  begin
    neededfromreadyinv := nil;
    readyinv := nil;
  end;

  storagelst := TStringList.Create;
  storagelst.Sorted := True;
  storagecompleteparts1 := TStringList.Create;
  storagecompleteparts1.Sorted := True;
  storagecompleteparts2 := TStringList.Create;
  storagecompleteparts2.Sorted := True;
  preferredparts := TStringList.Create;
  preferredparts.Sorted := True;

  dbstorageinvs := db.InventoriesForAllStorageBins;
  dbstorageinvs.Sorted := True;

  // Pass 0 - Preffered locations
  for i := 0 to inv.numlooseparts - 1 do
  begin
    pci := db.PieceColorInfo(@inv.looseparts[i]);
    if pci <> nil then
      FindAPartStorage(pci, inv.looseparts[i].num, 0);
  end;

  // Pass 1
  for i := 0 to inv.numlooseparts - 1 do
  begin
    pci := db.PieceColorInfo(@inv.looseparts[i]);
    if pci <> nil then
      FindAPartStorage(pci, inv.looseparts[i].num, 1);
  end;
  _trytomergestorages(1);
  SplashProgress('Working...', 0.05);

  // Pass 2
  for i := 0 to inv.numlooseparts - 1 do
  begin
    pci := db.PieceColorInfo(@inv.looseparts[i]);
    if pci <> nil then
      FindAPartStorage(pci, inv.looseparts[i].num, 2);
  end;
  SplashProgress('Working...', 0.10);

  // Pass 3
  for i := 0 to inv.numlooseparts - 1 do
  begin
    pci := db.PieceColorInfo(@inv.looseparts[i]);
    if pci <> nil then
      if _numfoundatstorage(pci.piece, pci.color) < inv.looseparts[i].num then
        FindAPartStorage(pci, inv.looseparts[i].num, 3);
  end;
  SplashProgress('Working...', 0.15);

  // Remove unused storages
  _removeunused;

  for i := 0 to inv.numlooseparts - 1 do
  begin
    pci := db.PieceColorInfo(@inv.looseparts[i]);
    if pci <> nil then
      _checkoverflow(pci, inv.looseparts[i].num);
  end;

  // Remove unused storages
  _trytomergestorages(1);
  _trytomergestorages(2);
  _trytomergestorages(3);
  _removeunused;

  tmpinv := TBrickInventory.Create;
  for i := 0 to storagelst.Count - 1 do
    tmpinv.MergeWith(storagelst.Objects[i] as TBrickInventory);
  missinginv := tmpinv.InventoryForMissingToBuildInventory(inv);

  stmp := ' (Storage bins';
  if optlocationsreadylist then
    stmp := stmp + ' + Ready List';
  if optlocationslugbulk then
    stmp := stmp + ' + LugBulks';
  if optlocationsorders then
    stmp := stmp + ' + orders';
  stmp := stmp + ')';

  DrawHeadLine(Format(
    'Found %d parts (%d lots) in %d storages out of %d parts (%d lots)' + stmp,
      [tmpinv.totallooseparts, tmpinv.numlooseparts, storagelst.Count, inv.totallooseparts, inv.numlooseparts]
    ));

  tmpinv.Free;
  SplashProgress('Working...', 0.20);

  olddodraworderinfo := dodraworderinfo;
  dodraworderinfo := false;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=' + itoa(decide(ppreview, 1, 2)) + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Storage Location</b></th>');

  aa := 0;
  if readyinv <> nil then
    if neededfromreadyinv.totallooseparts > 0 then
    begin
      inc(aa);
      document.write('<tr bgcolor=' + decide(ppreview, TBGCOLOR, THBGCOLOR) + '>');
      document.write('<td width=5% align=right>' + itoa(aa));

      stmp := Format(' (%d lots, %d parts)', [neededfromreadyinv.numlooseparts, neededfromreadyinv.totallooseparts]);

      document.write('<td width=95%><b>Set <a href="sinv/' + S_READYLIST_01 + '">' + db.SetDesc(S_READYLIST_01) + '</a></b>' + stmp + '</td>');

      document.write('</tr>');

      document.write('<tr bgcolor=' + decide(ppreview, TBGCOLOR, THBGCOLOR) + '><td colspan="2">');

      DrawInventoryTableForPartsStorageQuery(neededfromreadyinv, psinv, ppreview);

      document.write('</td></tr>');
    end;

  if neededfromreadyinv <> nil then
    neededfromreadyinv.Free;

  for i := 0 to storagelst.Count - 1 do
  begin
    inc(aa);
    document.write('<tr bgcolor=' + decide(ppreview, TBGCOLOR, THBGCOLOR) + '>');
    document.write('<td width=5% align=right>' + itoa(aa));

    splitstring(storagelst.Strings[i], s1, s2, ':');
    tmpinv := storagelst.Objects[i] as TBrickInventory;
    stmp := Format(' (%d lots, %d parts)', [tmpinv.numlooseparts, tmpinv.totallooseparts]);

    if s1 = 'order' then
      document.write('<td width=95%><b>Order <a href="order/' + s2 + '">' + s2 + '</a></b>' + stmp + '</td>')
    else if s1 = 'set' then
      document.write('<td width=95%><b>Set <a href="sinv/' + s2 + '">' + s2 + '</a></b>' + stmp + '</td>')
    else if s1 = 'storage' then
      document.write('<td width=95%><b>Storage <a href="storage/' + s2 + '">' + s2 + '</a></b>' + stmp + '</td>');

    document.write('</tr>');

    document.write('<tr bgcolor=' + decide(ppreview, TBGCOLOR, THBGCOLOR) + '><td colspan="2">');

    DrawInventoryTableForPartsStorageQuery(tmpinv, inv, ppreview);

    document.write('</td></tr>');

    if i mod 10 = 0 then
      SplashProgress('Working...', 0.2 + 0.75 * i / storagelst.Count);
  end;
  document.write('</table>');

  dodraworderinfo := olddodraworderinfo;

  SplashProgress('Working...', 1);

  if missinginv.numlooseparts > 0 then
  begin
    DrawHeadLine(
      Format('Missing parts (%d parts in %d lots)', [missinginv.totallooseparts, missinginv.numlooseparts]));
    DrawInventoryTableNoPages(missinginv, False, '', True, False);
  end;

  missinginv.Free;
  FreeList(storagelst);
  FreeList(dbstorageinvs);
  storagecompleteparts1.Free;
  storagecompleteparts2.Free;
  preferredparts.Free;
  inv.Free;

  HideSplash;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.PreviewSetInventory(const setid: string);
var
  inv: TBrickInventory;
  i, j: integer;
  desc: string;
  tmpsets: TStringList;
  s1, s2: string;
  ss1: string;
begin
  Screen.Cursor := crHourGlass;

  lastset := setid;

  document.write('<body background="splash.jpg">');
  document.title('Set ' + setid + ' (preview)');
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  inv := db.GetSetInventory(setid);
  if inv = nil then
  begin
    DrawHeadLine('Can not find inventory for ' + setid + '<br><br>');
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');

    tmpsets := TStringList.Create;
    for i := 0 to db.AllSets.Count - 1 do
    begin
      desc := db.SetDesc(db.AllSets.Strings[i]);
      if Pos(strupper(setid), strupper(desc)) > 0 then
        tmpsets.Add(db.AllSets.Strings[i]);
    end;

    if tmpsets.Count = 0 then
      for i := 0 to db.AllSets.Count - 1 do
      begin
        desc := db.AllSets.Strings[i];
        if Pos(strupper(setid), strupper(desc)) > 0 then
          tmpsets.Add(desc);
      end;

    tmpsets.Sort;

    for i := 0 to tmpsets.Count - 1 do
    begin
      inv := db.GetSetInventory(tmpsets.Strings[i]);
      DrawHeadLine(Format('<a href="sinv/%s">%s - %s</a><br>(%d lots, %d parts, %d sets)<br><img width=360px src=s\' + tmpsets.Strings[i] + '.jpg>',
        [tmpsets.Strings[i], tmpsets.Strings[i], db.SetDesc(tmpsets.Strings[i]), inv.numlooseparts, inv.totallooseparts, inv.totalsetsbuilted + inv.totalsetsdismantaled]));
    end;
    tmpsets.Free;
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  inv.DoUpdateCostValues;
  SortInventory(inv);

  s1 := basedefault + 'out\' + Trim(setid) + '\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s2 := s1 + Trim(setid) + '.txt';
  inv.SaveLoosePartsAndSets(s2);

  s2 := s1 + setid + '_priceguide.txt';
  inv.SavePartsInventoryPriceguide(s2);

  inv.StoreHistoryStatsRec(basedefault + 'out\' + Trim(setid) + '\' + Trim(setid) + '.stats');
  inv.StoreHistoryEvalRec(basedefault + 'out\' + Trim(setid) + '\' + Trim(setid) + '.ieval');
  inventory.StorePieceInventoryStatsRec(basedefault + 'out\' + Trim(setid) + '\' + Trim(setid) + '.history', Trim(setid), -1);

  for j := 0 to inv.numlooseparts - 1 do
    inventory.StorePieceInventoryStatsRec(
      basedefault + 'cache\' + itoa(decide(inv.looseparts[j].color = -1, 9999, inv.looseparts[j].color)) + '\' + inv.looseparts[j].part + '.history',
      inv.looseparts[j].part,
      inv.looseparts[j].color
    );
  for j := 0 to inv.numsets - 1 do
    inventory.StorePieceInventoryStatsRec(
      basedefault + 'out\' + Trim(inv.sets[j].setid) + '\' + Trim(inv.sets[j].setid) + '.history',
      Trim(inv.sets[j].setid), -1
    );

  if inv.totalsetsbuilted + inv.totalsetsdismantaled = 0 then
    ss1 := Format('%s - %s <br>(%d lots, %d parts)<br><img width=360px src=s\' + setid + '.jpg><br>',
      [setid, db.SetDesc(setid), inv.numlooseparts, inv.totallooseparts])
  else
    ss1 := Format('%s - %s <br>(%d lots, %d parts, %d sets)<br><img width=360px src=s\' + setid + '.jpg><br>',
      [setid, db.SetDesc(setid), inv.numlooseparts, inv.totallooseparts, inv.totalsetsbuilted + inv.totalsetsdismantaled]);
  DrawHeadLine(ss1);

  DrawPartOutValue(inv);
  PreviewInventoryTable(inv);

  document.write('<br>');
  document.write('<br>');
  if ShowInventorySets(inv, False, 3) then
  begin
    document.write('<br>');
    document.write('<br>');
  end;
  document.write('</p>');

  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowColors;
var
  inv: TBrickInventory;
  cp: colorinfo_p;
  aa, i: Integer;
  tlots, tparts: integer;
  tweight: double;
begin
  Screen.Cursor := crHourGlass;

  document.write('<body background="splash.jpg">');
  document.title('Colors');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  inv := inventory;
  if inv = nil then
  begin
    DrawHeadLine('Inventory is null');
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  DrawHeadLine('My Inventory - Colors');

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Color</b></th>');
  document.write('<th>Lots</th>');
  document.write('<th>Num pieces</th>');
  document.write('<th>Weight (Kg)</th>');
  document.write('</tr>');

  ShowSplash;
  SplashProgress('Working...', 0);

  tlots := 0;
  tparts := 0;
  tweight := 0.0;
  aa := 0;
  for i := -1 to MAXINFOCOLOR do
  begin
    cp := db.Colors(i);
    if (cp.id <> i) and (i <> -1) then
      Continue;
    inc(aa);
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');
    document.write('<td width=40%>');
    DrawColorCell(i, 25);
//    document.BlancColorCell(cp.RGB, 25);
    document.write('<a href="inv/' + IntToStr(Integer(inv)) +'/C/' + IntToStr(decide(i = -1, 9999, i)) + '">');
    document.write('<b>' + cp.name + '</b> (' + IntToStr(cp.id) + ') (BL=' + IntToStr(cp.BrickLingColor) +  ')' +
      GetRebrickableColorHtml(i) + '</a></td>');
    document.write('<td width=20% align=right>' + IntToStr(inv.numlotsbycolor(i)) + '</td>');
    document.write('<td width=20% align=right>' + IntToStr(inv.totalloosepartsbycolor(i)) + '</td>');
    document.write('<td width=20% align=right>' + Format('%2.3f', [inv.weightbycolor(i) / 1000]) + '</td>');

    if i >= 0 then
    begin
      tlots := tlots + inv.numlotsbycolor(i);
      tparts := tparts + inv.totalloosepartsbycolor(i);
      tweight := tweight + inv.weightbycolor(i) / 1000;
    end;

    document.write('</tr>');
    SplashProgress('Working...', (i + 1) / (MAXINFOCOLOR + 1));
  end;

  document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>*</td>');
  document.write('<td width=40%><b>Total</b></td>');
  document.write('<td width=20% align=right>' + IntToStr(tlots) + '</td>');
  document.write('<td width=20% align=right>' + IntToStr(tparts) + '</td>');
  document.write('<td width=20% align=right>' + Format('%2.3f', [tweight]) + '</td>');
  document.write('</tr>');

  document.write('</tr></table>');
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowTags;
var
  tags: TStringList;
  i: integer;
begin
  Screen.Cursor := crHourGlass;

  document.write('<body background="splash.jpg">');
  document.title('Tags');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine('Tags');

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Tag</b></th>');
  document.write('<th>Num parts</th>');
  document.write('</tr>');

  ShowSplash;
  SplashProgress('Working...', 0);

  tags := db.AllTags;
  tags.Sort;
  for i := 0 to tags.Count - 1 do
  begin
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(i + 1) + '.</td>');
    document.write('<td width=60%>' + taglink(tags.Strings[i]) + '</td>');
    document.write('<td width=35% align=right>' + IntToStr((tags.Objects[i] as TStringList).Count) + '</td>');

    document.write('</tr>');
    SplashProgress('Working...', (i + 1) / (tags.Count + 1));
  end;

  document.write('</table>');
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowTag(const tag: string);
var
  i: integer;
  lst: TStringList;
  pci: TPieceColorInfo;
  inv: TBrickInventory;
  s1: string;
  alltags: TStringList;
  idx: integer;
  tagpieces: TStringList;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  alltags := db.AllTags;
  idx := alltags.IndexOf(tag);
  if idx >= 0 then
  begin
    tagpieces := alltags.Objects[idx] as TStringList;
    if tagpieces <> nil then
      for i := 0 to tagpieces.Count - 1 do
      begin
        pci := tagpieces.Objects[i] as TPieceColorInfo;
        lst.Add(tagpieces.Strings[i]);
        inv.AddLoosePartFast(pci.piece, pci.color, 1, pci);
      end;
  end;

  lst.Sort;
  DrawPieceList('Tag: "' + '<a href="showtaginv/' + tag + '">' + tag + '</a>"<br>', lst, SORT_NONE, '', 'Tag: "' + tag + '"');
  lst.Free;

  s1 := basedefault + 'out\Tags\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'Tag_' + MakePathString(tag);
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListUsed(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListUsed(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListUsed(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListUsed(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListUsed(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListUsed(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListUsed(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListUsed(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_030%.xml', 0.3);

  inv.Free;
end;

procedure TMainForm.ShowTagInv(const tag: string);
var
  i: integer;
  pci: TPieceColorInfo;
  inv, minifiginv: TBrickInventory;
  s1: string;
  alltags: TStringList;
  idx: integer;
  tagpieces: TStringList;
begin
  Screen.Cursor := crHourGlass;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowInventoryForTag', tag);

  document.write('<body background="splash.jpg">');
  document.title('Inventory for tag "' + tag + '"');
  DrawNavigateBar;

  DrawHeadLine(Format('Inventory for tag "%s"',
    [taglink(tag)]));

  inv := TBrickInventory.Create;

  alltags := db.AllTags;
  idx := alltags.IndexOf(tag);
  if idx >= 0 then
  begin
    tagpieces := alltags.Objects[idx] as TStringList;
    if tagpieces <> nil then
      for i := 0 to tagpieces.Count - 1 do
      begin
        pci := tagpieces.Objects[i] as TPieceColorInfo;
        if pci.color = -1 then
          inv.AddLoosePartFast(pci.piece, pci.color, inventory.loosepartcount(pci.piece, pci.color) + inventory.totalsetsbuilted(pci.piece), pci)
        else
          inv.AddLoosePartFast(pci.piece, pci.color, inventory.loosepartcount(pci.piece, pci.color), pci);
      end;
  end;

  inv.DismandalAllSets;
  DrawInventoryTable(inv);

  minifiginv := inv.Minifigures;
  if minifiginv.numlooseparts > 0 then
  begin
    document.write('<p>');
    DrawHeadLine('Minifigures');
    DrawInventoryTableNoPages(minifiginv);
    document.write('<br>');
    document.write('<br>');
    document.write('</p>');
  end;
  minifiginv.free;

  s1 := basedefault + 'out\Tags\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'TagInv_' + MakePathString(tag);
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListUsed(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListUsed(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListUsed(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListUsed(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListUsed(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListUsed(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListUsed(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListUsed(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_030%.xml', 0.3);

  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;

  inv.Free;

  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowCategoryColors(const cat: integer);
var
  inv: TBrickInventory;
  cp: colorinfo_p;
  aa, i: Integer;
  tlots, tparts: integer;
  tweight: double;
begin
  Screen.Cursor := crHourGlass;

  document.write('<body background="splash.jpg">');
  document.title(db.categories[cat].name);
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  inv := inventory;
  if inv = nil then
  begin
    DrawHeadLine('Inventory is null');
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  DrawHeadLine('My Inventory - <a href="inv/' + IntToStr(Integer(inv)) +'/CAT/' + IntToStr(cat) + '"><b>' + db.categories[cat].name + '</b></a>');

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Color</b></th>');
  document.write('<th>Lots</th>');
  document.write('<th>Num pieces</th>');
  document.write('<th>Weight (Kg)</th>');
  document.write('</tr>');

  ShowSplash;
  SplashProgress('Working...', 0);

  tlots := 0;
  tparts := 0;
  tweight := 0.0;
  aa := 0;
  for i := -1 to MAXINFOCOLOR do
  begin
    cp := db.Colors(i);
    if (cp.id <> i) and (i <> -1) then
      Continue;
    if inv.numlotsbycatcolor(i, cat) > 0 then
    begin
      inc(aa);
      document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');
      document.write('<td width=50%>');
      DrawColorCell(i, 25);
      // document.BlancColorCell(cp.RGB, 25);
      document.write('<a href="invex/' + IntToStr(Integer(inv)) + '/' + IntToStr(decide(i = -1, 9999, i)) + '/' + IntToStr(cat) + '">');
      document.write('<b>' + cp.name + '</b> (' + IntToStr(cp.id) + ') (BL=' + IntToStr(cp.BrickLingColor) +  ')' +
        GetRebrickableColorHtml(i) + '</a></td>');
      document.write('<td width=25% align=right>' + IntToStr(inv.numlotsbycatcolor(i, cat)) + '</td>');
      document.write('<td width=25% align=right>' + IntToStr(inv.totalloosepartsbycatcolor(i, cat)) + '</td>');
      document.write('<td width=25% align=right>' + Format('%2.3f', [inv.weightbycatcolor(i, cat) / 1000]) + '</td>');

      if i >= 0 then
      begin
        tlots := tlots + inv.numlotsbycatcolor(i, cat);
        tparts := tparts + inv.totalloosepartsbycatcolor(i, cat);
        tweight := tweight + inv.weightbycatcolor(i, cat) / 1000;
      end;

      document.write('</tr>');

    end;
    SplashProgress('Working...', (i + 1) / (MAXINFOCOLOR + 1));
  end;

  document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>*</td>');
  document.write('<td width=50%><b>Total</b></td>');
  document.write('<td width=25% align=right>' + IntToStr(tlots) + '</td>');
  document.write('<td width=25% align=right>' + IntToStr(tparts) + '</td>');
  document.write('<td width=25% align=right>' + Format('%2.3f', [tweight]) + '</td>');
  document.write('</tr>');

  document.write('</tr></table>');
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  Screen.Cursor := crDefault;
end;


type
  categoryindex_t = record
    category: categoryinfo_t;
    index: integer;
  end;

type
  categoryindexarray_t = array[0..MAXCATEGORIES - 1] of categoryindex_t;
  categoryindexarray_p = ^categoryindexarray_t;

procedure QSortCategories(const A: categoryindexarray_p; const Len: integer);

  procedure qsortI(l, r: Integer);
  var
    i, j: integer;
    t: categoryindex_t;
    d: string;
  begin
    repeat
      i := l;
      j := r;
      d := UpperCase(A[(l + r) shr 1].category.name);
      repeat
        while UpperCase(A[i].category.name) < d do
          inc(i);
        while UpperCase(A[j].category.name) > d do
          dec(j);
        if i <= j then
        begin
          t := A[i];
          A[i] := A[j];
          A[j] := t;
          inc(i);
          dec(j);
        end;
      until i > j;
      if l < j then
        qsortI(l, j);
      l := i;
    until i >= r;
  end;

begin
  if Len > 1 then
    qsortI(0, Len - 1);
end;


function TMainForm.HtmlDrawInvImgLink(const pcs: string; const color: integer;
  const pi: TPieceInfo): string;
var
  inv: TBrickInventory;
  pci: TPieceColorInfo;
  instructions_exist: Boolean;
begin
  if (color = -1) or (color = 9999) or (color = 89) then
  begin
    inv := db.GetSetInventory(pcs);
    if inv <> nil then
      Result := ' <a href=sinv/' + pcs + '>' + '<img src="images\inv.png"></img></a>'
    else
    begin
      if color = -1 then
        inv := db.PieceInfo(pcs).Inventory(color);
      if inv <> nil then
      begin
        Result := ' <a href=spiececinv/' + pcs + '/' + itoa(color) + '>' + '<img src="images\inv.png"></img></a>';
        inv.Free;
      end
      else
      begin
        Result := '';
        pci := nil;
        if color = -1 then
          pci := db.PieceColorInfo(pcs, -1);
        if pci <> nil then
          if pci.ItemType = 'S' then
            Result := ' <a href=DoEditSet/' + pcs + '><img src="images\edit.png">';
      end;
    end;
    Exit;
  end;

  if (color = 9996) or (color = 9998) then
  begin
    Result := '';
    Exit;
  end;

  if color = 9997 then
  begin
    Result := ' <a href=updateinstructionsfromnet/' + pcs + '>' + '<img src="images\refresh.png"></img></a>' +
              ' <a href=updateinstructionsfrompdf/' + pcs + '>' + '<img src="images\pdf.png"></img></a>';
    instructions_exist := InstructionsExist(pcs);
    if instructions_exist or (findfile(basedefault + InstructionDir(pcs) + '*.pdf') <> '') then
      Result := Result + ' <a href=instructions/' + pcs + '>' + '<img src="images\instructions.png"></img></a>'
    else if not instructions_exist then
      Result := Result + ' <a href=instructions/' + pcs + '>' + '<img src="images\instructions_query.png"></img></a>';
    Exit;
  end;

  if pi = nil then
  begin
    Result := '';
    Exit;
  end;

  if pi.hasinventory then
  begin
    Result := ' <a href=spiececinv/' + pcs + '/' + itoa(color) + '>' + '<img src="images\inv.png"></img></a>';
    if savealwayspartinvinfo then
    begin
      inv := pi.Inventory(color);
      if inv <> nil then
      begin
        if inv.numlooseparts > 0 then
        begin
          if not DirectoryExists(basedefault + 'cache') then
            MkDir(basedefault + 'cache\');
          if not DirectoryExists(basedefault + 'cache\' + itoa(color)) then
            MkDir(basedefault + 'cache\' + itoa(color));
          inv.StoreHistoryStatsRec(basedefault + 'cache\' + itoa(color) + '\' + pcs + '.stats');
          inv.StoreHistoryEvalRec(basedefault + 'cache\' + itoa(color) + '\' + pcs + '.ieval');
        end;
        inv.Free;
      end;
    end;
    Exit;
  end
  else
    Result := '';
end;

function TMainForm.HtmlDrawInvCode(const pci: TPieceColorInfo; const extras1, extras2: string): string;
begin
  if pci = nil then
  begin
    Result := '';
    Exit;
  end;

  if pci.code = '' then
  begin
    Result := '';
    Exit;
  end;

  Result := extras1 + 'Code: <a href=spiece/' + pci.code + '>' + pci.code + '</a>' + extras2;
end;

procedure TMainForm.ShowCategories;
var
  inv: TBrickInventory;
  cp: categoryinfo_p;
  aa, i: Integer;
  tlots, tparts: integer;
  tweight: double;
  nparts: integer;
  nweight: double;
  nlots: integer;
  A: categoryindexarray_t;
  numcategories: integer;
  catid: integer;
  CA, CL: categorysum_t;
  CW: categorydouble_t;
begin
  Screen.Cursor := crHourGlass;

  document.write('<body background="splash.jpg">');
  document.title('Categories');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  inv := inventory;
  if inv = nil then
  begin
    DrawHeadLine('Inventory is null');
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  DrawHeadLine('My Inventory - Categories');

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Category</b></th>');
  document.write('<th>Lots</th>');
  document.write('<th>Num pieces</th>');
  document.write('<th>Weight (Kg)</th>');
  document.write('</tr>');

  ShowSplash;
  SplashProgress('Working...', 0);
  tlots := 0;
  tparts := 0;
  tweight := 0.0;
  aa := 0;

  numcategories := 0;
  for i := 0 to MAXCATEGORIES - 1 do
  begin
    cp := @db.categories[i];
    if (cp.knownpieces = nil) or (cp.knownpieces.Count = 0) then
      Continue;
    A[numcategories].category := cp^;
    A[numcategories].index := i;
    inc(numcategories);
  end;
  QSortCategories(@A, numcategories);

  inv.partscategorysum(@CA);
  inv.lotscategorysum(@CL);
  inv.weightcategorysum(@CW);
  for i := 0 to numcategories - 1 do
  begin
    cp := @A[i].category;
    catid := A[i].index;
    nparts := CA[catid]; // inv.totalloosepartsbycategory(catid);
    if nparts > 0 then
    begin
      inc(aa);
      document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');
      document.write('<td width=50%>');
      document.write('<a href="inv/' + IntToStr(Integer(inv)) +'/CAT/' + IntToStr(catid) + '">');
      document.write('<b>' + cp.name + '</b></a> ');
      document.write('<a href="catcolors/' + IntToStr(catid) + '"><img src="images\colors.png"></a>');
//      document.write('<b>...</b></a>');

      document.write('</td>');
      nlots := CL[catid]; // inv.numlotsbycategory(catid);
      document.write('<td width=25% align=right>' + IntToStr(nlots) + '</td>');
      document.write('<td width=25% align=right>' + IntToStr(nparts) + '</td>');
      nweight := CW[catid] / 1000; // inv.weightbycategory(catid) / 1000;
      document.write('<td width=25% align=right>' + Format('%2.3f', [nweight]) + '</td>');

      tlots := tlots + nlots;
      tparts := tparts + nparts;
      tweight := tweight + nweight;

      document.write('</tr>');
    end;
    SplashProgress('Working...', i / numcategories);
  end;

  document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>*</td>');
  document.write('<td width=50%><b>Total</b></td>');
  document.write('<td width=25% align=right>' + IntToStr(tlots) + '</td>');
  document.write('<td width=25% align=right>' + IntToStr(tparts) + '</td>');
  document.write('<td width=25% align=right>' + Format('%2.3f', [tweight]) + '</td>');
  document.write('</tr>');

  document.write('</tr></table>');
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  Screen.Cursor := crDefault;
end;

function TMainForm.GetAPieceColor(pcs: string): integer;
var
  i: integer;
  idx: integer;
begin
  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
      if db.Colors(i).knownpieces <> nil then
      begin
        idx := db.Colors(i).knownpieces.IndexOfUCS(pcs);
        if idx >= 0 then
        begin
          Result := i;
          Exit;
        end;
      end;
  Result := 0;
end;

procedure TMainForm.ShowPiece(pcs: string; const year: integer = -1);
var
  i: integer;
  idx: Integer;
  aa: integer;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  prn, pru: double;
  prnt, prut: double;
  numpieces: integer;
  mycost: Double;
  bp: brickpool_t;
  desc: string;
  tmpsets: TStringList;
  totpieces: integer;
  mycosttot: double;
  storages: TStringList;
  stortxt: string;
  cathtml: string;
  refrhtml: string;
  refrcathtml: string;
  nextP, prevP: string;
  spiece: string;
  scolor: integer;
  upcs: string;
  didmyinventoryheader: boolean;
  pcs1: string;
  pyears: TDNumberList;
  pyearsstr: string;
  cmolds: TStringList;
  pfamilystr: string;
  validmask: boolean;
  cinfo: colorinfo_p;
  willmultipage: boolean;
  si: string;
  ypdateyearlink: string;
  srelationships: string;
  rlst: TStringList;
  ts: TString;
  www: double;
begin
  UpdateDismantaledsetsinv;

  pcs1 := db.RebrickablePart(pcs);
  idx := db.AllPieces.IndexOf(pcs1);

  if idx >= 0 then
    pcs := pcs1;

  if idx < 0 then
  begin
    pcs1 := db.BrickLinkPart(pcs);
    idx := db.AllPieces.IndexOf(pcs1);
    if idx >= 0 then
      pcs := pcs1;
  end;

  if idx < 0 then
  begin
    upcs := strupper(pcs);
    for i := 0 to db.AllPieces.Count - 1 do
    begin
      if upcs = strupper(db.AllPieces.Strings[i]) then
      begin
        idx := i;
        break;
      end
    end;
  end;

  if idx < 0 then
  begin
    if db.GetPieceColorFromCode(pcs, spiece, scolor) then
    begin
      ShowColorPiece(spiece, scolor);
      Exit;
    end;

    if domultipagedocuments then
      document.NewMultiPageDocument('ShowPiece_', pcs + '_' + itoa(year));

    document.write('<body background="splash.jpg">');
    document.title(pcs + ' - ' + db.PieceDesc(pcs));
    DrawNavigateBar;
    document.write('<div style="color:' + DFGCOLOR + '">');
    document.write('<p align=center>');

    if (Pos('?', pcs) <= 0) and (Pos('*', pcs) <= 0) and (Trim(pcs) <> '') then
    begin
      DrawHeadLine('Can not find piece ' + pcs + ' <a href=editmold/' + Trim(pcs) + '><img src="images\edit.png"></a>' +
                                                 ' <a href=refreshpieceorgearfrombricklink/' + Trim(pcs) + '><img src="images\refreshcolors.png"></a>');
      willmultipage := False;
    end
    else
    begin
      DrawHeadLine('Can not find piece ' + pcs);
      willmultipage := True;
    end;

    document.write('<br>');
    document.write('</p>');
    document.write('</div>');

    tmpsets := TStringList.Create;

    validmask := IsValidDBMask(pcs);
    upcs := strupper(pcs);

    for i := 0 to db.AllPieces.Count - 1 do
    begin
      if validmask then
      begin
        if MatchesMask(db.AllPieces.Strings[i], pcs) then
          tmpsets.Add(db.AllPieces.Strings[i])
        else if MatchesMask(db.PieceDesc(db.AllPieces.Objects[i] as TPieceInfo), pcs) then
          tmpsets.Add(db.AllPieces.Strings[i])
        else if MatchesMask(db.GetNewPieceName(db.AllPieces.Strings[i]), pcs) then
          tmpsets.Add(db.AllPieces.Strings[i]);
//        if tmpsets.Count >= 500 then
//          Break;
      end;

      if upcs = strupper(db.GetNewPieceName(db.AllPieces.Strings[i])) then
        tmpsets.Insert(0, db.AllPieces.Strings[i]);
    end;

    if tmpsets.Count = 0 then
    begin
      upcs := strupper(pcs);

      for i := 0 to db.AllPieces.Count - 1 do
      begin
        desc := db.PieceDesc(db.AllPieces.Objects[i] as TPieceInfo);
        if Pos(upcs, strupper(desc)) > 0 then
          tmpsets.Add(db.AllPieces.Strings[i]);
      end;

      for i := 0 to db.AllPieces.Count - 1 do
      begin
        desc := db.AllPieces.Strings[i];
        if Pos(upcs, strupper(desc)) > 0 then
          tmpsets.Add(desc);
      end;
    end;

    tmpsets.Sort;

    for i := tmpsets.Count - 1 downto 1 do
      if tmpsets.strings[i] = tmpsets.strings[i - 1] then
        tmpsets.Delete(i);

    aa := 0;
    document.StartNavigateSection;

    for i := 0 to tmpsets.Count - 1 do
    begin
      if willmultipage or (tmpsets.Count > 500) then
      begin
        inc(aa);
        document.StartItemId(aa);
      end;
      DrawHeadLine(Format('<a href="spiece/%s">%s - %s</a>',
        [tmpsets.Strings[i], tmpsets.Strings[i], db.PieceDesc(tmpsets.Strings[i])]));
    end;
    tmpsets.Free;
    document.EndNavigateSection;
    document.MarkBottomNavigateSection;
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  document.write('<body background="splash.jpg">');
  document.title(pcs + ' - ' + db.PieceDesc(pcs));
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  if idx > 0 then
    prevP := db.AllPieces.Strings[idx - 1]
  else
    prevP := '';

  if idx < db.AllPieces.Count - 1 then
    nextP := db.AllPieces.Strings[idx + 1]
  else
    nextP := '';

  pi := db.AllPieces.Objects[idx] as TPieceInfo;

  refrhtml := ' <a href=refreshpiece/' + pcs + '><img src="images\refresh.png"></a>';

  if prevP <> '' then
    prevP := '<a href=spiece/' + prevP + '>(' + prevP + ') Prev</a>';

  if nextP <> '' then
  begin
    nextP := '<a href=spiece/' + nextP + '>Next (' + nextP + ')</a>';
    if prevP <> '' then
      nextP := ' - ' + nextP;
  end;

  refrhtml := refrhtml + '<br><br>' + prevP + nextP + '<br><br>';

  if pi.category = 0 then
    refrcathtml := ' <a href=refreshpiececat/' + pcs + '><img src="images\refresh.png"></a>'
  else
    refrcathtml := '';

  cathtml := '<a href="catcolors/' + IntToStr(pi.category) + '">' + '<b>' + db.categories[pi.category].name + '</a> ' +
             '<a href="catcolors/' + IntToStr(pi.category) + '"><img src="images\colors.png"></b></a> ' +
             '<a href="ShowCatalogList//-1/' + IntToStr(pi.category) + '"><img src="images\catalog.png"></b></a>' +
             refrcathtml;
{  if pi.weight > 0.0 then
    DrawHeadLine(pcs + ' - ' + db.PieceDesc(pcs) + ' (' + Format('%2.2f gr', [pi.weight]) + ')' + '<a href=editpiece/' + pcs + '/' + itoa(GetAPieceColor(pcs)) + '><img src="images\edit.png"></a>' + refrhtml + '<br>' + cathtml)
  else
    DrawHeadLine(pcs + ' - ' + db.PieceDesc(pcs) + '<a href=editpiece/' + pcs + '/' + itoa(GetAPieceColor(pcs)) + '><img src="images\edit.png"></a>' + refrhtml + '<br>' + cathtml);}
  if pi.weight > 0.0 then
    DrawHeadLine(pcs + ' - ' + db.PieceDesc(pi) + ' (' + Format('%2.2f gr', [pi.weight]) + ')' +
      ' <a href=editmold/' + pcs + '><img src="images\edit.png"></a>' +
      ' <a href=refreshpieceorgearfrombricklink/' + Trim(pcs) + '><img src="images\refreshcolors.png"></a>' + refrhtml + '<br>' + cathtml)
  else
    DrawHeadLine(pcs + ' - ' + db.PieceDesc(pi) +
      ' <a href=editmold/' + pcs + '><img src="images\edit.png"></a>' +
      ' <a href=refreshpieceorgearfrombricklink/' + Trim(pcs) + '><img src="images\refreshcolors.png"></a>' + refrhtml + '<br>' + cathtml);

  storages := db.StorageBinsForMold(pcs);
  if storages.Count > 0 then
  begin
    stortxt := '<b>Storage Bins:</b><br>';
    for i := 0 to storages.Count - 1 do
      stortxt := stortxt + '<a href=storage/' + storages.Strings[i] + '>' + storages.Strings[i] + '</a><br>';
    DrawHeadLine(stortxt);
  end;
  storages.Free;


  pyears := TDNumberList.Create;
  for i := -1 to MAXINFOCOLOR do
    if (i = -1) or (db.Colors(i).id = i) then
      if db.Colors(i).knownpieces <> nil then
      begin
        idx := db.Colors(i).knownpieces.IndexOfUCS(pcs);
        if idx >= 0 then
        begin
          pci := db.PieceColorInfo(pcs, i, db.Colors(i).knownpieces.Objects[idx]);

          if pyears.IndexOf(pci.year) < 0 then
            pyears.Add(pci.year);
        end;
      end;

  pyearsstr := '';

  if pyears.Count > 0 then
  begin
    pyears.Sort;
    pyearsstr := 'Years<br><br>';
    if year >= 0 then
      pyearsstr := pyearsstr + '<a href=spiece/' + pcs + '>All</a>'
    else
      pyearsstr := pyearsstr + 'All';
    if db.HasSetColorsOnly(pcs) then
      ypdateyearlink := '<a href="UpdateSetYearFromNet/' + pcs + '"><img src="images\refresh.png"></a>'
    else
      ypdateyearlink := '<a href="UpdateMoldYearsFromNet/' + pcs + '"><img src="images\refresh.png"></a>';
    for i := 0 to pyears.Count - 1 do
    begin
      pyearsstr := pyearsstr + ' - ';
      if year = pyears.Numbers[i] then
        pyearsstr := pyearsstr + decide(pyears.Numbers[i] = 0, '(Unknown) ' + ypdateyearlink, itoa(pyears.Numbers[i]))
      else
        pyearsstr := pyearsstr + '<a href=spiece/' + pcs + '/' + itoa(pyears.Numbers[i]) + '>' + decide(pyears.Numbers[i] = 0, '(Unknown)', itoa(pyears.Numbers[i])) + '</a>';
    end;
  end;

  pyears.Free;

  pfamilystr := '';

  if not db.HasSetColorsOnly(pcs) then
  begin
    cmolds := db.FamilyMolds(pcs);
    if cmolds <> nil then
      if cmolds.Count > 1 then
        pfamilystr := MakeThumbnailImage2(pcs) + '<br><a href=ShowFamilyMolds/' + pcs + '>Variations and other prints...</a>';
  end;

  srelationships := '';
  if pi.moldvariations.Count > 0 then
    srelationships := srelationships + '<tr bgcolor=' + TBGCOLOR + '><td><a href=ShowMoldVariations/' + pcs + '>Mold Variations</a></td></tr>';
  if pi.alternates.Count > 0 then
    srelationships := srelationships + '<tr bgcolor=' + TBGCOLOR + '><td><a href=ShowPieceAlternates/' + pcs + '>Alternates</a></td></tr>';
  if pi.patternof <> '' then
    srelationships := srelationships + '<tr bgcolor=' + TBGCOLOR + '><td>Pattern of <a href=spiece/' + pi.patternof + '>' + pi.patternof + '</a></td></tr>';
  if pi.printof <> '' then
    srelationships := srelationships + '<tr bgcolor=' + TBGCOLOR + '><td>Print of <a href=spiece/' + pi.printof + '>' + pi.printof + '</a></td></tr>';
  if pi.patterns.Count > 0 then
    srelationships := srelationships + '<tr bgcolor=' + TBGCOLOR + '><td><a href=ShowPiecePatterns/' + pcs + '>Patterns</a></td></tr>';
  if pi.prints.Count > 0 then
    srelationships := srelationships + '<tr bgcolor=' + TBGCOLOR + '><td><a href=ShowPiecePrints/' + pcs + '>Prints</a></td></tr>';

  if srelationships = '' then
  begin
    if pyearsstr + pfamilystr <> '' then
    begin
      if (pyearsstr = '') or (pfamilystr = '') then
        DrawHeadLine(pyearsstr + pfamilystr)
      else
        DrawHeadLine2(pyearsstr, pfamilystr);
    end;
  end
  else
  begin
    srelationships :=
      '<p valign=top><table width=99% bgcolor=' + TBGCOLOR + ' border=1>' +
      '<tr bgcolor=' + THBGCOLOR + '>' +
      '<th><b>Relationships</b></th>' + '</tr>' +
      srelationships +
      '</table></p>';
    rlst := TStringList.Create;
    try
      if pyearsstr <> '' then
      begin
        ts := TString.Create;
        ts.text := pyearsstr;
        rlst.AddObject('', ts);
      end;
      if pfamilystr <> '' then
      begin
        ts := TString.Create;
        ts.text := pfamilystr;
        rlst.AddObject('', ts);
      end;
      ts := TString.Create;
      ts.text := srelationships;
      rlst.AddObject('', ts);
      DrawHeadLineN(rlst);
    finally
      FreeList(rlst);
    end;
  end;

  didmyinventoryheader := False;
  aa := 0;

  for i := -1 to MAXINFOCOLOR do
    if (i = -1) or (db.Colors(i).id = i) then
      if db.Colors(i).knownpieces <> nil then
      begin
        idx := db.Colors(i).knownpieces.IndexOfUCS(pcs);
        if idx >= 0 then
        begin
          pci := db.PieceColorInfo(pcs, i, db.Colors(i).knownpieces.Objects[idx]);

          if (year < 0) or (pci.year = year) then
          begin
            inventory.StorePieceInventoryStatsRec(basedefault + 'cache\' + IntToStr(decide(i = -1, 9999, i)) + '\' + pcs + '.history', pcs, i);

            numpieces := inventory.LoosePartCount(pcs, i);
            if numpieces > 0 then
            begin
              if not didmyinventoryheader then
              begin
                document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
                document.write('<tr bgcolor=' + THBGCOLOR + '>');
                document.write('<th>Color</th>');
                document.write('<th>Num Pieces</th>');
                document.write('<th>Color</th>');
                document.write('<th>Num Pieces</th>');
                document.write('<th>Color</th>');
                document.write('<th>Num Pieces</th>');
                document.write('<th>Color</th>');
                document.write('<th>Num Pieces</th>');
                document.write('<th>Color</th>');
                document.write('<th>Num Pieces</th>');
                document.write('</tr>');
                didmyinventoryheader := True;
              end;

              inc(aa);
              si := IntToStr(i);
              document.write('<td width=10%><img width=100px src=' + si + '\' + pcs + '.png>');
              DrawColorCell(i, 25);
  //            document.BlancColorCell(db.colors(i).RGB, 25);
              cinfo := db.colors(i);
              document.write('<a href=spiecec/' + pcs + '/' + si + '>' + cinfo.name +
                ' (' + si + ') (BL=' + IntToStr(cinfo.BrickLingColor) + ')' + GetRebrickableColorHtml(i) +
                  '<img src="images\details.png"></a>' +
                  HtmlDrawInvImgLink(pcs, i, pi) + '</td>');
              document.write('<td width=10% align=right>' + Format('%d', [numpieces]) +
                '<br><a href=editpiece/' + pcs + '/' + si + '><img src="images\edit.png"></a>' +
                '<br><a href=diagrampiece/' + pcs + '/' + si + '><img src="images\diagram.png"></a>' +
                '</td>');
              if aa mod 5 = 0 then
                document.write('</tr>');
            end;
          end;
        end;
      end;

  if didmyinventoryheader then
    document.write('</table><br><br>')
  else
    document.write('<br><br>');

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Image</b></th>');
  document.write('<th>Color</th>');
  document.write('<th>Demand</th>');
  document.write('<th>Num Pieces</th>');
  document.write('<th>N</th>');
  document.write('<th>U</th>');
  document.write('<th>Cost</th>');
  document.write('</tr>');

  aa := 0;
  prnt := 0.0;
  prut := 0.0;
  totpieces := 0;
  mycosttot := 0.0;

  for i := -1 to MAXINFOCOLOR do
    if (i = -1) or (db.Colors(i).id = i) then
      if db.Colors(i).knownpieces <> nil then
      begin
        idx := db.Colors(i).knownpieces.IndexOfUCS(pcs);
        if idx >= 0 then
        begin
          pci := db.PieceColorInfo(pcs, i, db.Colors(i).knownpieces.Objects[idx]);

          if (year < 0) or (pci.year = year) then
          begin
            inc(aa);
            numpieces := inventory.LoosePartCount(pcs, i);
            totpieces := totpieces + numpieces;
            si := IntToStr(i);
            document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');
            document.write('<td width=35%><img width=100px src=' + si + '\' + pcs + '.png></td>');
            document.write('<td width=20%>');
            DrawColorCell(i, 25);
            // document.BlancColorCell(db.colors(i).RGB, 25);
            cinfo := db.colors(i);
            if pci <> nil then
            begin
              document.write('<a href=spiecec/' + pcs + '/' + si + '>' + cinfo.name +
                ' (' + si + ') (BL=' + IntToStr(cinfo.BrickLingColor) + ')' + GetRebrickableColorHtml(i) +
                '<img src="images\details.png"></a>' +
                HtmlDrawInvImgLink(pcs, i, pi) +
                decide(pci.setmost = '', '', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + lugbulklinks(pci) + '</td>');
              document.write('<td width=10% align=right>');
              document.write('N=%2.3f<br>U=%2.3f</td>', [pci.nDemand, pci.uDemand]);
            end
            else
            begin
              document.write('<a href=spiecec/' + pcs + '/' + si + '>' + cinfo.name +
                ' (' + si + ') (BL=' + IntToStr(cinfo.BrickLingColor) + ')' + GetRebrickableColorHtml(i) + '<img src="images\details.png"></a>' +
                HtmlDrawInvImgLink(pcs, i, pi) + '</td>');
              document.write('<td width=10% align=right></td>');
            end;
            document.write('<td width=15% align=right>' + Format('%d', [numpieces]) +
              '<br><a href=editpiece/' + pcs + '/' + si + '><img src="images\edit.png"></a>' +
              '<br><a href=diagrampiece/' + pcs + '/' + si + '><img src="images\diagram.png"></a>' +
              '</td>');

            if pci <> nil then
            begin
              www := db.GetItemWeight(pci.piece, pci.color);
              prn := pci.EvaluatePriceNew;
              pru := pci.EvaluatePriceUsed;
              document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Kgr', [prn, dbl_safe_div(prn, www) * 1000]) + '</td>');
              document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Kgr', [pru, dbl_safe_div(pru, www) * 1000]) + '</td>');
              prnt := prnt + prn * numpieces;
              prut := prut + pru * numpieces;

            end
            else
            begin
              document.write('<td width=15% align=right>-</td>');
              document.write('<td width=15% align=right>-</td>');
            end;
            mycost := GetItemCostDbl(pcs, i);
            document.write('<td width=10% align=right>' + Format('€ %2.4f<br>€ %2.4f', [mycost, mycost * numpieces]) + '</td>');
            mycosttot := mycosttot + mycost * numpieces;
            document.write('</tr>');

            bp.part := pcs;
            bp.color := i;
            bp.num := numpieces;

            DrawBrickOrderInfo(@bp);
          end;
        end;
      end;

  document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right><b>Total</b></td>');
  document.write('<td width=35%><b> </b></td>');
  document.write('<td width=20%><b> </b></td>');
  document.write('<td width=20%><b> </b></td>');
  document.write('<td width=10% align=right><b>' + IntToStr(totpieces) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [prnt]) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [prut]) + '</b></td>');
  document.write('<td width=10% align=right><b> </b></td>');

  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;

end;

procedure TMainForm.DrawMoldList(const tit: string; const lst: TStringList; const splitcolorflags: boolean;
  const donumlinks: boolean; const doctit: string = '');
var
  i: integer;
  pcs: string;
  aa: integer;
  ncolors: TDNumberList;
begin
  UpdateDismantaledsetsinv;
  ShowSplash;
  SplashProgress('Working...', 0);

  if domultipagedocuments then
    document.NewMultiPageDocument('DrawMoldList' + tit, lst.Text);

  document.write('<body background="splash.jpg">');
  if doctit = '' then
    document.title('Molds')
  else
    document.title(doctit);
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine(tit);

  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Mold</b></th>');
  document.write('<th>Num Colors</th>');
  document.write('<th>Colors</th>');
  document.write('</tr>');

  aa := 0;

  for i := 0 to lst.Count - 1 do
  begin
    pcs := lst.Strings[i];
    ncolors := db.GetMoldKnownColors(pcs);
    if ncolors.Count = 0 then
    begin
      ncolors.Free;
      Continue;
    end;

    inc(aa);
    document.StartItemId(aa);
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');

    document.write('<td width=25%>');
    document.write(MakeThumbnailImage2(pcs) + '<b><a href=spiece/' + pcs + '>' + pcs + '</a></b>');
    document.write(' - ' + db.PieceDesc(pcs) + '</td>');

    if donumlinks then
      document.write('<td width=10%><p align=center>' +
        '<a href=ShowMoldsWithNumColors/' + itoa(ncolors.Count) + '>' + itoa(ncolors.Count) + '</a>' +
        '</p></td>')
    else
      document.write('<td width=10%><p align=center>' + itoa(ncolors.Count) + '</p></td>');
    document.write('<td width=65%>');
    if splitcolorflags then
      DrawColorCells2(ncolors, 25)
    else
      DrawColorCells(ncolors, 25);
    ncolors.Free;

    document.write('</td></tr>');

    SplashProgress('Working...', i / lst.Count);
  end;

  document.EndNavigateSection;

  document.write('</table>');

  document.MarkBottomNavigateSection;

  document.write('<br></p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;
end;

type
  TCatalogInfoClass = class(TObject)
    N: TDNumberList;
    S: TStringList;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

constructor TCatalogInfoClass.Create;
begin
  N := TDNumberList.Create;
  S := TStringList.Create;
  Inherited;
end;

destructor TCatalogInfoClass.Destroy;
begin
  N.Free;
  S.Free;
  Inherited;
end;

procedure TMainForm.DrawMoldListCatalog(const tit: string; const lst: TStringList;
  const year: integer; const catid: integer; const typ: string);
var
  i, j, y: integer;
  pcs: string;
  aa: integer;
  years: TDNumberList;
  pcat: integer;
  pi: TPieceInfo;
  syears: string;
  syear: string;
  sz: integer;
  sinv: TBrickInventory;
begin
  UpdateDismantaledsetsinv;
  ShowSplash;
  SplashProgress('Working...', 0);

  if domultipagedocuments then
    document.NewMultiPageDocument('DrawMoldListCatalog' + tit, lst.Text);

  document.write('<body background="splash.jpg">');
  document.title('Catalog Molds');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine(tit);

  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Mold</b></th>');
  document.write('<th>Num Colors/Assets</th>');
  document.write('<th>Colors/Assets</th>');
  if catid = CATEGORYLUGBULK then
  begin
    document.write('<th>Num Pieces</th>');
    document.write('<th>Weight (Kg)</th>');
  end;
  if catid < 0 then
    document.write('<th>Category</th>');
  if year < 0 then
    document.write('<th>Years</th>');
  document.write('</tr>');

  aa := 0;

  years := TDNumberList.Create;

  sz := document.size;
  for i := 0 to lst.Count - 1 do
  begin
    inc(aa);
    document.StartItemId(aa);
    pcs := lst.Strings[i];
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');

    document.write('<td width=25%>');
    document.write(MakeThumbnailImage(pcs, (lst.Objects[i] as TCatalogInfoClass).N.Numbers[0]) + '<b><a href=spiece/' + pcs + '/' + itoa(year) + '>' + pcs + '</a></b>');
    document.write(' - ' + db.PieceDesc(pcs) + '</td>');

    document.write('<td width=10%><p align=center>' + itoa((lst.Objects[i] as TCatalogInfoClass).N.Count) + '</p></td>');
    document.write('<td width=45%>');
    DrawColorCells((lst.Objects[i] as TCatalogInfoClass).N, 25);
    document.write('</td>');

    if catid = CATEGORYLUGBULK then
    begin
      sinv := db.GetSetInventory(pcs);
      if sinv <> nil then
      begin
        document.write('<td width=10% align=right><a href=sinv/' + pcs + '>' + Format('%d', [sinv.totallooseparts]) + '</a></td>');
        document.write('<td width=10% align=right>' + Format('%2.3f Kgr', [sinv.LoosePartsWeight / 1000]) + '</td>');
      end
      else
      begin
        document.write('<td width=10%>-</td>');
        document.write('<td width=10%>-</td>');
      end;
    end;

    if catid < 0 then
    begin
      document.write('<td width=20%>');
      pi := db.PieceInfo(pcs);
      pcat := pi.category;
      if (pcat >= 0) and (pcat < MAXCATEGORIES) then
        document.write('<a href=ShowCatalogList/' + typ + '/' + itoa(year) + '/' +
                       itoa(pcat) + '>' + db.categories[pcat].name + '</a>')
      else
        document.write('-');
      document.write('</td>');
    end;
    if year < 0 then
    begin
      document.write('<td width=20%>');

      years.Clear;
      for j := 0 to (lst.Objects[i] as TCatalogInfoClass).S.Count - 1 do
      begin
        y := ((lst.Objects[i] as TCatalogInfoClass).S.Objects[j] as TPieceColorInfo).year;
        if y > 0 then
          if years.IndexOf(y) < 0 then
            years.Add(y);
      end;

      syears := '';
      years.Sort;
      for j := 0 to years.Count - 1 do
      begin
        if syears <> '' then
          syears := syears + ' - ';
        syear := itoa(years.Numbers[j]);
        syears := syears + '<a href=ShowCatalogList/' + typ + '/' + syear + '/' + itoa(catid) + '>' + syear + '</a>';
      end;
      if syears = '' then
        document.write('-')
      else
        document.write(syears);
      document.write('</td>');
    end;

    document.write('</td></tr>');

    if i = 0 then
      if not domultipagedocuments then
      begin
        sz := document.size - sz;
        document.PredictAdditionalSize(sz * lst.Count + 1024);
      end;

    if lst.Count < 1000 then
      SplashProgress('Working...', i / lst.Count)
    else if lst.Count < 3000 then
    begin
      if i mod 2 = 0 then
        SplashProgress('Working...', i / lst.Count)
    end
    else if i mod 4 = 0 then
      SplashProgress('Working...', i / lst.Count)
  end;

  years.Free;

  document.EndNavigateSection;

  document.write('</table>');

  document.MarkBottomNavigateSection;

  document.write('<br></p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

end;

function sortpiecelist_htmlstrip(List: TStringList; Index1, Index2: Integer): Integer;
var
  p1, p2: string;
begin
  p1 := htmlstripstring(List.Strings[Index1]);
  p2 := htmlstripstring(List.Strings[Index2]);
  Result := AnsiCompareStr(p1, p2);
end;

procedure TMainForm.ShowCatalogList(const ltyp: string; const year: integer;
  const catid1: integer; const doall: boolean);
var
  pilst: TStringList;
  pilsts: array[0..127] of TStringList;
  i, j, k, l, idx, idx2: integer;
  pi: TPieceInfo;
  pci: TPieceColorInfo;
  kp: THashStringList;
  tit: string;
  tit_typ: string;
  cranges: TDNumberList;
  okmoc: boolean;
  okpartinv: boolean;
  okboxinstructions: boolean;
  cyears: TStringList;
  ccategories: TStringList;
  catid: integer;
  ccatid: integer;
  cyearid: integer;
  sdoc: TStringList;
  hasunknowncategory: boolean;
  typ: string;
  fsavename: string;
  tot: integer;
  totstr: string;
begin
  Screen.Cursor := crHourGlass;

  if (catid1 >= MAXCATEGORIES) or (catid1 < -1) then
    catid := -1
  else
    catid := catid1;

  typ := ltyp;
  cranges := TDNumberList.Create;
  if typ = 'P' then
  begin
    tit_typ := '<a href="catalogparts">Parts</a>';
    cranges.AddRange(-1, LASTNORMALCOLORINDEX);
    cranges.Add(MAXINFOCOLOR);
  end
  else if typ = 'PI' then
  begin
    tit_typ := '<a href="catalogpartsinv">Parts with inventory</a>';
    cranges.AddRange(-1, LASTNORMALCOLORINDEX);
    cranges.Add(MAXINFOCOLOR);
  end
  else if typ = 'S' then
  begin
    tit_typ := '<a href="catalogsets">Sets</a>';
    cranges.Add(-1);
    cranges.Add(INSTRUCTIONCOLORINDEX);
    cranges.Add(BOXCOLORINDEX);
  end
  else if typ = 'SNI' then
  begin
    tit_typ := '<a href="catalogsetsnoinv">Sets without inventory</a>';
    cranges.AddRange(-1, LASTNORMALCOLORINDEX);
    cranges.Add(MAXINFOCOLOR);
  end
  else if typ = 'I' then
  begin
    tit_typ := '<a href="cataloginstructions">Instructions</a>';
    cranges.Add(INSTRUCTIONCOLORINDEX);
  end
  else if typ = 'O' then
  begin
    tit_typ := '<a href="catalogboxes">Original Boxes</a>';
    cranges.Add(BOXCOLORINDEX);
  end
  else if typ = 'MC' then
  begin
    tit_typ := '<a href="catalogmocs">Mocs</a>';
    cranges.Add(-1);
  end
  else if typ = 'G' then
  begin
    tit_typ := '<a href="cataloggears">Gears</a>';
    cranges.AddRange(-1, MAXINFOCOLOR);
  end
  else if typ = 'C' then
  begin
    tit_typ := '<a href="catalogcatalogs">Catalogs</a>';
    cranges.Add(CATALOGCOLORINDEX);
  end
  else if typ = 'M' then
  begin
    tit_typ := '<a href="catalogminifigures">Minifigures</a>';
    cranges.Add(-1);
    cranges.Add(MAXINFOCOLOR);
  end
  else if typ = 'B' then
  begin
    tit_typ := '<a href="catalogbooks">Books</a>';
    cranges.Add(-1);
    cranges.Add(MAXINFOCOLOR);
  end
  else
  begin
    typ := '';
    tit_typ := '';
    cranges.AddRange(-1, MAXINFOCOLOR);
  end;

  for i := 0 to 127 do
  begin
    pilsts[i] := TStringList.Create;
    pilsts[i].Sorted := True;
  end;

  cyears := TStringList.Create;
  cyears.Sorted := True;
  ccategories := TStringList.Create;
  ccategories.Sorted := True;

  hasunknowncategory := False;
  tot := 0;

  for k := 0 to cranges.Count - 1 do
  begin
    i := cranges.Numbers[k];
    if (db.Colors(i).id = i) or (i = -1) then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
      begin
        if (i = INSTRUCTIONCOLORINDEX) or (i = BOXCOLORINDEX) then
          okboxinstructions := (typ = 'S') or (typ = 'G') or (typ = 'M') or (typ = 'B')
        else
          okboxinstructions := False;
        for j := 0 to kp.Count - 1 do
        begin
          pci := kp.Objects[j] as TPieceColorInfo;

          if typ = 'MC' then
            okmoc := db.IsMoc(pci.piece)
          else
            okmoc := False;

          if typ = 'PI' then
            okpartinv := (pci.ItemType = 'P') and db.PieceInfo(pci).hasinventory
          else if typ = 'SNI' then
            okpartinv := (pci.ItemType = 'S') and (db.AllSets.IndexOfUCS(pci.piece) < 0)
          else
            okpartinv := False;

          if okpartinv or okmoc or okboxinstructions or (pci.ItemType = typ) or (typ = '') then
            if (year < 0) or (pci.year = year) then
            begin
              pi := db.PieceInfo(pci);

              if ((catid < 0) or (pi.category = catid)) and (pci.piece <> '') then
              begin
                hasunknowncategory := hasunknowncategory or (pi.category = 0);
                l := Ord(pci.piece[1]) div 2;
                idx := pilsts[l].IndexOf(pci.piece);
                if idx < 0 then
                  if not okboxinstructions then
                  begin
                    idx := pilsts[l].AddObject(pci.piece, TCatalogInfoClass.Create);
                    idx2 := ccategories.IndexOf(itoa(pi.category));
                    if idx2 < 0 then
                      idx2 := ccategories.AddObject(itoa(pi.category), TCounterObject.Create);
                    (ccategories.Objects[idx2] as TCounterObject).IncL;
                    inc(tot);
                  end;
                if idx >= 0 then
                begin
                  idx2 := cyears.IndexOf(itoa(pci.year));
                  if idx2 < 0 then
                  begin
                    idx2 := cyears.AddObject(itoa(pci.year), TStringList.Create);
                    (cyears.Objects[idx2] as TStringList).Sorted := True;
                  end;
                  if (cyears.Objects[idx2] as TStringList).IndexOf(pci.piece) < 0 then
                    (cyears.Objects[idx2] as TStringList).Add(pci.piece);
                  (pilsts[l].Objects[idx] as TCatalogInfoClass).N.Add(i);
                  (pilsts[l].Objects[idx] as TCatalogInfoClass).S.AddObject(itoa(i), pci);
                end;
              end;
            end;
        end;
      end;
    end;
  end;

  cranges.Free;

  pilst := TStringList.Create;
  for i := 0 to 127 do
    pilst.AddStrings(pilsts[i]);
  pilst.Sorted := True;
  for i := 0 to 127 do
    pilsts[i].Free;

  tit := '<a href="cataloghome">Catalog</a>';
  if (year >= 0) or (catid >= 0) or doall then
  begin
    if tit_typ <> '' then
      tit := tit + ' - ' + tit_typ;
    if (year >= 0) and (catid < 0) then
      tit := tit + ' - [Year: ' + itoa(year) + ']'
    else if (year < 0) and (catid >= 0) then
      tit := tit + ' - [Category: ' + db.categories[catid].name + ']'
    else if (year >=0) and (catid >= 0) then
      tit := tit +
        ' - [Category: <a href=ShowCatalogList/' + typ + '/-1/' + itoa(catid) + '>' + db.categories[catid].name + '</a>]' +
        ' - [Year: <a href=ShowCatalogList/' + typ + '/' + itoa(year) + '/-1>' + itoa(year) + '</a>]';
  end;

  if (year < 0) and (catid < 0) and not doall then
  begin
    document.write('<body background="splash.jpg">');
    document.title('Catalog');
    DrawNavigateBar;
    document.write('<div style="color:' + DFGCOLOR + '">');
    document.write('<p align=center>');

    totstr := ' (<a href=ShowCatalogListAll/' + typ + '/' + itoa(year) + '/' + itoa(catid) + '>' + itoa(tot) + ' items</a>)';
    if tit_typ = '' then
      DrawHeadLine(tit + ' - All' + totstr)
    else
      DrawHeadLine(tit + ' - ' + htmlstripstring(tit_typ) + totstr);

    document.StartNavigateSection;

    document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
    document.write('<tr bgcolor=' + THBGCOLOR + '>');
    document.write('<th><b>Categories</b></th>');
    document.write('<th>Years</th>');
    document.write('</tr>');

    document.write('<tr valign=top>');

    document.write('<td width=50%>');

    sdoc := TStringList.Create;
    try
      for i := 0 to ccategories.Count - 1 do
      begin
        ccatid := atoi(ccategories.Strings[i]);
          if ccatid < MAXCATEGORIES then
            if ccatid > 0 then
              sdoc.Add('<a href=ShowCatalogList/' + typ + '/-1/' + itoa(ccatid) + '>' + db.categories[ccatid].name + '</a> (' +
                itoa((ccategories.Objects[i] as TCounterObject).value) + ')<br>');
      end;
      sdoc.CustomSort(sortpiecelist_htmlstrip);
      if hasunknowncategory then
      begin
        sdoc.Insert(0, '<a href=ShowCatalogList/' + typ + '/-1/0>(Unknown)</a> (' +
                  itoa((ccategories.Objects[0] as TCounterObject).value) + ')<br>');
      end;
      document.write(sdoc.Text);
    finally
      sdoc.Free;
    end;
    document.write('</td>');

    document.write('<td width=50%>');

    sdoc := TStringList.Create;
    try
      for i := 0 to cyears.Count - 1 do
      begin
        cyearid := atoi(cyears.Strings[i]);
        if cyearid >= 0 then
          sdoc.Add('<a href=ShowCatalogList/' + typ + '/' + itoa(cyearid) + '/-1>' + decide(cyearid = 0, '(Unknown)', itoa(cyearid)) + '</a> (' +
            itoa((cyears.Objects[i] as TStringList).Count) + ')<br>');
      end;
      sdoc.Sort;
      document.write(sdoc.Text);
    finally
      sdoc.Free;
    end;
    document.write('</td>');

    document.write('</table></p></div></body>');
    document.SaveBufferToFile(diskmirror);

    document.Flash;
  end
  else
  begin
    fsavename := basedefault + 'out\catalog\';
    if not DirectoryExists(fsavename) then
      MkDir(fsavename);
    fsavename := fsavename + ChangeFileExt(ExtractFileName(diskmirror), '.txt');
    S_SaveToFile(pilst, fsavename);
    DrawMoldListCatalog(tit, pilst, year, catid, typ);
  end;

  FreeList(ccategories);
  FreeList(cyears);
  FreeList(pilst);
  Screen.Cursor := crDefault;
end;

function sortpiecelist_price_new(List: TStringList; Index1, Index2: Integer): Integer;
var
  p1, p2: string;
  c1, c2: string;
  pci1, pci2: TPieceColorInfo;
  price1, price2: integer;
begin
  splitstring(List.Strings[Index1], p1, c1, ',');
  splitstring(List.Strings[Index2], p2, c2, ',');
  pci1 := db.PieceColorInfo(p1, atoi(c1), List.Objects[Index1]);
  pci2 := db.PieceColorInfo(p2, atoi(c2), List.Objects[Index2]);
  if pci1 <> nil then
    price1 := round(pci1.EvaluatePriceNew * 10000)
  else
    price1 := 0;
  if pci2 <> nil then
    price2 := round(pci2.EvaluatePriceNew * 10000)
  else
    price2 := 0;
  Result := price1 - price2;
end;

function sortpiecelist_price_used(List: TStringList; Index1, Index2: Integer): Integer;
var
  p1, p2: string;
  c1, c2: string;
  pci1, pci2: TPieceColorInfo;
  price1, price2: integer;
begin
  splitstring(List.Strings[Index1], p1, c1, ',');
  splitstring(List.Strings[Index2], p2, c2, ',');
  pci1 := db.PieceColorInfo(p1, atoi(c1), List.Objects[Index1]);
  pci2 := db.PieceColorInfo(p2, atoi(c2), List.Objects[Index2]);
  if pci1 <> nil then
    price1 := round(pci1.EvaluatePriceUsed * 10000)
  else
    price1 := 0;
  if pci2 <> nil then
    price2 := round(pci2.EvaluatePriceUsed * 10000)
  else
    price2 := 0;
  Result := price1 - price2;
end;

function sortpiecelist_date_update(List: TStringList; Index1, Index2: Integer): Integer;
var
  p1, p2: string;
  c1, c2: string;
  pci1, pci2: TPieceColorInfo;
  date1, date2: integer;
begin
  splitstring(List.Strings[Index1], p1, c1, ',');
  splitstring(List.Strings[Index2], p2, c2, ',');
  pci1 := db.PieceColorInfo(p1, atoi(c1), List.Objects[Index1]);
  pci2 := db.PieceColorInfo(p2, atoi(c2), List.Objects[Index2]);
  if pci1 <> nil then
    date1 := round(pci1.date * 10000)
  else
    date1 := 0;
  if pci2 <> nil then
    date2 := round(pci2.date * 10000)
  else
    date2 := 0;
  Result := date2 - date1;
end;

function sortpiecelist_TDouble(List: TStringList; Index1, Index2: Integer): Integer;
var
  d1, d2: TDouble;
  v1, v2: integer;
begin
  d1 := List.Objects[Index1] as TDouble;
  d2 := List.Objects[Index2] as TDouble;
  if d1 <> nil then
    v1 := round(d1.value * 10000)
  else
    v1 := 0;
  if d2 <> nil then
    v2 := round(d2.value * 10000)
  else
    v2 := 0;
  Result := v2 - v1;
end;

function sortpiecelist_TCInteger(List: TStringList; Index1, Index2: Integer): Integer;
var
  c1, c2: TCInteger;
  v1, v2: integer;
begin
  c1 := List.Objects[Index1] as TCInteger;
  c2 := List.Objects[Index2] as TCInteger;
  if c1 <> nil then
    v1 := c1.value
  else
    v1 := 0;
  if c2 <> nil then
    v2 := c2.value
  else
    v2 := 0;
  Result := v2 - v1;
end;

procedure TMainForm.DrawPieceList(const tit: string; const lst: TStringList; const sortorder: integer = 0;
  const extratit: string = ''; const doctit: string = '');
var
  i, cl: integer;
  col: string;
  pcs: string;
  aa: integer;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  prn, pru: double;
  prnt, prut: double;
  numpieces: integer;
  mycost: Double;
  bp: brickpool_t;
  totpieces: integer;
  mycosttot: double;
  dn: TDateTime;
  cinfo: colorinfo_p;
  www: double;
begin
  UpdateDismantaledsetsinv;
  ShowSplash;
  SplashProgress('Working...', 0);

  if domultipagedocuments then
    document.NewMultiPageDocument('DrawPieceList', tit + itoa(sortorder) + itoa(lst.count));

  document.write('<body background="splash.jpg">');
  if doctit = '' then
    document.title('Piece list')
  else
    document.title(doctit);
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine(tit);

  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Image</b></th>');
  document.write('<th>Color</th>');
  document.write('<th>Num Pieces</th>');
  if sortorder = SORT_DATE_UPDATE then
    document.write('<th>Days without update</th>')
  else if sortorder = SORT_ITEMS_CINTEGER then
    document.write('<th>' + extratit + '</th>');
  document.write('<th>N</th>');
  document.write('<th>U</th>');
  document.write('<th>Cost</th>');
  document.write('</tr>');

  aa := 0;
  prnt := 0.0;
  prut := 0.0;
  totpieces := 0;
  mycosttot := 0.0;
  dn := Now();

  case sortorder of
    SORT_PRICE_NEW: lst.CustomSort(sortpiecelist_price_new);
    SORT_PRICE_USED: lst.CustomSort(sortpiecelist_price_used);
    SORT_DATE_UPDATE: lst.CustomSort(sortpiecelist_date_update);
    SORT_ITEMS_CINTEGER: lst.CustomSort(sortpiecelist_TCInteger);
  end;

  for i := 0 to lst.Count - 1 do
  begin
    inc(aa);
    document.StartItemId(aa);
    splitstring(lst.Strings[i], pcs, col, ',');
    cl := atoi(col);
    numpieces := inventory.LoosePartCount(pcs, cl);
    if cl = -1 then
      inc(numpieces, inventory.BuildSetCount(pcs));
    totpieces := totpieces + numpieces;
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');

    pci := db.PieceColorInfo(pcs, cl, lst.Objects[i]);
    pi := db.PieceInfo(pci);

    document.write('<td width=35%>' + decide((cl = 9996) or (cl = 9997) or (cl = 9998), MakeThumbnailImage(pcs, cl), '<img src=' + col + '\' + pcs + '.png>'));
    document.write('<a href=spiece/' + pcs + '>' + pcs + '</a></b>');
    document.write(' - ' + db.PieceDesc(pi) + '</td>');
    document.write('<td width=20%>');
    DrawColorCell(cl, 25);
//    document.BlancColorCell(db.colors(cl).RGB, 25);

    cinfo := db.colors(cl);
    if pci = nil then
      document.write('<a href=spiecec/' + pcs + '/' + col + '>' + cinfo.name +
        ' (' + col + ') (BL=' + IntToStr(cinfo.BrickLingColor) + ')' + GetRebrickableColorHtml(cl) + '<img src="images\details.png"></a>' +
        HtmlDrawInvImgLink(pcs, cl, pi) + '</td>')
    else
      document.write('<a href=spiecec/' + pcs + '/' + col + '>' + cinfo.name +
        ' (' + col + ') (BL=' + IntToStr(cinfo.BrickLingColor) + ')' + GetRebrickableColorHtml(cl) + '<img src="images\details.png"></a>' +
        HtmlDrawInvImgLink(pcs, cl, pi) +
          decide(pci.setmost = '', '', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + lugbulklinks(pci) + '</td>');

    document.write('<td width=15% align=right>' + Format('%d', [numpieces]) +
            '<br><a href=editpiece/' + pcs + '/' + itoa(cl) + '><img src="images\edit.png"></a>' +
            '<br><a href=diagrampiece/' + pcs + '/' + itoa(cl) + '><img src="images\diagram.png"></a>' +
            '</td>');

    if pci <> nil then
    begin
      if sortorder = SORT_DATE_UPDATE then
        document.write('<td width=15% align=right>' + Format('%d', [DaysBetween(dn, pci.date)]) + '</td>')
      else if sortorder = SORT_ITEMS_CINTEGER then
        document.write('<td width=15% align=right>' + Format('%d', [(lst.Objects[i] as TCInteger).value]) + '</td>');
      prn := pci.EvaluatePriceNew;
      pru := pci.EvaluatePriceUsed;
      www := db.GetItemWeight(pci.piece, pci.color, pi);
      document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Kgr', [prn, dbl_safe_div(prn, www) * 1000]) + '</td>');
      document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Kgr', [pru, dbl_safe_div(pru, www) * 1000]) + '</td>');
      prnt := prnt + prn * numpieces;
      prut := prut + pru * numpieces;

    end
    else
    begin
      if (sortorder = SORT_DATE_UPDATE) or (sortorder = SORT_ITEMS_CINTEGER) then
        document.write('<td width=15% align=right>-</td>');
      document.write('<td width=15% align=right>-</td>');
      document.write('<td width=15% align=right>-</td>');
    end;
    mycost := GetItemCostDbl(pcs, cl);
    document.write('<td width=10% align=right>' + Format('€ %2.4f<br>€ %2.4f', [mycost, mycost * numpieces]) + '</td>');
    mycosttot := mycosttot + mycost * numpieces;
    document.write('</tr>');

    bp.part := pcs;
    bp.color := cl;
    bp.num := numpieces;

    if (sortorder = SORT_DATE_UPDATE) or (sortorder = SORT_ITEMS_CINTEGER) then
      DrawBrickOrderInfo(@bp, '', 3, 4)
    else
      DrawBrickOrderInfo(@bp);
    if (lst.Count < 250) or (i mod 5 = 0) then
      SplashProgress('Working...', i / lst.Count);
  end;
  document.EndNavigateSection;

  document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right><b>Total</b></td>');
  document.write('<td width=35%><b> </b></td>');
  document.write('<td width=20%><b> </b></td>');
  if sortorder = SORT_DATE_UPDATE then
    document.write('<td width=15%><b> </b></td>');
  document.write('<td width=10% align=right><b>' + IntToStr(totpieces) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [prnt]) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [prut]) + '</b></td>');
  document.write('<td width=10% align=right><b> </b></td>');

  document.write('<br></table>');

  document.MarkBottomNavigateSection;

  document.write('</p></div></body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

end;

procedure TMainForm.DrawPieceListSet(const tit: string; const settit: string; const lst: TStringList);
var
  i, cl: integer;
  col: string;
  pcs: string;
  aa: integer;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  prn, pru: double;
  prnt, prut: double;
  numpieces: integer;
  mycost: Double;
  bp: brickpool_t;
  totpieces: integer;
  mycosttot: double;
  cursetid: string;
  sinv: TBrickInventory;
  numpcsinset: integer;
  cinfo: colorinfo_p;
  www: double;
begin
  UpdateDismantaledsetsinv;
  ShowSplash;
  SplashProgress('Working...', 0);

  if domultipagedocuments then
    document.NewMultiPageDocument('DrawPieceListSet', tit + settit + itoa(lst.count));
  document.write('<body background="splash.jpg">');
  document.title(tit);
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine(tit);

  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Image</b></th>');
  document.write('<th>Color</th>');
  document.write('<th>Num Pieces</th>');
  document.write('<th>' + settit + '</th>');
  document.write('<th>N</th>');
  document.write('<th>U</th>');
  document.write('<th>Cost</th>');
  document.write('</tr>');

  aa := 0;
  prnt := 0.0;
  prut := 0.0;
  totpieces := 0;
  mycosttot := 0.0;

  for i := 0 to lst.Count - 1 do
  begin
    inc(aa);
    document.StartItemId(aa);
    splitstring(lst.Strings[i], pcs, col, cursetid, ',');
    cl := atoi(col);
    numpieces := inventory.LoosePartCount(pcs, cl);
    if cl = -1 then
      inc(numpieces, inventory.BuildSetCount(pcs));
    totpieces := totpieces + numpieces;
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');

    pci := db.PieceColorInfo(pcs, cl, lst.Objects[i]);
    pi := db.PieceInfo(pci);

    document.write('<td width=35%><img width=100px src=' + col + '\' + pcs + '.png>');
    document.write('<a href=spiece/' + pcs + '>' + pcs + '</a></b>');
    document.write(' - ' + db.PieceDesc(pi) + '</td>');
    document.write('<td width=20%>');
    DrawColorCell(cl, 25);
    // document.BlancColorCell(db.colors(cl).RGB, 25);

    cinfo := db.colors(cl);
    if pci = nil then
      document.write('<a href=spiecec/' + pcs + '/' + col + '>' + cinfo.name +
        ' (' + col + ') (BL=' + IntToStr(cinfo.BrickLingColor) + ')' + GetRebrickableColorHtml(cl) + '<img src="images\details.png"></a>' +
        HtmlDrawInvImgLink(pcs, cl, pi) + '</td>')
    else
      document.write('<a href=spiecec/' + pcs + '/' + col + '>' + cinfo.name +
        ' (' + col + ') (BL=' + IntToStr(cinfo.BrickLingColor) + ')' + GetRebrickableColorHtml(cl) + '<img src="images\details.png"></a>' +
        HtmlDrawInvImgLink(pcs, cl, pi) +
          decide(pci.setmost = '', '', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + lugbulklinks(pci) + '</td>');

    document.write('<td width=10% align=right>' + Format('%d', [numpieces]) +
            '<br><a href=editpiece/' + pcs + '/' + itoa(cl) + '><img src="images\edit.png"></a>' +
            '<br><a href=diagrampiece/' + pcs + '/' + itoa(cl) + '><img src="images\diagram.png"></a>' +
            '</td>');

    document.write('<td width=15%>');
//    document.write('<a href=sinv/' + cursetid +'><img width=56px src="s\' + cursetid + '.jpg"></a><br>');
    document.write('<a href=sinv/' + cursetid +'>' + MakeThumbNailImage2(cursetid, -1) + '</a><br>');

    document.write(' <a href=sinv/' + cursetid +'>' + cursetid + ' - ' + db.SetDesc(cursetid) + '</a>');

    numpcsinset := 0;
    sinv := db.GetSetInventoryWithOutExtra(cursetid);
    if sinv <> nil then
      numpcsinset := sinv.LoosePartCount(pcs, cl);
    if numpcsinset > 0 then
    begin
      if numpcsinset = 1 then
        document.write('<br>(1 time)')
      else
        document.write('<br>(' + itoa(numpcsinset) + ' times)');
    end;

    document.write('</td>');

    if pci <> nil then
    begin
      prn := pci.EvaluatePriceNew;
      pru := pci.EvaluatePriceUsed;
      www := db.GetItemWeight(pci.piece, pci.color, pi);
      document.write('<td width=12% align=right>' + Format('€ %2.4f<br>€ %2.2f / Kgr', [prn, dbl_safe_div(prn, www) * 1000]) + '</td>');
      document.write('<td width=12% align=right>' + Format('€ %2.4f<br>€ %2.2f / Kgr', [pru, dbl_safe_div(pru, www) * 1000]) + '</td>');
      prnt := prnt + prn * numpieces;
      prut := prut + pru * numpieces;
    end
    else
    begin
      document.write('<td width=12% align=right>-</td>');
      document.write('<td width=12% align=right>-</td>');
    end;
    mycost := GetItemCostDbl(pcs, cl);
    document.write('<td width=10% align=right>' + Format('€ %2.4f<br>€ %2.4f', [mycost, mycost * numpieces]) + '</td>');
    mycosttot := mycosttot + mycost * numpieces;
    document.write('</tr>');

    bp.part := pcs;
    bp.color := cl;
    bp.num := numpieces;

    DrawBrickOrderInfo(@bp);
    SplashProgress('Working...', i / lst.Count);
  end;

  document.EndNavigateSection;

  document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right><b>Total</b></td>');
  document.write('<td width=35%><b> </b></td>');
  document.write('<td width=20%><b> </b></td>');
  document.write('<td width=10% align=right><b>' + IntToStr(totpieces) + '</b></td>');
  document.write('<td width=10% align=right><b> </b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [prnt]) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [prut]) + '</b></td>');
  document.write('<td width=10% align=right><b> </b></td>');

  document.write('<br></table>');

  document.MarkBottomNavigateSection;

  document.write('</p></div></body>');
  document.SaveBufferToFile(diskmirror);
  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

end;

procedure TMainForm.DrawSetAlternatePieceList(const tit: string; const lst: TStringList);
var
  i, cl: integer;
  col: string;
  pcs: string;
  aa: integer;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  prn, pru: double;
  prnt, prut: double;
  numpieces: integer;
  mycost: Double;
  bp: brickpool_t;
  totpieces: integer;
  mycosttot: double;
  foo: string;
  cinfo: colorinfo_p;
  www: double;
begin
  DrawHeadLine('<p align=center>' + tit + '</p>');

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Image</b></th>');
  document.write('<th>Color</th>');
  document.write('<th>Edit</th>');
  document.write('<th>N</th>');
  document.write('<th>U</th>');
  document.write('<th>Cost</th>');
  document.write('</tr>');

  aa := 0;
  prnt := 0.0;
  prut := 0.0;
  totpieces := 0;
  mycosttot := 0.0;

  for i := 0 to lst.Count - 1 do
  begin
    if i = 0 then
      if lst.Strings[0] = 'Part,Color,Desc' then
        continue;
    inc(aa);
    splitstring(lst.Strings[i], pcs, col, foo, ',');
    if Pos1('BL ', pcs) then
      pcs := Trim(db.RebrickablePart(Copy(pcs, 4, Length(pcs) - 3)))
    else
      pcs := db.RebrickablePart(Trim(pcs));

    if Pos1('BL', col) then
    begin
      col := Trim(Copy(col, 3, Length(col) - 2));
      cl := db.BrickLinkColorToSystemColor(StrToIntDef(col, 0))
    end
    else
    begin
      col := Trim(col);
      cl := StrToIntDef(col, 0);
    end;
    col := itoa(cl);

    numpieces := inventory.LoosePartCount(pcs, cl);
    totpieces := totpieces + numpieces;
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');

    pci := db.PieceColorInfo(pcs, cl);
    pi := db.PieceInfo(pci);

    document.write('<td width=35%><img width=100px src=' + col + '\' + pcs + '.png>');
    document.write('<a href=spiece/' + pcs + '>' + pcs + '</a></b>');
    document.write(' - ' + db.PieceDesc(pi) + '</td>');
    document.write('<td width=20%>');
    DrawColorCell(cl, 25);
    // document.BlancColorCell(db.colors(cl).RGB, 25);

    cinfo := db.colors(cl);
    if pci = nil then
      document.write('<a href=spiecec/' + pcs + '/' + col + '>' + cinfo.name +
        ' (' + col + ') (BL=' + IntToStr(cinfo.BrickLingColor) + ')' + GetRebrickableColorHtml(cl) +
        '<img src="images\details.png"></a>' +
        HtmlDrawInvImgLink(pcs, cl, pi) + '</td>')
    else
      document.write('<a href=spiecec/' + pcs + '/' + col + '>' + cinfo.name +
        ' (' + col + ') (BL=' + IntToStr(cinfo.BrickLingColor) + ')' + GetRebrickableColorHtml(cl) +
        '<img src="images\details.png"></a>' +
        HtmlDrawInvImgLink(pcs, cl, pi) +
          decide(pci.setmost = '', '', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + lugbulklinks(pci) + '</td>');

    document.write('<td width=15% align=right>' +
            '<br><a href=editpiece/' + pcs + '/' + itoa(cl) + '><img src="images\edit.png"></a>' +
            '<br><a href=diagrampiece/' + pcs + '/' + itoa(cl) + '><img src="images\diagram.png"></a>' +
            '</td>');

    if pci <> nil then
    begin
      prn := pci.EvaluatePriceNew;
      pru := pci.EvaluatePriceUsed;
      www := db.GetItemWeight(pci.piece, pci.color, pi);
      document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Kgr', [prn, dbl_safe_div(prn, www) * 1000]) + '</td>');
      document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Kgr', [pru, dbl_safe_div(pru, www) * 1000]) + '</td>');
      prnt := prnt + prn * numpieces;
      prut := prut + pru * numpieces;

    end
    else
    begin
      document.write('<td width=15% align=right>-</td>');
      document.write('<td width=15% align=right>-</td>');
    end;
    mycost := GetItemCostDbl(pcs, cl);
    document.write('<td width=10% align=right>' + Format('€ %2.4f<br>€ %2.4f', [mycost, mycost * numpieces]) + '</td>');
    mycosttot := mycosttot + mycost * numpieces;
    document.write('</tr>');

    bp.part := pcs;
    bp.color := cl;
    bp.num := numpieces;

    DrawBrickOrderInfo(@bp);
  end;
  document.write('</table>');

{  document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right><b>Total</b></td>');
  document.write('<td width=35%><b> </b></td>');
  document.write('<td width=20%><b> </b></td>');
  document.write('<td width=10% align=right><b>' + IntToStr(totpieces) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [prnt]) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [prut]) + '</b></td>');
  document.write('<td width=10% align=right><b> </b></td>');}

end;

procedure TMainForm.DrawPieceListLugbulk(const tit: string; const lst: TStringList);
var
  i, cl: integer;
  col: string;
  pcs: string;
  aa: integer;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  prn, pru: double;
  prnt, prut: double;
  numpieces: integer;
  bp: brickpool_t;
  totpieces: integer;
  mycosttot: double;
  cinfo: colorinfo_p;
  www: double;
begin
  UpdateDismantaledsetsinv;
  ShowSplash;
  SplashProgress('Working...', 0);

  if domultipagedocuments then
    document.NewMultiPageDocument('DrawPieceListLugbulk', tit + itoa(lst.count));

  document.write('<body background="splash.jpg">');
  document.title(tit);
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine(tit);

  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Image</b></th>');
  document.write('<th>Color</th>');
  document.write('<th>Demand</th>');
  document.write('<th>Cost (New)</th>');
  document.write('<th>Times Sold</th>');
  document.write('<th>Total Qty</th>');
  document.write('</tr>');

  aa := 0;
  prnt := 0.0;
  prut := 0.0;
  totpieces := 0;
  mycosttot := 0.0;

  for i := 0 to lst.Count - 1 do
  begin
    inc(aa);
    document.StartItemId(aa);
    splitstring(lst.Strings[i], pcs, col, ',');
    cl := atoi(col);
    numpieces := inventory.LoosePartCount(pcs, cl);
    totpieces := totpieces + numpieces;
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');

    pci := db.PieceColorInfo(pcs, cl, lst.Objects[i]);
    pi := db.PieceInfo(pci);

    document.write('<td width=35%><img width=100px src=' + col + '\' + pcs + '.png>');
    document.write('<a href=spiece/' + pcs + '>' + pcs + '</a></b>');
    document.write(' - ' + db.PieceDesc(pi) + '</td>');
    document.write('<td width=20%>');
    DrawColorCell(cl, 25);
    
    // document.BlancColorCell(db.colors(cl).RGB, 25);
    cinfo := db.colors(cl);
    if pci = nil then
    begin
      document.write('<a href=spiecec/' + pcs + '/' + col + '>' + cinfo.name +
        ' (' + col + ') (BL=' + IntToStr(cinfo.BrickLingColor) + ')' + GetRebrickableColorHtml(cl) + '<img src="images\details.png"></a>' +
        HtmlDrawInvImgLink(pcs, cl, pi) +
          '</td>');
      document.write('<td width=15% align=right>' + Format('%2.3f', [0.0]) +
              '<br><a href=editpiece/' + pcs + '/' + itoa(cl) + '><img src="images\edit.png"></a>' +
              '<br><a href=diagrampiece/' + pcs + '/' + itoa(cl) + '><img src="images\diagram.png"></a>' +
              '</td>');
    end
    else
    begin
      document.write('<a href=spiecec/' + pcs + '/' + col + '>' + cinfo.name +
        ' (' + col + ') (BL=' + IntToStr(cinfo.BrickLingColor) + ')' + GetRebrickableColorHtml(cl) + '<img src="images\details.png"></a>' +
        HtmlDrawInvImgLink(pcs, cl, pi) +
         decide(pci.setmost = '', '', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + lugbulklinks(pci) + '</td>');
       document.write('<td width=15% align=right>' + Format('%2.3f', [pci.nDemand]) +
              '<br><a href=editpiece/' + pcs + '/' + itoa(cl) + '><img src="images\edit.png"></a>' +
              '<br><a href=diagrampiece/' + pcs + '/' + itoa(cl) + '><img src="images\diagram.png"></a>' +
              '</td>');
    end;

    if pci <> nil then
    begin
      prn := pci.EvaluatePriceNew;
      pru := pci.EvaluatePriceUsed;
      www := db.GetItemWeight(pci.piece, pci.color, pi);
      document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Kgr', [prn, dbl_safe_div(prn, www) * 1000]) + '</td>');
      prnt := prnt + prn * numpieces;
      prut := prut + pru * numpieces;
      document.write('<td width=10% align=right>' + itoa(pci.priceguide.nTimesSold) + '</td>');
      document.write('<td width=10% align=right>' + itoa(pci.priceguide.nTotalQty) + '</td>');

    end
    else
    begin
      document.write('<td width=15% align=right>-</td>');
      document.write('<td width=15% align=right>-</td>');
      document.write('<td width=15% align=right>-</td>');
    end;
    document.write('</tr>');

    bp.part := pcs;
    bp.color := cl;
    bp.num := numpieces;

    DrawBrickOrderInfo(@bp);
    SplashProgress('Working...', i / lst.Count);
  end;

  document.EndNavigateSection;

  document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right><b>Total</b></td>');
  document.write('<td width=35%><b> </b></td>');
  document.write('<td width=20%><b> </b></td>');
  document.write('<td width=10% align=right><b>' + IntToStr(totpieces) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [prnt]) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [prut]) + '</b></td>');
  document.write('<td width=10% align=right><b> </b></td>');

  document.write('<br></table>');

  document.MarkBottomNavigateSection;

  document.write('</p></div></body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

end;

procedure TMainForm.ShowPieceCInventory(const pcs: string; const color: integer);
var
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  inv: TBrickInventory;
  i, idx: integer;
  numpieces: integer;
  prn, pru, mycost: double;
  bp: brickpool_t;
  www: double;
begin
  Screen.Cursor := crHourglass;
  UpdateDismantaledsetsinv;

  pci := db.PieceColorInfo(pcs, color);
  pi := db.PieceInfo(pci);

  document.write('<body background="splash.jpg">');
  document.title(db.Colors(color).name + ' ' + db.PieceDesc(pi));
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine('Inventory for <a href=spiece/' + pcs + '>' + pcs + '</a> - ' + db.Colors(color).name + ' ' + db.PieceDesc(pi) +
    ' <a href=editpiece/' + pcs + '/' + itoa(color) + '><img src="images\edit.png"></a>' +
    ' <a href=spiecec/' + pcs + '/' + itoa(color) + '><img src="images\details.png"></a>' +
    '<br><a href=diagrampiece/' + pcs + '/' + itoa(color) + '><img src="images\diagram.png"></a>' +
    '<br><br><img width=100px src=' + IntToStr(color) + '\' + pcs + '.png>');


  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Image</b></th>');
  document.write('<th>Color</th>');
  document.write('<th>Num Pieces</th>');
  document.write('<th>N</th>');
  document.write('<th>U</th>');
  document.write('<th>Cost</th>');
  document.write('</tr>');

  if (color >= -1) and (color <= MAXINFOCOLOR) then
    if (color = -1) or (db.Colors(color).id = color) then
      if db.Colors(color).knownpieces <> nil then
      begin
        idx := db.Colors(color).knownpieces.IndexOfUCS(pcs);
        if idx >= 0 then
        begin
          i := color;

          inventory.StorePieceInventoryStatsRec(basedefault + 'cache\' + IntToStr(decide(i = -1, 9999, i)) + '\' + pcs + '.history', pcs, i);
          numpieces := inventory.LoosePartCount(pcs, i);
          document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + '1' + '.</td>');
          document.write('<td width=35%><a href=spiece/' + pcs + '><img width=100px src=' + IntToStr(i) + '\' + pcs + '.png></a></td>');
          document.write('<td width=20%>');
          DrawColorCell(i, 25);
          // document.BlancColorCell(db.colors(i).RGB, 25);
          if pci = nil then
            document.write(db.colors(i).name + ' (' + IntToStr(i) + ') (BL=' + IntToStr(db.colors(i).BrickLingColor) + ')' + GetRebrickableColorHtml(i) + '</td>')
          else
            document.write(db.colors(i).name + ' (' + IntToStr(i) + ') (BL=' + IntToStr(db.colors(i).BrickLingColor) + ')' + GetRebrickableColorHtml(i) +
              decide(pci.setmost='', '', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + lugbulklinks(pci) + '</td>');
          document.write('<td width=15% align=right>' + Format('%d', [numpieces]) + '</td>');

          if pci <> nil then
          begin
            prn := pci.EvaluatePriceNew;
            pru := pci.EvaluatePriceUsed;
            www := db.GetItemWeight(pci.piece, pci.color, pi);
            document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Kgr', [prn, dbl_safe_div(prn, www) * 1000]) + '</td>');
            document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Kgr', [pru, dbl_safe_div(pru, www) * 1000]) + '</td>');
          end
          else
          begin
            document.write('<td width=15% align=right>-</td>');
            document.write('<td width=15% align=right>-</td>');
          end;
          mycost := GetItemCostDbl(pcs, i);
          document.write('<td width=10% align=right>' + Format('€ %2.4f<br>€ %2.4f', [mycost, mycost * numpieces]) + '</td>');
          document.write('</tr>');

          bp.part := pcs;
          bp.color := i;
          bp.num := numpieces;

          DrawBrickOrderInfo(@bp);
        end;
      end;

  document.write('</table>');

  inv := pi.Inventory(color);
  if (inv = nil) or (inv.numlooseparts = 0) then
  begin
    DrawHeadLine('Can not find inventory for ' + db.Colors(color).name + ' ' + db.PieceDesc(pi));
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    if inv <> nil then
      inv.Free;
    Screen.Cursor := crDefault;
    Exit;
  end;

  SortInventory(inv);

  DrawInventoryTableNoPages(inv);
  if not DirectoryExists(basedefault + 'cache') then
    MkDir(basedefault + 'cache\');
  if not DirectoryExists(basedefault + 'cache\' + itoa(color)) then
    MkDir(basedefault + 'cache\' + itoa(color));
  inv.StoreHistoryStatsRec(basedefault + 'cache\' + itoa(color) + '\' + decide(pi.name = '', pcs, pi.name) + '.stats');
  inv.StoreHistoryEvalRec(basedefault + 'cache\' + itoa(color) + '\' + decide(pi.name = '', pcs, pi.name) + '.ieval');

  inv.Free;

  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;
end;

procedure TMainForm.ShowColorPiece(const apcs: string; const color: integer;
  const ayear: integer = -1; const doshowsets: boolean = True);
var
  i, j: integer;
  idx: Integer;
  pcs: string;
  aa: integer;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  prn, pru: double;
  numpieces: integer;
  mycost: Double;
  bp: brickpool_t;
  inv: TBrickInventory;
  stmp: string;
  y: string;
  ylist: TDNumberList;
  yyyy: integer;
  ytext: string;
  ayearcount: integer;
  fryear, toyear: integer;
  ofsetscnt: integer;
  storages: TStringList;
  stortxt: string;
  tmpyear: integer;
  tmpset: string;
  www: double;
  taghtml: string;
  slugbulks: string;
begin
  UpdateDismantaledsetsinv;

  pci := db.PieceColorInfo(apcs, color);
  if pci <> nil then
    pcs := pci.piece
  else
    pcs := apcs;
  pi := db.PieceInfo(pci);

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowColorPiece', pcs + '_' + itoa(color) + '_' + itoa(ayear) + '_' + btoa(doshowsets));
  document.write('<body background="splash.jpg">');
  document.title(db.Colors(color).name + ' ' + db.PieceDesc(pi));
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  ylist := TDNumberList.Create;

  ayearcount := 0;
  stmp := '';
  taghtml := '';
  if pci <> nil then
  begin
    if ayear < 0 then
      if color <> CATALOGCOLORINDEX then
        if color <> INSTRUCTIONCOLORINDEX then
          if color <> BOXCOLORINDEX then
            stmp := '<br>Appears in ' + IntToStr(pci.sets.Count) + ' set' + decide(pci.sets.Count = 1, '', 's');
    fryear := pci.firstsetyear;
    toyear := pci.lastsetyear;
    if fryear > 0 then
      if toyear > 0 then
      begin
        ofsetscnt := 0;
        for i := 0 to pci.sets.Count - 1 do
          if not db.IsMoc(pci.sets.Strings[i]) then
            inc(ofsetscnt);
        stmp := stmp + ' (In ' + itoa(ofsetscnt) + ' official sets from ' + itoa(fryear) + ' to ' + itoa(toyear) + ')';
      end;
    for i := 0 to pci.sets.Count - 1 do
    begin
      yyyy := db.SetYear(pci.sets.Strings[i]);
      if ylist.IndexOf(yyyy) < 0 then
        ylist.Add(yyyy);
      if ayear = yyyy then
        inc(ayearcount);
    end;
    if ayear >= 0 then
      if color <> CATALOGCOLORINDEX then
        if color <> INSTRUCTIONCOLORINDEX then
          if color <> BOXCOLORINDEX then
            stmp := '<br>Appears in ' + IntToStr(ayearcount) + ' set' + decide(ayearcount = 1, '', 's') + ' in year ' + IntToStr(ayear);
    if pci.code <> '' then
      stmp := stmp + '<br>(Lego Code="' + pci.code + '")';
    if pci.tags <> nil then
      if pci.tags.Count > 0 then
      begin
        taghtml := '<b>Tags: </b> ';
        taghtml := taghtml + taglink(pci.tags.Strings[0]);
        for i := 1 to pci.tags.Count - 1 do
          taghtml := taghtml + ', ' + taglink(pci.tags.Strings[i]);
      end;
  end;

  ytext := '';
  if ylist.Count > 0 then
  begin
    ylist.Sort;
    ytext := 'Years<br><br>';
    if ayear >= 0 then
      ytext := ytext + '<a href=spiecec/' + pcs + '/' + itoa(color) + '>All</a>'
    else
      ytext := ytext + 'All';
    for i := 0 to ylist.Count - 1 do
    begin
      ytext := ytext + ' - ';
      if ayear = ylist.Numbers[i] then
        ytext := ytext + decide(ylist.Numbers[i] = 0, '(Unknown)', itoa(ylist.Numbers[i]))
      else
        ytext := ytext + '<a href=spiecec/' + pcs + '/' + itoa(color) + '/' + itoa(ylist.Numbers[i]) + '>' + decide(ylist.Numbers[i] = 0, '(Unknown)', itoa(ylist.Numbers[i])) + '</a>';
    end;
  end;

  ylist.Free;

  DrawHeadLine('<a href=spiece/' + pcs + '>' + pcs + '</a> - ' + db.Colors(color).name + ' ' + db.PieceDesc(pi) +
    ' <a href=editpiece/' + pcs + '/' + itoa(color) + '><img src="images\edit.png"></a>' +
    HtmlDrawInvImgLink(pcs, color, pi) +
    '<br><a href=diagrampiece/' + pcs + '/' + itoa(color) + '><img src="images\diagram.png"></a>' +
    '<br><br><img width=100px src=' + IntToStr(color) + '\' + pcs + '.png>' + stmp);

  storages := db.StorageBinsForPart(pcs, color);
  if storages.Count > 0 then
  begin
    stortxt := '<b>Storage Bins:</b><br>';
    for i := 0 to storages.Count - 1 do
      stortxt := stortxt + '<a href=storage/' + storages.Strings[i] + '>' + storages.Strings[i] + '</a><br>';
    DrawHeadLine(stortxt);
  end;
  storages.Free;

  if ytext <> '' then
    DrawHeadLine(ytext);

  if taghtml <> '' then
    DrawHeadLine(taghtml);

  slugbulks := lugbulklinks(pci);
  if slugbulks <> '' then
    DrawHeadLine(slugbulks);

  DrawPriceguideEx(pcs, color, IncDay(Now, -30));

  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Image</b></th>');
  document.write('<th>Color</th>');
  document.write('<th>Num Pieces</th>');
  document.write('<th>N</th>');
  document.write('<th>U</th>');
  document.write('<th>Cost</th>');
  document.write('</tr>');

  aa := 0;

  if (color >= -1) and (color <= MAXINFOCOLOR) then
    if (color = -1) or (db.Colors(color).id = color) then
      if db.Colors(color).knownpieces <> nil then
      begin
        idx := db.Colors(color).knownpieces.IndexOfUCS(pcs);
        if idx >= 0 then
        begin
          i := color;

          inventory.StorePieceInventoryStatsRec(basedefault + 'cache\' + IntToStr(decide(i = -1, 9999, i)) + '\' + pcs + '.history', pcs, i);
          numpieces := inventory.LoosePartCount(pcs, i);
          document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>1.</td>');
          document.write('<td width=35%><a href=spiece/' + pcs + '><img width=100px src=' + IntToStr(i) + '\' + pcs + '.png></a></td>');
          document.write('<td width=20%>');
          DrawColorCell(i, 25);
          // document.BlancColorCell(db.colors(i).RGB, 25);
          if pci = nil then
            document.write(db.colors(i).name + ' (' + IntToStr(i) + ') (BL=' + IntToStr(db.colors(i).BrickLingColor) + ')' + GetRebrickableColorHtml(i) + '</td>')
          else
            document.write(db.colors(i).name + ' (' + IntToStr(i) + ') (BL=' + IntToStr(db.colors(i).BrickLingColor) + ')' + GetRebrickableColorHtml(i) +
              decide(pci.setmost='', '', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + '</td>');
          document.write('<td width=15% align=right>' + Format('%d', [numpieces]) + '</td>');

          if pci <> nil then
          begin
            prn := pci.EvaluatePriceNew;
            pru := pci.EvaluatePriceUsed;
            www := db.GetItemWeight(pci.piece, pci.color, pi);
            document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Kgr', [prn, dbl_safe_div(prn, www) * 1000]) + '</td>');
            document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Kgr', [pru, dbl_safe_div(pru, www) * 1000]) + '</td>');
          end
          else
          begin
            document.write('<td width=15% align=right>-</td>');
            document.write('<td width=15% align=right>-</td>');
          end;
          mycost := GetItemCostDbl(pcs, i);
          document.write('<td width=10% align=right>' + Format('€ %2.4f<br>€ %2.4f', [mycost, mycost * numpieces]) + '</td>');
          document.write('</tr>');

          bp.part := pcs;
          bp.color := i;
          bp.num := numpieces;

          DrawBrickOrderInfo(@bp);

          if doshowsets then
            if pci <> nil then
              for j := 0 to pci.sets.Count - 1 do
              begin
                tmpset := pci.sets.Strings[j];
                tmpyear := db.SetYear(tmpset);
                if (ayear < 0) or (tmpyear = ayear) then
                begin
                  inc(aa);
                  document.StartItemId(aa);
                  inv := db.GetSetInventory(tmpset);
                  if tmpyear > 1931 then
                    y := Format('[Year: <a href=ShowSetsAtYear/%d>%d</a>]', [tmpyear, tmpyear])
                  else
                    y := '';
                  DrawHeadLine(Format('%d. <a href="sinv/%s">%s - %s</a> %s<br>(appears %d times)<br><img width=240px src=s\' + tmpset + '.jpg>',
                    [aa, tmpset, tmpset, db.SetDesc(tmpset), y, inv.LoosePartCount(pcs, color)]));
                end;
              end;
        end;
      end;

  document.EndNavigateSection;

  document.write('</table>');
  document.MarkBottomNavigateSection;
  document.write('<br></p></div>');
  document.write(BLColorPieceInfo(pcs, color));
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;

end;

procedure TMainForm.ShowInstructions(const apcs: string);
var
  i: integer;
  pcs: string;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  stmp: string;
  instructions: TStringList;
  pdfs: TDStringList;
  spdfreader: string;
  refreshstr: string;
  rotatestr: string;
begin
  Screen.Cursor := crHourglass;

  UpdateDismantaledsetsinv;

  pci := db.PieceColorInfo(apcs, 9997);
  if pci <> nil then
    pcs := pci.piece
  else
    pcs := apcs;
  pi := db.PieceInfo(pci);

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowInstructions', pcs);
  document.write('<body background="splash.jpg">');
  document.title('Building instructions for ' + db.PieceDesc(pi));
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  stmp := '';
  if pci <> nil then
    if pci.code <> '' then
      stmp := stmp + '<br>(Lego Code="' + pci.code + '")';

  DrawHeadLine('<a href=spiece/' + pcs + '>' + pcs + '</a> - ' + db.Colors(9997).name + ' ' + db.PieceDesc(pi) +
    ' <a href=editpiece/' + pcs + '/9997><img src="images\edit.png"></a>' +
    HtmlDrawInvImgLink(pcs, 9997, pi) +
    '<br><a href=diagrampiece/' + pcs + '/9997><img src="images\diagram.png"></a>' +
    '<br><br><img width=360px src=9997\' + pcs + '.png>' + stmp);

  document.StartNavigateSection;

  inventory.StorePieceInventoryStatsRec(basedefault + 'cache\9997\' + pcs + '.history', pcs, 9997);

  pdfs := findfiles(basedefault + InstructionDir(pcs) + '*.pdf');
  if pdfs.Count > 0 then
  begin
    spdfreader :=
      '<table width=99% bgcolor=' + TBGCOLOR + ' border=2><tr>' +
      '<tr bgcolor=' + THBGCOLOR + '><th><b>Files</b></th></tr></table>' +
      '<table width=99% bgcolor=' + TBGCOLOR + ' border=2><tr>' +
      '<tr bgcolor=' + TBGCOLOR + '>';
    for i := 0 to pdfs.Count - 1 do
      spdfreader := spdfreader +
        '<td><p align=center><a href="pdfreader/' + pcs + '/' +
        ChangeFileExt(ExtractFileName(pdfs.Strings[i]), '') + '"><img src="images\pdfreader.png"><br>' +
        ChangeFileExt(ExtractFileName(pdfs.Strings[i]), '') + '</p></td></a>';
    spdfreader := spdfreader + '</tr></table>';
    document.write(spdfreader);
  end;

  instructions := TStringList.Create;
  GetInstructions(pcs, instructions);

  if instructions.Count > 0 then
  begin
    document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
    document.write('<tr bgcolor=' + THBGCOLOR + '>');
    document.write('<th><b>#</b></th>');
    document.write('<th><b>Image</b></th>');
    document.write('</tr>');

    for i := 0 to instructions.Count - 1 do
    begin
      document.StartItemId(i + 1);
      document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + itoa(i + 1) + '</td>');
      rotatestr :=
        ' <a href="rotatejpegleft/' + instructions.Strings[i] + '"><img src=images/rotate_left.png></a>' +
        ' <a href="rotatejpegright/' + instructions.Strings[i] + '"><img src=images/rotate_right.png></a>';
      document.write('<td width=95%><img src=' + instructions.Strings[i] + '>' + rotatestr + '</td>');
    end;
  end;

  if (pdfs.Count = 0) and (instructions.Count = 0) then
    DrawHeadline('No instructions found for ' + pcs);

  pdfs.Free;
  instructions.Free;

  document.EndNavigateSection;

  document.write('</table>');
  document.MarkBottomNavigateSection;

  document.write('<br>');
  refreshstr :=
    '<a href="updateinstructionsfromnethost/' + pcs + '/lego">Update from lego.com</a><br>' +
    '<a href="updateinstructionsfromnethost/' + pcs + '/letsbuilditagain">Update from letsbuilditagain.com</a><br>' +
    '<a href="updateinstructionsfromnethost/' + pcs + '/brickfactory">Update from brickfactory.info</a><br>';
  DrawHeadline(refreshstr);

  document.write('<br></p></div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;

  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowSetsICanBuild(const pct: double; const dosets, dofigs: boolean);
const
  MAXST1SETS = $20000;
type
  struct1 = record
    setid: string[15];
    set_tot_pieces: integer;
    set_have_pieces: integer;
    set_pct: double;
  end;
  struct1A = array[0..MAXST1SETS - 1] of struct1;
  struct1AP = ^struct1A;
var
  A: struct1AP;
  tot1, mis1: integer;
  pct1: double;
  inv: TBrickInventory;
  i: integer;
  numsets: integer;
  aa: integer;
  links: string;
  ok: boolean;
  pci: TPieceColorInfo;
begin
  Screen.Cursor := crHourGlass;

  ShowSplash;
  SplashProgress('Working...', 0);
  new(A);
  numsets := 0;
  for i := 0 to db.AllSets.Count - 1 do
  begin
    if i mod 100 = 0 then
      SplashProgress('Working...', i / db.AllSets.Count);

    if dosets and dofigs then
      ok := True
    else
    begin
      ok := False;
      if dofigs then
      begin
        pci := db.PieceColorInfo(db.AllSets.Strings[i], -1);
        if pci <> nil then
          ok := pci.sparttype = 'M';
      end
      else
      begin
        pci := db.PieceColorInfo(db.AllSets.Strings[i], -1);
        if pci <> nil then
          ok := pci.sparttype <> 'M';
      end;
    end;

    if ok then
    begin
      inv := db.GetSetInventory(db.AllSets.Strings[i]);
      if inv.numsets = 0 then
      begin
        tot1 := inv.totallooseparts;
        if tot1 > 0 then
        begin
          mis1 := inventory.MissingToBuildInventory(inv);
          pct1 := (tot1 - mis1) / tot1;
          if pct1 >= pct then
          begin
            A[numsets].setid := db.AllSets.Strings[i];
            A[numsets].set_tot_pieces := tot1;
            A[numsets].set_have_pieces := tot1 - mis1;
            A[numsets].set_pct := pct1;
            inc(numsets);
            if numsets = MAXST1SETS then
              break;
          end;
        end;
      end;
    end;

  end;

  SplashProgress('Working...', 1);
  if domultipagedocuments then
    document.NewMultiPageDocument('ShowSetsICanBuild_', ftoa(pct) + '_' + btoa(dosets) + '_' + btoa(dofigs));

  document.write('<body background="splash.jpg">');
  document.title('Sets I can build');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  links := '';
  if not dbl_equal(pct, 0.7) then
    links := links + '<a href=' + decide(dosets, 'setsIcanbuild', 'figsIcanbuild') + '/7/10>Check 70%</a><br>';
  if not dbl_equal(pct, 0.8) then
    links := links + '<a href=' + decide(dosets, 'setsIcanbuild', 'figsIcanbuild') + '/8/10>Check 80%</a><br>';
  if not dbl_equal(pct, 0.9) then
    links := links + '<a href=' + decide(dosets, 'setsIcanbuild', 'figsIcanbuild') + '/9/10>Check 90%</a><br>';

  DrawHeadLine(Format(decide(dosets, 'Sets', 'Minifigs') + ' that I can build<br> (Loose parts are at least %2.3f%s of ' + decide(dosets, 'set', 'minifig') + ' inventory)<br>', [100 * pct, '%']) + links);

  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Set</b></th>');
  document.write('<th>Num Pieces</th>');
  document.write('<th>Exists</th>');
  document.write('<th>Missing</th>');
  document.write('<th>%s</th>', ['%']);
  document.write('</tr>');
  HideSplash;

  ShowSplash;
  SplashProgress('Working...', 0);
  aa := 0;
  for i := 0 to numsets - 1 do
  begin
    inc(aa);
    document.StartItemId(aa);
//    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%><img width=64px src=s\' + A[i].setid + '.jpg><br>');
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%>' +
                    MakeThumbnailImage2(A[i].setid, -1) + '<br>');
    document.write('<a href="sinv/' + A[i].setid + '">');
    document.write('<b>' + A[i].setid + '</b> - ' + db.SetDesc(A[i].setid));
    document.write('</td><td width=15% align=right>');
    document.write(IntToStr(A[i].set_tot_pieces) + '</td>');
    document.write('</td><td width=15% align=right>');
    document.write(IntToStr(A[i].set_have_pieces) + '</td>');
    document.write('</td><td width=15% align=right>');
    document.write(IntToStr(A[i].set_tot_pieces - A[i].set_have_pieces) + '</td>');
    document.write('</td><td width=15% align=right>');
    document.write(Format('%2.3f%s', [A[i].set_pct * 100, '%']) + '</td>');

    if numsets > 1 then
    begin
      if numsets < 20 then
        SplashProgress('Working...', i / (numsets - 1))
      else
        if (i mod 5) = 0 then
          SplashProgress('Working...', i / (numsets - 1));
    end;
    document.write('</tr>');
  end;
  document.EndNavigateSection;

  document.write('</table>');

  document.MarkBottomNavigateSection;

  document.write('</p><br><br></p></div></body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  Screen.Cursor := crDefault;

  dispose(A);
end;

procedure TMainForm.UpdateSetAssetsFromBricklink(const s: string);
begin
  db.UpdateSetAssetsFromBricklink(s);
end;

procedure TMainForm.ShowSetsAtUnknownYear;
var
  lsets: TStringList;
  i: integer;
  aa: integer;
begin
  Screen.Cursor := crHourGlass;
  ShowSplash;
  SplashProgress('Working...', 0);

  lsets := db.SetListAtYear(0);

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowSetsAtUnknownYear', itoa(lsets.count));

  document.write('<body background="splash.jpg">');
  document.title('Sets with unknown release year');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine('Sets with unknown release year');

  DrawHeadLine('Sets');
  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Set</b></th>');
  document.write('</tr>');
  HideSplash;

  aa := 0;
  for i := 0 to lsets.Count - 1 do
  begin
    inc(aa);
    document.StartItemId(aa);
//    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%><img width=64px src=s\' + lsets.Strings[i] + '.jpg><br>');
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%>' +
                    MakeThumbnailImage2(lsets.Strings[i], -1) + '<br>');
    document.write('<a href="sinv/' + lsets.Strings[i] + '">');
    document.write('<b>' + lsets.Strings[i] + '</b> - ' + db.SetDesc(lsets.Strings[i]));
    if not db.IsMoc(lsets.Strings[i]) then
      document.write(' <a href=UpdateSetAssetsFromBricklink/' + lsets.Strings[i] + '><img src="images\refresh.png"></a>');

    if lsets.Count > 1 then
    begin
      if lsets.Count < 20 then
        SplashProgress('Working...', i / (lsets.Count - 1))
      else
        if (i mod 5) = 0 then
          SplashProgress('Working...', i / (lsets.Count - 1));
    end;
    document.write('</tr>');
  end;
  document.EndNavigateSection;
  HideSplash;

  document.write('</table>');

  document.MarkBottomNavigateSection;

  document.write('</p><br><br></p></div></body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;

  lsets.Free;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowSetsAtYear(const year: integer);
var
  lsets: TStringList;
  i: integer;
  aa: integer;
begin
  Screen.Cursor := crHourGlass;

  lsets := db.SetListAtYear(year);

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowSetsAtYear', itoa(year));

  document.write('<body background="splash.jpg">');
  document.title('Sets released at ' + itoa(year));
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine('<a href=ShowSetsAtYear/' + itoa(year - 1) + '>' + itoa(year - 1) + '</a> - ' +
               '<b>' + itoa(year) + '</b>' +
               ' - <a href=ShowSetsAtYear/' + itoa(year + 1) + '>' + itoa(year + 1) + '</a> ');

  DrawHeadLine('Sets');

  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Set</b></th>');
  document.write('</tr>');
  HideSplash;

  ShowSplash;
  aa := 0;
  for i := 0 to lsets.Count - 1 do
  begin
    inc(aa);
    document.StartItemId(aa);
//    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%><img width=64px src=s\' + lsets.Strings[i] + '.jpg><br>');
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%>' +
                    MakeThumbnailImage2(lsets.Strings[i], -1) + '<br>');
    document.write('<a href="sinv/' + lsets.Strings[i] + '">');
    document.write('<b>' + lsets.Strings[i] + '</b> - ' + db.SetDesc(lsets.Strings[i]));

    if lsets.Count > 1 then
    begin
      if lsets.Count < 20 then
        SplashProgress('Working...', i / (lsets.Count - 1))
      else
        if (i mod 5) = 0 then
          SplashProgress('Working...', i / (lsets.Count - 1));
    end;
  document.write('</tr>');
  end;
  document.EndNavigateSection;
  HideSplash;

  document.write('</table>');

  document.MarkBottomNavigateSection;

  document.write('</p><br><br></p></div></body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;

  lsets.Free;
  Screen.Cursor := crDefault;

end;

type
  _NUcostclass = class(TObject)
    Nsetcost: double;
    Ninvcost: double;
    Ndemand: double;
    Usetcost: double;
    Uinvcost: double;
    Udemand: double;
    numparts: integer;
    numlots: integer;
  end;

procedure TMainForm.ShowSetsDataCompare(const tit: string; const setlist: TStringList);
var
  i: integer;
  list: TStringList;
  setcostN, setcostU: double;
  invcostN, invcostU: double;
  demandN, demandU: double;
  inv: TBrickInventory;
  inv2: TBrickInventory;
  av: availability_t;
  aa: integer;
  cls: _NUcostclass;
  numparts: integer;
  numlots: integer;
  yyy: integer;
begin
  Screen.Cursor := crHourGlass;

  list := TStringList.Create;

  ShowSplash;
  SplashProgress('Working...', 0);
  for i := 0 to setlist.Count - 1 do
  begin
    av := db.Availability(setlist.Strings[i]);
    setcostN := av.nQtyAvgPrice;
    setcostU := av.uQtyAvgPrice;
    invcostN := 0.0;
    invcostU := 0.0;

    inv := db.GetSetInventory(setlist.Strings[i]);
    numparts := 0;
    numlots := 0;
    if inv.numsets > 0 then
    begin
      inv2 := inv.Clone;
      inv2.DismandalAllSets;

      demandN := inv2.nDemand.value;
      demandU := inv2.nDemand.value;
      inv2.Reorganize;
      invcostN := invcostN + inv2.EvaluatedPartOutValue_nQtyAvg.value;
      invcostU := invcostU + inv2.EvaluatedPartOutValue_uQtyAvg.value;
      numparts := numparts + inv2.totallooseparts;
      numlots := numlots + inv2.numlooseparts;
      inv2.Free;
    end
    else
    begin
      demandN := inv.nDemand.value;
      demandU := inv.uDemand.value;
      invcostN := invcostN + inv.EvaluatedPartOutValue_nQtyAvg.value;
      invcostU := invcostU + inv.EvaluatedPartOutValue_uQtyAvg.value;
      numparts := numparts + inv.totallooseparts;
      numlots := numlots + inv.numlooseparts;
    end;

    cls := _NUcostclass.Create;
    cls.Ninvcost := invcostN;
    cls.Nsetcost := setcostN;
    cls.Ndemand := demandN;
    cls.Uinvcost := invcostU;
    cls.Usetcost := setcostU;
    cls.Udemand := demandU;
    cls.numparts := numparts;
    cls.numlots := numlots;
    list.AddObject(setlist.Strings[i], cls);
  end;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowSetsDataCompare', itoa(list.count) + '_' + list.Text + '_' + tit);

  document.write('<body background="splash.jpg">');
  document.title(tit);
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine(tit);
  DrawHeadLine('<a href=multysetinv/' + IntToStr(Integer(setlist)) + '>Show inventory of the above sets</a>');

  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Set</b></th>');
  document.write('<th>Year</th>');
  document.write('<th>Num Pieces</th>');

  document.write('<th>Set Cost (N)</th>');
  document.write('<th>Part Out value (N)</th>');
  document.write('<th>Demand (N)</th>');
  document.write('<th>GAIN (N)</th>');
  document.write('<th>J-value (N)</th>');

  document.write('<th>Set Cost (U)</th>');
  document.write('<th>Part Out value (U)</th>');
  document.write('<th>Demand (U)</th>');
  document.write('<th>GAIN (U)</th>');
  document.write('<th>J-value (U)</th>');

  document.write('</tr>');
  HideSplash;

  ShowSplash;
  aa := 0;
  for i := 0 to list.Count - 1 do
  begin
    inc(aa);
    document.StartItemId(aa);
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=25%>' +
                    MakeThumbnailImage2(list.Strings[i], -1) + '<br>');
    document.write('<a href="sinv/' + list.Strings[i] + '">');
    document.write('<b>' + list.Strings[i] + '</b> - ' + db.SetDesc(list.Strings[i]));
    document.write('</td><td width=8% align=right>');
    yyy := db.SetYear(list.Strings[i]);
    document.write('<a href=ShowSetsAtYear/%d>%d</a>', [yyy, yyy]);
    document.write('</td><td width=8% align=right>');
    cls := list.Objects[i] as _NUcostclass;
    document.write(IntToStr(cls.numparts) + ' parts <br>' + IntToStr(cls.numlots) + ' lots</td>');

    document.write('</td><td width=8% align=right>');
    document.write('€ %2.2f</td>', [cls.Nsetcost]);
    document.write('</td><td width=8% align=right>');
    document.write('€ %2.2f</td>', [cls.Ninvcost]);
    document.write('</td><td width=8% align=right>');
    document.write('%2.2f</td>', [cls.Ndemand]);
    document.write('</td><td width=8% align=right>');
    document.write(Format('%2.3f%s', [dbl_safe_div(cls.Ninvcost, cls.Nsetcost) * 100, '%']) + '</td>');
    document.write('</td><td width=8% align=right>');
    document.write(Format('%2.3f', [dbl_safe_div(cls.Ninvcost, cls.Nsetcost) * cls.Ndemand]) + '</td>');

    document.write('</td><td width=8% align=right>');
    document.write('€ %2.2f</td>', [cls.Usetcost]);
    document.write('</td><td width=8% align=right>');
    document.write('€ %2.2f</td>', [cls.Uinvcost]);
    document.write('</td><td width=8% align=right>');
    document.write('%2.2f</td>', [cls.Udemand]);
    document.write('</td><td width=8% align=right>');
    document.write(Format('%2.3f%s', [dbl_safe_div(cls.Uinvcost, cls.Usetcost) * 100, '%']) + '</td>');
    document.write('</td><td width=8% align=right>');
    document.write(Format('%2.3f', [dbl_safe_div(cls.Uinvcost, cls.Usetcost) * cls.Udemand]) + '</td></tr>');

    if list.Count > 1 then
    begin
      if list.Count < 20 then
        SplashProgress('Working...', i / (list.Count - 1))
      else
        if (i mod 5) = 0 then
          SplashProgress('Working...', i / (list.Count - 1));
    end;
  end;
  document.EndNavigateSection;

  document.write('</table>');

  document.MarkBottomNavigateSection;

  document.write('</p><br><br></p></div></body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  Screen.Cursor := crDefault;

  FreeList(list);
end;

type
  _costclass = class(TObject)
    setcost: double;
    invcost: double;
    numparts: integer;
    numlots: integer;
    demand: double;
  end;

procedure TMainForm.ShowSetsForPartOutNew(const minyear, minavailablelots: integer;
  const mindemand, mincostmultiplier: double);
var
  i: integer;
  list: TStringList;
  setcost: double;
  invcost: double;
  inv: TBrickInventory;
  inv2: TBrickInventory;
  av: availability_t;
  aa: integer;
  cls: _costclass;
  numparts: integer;
  numlots: integer;
  demand: double;
  yyy: integer;
begin
  Screen.Cursor := crHourGlass;

  list := TStringList.Create;

  ShowSplash;
  SplashProgress('Working...', 0);
  for i := 0 to db.AllSets.Count - 1 do
  begin
    if i mod 100 = 0 then
      SplashProgress('Working...', i / db.AllSets.Count);

    if db.SetYear(db.AllSets.Strings[i]) < minyear then
      continue;

    av := db.Availability(db.AllSets.Strings[i]);
    if av.nTotalLots < minavailablelots then
      continue;
    setcost := av.nQtyAvgPrice;
    invcost := 0.0;

    inv := db.GetSetInventory(db.AllSets.Strings[i]);
    numparts := 0;
    numlots := 0;
    if inv.numsets > 0 then
    begin
      inv2 := inv.Clone;
      inv2.DismandalAllSets;

      demand := inv2.nDemand.value;
      if demand < mindemand then
      begin
        inv2.Free;
        continue;
      end;
      inv2.Reorganize;
      invcost := invcost + inv2.EvaluatedPartOutValue_nQtyAvg.value;
      numparts := numparts + inv2.totallooseparts;
      numlots := numlots + inv2.numlooseparts;
      inv2.Free;
    end
    else
    begin
      demand := inv.nDemand.value;
      if demand < mindemand then
        continue;
      invcost := invcost + inv.EvaluatedPartOutValue_nQtyAvg.value;
      numparts := numparts + inv.totallooseparts;
      numlots := numlots + inv.numlooseparts;
    end;

    if invcost > setcost * mincostmultiplier then
    begin
      cls := _costclass.Create;
      cls.invcost := invcost;
      cls.setcost := setcost;
      cls.numparts := numparts;
      cls.numlots := numlots;
      cls.demand := demand;
      list.AddObject(db.AllSets.Strings[i], cls);
    end;
  end;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowSetsForPartOutNew', itoa(minyear) + '_' + itoa(minavailablelots) + '_' + ftoa(mindemand) + '_' + ftoa(mincostmultiplier));

  document.write('<body background="splash.jpg">');
  document.title('New sets to buy for partout');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine('New sets to buy for partout');
  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Set</b></th>');
  document.write('<th>Year</th>');
  document.write('<th>Num Pieces</th>');
  document.write('<th>Set Cost</th>');
  document.write('<th>Part Out value</th>');
  document.write('<th>Demand</th>');
  document.write('<th>GAIN</th>');
  document.write('<th>J-value</th>');
  document.write('</tr>');
  HideSplash;

  ShowSplash;
  aa := 0;
  for i := 0 to list.Count - 1 do
  begin
    inc(aa);
    document.StartItemId(aa);
//    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%><img width=64px src=s\' + list.Strings[i] + '.jpg><br>');
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%>' +
                    MakeThumbnailImage2(list.Strings[i], -1) + '<br>');
    document.write('<a href="sinv/' + list.Strings[i] + '">');
    document.write('<b>' + list.Strings[i] + '</b> - ' + db.SetDesc(list.Strings[i]));
    document.write('</td><td width=8% align=right>');
    yyy := db.SetYear(list.Strings[i]);
    document.write('<a href=ShowSetsAtYear/%d>%d</a>', [yyy, yyy]);
    document.write('</td><td width=15% align=right>');
    cls := list.Objects[i] as _costclass;
    document.write(IntToStr(cls.numparts) + ' parts <br>' + IntToStr(cls.numlots) + ' lots</td>');
    document.write('</td><td width=15% align=right>');
    document.write('€ %2.2f</td>', [cls.setcost]);
    document.write('</td><td width=15% align=right>');
    document.write('€ %2.2f</td>', [cls.invcost]);
    document.write('</td><td width=15% align=right>');
    document.write('%2.2f</td>', [cls.demand]);
    document.write('</td><td width=15% align=right>');
    document.write(Format('%2.3f%s', [dbl_safe_div(cls.invcost, cls.setcost) * 100, '%']) + '</td>');
    document.write('</td><td width=15% align=right>');
    document.write(Format('%2.3f', [dbl_safe_div(cls.invcost, cls.setcost) * cls.demand]) + '</td></tr>');

    if list.Count > 1 then
    begin
      if list.Count < 20 then
        SplashProgress('Working...', i / (list.Count - 1))
      else
        if (i mod 5) = 0 then
          SplashProgress('Working...', i / (list.Count - 1));
    end;
  end;
  document.EndNavigateSection;

  document.write('</table>');

  document.MarkBottomNavigateSection;

  document.write('</p><br><br></p></div></body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  Screen.Cursor := crDefault;

  FreeList(list);
end;

procedure TMainForm.ShowSetsForPartOutUsed(const minyear, minavailablelots: integer;
  const mindemand, mincostmultiplier: double);
var
  i: integer;
  list: TStringList;
  setcost: double;
  invcost: double;
  inv: TBrickInventory;
  inv2: TBrickInventory;
  av: availability_t;
  aa: integer;
  cls: _costclass;
  numparts: integer;
  numlots: integer;
  demand: double;
  sset: string;
  yyy: integer;
begin
  Screen.Cursor := crHourGlass;

  list := TStringList.Create;

  ShowSplash;
  SplashProgress('Working...', 0);
  for i := 0 to db.AllSets.Count - 1 do
  begin
    if i mod 100 = 0 then
      SplashProgress('Working...', i / db.AllSets.Count);

    if db.SetYear(db.AllSets.Strings[i]) < minyear then
      continue;

    av := db.Availability(db.AllSets.Strings[i]);
    if av.uTotalLots < minavailablelots then
      continue;
    setcost := av.uQtyAvgPrice;

    numparts := 0;
    numlots := 0;
    invcost := 0.0;
    inv := db.GetSetInventory(db.AllSets.Strings[i]);
    if inv.numsets > 0 then
    begin
      inv2 := inv.Clone;
      inv2.DismandalAllSets;

      demand := inv2.uDemand.value;
      if demand < mindemand then
      begin
        inv2.Free;
        continue;
      end;

      invcost := inv2.EvaluatedPartOutValue_uQtyAvg.value;
      numparts := numparts + inv2.totallooseparts;
      numlots := numlots + inv2.numlooseparts;
      inv2.Free;
    end
    else
    begin
      demand := inv.uDemand.value;
      if demand < mindemand then
        continue;

      invcost := inv.EvaluatedPartOutValue_uQtyAvg.value;
      numparts := numparts + inv.totallooseparts;
      numlots := numlots + inv.numlooseparts;
    end;

    if invcost > setcost * mincostmultiplier then
    begin
      cls := _costclass.Create;
      cls.invcost := invcost;
      cls.setcost := setcost;
      cls.numparts := numparts;
      cls.demand := demand;
      cls.numlots := numlots;
      list.AddObject(db.AllSets.Strings[i], cls);
    end;
  end;

  SplashProgress('Working...', 1);
  if domultipagedocuments then
    document.NewMultiPageDocument('ShowSetsForPartOutUsed', itoa(minyear) + '_' + itoa(minavailablelots) + '_' + ftoa(mindemand) + '_' + ftoa(mincostmultiplier));

  document.write('<body background="splash.jpg">');
  document.title('Used sets to buy for partout');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine('Used sets to buy for partout');
  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Set</b></th>');
  document.write('<th>Year</th>');
  document.write('<th>Num Pieces</th>');
  document.write('<th>Set Cost</th>');
  document.write('<th>Part Out value</th>');
  document.write('<th>Demand</th>');
  document.write('<th>GAIN</th>');
  document.write('</tr>');
  HideSplash;

  ShowSplash;
  SplashProgress('Working...', 0);
  aa := 0;
  for i := 0 to list.Count - 1 do
  begin
    inc(aa);
    document.StartItemId(aa);
    sset := list.Strings[i];
//    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%><img width=64px src=s\' + sset + '.jpg><br>');
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%>' +
                    MakeThumbnailImage2(sset, -1) + '<br>');
    document.write('<a href="sinv/' + sset + '">');
    document.write('<b>' + sset + '</b> - ' + db.SetDesc(sset));
    cls := list.Objects[i] as _costclass;
    document.write('</td><td width=15% align=right>');
    yyy := db.SetYear(list.Strings[i]);
    document.write('<a href=ShowSetsAtYear/%d>%d</a>', [yyy, yyy]);
    document.write('</td><td width=15% align=right>');
    document.write(IntToStr(cls.numparts) + ' parts <br>' + IntToStr(cls.numlots) + ' lots</td>');
    document.write('</td><td width=15% align=right>');
    document.write('€ %2.2f</td>', [cls.setcost]);
    document.write('</td><td width=15% align=right>');
    document.write('€ %2.2f</td>', [cls.invcost]);
    document.write('</td><td width=15% align=right>');
    document.write('%2.2f</td>', [cls.demand]);
    document.write('</td><td width=15% align=right>');
    document.write(Format('%2.3f%s', [dbl_safe_div(cls.invcost, cls.setcost) * 100, '%']) + '</td></tr>');

    if list.Count > 1 then
    begin
      if list.Count < 20 then
        SplashProgress('Working...', i / (list.Count - 1))
      else
        if (i mod 5) = 0 then
          SplashProgress('Working...', i / (list.Count - 1));
    end;
  end;
  document.EndNavigateSection;

  document.write('</table>');

  document.MarkBottomNavigateSection;

  document.write('</p><br><br></p></div></body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  Screen.Cursor := crDefault;

  FreeList(list);
end;

procedure TMainForm.ShowSetsForPartInUsed(const posost: integer);
var
  i: integer;
  list: TStringList;
  setcost: double;
  invcost: double;
  inv: TBrickInventory;
  inv2: TBrickInventory;
  pg: priceguide_t;
  aa: integer;
  cls: _costclass;
  numparts: integer;
  minv: TBrickInventory;
  missing: integer;
  mvalue: double;
  setid: string;
begin
  Screen.Cursor := crHourGlass;

  list := TStringList.Create;

  ShowSplash;
  SplashProgress('Working...', 0);
  for i := 0 to db.AllSets.Count - 1 do
  begin
    if i mod 100 = 0 then
      SplashProgress('Working...', i / db.AllSets.Count);

    if db.SetYear(db.AllSets.Strings[i]) < 1990 then
      continue;

    pg := db.Priceguide(db.AllSets.Strings[i]);
    setcost := pg.uQtyAvgPrice;
    if setcost < 1.0 then
      continue;

    numparts := 0;
    inv := db.GetSetInventory(db.AllSets.Strings[i]);

    invcost := 0.0;

    if inv.numsets > 0 then
    begin
      inv2 := inv.Clone;
      inv2.DismandalAllSets;

      if inv2.EvaluatedPartOutValue_uQtyAvg.percentage < 0.999 then
      begin
        inv2.Free;
        continue;
      end;

      invcost := invcost + inv2.EvaluatedPartOutValue_uQtyAvg.value;
      numparts := numparts + inv2.totallooseparts;
      inv2.Free;
    end
    else
    begin
      if inv.EvaluatedPartOutValue_uQtyAvg.percentage < 0.999 then
        continue;

      invcost := invcost + inv.EvaluatedPartOutValue_uQtyAvg.value;
      numparts := numparts + inv.totallooseparts;
    end;

    if invcost < setcost / (posost / 100) then
      if invcost > 0.10 then
      begin
        cls := _costclass.Create;
        cls.invcost := invcost;
        cls.setcost := setcost;
        cls.numparts := numparts;
        list.AddObject(db.AllSets.Strings[i], cls);
      end;
  end;

  SplashProgress('Working...', 1);
  if domultipagedocuments then
    document.NewMultiPageDocument('ShowSetsForPartInUsed', itoa(posost));

  document.write('<body background="splash.jpg">');
  document.title('Used sets to built from scratch');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine('Used sets to built from scratch');
  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Set</b></th>');
  document.write('<th>Num Pieces</th>');
  document.write('<th>Missing</th>');
  document.write('<th>Set Cost</th>');
  document.write('<th>Part Out value</th>');
  document.write('<th>Missing cost</th>');
  document.write('<th>GAIN</th>');
  document.write('</tr>');
  HideSplash;

  ShowSplash;
  SplashProgress('Working...', 0);
  aa := 0;
  for i := 0 to list.Count - 1 do
  begin
    inc(aa);
    document.StartItemId(aa);
    setid := list.Strings[i];
    minv := inventory.InventoryForMissingToBuildSet(setid, 1);
    mvalue := minv.EvaluatedPartOutValue_uQtyAvg.value;
    minv.Free;
    missing := inventory.MissingToBuildSet(setid);

//    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%><img width=64px src=s\' + setid + '.jpg><br>');
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%>' +
                    MakeThumbnailImage2(setid, -1) + '<br>');
    document.write('<a href="sinv/' + setid + '">');
    document.write('<b>' + setid + '</b> - ' + db.SetDesc(setid));
    cls := list.Objects[i] as _costclass;
    document.write('</td><td width=15% align=right>');
    document.write(IntToStr(cls.numparts) + '</td>');
    document.write('</td><td width=15% align=right>');
    document.write(Format('%d<br>%2.2f%s', [missing, dbl_safe_div(missing, cls.numparts) * 100, '%']) + '</td>');
    document.write('</td><td width=15% align=right>');
    document.write('€ %2.2f</td>', [cls.setcost]);
    document.write('</td><td width=15% align=right>');
    document.write('€ %2.2f</td>', [cls.invcost]);
    document.write('</td><td width=15% align=right>');
    document.write('€ %2.2f<br>%2.2f%s</td>', [mvalue, dbl_safe_div(mvalue, cls.invcost) * 100, '%']);
    document.write('</td><td width=15% align=right>');
    document.write(Format('%2.3f%s', [dbl_safe_div(cls.setcost, cls.invcost) * 100, '%']) + '</td></tr>');

    if list.Count > 1 then
    begin
      if list.Count < 20 then
        SplashProgress('Working...', i / (list.Count - 1))
      else
        if (i mod 5) = 0 then
          SplashProgress('Working...', i / (list.Count - 1));
    end;
  end;
  document.EndNavigateSection;

  document.write('</table>');

  document.MarkBottomNavigateSection;

  document.write('</p><br><br></p></div></body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  Screen.Cursor := crDefault;

  FreeList(list);
end;

type
  _minifigpartoutclass = class(TObject)
    setcost: double;
    invcost: double;
    partscost: double;
    minifigscost: double;
    numparts: integer;
    numlots: integer;
    numminifigs: integer;
    demand: double;
  end;

procedure TMainForm.ShowSetsForPartOutWithMiniFigsNew(const minyear, minavailablelots, minminifignum: integer;
  const mindemand, minpartscostmultiplier, minminifigscostmultiplier, minpartoutmultiplier: double);
var
  i: integer;
  list: TStringList;
  setcost: double;
  invcost: double;
  partscost: double;
  inv: TBrickInventory;
  minv: TBrickInventory;
  inv2: TBrickInventory;
  av: availability_t;
  aa: integer;
  cls: _minifigpartoutclass;
  numparts: integer;
  numlots: integer;
  demand: double;
  numminifigs: integer;
  minifigscost: double;
  yyy: integer;
begin
  Screen.Cursor := crHourGlass;

  list := TStringList.Create;

  ShowSplash;
  SplashProgress('Working...', 0);
  for i := 0 to db.AllSets.Count - 1 do
  begin
    if i mod 100 = 0 then
      SplashProgress('Working...', i / db.AllSets.Count);

    if db.SetYear(db.AllSets.Strings[i]) < minyear then
      continue;

    av := db.Availability(db.AllSets.Strings[i]);
    if av.nTotalLots < minavailablelots then
      continue;
    setcost := av.nQtyAvgPrice;
    invcost := 0.0;

    inv := db.GetSetInventory(db.AllSets.Strings[i]);
    numparts := 0;
    numlots := 0;
    minifigscost := 0;
    numminifigs := 0;
    if inv.numsets > 0 then
    begin
      inv2 := inv.Clone;
      inv2.DismandalAllSets;

      demand := inv2.nDemand.value;
      if demand < mindemand then
      begin
        inv2.Free;
        continue;
      end;

      inv2.Reorganize;
      invcost := invcost + inv2.EvaluatedPartOutValue_nQtyAvg.value;
      if invcost < minpartoutmultiplier * setcost then
      begin
        inv2.Free;
        continue;
      end;

      numparts := numparts + inv2.totallooseparts;
      numlots := numlots + inv2.numlooseparts;
      minv := inv2.Minifigures;
      numminifigs := minv.totallooseparts;
      minifigscost := minv.EvaluatedPartOutValue_nQtyAvg.value;
      minv.Free;
      inv2.Free;
    end
    else
    begin
      demand := inv.nDemand.value;
      if demand < mindemand then
        continue;
      invcost := invcost + inv.EvaluatedPartOutValue_nQtyAvg.value;
      if invcost < minpartoutmultiplier * setcost then
         continue;
      numparts := numparts + inv.totallooseparts;
      numlots := numlots + inv.numlooseparts;
      minv := inv.Minifigures;
      numminifigs := minv.totallooseparts;
      minifigscost := minv.EvaluatedPartOutValue_nQtyAvg.value;
      minv.Free;
    end;

    if numminifigs < minminifignum then
       continue;
    if minifigscost < minminifigscostmultiplier * setcost then
       continue;

    partscost := invcost - minifigscost;
    if partscost < minpartscostmultiplier * setcost then
       continue;

    cls := _minifigpartoutclass.Create;
    cls.invcost := invcost;
    cls.partscost := partscost;
    cls.setcost := setcost;
    cls.numparts := numparts;
    cls.numlots := numlots;
    cls.numminifigs := numminifigs;
    cls.minifigscost := minifigscost;
    cls.demand := demand;
    list.AddObject(db.AllSets.Strings[i], cls);
  end;

  SplashProgress('Working...', 1);
  if domultipagedocuments then
    document.NewMultiPageDocument('ShowSetsForPartOutWithMiniFigsNew', itoa(minyear) + '_' + itoa(minavailablelots) + '_' + itoa(minminifignum) +
      ftoa(mindemand) + '_' + ftoa(minpartscostmultiplier) + '_' + ftoa(minminifigscostmultiplier) + '_' + ftoa(minpartoutmultiplier) + '_' + itoa(list.Count));

  document.write('<body background="splash.jpg">');
  document.title('New sets to buy for minifigures');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine('New sets to buy for minifigures');
  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Set</b></th>');
  document.write('<th>Year</th>');
  document.write('<th>Num Pieces</th>');
  document.write('<th>Set Cost</th>');
  document.write('<th>Part Out value</th>');
  document.write('<th>Parts cost</th>');
  document.write('<th>Minifigures cost</th>');
  document.write('<th>Demand</th>');
  document.write('<th>GAIN</th>');
  document.write('<th>J-value</th>');
  document.write('</tr>');
  HideSplash;

  ShowSplash;
  SplashProgress('Working...', 0);
  aa := 0;
  for i := 0 to list.Count - 1 do
  begin
    inc(aa);
    document.StartItemId(aa);
//    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%><img width=64px src=s\' + list.Strings[i] + '.jpg><br>');
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%>' +
                    MakeThumbnailImage2(list.Strings[i], -1) + '<br>');
    document.write('<a href="sinv/' + list.Strings[i] + '">');
    document.write('<b>' + list.Strings[i] + '</b> - ' + db.SetDesc(list.Strings[i]));
    document.write('</td><td width=8% align=right>');
    yyy := db.SetYear(list.Strings[i]);
    document.write('<a href=ShowSetsAtYear/%d>%d</a>', [yyy, yyy]);
    document.write('</td><td width=15% align=right>');
    cls := list.Objects[i] as _minifigpartoutclass;
    document.write(IntToStr(cls.numparts) + ' parts <br>' + IntToStr(cls.numlots) + ' lots<br>' + IntToStr(cls.numminifigs) + ' minifigs</td>');
    document.write('</td><td width=10% align=right>');
    document.write('€ %2.2f</td>', [cls.setcost]);
    document.write('</td><td width=10% align=right>');
    document.write('€ %2.2f</td>', [cls.invcost]);
    document.write('</td><td width=10% align=right>');
    document.write('€ %2.2f<br>(%2.2f%s)</td>', [cls.partscost, dbl_safe_div(cls.partscost, cls.setcost) * 100, '%']);
    document.write('</td><td width=10% align=right>');
    document.write('€ %2.2f<br>(%2.2f%s)</td>', [cls.minifigscost, dbl_safe_div(cls.minifigscost, cls.setcost) * 100, '%']);
    document.write('</td><td width=10% align=right>');
    document.write('%2.2f</td>', [cls.demand]);
    document.write('</td><td width=10% align=right>');
    document.write(Format('%2.3f%s', [dbl_safe_div(cls.invcost, cls.setcost) * 100, '%']) + '</td>');
    document.write('</td><td width=10% align=right>');
    document.write(Format('%2.3f', [dbl_safe_div(cls.invcost, cls.setcost) * cls.demand]) + '</td></tr>');

    if list.Count > 1 then
    begin
      if list.Count < 20 then
        SplashProgress('Working...', i / (list.Count - 1))
      else
        if (i mod 5) = 0 then
          SplashProgress('Working...', i / (list.Count - 1));
    end;
  end;
  document.EndNavigateSection;

  document.write('</table>');

  document.MarkBottomNavigateSection;

  document.write('</p><br><br></p></div></body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  Screen.Cursor := crDefault;

  FreeList(list);
end;

procedure TMainForm.ShowSetsForPartOutWithMiniFigsUsed(const minyear, minavailablelots, minminifignum: integer;
  const mindemand, minpartscostmultiplier, minminifigscostmultiplier, minpartoutmultiplier: double);
var
  i: integer;
  list: TStringList;
  setcost: double;
  invcost: double;
  partscost: double;
  inv: TBrickInventory;
  minv: TBrickInventory;
  inv2: TBrickInventory;
  av: availability_t;
  aa: integer;
  cls: _minifigpartoutclass;
  numparts: integer;
  numlots: integer;
  demand: double;
  numminifigs: integer;
  minifigscost: double;
  yyy: integer;
begin
  Screen.Cursor := crHourGlass;

  list := TStringList.Create;

  ShowSplash;
  SplashProgress('Working...', 0);
  for i := 0 to db.AllSets.Count - 1 do
  begin
    if i mod 100 = 0 then
      SplashProgress('Working...', i / db.AllSets.Count);

    if db.SetYear(db.AllSets.Strings[i]) < minyear then
      continue;

    av := db.Availability(db.AllSets.Strings[i]);
    if av.uTotalLots < minavailablelots then
      continue;
    setcost := av.uQtyAvgPrice;
    invcost := 0.0;

    inv := db.GetSetInventory(db.AllSets.Strings[i]);
    numparts := 0;
    numlots := 0;
    minifigscost := 0;
    numminifigs := 0;
    if inv.numsets > 0 then
    begin
      inv2 := inv.Clone;
      inv2.DismandalAllSets;

      demand := inv2.uDemand.value;
      if demand < mindemand then
      begin
        inv2.Free;
        continue;
      end;

      inv2.Reorganize;
      invcost := invcost + inv2.EvaluatedPartOutValue_uQtyAvg.value;
      if invcost < minpartoutmultiplier * setcost then
      begin
        inv2.Free;
        continue;
      end;

      numparts := numparts + inv2.totallooseparts;
      numlots := numlots + inv2.numlooseparts;
      minv := inv2.Minifigures;
      numminifigs := minv.totallooseparts;
      minifigscost := minv.EvaluatedPartOutValue_uQtyAvg.value;
      minv.Free;
      inv2.Free;
    end
    else
    begin
      demand := inv.uDemand.value;
      if demand < mindemand then
        continue;

      invcost := invcost + inv.EvaluatedPartOutValue_uQtyAvg.value;
      if invcost < minpartoutmultiplier * setcost then
        continue;

      numparts := numparts + inv.totallooseparts;
      numlots := numlots + inv.numlooseparts;
      minv := inv.Minifigures;
      numminifigs := minv.totallooseparts;
      minifigscost := minv.EvaluatedPartOutValue_uQtyAvg.value;
      minv.Free;
    end;

    if numminifigs < minminifignum then
       continue;
    if minifigscost < minminifigscostmultiplier * setcost then
       continue;

    partscost := invcost - minifigscost;
    if partscost < minpartscostmultiplier * setcost then
       continue;

    cls := _minifigpartoutclass.Create;
    cls.invcost := invcost;
    cls.partscost := partscost;
    cls.setcost := setcost;
    cls.numparts := numparts;
    cls.numlots := numlots;
    cls.numminifigs := numminifigs;
    cls.minifigscost := minifigscost;
    cls.demand := demand;
    list.AddObject(db.AllSets.Strings[i], cls);
  end;

  SplashProgress('Working...', 1);
  if domultipagedocuments then
    document.NewMultiPageDocument('ShowSetsForPartOutWithMiniFigsUsed', itoa(minyear) + '_' + itoa(minavailablelots) + '_' + itoa(minminifignum) +
      ftoa(mindemand) + '_' + ftoa(minpartscostmultiplier) + '_' + ftoa(minminifigscostmultiplier) + '_' + ftoa(minpartoutmultiplier) + '_' + itoa(list.Count));

  document.write('<body background="splash.jpg">');
  document.title('Used sets to buy for minifigures');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine('Used sets to buy for minifigures');
  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Set</b></th>');
  document.write('<th>Year</th>');
  document.write('<th>Num Pieces</th>');
  document.write('<th>Set Cost</th>');
  document.write('<th>Part Out value</th>');
  document.write('<th>Parts cost</th>');
  document.write('<th>Minifigures cost</th>');
  document.write('<th>Demand</th>');
  document.write('<th>GAIN</th>');
  document.write('<th>J-value</th>');
  document.write('</tr>');
  HideSplash;

  ShowSplash;
  SplashProgress('Working...', 0);
  aa := 0;
  for i := 0 to list.Count - 1 do
  begin
    inc(aa);
    document.StartItemId(aa);
//    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%><img width=64px src=s\' + list.Strings[i] + '.jpg><br>');
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%>' +
                    MakeThumbnailImage2(list.Strings[i], -1) + '<br>');
    document.write('<a href="sinv/' + list.Strings[i] + '">');
    document.write('<b>' + list.Strings[i] + '</b> - ' + db.SetDesc(list.Strings[i]));
    document.write('</td><td width=8% align=right>');
    yyy := db.SetYear(list.Strings[i]);
    document.write('<a href=ShowSetsAtYear/%d>%d</a>', [yyy, yyy]);
    document.write('</td><td width=15% align=right>');
    cls := list.Objects[i] as _minifigpartoutclass;
    document.write(IntToStr(cls.numparts) + ' parts <br>' + IntToStr(cls.numlots) + ' lots<br>' + IntToStr(cls.numminifigs) + ' minifigs</td>');
    document.write('</td><td width=10% align=right>');
    document.write('€ %2.2f</td>', [cls.setcost]);
    document.write('</td><td width=10% align=right>');
    document.write('€ %2.2f</td>', [cls.invcost]);
    document.write('</td><td width=10% align=right>');
    document.write('€ %2.2f<br>(%2.2f%s)</td>', [cls.partscost, dbl_safe_div(cls.partscost, cls.setcost) * 100, '%']);
    document.write('</td><td width=10% align=right>');
    document.write('€ %2.2f<br>(%2.2f%s)</td>', [cls.minifigscost, dbl_safe_div(cls.minifigscost, cls.setcost) * 100, '%']);
    document.write('</td><td width=10% align=right>');
    document.write('%2.2f</td>', [cls.demand]);
    document.write('</td><td width=10% align=right>');
    document.write(Format('%2.3f%s', [dbl_safe_div(cls.invcost, cls.setcost) * 100, '%']) + '</td>');
    document.write('</td><td width=10% align=right>');
    document.write(Format('%2.3f', [dbl_safe_div(cls.invcost, cls.setcost) * cls.demand]) + '</td></tr>');

    if list.Count > 1 then
    begin
      if list.Count < 20 then
        SplashProgress('Working...', i / (list.Count - 1))
      else
        if (i mod 5) = 0 then
          SplashProgress('Working...', i / (list.Count - 1));
    end;
  end;
  document.EndNavigateSection;

  document.write('</table>');

  document.MarkBottomNavigateSection;

  document.write('</p><br><br></p></div></body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  Screen.Cursor := crDefault;

  FreeList(list);
end;

procedure TMainForm.ShowSetStatsByNumPieces;
const
  NUMPSTATS = 50;
  NUMPSTEP = 100;
var
  A: array[0..NUMPSTATS] of integer;
  i: integer;
  n: integer;
begin
  Screen.Cursor := crHourGlass;

  for i := 0 to NUMPSTATS do
    A[i] := 0;

  SplashProgress('Working...', 0);
  for i := 0 to db.AllSets.Count - 1 do
  begin
    if i mod 1000 = 0 then
      SplashProgress('Working...', i / db.AllSets.Count / 2);
    n := (db.AllSets.Objects[i] as  TBrickInventory).totallooseparts div NUMPSTEP;
    if n > NUMPSTATS then
      n := NUMPSTATS;
    inc(A[n]);
  end;
  SplashProgress('Working...', 0.5);

  document.write('<body background="splash.jpg">');
  document.title('Sets by num pieces');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine('Sets by num pieces');

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Num pieces</b></th>');
  document.write('<th><b>Num Sets</b></th>');
  document.write('</tr>');

  for i := 0 to NUMPSTATS do
  begin
    SplashProgress('Working...', 0.5 + i / NUMPSTATS / 2);
    document.write('<tr bgcolor=' + TBGCOLOR + '>');
    document.write('<td width=5% align=right>' + IntToStr(i + 1) + '.</td>');

    if i = NUMPSTATS then
      document.write('<td width=55% align=ceneter><a href=ShowSetsPartsBetween/' + itoa(i * NUMPSTEP) + '/' + itoa(MAXINT) + '>' + IntToStr(i * NUMPSTEP) + ' or more</a></td>')
    else
      document.write('<td width=55% align=ceneter><a href=ShowSetStatsByNumPieces2/' + IntToStr(i * NUMPSTEP) + '>' + IntToStr(i * NUMPSTEP) + ' - ' + IntToStr((i + 1) * NUMPSTEP - 1) + '</a></td>');

    document.write('<td width=40% align=ceneter>' + IntToStr(A[i]) + '</td>');
    document.write('</tr>');
  end;

  document.write('</table></p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  Screen.Cursor := crDefault;
end;

type
  statrange_t = record
    start, stop: integer;
    num: integer;
  end;

procedure TMainForm.ShowSetStatsByNumPieces2(const x: integer);
const
  NUMPSTATS2 = 10;
  NUMPSTEP2 = 10;
var
  A: array[0..NUMPSTATS2 - 1] of statrange_t;
  i, j: integer;
  n: integer;
  b: integer;
  tit: string;
  tot: integer;
begin
  Screen.Cursor := crHourGlass;

  ZeroMemory(@A, SizeOf(A));
  A[0].start := x;
  for i := 1 to NUMPSTATS2 - 1 do
  begin
    A[i - 1].stop := A[i - 1].start + NUMPSTEP2 - 1;
    A[i].start := A[i - 1].stop + 1;
  end;
  A[NUMPSTATS2 - 1].stop := A[NUMPSTATS2 - 1].start + NUMPSTEP2 - 1;
  b := A[NUMPSTATS2 - 1].stop;

  SplashProgress('Working...', 0);
  for i := 0 to db.AllSets.Count - 1 do
  begin
    if i mod 1000 = 0 then
      SplashProgress('Working...', i / db.AllSets.Count / 2);
    n := (db.AllSets.Objects[i] as  TBrickInventory).totallooseparts;
    if n >= x then
      if n <= b then
      begin
        for j := 0 to NUMPSTATS2 - 1 do
          if n >= A[j].start then
            if n <= A[j].stop then
            begin
              inc(A[j].num);
              break;
            end;
      end;
  end;
  SplashProgress('Working...', 0.5);

  document.write('<body background="splash.jpg">');
  tit := Format('Sets by num pieces (from %d to %d)', [x, b]);
  document.title(tit);
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine(tit + Format(' (<a href=ShowSetsPartsBetween/%d/%d>Show all</a>)', [x, b]));

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Num pieces</b></th>');
  document.write('<th><b>Num Sets</b></th>');
  document.write('</tr>');

  tot := 0;
  for i := 0 to NUMPSTATS2 - 1 do
  begin
    SplashProgress('Working...', 0.5 + i / NUMPSTATS2 / 2);
    document.write('<tr bgcolor=' + TBGCOLOR + '>');
    document.write('<td width=5% align=right>' + IntToStr(i + 1) + '.</td>');

    document.write('<td width=55% align=ceneter><a href=ShowSetsPartsBetween/' + itoa(A[i].start) + '/' + itoa(A[i].stop) + '>' + IntToStr(A[i].start) + ' - ' + IntToStr(A[i].stop) + '</a></td>');

    document.write('<td width=40% align=ceneter>' + IntToStr(A[i].num) + '</td>');
    tot := tot + A[i].num;
    document.write('</tr>');
  end;

// Footer
  document.write('<tr bgcolor=' + TBGCOLOR + '>');
  document.write('<td width=5% align=right>#</td>');
  document.write('<td width=55% align=ceneter><a href=ShowSetsPartsBetween/' + itoa(A[0].start) + '/' + itoa(A[NUMPSTATS2 - 1].stop) + '><b>Total</b></a></td>');
  document.write('<td width=40% align=ceneter><b>' + IntToStr(tot) + '</b></td>');
  document.write('</tr>');

  document.write('</table></p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowSetStatsByNumLots;
const
  NUMLSTATS = 21;
var
  A: array[0..NUMLSTATS - 1] of statrange_t;
  i, j: integer;
  n: integer;
  tit: string;
begin
  Screen.Cursor := crHourGlass;

  ZeroMemory(@A, SizeOf(A));
  A[0].start := 0;
  A[0].stop := 4;
  A[1].start := 5;
  A[1].stop := 9;
  A[2].start := 10;
  A[2].stop := 14;
  A[3].start := 15;
  A[3].stop := 19;
  A[4].start := 20;
  A[4].stop := 29;
  A[5].start := 30;
  A[5].stop := 39;
  A[6].start := 40;
  A[6].stop := 49;

  A[7].start := 50;
  A[7].stop := 74;
  A[8].start := 75;
  A[8].stop := 99;
  A[9].start := 100;
  A[9].stop := 149;
  A[10].start := 150;
  A[10].stop := 199;
  A[11].start := 200;
  A[11].stop := 249;
  A[12].start := 250;
  A[12].stop := 299;
  A[13].start := 300;
  A[13].stop := 399;
  A[14].start := 400;
  A[14].stop := 499;
  A[15].start := 500;
  A[15].stop := 599;
  A[16].start := 600;
  A[16].stop := 699;
  A[17].start := 700;
  A[17].stop := 799;
  A[18].start := 800;
  A[18].stop := 899;
  A[19].start := 900;
  A[19].stop := 999;
  A[20].start := 1000;
  A[20].stop := MAXINT;

  SplashProgress('Working...', 0);
  for i := 0 to db.AllSets.Count - 1 do
  begin
    if i mod 1000 = 0 then
      SplashProgress('Working...', i / db.AllSets.Count / 2);
    n := (db.AllSets.Objects[i] as TBrickInventory).numlooseparts;
    for j := 0 to NUMLSTATS - 1 do
      if n >= A[j].start then
        if n <= A[j].stop then
        begin
          inc(A[j].num);
          break;
        end;
  end;
  SplashProgress('Working...', 0.5);

  document.write('<body background="splash.jpg">');
  tit := 'Sets by num lots';
  document.title(tit);
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine(tit);

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Num lots</b></th>');
  document.write('<th><b>Num Sets</b></th>');
  document.write('</tr>');

  for i := 0 to NUMLSTATS - 1 do
  begin
    SplashProgress('Working...', 0.5 + i / NUMLSTATS / 2);
    document.write('<tr bgcolor=' + TBGCOLOR + '>');
    document.write('<td width=5% align=right>' + IntToStr(i + 1) + '.</td>');

    if i = NUMLSTATS - 1 then
      document.write('<td width=55% align=ceneter><a href=ShowSetsLotsBetween/' + IntToStr(A[i].start) + '/' + IntToStr(MAXINT) + '>' + IntToStr(A[i].start) + ' or more</a></td>')
    else
      document.write('<td width=55% align=ceneter><a href=ShowSetsLotsBetween/' + IntToStr(A[i].start) + '/' + IntToStr(A[i].stop) + '>' + IntToStr(A[i].start) + ' - ' + IntToStr(A[i].stop) + '</a></td>');

    document.write('<td width=40% align=ceneter>' + IntToStr(A[i].num) + '</td>');
    document.write('</tr>');
  end;

  document.write('</table></p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowMissingFromStorageBins;
var
  inv: TBrickInventory;
  inv2: TBrickInventory;
  missing: integer;
begin
  Screen.Cursor := crHourGlass;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowMissingFromStorageBins', '');

  document.write('<body background="splash.jpg">');
  document.title('Missing from Storage Bins');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  inv2 := db.InventoryForAllStorageBins;
  inv := inventory.InventoryForMissingToBuildInventory(inv2);
  inv2.Free;
  if inv = nil then
  begin
    DrawHeadLine('No pieces found');
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;
  missing := inv.totallooseparts;

  DrawHeadLine(Format('Missing items from storage bins (%d parts)', [missing]));

  DrawInventoryTable(inv);
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  inv.Free;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowCheckStorageReport;
var
  report: TStringList;
  aa, i, j: integer;
  brick: brickpool_t;
  scolor: string;
  spart: string;
  lst: TStringList;
  ncolor: integer;
  nnum: integer;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
begin
  Screen.Cursor := crHourGlass;

  ShowSplash;
  SplashProgress('Working...', 0);

  report := db.CheckStorageReport;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowCheckStorageReport', report.Text);

  document.write('<body background="splash.jpg">');
  document.title('Check Storage Bins For Errors');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  DrawHeadLine('Check Storage Bins For Errors');
  document.write('</p>');
  document.StartNavigateSection;

  document.write('<p align=center>');
  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Part</b></th>');
  document.write('<th>Color</th>');
  document.write('<th>Demand</th>');
  document.write('<th>Num pieces</th>');
  document.write('<th>Storage Information</th>');
  document.write('</tr>');

  aa := 0;
  for i := 0 to report.Count - 1 do
  begin
    if report.Count < 20 then
      SplashProgress('Working...', i / (report.Count))
    else
      if (i mod 5) = 0 then
        SplashProgress('Working...', i / (report.Count));

    lst := report.Objects[i] as TStringList;
    if lst.Count < 2 then
       Continue;
    inc(aa);
    document.StartItemId(aa);
    splitstring(report.Strings[i], spart, scolor, ',');
    ncolor := atoi(scolor);

    pci := db.PieceColorInfo(spart, ncolor, report.Objects[i]);
    pi := db.PieceInfo(pci);

    document.write('<tr bgcolor=' + TBGCOLOR + '>');
    document.write('<td width=5% align=right>' + IntToStr(aa) + '.</td><td width=35%><img width=100px src=' + scolor + '\' + spart + '.png><br><b>');
    document.write('<a href=spiece/' + spart + '>' + spart + '</a></b>');
    document.write(' - ' + db.PieceDesc(pi) + '</td><td width=20%>');
    DrawColorCell(ncolor, 25);
//    document.BlancColorCell(db.colors(ncolor).RGB, 25);

    document.write('<a href=spiecec/' + spart + '/' + scolor + '>' +  db.colors(ncolor).name +
      ' (' + scolor + ') (BL=' + IntToStr(db.colors(ncolor).BrickLingColor) + ')' + GetRebrickableColorHtml(ncolor) + '<img src="images\details.png"></a>' +
      HtmlDrawInvImgLink(spart, ncolor, pi));
    if pci <> nil then
      document.write((decide(pci.setmost = '', '', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + '</td>'))
    else
      document.write('</td>');

    if pci <> nil then
    begin
      document.write('<td width=10% align=right>');
      document.write('N=%2.3f<br>U=%2.3f</td>', [pci.nDemand, pci.uDemand]);
    end
    else
      document.write('<td width=10% align=right></td>');

    nnum := inventory.LoosePartCount(spart, ncolor);
    document.write('<td width=10% align=right>' + IntToStr(nnum));
    document.write('<br><a href=editpiece/' + spart + '/' + scolor + '><img src="images\edit.png"></a>');
    document.write('<br><a href=diagrampiece/' + spart + '/' + scolor + '><img src="images\diagram.png"></a>');
    document.write('</td>');

    document.write('<td width=55%>');
    for j := 0 to lst.Count - 1 do
    begin
      document.write(lst.Strings[j]);
      if j <> lst.Count then
        document.Write('<br>');
    end;
    document.write('</td>');

    document.write('</tr>');

    brick.part := spart;
    brick.color := ncolor;
    brick.num := nnum;
    DrawBrickOrderInfo(@brick);
  end;
  document.EndNavigateSection;

  document.write('</table>');

  document.MarkBottomNavigateSection;

  document.write('</div></body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  FreeList(report);

  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowMissingToBuildSetInventory(const setid: string; const numsets: integer; legacyignore: boolean);
var
  inv, sinv: TBrickInventory;
  missing: integer;
  s1: string;
  nparts: integer;
  legstr: string;
begin
  Screen.Cursor := crHourGlass;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowMissingToBuildSetInventory', setid + '_' + itoa(numsets) + '_' + itoa(intval(legacyignore)));

  document.write('<body background="splash.jpg">');
  document.title('Missing to build set ' + setid);
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  if legacyignore then
  begin
    inv := inventory.InventoryForMissingToBuildSetLegacyIgnore(setid, numsets);
    legstr := 'LegacyIgnore_';
  end
  else
  begin
    inv := inventory.InventoryForMissingToBuildSet(setid, numsets);
    legstr := '';
  end;
  if inv = nil then
  begin
    DrawHeadLine('Can not find missing inventory for ' + setid);
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;
  sinv := db.GetSetInventory(setid);
  if sinv = nil then
  begin
    DrawHeadLine('Can not find inventory for ' + setid);
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  missing := inv.totallooseparts;
  s1 := basedefault + 'out\' + setid + '\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + legstr;
  s1 := s1 + 'missing_' + setid + '_X_' + IntToStrZfill(3, numsets);
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_030%.xml', 0.3);

  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_030%.xml', 0.3);

  if sinv <> nil then
  begin
    DrawHeadLine(Format('<a href="sinv/%s">%s - %s</a><br><br><img width=360px src=s\' + setid + '.jpg>', [setid, setid, db.SetDesc(setid)]));
    if numsets <= 1 then
      DrawHeadLine(Format('%d part' + decide(missing = 1, '', 's') + ' in %d ' + decide(inv.numlooseparts = 1, 'lot', 'lots') + ' ' +
        decide(missing = 1, 'is', 'are') + ' missing to build a copy of this set %s (%2.2f%s)',
          [missing, inv.numlooseparts, setid, dbl_safe_div(100 * missing, sinv.totallooseparts), '%']))
    else
    begin
      nparts := sinv.totallooseparts;
      if nparts > 0 then
        DrawHeadLine(Format('%d part' + decide(missing = 1, '', 's') + ' in %d ' + decide(inv.numlooseparts = 1, 'lot', 'lots') + ' ' +
          decide(missing = 1, 'is', 'are') + ' missing to build %d copies of this set %s (%2.2f%s)',
            [missing, inv.numlooseparts, numsets, setid, missing / nparts / numsets * 100, '%']));
    end;
  end;

  if legacyignore then
  begin
    DrawHeadLine('(Ignoring legacy colors)');
    DrawHeadLine(Format('<a href="missingtobuildsetLI/%s/%d">Check to build %d sets</a>', [setid, numsets + 1, numsets + 1]));
  end
  else
    DrawHeadLine(Format('<a href="missingtobuildset/%s/%d">Check to build %d sets</a>', [setid, numsets + 1, numsets + 1]));

  DrawInventoryTable(inv);
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  inv.Free;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowExpensiveSetLotsNew(const setid: string; const numlots: integer);
var
  sinv, inv: TBrickInventory;
  missing: integer;
  s1: string;
  legstr: string;
  prevstr, nextstr: string;
  headstr: string;
  reallots: integer;
begin
  Screen.Cursor := crHourGlass;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowExpensiveSetLotsNew', setid + '_' + itoa(numlots));

  document.write('<body background="splash.jpg">');
  document.title('Expensive lots of set ' + setid + ' (NEW)');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  sinv := db.GetSetInventory(setid);
  if sinv = nil then
  begin
    DrawHeadLine('Can not find inventory for set ' + setid);
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;


  inv := sinv.InventoryForExpensiveLotsNew(numlots);
  legstr := 'expensivelotsNew_' + itoa(numlots);

  if inv = nil then
  begin
    DrawHeadLine('Can not find inventory for set ' + setid);
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  missing := inv.totallooseparts;
  s1 := basedefault + 'out\' + setid + '\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + legstr;
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_030%.xml', 0.3);

  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_030%.xml', 0.3);

  DrawHeadLine(Format('<a href="sinv/%s">%s - %s</a><br><br><img width=360px src=s\' + setid + '.jpg>', [setid, setid, db.SetDesc(setid)]));

  reallots := numlots;
  if reallots > sinv.numlooseparts then
    reallots := sinv.numlooseparts;

  if reallots > 1 then
  begin
    prevstr := '<a href="ShowExpensiveSetLotsNew/' + setid + '/' + itoa(reallots - 1) + '">' + itoa(reallots - 1) + '</a> - '
  end
  else
    prevstr := '';

  if reallots < sinv.numlooseparts then
  begin
    nextstr := ' - <a href="ShowExpensiveSetLotsNew/' + setid + '/' + itoa(reallots + 1) + '">' + itoa(reallots + 1) + '</a>'
  end
  else
    nextstr := '';


  headstr := Format('%d part' + decide(missing = 1, ' ', 's ') +
    decide(missing = 1, 'is', 'are') + ' in the ' + itoa(inv.numlooseparts) + ' most expensive lot' + decide(missing = 1, '', 's') + ' (New) of set %s (%2.2f%s of parts are the %2.2f%s of value)',
      [missing, setid, dbl_safe_div(100 * missing, db.GetSetInventory(setid).totallooseparts), '%',
                       dbl_safe_div(100 * inv.EvaluatedPartOutValue_nQtyAvg.value, sinv.EvaluatedPartOutValue_nQtyAvg.value), '%']);

  DrawHeadLine(prevstr + headstr + nextstr);

  DrawInventoryTable(inv, False, '', False);
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  inv.Free;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowExpensiveSetLotsUsed(const setid: string; const numlots: integer);
var
  sinv, inv: TBrickInventory;
  missing: integer;
  s1: string;
  legstr: string;
  prevstr, nextstr: string;
  headstr: string;
begin
  Screen.Cursor := crHourGlass;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowExpensiveSetLotsUsed', setid + '_' + itoa(numlots));

  document.write('<body background="splash.jpg">');
  document.title('Expensive lots of set ' + setid + ' (USED)');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  sinv := db.GetSetInventory(setid);
  if sinv = nil then
  begin
    DrawHeadLine('Can not find inventory for set ' + setid);
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;


  inv := sinv.InventoryForExpensiveLotsUsed(numlots);
  legstr := 'expensivelotsUsed_' + itoa(numlots);

  if inv = nil then
  begin
    DrawHeadLine('Can not find inventory for set ' + setid);
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  missing := inv.totallooseparts;
  s1 := basedefault + 'out\' + setid + '\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + legstr;
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_030%.xml', 0.3);

  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_030%.xml', 0.3);

  DrawHeadLine(Format('<a href="sinv/%s">%s - %s</a><br><br><img width=360px src=s\' + setid + '.jpg>', [setid, setid, db.SetDesc(setid)]));

  if numlots > 1 then
  begin
    prevstr := '<a href="ShowExpensiveSetLotsUsed/' + setid + '/' + itoa(numlots - 1) + '">' + itoa(numlots - 1) + '</a> - '
  end
  else
    prevstr := '';

  if numlots < sinv.numlooseparts then
  begin
    nextstr := ' - <a href="ShowExpensiveSetLotsUsed/' + setid + '/' + itoa(numlots + 1) + '">' + itoa(numlots + 1) + '</a>'
  end
  else
    nextstr := '';


  headstr := Format('%d part' + decide(missing = 1, ' ', 's ') +
    decide(missing = 1, 'is', 'are') + ' in the ' + itoa(inv.numlooseparts) + ' most expensive lot' + decide(missing = 1, '', 's') + ' (Used) of set %s (%2.2f%s of parts are the %2.2f%s of value)',
      [missing, setid, dbl_safe_div(100 * missing, db.GetSetInventory(setid).totallooseparts), '%',
                       dbl_safe_div(100 * inv.EvaluatedPartOutValue_uQtyAvg.value, sinv.EvaluatedPartOutValue_uQtyAvg.value), '%']);

  DrawHeadLine(prevstr + headstr + nextstr);

  DrawInventoryTable(inv, False, '', False);
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  inv.Free;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowExpensiveInvNew(const atitle: string; const numlots: integer);
var
  sinv, inv: TBrickInventory;
  missing: integer;
  prevstr, nextstr: string;
  headstr: string;
  reallots: integer;
begin
  Screen.Cursor := crHourGlass;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowExpensiveInvNew', atitle + '_' + itoa(numlots));

  document.write('<body background="splash.jpg">');
  document.title('Expensive lots of my inventory (NEW)');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  sinv := inventory;

  if sinv = nil then
  begin
    DrawHeadLine('Inventory is empty');
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  inv := sinv.InventoryForExpensiveLotsNew(numlots);

  if inv = nil then
  begin
    DrawHeadLine('Inventory is empty');
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  missing := inv.totallooseparts;

  DrawHeadLine(atitle);

  reallots := numlots;
  if sinv.numlooseparts < reallots then
    reallots := sinv.numlooseparts;

  if reallots > 1 then
  begin
    prevstr := '<a href="ShowExpensiveInvNew/' + atitle + '/' + itoa(reallots - 1) + '">' + itoa(reallots - 1) + '</a> - '
  end
  else
    prevstr := '';

  if reallots < sinv.numlooseparts then
  begin
    nextstr := ' - <a href="ShowExpensiveInvNew/' + atitle + '/' + itoa(reallots + 1) + '">' + itoa(reallots + 1) + '</a>'
  end
  else
    nextstr := '';


  headstr := Format('%d part' + decide(missing = 1, ' ', 's ') +
    decide(missing = 1, 'is', 'are') + ' in the ' + itoa(inv.numlooseparts) + ' most expensive lot' + decide(missing = 1, '', 's') + ' of my inventory (NEW) (%2.2f%s of parts are the %2.2f%s of value)',
      [missing, dbl_safe_div(100 * missing, sinv.totallooseparts), '%',
                dbl_safe_div(100 * inv.EvaluatedPartOutValue_nQtyAvg.value, sinv.EvaluatedPartOutValue_nQtyAvg.value), '%']);

  DrawHeadLine(prevstr + headstr + nextstr);

  DrawInventoryTable(inv, False, '', False);
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  inv.Free;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowExpensiveInvUsed(const atitle: string; const numlots: integer);
var
  sinv, inv: TBrickInventory;
  missing: integer;
  prevstr, nextstr: string;
  headstr: string;
  reallots: integer;
begin
  Screen.Cursor := crHourGlass;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowExpensiveInvUsed', atitle + '_' + itoa(numlots));

  document.write('<body background="splash.jpg">');
  document.title('Expensive lots of my inventory (USED)');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  sinv := inventory;

  if sinv = nil then
  begin
    DrawHeadLine('Inventory is empty');
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  inv := sinv.InventoryForExpensiveLotsUsed(numlots);

  if inv = nil then
  begin
    DrawHeadLine('Inventory is empty');
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  missing := inv.totallooseparts;

  DrawHeadLine(atitle);

  reallots := numlots;
  if reallots > sinv.numlooseparts then
    reallots := sinv.numlooseparts;

  if reallots > 1 then
  begin
    prevstr := '<a href="ShowExpensiveInvUsed/' + atitle + '/' + itoa(reallots - 1) + '">' + itoa(reallots - 1) + '</a> - '
  end
  else
    prevstr := '';

  if reallots < sinv.numlooseparts then
  begin
    nextstr := ' - <a href="ShowExpensiveInvUsed/' + atitle + '/' + itoa(reallots + 1) + '">' + itoa(reallots + 1) + '</a>'
  end
  else
    nextstr := '';


  headstr := Format('%d part' + decide(missing = 1, ' ', 's ') +
    decide(missing = 1, 'is', 'are') + ' in the ' + itoa(inv.numlooseparts) + ' most expensive lot' + decide(missing = 1, '', 's') + ' of my inventory (USED) (%2.2f%s of parts are the %2.2f%s of value)',
      [missing, dbl_safe_div(100 * missing, sinv.totallooseparts), '%',
                dbl_safe_div(100 * inv.EvaluatedPartOutValue_uQtyAvg.value, sinv.EvaluatedPartOutValue_uQtyAvg.value), '%']);

  DrawHeadLine(prevstr + headstr + nextstr);

  DrawInventoryTable(inv, False, '', False);
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  inv.Free;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowExpensiveInvPartsNew(const atitle: string; const numlots: integer);
var
  sinv, inv: TBrickInventory;
  missing: integer;
  prevstr, nextstr: string;
  headstr: string;
  reallots: integer;
begin
  Screen.Cursor := crHourGlass;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowExpensiveInvPartsNew', atitle + '_' + itoa(numlots));

  document.write('<body background="splash.jpg">');
  document.title('Expensive parts of my inventory (NEW)');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  sinv := inventory;

  if sinv = nil then
  begin
    DrawHeadLine('Inventory is empty');
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  inv := sinv.InventoryForExpensivePartsNew(numlots);

  if inv = nil then
  begin
    DrawHeadLine('Inventory is empty');
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  missing := inv.totallooseparts;

  DrawHeadLine(atitle);

  reallots := numlots;
  if reallots > sinv.numlooseparts then
    reallots := sinv.numlooseparts;

  if reallots > 1 then
  begin
    prevstr := '<a href="ShowExpensiveInvPartsNew/' + atitle + '/' + itoa(reallots - 1) + '">' + itoa(reallots - 1) + '</a> - '
  end
  else
    prevstr := '';

  if reallots < sinv.numlooseparts then
  begin
    nextstr := ' - <a href="ShowExpensiveInvPartsNew/' + atitle + '/' + itoa(reallots + 1) + '">' + itoa(reallots + 1) + '</a>'
  end
  else
    nextstr := '';


  headstr := Format(itoa(inv.numlooseparts) + ' most expensive part' + decide(missing = 1, '', 's') + ' of my inventory (NEW) (Total qty=%d - %2.2f%s of parts are the %2.2f%s of value)',
      [missing, dbl_safe_div(100 * missing, sinv.totallooseparts), '%',
                dbl_safe_div(100 * inv.EvaluatedPartOutValue_nQtyAvg.value, sinv.EvaluatedPartOutValue_nQtyAvg.value), '%']);

  DrawHeadLine(prevstr + headstr + nextstr);

  DrawInventoryTable(inv, False, '', False);
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  inv.Free;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowExpensiveInvPartsUsed(const atitle: string; const numlots: integer);
var
  sinv, inv: TBrickInventory;
  missing: integer;
  prevstr, nextstr: string;
  headstr: string;
  reallots: integer;
begin
  Screen.Cursor := crHourGlass;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowExpensiveInvPartsUsed', atitle + '_' + itoa(numlots));

  document.write('<body background="splash.jpg">');
  document.title('Expensive parts of my inventory (USED)');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  sinv := inventory;

  if sinv = nil then
  begin
    DrawHeadLine('Inventory is empty');
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  inv := sinv.InventoryForExpensivePartsUsed(numlots);

  if inv = nil then
  begin
    DrawHeadLine('Inventory is empty');
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  missing := inv.totallooseparts;

  DrawHeadLine(atitle);

  reallots := numlots;
  if reallots > sinv.numlooseparts then
    reallots := sinv.numlooseparts;

  if reallots > 1 then
  begin
    prevstr := '<a href="ShowExpensiveInvPartsUsed/' + atitle + '/' + itoa(reallots - 1) + '">' + itoa(reallots - 1) + '</a> - '
  end
  else
    prevstr := '';

  if reallots < sinv.numlooseparts then
  begin
    nextstr := ' - <a href="ShowExpensiveInvPartsUsed/' + atitle + '/' + itoa(reallots + 1) + '">' + itoa(reallots + 1) + '</a>'
  end
  else
    nextstr := '';


  headstr := Format(itoa(inv.numlooseparts) + ' most expensive part' + decide(missing = 1, '', 's') + ' of my inventory (USED) (Total qty=%d - %2.2f%s of parts are the %2.2f%s of value)',
      [missing, dbl_safe_div(100 * missing, sinv.totallooseparts), '%',
                dbl_safe_div(100 * inv.EvaluatedPartOutValue_uQtyAvg.value, sinv.EvaluatedPartOutValue_uQtyAvg.value), '%']);

  DrawHeadLine(prevstr + headstr + nextstr);

  DrawInventoryTable(inv, False, '', False);
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  inv.Free;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowBigInvLots(const lotparts: integer);
var
  sinv, inv: TBrickInventory;
  prevstr, nextstr: string;
  headstr: string;
  displparts: integer;
  atitle: string;
begin
  Screen.Cursor := crHourGlass;

  atitle := Format('Bigger lots of my inventory (%d part%s or more)', [lotparts, decide(lotparts > 1, 's', '')]);

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowBigInvLots', itoa(lotparts));

  document.write('<body background="splash.jpg">');
  document.title(atitle);
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  sinv := inventory;

  if sinv = nil then
  begin
    DrawHeadLine('Inventory is empty');
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  inv := sinv.InventoryForBigLots(lotparts);

  if inv = nil then
  begin
    DrawHeadLine('Inventory is empty');
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  DrawHeadLine(atitle);

  displparts := lotparts;
  if displparts > sinv.totallooseparts then
    displparts := sinv.totallooseparts;

  if displparts > 1 then
  begin
    prevstr := '<a href="ShowBigInvLots/' + itoa(displparts - 1) + '">' + itoa(displparts - 1) + '</a> - '
  end
  else
    prevstr := '';

  if displparts < sinv.totallooseparts then
  begin
    nextstr := ' - <a href="ShowBigInvLots/' + itoa(displparts + 1) + '">' + itoa(displparts + 1) + '</a>'
  end
  else
    nextstr := '';


  headstr := ' <b>' + itoa(displparts) + '</b> ';

  DrawHeadLine(Trim(prevstr + headstr + nextstr));

  DrawInventoryTable(inv, False, '', False);
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  inv.Free;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowMissingToBuilMultipledSets(const setids: TStringList);
var
  sinv: TBrickInventory;
  i: integer;
  inv: TBrickInventory;
  missing: integer;
  s1: string;
begin
  Screen.Cursor := crHourGlass;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowMissingToBuilMultipledSets', decide(setids = nil, 'nil', setids.Text));

  document.write('<body background="splash.jpg">');
  document.title('Missing to build multiple sets');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  sinv := TBrickInventory.Create;
  for i := 0 to setids.Count - 1 do
    sinv.MergeWith(db.GetSetInventory(setids.Strings[i]));
  inv := inventory.InventoryForMissingToBuildInventory(sinv);
  if inv = nil then
  begin
    DrawHeadLine('Can not find missing inventory for the given sets');
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    sinv.Free;
    Screen.Cursor := crDefault;
    Exit;
  end;
  missing := inv.totallooseparts;
  if setids = Missingfordismandaledsetslist then
  begin
    s1 := basedefault + 'out\dismandaledsetsquery\';
    if not DirectoryExists(s1) then
      ForceDirectories(s1);
    sinv.SaveLooseParts(s1 + 'all_parts.txt');
    s1 := s1 + 'missing_dismandaledsetsquery';
    inv.SaveLooseParts(s1 + '.txt');
    setids.SaveToFile(s1 + '_sets.txt');
  end
  else
  begin
    s1 := basedefault + 'out\multisetsquery\';
    if not DirectoryExists(s1) then
      ForceDirectories(s1);
    sinv.SaveLooseParts(s1 + 'all_parts.txt');
    s1 := s1 + 'missing_multisetsquery';
    inv.SaveLooseParts(s1 + '.txt');
    setids.SaveToFile(s1 + '_sets.txt');
  end;
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_030%.xml', 0.3);

  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_030%.xml', 0.3);

  s1 := '';
  for i := 0 to setids.Count - 1 do
    s1 := s1 + '<br><a href="sinv/' + setids.Strings[i] + '">' + setids.Strings[i] + ' - ' + db.SetDesc(setids.Strings[i]) + '</a>';
  DrawHeadLine(Format('%d parts in %d lots are missing to build the following sets (%2.2f%s), total parts = %d<br>%s',
    [missing, inv.numlooseparts, dbl_safe_div(missing * 100, sinv.totallooseparts), '%', sinv.totallooseparts, s1]));

  DrawHeadLine('<a href=multysetinv/' + IntToStr(Integer(setids)) + '>Show inventory of the above sets</a>');

  DrawInventoryTable(inv);
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  inv.Free;
  sinv.Free;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowInventoryForMultipledSets(const setids: TStringList);
var
  inv: TBrickInventory;
  sinv: TBrickInventory;
  minv: TBrickInventory;
  i: integer;
  s1: string;
  nlots: integer;
begin
  Screen.Cursor := crHourGlass;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowInventoryForMultipledSets', decide(setids = nil, 'nil', setids.Text));

  document.write('<body background="splash.jpg">');
  document.title('Inventory for multiple sets');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  inv := TBrickInventory.Create;
  nlots := 0;
  for i := 0 to setids.Count - 1 do
  begin
    sinv := db.GetSetInventory(setids.Strings[i]);
    if sinv <> nil then
    begin
      inv.MergeWith(sinv);
      nlots := nlots + sinv.numlooseparts;
    end;
  end;
  if inv = nil then
  begin
    DrawHeadLine('Can not find inventory for the given sets');
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;
  inv.Reorganize;

  s1 := '';
  for i := 0 to setids.Count - 1 do
    s1 := s1 + '<br><a href="sinv/' + setids.Strings[i] + '">' + setids.Strings[i] + ' - ' + db.SetDesc(setids.Strings[i]) + '</a>';
  DrawHeadLine(Format('Inventory for multiple sets<br>%d parts in %d lots (%d unique)<br>%s',
    [inv.totallooseparts, nlots, inv.numlooseparts, s1]));

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + DBGCOLOR + '>');
  document.write('<td width=50%>');
  minv := inventory.InventoryForMissingToBuildInventory(inv);
  DrawHeadLine(Format('<a href=multymissing/' + IntToStr(Integer(setids)) +
    '>%d parts in %d lots are missing to build this list</a>', [minv.totallooseparts, minv.numlooseparts]));
  minv.Free;
  document.write('</td><td width=50%>');
  DrawHeadLine('<a href=multysetstorages/' + IntToStr(Integer(setids)) +
    '>Storage locations for these parts</a>');
  document.write('</td></tr></table>');

  DrawInventoryTable(inv, False, '', True, True, nil, True, True);

  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  inv.Free;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowStorageLocationsForMultipledSets(const setids: TStringList; const ppreview: boolean);
var
  inv: TBrickInventory;
  sinv: TBrickInventory;
  i: integer;
  s1: string;
  nlots: integer;
begin
  Screen.Cursor := crHourGlass;

  document.write('<body background="splash.jpg">');
  document.title('Storage Locations for multiple sets inventory');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  inv := TBrickInventory.Create;
  nlots := 0;
  for i := 0 to setids.Count - 1 do
  begin
    sinv := db.GetSetInventory(setids.Strings[i]);
    if sinv <> nil then
    begin
      inv.MergeWith(sinv);
      nlots := nlots + sinv.numlooseparts;
    end;
  end;
  if inv = nil then
  begin
    DrawHeadLine('Can not find inventory for the given sets');
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;
  inv.Reorganize;

  s1 := '';
  for i := 0 to setids.Count - 1 do
    s1 := s1 + '<br><a href="sinv/' + setids.Strings[i] + '">' + setids.Strings[i] + ' - ' + db.SetDesc(setids.Strings[i]) + '</a>';
  if not ppreview then
    s1 := s1 + ' <a href=multysetstoragespv/' + IntToStr(Integer(setids)) + '><img src="images\print.png"></a>';

  DrawHeadLine(Format('<a href=multysetinv/' + IntToStr(Integer(setids)) + '>Inventory for multiple sets</a><br>%d parts in %d lots (%d unique)<br>%s',
    [inv.totallooseparts, nlots, inv.numlooseparts, s1]));

  DrawInventoryPartsStorage(inv, ppreview);

  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  inv.Free;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowCompare2Sets(const set1, set2: string);
var
  inv1, inv2: TBrickInventory;
  miss1, miss2: integer;
  missinv1, missinv2: TBrickInventory;
  commoninv: TBrickInventory;
  b: Boolean;
begin
  Screen.Cursor := crHourGlass;

  document.write('<body background="splash.jpg">');
  document.title('Compare 2 sets');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  inv1 := db.GetSetInventoryWithOutExtra(set1);
  inv2 := db.GetSetInventoryWithOutExtra(set2);

  if (inv1 = nil) or (inv2 = nil) then
  begin
    if inv1 = nil then
      DrawHeadLine('Can not find inventory for set ' + set1);
    if inv2 = nil then
      DrawHeadLine('Can not find inventory for set ' + set2);
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  miss1 := inv1.MissingToBuildInventory(inv2);
  miss2 := inv2.MissingToBuildInventory(inv1);
  missinv1 := inv1.InventoryForMissingToBuildInventory(inv2);
  missinv2 := inv2.InventoryForMissingToBuildInventory(inv1);

  b := dodraworderinfo;
  dodraworderinfo := False;

  document.write('<table width=100%><tr valign=top>');

  document.write('<td width=50%>');
  DrawHeadLine(Format('<a href="sinv/%s">%s - %s</a><br><br><img width=240px src=s\' + set1 + '.jpg>', [set1, set1, db.SetDesc(set1)]));
  DrawHeadLine(Format('%d parts in %d lots are missing from this set to build set %s (%2.2f%s)',
//    [miss1, missinv1.numlooseparts, set2, miss1/db.GetSetInventoryWithOutExtra(set2).totallooseparts*100, '%']));
    [miss1, missinv1.numlooseparts, set2, dbl_safe_div(miss1 * 100, inv2.totallooseparts), '%']));
  DrawInventoryTableNoPages(missinv1, True);
  document.write('</td>');

  document.write('<td width=50%>');
  DrawHeadLine(Format('<a href="sinv/%s">%s - %s</a><br><br><img width=240px src=s\' + set2 + '.jpg>', [set2, set2, db.SetDesc(set2)]));
  DrawHeadLine(Format('%d parts in %d lots are missing from this set to build set %s (%2.2f%s)',
//    [miss2, missinv2.numlooseparts, set1, miss2/db.GetSetInventoryWithOutExtra(set1).totallooseparts*100, '%']));
    [miss2, missinv2.numlooseparts, set1, dbl_safe_div(miss2 * 100, inv1.totallooseparts), '%']));
  DrawInventoryTableNoPages(missinv2, True);
  document.write('</td></tr></table>');

  document.write('<br>');

  missinv1.Free;
  missinv2.Free;
  dodraworderinfo := b;

  commoninv := inv1.CommonInventory(inv2);

  document.write('<table width=100%><tr valign=top>');

  document.write('<td width=100%>');
  DrawHeadLine(
    Format('<a href="common2sets/%s/%s">Common inventory</a> for sets <a href="sinv/%s">%s</a> and  <a href="sinv/%s">%s</a>',
      [set1, set2, set1, set1, set2, set2])
  );

  DrawHeadLine(Format('%d parts in %d lots common parts',
    [commoninv.totallooseparts, commoninv.numlooseparts]));
  DrawInventoryTableNoPages(commoninv, True);
  commoninv.Free;

  document.write('</td>');

  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowCommon2Sets(const set1, set2: string);
var
  inv1, inv2: TBrickInventory;
  commoninv: TBrickInventory;
begin
  Screen.Cursor := crHourGlass;

  document.write('<body background="splash.jpg">');
  document.title('Common inventory for ' + set1 + ' ' + set2);
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  inv1 := db.GetSetInventoryWithOutExtra(set1);
  inv2 := db.GetSetInventoryWithOutExtra(set2);

  if (inv1 = nil) or (inv2 = nil) then
  begin
    if inv1 = nil then
      DrawHeadLine('Can not find inventory for set ' + set1);
    if inv2 = nil then
      DrawHeadLine('Can not find inventory for set ' + set2);
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  commoninv := inv1.CommonInventory(inv2);

  document.write('<table width=100%><tr valign=top>');

  document.write('<td width=100%>');
  DrawHeadLine(Format('Common inventory for: <br><br>' +
  ' <a href="sinv/%s">%s - %s</a><br><br><img width=240px src=s\' + set1 + '.jpg><br><br>' +
  ' <a href="sinv/%s">%s - %s</a><br><br><img width=240px src=s\' + set2 + '.jpg>',
  [set1, set1, db.SetDesc(set1), set2, set2, db.SetDesc(set2)]));
  DrawHeadLine(Format('%d parts in %d lots common parts',
    [commoninv.totallooseparts, commoninv.numlooseparts]));
  DrawInventoryTableNoPages(commoninv, True);
  document.write('</td>');

  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  commoninv.Free;
  Screen.Cursor := crDefault;
end;

function TMainForm.CheckAA(const AA, fromAA, toAA: integer): boolean;
begin
  Result := (AA >= fromAA) and (AA <= toAA);
end;

procedure TMainForm.ShowLooseParts(inv: TBrickInventory;
  colormask: Integer = -1; partmask: string = ''; cat: Integer = -1;
      const fromAA: integer = -1; const toAA: integer = MAXINT);
var
  aa, i: integer;
  brick: brickpool_p;
  pci: TPieceColorInfo;
  prn, pru: double;
  prnt, prut: double;
  mycost: Double;
  mycosttot: Double;
  mytotcostpieces: integer;
  cl: TDNumberList;
  pl: TStringList;
  num: integer;
  pi: TPieceInfo;
  totalweight: double;
  totalcostwn: double;
  totalcostwu: double;
  invs: string;
  scolor: string;
  looseparts: boolean;
  www: double;
begin
  UpdateDismantaledsetsinv;

  if inv = nil then
  begin
    inv := inventory;
    looseparts := True;
  end
  else
    looseparts := inv = inventory;

  if looseparts then
  begin
    if not DirectoryExists(basedefault + 'out\looseparts\') then
      ForceDirectories(basedefault + 'out\looseparts\');
    inv.StoreHistoryStatsRec(basedefault + 'out\looseparts\looseparts.stats');
    inv.StoreHistoryEvalRec(basedefault + 'out\looseparts\looseparts.ieval');
  end;

  Screen.Cursor := crHourGlass;
  SortInventory(inv);

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowLooseParts' + itoa(Integer(inv)), itoa(colormask) + '_' + partmask + '_' + itoa(cat));

  document.write('<body background="splash.jpg">');
  document.title('My loose parts');
  DrawNavigateBar;
  document.write('<div style="color:#' + DFGCOLOR + '">');
  document.write('<p align=center>');
  if (colormask <> -1) and (cat <> -1) then
    DrawHeadLine('Loose Parts - ' +
      IntToStr(inv.numlotsbycolor(colormask)) + ' lots, ' +
      IntToStr(inv.totalloosepartsbycatcolor(colormask, cat)) + ' parts (filtered)')
  else if colormask <> -1 then
    DrawHeadLine('Loose Parts - ' +
      IntToStr(inv.numlotsbycolor(colormask)) + ' lots, ' +
      IntToStr(inv.totalloosepartsbycolor(colormask)) + ' parts (filtered)')
  else if partmask <> '' then
    DrawHeadLine('Loose Parts - ' +
      IntToStr(inv.numlotsbypart(partmask)) + ' lots, ' +
      IntToStr(inv.totalloosepartsbypart(partmask)) + ' parts (filtered)')
  else if cat <> -1 then
    DrawHeadLine('Loose Parts - ' +
      IntToStr(inv.numlotsbycategory(cat)) + ' lots, ' +
      IntToStr(inv.totalloosepartsbycategory(cat)) + ' parts (filtered)')
  else
  begin
    DrawHeadLine('Loose Parts - ' +
      IntToStr(inv.numlooseparts) + ' lots, ' +
      IntToStr(inv.totallooseparts) + ' parts');
    if looseparts then
      DrawHeadLine('Inventory Statistics <a href="diagramstorage/Loose Parts"><img src="images\diagram.png"></a>');

    DrawPartOutValueWithOutSets(inv);
  end;

  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Part</b></th>');
  document.write('<th>Color</th>');
  document.write('<th>Demand</th>');
  document.write('<th>Num pieces</th>');
  document.write('<th>N</th>');
  document.write('<th>U</th>');
  document.write('<th>Cost</th>');
  document.write('</tr>');

  ShowSplash;
  SplashProgress('Working...', 0);
  brick := @inv.looseparts[0];
  aa := 0;
  prnt := 0;
  prut := 0;
  num := 0;
  mycosttot := 0.0;
  mytotcostpieces := 0;
  cl := TDNumberList.Create;
  pl := TStringList.Create;
  totalweight := 0.0;
  totalcostwn := 0.0;
  totalcostwu := 0.0;
  invs := IntToStr(Integer(inv));
  for i := 0 to inv.numlooseparts - 1 do
  begin
    if ((colormask = -1) or (colormask = brick.color)) and
       ((partmask = '') or (partmask = brick.part)) and
       ((cat = -1) or (cat = db.PieceInfo(brick.part).category)) then
    begin
      inc(aa);
      document.StartItemId(aa);
      scolor := itoa(brick.color);

      pci := db.PieceColorInfo(brick);
      pi := db.PieceInfo(pci);

      document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=35%><img width=100px src=' + scolor + '\' + brick.part + '.png><br>');
      document.write('<b><a href=spiece/' + brick.part + '>' + brick.part + '</a></b>');
      document.write('<a href="inv/' + invs +'/P/' + decide(partmask = '', brick.part, '') + '">' + ' - ' );
      document.write(db.PieceDesc(pi) + '</a> <a href=spiece/' + brick.part + '>...</a></td><td width=25%>');
      DrawColorCell(brick.color, 20);
  //    document.BlancColorCell(db.colors(brick.color).RGB, 20);
      document.write('<a href="inv/' + invs +'/C/' + IntToStr(decide(colormask = -1, brick.color, -1)) + '">');

      document.write(db.colors(brick.color).name + ' (' + scolor + ') (BL=' +
                     IntToStr(db.colors(brick.color).BrickLingColor) +  ')' + GetRebrickableColorHtml(brick.color) + '</a> <a href=spiecec/' +
                     brick.part + '/' + scolor + '><img src="images\details.png"></a>' +  HtmlDrawInvImgLink(brick.part, brick.color, pi));

//      pci := db.PieceColorInfo(brick.part, brick.color);
      if pci = nil then
        document.write('</td>')
      else
        document.write(decide(pci.setmost = '', '', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + '</td>');

      if pci <> nil then
      begin
        document.write('<td width=10% align=right>');
        document.write('N=%2.3f<br>U=%2.3f</td>', [pci.nDemand, pci.uDemand]);
      end
      else
        document.write('<td width=10% align=right></td>');

      document.write('<td width=15% align=right>' + IntToStr(brick.num) +
      '<br><a href=editpiece/' + brick.part + '/' + scolor + '><img src="images\edit.png"></a>' +
      '<br><a href=diagrampiece/' + brick.part + '/' + scolor + '><img src="images\diagram.png"></a>' +
      '</td>');
      if pci <> nil then
      begin
        prn := pci.EvaluatePriceNew;
        pru := pci.EvaluatePriceUsed;
        www := db.GetItemWeight(pci.piece, pci.color, pi);
        document.write('<td width=10% align=right>' +
          Format('€ %2.4f<br>€ %2.4f<br>€ %2.2f / Kgr', [prn, prn * brick.num, dbl_safe_div(prn, www) * 1000]) + '</td>');
        document.write('<td width=10% align=right>' +
          Format('€ %2.4f<br>€ %2.4f<br>€ %2.2f / Kgr', [pru, pru * brick.num, dbl_safe_div(pru, www) * 1000]) + '</td>');
        prnt := prnt + prn * brick.num;
        prut := prut + pru * brick.num;
        if www > 0.0 then
        begin
          totalweight := totalweight + www * brick.num;
          totalcostwn := totalcostwn + prn * brick.num;
          totalcostwu := totalcostwu + pru * brick.num;
        end;
      end;

      mycost := GetItemCostDbl(brick.part, brick.color);
      document.write('<td width=10% align=right>' + Format('€ %2.4f<br>€ %2.4f', [mycost, mycost * brick.num]) + '</td>');
      mycosttot := mycosttot + mycost * brick.num;
      if mycost > 0.0 then
        mytotcostpieces := mytotcostpieces + brick.num;

      document.write('</tr>');
      DrawBrickOrderInfo(brick);

      if cl.IndexOf(brick.color) < 0 then
        cl.Add(brick.color);
      if pl.IndexOf(brick.part) < 0 then
        pl.Add(brick.part);
      num := num + brick.num;
    end;

    Inc(brick);
    if inv.numlooseparts < 20 then
      SplashProgress('Working...', i / (inv.numlooseparts))
    else
      if (i mod 5) = 0 then
        SplashProgress('Working...', i / (inv.numlooseparts));
  end;
  document.EndNavigateSection;

  document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right><b>Total</b></td>');
  document.write('<td width=35%><b>' + IntToStr(pl.Count) + ' unique mold' + decide(pl.Count = 1, '', 's') + '</b></td>');
  document.write('<td width=20%><b>' + IntToStr(cl.Count) + ' unique color' + decide(cl.Count = 1, '', 's') + '</b></td>');
  document.write('<td width=10% align=right>');
  document.write('N=%2.3f<br>U=%2.3f</td>', [inv.nDemand.value, inv.uDemand.value]);
  document.write('<td width=10% align=right><b>' + IntToStr(num)  + '<br>' + Format('%2.3f Kgr', [totalweight / 1000]) + '</b></td>');
  document.write('<td width=10% align=right><b>' +
    Format('€ %2.2f<br>€ %2.2f / Kgr', [prnt, dbl_safe_div(totalcostwn, totalweight) * 1000]) + '</b></td>');
  document.write('<td width=10% align=right><b>' +
    Format('€ %2.2f<br>€ %2.2f / Kgr', [prut, dbl_safe_div(totalcostwu, totalweight) * 1000]) + '</b></td>');
  if num = 0 then
    document.write('<td width=10% align=right><b>' +
      Format('€ %2.2f', [mycosttot]) + '</b></td>')
  else
    document.write('<td width=10% align=right><b>' +
      Format('€ %2.2f<br>%2.3f%s', [mycosttot, 100 * mytotcostpieces / num, '%']) + '</b></td>');
  document.write('</tr>');

  cl.Free;
  pl.Free;

  document.write('</tr></table>');

  document.MarkBottomNavigateSection;

  document.write('</p><br><br></p></div></body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  Screen.Cursor := crDefault;
end;

procedure TMainForm.HTMLHotSpotClick(Sender: TObject; const SRC: String;
  var Handled: Boolean);
begin
  HTMLClick(SRC, Handled);
end;

procedure TMainForm.HTMLClick(const SRC1: String; var Handled: Boolean);
var
  idx: integer;
begin
  Screen.Cursor := crHourGlass;
  try
    DecimalSeparator := '.';
    doHTMLClick(SRC1, Handled);
    Caption := Application.Title + ' - ' + HTML.DocumentTitle;
    idx := TabControl1.TabIndex;
    if (idx >= 0) and (idx < TabControl1.Tabs.Count) then
      TabControl1.Tabs.Strings[idx] := HTML.DocumentTitle;
  finally
    Screen.Cursor := crDefault;
  end;
end;

function TMainForm.CanOpenInNewTab(const surl: string): boolean;
var
  s1, s2: string;
begin
  Result := False;

  s1 := UpperCase(Trim(surl));

  if s1 = UpperCase('back') then Exit;
  if s1 = UpperCase('fwd') then Exit;
  if s1 = UpperCase('refresh') then Exit;
  if s1 = UpperCase('refreshpieceall') then Exit;

  splitstring(surl, s1, s2, '/');
  s1 := UpperCase(Trim(s1)) + '/';

  if s1 = UpperCase('editpiece/') then Exit;
  if s1 = UpperCase('DoEditSet/') then Exit;
  if s1 = UpperCase('EditLugbulkPrice/') then Exit;
  if s1 = UpperCase('editmold/') then Exit;
  if s1 = UpperCase('removepiecefromstorageset/') then Exit;
  if s1 = UpperCase('removepiecefromstoragenum/') then Exit;
  if s1 = UpperCase('UpdateSetYearFromNet/') then Exit;
  if s1 = UpperCase('UpdateMoldYearsFromNet/') then Exit;
  if s1 = UpperCase('StoreInventoryStatsRec/') then Exit;
  if s1 = UpperCase('refreshset/') then Exit;
  if s1 = UpperCase('refreshsetlite/') then Exit;
  if s1 = UpperCase('refreshpiece/') then Exit;
  if s1 = UpperCase('refreshpiececat/') then Exit;
  if s1 = UpperCase('refreshpiecefrombricklink/') then Exit;
  if s1 = UpperCase('UpdateItemYearFromDiskCache/') then Exit;
  if s1 = UpperCase('UpdateGearsWithUnknownYearFromDisk/') then Exit;
  if s1 = UpperCase('UpdateAllPartsUnknownYear/') then Exit;
  if s1 = UpperCase('UpdatePartsWithUnknownYearFromDisk/') then Exit;
  if s1 = UpperCase('UpdatePartFromDisk/') then Exit;
  if s1 = UpperCase('refreshpieceorgearfrombricklink/') then Exit;
  if s1 = UpperCase('refreshpieceorgearfromrebrickable/') then Exit;
  if s1 = UpperCase('refreshpieceorgearfromrebrickablenorefresh/') then Exit;
  if s1 = UpperCase('refreshpiecefrombricklinkalias/') then Exit;
  if s1 = UpperCase('tryrefreshpiecefrombricklinkalias/') then Exit;
  if s1 = UpperCase('refreshpiecefrombricklinknorefresh/') then Exit;
  if s1 = UpperCase('refreshgearfrombricklinknorefresh/') then Exit;
  if s1 = UpperCase('refreshbookfrombricklinknorefresh/') then Exit;
  if s1 = UpperCase('refreshpiecefrombricklinkaliasnorefresh/') then Exit;
  if s1 = UpperCase('tryrefreshpiecefrombricklinkaliasnorefresh/') then Exit;
  if s1 = UpperCase('tryrefreshpiecerebrickable/') then Exit;
  if s1 = UpperCase('tryrefreshpiecerebrickablenorefresh/') then Exit;
  if s1 = UpperCase('UpdateCatalogFromBricklink/') then Exit;
  if s1 = UpperCase('UpdateCatalogFromBricklinknorefresh/') then Exit;
  if s1 = UpperCase('updatepartnamerebrickable/') then Exit;
  if s1 = UpperCase('updatepartnamerebrickablenorefresh/') then Exit;
  if s1 = UpperCase('UpdateSetAsPartFromBricklink/') then Exit;
  if s1 = UpperCase('UpdateSetAsPartFromBricklinknorefresh/') then Exit;
  if s1 = UpperCase('UpdateMinifigAsPartFromBricklink/') then Exit;
  if s1 = UpperCase('UpdateMinifigAsPartFromBricklinknorefresh/') then Exit;
  if s1 = UpperCase('refreshminifigcat/') then Exit;
  if s1 = UpperCase('refreshpiece100/') then Exit;
  if s1 = UpperCase('refreshpiece1000/') then Exit;
  if s1 = UpperCase('addset/') then Exit;
  if s1 = UpperCase('AddNewSetAsPiece/') then Exit;
  if s1 = UpperCase('DoAddNewSetAsPiece/') then Exit;
  if s1 = UpperCase('RefreshUnKnownSetsCategoryAndWeight/') then Exit;
  if s1 = UpperCase('RefreshUnKnownPiecesCategory/') then Exit;
  if s1 = UpperCase('RefreshUnKnownPiecesWeight/') then Exit;
  if s1 = UpperCase('RefreshUnKnownMinifigCategory/') then Exit;
  if s1 = UpperCase('RefreshUnKnownMinifigWeight/') then Exit;
  if s1 = UpperCase('RefreshUnKnownInventoryPiecesCategory') then Exit;
  if s1 = UpperCase('downloadset/') then Exit;
  if s1 = UpperCase('downloadgear/') then Exit;
  if s1 = UpperCase('downloadsetnorefresh/') then Exit;
  if s1 = UpperCase('downloadgearnorefresh/') then Exit;
  if s1 = UpperCase('downloadminifig/') then Exit;
  if s1 = UpperCase('downloadminifignorefresh/') then Exit;
  if s1 = UpperCase('downloadsetaltparts/') then Exit;
  if s1 = UpperCase('downloadsetex/') then Exit;
  if s1 = UpperCase('downloadsetandrefresh/') then Exit;
  if s1 = UpperCase('downloadsetandrefreshex/') then Exit;
  if s1 = UpperCase('addsetdismantaled/') then Exit;
  if s1 = UpperCase('UpdatePartInventory/') then Exit;
  if s1 = UpperCase('UpdatePartInventorynorefresh/') then Exit;
  if s1 = UpperCase('DownloadPartInventorynorefresh/') then Exit;
  if s1 = UpperCase('removeset/') then Exit;
  if s1 = UpperCase('UpdateSetAssetsFromBricklink/') then Exit;
  if s1 = UpperCase('UpdateSetAssetsFromBricklinknorefresh/') then Exit;
  if s1 = UpperCase('removesetdismantaled/') then Exit;
  if s1 = UpperCase('diagrampiece/') then Exit;
  if s1 = UpperCase('diagrampieceex/') then Exit;
  if s1 = UpperCase('diagramstorage/') then Exit;
  if s1 = UpperCase('diagramorder/') then Exit;
  if s1 = UpperCase('autofixsticker/') then Exit;
  if s1 = UpperCase('autofixstickernorefresh/') then Exit;
  if s1 = UpperCase('AutoCorrectUnknownPieceYears/') then Exit;
  if s1 = UpperCase('updateinstructionweight/') then Exit;
  if s1 = UpperCase('updateinstructionweightnorefresh/') then Exit;
  if s1 = UpperCase('updateboxweight/') then Exit;
  if s1 = UpperCase('updateboxweightnorefresh/') then Exit;
  if s1 = UpperCase('updateinstructionsfromnet/') then Exit;
  if s1 = UpperCase('updateinstructionsfromnethost/') then Exit;
  if s1 = UpperCase('updateinstructionsfrompdf/') then Exit;
  if s1 = UpperCase('pdfreader/') then Exit;
  if s1 = UpperCase('rotatejpegright/') then Exit;
  if s1 = UpperCase('rotatejpegleft/') then Exit;

  Result := True;
end;

procedure TMainForm.AdjustStreamsSize;
var
  i, num, totalsize: integer;
begin
  // Limit to 300 streams if we have more than 400 streams;
  if streams.Count > 400  then
  begin
    for i := streams.Count - 1 downto 300 do
    begin
      streams.Objects[i].Free;
      streams.Delete(i);
    end;
  end;

  if GetMemoryUsed > 1024 * 1024 * 1024 then
  begin
    num := 2 * streams.Count div 3;
    for i := streams.Count - 1 downto num do
    begin
      streams.Objects[i].Free;
      streams.Delete(i);
    end;
  end;

  totalsize := 0;
  for i := 0 to streams.Count - 1 do
    totalsize := totalsize + (streams.Objects[i] as TMemoryStream).Size;

  if totalsize > 256 * 1024 * 1024 then // 256 MB
  begin
    while (totalsize > 256 * 1024 * 1024) and (streams.Count > 100) do
    begin
      totalsize := totalsize - (streams.Objects[streams.Count - 1] as TMemoryStream).Size;
      streams.Objects[streams.Count - 1].Free;
      streams.Delete(streams.Count - 1);
    end;
  end;
end;

procedure TMainForm.doHTMLClick(const SRC1: String; var Handled: Boolean);
var
  s1, s2, s3, s4, s5, s6, s7: string;
  slink: string;
  stmp: string;
  scrollx, scrolly: integer;
  i: integer;
  idx: integer;
  inv: TBrickInventory;
  tmpfname: string;
  SRC: string;
  PARAMS: string;

  function parseparamS(const QRY: string): string;
  var
    iii: integer;
    parmlist: TStringList;
    sss1, sss2: string;
    check: string;
  begin
    Result := '';
    parmlist := string2stringlist(PARAMS, '&');
    check := UpperCase(QRY);
    for iii := 0 to parmlist.Count - 1 do
    begin
      splitstring(parmlist.Strings[iii], sss1, sss2, '=');
      if UpperCase(sss1) = check then
      begin
        Result := sss2;
        break;
      end;
    end;
    parmlist.Free;
  end;

  function parseparamI(const QRY: string; const default: integer = -1): integer;
  begin
    Result := atoi(parseparamS(QRY), default);
  end;

begin
  AdjustStreamsSize;

  splitstring(SRC1, SRC, PARAMS, '&');

  diskmirror := '';

  scrollx := 0;
  scrolly := 0;
  if SRC <> 'refresh' then
  begin
    if Pos1('editpiece/', SRC) then
    begin
      splitstring(SRC, s1, s2, s3, '/');
      inventory.StorePieceInventoryStatsRec(basedefault + 'cache\' + decide(s3 = '-1', '9999', s3) + '\' + s2 + '.history', s2, atoi(s3));
      if EditPiece(s2, atoi(s3)) then
      begin
        inventory.StorePieceInventoryStatsRec(basedefault + 'cache\' + s3 + '\' + s2 + '.history', s2, atoi(s3));
        btn_SaveClick(nil);
        idx := streams.IndexOf(strupper(s3 + '\' + s2 + '.png'));
        if idx > -1 then  // Clear cache
        begin
          streams.Objects[idx].Free;
          streams.Delete(idx);
        end;
        HTMLClick('refresh', Handled);
      end;
      Handled := True;
      Exit;
    end;

    if Pos1('DoEditSet/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      DoEditSet(s2);
      Handled := True;
      Exit;
    end;

    if Pos1('EditLugbulkPrice/', SRC) then
    begin
      splitstring(SRC, s1, s2, s3, s4, '/');
      if EditLugbulkPrice(s2, atoi(s3), atoi(s4)) then
        HTMLClick('refresh', Handled);
      Handled := True;
      Exit;
    end;

    if Pos1('editmold/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      if EditMold(s2) then
      begin
        btn_SaveClick(nil);
        HTMLClick('refresh', Handled);
      end;
      Handled := True;
      Exit;
    end;

    if Pos1('updateinstructionweight/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      if db.UpdateAssetWeightFromNET(s2, 9997) then
        HTMLClick('refresh', Handled);
      Handled := True;
      Exit;
    end;

    if Pos1('updateinstructionweightnorefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      db.UpdateAssetWeightFromNET(s2, 9997);
      Handled := True;
      Exit;
    end;

    if Pos1('updateboxweight/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      if db.UpdateAssetWeightFromNET(s2, 9998) then
        HTMLClick('refresh', Handled);
      Handled := True;
      Exit;
    end;

    if Pos1('updateboxweightnorefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      db.UpdateAssetWeightFromNET(s2, 9998);
      Handled := True;
      Exit;
    end;

    if Pos1('updateinstructionsfromnet/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      DoUpdateInstructionsFromNet(s2);
      Handled := True;
      Exit;
    end;

    if Pos1('updateinstructionsfromnethost/', SRC) then
    begin
      splitstring(SRC, s1, s2, s3, '/');
      DoUpdateInstructionsFromNetHost(s2, s3);
      Handled := True;
      Exit;
    end;

    if Pos1('updateinstructionsfrompdf/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      DoUpdateInstructionsFromPdf(s2);
      Handled := True;
      Exit;
    end;

    if Pos1('pdfreader/', SRC) then
    begin
      splitstring(SRC, s1, s2, s3, '/');
      s4 := InstructionDir(s2) + s3 + '.pdf';
      ShellExecute(Handle, 'open', PChar(s4), nil, nil, SW_SHOWNORMAL);
      Handled := True;
      Exit;
    end;

    if Pos1('rotatejpegright/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      ChDir(basedefault);
      if RotateJPEGFile90DegreesClockwise(s2) then
      begin
        RemoveImageFromCache(s2);
        HTMLClick('refresh', Handled);
      end;
      Handled := True;
      Exit;
    end;

    if Pos1('rotatejpegleft/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      ChDir(basedefault);
      if RotateJPEGFile90DegreesCounterClockwise(s2) then
      begin
        RemoveImageFromCache(s2);
        HTMLClick('refresh', Handled);
      end;
      Handled := True;
      Exit;
    end;

    if Pos1('UpdateSetYearFromNet/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      if db.UpdateSetYearFromNet(s2) then
        HTMLClick('refresh', Handled);
      Handled := True;
      Exit;
    end;

    if Pos1('UpdateMoldYearsFromNet/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      if db.UpdateMoldYearsFromNet(s2) then
        HTMLClick('refresh', Handled);
      Handled := True;
      Exit;
    end;

    if Pos1('removepiecefromstorageset/', SRC) then
    begin
      splitstring(SRC, s1, s2, s3, s4, s5, '/');
      inventory.StorePieceInventoryStatsRec(basedefault + 'cache\' + decide(s3 = '-1', '9999', s3) + '\' + s2 + '.history', s2, atoi(s3));
      if RemovePieceFromStorageForSet(s2, atoi(s3), s4, s5) then
      begin
        inventory.StorePieceInventoryStatsRec(basedefault + 'cache\' + s3 + '\' + s2 + '.history', s2, atoi(s3));
        btn_SaveClick(nil);
        idx := streams.IndexOf(strupper(s3 + '\' + s2 + '.png'));
        if idx > -1 then  // Clear cache
        begin
          streams.Objects[idx].Free;
          streams.Delete(idx);
        end;
        HTMLClick('refresh', Handled);
      end;
      Handled := True;
      Exit;
    end;

    if Pos1('removepiecefromstoragenum/', SRC) then
    begin
      splitstring(SRC, s1, s2, s3, s4, s5, '/');
      inventory.StorePieceInventoryStatsRec(basedefault + 'cache\' + decide(s3 = '-1', '9999', s3) + '\' + s2 + '.history', s2, atoi(s3));
      if RemovePieceFromStorage(s2, atoi(s3), atoi(s4), s5) then
      begin
        inventory.StorePieceInventoryStatsRec(basedefault + 'cache\' + s3 + '\' + s2 + '.history', s2, atoi(s3));
        btn_SaveClick(nil);
        idx := streams.IndexOf(strupper(s3 + '\' + s2 + '.png'));
        if idx > -1 then  // Clear cache
        begin
          streams.Objects[idx].Free;
          streams.Delete(idx);
        end;
        HTMLClick('refresh', Handled);
      end;
      Handled := True;
      Exit;
    end;

    if Pos1('StoreInventoryStatsRec/', SRC) then
    begin
      splitstring(SRC, s1, s2, s3, '/');
      StoreInventoryStatsRec(s2, s3);
      Handled := True;
      Exit;
    end;

    if Pos1('refreshset/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if db.RefreshSet(s2) then
        HTMLClick('refresh', Handled);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('refreshsetlite/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      if db.RefreshSet(s2, True) then
        HTMLClick('refresh', Handled);
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('refreshpiece/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if db.RefreshPart(s2) then
      begin
        db.UpdatePartInventory(s2, True);
        HTMLClick('refresh', Handled);
      end;
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('refreshpiececat/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if db.RefreshPartCategory(s2) then
        HTMLClick('refresh', Handled);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if SRC = 'UpdateAllPartsUnknownYear' then
    begin
      UpdateAllPartsUnknownYear;
      HTMLClick('refresh', Handled);
      Handled := True;
      Exit;
    end;

    if SRC = 'UpdateAllItemsUnknownDesc' then
    begin
      UpdateAllItemsUnknownDesc;
      HTMLClick('refresh', Handled);
      Handled := True;
      Exit;
    end;

    if SRC = 'UpdateGearsWithUnknownYearFromDisk' then
    begin
      UpdateUnknownYearFromDisk('G');
      HTMLClick('refresh', Handled);
      Handled := True;
      Exit;
    end;

    if SRC = 'UpdatePartsWithUnknownYearFromDisk' then
    begin
      UpdateUnknownYearFromDisk('P');
      HTMLClick('refresh', Handled);
      Handled := True;
      Exit;
    end;

    if Pos1('UpdatePartFromDisk/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      UpdatePartFromDisk(s2);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('UpdateItemYearFromDiskCache/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if db.UpdateItemYearFromDiskCache(s2) then
        HTMLClick('refresh', Handled);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('refreshpieceorgearfrombricklink/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if db.UpdatePartKnownColorsFromBricklink(s2) then
        HTMLClick('refresh', Handled)
      else if db.UpdateGearKnownColorsFromBricklink(s2) then
        HTMLClick('refresh', Handled);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('refreshpieceorgearfromrebrickable/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      s3 := NET_GetBricklinkAlias(s2);
      if s3 <> '' then
      begin
        ShowSplash;
        db.SetMoldName(s2, db.PieceInfo(s2).desc);
        db.SetNewPieceName(s2, s3);
        db.TryUpdatePartNameFromRebrickable(s2);
        if db.UpdatePartKnownColorsFromBricklink(s2) then
          HTMLClick('refresh', Handled)
        else if db.UpdateGearKnownColorsFromBricklink(s2) then
          HTMLClick('refresh', Handled);
        HideSplash;
      end;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('refreshpieceorgearfromrebrickablenorefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      s3 := NET_GetBricklinkAlias(s2);
      if s3 <> '' then
      begin
        ShowSplash;
        db.SetMoldName(s2, db.PieceInfo(s2).desc);
        db.SetNewPieceName(s2, s3);
        db.TryUpdatePartNameFromRebrickable(s2);
        if not db.UpdatePartKnownColorsFromBricklink(s2) then
          db.UpdateGearKnownColorsFromBricklink(s2);
        HideSplash;
      end;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('refreshpiecefrombricklink/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if db.UpdatePartKnownColorsFromBricklink(s2) then
        HTMLClick('refresh', Handled);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('tryrefreshpiecefrombricklinkalias/', SRC) then
    begin
      splitstring(SRC, s1, s2, s3, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      db.SetNewPieceName(s2, s3);
      if db.UpdatePartKnownColorsFromBricklink(s2) then
        HTMLClick('refresh', Handled)
      else
        db.UnSetNewPieceName(s2, s3);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('refreshpiecefrombricklinkalias/', SRC) then
    begin
      splitstring(SRC, s1, s2, s3, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      db.SetNewPieceName(s2, s3);
      if db.UpdatePartKnownColorsFromBricklink(s2) then
        HTMLClick('refresh', Handled);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('refreshpiecefrombricklinknorefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if not db.UpdatePartKnownColorsFromBricklink(s2) then
        ErrorBeep;
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('refreshgearfrombricklinknorefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if not db.UpdateGearKnownColorsFromBricklink(s2) then
        ErrorBeep;
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('refreshbookfrombricklinknorefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if not db.UpdateBookKnownColorsFromBricklink(s2) then
        ErrorBeep;
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('tryrefreshpiecefrombricklinkaliasnorefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, s3, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      s3 := Trim(s3);
//      if length(s3) < 16 then
      begin
        db.SetNewPieceName(s2, s3);
        if not db.UpdatePartKnownColorsFromBricklink(s2) then
        begin
          db.UnSetNewPieceName(s2, s3);
          ErrorBeep;
        end;
      end;
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('tryrefreshpiecerebrickable/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      s3 := db.GetNewPieceName(s2);
//      if length(Trim(s2)) < 16 then
        if (s3 = '') or (s3 = s2) then
        begin
          s3 := NET_GetBricklinkAlias(s2);
          if (s3 <> '') and (s3 <> s2) then
          begin
            db.SetNewPieceName(s2, s3);
            if not db.UpdatePartKnownColorsFromBricklink(s2) then
            begin
              db.UnSetNewPieceName(s2, s3);
              ErrorBeep;
            end
            else
              HTMLClick('refresh', Handled);
          end;
        end;
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('tryrefreshpiecerebrickablenorefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      s3 := db.GetNewPieceName(s2);
//      if length(Trim(s2)) < 16 then
        if (s3 = '') or (s3 = s2) then
        begin
          s3 := NET_GetBricklinkAlias(s2);
          if (s3 <> '') and (s3 <> s2) then
          begin
            db.SetNewPieceName(s2, s3);
            if not db.UpdatePartKnownColorsFromBricklink(s2) then
            begin
              db.UnSetNewPieceName(s2, s3);
              ErrorBeep;
            end;
          end;
        end;
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('refreshpiecefrombricklinkaliasnorefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, s3, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      db.SetNewPieceName(s2, s3);
      if not db.UpdatePartKnownColorsFromBricklink(s2) then
        ErrorBeep;
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('UpdateCatalogFromBricklink/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if db.UpdateCatalogFromBricklink(s2) then
        HTMLClick('refresh', Handled);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('UpdateCatalogFromBricklinknorefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if not db.UpdateCatalogFromBricklink(s2) then
        ErrorBeep;
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('updatepartnamerebrickable/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if db.UpdateNameFromRebrickable(s2) then
        HTMLClick('refresh', Handled);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('updatepartnamerebrickablenorefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if not db.UpdateNameFromRebrickable(s2) then
        ErrorBeep;
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('UpdateSetAsPartFromBricklink/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if db.UpdateSetAsPartFromBricklink(s2) then
        HTMLClick('refresh', Handled);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('UpdateSetAsPartFromBricklinknorefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if not db.UpdateSetAsPartFromBricklink(s2) then
        ErrorBeep;
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('UpdateMinifigAsPartFromBricklink/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if db.UpdateMinifigAsPartFromBricklink(s2) then
        HTMLClick('refresh', Handled);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('UpdateMinifigAsPartFromBricklinknorefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if not db.UpdateMinifigAsPartFromBricklink(s2) then
        ErrorBeep;
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('refreshminifigcat/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if db.RefreshMinifigCategory(s2) then
        HTMLClick('refresh', Handled);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('refreshpiece100/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      idx := db.AllPieces.IndexOf(s2);
      if idx > -1 then
      begin
        for i := idx to idx + 99  do
          db.RefreshPart(db.AllPieces.Strings[i mod db.AllPieces.Count]);
      end;
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('refreshpiece1000/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      idx := db.AllPieces.IndexOf(s2);
      if idx > -1 then
      begin
        for i := idx to idx + 999  do
          db.RefreshPart(db.AllPieces.Strings[i mod db.AllPieces.Count]);
      end;
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if SRC = 'refreshpieceall' then
    begin
      Screen.Cursor := crHourglass;
      ShowSplash;
      for i := 0 to db.AllPieces.Count - 1 do
        db.RefreshPart(db.AllPieces.Strings[i]);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos1('addset/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      inventory.StorePieceInventoryStatsRec(basedefault + 'out\' + s2 + '\' + s2 + '.history', s2, -1);
      inventory.AddSet(s2, False);
      inventory.StorePieceInventoryStatsRec(basedefault + 'out\' + s2 + '\' + s2 + '.history', s2, -1);
      btn_SaveClick(nil);
      HTMLClick('refresh', Handled);
      Handled := True;
      Exit;
    end;

    if Pos1('AddNewSetAsPiece/', SRC) then
    begin
      Screen.Cursor := crHourglass;
      splitstring(SRC, s1, s2, s3, '/');
      AddNewSetAsPiece(s2, s3);
      Handled := True;
      Screen.Cursor := crDefault;
      Exit;
    end;

    if Pos1('DoAddNewSetAsPiece/', SRC) then
    begin
      Screen.Cursor := crHourglass;
      splitstring(SRC, s1, s2, s3, '/');
      DoAddNewSetAsPiece(s2, s3);
      Handled := True;
      Screen.Cursor := crDefault;
      Exit;
    end;

    if Pos1('RefreshUnKnownSetsCategoryAndWeight/', SRC) then
    begin
      Screen.Cursor := crHourglass;
      splitstring(SRC, s1, s2, '/');
      RefreshUnKnownSetsCategoryAndWeight(atoi(s2));
      HTMLClick('refresh', Handled);
      Handled := True;
      Screen.Cursor := crDefault;
      Exit;
    end;

    if Pos1('RefreshUnKnownPiecesCategory/', SRC) then
    begin
      Screen.Cursor := crHourglass;
      splitstring(SRC, s1, s2, '/');
      RefreshUnKnownPiecesCategory(atoi(s2));
      HTMLClick('refresh', Handled);
      Handled := True;
      Screen.Cursor := crDefault;
      Exit;
    end;

    if Pos1('RefreshUnKnownPiecesWeight/', SRC) then
    begin
      Screen.Cursor := crHourglass;
      splitstring(SRC, s1, s2, '/');
      RefreshUnKnownPiecesWeight(atoi(s2));
      HTMLClick('refresh', Handled);
      Handled := True;
      Screen.Cursor := crDefault;
      Exit;
    end;

    if Pos1('RefreshUnKnownMinifigCategory/', SRC) then
    begin
      Screen.Cursor := crHourglass;
      splitstring(SRC, s1, s2, '/');
      RefreshUnKnownMinifigCategory(atoi(s2));
      HTMLClick('refresh', Handled);
      Handled := True;
      Screen.Cursor := crDefault;
      Exit;
    end;

    if Pos1('RefreshUnKnownMinifigWeight/', SRC) then
    begin
      Screen.Cursor := crHourglass;
      splitstring(SRC, s1, s2, '/');
      RefreshUnKnownMinifigWeight(atoi(s2));
      HTMLClick('refresh', Handled);
      Handled := True;
      Screen.Cursor := crDefault;
      Exit;
    end;

    if SRC = 'RefreshUnKnownInventoryPiecesCategory' then
    begin
      Screen.Cursor := crHourglass;
      RefreshUnKnownInventoryPiecesCategory;
      HTMLClick('refresh', Handled);
      Handled := True;
      Screen.Cursor := crDefault;
      Exit;
    end;

    if Pos1('downloadset/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      if db.DownloadSetFromBricklinkNew(s2) then
        HTMLClick('refresh', Handled)
      else
        ErrorBeep;
      Handled := True;
      Exit;
    end;

    if Pos1('downloadgear/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      if db.DownloadSetFromBricklinkNew(s2, 'G') then
        HTMLClick('refresh', Handled)
      else
        ErrorBeep;
      Handled := True;
      Exit;
    end;

    if Pos1('downloadsetnorefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      if not db.DownloadSetFromBricklinkNew(s2) then
        ErrorBeep;
      Handled := True;
      Exit;
    end;

    if Pos1('downloadgearnorefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      if not db.DownloadSetFromBricklinkNew(s2, 'G') then
        ErrorBeep;
      Handled := True;
      Exit;
    end;

    if Pos1('downloadminifig/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      if db.DownloadSetFromBricklinkNew(s2, 'M') then
        HTMLClick('refresh', Handled)
      else
        ErrorBeep;
      Handled := True;
      Exit;
    end;

    if Pos1('downloadminifignorefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      if not db.DownloadSetFromBricklinkNew(s2, 'M') then
        ErrorBeep;
      Handled := True;
      Exit;
    end;

    if Pos1('downloadsetaltparts/', SRC) then
    begin
      Screen.Cursor := crHourglass;
      splitstring(SRC, s1, s2, '/');
      if db.DownloadSetAlternatesFromBricklinkNew(s2) then
        HTMLClick('refresh', Handled)
      else
        ErrorBeep;
      Handled := True;
      Screen.Cursor := crDefault;
      Exit;
    end;

    if Pos1('autofixstickernorefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      if not db.AutoFixSticker(s2) then
        ErrorBeep;
      Handled := True;
      Screen.Cursor := crDefault;
      Exit;
    end;

    if Pos1('autofixsticker/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      if db.AutoFixSticker(s2) then
        HTMLClick('refresh', Handled)
      else
        ErrorBeep;
      Handled := True;
      Screen.Cursor := crDefault;
      Exit;
    end;

    if Pos1('downloadsetex/', SRC) then
    begin
      splitstring(SRC, s1, s2, s3, s4, '/');
      Screen.Cursor := crHourglass;
      if db.DownloadSetFromBricklinkNew(s2) then
      begin
        HTMLClick('refresh', Handled);
        db.UpdateSetInfo(s2, s3, atoi(s4, -1), False);
      end
      else
        ErrorBeep;
      Handled := True;
      Screen.Cursor := crDefault;
      Exit;
    end;

    if Pos1('downloadsetandrefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      if db.DownloadSetFromBricklinkNew(s2) then
      begin
        Screen.Cursor := crHourglass;
        ShowSplash;
        if db.RefreshSet(s2) then
          HTMLClick('refresh', Handled);
        HideSplash;
        Screen.Cursor := crDefault;
      end
      else
        ErrorBeep;
      Handled := True;
      Exit;
    end;

    if Pos1('downloadsetandrefreshex/', SRC) then
    begin
      splitstring(SRC, s1, s2, s3, s4, '/');
      if db.DownloadSetFromBricklinkNew(s2) then
      begin
        Screen.Cursor := crHourglass;
        ShowSplash;
        if db.RefreshSet(s2) then
          HTMLClick('refresh', Handled);
        db.UpdateSetInfo(s2, s3, atoi(s4), False);
        HideSplash;
        Screen.Cursor := crDefault;
      end
      else
        ErrorBeep;
      Handled := True;
      Exit;
    end;

    if Pos1('addsetdismantaled/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      inventory.StorePieceInventoryStatsRec(basedefault + 'out\' + s2 + '\' + s2 + '.history', s2, -1);
      inventory.AddSet(s2, True);
      inventory.StorePieceInventoryStatsRec(basedefault + 'out\' + s2 + '\' + s2 + '.history', s2, -1);
      btn_SaveClick(nil);
      HTMLClick('refresh', Handled);
      Handled := True;
      Exit;
    end;

    if SRC = 'AutoCorrectUnknownPieceYears' then
    begin
      if AutoCorrectUnknownPieceYears then
        HTMLClick('refresh', Handled);
      Handled := True;
      Exit;
    end;

    if Pos1('UpdatePartInventory/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      if db.UpdatePartInventory(s2, False) then
        HTMLClick('refresh', Handled);
      Handled := True;
      Exit;
    end;

    if Pos1('UpdatePartInventorynorefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      db.UpdatePartInventory(s2, False);
      Handled := True;
      Exit;
    end;

    if Pos1('DownloadPartInventorynorefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      db.UpdatePartInventory(s2, True);
      Handled := True;
      Exit;
    end;

    if Pos1('removeset/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      inventory.RemoveSet(s2, False);
      btn_SaveClick(nil);
      HTMLClick('refresh', Handled);
      Handled := True;
      Exit;
    end;

    if Pos1('UpdateSetAssetsFromBricklink/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      UpdateSetAssetsFromBricklink(s2);
      HTMLClick('refresh', Handled);
      Handled := True;
      Exit;
    end;

    if Pos1('UpdateSetAssetsFromBricklinknorefresh/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      UpdateSetAssetsFromBricklink(s2);
      Handled := True;
      Exit;
    end;

    if Pos1('removesetdismantaled/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      inventory.RemoveSet(s2, True);
      btn_SaveClick(nil);
      HTMLClick('refresh', Handled);
      Handled := True;
      Exit;
    end;

    if Pos1('diagrampiece/', SRC) then
    begin
      splitstring(SRC, s1, s2, s3, '/');
      DiagramPiece(s2, atoi(s3));
      Handled := True;
      Exit;
    end;

    if Pos1('diagrampieceex/', SRC) then
    begin
      splitstring(SRC, s1, s2, s3, s4, '/');
      DiagramPiece(s2, atoi(s3), atoi(s4));
      Handled := True;
      Exit;
    end;

    if Pos1('diagramstorage/', SRC) then
    begin
      if SRC = 'diagramstorage/Storage Bins' then
      begin
        Screen.Cursor := crHourglass;
        inv := db.InventoryForAllStorageBins;
        inv.StoreHistoryStatsRec(basedefault + 'storage\storagebins.stats');
        inv.StoreHistoryEvalRec(basedefault + 'storage\storagebins.ieval');
        inv.Free;
        Screen.Cursor := crDefault;
      end;
      splitstring(SRC, s1, s2, '/');
      DiagramStorage(s2);
      Handled := True;
      Exit;
    end;

    if Pos1('diagramorder/', SRC) then
    begin
      splitstring(SRC, s1, s2, '/');
      DiagramOrder(s2);
      Handled := True;
      Exit;
    end;


    if SRC = 'back' then
    begin
      if goback.Count > 1 then
      begin
        slink := goback.Strings[goback.Count - 2];
        scrollx := (goback.Objects[goback.Count - 2] as TScrollPos).x;
        scrolly := (goback.Objects[goback.Count - 2] as TScrollPos).y;
        gofwd.AddObject(goback.Strings[goback.Count - 1], TScrollPos.Create(HTML.HScrollBarPosition, HTML.VScrollBarPosition));
        goback.Objects[goback.Count - 1].Free;
        goback.Delete(goback.Count - 1);
      end
      else
      begin
        Handled := True;
        Exit;
      end;
    end
    else if SRC = 'fwd' then
    begin
      if gofwd.Count > 0 then
      begin
        slink := gofwd.Strings[gofwd.Count - 1];
        scrollx := (gofwd.Objects[gofwd.Count - 1] as TScrollPos).x;
        scrolly := (gofwd.Objects[gofwd.Count - 1] as TScrollPos).y;
        goback.AddObject(slink, TScrollPos.Create(HTML.HScrollBarPosition, HTML.VScrollBarPosition));
        gofwd.Objects[gofwd.Count - 1].Free;
        gofwd.Delete(gofwd.Count - 1);
      end
      else
      begin
        Handled := True;
        Exit;
      end;
    end
    else
    begin
      for i := 0 to gofwd.Count - 1 do
        gofwd.Objects[i].Free;
      gofwd.Clear;
      slink := SRC;
      goback.AddObject(slink, TScrollPos.Create(0, 0));
      if goback.Count > 1 then
      begin
        (goback.Objects[goback.Count - 2] as TScrollPos).x := HTML.HScrollBarPosition;
        (goback.Objects[goback.Count - 2] as TScrollPos).y := HTML.VScrollBarPosition;
      end;
    end;
  end
  else
  begin
    slink := goback.Strings[goback.Count - 1];
    scrollx := HTML.HScrollBarPosition;
    scrolly := HTML.VScrollBarPosition;
  end;

  if not DirectoryExists(basedefault + 'out\') then
    ForceDirectories(basedefault + 'out\');
  if not DirectoryExists(basedefault + 'out\html\') then
    ForceDirectories(basedefault + 'out\html\');
  tmpfname := slink;
  for i := 1 to Length(tmpfname) do
    if tmpfname[i] in ['\', '/', '|', ':', '<', '>'] then
      tmpfname[i] := '_';
  diskmirror := basedefault + 'out\html\' + tmpfname + '.htm';

  Handled := True;
  AddressEdit.Text := slink;
  if Pos1('navigate/', slink) then
  begin
    splitstring(slink, s1, s2, s3, '/');
    Navigate(s2, atoi(s3));
  end
  else if Pos1('inv/', slink) then
  begin
    splitstring(slink, s1, s2, s3, s4, '/');
    if s3 = 'C' then
      ShowLooseParts(TBrickInventory(StrToInt(s2)), StrToInt(s4))
    else if s3 = 'P' then
      ShowLooseParts(TBrickInventory(StrToInt(s2)), -1, s4)
    else if s3 = 'CAT' then
      ShowLooseParts(TBrickInventory(StrToInt(s2)), -1, '', StrToInt(s4));
  end
  else if Pos1('invex/', slink) then
  begin
    splitstring(slink, s1, s2, s3, s4, '/');
    ShowLooseParts(TBrickInventory(StrToInt(s2)), StrToInt(s3), '', StrToInt(s4));
  end
  else if Pos1('lugbulksuggest/', slink) then
  begin
    splitstring(slink, s1, s2, s3, s4, s5, '/');
    ShowLugbulkSuggestions(s2, atoi(s3), atoi(s4), atof(s5));
  end
  else if Pos1('ShowLugbulk/', slink) then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowLugbulk(s2, atoi(s3, -1));
  end
  else if Pos1('lugbulkbstprice/', slink) then
  begin
    splitstring(slink, s1, s2, s3, s4, '/');
    ShowLugbulkBestPrice(s2, atof(s3, -1.0), atoi(s4, -1));
  end
  else if Pos1('lugbulkbstpricelt/', slink) then
  begin
    splitstring(slink, s1, s2, s3, s4, '/');
    ShowLugbulkBestPriceNoBrickOrder(s2, atof(s3, -1.0), atoi(s4, -1));
  end
  else if Pos1('ShowSetsAtYear/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowSetsAtYear(atoi(s2));
  end
  else if slink = 'ShowSetsAtUnknownYear' then
  begin
    ShowSetsAtUnknownYear;
  end
  else if Pos1('multymissing/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowMissingToBuilMultipledSets(TStringList(atoi(s2)));
  end
  else if Pos1('ShowSetsDataCompare/', slink) then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowSetsDataCompare(s3, TStringList(atoi(s2)));
  end
  else if Pos1('multysetinv/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowInventoryForMultipledSets(TStringList(atoi(s2)));
  end
  else if Pos1('multysetstorages/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowStorageLocationsForMultipledSets(TStringList(atoi(s2)), false);
  end
  else if Pos1('multysetstoragespv/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowStorageLocationsForMultipledSets(TStringList(atoi(s2)), true);
  end
  else if slink = 'ShowStorageBins' then
  begin
    ShowStorageBins;
  end
  else if Pos1('storage/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowStorageInventory(s2);
  end
  else if Pos1('sinv/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowSetInventory(s2);
  end
  else if Pos1('sstorageloc/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowSetPartsStorage(s2, false);
  end
  else if Pos1('sstoragelocpv/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowSetPartsStorage(s2, true);
  end
  else if slink = 'catalogparts' then
  begin
    ShowCatalogList('P', -1, -1, false);
  end
  else if slink = 'catalogpartsinv' then
  begin
    ShowCatalogList('PI', -1, -1, false);
  end
  else if slink = 'catalogsets' then
  begin
    ShowCatalogList('S', -1, -1, false);
  end
  else if slink = 'catalogsetsnoinv' then
  begin
    ShowCatalogList('SNI', -1, -1, false);
  end
  else if slink = 'cataloginstructions' then
  begin
    ShowCatalogList('I', -1, -1, false);
  end
  else if slink = 'catalogboxes' then
  begin
    ShowCatalogList('O', -1, -1, false);
  end
  else if slink = 'catalogmocs' then
  begin
    ShowCatalogList('MC', -1, -1, false);
  end
  else if slink = 'cataloggears' then
  begin
    ShowCatalogList('G', -1, -1, false);
  end
  else if slink = 'catalogcatalogs' then
  begin
    ShowCatalogList('C', -1, -1, false);
  end
  else if slink = 'catalogminifigures' then
  begin
    ShowCatalogList('M', -1, -1, false);
  end
  else if slink = 'catalogbooks' then
  begin
    ShowCatalogList('B', -1, -1, false);
  end
  else if Pos1('ShowCatalogList/', slink) then
  begin
    splitstring(slink, s1, s2, s3, s4, '/');
    ShowCatalogList(s2, atoi(s3, -1), atoi(s4, -1), false);
  end
  else if Pos1('ShowCatalogListAll/', slink) then
  begin
    splitstring(slink, s1, s2, s3, s4, '/');
    ShowCatalogList(s2, atoi(s3, -1), atoi(s4, -1), true);
  end
  else if Pos1('PreviewSetInventory/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    PreviewSetInventory(s2);
  end
  else if Pos1('PiecesDiscontinuedAtYear/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    PiecesDiscontinuedAtYear(atoi(s2));
  end
  else if Pos1('MinifigurePiecesDiscontinuedAtYear/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    PiecesDiscontinuedAtYear_Minifigure(atoi(s2));
  end
  else if Pos1('PiecesDiscontinuedAtYearExcludingVariations/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    PiecesDiscontinuedAtYearExcludingVariations(atoi(s2));
  end
  else if Pos1('MoldsFirstAppearedAtYear/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    MoldsYearQry(atoi(s2), True);
  end
  else if Pos1('MoldsDiscontinuedAtYear/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    MoldsYearQry(atoi(s2), False);
  end
  else if Pos1('PiecesNewAtYear/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    PiecesNewAtYear(atoi(s2));
  end
  else if Pos1('MinifigurePiecesNewAtYear/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    PiecesNewAtYear_Minifigure(atoi(s2));
  end
  else if Pos1('PiecesNewAtYearExcludingVariations/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    PiecesNewAtYearExcludingVariations(atoi(s2));
  end
  else if Pos1('ShowUniquePiecesOfMyInventory/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowUniquePiecesOfMyInventory(atoi(s2));
  end
  else if Pos1('ShowMoldsWithNumColors/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowMoldsWithNumColors(atoi(s2));
  end
  else if Pos1('ShowMoldsWithMoreThanColors/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowMoldsWithMoreThanColors(atoi(s2));
  end
  else if Pos1('ShowFamilyMolds/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowFamilyMolds(s2);
  end
  else if Pos1('ShowMoldVariations/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowMoldVariations(s2);
  end
  else if Pos1('ShowPieceAlternates/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowPieceAlternates(s2);
  end
  else if Pos1('ShowPiecePatterns/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowPiecePatterns(s2);
  end
  else if Pos1('ShowPiecePrints/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowPiecePrints(s2);
  end
  else if Pos1('ShowChildMolds/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowChildMolds(s2);
  end
  else if slink = 'Nameswithbothpartandsetcolorindexes' then
  begin
    ShowNameswithbothpartandsetcolorindexes;
  end
  else if Pos1('ShowExpensiveSetLotsNew/', slink) then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowExpensiveSetLotsNew(s2, atoi(s3));
  end
  else if Pos1('ShowExpensiveSetLotsUsed/', slink) then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowExpensiveSetLotsUsed(s2, atoi(s3));
  end
  else if Pos1('ShowExpensiveInvNew/', slink) then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowExpensiveInvNew(s2, atoi(s3));
  end
  else if Pos1('ShowExpensiveInvUsed/', slink) then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowExpensiveInvUsed(s2, atoi(s3));
  end
  else if Pos1('ShowExpensiveInvPartsNew/', slink) then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowExpensiveInvPartsNew(s2, atoi(s3));
  end
  else if Pos1('ShowExpensiveInvPartsUsed/', slink) then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowExpensiveInvPartsUsed(s2, atoi(s3));
  end
  else if Pos1('ShowBigInvLots/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowBigInvLots(atoi(s2));
  end
  else if Pos1('sinvl/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowSetInventory(s2, True);
  end
  else if Pos1('spiece/', slink) then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowPiece(s2, atoi(s3, -1));
  end
  else if Pos1('spiecec/', slink) then
  begin
    splitstring(slink, s1, s2, s3, s4, '/');
    ShowColorPiece(s2, atoi(s3, 0), atoi(s4, -1));
  end
  else if Pos1('spiececlt/', slink) then
  begin
    splitstring(slink, s1, s2, s3, s4, '/');
    ShowColorPiece(s2, atoi(s3, 0), atoi(s4, -1), false);
  end
  else if Pos1('instructions/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowInstructions(s2);
  end
  else if Pos1('spiececinv/', slink) then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowPieceCInventory(s2, atoi(s3, 0));
  end
  else if Pos1('buildset/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    inventory.BuildSet(s2);
    btn_SaveClick(nil);
    ShowSetInventory(s2);
  end
  else if Pos1('dismantleset/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    inventory.DismandalSet(s2);
    btn_SaveClick(nil);
    ShowSetInventory(s2);
  end
  else if Pos1('missingtobuildset/', slink) then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowMissingToBuildSetInventory(s2, StrToIntDef(s3, 1), False);
  end
  else if Pos1('missingtobuildsetLI/', slink) then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowMissingToBuildSetInventory(s2, StrToIntDef(s3, 1), True);
  end
  else if Pos1('PiecesWithDaysToUpdate/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    PiecesWithDaysToUpdate(atoi(s2));
  end
  else if Pos1('PiecesWithDaysToUpdateRange/', slink) then
  begin
    splitstring(slink, s1, s2, s3, '/');
    PiecesWithDaysToUpdateRange(atoi(s2), atoi(s3));
  end
  else if slink = 'PiecesUnknownWeight' then
  begin
    PiecesUnknownWeight;
  end
  else if slink = 'InstructionsUnknownWeight' then
  begin
    InstructionsUnknownWeight;
  end
  else if slink = 'BoxesUnknownWeight' then
  begin
    BoxesUnknownWeight;
  end
  else if Pos1('UsedPiecesbeloweuroKgr/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    UsedPiecesbeloweuroKgr(atoi(s2));
  end
  else if Pos1('NewPiecesbeloweuroKgr/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    NewPiecesbeloweuroKgr(atoi(s2));
  end
  else if Pos1('UsedPiecesaboveeuroKgr/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    UsedPiecesaboveeuroKgr(atoi(s2));
  end
  else if Pos1('NewPiecesaboveeuroKgr/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    NewPiecesaboveeuroKgr(atoi(s2));
  end
  else if Pos1('NewPiecesPriceAbove/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    NewPiecesPriceAbove(atof(s2));
  end
  else if Pos1('UsedPiecesPriceAbove/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    UsedPiecesPriceAbove(atof(s2));
  end
  else if Pos1('NewPiecesPriceAboveEvaluated/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    NewPiecesPriceAboveEvaluated(atof(s2));
  end
  else if Pos1('UsedPiecesPriceAboveEvaluated/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    UsedPiecesPriceAboveEvaluated(atof(s2));
  end
  else if Pos1('PriceGuideQry/', slink) then
  begin
    splitstring(slink, s1, s2, s3, s4, s5, s6, s7, '/');
    PriceGuideQry(atoi(s3), s2, itob(atoi(s4)), itob(atoi(s5)), itob(atoi(s6)), itob(atoi(s7)));
  end
  else if slink = 'NewPiecesCheaperUsed' then
  begin
    NewPiecesCheaperUsed;
  end
  else if Pos1('NewPiecesMuchMoreExpensiveThanUsed/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    NewPiecesMuchMoreExpensiveThanUsed(atof(s2));
  end
  else if slink = 'ShowMySetsPieces' then
  begin
    ShowMySetsPieces;
  end
  else if slink = 'ShowMyMinifiguresMenu' then
  begin
    ShowMyMinifiguresMenu;
  end
  else if Pos1('ShowMyMinifigInventory/', slink) then
  begin
    splitstring(slink, s1, s2, s3, s4, '/');
    ShowMyMinifigInventory(atob(s2), atob(s3), atob(s4));
  end
  else if slink = 'ShowMyMocsPieces' then
  begin
    ShowMyMocsPieces;
  end
  else if slink = 'ShowMyPiecesValue' then
  begin
    ShowMyPiecesValue;
  end
  else if Pos1('order/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowOrder(s2);
  end
  else if Pos1('catcolors/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowCategoryColors(StrToInt(s2));
  end

  else if slink = 'home' then
  begin
    ShowHomePage;
  end
  else if slink = 'cataloghome' then
  begin
    DrawNavigateCatalog;
  end
  else if slink = 'colors' then
  begin
    ShowColors;
  end
  else if slink = 'tags' then
  begin
    ShowTags;
  end
  else if slink = 'categories' then
  begin
    ShowCategories;
  end
  else if slink = 'mysetsandmocs' then
  begin
    ShowMySetsAndMocs;
  end
  else if slink = 'myofficialsets' then
  begin
    ShowMyOfficialSets;
  end
  else if slink = 'mymocs' then
  begin
    ShowMyMocs;
  end
  else if slink = 'dismantleallsets' then
  begin
    Screen.Cursor := crHourGlass;
    inventory.DismandalAllSets;
    Screen.Cursor := crDefault;
    ShowMySetsAndMocs;
  end
  else if slink = 'buildallsets' then
  begin
    Screen.Cursor := crHourGlass;
    inventory.BuildAllSets;
    Screen.Cursor := crDefault;
    ShowMySetsAndMocs;
  end
  else if slink = 'orders' then
  begin
    ShowOrders;
  end
  else if Pos1('sellerorders/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowOrders(s2);
  end
  else if Pos1('showtag/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowTag(s2);
  end
  else if Pos1('showtaginv/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowTagInv(s2);
  end
  else if Pos1('compare2sets/', slink) then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowCompare2Sets(s2, s3);
  end
  else if Pos1('common2sets/', slink) then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowCommon2Sets(s2, s3);
  end
  else if Pos1('setsIcanbuild/', slink) then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowSetsICanBuild(atoi(s2) / atoi(s3), True, True);
  end
  else if Pos1('figsIcanbuild/', slink) then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowSetsICanBuild(atoi(s2) / atoi(s3), False, True);
  end
  else if Pos1('ShowSetsForPartOutNew/', slink) then
  begin
    splitstring(slink, stmp, s1, s2, s3, s4, '/');
    ShowSetsForPartOutNew(atoi(s1), atoi(s2), atoi(s3) / 100, atoi(s4) / 100);
  end
  else if Pos1('ShowSetsForPartOutUsed/', slink) then
  begin
    splitstring(slink, stmp, s1, s2, s3, s4, '/');
    ShowSetsForPartOutUsed(atoi(s1), atoi(s2), atoi(s3) / 100, atoi(s4) / 100);
  end
  else if slink = 'ShowSetStatsByNumLots' then
  begin
    ShowSetStatsByNumLots;
  end
  else if slink = 'ShowSetStatsByNumPieces' then
  begin
    ShowSetStatsByNumPieces;
  end
  else if Pos1('ShowSetStatsByNumPieces2/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowSetStatsByNumPieces2(atoi(s2));
  end
  else if Pos1('ShowSetsLotsBetween/', slink) then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowSetsLotsBetween(atoi(s2), atoi(s3));
  end
  else if Pos1('ShowSetsPartsBetween/', slink) then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowSetsPartsBetween(atoi(s2), atoi(s3));
  end
  else if Pos1('ShowSetsForPartInUsed/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowSetsForPartInUsed(atoi(s2));
  end
  else if Pos1('ShowSetsForPartOutWithMiniFigsNew/', slink) then
  begin
    splitstring(slink, stmp, s1, s2, s3, s4, s5, s6, s7, '/');
    ShowSetsForPartOutWithMiniFigsNew(atoi(s1), atoi(s2), atoi(s3), atoi(s4) / 100, atoi(s5) / 100, atoi(s6) / 100, atoi(s7) / 100);
  end
  else if Pos1('ShowSetsForPartOutWithMiniFigsUsed/', slink) then
  begin
    splitstring(slink, stmp, s1, s2, s3, s4, s5, s6, s7, '/');
    ShowSetsForPartOutWithMiniFigsUsed(atoi(s1), atoi(s2), atoi(s3), atoi(s4) / 100, atoi(s5) / 100, atoi(s6) / 100, atoi(s7) / 100);
  end
  else if Pos1('lengthquery/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowLengthQuery(s2);
  end
  else if Pos1('lengthqueryslopes/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowLengthQuerySlopes(s2);
  end
  else if Pos1('dimentionsquery/', slink) then
  begin
    splitstring(slink, s1, s2, '/');
    ShowDimentionsQuery(s2);
  end
  else if slink = 'ShowMissingFromStorageBins' then
  begin
   ShowMissingFromStorageBins;
  end
  else if slink = 'ShowCheckStorageReport' then
  begin
   ShowCheckStorageReport;
  end
  else
    Handled := False;

  if Handled then
  begin
    HTML.HScrollBarPosition := scrollx;
    HTML.VScrollBarPosition := scrolly;
  end;

end;

procedure TMainForm.HTMLMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  activebits := 20000;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  if activebits > 0 then
    activebits := activebits - Timer1.Interval;
end;

procedure TMainForm.HTMLMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  activebits := 20000;
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.Set1Click(Sender: TObject);
var
  setid: string;
  foo: Boolean;
begin
  setid := lastset;
  if GetSetID(setid) then
    HTMLClick('sinv/' + setid, foo);
end;

procedure TMainForm.Missingformultiplesets1Click(Sender: TObject);
var
  foo: Boolean;
begin
  if GetMultipleSetsList(Missingformultiplesetslist) then
    HTMLClick('multymissing/' + IntToStr(Integer(Missingformultiplesetslist)), foo);
end;

procedure TMainForm.Printpreview1Click(Sender: TObject);
var
  pf: TPreviewForm;
  Abort: boolean;
begin
  pf := TPreviewForm.CreateIt(Self, HTML, Abort);
  try
    if not Abort then
      pf.ShowModal;
  finally
    pf.Free;
  end;
end;

procedure TMainForm.btn_backClick(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('back', foo);
end;


procedure TMainForm.btn_fwdClick(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('fwd', foo);
end;

procedure TMainForm.btn_homeClick(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('home', foo);
end;

procedure TMainForm.Compare2sets1Click(Sender: TObject);
var
  set1, set2: string;
  foo: Boolean;
begin
  if Compare2SetsQuery('Compare sets', set1, set2) then
    HTMLClick('compare2sets/' + set1 + '/' + set2, foo);
end;

procedure TMainForm.Mosaic1Click(Sender: TObject);
var
  frm: TMosaicForm;
  fname: string;
  foo: boolean;
begin
  frm := TMosaicForm.Create(nil);
  frm.ShowModal;
  if frm.ModalResult = mrOK then
  begin
    fname := frm.name;
    if fname <> '' then
    begin
      fname := 'mosaic_' + fname + Format('_%dx%d', [frm.mosaic.width, frm.mosaic.height]);
      frm.mosaic.inv.SaveLooseParts(basedefault + 'mosaic\' + fname + '.txt');
      db.binarysets.UpdateSetFromTextFile(fname, basedefault + 'mosaic\' + fname + '.txt');
      frm.mosaic.Bitmap.SaveToFile(basedefault + 'mosaic\' + fname + '.bmp');
      frm.mosaic.PlaneBitmap.SaveToFile(basedefault + 'mosaic\' + fname + '_plane.bmp');
      SaveBmpToJpeg(frm.mosaic.Bitmap, basedefault + 's\' + fname + '.jpg');
      HTMLClick('sinv/' + fname, foo);
    end;
  end;
  frm.Free;
end;

procedure TMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  activebits := 20000;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  activebits := 20000;
end;

procedure TMainForm.FormDeactivate(Sender: TObject);
begin
  activebits := 10000;
end;

procedure TMainForm.Piece1Click(Sender: TObject);
var
  pieceid: string;
  foo: Boolean;
begin
  pieceid := '';
  if GetPieceID(pieceid) then
    HTMLClick('spiece/' + pieceid, foo);
end;

procedure TMainForm.Sets1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('setsIcanbuild/9/10', foo);
end;

procedure TMainForm.btn_saveClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    inventory.SaveLooseParts(basedefault + 'myparts.txt');
    inventory.SaveSets(basedefault + 'mysets.txt');
    db.SaveStorage;
    S_FlashFileSystem;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.SetstobuyNew1Click(Sender: TObject);
var
  foo: Boolean;
begin
  if QueryPartOutParameters(qpa_minyear, qpa_minavailablelots, qpa_mindemand, qpa_mincostmultiplier) then
    HTMLClick('ShowSetsForPartOutNew/' +
        itoa(qpa_minyear) + '/' +
        itoa(qpa_minavailablelots) + '/' +
        itoa(round(qpa_mindemand * 100)) + '/' +
        itoa(round(qpa_mincostmultiplier * 100)),
      foo);
end;

procedure TMainForm.btn_CopyClick(Sender: TObject);
var
  s: WideString;
begin
  s := HTML.SelText;
  SetClipboardTextWideString(s);
end;

procedure TMainForm.SetstobuyUsed1Click(Sender: TObject);
var
  foo: Boolean;
begin
  if QueryPartOutParameters(qpa_minyear, qpa_minavailablelots, qpa_mindemand, qpa_mincostmultiplier) then
    HTMLClick('ShowSetsForPartOutUsed/' +
        itoa(qpa_minyear) + '/' +
        itoa(qpa_minavailablelots) + '/' +
        itoa(round(qpa_mindemand * 100)) + '/' +
        itoa(round(qpa_mincostmultiplier * 100)),
      foo);
end;

procedure TMainForm.Pieceswithunknownweight1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PiecesUnknownWeight', foo);
end;

procedure TMainForm.PiecesUnknownWeight;
var
  i, j: integer;
  lst: TStringList;
  pcs: string;
  pi: TPieceInfo;
  pci: TPieceColorInfo;
  inv: TBrickInventory;
  s1: string;
  kp: THashStringList;
  titstr: string;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to LASTNORMALCOLORINDEX do
    if db.Colors(i).id = i then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
        for j := 0 to kp.Count - 1 do
        begin
          pcs := kp.Strings[j];
          pci := kp.Objects[j] as TPieceColorInfo;
          pi := db.PieceInfo(pci);
          if pi.Weight = 0.0 then
          begin
            lst.Add(pcs + ',' + itoa(i));
            inv.AddLoosePartFast(pcs, i, 1, pci);
          end;
        end;
    end;

  lst.Sort;
  titstr := 'Pieces with unknown weight';
  DrawPieceList(titstr, lst, SORT_NONE, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\PiecesUnknownWeight\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'PiecesUnknownWeight';
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListUsed(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListUsed(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListUsed(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListUsed(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListUsed(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListUsed(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListUsed(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListUsed(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_030%.xml', 0.3);

  inv.Free;
end;

procedure TMainForm.InstructionsUnknownWeight;
var
  i: integer;
  lst: TStringList;
  inv: TBrickInventory;
  ss: TSetExtraInfo;
  s1, titstr: string;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to db.Sets.Count - 1 do
  begin
    ss := db.Sets.Objects[i] as TSetExtraInfo;
    if ss.hasinstructions then
      if ss.instructionsweight = 0.0 then
      begin
        lst.Add(db.Sets.Strings[i] + ',9997');
        inv.AddLoosePartFast(db.Sets.Strings[i], 9997, 1);
      end;
  end;

  lst.Sort;
  titstr := 'Intructions with unknown weight';
  DrawPieceList(titstr, lst, SORT_NONE, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\InstructionsUnknownWeight\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'InstructionsUnknownWeight';
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListUsed(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListUsed(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListUsed(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListUsed(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListUsed(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListUsed(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListUsed(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListUsed(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_030%.xml', 0.3);

  inv.Free;
end;

procedure TMainForm.BoxesUnknownWeight;
var
  i: integer;
  lst: TStringList;
  inv: TBrickInventory;
  ss: TSetExtraInfo;
  s1, titstr: string;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to db.Sets.Count - 1 do
  begin
    ss := db.Sets.Objects[i] as TSetExtraInfo;
    if ss.hasoriginalbox then
      if ss.originalboxweight = 0.0 then
      begin
        lst.Add(db.Sets.Strings[i] + ',9998');
        inv.AddLoosePartFast(db.Sets.Strings[i], 9998, 1);
      end;
  end;

  lst.Sort;
  titstr := 'Original Boxes with unknown weight';
  DrawPieceList(titstr, lst, SORT_NONE, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\BoxesUnknownWeight\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'BoxesUnknownWeight';
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListUsed(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListUsed(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListUsed(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListUsed(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListUsed(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListUsed(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListUsed(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListUsed(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_030%.xml', 0.3);

  inv.Free;
end;

procedure TMainForm.UsedPiecesbelow30euroKgr1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('UsedPiecesbeloweuroKgr/30', foo);
end;

procedure TMainForm.UsedPiecesbeloweuroKgr(const x: integer);
var
  i, j: integer;
  lst: TStringList;
  pcs: string;
  pi: TPieceInfo;
  pci: TPieceColorInfo;
  weight: double;
  cost: double;
  perkgr: double;
  inv: TBrickInventory;
  s1: string;
  nextstr, prevstr: string;
  nextx, prevx: integer;
  kp: THashStringList;
  titstr: string;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to LASTNORMALCOLORINDEX do
    if db.Colors(i).id = i then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
        for j := 0 to kp.Count - 1 do
        begin
          pcs := kp.Strings[j];
          pci := kp.Objects[j] as TPieceColorInfo;
          if pci <> nil then
          begin
            pi := db.PieceInfo(pci);
            weight := db.GetItemWeight(pci.piece, pci.color, pi);
            if weight > 0.0 then
            begin
              cost := pci.EvaluatePriceUsed;
              if cost > 0.0 then
              begin
                perkgr := dbl_safe_div(cost, weight) * 1000;
                if perkgr <= x then
                begin
                  lst.Add(pcs + ',' + itoa(i));
                  inv.AddLoosePartFast(pcs, i, 1, pci);
                end;
              end;
            end;
          end;
        end;
    end;

  if x > 1 then
  begin
    if x <= 10 then
      prevx := x - 1
    else
      prevx := round(x * 0.8);
    prevstr := '<a href=UsedPiecesbeloweuroKgr/' + itoa(prevx) + '>Used pieces with price below ' + itoa(prevx) + ' euro/Kgr</a><br>';
  end
  else
    prevstr := '';
  if x < 50 then
  begin
    nextx := round(x * 1.25);
    if nextx > 50 then
      nextx := 50;
    nextstr := '<br><a href=UsedPiecesbeloweuroKgr/' + itoa(nextx) + '>Used pieces with price below ' + itoa(nextx) + ' euro/Kgr</a>';
  end
  else
    nextstr := '';

  lst.Sort;
  titstr := 'Used pieces with price below ' + itoa(x) + ' euro/Kgr';
  DrawPieceList(prevstr + titstr + nextstr, lst, SORT_PRICE_USED, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\UsedPiecesbelow' + IntToStrzFill(4, x) + 'euroKgr\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'UsedPiecesbelow' + IntToStrzFill(4, x) + 'euroKgr';
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListUsed(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListUsed(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListUsed(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListUsed(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListUsed(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListUsed(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListUsed(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListUsed(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_030%.xml', 0.3);

  inv.Free;
end;

procedure TMainForm.NewPiecesbeloweuroKgr(const x: integer);
var
  i, j: integer;
  lst: TStringList;
  pcs: string;
  pi: TPieceInfo;
  pci: TPieceColorInfo;
  weight: double;
  cost: double;
  perkgr: double;
  inv: TBrickInventory;
  s1: string;
  nextstr, prevstr: string;
  nextx, prevx: integer;
  kp: THashStringList;
  titstr: string;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to LASTNORMALCOLORINDEX do
    if db.Colors(i).id = i then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
        for j := 0 to kp.Count - 1 do
        begin
          pcs := kp.Strings[j];
          pci := kp.Objects[j] as TPieceColorInfo;
          if pci <> nil then
          begin
            pi := db.PieceInfo(pci);
            weight :=  db.GetItemWeight(pci.piece, pci.color, pi);
            if weight > 0.0 then
            begin
              if pci.priceguide.nTimesSold > 0 then
              begin
                cost := pci.EvaluatePriceNew;
                if cost > 0.0 then
                begin
                  perkgr := dbl_safe_div(cost, weight) * 1000;
                  if perkgr <= x then
                  begin
                    lst.Add(pcs + ',' + itoa(i));
                    inv.AddLoosePartFast(pcs, i, 1, pci);
                  end;
                end;
              end;
            end;
          end;
        end;
    end;

  if x > 1 then
  begin
    if x <= 10 then
      prevx := x - 1
    else
      prevx := round(x * 0.8);
    prevstr := '<a href=NewPiecesbeloweuroKgr/' + itoa(prevx) + '>New pieces with price below ' + itoa(prevx) + ' euro/Kgr</a><br>';
  end
  else
    prevstr := '';
  if x < 50 then
  begin
    nextx := round(x * 1.25);
    if nextx > 50 then
      nextx := 50;
    nextstr := '<br><a href=NewPiecesbeloweuroKgr/' + itoa(nextx) + '>New pieces with price below ' + itoa(nextx) + ' euro/Kgr</a>';
  end
  else
    nextstr := '';

  lst.Sort;
  titstr := 'New pieces with price below ' + itoa(x) + ' euro/Kgr';
  DrawPieceList(prevstr + titstr + nextstr, lst, SORT_PRICE_NEW, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\NewPiecesbelow' + IntToStrzFill(4, x) + 'euroKgr\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'NewPiecesbelow' + IntToStrzFill(4, x) + 'euroKgr';
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListUsed(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListUsed(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListUsed(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListUsed(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListUsed(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListUsed(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListUsed(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListUsed(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_030%.xml', 0.3);

  inv.Free;

end;

procedure TMainForm.UsedPiecesbelow10euroKgr1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('UsedPiecesbeloweuroKgr/10', foo);
end;

procedure TMainForm.UsedPiecesbelow15euroKgr1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('UsedPiecesbeloweuroKgr/15', foo);
end;

procedure TMainForm.UsedPiecesbelow20euroKgr1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('UsedPiecesbeloweuroKgr/20', foo);
end;

procedure TMainForm.UsedPiecesbelow25euroKgr1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('UsedPiecesbeloweuroKgr/25', foo);
end;

procedure TMainForm.ShowMySetsPieces;
var
  inv: TBrickInventory;
  i, j: integer;
begin
  Screen.Cursor := crHourGlass;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowMySetsPieces', itoa(inventory.numsets));

  document.write('<body background="splash.jpg">');
  document.title('Pieces of my sets');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  inv := TBrickInventory.Create;
  for i := 0 to inventory.numsets - 1 do
    for j := 1 to inventory.sets[i].num do
      inv.AddSet(inventory.sets[i].setid, False);
  inv.DismandalAllSets;
  SortInventory(inv);

  DrawHeadLine('My builded sets Inventory');

  DrawInventoryTable(inv, False);
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  inv.Free;
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  Screen.Cursor := crDefault;

end;

procedure TMainForm.ShowMyMocsPieces;
var
  inv: TBrickInventory;
  i, j: integer;
begin
  Screen.Cursor := crHourGlass;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowMyMocsPieces', itoa(inventory.numsets));

  document.write('<body background="splash.jpg">');
  document.title('Pieces of my mocs');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  inv := TBrickInventory.Create;
  for i := 0 to inventory.numsets - 1 do
    if db.IsMoc(inventory.sets[i].setid) then
      for j := 1 to inventory.sets[i].num do
        inv.AddSet(inventory.sets[i].setid, False);
  inv.DismandalAllSets;
  SortInventory(inv);

  DrawHeadLine('My builded mocs Inventory');

  DrawInventoryTable(inv, False);
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  inv.Free;
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowMyMinifiguresMenu;
begin
  document.write('<body background="splash.jpg">');
  document.title('My Minifigures');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  DrawHeadLine('Bricks Inventory - My Minifigures');
  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');

  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>Quick links</b></th>');
  document.write('</tr>');

  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="ShowMyMinifigInventory/1/1/1">All Minifigures</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="ShowMyMinifigInventory/1/0/0">My loose parts minifigures</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="ShowMyMinifigInventory/0/1/0">My official sets minifigures</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="ShowMyMinifigInventory/0/0/1">My mocs minifigures</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="ShowMyMinifigInventory/0/1/1">My official sets and mocs minifigures</a></td></tr>');

  document.write('</table></p></div></body>');

  document.SaveBufferToFile(diskmirror);
  document.Flash;
end;

procedure TMainForm.ShowMyPiecesValue;
var
  inv1, inv2, inv3: TBrickInventory;
  i, j: integer;
begin
  Screen.Cursor := crHourGlass;

  document.write('<body background="splash.jpg">');
  document.title('My Inventory Value');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  inv1 := TBrickInventory.Create;
  inv2 := TBrickInventory.Create;
  inv3 := inventory.Clone;

  for i := 0 to inventory.numsets - 1 do
    if db.IsMoc(inventory.sets[i].setid) then
    begin
      for j := 1 to inventory.sets[i].num do
        inv1.AddSet(inventory.sets[i].setid, False);
    end
    else
    begin
      for j := 1 to inventory.sets[i].num do
        inv2.AddSet(inventory.sets[i].setid, False);
    end;

  inv1.DismandalAllSets;
  inv1.Reorganize;

  inv2.DismandalAllSets;
  inv2.Reorganize;

  inv3.DismandalAllSets;
  inv3.Reorganize;

  DrawHeadLine('Loose Parts');
  DrawPartOutValueWithOutSets(inventory);

  DrawHeadLine('Mocs');
  DrawPartOutValue(inv1);

  DrawHeadLine('Official Sets');
  DrawPartOutValue(inv2);

  DrawHeadLine('Total');
  DrawPartOutValue(inv3);

  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  
  inv1.Free;
  inv2.Free;
  inv3.Free;

  document.SaveBufferToFile(diskmirror);
  document.Flash;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.doShowLengthQueryColor(inv: TBrickInventory; const id: string;
  const color: integer; inflst: TStringList);
var
  scode, spart, slen, sarea: string;
  len, area: integer;
  aa, i: integer;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  num: integer;
  totalweight: double;
  stmp, scolor: string;
  totallen: integer;
  totalarea: integer;
  totalpieces: integer;
begin
  if inv = nil then
    inv := inventory;

  scolor := itoa(color);

  totallen := 0;
  totalarea := 0;
  totalpieces := 0;
  totalweight := 0.0;
  aa := 0;

  document.write('<p valign=top>');
  stmp := db.colors(color).name + ' (' + itoa(color) + ') (BL=' + IntToStr(db.colors(color).BrickLingColor) + ')' + GetRebrickableColorHtml(color) +
    '<table border=1 width=25 bgcolor="#' + IntToHex(db.colors(color).RGB, 6) + '"><tr><td><br></td></tr></table>';
  DrawHeadLine(stmp);

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');

  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Part</b></th>');
  document.write('<th>Qty</th>');
  document.write('<th>Len</th>');
  document.write('<th>Area</th>');
  document.write('</tr>');


  for i := 1 to inflst.Count - 1 do // skip 0 - header
  begin
    splitstring(inflst.Strings[i], scode, spart, slen, sarea, ',');
    if scode <> id then
      Continue;

    pci := db.PieceColorInfo(spart, color, inflst.Objects[i]);
    if pci = nil then
      Continue;

    pi := db.PieceInfo(pci);

    len := atoi(slen);
    area := atoi(sarea);

    num := inv.LoosePartCount(spart, color);

    totallen := totallen + num * len;
    totalarea := totalarea + num * area;
    totalpieces := totalpieces + num;

    Inc(aa);

    document.write('<tr bgcolor=' + THBGCOLOR + '>');
    document.write('<td><p align="right">' + itoa(aa) + '.</b></td>');

    document.write('<td width=25%><img width=100px src=' + scolor + '\' + spart + '.png><br><b>');
    document.write('<a href=spiece/' + spart + '>' + spart + '</a></b>');
    document.write(' - ' + db.PieceDesc(pi));
    document.write(' <a href=spiecec/' + spart + '/' + scolor + '><img src="images\details.png"></a>' + HtmlDrawInvImgLink(spart, color, pi) + '</td>');

    document.write('<td width=10% align=right><b>' + IntToStr(num) + '</b>');
    document.write('<br><a href=editpiece/' + spart + '/' + scolor + '><img src="images\edit.png"></a>');
    document.write('<br><a href=diagrampiece/' + spart + '/' + scolor + '><img src="images\diagram.png"></a>');
    document.write('</td>');

    document.write('<td width=25% align=right><b>' + IntToStr(num * len) + '</b><br>');
    if pci <> nil then
    begin
      document.write(Format('(N) %2.4f €/stud<br>', [dbl_safe_div(pci.EvaluatePriceNew, len)]));
      document.write(Format('(U) %2.4f €/stud<br>', [dbl_safe_div(pci.EvaluatePriceUsed, len)]));
    end;
    document.write('</td>');

    document.write('<td width=25% align=right><b>' + IntToStr(num * area) + '</b><br>');
    if pci <> nil then
    begin
      document.write(Format('(N) %2.4f €/stud<br>', [dbl_safe_div(pci.EvaluatePriceNew, area)]));
      document.write(Format('(U) %2.4f €/stud<br>', [dbl_safe_div(pci.EvaluatePriceUsed, area)]));
    end;
    document.write('</td>');

    if pi <> nil then
      if pi.weight > 0.0 then
        totalweight := totalweight + pi.weight * num;

    document.write('</tr>');
  end;


  document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right><b>Total</b></td>');
  document.write('<td width=35%><b>' + IntToStr(aa) + ' unique mold' + decide(aa = 1, '', 's') + '</b></td>');

  document.write('<td width=10% align=right><b>' + IntToStr(totalpieces) + '<br>' + Format('%2.3f Kgr', [totalweight / 1000]) + '</b></td>');

  document.write('<td width=10% align=right><b>' + IntToStr(totallen) + '</b></td>');
  document.write('<td width=10% align=right><b>' + IntToStr(totalarea) + '</b></td>');

  document.write('</tr></table></p>');

end;

procedure TMainForm.doShowLengthQueryColorSlopes(inv: TBrickInventory; const id: string; const color: integer; inflst: TStringList);
var
  scode, spart, slen, sarea, sslopearea: string;
  len, area, slopearea: integer;
  aa, i: integer;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  num: integer;
  totalweight: double;
  stmp, scolor: string;
  totallen: integer;
  totalarea: integer;
  totalslopearea: integer;
  totalpieces: integer;
begin
  if inv = nil then
    inv := inventory;

  scolor := itoa(color);

  totallen := 0;
  totalarea := 0;
  totalslopearea := 0;
  totalpieces := 0;
  totalweight := 0.0;
  aa := 0;

  document.write('<p valign=top>');
  stmp := db.colors(color).name + ' (' + itoa(color) + ') (BL=' + IntToStr(db.colors(color).BrickLingColor) + ')' + GetRebrickableColorHtml(color) +
    '<table border=1 width=' + IntToStr(25) + ' bgcolor="#' + IntToHex(db.colors(color).RGB, 6) + '"><tr><td><br></td></tr></table>';
  DrawHeadLine(stmp);

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');

  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Part</b></th>');
  document.write('<th>Qty</th>');
  document.write('<th>Len</th>');
  document.write('<th>Area</th>');
  document.write('<th>Slope Area</th>');
  document.write('</tr>');


  for i := 1 to inflst.Count - 1 do // skip 0 - header
  begin
    splitstring(inflst.Strings[i], scode, spart, slen, sarea, sslopearea, ',');
    if scode <> id then
      Continue;

    pci := db.PieceColorInfo(spart, color, inflst.Objects[i]);
    if pci = nil then
      Continue;

    pi := db.PieceInfo(pci);

    len := atoi(slen);
    area := atoi(sarea);
    slopearea := atoi(sslopearea);

    num := inv.LoosePartCount(spart, color);

    totallen := totallen + num * len;
    totalarea := totalarea + num * area;
    totalslopearea := totalslopearea + num * slopearea;
    totalpieces := totalpieces + num;

    Inc(aa);

    document.write('<tr bgcolor=' + THBGCOLOR + '>');
    document.write('<td><p align="right">' + itoa(aa) + '.</b></td>');

    document.write('<td width=25%><img width=100px src=' + scolor + '\' + spart + '.png><br><b>');
    document.write('<a href=spiece/' + spart + '>' + spart + '</a></b>');
    document.write(' - ' + db.PieceDesc(pi));
    document.write(' <a href=spiecec/' + spart + '/' + scolor + '><img src="images\details.png"></a>' + HtmlDrawInvImgLink(spart, color, pi) + '</td>');

    document.write('<td width=10% align=right><b>' + IntToStr(num) + '</b>');
    document.write('<br><a href=editpiece/' + spart + '/' + scolor + '><img src="images\edit.png"></a>');
    document.write('<br><a href=diagrampiece/' + spart + '/' + scolor + '><img src="images\diagram.png"></a>');
    document.write('</td>');

    document.write('<td width=25% align=right><b>' + IntToStr(num * len) + '</b><br>');
    if pci <> nil then
    begin
      document.write(Format('(N) %2.4f €/stud<br>', [dbl_safe_div(pci.EvaluatePriceNew, len)]));
      document.write(Format('(U) %2.4f €/stud<br>', [dbl_safe_div(pci.EvaluatePriceUsed, len)]));
    end;
    document.write('</td>');

    document.write('<td width=25% align=right><b>' + IntToStr(num * area) + '</b><br>');
    if pci <> nil then
    begin
      document.write(Format('(N) %2.4f €/stud<br>', [dbl_safe_div(pci.EvaluatePriceNew, area)]));
      document.write(Format('(U) %2.4f €/stud<br>', [dbl_safe_div(pci.EvaluatePriceUsed, area)]));
    end;

    document.write('<td width=25% align=right><b>' + IntToStr(num * slopearea) + '</b><br>');
    if pci <> nil then
    begin
      document.write(Format('(N) %2.4f €/stud<br>', [dbl_safe_div(pci.EvaluatePriceNew, slopearea)]));
      document.write(Format('(U) %2.4f €/stud<br>', [dbl_safe_div(pci.EvaluatePriceUsed, slopearea)]));
    end;
    document.write('</td>');

    if pi <> nil then
      if pi.weight > 0.0 then
        totalweight := totalweight + pi.weight * num;

    document.write('</tr>');
  end;


  document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right><b>Total</b></td>');
  document.write('<td width=35%><b>' + IntToStr(aa) + ' unique mold' + decide(aa = 1, '', 's') + '</b></td>');

  document.write('<td width=10% align=right><b>' + IntToStr(totalpieces) + '<br>' + Format('%2.3f Kgr', [totalweight / 1000]) + '</b></td>');

  document.write('<td width=10% align=right><b>' + IntToStr(totallen) + '</b></td>');
  document.write('<td width=10% align=right><b>' + IntToStr(totalarea) + '</b></td>');
  document.write('<td width=10% align=right><b>' + IntToStr(totalslopearea) + '</b></td>');

  document.write('</tr></table></p>');

end;

procedure TMainForm.doShowDimentionsQueryColor(inv: TBrickInventory; const id: string; const color: integer; inflst: TStringList);
var
  scode, spart, sbwidth, slen, sarea: string;
  len, area, bwidth: integer;
  aa, i: integer;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  num: integer;
  totalweight: double;
  stmp, scolor: string;
  totallen: integer;
  totalarea: integer;
  totalbwidth: integer;
  totalpieces: integer;
begin
  if inv = nil then
    inv := inventory;

  scolor := itoa(color);

  totallen := 0;
  totalarea := 0;
  totalbwidth := 0;
  totalpieces := 0;
  totalweight := 0.0;
  aa := 0;

  document.write('<p valign=top>');
  stmp := db.colors(color).name + ' (' + itoa(color) + ') (BL=' + IntToStr(db.colors(color).BrickLingColor) + ')' + GetRebrickableColorHtml(color) +
    '<table border=1 width=' + IntToStr(25) + ' bgcolor="#' + IntToHex(db.colors(color).RGB, 6) + '"><tr><td><br></td></tr></table>';
  DrawHeadLine(stmp);

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');

  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Part</b></th>');
  document.write('<th>Qty</th>');
  document.write('<th>Width</th>');
  document.write('<th>Len</th>');
  document.write('<th>Area</th>');
  document.write('</tr>');


  for i := 1 to inflst.Count - 1 do // skip 0 - header
  begin
    splitstring(inflst.Strings[i], scode, spart, sbwidth, slen, sarea, ',');
    if scode <> id then
      Continue;

    pci := db.PieceColorInfo(spart, color, inflst.Objects[i]);
    if pci = nil then
      Continue;

    pi := db.PieceInfo(pci);

    bwidth := atoi(sbwidth);
    len := atoi(slen);
    area := atoi(sarea);

    num := inv.LoosePartCount(spart, color);

    totallen := totallen + num * len;
    totalarea := totalarea + num * area;
    totalbwidth := totalbwidth + num * bwidth;
    totalpieces := totalpieces + num;

    Inc(aa);

    document.write('<tr bgcolor=' + THBGCOLOR + '>');
    document.write('<td><p align="right">' + itoa(aa) + '.</b></td>');

    document.write('<td width=25%><img width=100px src=' + scolor + '\' + spart + '.png><br><b>');
    document.write('<a href=spiece/' + spart + '>' + spart + '</a></b>');
    document.write(' - ' + db.PieceDesc(pi));
    document.write(' <a href=spiecec/' + spart + '/' + scolor + '><img src="images\details.png"></a>' + HtmlDrawInvImgLink(spart, color, pi) + '</td>');

    document.write('<td width=10% align=right><b>' + IntToStr(num) + '</b>');
    document.write('<br><a href=editpiece/' + spart + '/' + scolor + '><img src="images\edit.png"></a>');
    document.write('<br><a href=diagrampiece/' + spart + '/' + scolor + '><img src="images\diagram.png"></a>');
    document.write('</td>');

    document.write('<td width=25% align=right><b>' + IntToStr(num * bwidth) + '</b><br>');
    if pci <> nil then
    begin
      document.write(Format('(N) %2.4f €/stud<br>', [dbl_safe_div(pci.EvaluatePriceNew, bwidth)]));
      document.write(Format('(U) %2.4f €/stud<br>', [dbl_safe_div(pci.EvaluatePriceUsed, bwidth)]));
    end;

    document.write('<td width=25% align=right><b>' + IntToStr(num * len) + '</b><br>');
    if pci <> nil then
    begin
      document.write(Format('(N) %2.4f €/stud<br>', [dbl_safe_div(pci.EvaluatePriceNew, len)]));
      document.write(Format('(U) %2.4f €/stud<br>', [dbl_safe_div(pci.EvaluatePriceUsed, len)]));
    end;
    document.write('</td>');

    document.write('<td width=25% align=right><b>' + IntToStr(num * area) + '</b><br>');
    if pci <> nil then
    begin
      document.write(Format('(N) %2.4f €/stud<br>', [dbl_safe_div(pci.EvaluatePriceNew, area)]));
      document.write(Format('(U) %2.4f €/stud<br>', [dbl_safe_div(pci.EvaluatePriceUsed, area)]));
    end;

    document.write('</td>');

    if pi <> nil then
      if pi.weight > 0.0 then
        totalweight := totalweight + pi.weight * num;

    document.write('</tr>');
  end;


  document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right><b>Total</b></td>');
  document.write('<td width=35%><b>' + IntToStr(aa) + ' unique mold' + decide(aa = 1, '', 's') + '</b></td>');

  document.write('<td width=10% align=right><b>' + IntToStr(totalpieces) + '<br>' + Format('%2.3f Kgr', [totalweight / 1000]) + '</b></td>');

  document.write('<td width=10% align=right><b>' + IntToStr(totalbwidth) + '</b></td>');
  document.write('<td width=10% align=right><b>' + IntToStr(totallen) + '</b></td>');
  document.write('<td width=10% align=right><b>' + IntToStr(totalarea) + '</b></td>');

  document.write('</tr></table></p>');
end;

const
  NUMCROSSCOLORS = 32;
  CROSSCOLOR: array[0..NUMCROSSCOLORS - 1] of integer = (
    0,
    1,
    2,
    4,
    5,
    6,
    7,
    8,
    14,
    15,
    19,
    28,
    25,
    27,
    70,
    308,
    71,
    72,
    272,
    288,
    320,
    326,
    378,
    379,
    84,
    484,
    73,
    74,
    22,
    85,
    26,
    226
  );

procedure TMainForm.doShowLengthQuery(inv: TBrickInventory; const id: string);
var
  lenlst: TStringList;
  i: integer;
begin
  ShowSplash;
  SplashProgress('Working...', 0);

  lenlst := TStringList.Create;
  if fexists(basedefault + 'db\db_cross_stats.txt') then
    lenlst.LoadFromFile(basedefault + 'db\db_cross_stats.txt');

  UpdateDismantaledsetsinv;

  if inv = nil then
    inv := inventory;

  Screen.Cursor := crHourglass;

  document.write('<body background="splash.jpg">');
  document.title(id + 'Stats');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  DrawHeadLine(id);

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');

  for i := 0 to NUMCROSSCOLORS - 1 do
  begin
    if not odd(i) then
      document.write('<tr valign=top bgcolor=' + THBGCOLOR + '>');

    document.write('<td valign="top">');
    doShowLengthQueryColor(inv, id, CROSSCOLOR[i], lenlst);
    document.write('</td>');

    if odd(i) then
      document.write('</tr>');

    SplashProgress('Working...', (i + 1) / NUMCROSSCOLORS);
  end;

  document.write('</table></p></div>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  lenlst.Free;

  Screen.Cursor := crDefault;
end;

procedure TMainForm.doShowLengthQuerySlopes(inv: TBrickInventory; const id: string);
var
  lenlst: TStringList;
  i: integer;
begin
  ShowSplash;
  SplashProgress('Working...', 0);

  lenlst := TStringList.Create;
  if fexists(basedefault + 'db\db_cross_stats.txt') then
    lenlst.LoadFromFile(basedefault + 'db\db_cross_stats.txt');

  UpdateDismantaledsetsinv;

  if inv = nil then
    inv := inventory;

  Screen.Cursor := crHourglass;

  document.write('<body background="splash.jpg">');
  document.title(id + 'Stats');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  DrawHeadLine(id);

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');

  for i := 0 to NUMCROSSCOLORS - 1 do
  begin
    if not odd(i) then
      document.write('<tr valign=top bgcolor=' + THBGCOLOR + '>');

    document.write('<td>');
    doShowLengthQueryColorSlopes(inv, id, CROSSCOLOR[i], lenlst);
    document.write('</td>');

    if odd(i) then
      document.write('</tr>');

    SplashProgress('Working...', (i + 1) / NUMCROSSCOLORS);
  end;

  document.write('</table></p></div>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  lenlst.Free;

  Screen.Cursor := crDefault;
end;

procedure TMainForm.doShowDimentionsQuery(inv: TBrickInventory; const id: string);
var
  lenlst: TStringList;
  i: integer;
begin
  ShowSplash;
  SplashProgress('Working...', 0);

  lenlst := TStringList.Create;
  if fexists(basedefault + 'db\db_cross_stats.txt') then
    lenlst.LoadFromFile(basedefault + 'db\db_cross_stats.txt');

  UpdateDismantaledsetsinv;

  if inv = nil then
    inv := inventory;

  Screen.Cursor := crHourglass;

  document.write('<body background="splash.jpg">');
  document.title(id + 'Stats');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  DrawHeadLine(id);

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');

  for i := 0 to NUMCROSSCOLORS - 1 do
  begin
    if not odd(i) then
      document.write('<tr valign=top bgcolor=' + THBGCOLOR + '>');

    document.write('<td>');
    doShowDimentionsQueryColor(inv, id, CROSSCOLOR[i], lenlst);
    document.write('</td>');

    if odd(i) then
      document.write('</tr>');

    SplashProgress('Working...', (i + 1) / NUMCROSSCOLORS);
  end;

  document.write('</table></p></div>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  lenlst.Free;

  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowLengthQuery(const id: string);
begin
  doShowLengthQuery(inventory, id);
end;

procedure TMainForm.ShowLengthQuerySlopes(const id: string);
begin
  doShowLengthQuerySlopes(inventory, id);
end;

procedure TMainForm.ShowDimentionsQuery(const id: string);
begin
  doShowDimentionsQuery(inventory, id);
end;

procedure TMainForm.Missingfromstoragebins1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('ShowMissingFromStorageBins', foo);
end;

procedure TMainForm.Priceguide1Click(Sender: TObject);
begin
  if SaveDialogPriceGuide.Execute then
  begin
    ShowSplash;
    progress_string := 'Saving Priceguide...';
    SplashProgress(progress_string, 0.05);
    db.ExportPriceGuide(SaveDialogPriceGuide.Filename);
    HideSplash;
  end;
end;

procedure TMainForm.Partoutguide1Click(Sender: TObject);
begin
  if SaveDialogPartOut.Execute then
  begin
    ShowSplash;
    progress_string := 'Saving Part Out guide...';
    SplashProgress(progress_string, 0.05);
    db.ExportPartOutGuide(SaveDialogPartOut.Filename);
    HideSplash;
  end;
end;

procedure TMainForm.BricklinkOrder1Click(Sender: TObject);
var
  fname, fname2: string;
  orderstmp: TOrders;
  lst: TStringList;
  path1, path2: string;
  i, j: integer;
  inv: TBrickInventory;
  foo: boolean;
begin
  if OpenDialog1.Execute then
  begin
    Screen.Cursor := crHourglass;
    fname := strupper(ExpandFilename(OpenDialog1.FileName));
    orderstmp := TOrders.Create;
    lst := TStringList.Create;
    path1 := ExtractFilePath(fname);
    fname2 := ChangeFileExt(strupper(ExpandFilename(basedefault + 'orders\' + ExtractFileName(fname))), '.XML');
    path2 := ExtractFilePath(fname2);
    if path1 <> path2 then
    begin
      ForceDirectories(basedefault + 'orders\');
      if CopyFile(fname, fname2) then
        orders.LoadFile(fname2);
    end;
    orderstmp.LoadFile(fname);
    for i := 0 to orderstmp.numorders - 1 do
      for j := 0 to orderstmp.orders[i].Count - 1 do
        lst.Add(itoa(orderstmp.orders[i].ORDER[j].ORDERID));

    for i := 0 to lst.Count - 1 do
    begin
      inv := orderstmp.OrderInventory(lst.Strings[i]);
      inv.Reorganize;
      for j := 0 to inv.numlooseparts - 1 do
        inventory.StorePieceInventoryStatsRec(
          basedefault + 'cache\' + itoa(decide(inv.looseparts[j].color = -1, 9999, inv.looseparts[j].color)) + '\' + inv.looseparts[j].part + '.history',
          inv.looseparts[j].part,
          inv.looseparts[j].color
        );
      for j := 0 to inv.numsets - 1 do
        inventory.StorePieceInventoryStatsRec(basedefault + 'out\' + inv.sets[j].setid + '\' + inv.sets[j].setid + '.history',
          inv.sets[j].setid, -1
        );
      inventory.MergeWith(inv);
      for j := 0 to inv.numlooseparts - 1 do
        inventory.StorePieceInventoryStatsRec(
          basedefault + 'cache\' + itoa(decide(inv.looseparts[j].color = -1, 9999, inv.looseparts[j].color)) + '\' + inv.looseparts[j].part + '.history',
          inv.looseparts[j].part,
          inv.looseparts[j].color
        );
      for j := 0 to inv.numsets - 1 do
        inventory.StorePieceInventoryStatsRec(basedefault + 'out\' + inv.sets[j].setid + '\' + inv.sets[j].setid + '.history',
          inv.sets[j].setid, -1
        );
      inv.Free;
    end;

    lst.Free;
    orderstmp.Free;
    Screen.Cursor := crDefault;
    btn_SaveClick(nil);
    HTMLClick('refresh', foo);
  end;
  ChDir(basedefault);
end;

procedure TMainForm.LUGBULKSuggestions1Click(Sender: TObject);
var
  iyears: string;
  idemand, isold: integer;
  iprice: double;
  foo: boolean;
begin
  if GetLugbulkSuggestParams(iyears, idemand, isold, iprice) then
    HTMLClick('lugbulksuggest/' + iyears + '/' + itoa(idemand) + '/' + itoa(isold) + '/' + ftoa(iprice), foo);
end;

procedure TMainForm.ShowLugbulkSuggestions(const years: string; const demand, sold: integer; const price: double);
var
  i: integer;
  lst, lst2: TStringList;
  pcs: string;
  pi: TPieceInfo;
  pci: TPieceColorInfo;
  weight: double;
  cost: double;
  perkgr: double;
  inv: TBrickInventory;
  s1: string;
  sc: TScriptEngine;
  YY: TDNumberList;
  scolor: string;
  color: integer;
begin
  Screen.Cursor := crHourglass;

  lst := TStringList.Create;

  YY := TDNumberList.Create;

  sc := TScriptEngine.Create(years);

  while sc.GetInteger do
    YY.Add(sc._Integer);

  sc.Free;

  for i := 0 to YY.Count - 1 do
  begin
    lst2 := db.PieceListForYear(YY.Numbers[i]);
    lst.AddStrings(lst2);
    lst2.Free;
  end;

  YY.Free;

  RemoveDuplicates(lst);

  inv := TBrickInventory.Create;

  lst2 := TStringList.Create;

  for i := 0 to lst.Count - 1 do
  begin
    splitstring(lst.Strings[i], pcs, scolor, ',');
    color := atoi(scolor);
    pi := db.PieceInfo(pcs);
    weight := db.GetItemWeight(pcs, color, pi);
    if weight > 0.0 then
    begin
      pci := db.PieceColorInfo(pcs, color, lst.Objects[i]);
      if pci <> nil then
      begin
        pci.pieceinfo := pi;
        cost := pci.EvaluatePriceNew;
        if cost > 0.0 then
        begin
          perkgr := dbl_safe_div(cost, weight) * 1000;
          if perkgr >= price then
            if pci.nDemand >= demand / 100 then
              if pci.priceguide.nTotalQty > sold then
              begin
                lst2.Add(pcs + ',' + itoa(color));
                inv.AddLoosePartFast(pcs, color, 1, pci);
              end;
        end;
      end;
    end;
  end;

  lst.Free;
  lst2.Sort;
  DrawPieceListLugbulk('Lugbulk suggestions', lst2);
  lst2.Free;

  s1 := basedefault + 'out\Lugbulksuggest\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'partlist';
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListNew(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListNew(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListNew(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListNew(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListNew(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListNew(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListNew(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListNew(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_030%.xml', 0.3);

  inv.Free;

  Screen.Cursor := crDefault;
end;


procedure TMainForm.SpeedButton1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick(AddressEdit.Text, foo);
end;

procedure TMainForm.Missingfordismandaledsets1Click(Sender: TObject);
var
  foo: Boolean;
  lst: TStringList;
begin
  lst := inventory.GetDismandaledSets;
  Missingfordismandaledsetslist.Text := lst.Text;
  lst.Free;
  if SelectSets(Missingfordismandaledsetslist) then
    HTMLClick('multymissing/' + IntToStr(Integer(Missingfordismandaledsetslist)), foo);

end;

procedure TMainForm.Usedsetstobuiltfromscratch1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('ShowSetsForPartInUsed/150', foo);
end;

procedure TMainForm.Batchlink1Click(Sender: TObject);
var
  s: TStringList;
  i: integer;
  foo: boolean;
begin
  s := TStringList.Create;
  if GetBatchLinks(s) then
  begin
    Screen.Cursor := crHourglass;
    for i := 0 to s.Count - 1 do
      HTMLClick(s.Strings[i], foo);
    S_FlashFileSystem;
    Screen.Cursor := crDefault;
  end;
  s.Free;
end;

procedure TMainForm.StoreInventoryStatsRec(const piece: string; const color: string = '');
begin
  if color = '' then
    inventory.StorePieceInventoryStatsRec(basedefault + 'out\' + piece + '\' + piece + '.history',
      piece, -1
    )
  else
    inventory.StorePieceInventoryStatsRec(
       basedefault + 'cache\' + decide(color = '-1', '9999', color) + '\' + piece + '.history',
          piece, atoi(color, 9999)
        );

end;

procedure TMainForm.Reloadcache1Click(Sender: TObject);
var
  foo: boolean;
begin
  Screen.Cursor := crHourglass;
  ShowSplash;
  db.ReloadCache;
  HideSplash;
  Screen.Cursor := crDefault;
  CheckCacheHashEfficiency1Click(Sender);
  HTMLClick('refresh', foo);
end;

procedure TMainForm.CheckCacheHashEfficiency1Click(Sender: TObject);
var
  hits, total: integer;
begin
  ShowSplash;
  db.GetCacheHashEfficiency(hits, total);
  HideSplash;
  ShowMessage(
    Format('Direct Hit:'#13#10'%d hits / %d total'#13#10'Efficiency = %2.2f%s'#13#10#13#10 +
           'Current:'#13#10'%d hits / %d total'#13#10'Efficiency = %2.2f%s'#13#10#13#10'Average hit level = %2.2f',
            [hits, total, dbl_safe_div(hits * 100, total), '%', db.pciloadscache,
             db.pciloads, dbl_safe_div(db.pciloadscache * 100, db.pciloads), '%', db.CacheDB.GetHitLevel])
             );
end;

procedure TMainForm.Bricks1x1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/bricks1x', foo);
end;

procedure TMainForm.Bricks2x1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/bricks2x', foo);
end;

procedure TMainForm.Plates1x1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/plates1x', foo);
end;

procedure TMainForm.Plates2x1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/plates2x', foo);
end;

procedure TMainForm.N10Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/plates4x', foo);
end;

procedure TMainForm.Plates6x1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/plates6x', foo);
end;

procedure TMainForm.Plates8x1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/plates8x', foo);
end;

procedure TMainForm.Tiles1x1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/tiles1x', foo);
end;

procedure TMainForm.Tiles2x1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/tiles2x', foo);
end;

procedure TMainForm.Set2Click(Sender: TObject);
var
  setid: string;
  foo: Boolean;
begin
  setid := lastset;
  if GetSetID(setid) then
    HTMLClick('DoEditSet/' + setid, foo);
end;

procedure TMainForm.DoEditSet(const setid: string);
var
  foo: Boolean;
  lst: TStringList;
  data: string;
  desc: string;
  ismoc: Boolean;
  year: integer;
  inv: TBrickInventory;
  s1, s2: string;
  oldpci: TPieceColorInfo;
begin
  lst := TStringList.Create;
  lst.Text := db.binarysets.GetSetAsText(setid);
  if lst.Count < 2 then
    if fexists(basedefault + 'db\sets\' + setid + '.txt') then
      lst.LoadFromFile(basedefault + 'db\sets\' + setid + '.txt');
  if lst.Count < 2 then
//    if db.SetDesc(setid) <> '' then
    begin
      inv := db.GetSetInventory(setid);
      if inv <> nil then
      begin
        s1 := basedefault + 'out\' + setid + '\';
        if not DirectoryExists(s1) then
          ForceDirectories(s1);
        s2 := s1 + setid + '.txt';
        inv.SaveLoosePartsAndSets(s2);
        lst.LoadFromFile(s2);
      end;
    end;
  if lst.Count < 2 then
    if fexists(basedefault + 'mosaic\' + setid + '.txt') then
      lst.LoadFromFile(basedefault + 'mosaic\' + setid + '.txt');

  data := lst.Text;
  desc := db.SetDesc(setid);
  year := db.SetYear(setid);
  ismoc := db.IsMoc(setid);
  oldpci := db.PieceColorInfo(setid, -1);
  if EditSetAsTextForm(setid, data, desc, year, ismoc) then
  begin
    Screen.Cursor := crHourglass;
    try
      db.UpdateSet(setid, data);
      db.UpdateSetInfo(setid, desc, year, ismoc);
      if oldpci = nil then
        db.AddKnownPiece(setid, -1, desc);
      HTMLClick('sinv/' + setid, foo);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
  lst.Free;
end;

procedure TMainForm.TechnicBricks1x1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/technic1x', foo);
end;

procedure TMainForm.DrawPieceListLugbulkKnownCost(const tit: string; const lb: TLugBulk2017; const year: integer;
  const over: double; const dobrickorederinfo: boolean; const catid: Integer = -1);
var
  i, cl: integer;
  col: string;
  pcs: string;
  aa: integer;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  prn, pru: double;
  prnt, prut: double;
  numpieces: integer;
  bp: brickpool_t;
  totpieces: integer;
  mycosttot: double;
  lst: TStringList;
  lprice: Double;
  syear, lugcostedit: string;
  www: double;
  fn: string;
begin
  UpdateDismantaledsetsinv;
  ShowSplash;
  SplashProgress('Working...', 0);

  lst := TStringList.Create;

  if over >= 1.0 then
  begin
    for i := 0 to lb.List.Count - 1 do
    begin
      lprice := (lb.List.Objects[i] as TLugBulkDouble).value;
      if lprice > 0.0 then
      begin
        pcs := lb.Part[i];
        if (catid = -1) or (catid = db.PieceInfo(pcs).category) then
        begin
          pci := db.PieceColorInfo(pcs, lb.Color[i]);
          if pci <> nil then
            if lprice * over < pci.EvaluatePriceNew then
              lst.Add(lb.List.Strings[i]);
        end;
      end;
    end;
  end
  else
  begin
    for i := 0 to lb.List.Count - 1 do
    begin
      lprice := (lb.List.Objects[i] as TLugBulkDouble).value;
      if lprice > 0.0 then
      begin
        pcs := lb.Part[i];
        if (catid = -1) or (catid = db.PieceInfo(pcs).category) then
        begin
          pci := db.PieceColorInfo(pcs, lb.Color[i]);
          if pci <> nil then
            lst.Add(lb.List.Strings[i]);
        end;
      end;
    end;
  end;

  // Remove doupicates
  lst.Sort;
  for i := lst.Count - 1 downto 1 do
    if lst.Strings[i] = lst.Strings[i - 1] then
      lst.Delete(i);

  if domultipagedocuments then
    document.NewMultiPageDocument('DrawPieceListLugbulkKnownCost', tit + '_' + itoa(lb.List.Count) + '_' + itoa(lst.Count) + '_' + ftoa(over) + '_' + itoa(Ord(dobrickorederinfo)) + '_' + itoa(catid));

  document.write('<body background="splash.jpg">');
  document.title(tit);
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine(tit);
  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Image</b></th>');
  document.write('<th>Color</th>');
  document.write('<th>Demand</th>');
  document.write('<th>Lugbulk Cost</th>');
  document.write('<th>Bricklink Cost (New)</th>');
  document.write('<th>Times Sold</th>');
  document.write('<th>Total Qty</th>');
  document.write('</tr>');

  aa := 0;
  prnt := 0.0;
  prut := 0.0;
  totpieces := 0;
  mycosttot := 0.0;

  syear := itoa(lb.year);

  for i := 0 to lst.Count - 1 do
  begin
    inc(aa);
    document.StartItemId(aa);
    splitstring(lst.Strings[i], pcs, col, ',');
    cl := atoi(col);
    numpieces := inventory.LoosePartCount(pcs, cl);
    totpieces := totpieces + numpieces;
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');

    pci := db.PieceColorInfo(pcs, cl);
    pi := db.PieceInfo(pci);

    document.write('<td width=25%><img width=100px src=' + col + '\' + pcs + '.png>');
    document.write('<a href=spiece/' + pcs + '>' + pcs + '</a></b>');
    document.write(' - ' + db.PieceDesc(pi) + '</td>');
    document.write('<td width=20%>');
    DrawColorCell(cl, 25);
//    document.BlancColorCell(db.colors(cl).RGB, 25);
    if pci = nil then
    begin
      document.write('<a href=spiecec/' + pcs + '/' + col + '>' + db.colors(cl).name +
        ' (' + col + ') (BL=' + IntToStr(db.colors(cl).BrickLingColor) + ')' + GetRebrickableColorHtml(cl) +
        '<img src="images\details.png"></a>' + HtmlDrawInvImgLink(pcs, cl, pi) + '</td>');
      document.write(
        '<td width=15% align=right>' + Format('%2.3f', [0.0]) +
        '<br><a href=editpiece/' + pcs + '/' + col + '><img src="images\edit.png"></a>' +
        '<br><a href=diagrampiece/' + pcs + '/' + col + '><img src="images\diagram.png"></a></td>');
    end
    else
    begin
      document.write('<a href=spiecec/' + pcs + '/' + col + '>' + db.colors(cl).name +
        ' (' + col + ') (BL=' + IntToStr(db.colors(cl).BrickLingColor) + ')' + GetRebrickableColorHtml(cl) +
        '<img src="images\details.png"></a>' +
        HtmlDrawInvImgLink(pcs, cl, pi) +
        HtmlDrawInvCode(pci, '<br>', '') +
         decide(pci.setmost = '', '', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + '</td>');
      document.write(
        '<td width=15% align=right>' + Format('%2.3f', [pci.nDemand]) +
        '<br><a href=editpiece/' + pcs + '/' + col + '><img src="images\edit.png"></a>' +
        '<br><a href=diagrampiece/' + pcs + '/' + col + '><img src="images\diagram.png"></a></td>');
    end;

    lprice := lb.ItemCost(pcs, cl);
    lugcostedit := 'EditLugbulkPrice/' + pcs + '/' + col + '/' + syear;
    www := db.GetItemWeight(pcs, cl, pi);
    if lprice >= 0.0 then
      document.write('<td width=15% align=right>' +
        Format('€ ' + '<a href=' + lugcostedit + '>' + '%2.4f</a><br>€ %2.2f / Kgr', [lprice, dbl_safe_div(lprice, www) * 1000]) + '</td>')
    else
      document.write('<td width=15% align=right><a href=' + lugcostedit + '>-</a></td>');

    if pci <> nil then
    begin
      prn := pci.EvaluatePriceNew;
      pru := pci.EvaluatePriceUsed;
      document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Kgr', [prn, dbl_safe_div(prn, www) * 1000]) + '</td>');
      prnt := prnt + prn * numpieces;
      prut := prut + pru * numpieces;
      document.write('<td width=5% align=right>' + itoa(pci.priceguide.nTimesSold) + '</td>');
      document.write('<td width=5% align=right>' + itoa(pci.priceguide.nTotalQty) + '</td>');
    end
    else
    begin
      document.write('<td width=10% align=right>-</td>');
      document.write('<td width=5% align=right>-</td>');
      document.write('<td width=5% align=right>-</td>');
    end;
    document.write('</tr>');

    bp.part := pcs;
    bp.color := cl;
    bp.num := numpieces;

    if dobrickorederinfo then
      DrawBrickOrderInfo(@bp);
    SplashProgress('Working...', i / lst.Count);
  end;

  document.EndNavigateSection;
  document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right><b>Total</b></td>');
  document.write('<td width=25%><b> </b></td>');
  document.write('<td width=20%><b> </b></td>');
  document.write('<td width=10% align=right></td>');
  document.write('<td width=10% align=right><b>' + IntToStr(totpieces) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [prnt]) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [prut]) + '</b></td>');
  document.write('<td width=10% align=right><b> </b></td>');
  document.write('</tr></table>');

  document.MarkBottomNavigateSection;

  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  lst.Insert(0, 'Part,Color');
  fn := basedefault + 'lugbulks\' + IntToStr(year) + '_' + FloatToStr(over) + '_' + IntToStr(catid) + '.txt';
  BackUpFile(fn);
  lst.SaveToFile(fn);
  lst.Free;
end;

procedure TMainForm.LugBulk20171Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2017/5.0', foo);
end;

procedure TMainForm.ShowLugbulk(const year: string; const catid: integer  = -1);
var
  lb: TLugBulk2017;
  slugbulkfile: string;
  catname: string;
begin
  slugbulkfile := basedefault + 'lugbulks\' + year + '.txt';
  if not fexists(slugbulkfile) then
  begin
    ShowMessage('File ' + slugbulkfile + #13#10 + 'not found!');
    Exit;
  end;

  lb := TLugBulk2017.Create;
  lb.LoadFromFile(basedefault + 'lugbulks\' + year + '.txt');

  catname := '';
  if catid >= 0 then
    if catid < MAXCATEGORIES then
      if db.categories[catid].fetched then
        catname := ' (' + db.categories[catid].name + ')';

  DrawPieceListLugbulkKnownCost('Lugbulk ' + year + catname, lb, StrToIntDef(year, 2020), 0.0, True, catid);

  lb.Free;
end;

procedure TMainForm.ShowLugbulkBestPrice(const year: string; const over: double; const catid: integer  = -1);
var
  lb: TLugBulk2017;
  slugbulkfile: string;
  catname: string;
begin
  if over < 0 then
    Exit;

  slugbulkfile := basedefault + 'lugbulks\' + year + '.txt';
  if not fexists(slugbulkfile) then
  begin
    ShowMessage('File ' + slugbulkfile + #13#10 + 'not found!');
    Exit;
  end;

  lb := TLugBulk2017.Create;
  lb.LoadFromFile(basedefault + 'lugbulks\' + year + '.txt');

  catname := '';
  if catid >= 0 then
    if catid < MAXCATEGORIES then
      if db.categories[catid].fetched then
        catname := ' (' + db.categories[catid].name + ')';

  DrawPieceListLugbulkKnownCost('Lugbulk suggestions ' + year + catname, lb, StrToIntDef(year, 2020), over, True, catid);

  lb.Free;
end;

procedure TMainForm.ShowLugbulkBestPriceNoBrickOrder(const year: string; const over: double; const catid: integer  = -1);
var
  lb: TLugBulk2017;
  slugbulkfile: string;
  catname: string;
begin
  if over < 0 then
    Exit;

  slugbulkfile := basedefault + 'lugbulks\' + year + '.txt';
  if not fexists(slugbulkfile) then
  begin
    ShowMessage('File ' + slugbulkfile + #13#10 + 'not found!');
    Exit;
  end;

  lb := TLugBulk2017.Create;
  lb.LoadFromFile(basedefault + 'lugbulks\' + year + '.txt');

  catname := '';
  if catid >= 0 then
    if catid < MAXCATEGORIES then
      if db.categories[catid].fetched then
        catname := ' (' + db.categories[catid].name + ')';

  DrawPieceListLugbulkKnownCost('Lugbulk suggestions ' + year, lb, StrToIntDef(year, 2020), over, False, catid);

  lb.Free;
end;

procedure TMainForm.LugBulk2017CheapBricks1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2017/5.0/5', foo);
end;

procedure TMainForm.LugBulk2017CheapSlopes1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2017/5.0/31', foo);
end;

procedure TMainForm.LugBulk2017CheapInvertedSlopes1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2017/5.0/32', foo);
end;

procedure TMainForm.LugBulk2017CheapPlates1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2017/5.0/26', foo);
end;

procedure TMainForm.LugBulk2017CheapTiles1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2017/5.0/37', foo);
end;

procedure TMainForm.Specialbricks1x1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/bricks1xS', foo);
end;

procedure TMainForm.Rebrickableparts1Click(Sender: TObject);
begin
  if SaveToRebrickableDialog1.Execute then
  begin
    Screen.Cursor := crHourglass;
    inventory.SaveLoosePartsForRebrickable(SaveToRebrickableDialog1.FileName);
    Screen.Cursor := crDefault;
  end;
  ChDir(basedefault);
end;

procedure TMainForm.Setswithunknownreleaseyear1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('ShowSetsAtUnknownYear', foo);
end;

procedure TMainForm.LugBulk2018CheapParts1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2018/5.0', foo);
end;

procedure TMainForm.LugBulk2018CheapBricks1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2018/5.0/5', foo);
end;

procedure TMainForm.LugBulk2018CheapPlates1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2018/5.0/26', foo);
end;

procedure TMainForm.LugBulk2018CheapTiles1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2018/5.0/37', foo);
end;

procedure TMainForm.LugBulk2018CheapSlopes1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2018/5.0/31', foo);
end;

procedure TMainForm.LugBulk2018CheapInvertedSlopes1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2018/5.0/32', foo);
end;

procedure TMainForm.S1Click(Sender: TObject);
begin
  Showorderinformation1.Checked := dodraworderinfo;
  Multipagedisplay1.Checked := domultipagedocuments;
  UsemultipleCPUcores1.Checked := usemultithread;
end;

procedure TMainForm.Showorderinformation1Click(Sender: TObject);
var
  foo: boolean;
begin
  dodraworderinfo := not dodraworderinfo;
  HTMLClick(AddressEdit.Text, foo);
end;

procedure TMainForm.Rebrickablesets1Click(Sender: TObject);
begin
  if SaveToRebrickableDialog1.Execute then
  begin
    Screen.Cursor := crHourglass;
    inventory.SaveLooseSetsForRebrickable(SaveToRebrickableDialog1.FileName);
    Screen.Cursor := crDefault;
  end;
  ChDir(basedefault);
end;

procedure TMainForm.Rebrickablepartsofbuildedsets1Click(Sender: TObject);
var
  inv: TBrickInventory;
  i, j: integer;
begin
  if SaveToRebrickableDialog1.Execute then
  begin
    Screen.Cursor := crHourGlass;

    inv := TBrickInventory.Create;
    for i := 0 to inventory.numsets - 1 do
      for j := 1 to inventory.sets[i].num do
        inv.AddSet(inventory.sets[i].setid, False);
    inv.DismandalAllSets;

    inv.SaveLoosePartsForRebrickable(SaveToRebrickableDialog1.FileName);
    inv.Free;
    Screen.Cursor := crDefault;
  end;
  ChDir(basedefault);
end;

procedure TMainForm.About1Click(Sender: TObject);
var
  rsTitle: string;
  mused: float;
begin
  mused := GetMemoryUsed / 1024 / 1024;
  rsTitle := 'Bricks Inventory';
  MessageBox(
    Handle,
    PChar(Format('%s'#13#10'Version %s'#13#10#13#10'A tool for managing your brick collection.'#13#10'© 2014 - 2023, jvalavanis@gmail.com'#13#10'%2.2f MB memory used', [rsTitle, I_VersionBuilt, mused])),
    PChar(rsTitle),
    MB_OK or MB_ICONINFORMATION or MB_APPLMODAL);
end;

procedure TMainForm.Rebrickablepartsofbuildedmocs1Click(Sender: TObject);
var
  inv: TBrickInventory;
  i, j: integer;
begin
  if SaveToRebrickableDialog1.Execute then
  begin
    Screen.Cursor := crHourGlass;

    inv := TBrickInventory.Create;
    for i := 0 to inventory.numsets - 1 do
      if db.IsMoc(inventory.sets[i].setid) then
        for j := 1 to inventory.sets[i].num do
          inv.AddSet(inventory.sets[i].setid, False);
    inv.DismandalAllSets;

    inv.SaveLoosePartsForRebrickable(SaveToRebrickableDialog1.FileName);
    inv.Free;
    Screen.Cursor := crDefault;
  end;
  ChDir(basedefault);
end;

procedure TMainForm.PlateMosaic1Click(Sender: TObject);
var
  frm: TMosaicFormPlates;
  fname: string;
  foo: boolean;
begin
  frm := TMosaicFormPlates.Create(nil);
  frm.ShowModal;
  if frm.ModalResult = mrOK then
  begin
    fname := frm.name;
    if fname <> '' then
    begin
      fname := 'mosaic_plates_' + fname + Format('_%dx%d', [frm.mosaic.width, frm.mosaic.height]);
      frm.mosaic.inv.SaveLooseParts(basedefault + 'mosaic\' + fname + '.txt');
      db.binarysets.UpdateSetFromTextFile(fname, basedefault + 'mosaic\' + fname + '.txt');
      frm.mosaic.Bitmap.SaveToFile(basedefault + 'mosaic\' + fname + '.bmp');
      frm.mosaic.PlaneBitmap.SaveToFile(basedefault + 'mosaic\' + fname + '_plane.bmp');
      SaveBmpToJpeg(frm.mosaic.Bitmap, basedefault + 's\' + fname + '.jpg');
      HTMLClick('sinv/' + fname, foo);
    end;
  end;
  frm.Free;
end;

procedure TMainForm.TileMosaic1Click(Sender: TObject);
var
  frm: TMosaicFormTiles;
  fname: string;
  foo: boolean;
begin
  frm := TMosaicFormTiles.Create(nil);
  frm.ShowModal;
  if frm.ModalResult = mrOK then
  begin
    fname := frm.name;
    if fname <> '' then
    begin
      fname := 'mosaic_tiles_' + fname + Format('_%dx%d', [frm.mosaic.width, frm.mosaic.height]);
      frm.mosaic.inv.SaveLooseParts(basedefault + 'mosaic\' + fname + '.txt');
      db.binarysets.UpdateSetFromTextFile(fname, basedefault + 'mosaic\' + fname + '.txt');
      frm.mosaic.Bitmap.SaveToFile(basedefault + 'mosaic\' + fname + '.bmp');
      frm.mosaic.PlaneBitmap.SaveToFile(basedefault + 'mosaic\' + fname + '_plane.bmp');
      SaveBmpToJpeg(frm.mosaic.Bitmap, basedefault + 's\' + fname + '.jpg');
      HTMLClick('sinv/' + fname, foo);
    end;
  end;
  frm.Free;
end;

procedure TMainForm.SnotBricks1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/snot', foo);
end;

procedure TMainForm.UsedPiecesaboveeuroKgr(const x: integer);
var
  i, j: integer;
  lst: TStringList;
  pcs: string;
  pi: TPieceInfo;
  pci: TPieceColorInfo;
  weight: double;
  cost: double;
  perkgr: double;
  inv: TBrickInventory;
  s1: string;
  nextstr, prevstr: string;
  nextx, prevx: integer;
  kp: THashStringList;
  titstr: string;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
        for j := 0 to kp.Count - 1 do
        begin
          pcs := kp.Strings[j];
          pci := kp.Objects[j] as TPieceColorInfo;
          if pci <> nil then
          begin
            pi := db.PieceInfo(pci);
            weight := db.GetItemWeight(pci.piece, pci.color, pi);
            if weight > 0.0 then
            begin
              cost := pci.EvaluatePriceUsed;
              if cost > 0.0 then
              begin
                perkgr := dbl_safe_div(cost, weight) * 1000;
                if perkgr >= x then
                begin
                  lst.Add(pcs + ',' + itoa(i));
                  inv.AddLoosePartFast(pcs, i, 1, pci);
                end;
              end;
            end;
          end;
        end;
    end;

  if x > 1000 then
  begin
    prevx := round(x * 0.8);
    if prevx < 1000 then
      prevx := 1000;
    prevstr := '<a href=UsedPiecesaboveeuroKgr/' + itoa(prevx) + '>Used pieces with price above ' + itoa(prevx) + ' euro/Kgr</a><br>';
  end
  else
    prevstr := '';
  nextx := round(x * 1.25);
  nextstr := '<br><a href=UsedPiecesaboveeuroKgr/' + itoa(nextx) + '>Used pieces with price above ' + itoa(nextx) + ' euro/Kgr</a>';

  lst.Sort;
  titstr := 'Used pieces with price above ' + itoa(x) + ' euro/Kgr';
  DrawPieceList(prevstr + titstr + nextstr, lst, SORT_PRICE_USED, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\UsedPiecesabove' + IntToStrzFill(4, x) + 'euroKgr\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'UsedPiecesabove' + IntToStrzFill(4, x) + 'euroKgr';
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListUsed(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListUsed(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListUsed(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListUsed(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListUsed(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListUsed(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListUsed(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListUsed(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_030%.xml', 0.3);

  inv.Free;

end;

procedure TMainForm.NewPiecesaboveeuroKgr(const x: integer);
var
  i, j: integer;
  lst: TStringList;
  pcs: string;
  pi: TPieceInfo;
  pci: TPieceColorInfo;
  weight: double;
  cost: double;
  perkgr: double;
  inv: TBrickInventory;
  s1: string;
  nextstr, prevstr: string;
  nextx, prevx: integer;
  kp: THashStringList;
  titstr: string;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
        for j := 0 to kp.Count - 1 do
        begin
          pcs := kp.Strings[j];
          pci := kp.Objects[j] as TPieceColorInfo;
          if pci <> nil then
          begin
            pi := db.PieceInfo(pci);
            weight := db.GetItemWeight(pci.piece, pci.color, pi);
            if weight > 0.0 then
            begin
              if pci.priceguide.nTimesSold > 0 then
              begin
                cost := pci.EvaluatePriceNew;
                if cost > 0.0 then
                begin
                  perkgr := dbl_safe_div(cost, weight) * 1000;
                  if perkgr >= x then
                  begin
                    lst.Add(pcs + ',' + itoa(i));
                    inv.AddLoosePartFast(pcs, i, 1, pci);
                  end;
                end;
              end;
            end;
          end;
        end;
    end;

  lst.Sort;

  if x > 1000 then
  begin
    prevx := round(x * 0.8);
    if prevx < 1000 then
      prevx := 1000;
    prevstr := '<a href=NewPiecesaboveeuroKgr/' + itoa(prevx) + '>New pieces with price above ' + itoa(prevx) + ' euro/Kgr</a><br>';
  end
  else
    prevstr := '';
  nextx := round(x * 1.25);
  nextstr := '<br><a href=NewPiecesaboveeuroKgr/' + itoa(nextx) + '>New pieces with price above ' + itoa(nextx) + ' euro/Kgr</a><br>';

  titstr := 'New pieces with price above ' + itoa(x) + ' euro/Kgr';
  DrawPieceList(prevstr + titstr + nextstr, lst, SORT_PRICE_NEW, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\NewPiecesabove' + IntToStrzFill(4, x) + 'euroKgr\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'NewPiecesabove' + IntToStrzFill(4, x) + 'euroKgr';
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListNew(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListNew(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListNew(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListNew(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListNew(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListNew(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListNew(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListNew(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_030%.xml', 0.3);

  inv.Free;

end;

procedure TMainForm.UsedPiecesabove1000euroKgr1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('UsedPiecesaboveeuroKgr/1000', foo);
end;

procedure TMainForm.UsedPiecesabove5000euroKgr1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('UsedPiecesaboveeuroKgr/5000', foo);
end;

procedure TMainForm.UsedPiecesabove10000euroKgr1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('UsedPiecesaboveeuroKgr/10000', foo);
end;

procedure TMainForm.NewPiecesbelow10euroKgr1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('NewPiecesbeloweuroKgr/10', foo);
end;

procedure TMainForm.NewPiecesbelow15euroKgr1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('NewPiecesbeloweuroKgr/15', foo);
end;

procedure TMainForm.NewPiecesbelow20euroKgr1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('NewPiecesbeloweuroKgr/20', foo);
end;

procedure TMainForm.NewPiecesbelow25euroKgr1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('NewPiecesbeloweuroKgr/25', foo);
end;

procedure TMainForm.NewPiecesbelow30euroKgr1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('NewPiecesbeloweuroKgr/30', foo);
end;

procedure TMainForm.NewPiecesabove1000euroKgr1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('NewPiecesaboveeuroKgr/1000', foo);
end;

procedure TMainForm.NewPiecesabove5000euroKgr1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('NewPiecesaboveeuroKgr/5000', foo);
end;

procedure TMainForm.NewPiecesabove10000euroKgr1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('NewPiecesaboveeuroKgr/10000', foo);
end;

procedure TMainForm.Newcheaperthanused1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('NewPiecesCheaperUsed', foo);
end;

procedure TMainForm.PriceGuideQry(const n: integer; const typ: string;
  const dosoldnew, dosoldused, doavailnew, doavailused: boolean);
var
  i, j, idx: integer;
  lst: TStringList;
  dd: TCInteger;
  pcs, scolor: string;
  pci: TPieceColorInfo;
  dnew, dused: boolean;
  snewused: string;
  inv: TBrickInventory;
  s1: string;
  dtit, stit, sprev, snext: string;
  blname1, blname2: string;
  kp: THashStringList;
  dominifig: boolean;
  sexclude: TStringList;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  sexclude := TStringList.Create;

  if (typ = 'P') or (typ = '') then
  begin
    sexclude.Text := S_PGQRY_EXCLUDE_P;
    sexclude.Sorted := True;
    for i := 0 to MAXINFOCOLOR do
      if db.Colors(i).id = i then
      begin
        scolor := itoa(i);
        kp := db.Colors(i).knownpieces;
        if kp <> nil then
          for j := 0 to kp.Count - 1 do
          begin
            pcs := kp.Strings[j];
            blname1 := db.GetBLNetPieceName(pcs) + ',' + scolor;
            blname2 := db.BricklinkPart(pcs) + ',' + scolor;
            if sexclude.IndexOf(pcs) < 0 then
              if sexclude.IndexOf(blname1) < 0 then
                if sexclude.IndexOf(blname2) < 0 then
                begin
                  pci := kp.Objects[j] as TPieceColorInfo;
                  if pci <> nil then
                  begin
                    pcs := pcs + ',' + scolor;
                    idx := lst.Add(pcs);
                    dd := TCInteger.Create;
                    if dosoldnew then
                      dd.Add(pci.priceguide.nTotalQty);
                    if dosoldused then
                      dd.Add(pci.priceguide.uTotalQty);
                    if doavailnew then
                      dd.Add(pci.availability.nTotalQty);
                    if doavailused then
                      dd.Add(pci.availability.uTotalQty);

                    lst.Objects[idx] := dd;
                    sexclude.Add(pcs);
                    if blname1 <> pcs then
                      sexclude.Add(blname1);
                    if blname2 <> pcs then
                      if blname2 <> blname1 then
                        sexclude.Add(blname2);
                  end;
                end;
          end;
      end;
  end
  else if (typ = 'S') or (typ = 'M') then
  begin
    dominifig := typ = 'M';
    if dominifig then
      sexclude.Text := S_PGQRY_EXCLUDE_M
    else
      sexclude.Text := S_PGQRY_EXCLUDE_S;
    sexclude.Sorted := True;
    kp := db.AllSets;
    for i := 0 to kp.Count - 1 do
    begin
      pcs := kp.Strings[i];
      if sexclude.IndexOf(pcs) < 0 then
      begin
        pci := db.PieceColorInfo(pcs, -1);
        if pci <> nil then
        begin
          if dominifig then
          begin
            if pci.sparttype <> 'M' then
              Continue;
          end
          else
          begin
            if pci.sparttype = 'M' then
              Continue;
          end;
          idx := lst.Add(pcs + ',-1');
          dd := TCInteger.Create;
          if dosoldnew then
            dd.Add(pci.priceguide.nTotalQty);
          if dosoldused then
            dd.Add(pci.priceguide.uTotalQty);
          if doavailnew then
            dd.Add(pci.availability.nTotalQty);
          if doavailused then
            dd.Add(pci.availability.uTotalQty);

          lst.Objects[idx] := dd;
        end;
      end;
    end;
  end;

  sexclude.Free;

  lst.CustomSort(sortpiecelist_TCInteger);
  for i := lst.Count - 1 downto n do
  begin
    lst.Objects[i].Free;
    lst.Delete(i);
  end;

  for i := 0 to lst.Count - 1 do
  begin
    splitstring(lst.Strings[i], pcs, scolor, ',');
    inv.AddLoosePartFast(pcs, atoi(scolor), 1);
  end;

  dnew := dosoldnew or doavailnew;
  dused := dosoldused or doavailused;
  if dnew and dused then
    snewused := 'New and Used '
  else if dnew then
    snewused := 'New '
  else if dused then
    snewused := 'Used ';

  if typ = 'P' then
    snewused := snewused + 'Pieces'
  else if typ = 'S' then
    snewused := snewused + 'Sets'
  else if typ = 'M' then
    snewused := snewused + 'Minifigs'
  else
    snewused := snewused + 'Items';

  if n > 1 then
  begin
    sprev := '<a href="PriceGuideQry/' + typ + '/' + Format('%d', [n - 1]) + '/' + itoa(btoi(dosoldnew)) + '/' + itoa(btoi(dosoldused)) + '/' + itoa(btoi(doavailnew)) + '/' + itoa(btoi(doavailused));
    sprev := sprev + '">' + itoa(n - 1) + ' ' + snewused + ' with ' + decide(dosoldnew or dosoldused, 'most items sold', 'bigger available qty') + '</a><br>';
  end
  else
    sprev := '';
  if n < 1000000 then
  begin
    snext := '<br><a href="PriceGuideQry/' + typ + '/' + Format('%d', [n + 1]) + '/' + itoa(btoi(dosoldnew)) + '/' + itoa(btoi(dosoldused)) + '/' + itoa(btoi(doavailnew)) + '/' + itoa(btoi(doavailused));
    snext := snext + '">' + itoa(n + 1) + ' ' + snewused + ' with ' + decide(dosoldnew or dosoldused, 'most items sold', 'bigger available qty') + '</a>';
  end
  else
    snext := '';

  stit := itoa(n) + ' ' + snewused + ' with ' + decide(dosoldnew or dosoldused, 'most items sold', 'bigger available qty');

  if dosoldnew or dosoldused then
    dtit := 'Items Sold'
  else
    dtit := 'Items Available';

  if dnew and dused then
    dtit := dtit + '<br>(New & Used)'
  else if dnew then
    dtit := dtit + '<br>(New)'
  else if dused then
    dtit := dtit + '<br>(Used)';

  DrawPieceList(sprev + stit + snext, lst, SORT_ITEMS_CINTEGER, dtit, stit);
  FreeList(lst);

  s1 := basedefault + 'out\PriceGuideQry\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'PriceGuideQry_' + typ + '_' + itoa(n) + '_' + itoa(btoi(dosoldnew)) + '_' + itoa(btoi(dosoldused)) + '_' + itoa(btoi(doavailnew)) + '_' + itoa(btoi(doavailused));
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListNew(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListNew(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListNew(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListNew(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListNew(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListNew(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListNew(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListNew(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_030%.xml', 0.3);

  inv.Free;
end;

procedure TMainForm.NewPiecesCheaperUsed;
var
  i, j: integer;
  lst: TStringList;
  pcs: string;
  pci: TPieceColorInfo;
  cost: double;
  cost2: double;
  inv: TBrickInventory;
  s1: string;
  kp: THashStringList;
  titstr: string;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
        for j := 0 to kp.Count - 1 do
        begin
          pcs := kp.Strings[j];
          pci := kp.Objects[j] as TPieceColorInfo;
          if pci <> nil then
          begin
            if (pci.priceguide.nTimesSold > 0) and (pci.priceguide.uTimesSold > 0) then
            begin
              cost := pci.EvaluatePriceNew;
              cost2 := pci.EvaluatePriceUsed;
              if cost < cost2 then
              begin
                lst.Add(pcs + ',' + itoa(i));
                inv.AddLoosePartFast(pcs, i, 1, pci);
              end;
            end;
          end;
        end;
    end;

  lst.Sort;
  titstr := 'New pieces cheaper than Used';
  DrawPieceList(titstr, lst, SORT_PRICE_NEW, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\NewPiecesCheaperUsed\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'NewPiecesCheaperUsed';
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListNew(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListNew(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListNew(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListNew(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListNew(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListNew(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListNew(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListNew(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_030%.xml', 0.3);

  inv.Free;
end;

procedure TMainForm.Slopes18degrees1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthqueryslopes/slope18', foo);
end;

procedure TMainForm.Slopes33degrees1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthqueryslopes/slope33', foo);
end;

procedure TMainForm.Slopes45degrees1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthqueryslopes/slope45', foo);
end;

procedure TMainForm.Slopes65degrees1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthqueryslopes/slope65', foo);
end;

procedure TMainForm.Slopes75degrees1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthqueryslopes/slope75', foo);
end;

procedure TMainForm.NewPiecesPriceAbove(const x: double);
var
  i, j: integer;
  lst: TStringList;
  pcs: string;
  pci: TPieceColorInfo;
  cost: double;
  inv: TBrickInventory;
  s1: string;
  sprev, snext: string;
  kp: THashStringList;
  titstr: string;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
        for j := 0 to kp.Count - 1 do
        begin
          pcs := kp.Strings[j];
          pci := kp.Objects[j] as TPieceColorInfo;
          if pci <> nil then
          begin
            if pci.priceguide.nTimesSold > 0 then
            begin
              cost := pci.EvaluatePriceNew;
              if cost >= x then
              begin
                lst.Add(pcs + ',' + itoa(i));
                inv.AddLoosePartFast(pcs, i, 1, pci);
              end;
            end;
          end;
        end;
    end;

  sprev := '<a href="NewPiecesPriceAbove/' + Format('%2.4f', [x / 2]) + '">New pieces with price greater than ' + Format('%2.4f', [x / 2]) + ' euro</a>';
  snext := '<a href="NewPiecesPriceAbove/' + Format('%2.4f', [x * 2]) + '">New pieces with price greater than ' + Format('%2.4f', [x * 2]) + ' euro</a>';

  lst.Sort;
  titstr := 'New pieces with price greater than ' + Format('%2.4f', [x]) + ' euro';
  DrawPieceList(sprev + '<br>' + titstr + '<br>' + snext, lst, SORT_PRICE_NEW, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\NewPiecesPriceAbove\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'NewPiecesPriceAbove_' + Format('%2.4f', [x]);
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListNew(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListNew(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListNew(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListNew(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListNew(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListNew(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListNew(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListNew(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_030%.xml', 0.3);

  inv.Free;
end;

procedure TMainForm.UsedPiecesPriceAbove(const x: double);
var
  i, j: integer;
  lst: TStringList;
  pcs: string;
  pci: TPieceColorInfo;
  cost: double;
  inv: TBrickInventory;
  s1: string;
  sprev, snext: string;
  kp: THashStringList;
  titstr: string;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
        for j := 0 to kp.Count - 1 do
        begin
          pcs := kp.Strings[j];
          pci := kp.Objects[j] as TPieceColorInfo;
          if pci <> nil then
          begin
            if pci.priceguide.uTimesSold > 0 then
            begin
              cost := pci.EvaluatePriceUsed;
              if cost >= x then
              begin
                lst.Add(pcs + ',' + itoa(i));
                inv.AddLoosePartFast(pcs, i, 1, pci);
              end;
            end;
          end;
        end;
    end;

  sprev := '<a href="UsedPiecesPriceAbove/' + Format('%2.4f', [x / 2]) + '">Used pieces with price greater than ' + Format('%2.4f', [x / 2]) + ' euro</a>';
  snext := '<a href="UsedPiecesPriceAbove/' + Format('%2.4f', [x * 2]) + '">Used pieces with price greater than ' + Format('%2.4f', [x * 2]) + ' euro</a>';

  lst.Sort;
  titstr := 'Used pieces with price greater than ' + Format('%2.4f', [x]) + ' euro';
  DrawPieceList(sprev + '<br>' + titstr + '<br>' + snext, lst, SORT_PRICE_USED, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\UsedPiecesPriceAbove\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'UsedPiecesPriceAbove_' + Format('%2.4f', [x]);
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListUsed(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListUsed(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListUsed(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListUsed(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListUsed(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListUsed(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListUsed(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListUsed(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_030%.xml', 0.3);

  inv.Free;
end;

procedure TMainForm.NewPiecesPriceAboveEvaluated(const x: double);
var
  i, j: integer;
  lst: TStringList;
  pcs: string;
  pci: TPieceColorInfo;
  cost: double;
  inv: TBrickInventory;
  s1: string;
  sprev, snext: string;
  kp: THashStringList;
  titstr: string;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
        for j := 0 to kp.Count - 1 do
        begin
          pcs := kp.Strings[j];
          pci := kp.Objects[j] as TPieceColorInfo;
          if pci <> nil then
          begin
            cost := pci.EvaluatePriceNew;
            if cost >= x then
            begin
              lst.Add(pcs + ',' + itoa(i));
              inv.AddLoosePartFast(pcs, i, 1, pci);
            end;
          end;
        end;
    end;

  sprev := '<a href="NewPiecesPriceAboveEvaluated/' + Format('%2.4f', [x / 2]) + '">New pieces with evaluated price greater than ' + Format('%2.4f', [x / 2]) + ' euro</a>';
  snext := '<a href="NewPiecesPriceAboveEvaluated/' + Format('%2.4f', [x * 2]) + '">New pieces with evaluated price greater than ' + Format('%2.4f', [x * 2]) + ' euro</a>';

  lst.Sort;
  titstr := 'New pieces with evaluated price greater than ' + Format('%2.4f', [x]) + ' euro';
  DrawPieceList(sprev + '<br>' + titstr + '<br>' + snext, lst, SORT_PRICE_NEW, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\NewPiecesPriceAboveEvaluated\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'NewPiecesPriceAboveEvaluated_' + Format('%2.4f', [x]);
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListNew(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListNew(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListNew(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListNew(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListNew(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListNew(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListNew(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListNew(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_030%.xml', 0.3);

  inv.Free;
end;

procedure TMainForm.UsedPiecesPriceAboveEvaluated(const x: double);
var
  i, j: integer;
  lst: TStringList;
  pcs: string;
  pci: TPieceColorInfo;
  cost: double;
  inv: TBrickInventory;
  s1: string;
  sprev, snext: string;
  kp: THashStringList;
  titstr: string;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
        for j := 0 to kp.Count - 1 do
        begin
          pcs := kp.Strings[j];
          pci := kp.Objects[j] as TPieceColorInfo;
          if pci <> nil then
          begin
            cost := pci.EvaluatePriceUsed;
            if cost >= x then
            begin
              lst.Add(pcs + ',' + itoa(i));
              inv.AddLoosePartFast(pcs, i, 1, pci);
            end;
          end;
        end;
    end;

  sprev := '<a href="UsedPiecesPriceAboveEvaluated/' + Format('%2.4f', [x / 2]) + '">Used pieces with evaluated price greater than ' + Format('%2.4f', [x / 2]) + ' euro</a>';
  snext := '<a href="UsedPiecesPriceAboveEvaluated/' + Format('%2.4f', [x * 2]) + '">Used pieces with evaluated price greater than ' + Format('%2.4f', [x * 2]) + ' euro</a>';

  lst.Sort;
  titstr := 'Used pieces with evaluated price greater than ' + Format('%2.4f', [x]) + ' euro';
  DrawPieceList(sprev + '<br>' + titstr + '<br>' + snext, lst, SORT_PRICE_USED, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\UsedPiecesPriceAboveEvaluated\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'UsedPiecesPriceAboveEvaluated_' + Format('%2.4f', [x]);
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListUsed(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListUsed(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListUsed(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListUsed(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListUsed(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListUsed(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListUsed(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListUsed(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_030%.xml', 0.3);

  inv.Free;
end;

procedure TMainForm.Newpieceswithpricegraterthan50euro1Click(
  Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('NewPiecesPriceAbove/50', foo);
end;

procedure TMainForm.Usedpieceswithpricegraterthan50euro1Click(
  Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('UsedPiecesPriceAbove/50', foo);
end;

procedure TMainForm.MostexpensivelotsofmyNEWparts1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('ShowExpensiveInvNew/Most expensive lots of my inventory (NEW)/10', foo);
end;

procedure TMainForm.MostexpensivelotsofmyUSEDparts1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('ShowExpensiveInvUsed/Most expensive lots of my inventory (USED)/10', foo);
end;

procedure TMainForm.InvertedSlopes33degrees1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthqueryslopes/invslope33', foo);
end;

procedure TMainForm.InvertedSlopes45degrees1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthqueryslopes/invslope45', foo);
end;

procedure TMainForm.InvertedSlopes75degrees1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthqueryslopes/invslope75', foo);
end;

procedure TMainForm.Checkstoragebinsreport1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('ShowCheckStorageReport', foo);
end;

procedure TMainForm.HTMLProcessing(Sender: TObject; ProcessingOn: Boolean);
begin
  ProgressBar1.Visible := ProcessingOn;
end;

procedure TMainForm.HTMLProgress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Integer);
begin
  ProgressBar1.Position := PercentDone;
end;

procedure TMainForm.Storage1Click(Sender: TObject);
var
  stid: string;
  foo: Boolean;
begin
  stid := '';
  if GetStorageID(stid) then
    HTMLClick('storage/' + stid, foo);
end;

procedure TMainForm.DoAddNewSetAsPiece(const pcs: string; const desc: string);
var
  i: integer;
  desc2: string;
  pcs2: string;
begin
  desc2 := Trim(RemoveSpecialTagsFromString(desc));
  if desc2 = '' then
    desc2 := Trim(RemoveSpecialTagsFromString(db.SetDesc(pcs)));

  for i := 1 to Length(desc2) do
    if desc2[i] = ',' then
      desc2[i] := ' ';

  pcs2 := Trim(pcs);
  for i := 1 to Length(pcs2) do
    if pcs2[i] = ',' then
      pcs2[i] := ' ';
  pcs2 := Trim(pcs2);

  db.SetMoldName(pcs2, desc2);
  db.AddMoldColor(pcs2, -1);
  db.RefreshPartCategory(pcs2);
end;

procedure TMainForm.AddNewSetAsPiece(const pcs: string; const desc: string);
var
  foo: Boolean;
  pcs2: string;
  i: integer;
begin
  Screen.Cursor := crHourglass;
  try
    pcs2 := Trim(pcs);
    for i := 1 to Length(pcs2) do
      if pcs2[i] = ',' then
        pcs2[i] := ' ';
    pcs2 := Trim(pcs2);

    if pcs2 <> '' then
    begin
      DoAddNewSetAsPiece(pcs2, desc);
      HTMLClick('spiece/' + pcs2, foo);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

function RefreshUnKnownSetsCategoryAndWeight_thr(p: pointer): integer; stdcall;
var
  params: iterator_double_p;
  i: integer;
  pcs: string;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
begin
  params := iterator_double_p(p);
  for i := 0 to db.AllPieces.Count - 1 do
    if i mod params.numidxs = params.idx then
    begin
      pcs := db.AllPieces.Strings[i];
      pi := db.AllPieces.Objects[i] as TPieceInfo;
      if db.IsValidPieceInfo(pi) then
        if (pi.category = 0) or (pi.weight = 0.0) then
        begin
          pci := db.PieceColorInfo(pcs, -1);
          if pci <> nil then
            params.list.Add(pcs);
        end;
    end;
  Result := 0;
end;

procedure TMainForm.RefreshUnKnownSetsCategoryAndWeight(const limit: integer);
var
  pi: TPieceInfo;
  pci: TPieceColorInfo;
  i: integer;
  pcs: string;
  cnt: integer;
  lst: TStringList;
  lst2: TStringList;
  rnd: integer;
begin
  ShowSplash;
  SplashProgress('Working...', 0);

  if usemultithread then
  begin
    lst := MT_Iterate_Base(@RefreshUnKnownSetsCategoryAndWeight_thr);
  end
  else
  begin
    lst := TStringList.Create;
    for i := 0 to db.AllPieces.Count - 1 do
    begin
      pcs := db.AllPieces.Strings[i];
      pi := db.AllPieces.Objects[i] as TPieceInfo;
      if db.IsValidPieceInfo(pi) then
        if (pi.category = 0) or (pi.weight = 0.0) then
        begin
          pci := db.PieceColorInfo(pcs, -1);
          if pci <> nil then
            lst.Add(pcs);
        end;
    end;
  end;

  lst2 := TStringList.Create;
  randomize;
  cnt := 0;
  for i := 0 to limit - 1 do
  begin
    if lst.Count = 0 then
      break;
    rnd := Random(lst.Count);
    lst2.Add(lst.Strings[rnd]);
    lst.Delete(rnd);
    inc(cnt);
    if cnt > limit then
      Break;
    SplashProgress('Working...', dbl_safe_div(cnt, limit));
  end;

  lst.Free;

  SplashProgress('Working...', 1);

  db.RefreshPartsCategory(lst2);
  lst2.Free;

  HideSplash;
end;

procedure TMainForm.RefreshUnKnownPiecesCategory(const limit: integer);
var
  pi: TPieceInfo;
  i: integer;
  pcs: string;
  cnt: integer;
  lst: TStringList;
  rnd: integer;
  kc: TDNumberList;
begin
  ShowSplash;
  SplashProgress('Working...', 0);

  lst := TStringList.Create;
  for i := 0 to db.AllPieces.Count - 1 do
  begin
    pcs := db.AllPieces.Strings[i];
    kc := db.GetMoldKnownColors(pcs);
    if kc.Count > 0 then
    begin
      pi := db.AllPieces.Objects[i] as TPieceInfo;
      if pi.category = 0 then
        lst.Add(pcs);
    end;
    kc.Free;
  end;

  randomize;
  cnt := 0;
  for i := 0 to limit - 1 do
  begin
    if lst.Count = 0 then
      break;
    rnd := Random(lst.Count);
    db.RefreshPartCategory(lst.Strings[rnd]);
    lst.Delete(rnd);
    inc(cnt);
    if cnt > limit then
      Break;
    SplashProgress('Working...', dbl_safe_div(cnt, limit));
  end;
  SplashProgress('Working...', 1);

  lst.Free;

  HideSplash;
end;

procedure TMainForm.RefreshUnKnownPiecesWeight(const limit: integer);
var
  pi: TPieceInfo;
  i: integer;
  pcs: string;
  cnt: integer;
  lst: TStringList;
  rnd: integer;
  kc: TDNumberList;
begin
  ShowSplash;
  SplashProgress('Working...', 0);

  lst := TStringList.Create;
  for i := 0 to db.AllPieces.Count - 1 do
  begin
    pcs := db.AllPieces.Strings[i];
    kc := db.GetMoldKnownColors(pcs);
    if kc.Count > 0 then
    begin
      pi := db.AllPieces.Objects[i] as TPieceInfo;
      if pi.weight <= 0.0 then
        lst.Add(pcs);
    end;
    kc.Free;
  end;

  randomize;
  cnt := 0;
  for i := 0 to limit - 1 do
  begin
    if lst.Count = 0 then
      break;
    rnd := Random(lst.Count);
    db.RefreshPartWeight(lst.Strings[rnd]);
    lst.Delete(rnd);
    inc(cnt);
    if cnt > limit then
      Break;
    SplashProgress('Working...', dbl_safe_div(cnt, limit));
  end;
  SplashProgress('Working...', 1);

  lst.Free;

  HideSplash;
end;

procedure TMainForm.RefreshUnKnownMinifigCategory(const limit: integer);
var
  pi: TPieceInfo;
  pci: TPieceColorInfo;
  i: integer;
  pcs: string;
  cnt: integer;
  lst: TStringList;
  rnd: integer;
begin
  ShowSplash;
  SplashProgress('Working...', 0);

  lst := TStringList.Create;
  for i := 0 to db.AllPieces.Count - 1 do
  begin
    pcs := db.AllPieces.Strings[i];
    pi := db.AllPieces.Objects[i] as TPieceInfo;
    if pi.category = 0 then
    begin
      pci := db.PieceColorInfo(pcs, -1);
      if pci <> nil then
        if pci.parttype = TYPE_MINIFIGURE then
          lst.Add(pcs);
    end;
  end;

  randomize;
  cnt := 0;
  for i := 0 to limit - 1 do
  begin
    if lst.Count = 0 then
      break;
    rnd := Random(lst.Count);
    db.RefreshPartCategory(lst.Strings[rnd]);
    lst.Delete(rnd);
    inc(cnt);
    if cnt > limit then
      Break;
    SplashProgress('Working...', dbl_safe_div(cnt, limit));
  end;
  SplashProgress('Working...', 1);

  lst.Free;

  HideSplash;
end;

procedure TMainForm.RefreshUnKnownMinifigWeight(const limit: integer);
var
  pi: TPieceInfo;
  pci: TPieceColorInfo;
  i: integer;
  pcs: string;
  cnt: integer;
  lst: TStringList;
  rnd: integer;
begin
  ShowSplash;
  SplashProgress('Working...', 0);

  lst := TStringList.Create;
  for i := 0 to db.AllPieces.Count - 1 do
  begin
    pcs := db.AllPieces.Strings[i];
    pi := db.AllPieces.Objects[i] as TPieceInfo;
    if pi.Weight <= 0.0 then
    begin
      pci := db.PieceColorInfo(pcs, -1);
      if pci <> nil then
        if pci.parttype = TYPE_MINIFIGURE then
          lst.Add(pcs);
    end;
  end;

  randomize;
  cnt := 0;
  for i := 0 to limit - 1 do
  begin
    if lst.Count = 0 then
      break;
    rnd := Random(lst.Count);
    db.RefreshMinifigWeight(lst.Strings[rnd]);
    lst.Delete(rnd);
    inc(cnt);
    if cnt > limit then
      Break;
    SplashProgress('Working...', dbl_safe_div(cnt, limit));
  end;
  SplashProgress('Working...', 1);

  lst.Free;

  HideSplash;
end;

procedure TMainForm.RefreshUnKnownInventoryPiecesCategory;
var
  pi: TPieceInfo;
  i: integer;
  pcs: string;
  cnt: integer;
  molds: TStringList;
begin
  ShowSplash;
  SplashProgress('Working...', 0);

  cnt := 0;
  molds := TStringList.Create;

  for i := 0 to inventory.numlooseparts - 1 do
  begin
    pcs := inventory.looseparts[i].part;
    if molds.IndexOf(pcs) < 0 then
      molds.Add(pcs);
  end;

  for i := 0 to molds.Count - 1 do
  begin
    pcs := molds.Strings[i];
    pi := db.PieceInfo(pcs);
    if pi.category = 0 then
    begin
      db.RefreshPartCategory(pcs);
      inc(cnt);
      SplashProgress('Working...', cnt / molds.Count);
    end;
  end;
  SplashProgress('Working...', 1);

  molds.Free;

  HideSplash;
end;

procedure TMainForm.Correntunknowncategoriesofmyinventory1Click(
  Sender: TObject);
var                                                 
  foo: Boolean;
begin
  HTMLClick('RefreshUnKnownInventoryPiecesCategory', foo);
end;

procedure TMainForm.Clearimagecache1Click(Sender: TObject);
var
  foo: boolean;
  i: integer;
begin
  Screen.Cursor := crHourglass;
  for i := 0 to streams.Count - 1 do
    streams.Objects[i].Free;
  streams.Clear;
  imagerequests.Free;
  imagerequests := THashStringList.Create;
  Screen.Cursor := crDefault;
  HTMLClick('refresh', foo);
end;

function PiecesDiscontinuedAtYear_thr(p: pointer): integer; stdcall;
var
  params: iterator_double_p;
  i, j: integer;
  pci: TPieceColorInfo;
  cp: colorinfo_p;
  kp: THashStringList;
  y: integer;
begin
  params := iterator_double_p(p);
  y := round(params.param) - 1;
  for i := 0 to MAXINFOCOLOR do
    if i mod params.numidxs = params.idx then
    begin
      cp := db.Colors(i);
      if cp.id = i then
      begin
        kp := cp.knownpieces;
        if kp <> nil then
        begin
          for j := 0 to kp.Count - 1 do
          begin
            pci := kp.Objects[j] as TPieceColorInfo;
            if pci <> nil then
            begin
              if pci.lastsetyear = y then
                params.list.AddObject(kp.Strings[j] + ',' + itoa(i), pci);
            end;
          end;
        end;
      end;
    end;
  Result := 0;
end;

procedure TMainForm.PiecesDiscontinuedAtYear(const y: integer);
var
  i, j: integer;
  lst: TStringList;
  pcs: string;
  scolor: string;
  pci: TPieceColorInfo;
  inv: TBrickInventory;
  s1: string;
  prevstr, nextstr: string;
  kp: THashStringList;
  titstr: string;
begin
  inv := TBrickInventory.Create;
  if usemultithread then
  begin
    lst := MT_Iterate_Double(@PiecesDiscontinuedAtYear_thr, y);
    for i := 0 to lst.Count - 1 do
    begin
      splitstring(lst.Strings[i], pcs, scolor, ',');
      inv.AddLoosePartFast(pcs, atoi(scolor), 1, lst.Objects[i]);
    end;
  end
  else
  begin
    lst := TStringList.Create;
    for i := 0 to MAXINFOCOLOR do
      if db.Colors(i).id = i then
      begin
        kp := db.Colors(i).knownpieces;
        if kp <> nil then
          for j := 0 to kp.Count - 1 do
          begin
            pcs := kp.Strings[j];
            pci := kp.Objects[j] as TPieceColorInfo;
            if pci <> nil then
            begin
              if pci.lastsetyear = y - 1 then
              begin
                lst.Add(pcs + ',' + itoa(i));
                inv.AddLoosePartFast(pcs, i, 1, pci);
              end;
            end;
          end;
      end;
  end;

  if y > 1931 then
    prevstr := '<a href=PiecesDiscontinuedAtYear/' + itoa(y - 1) + '>Pieces discontinued at year ' + itoa(y - 1) + '</a><br>'
  else
    prevstr := '';
  if y < atoi(FormatDateTime('yyyy', Now)) then
    nextstr := '<br><a href=PiecesDiscontinuedAtYear/' + itoa(y + 1) + '>Pieces discontinued at year ' + itoa(y + 1) + '</a><br>'
  else
    nextstr := '';

  lst.Sort;
  titstr := 'Pieces discontinued at year ' + itoa(y);
  DrawPieceList(prevstr + titstr + nextstr, lst, SORT_NONE, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\PiecesDiscontinuedAtYear\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'PiecesDiscontinuedAtYear_' + itoa(y);
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListNew(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListNew(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListNew(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListNew(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListNew(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListNew(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListNew(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListNew(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_030%.xml', 0.3);

  inv.Free;
end;

function PiecesNewAtYear_thr(p: pointer): integer; stdcall;
var
  params: iterator_double_p;
  i, j: integer;
  pci: TPieceColorInfo;
  cp: colorinfo_p;
  kp: THashStringList;
  y: integer;
begin
  params := iterator_double_p(p);
  y := round(params.param);
  for i := 0 to MAXINFOCOLOR do
    if i mod params.numidxs = params.idx then
    begin
      cp := db.Colors(i);
      if cp.id = i then
      begin
        kp := cp.knownpieces;
        if kp <> nil then
        begin
          for j := 0 to kp.Count - 1 do
          begin
            pci := kp.Objects[j] as TPieceColorInfo;
            if pci <> nil then
            begin
              if pci.firstsetyear = y then
                params.list.Add(kp.Strings[j] + ',' + itoa(i));
            end;
          end;
        end;
      end;
    end;
  Result := 0;
end;

procedure TMainForm.PiecesNewAtYear(const y: integer);
var
  i, j: integer;
  lst: TStringList;
  pcs: string;
  scolor: string;
  pci: TPieceColorInfo;
  inv: TBrickInventory;
  s1: string;
  prevstr, nextstr: string;
  kp: THashStringList;
  titstr: string;
begin
  inv := TBrickInventory.Create;
  if usemultithread then
  begin
    lst := MT_Iterate_Double(@PiecesNewAtYear_thr, y);
    for i := 0 to lst.Count - 1 do
    begin
      splitstring(lst.Strings[i], pcs, scolor, ',');
      inv.AddLoosePartFast(pcs, atoi(scolor), 1);
    end;
  end
  else
  begin
    lst := TStringList.Create;
    for i := 0 to MAXINFOCOLOR do
      if db.Colors(i).id = i then
      begin
        kp := db.Colors(i).knownpieces;
        if kp <> nil then
          for j := 0 to kp.Count - 1 do
          begin
            pcs := kp.Strings[j];
            pci := kp.Objects[j] as TPieceColorInfo;
            if pci <> nil then
            begin
              if pci.firstsetyear = y then
              begin
                lst.Add(pcs + ',' + itoa(i));
                inv.AddLoosePartFast(pcs, i, 1, pci);
              end;
            end;
          end;
      end;
  end;

  if y > 1931 then
    prevstr := '<a href=PiecesNewAtYear/' + itoa(y - 1) + '>Pieces first appeared at year ' + itoa(y - 1) + '</a><br>'
  else
    prevstr := '';
  if y < atoi(FormatDateTime('yyyy', Now)) then
    nextstr := '<br><a href=PiecesNewAtYear/' + itoa(y + 1) + '>Pieces first appeared at year ' + itoa(y + 1) + '</a><br>'
  else
    nextstr := '';

  lst.Sort;
  titstr := 'Pieces first appeared at year ' + itoa(y);
  DrawPieceList(prevstr + titstr + nextstr, lst, SORT_NONE, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\PiecesNewAtYear\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'PiecesNewAtYear_' + itoa(y);
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListNew(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListNew(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListNew(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListNew(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListNew(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListNew(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListNew(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListNew(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_030%.xml', 0.3);

  inv.Free;
end;

function PartIsVariation(const pcs: string): boolean;
var
  i, cnt: integer;
begin
  cnt := 0;
  for i := 1 to Length(pcs) do
    if not (pcs[i] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) then
      inc(cnt);
  Result := cnt >= 2;
end;

procedure TMainForm.PiecesDiscontinuedAtYearExcludingVariations(const y: integer);
var
  i, j: integer;
  lst: TStringList;
  pcs: string;
  pci: TPieceColorInfo;
  inv: TBrickInventory;
  s1: string;
  prevstr, nextstr: string;
  kp: THashStringList;
  titstr: string;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
        for j := 0 to kp.Count - 1 do
        begin
          pcs := kp.Strings[j];
          pci := kp.Objects[j] as TPieceColorInfo;
          if pci <> nil then
          begin
            if pci.lastsetyear = y - 1 then
              if not PartIsVariation(pcs) then
              begin
                lst.Add(pcs + ',' + itoa(i));
                inv.AddLoosePartFast(pcs, i, 1, pci);
              end;
          end;
        end;
    end;

  if y > 1931 then
    prevstr := '<a href=PiecesDiscontinuedAtYearExcludingVariations/' + itoa(y - 1) + '>Pieces discontinued at year ' + itoa(y - 1) + ' (Excluding variations)</a><br>'
  else
    prevstr := '';
  if y < atoi(FormatDateTime('yyyy', Now)) then
    nextstr := '<br><a href=PiecesDiscontinuedAtYearExcludingVariations/' + itoa(y + 1) + '>Pieces discontinued at year ' + itoa(y + 1) + ' (Excluding variations)</a><br>'
  else
    nextstr := '';

  lst.Sort;
  titstr := 'Pieces discontinued at year ' + itoa(y) + ' (Excluding variations)';
  DrawPieceList(prevstr + titstr + nextstr, lst, SORT_NONE, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\PiecesDiscontinuedAtYearExcludingVariations\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'PiecesDiscontinuedAtYearExcludingVariations_' + itoa(y);
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListNew(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListNew(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListNew(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListNew(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListNew(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListNew(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListNew(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListNew(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_030%.xml', 0.3);

  inv.Free;
end;

procedure TMainForm.PiecesNewAtYearExcludingVariations(const y: integer);
var
  i, j: integer;
  lst: TStringList;
  pcs: string;
  pci: TPieceColorInfo;
  inv: TBrickInventory;
  s1: string;
  prevstr, nextstr: string;
  kp: THashStringList;
  titstr: string;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
        for j := 0 to kp.Count - 1 do
        begin
          pcs := kp.Strings[j];
          pci := kp.Objects[j] as TPieceColorInfo;
          if pci <> nil then
          begin
            if pci.firstsetyear = y then
              if not PartIsVariation(pcs) then
              begin
                lst.Add(pcs + ',' + itoa(i));
                inv.AddLoosePartFast(pcs, i, 1, pci);
              end;
          end;
        end;
    end;

  if y > 1931 then
    prevstr := '<a href=PiecesNewAtYearExcludingVariations/' + itoa(y - 1) + '>Pieces first appeared at year ' + itoa(y - 1) + ' (Excluding variations)</a><br>'
  else
    prevstr := '';
  if y < atoi(FormatDateTime('yyyy', Now)) then
    nextstr := '<br><a href=PiecesNewAtYearExcludingVariations/' + itoa(y + 1) + '>Pieces first appeared at year ' + itoa(y + 1) + ' (Excluding variations)</a><br>'
  else
    nextstr := '';

  lst.Sort;
  titstr := 'Pieces first appeared at year ' + itoa(y) + ' (Excluding variations)';
  DrawPieceList(prevstr + titstr + nextstr, lst, SORT_NONE, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\PiecesNewAtYearExcludingVariations\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'PiecesNewAtYearExcludingVariations_' + itoa(y);
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListNew(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListNew(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListNew(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListNew(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListNew(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListNew(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListNew(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListNew(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_030%.xml', 0.3);

  inv.Free;
end;

procedure TMainForm.Piecesfirstappearedinyear1Click(Sender: TObject);
var
  yyyy: integer;
  foo: boolean;
begin
  yyyy := atoi(FormatDateTime('yyyy', Now));
  if InputInteger('First appeared pieces', 'Year', yyyy) then
    HTMLClick('PiecesNewAtYear/' + itoa(yyyy), foo);
end;

procedure TMainForm.Piecesdiscontinuedatyear1Click(Sender: TObject);
var
  yyyy: integer;
  foo: boolean;
begin
  yyyy := atoi(FormatDateTime('yyyy', Now));
  if InputInteger('Discontinued pieces', 'Year', yyyy) then
    HTMLClick('PiecesDiscontinuedAtYear/' + itoa(yyyy), foo);
end;

procedure TMainForm.FirstappearedExcludingvariations1Click(
  Sender: TObject);
var
  yyyy: integer;
  foo: boolean;
begin
  yyyy := atoi(FormatDateTime('yyyy', Now));
  if InputInteger('First appeared pieces', 'Year', yyyy) then
    HTMLClick('PiecesNewAtYearExcludingVariations/' + itoa(yyyy), foo);
end;

procedure TMainForm.DiscontinuedAllpartsExcl1Click(Sender: TObject);
var
  yyyy: integer;
  foo: boolean;
begin
  yyyy := atoi(FormatDateTime('yyyy', Now));
  if InputInteger('Discontinued pieces', 'Year', yyyy) then
    HTMLClick('PiecesDiscontinuedAtYearExcludingVariations/' + itoa(yyyy), foo);
end;

procedure TMainForm.ShowUniquePiecesOfMyInventory(const ntimes: integer);
var
  i, j: integer;
  lst: TStringList;
  pci: TPieceColorInfo;
  cnt: integer;
  setid: string;
  titstr: string;
begin
  lst := TStringList.Create;

  for i := 0 to inventory.numlooseparts - 1 do
  begin
    pci := db.PieceColorInfo(@inventory.looseparts[i]);
    if pci <> nil then
      if pci.sets <> nil then
      begin
        cnt := 0;
        setid := '';
        for j := 0 to pci.sets.Count - 1 do
          if not db.IsMoc(pci.sets.Strings[j]) then
          begin
            inc(cnt);
            setid := pci.sets.Strings[j];
            if cnt > ntimes then
              break;
          end;
        if cnt = ntimes then
        begin
          if ntimes = 1 then
            lst.AddObject(inventory.looseparts[i].part + ',' + itoa(inventory.looseparts[i].color) + ',' + setid, pci)
          else
            lst.AddObject(inventory.looseparts[i].part + ',' + itoa(inventory.looseparts[i].color), pci);
        end;
      end;
  end;

  lst.Sort;
  if ntimes = 1 then
    DrawPieceListSet('Pieces of my inventory that appear only in 1 official set', 'Unique Set', lst)
  else
  begin
    titstr := 'Pieces of my inventory that appear only in ' + itoa(ntimes) + ' official sets';
    DrawPieceList(titstr, lst, SORT_NONE, '', titstr);
  end;
  lst.Free;
end;

procedure TMainForm.Appersin1set1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('ShowUniquePiecesOfMyInventory/1', foo);
end;

procedure TMainForm.Appearsin2sets1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('ShowUniquePiecesOfMyInventory/2', foo);
end;

procedure TMainForm.Appearsin3sets1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('ShowUniquePiecesOfMyInventory/3', foo);
end;

procedure TMainForm.MostexpensiveofmypartsNEW1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('ShowExpensiveInvPartsNew/Most expensive parts of my inventory (NEW)/10', foo);
end;

procedure TMainForm.MostexpensiveofmypartsUSED1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('ShowExpensiveInvPartsUsed/Most expensive parts of my inventory (USED)/10', foo);
end;

procedure TMainForm.SetstobuyforminifigsNEW1Click(Sender: TObject);
var
  foo: Boolean;
begin
  if QueryMinifigPartOutParameters(
    qpm_minyear,
    qpm_minavailablelots,
    qpm_minminifigs,
    qpm_mindemand,
    qpm_minpartsmultiplier,
    qpm_minminifigsmultiplier,
    qpm_partoutmultiplier) then
    HTMLClick('ShowSetsForPartOutWithMiniFigsNew/' +
        itoa(qpm_minyear) + '/' +
        itoa(qpm_minavailablelots) + '/' +
        itoa(qpm_minminifigs) + '/' +
        itoa(round(qpm_mindemand * 100)) + '/' +
        itoa(round(qpm_minpartsmultiplier * 100)) + '/' +
        itoa(round(qpm_minminifigsmultiplier * 100)) + '/' +
        itoa(round(qpm_partoutmultiplier * 100)),
      foo);
end;

procedure TMainForm.SetstobuyforminifigsUSED1Click(Sender: TObject);
var
  foo: Boolean;
begin
  if QueryMinifigPartOutParameters(
    qpm_minyear,
    qpm_minavailablelots,
    qpm_minminifigs,
    qpm_mindemand,
    qpm_minpartsmultiplier,
    qpm_minminifigsmultiplier,
    qpm_partoutmultiplier) then
    HTMLClick('ShowSetsForPartOutWithMiniFigsUsed/' +
        itoa(qpm_minyear) + '/' +
        itoa(qpm_minavailablelots) + '/' +
        itoa(qpm_minminifigs) + '/' +
        itoa(round(qpm_mindemand * 100)) + '/' +
        itoa(round(qpm_minpartsmultiplier * 100)) + '/' +
        itoa(round(qpm_minminifigsmultiplier * 100)) + '/' +
        itoa(round(qpm_partoutmultiplier * 100)),
      foo);
end;

procedure TMainForm.PiecesNewAtYear_Minifigure(const y: integer);
var
  i, j: integer;
  lst: TStringList;
  pcs: string;
  pci: TPieceColorInfo;
  inv: TBrickInventory;
  s1: string;
  scheck: string;
  p: integer;
  pcat: integer;
  prevstr, nextstr: string;
  kp: THashStringList;
  titstr: string;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
        for j := 0 to kp.Count - 1 do
        begin
          pcs := kp.Strings[j];
          scheck := UpperCase(pcs);
          pcat := 0;
          p := Pos('970C', scheck);
          if p <> 1 then
          begin
            p := Pos('3626C', scheck);
            if p <> 1 then
            begin
              p := Pos('973P', scheck);
              if p <> 1 then
                pcat := db.PieceInfo(pcs).category;
            end;
          end;
          if (p = 1) or (pcat = 20) or (pcat = 142) or (pcat = 857) or (pcat = 16) or (pcat = 636) or (pcat = 484) or (pcat = 418) or (pcat = 485) or (pcat = 18) or (pcat = 19) then
          begin
            pci := kp.Objects[j] as TPieceColorInfo;
            if pci <> nil then
            begin
              if pci.firstsetyear = y then
                if not PartIsVariation(pcs) then
                begin
                  lst.Add(pcs + ',' + itoa(i));
                  inv.AddLoosePartFast(pcs, i, 1, pci);
                end;
            end;
          end;
        end;
    end;

  if y > 1931 then
    prevstr := '<a href=MinifigurePiecesNewAtYear/' + itoa(y - 1) + '>Minifigure parts first appeared at year ' + itoa(y - 1) + '</a><br>'
  else
    prevstr := '';
  if y < atoi(FormatDateTime('yyyy', Now)) then
    nextstr := '<br><a href=MinifigurePiecesNewAtYear/' + itoa(y + 1) + '>Minifigure parts first appeared at year ' + itoa(y + 1) + '</a><br>'
  else
    nextstr := '';

  lst.Sort;
  titstr := 'Minifigure parts first appeared at year ' + itoa(y);
  DrawPieceList(prevstr + titstr + nextstr, lst, SORT_NONE, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\PiecesNewAtYear_Minifigure\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'PiecesNewAtYear_Minifigure_' + itoa(y);
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListNew(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListNew(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListNew(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListNew(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListNew(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListNew(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListNew(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListNew(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_030%.xml', 0.3);

  inv.Free;
end;

procedure TMainForm.PiecesDiscontinuedAtYear_Minifigure(const y: integer);
var
  i, j: integer;
  lst: TStringList;
  pcs: string;
  pci: TPieceColorInfo;
  inv: TBrickInventory;
  s1: string;
  scheck: string;
  p: integer;
  pcat: integer;
  prevstr, nextstr: string;
  kp: THashStringList;
  titstr: string;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
        for j := 0 to kp.Count - 1 do
        begin
          pcs := kp.Strings[j];
          scheck := UpperCase(pcs);
          pcat := 0;
          p := Pos('970C', scheck);
          if p <> 1 then
          begin
            p := Pos('3626C', scheck);
            if p <> 1 then
            begin
              p := Pos('973P', scheck);
              if p <> 1 then
                pcat := db.PieceInfo(pcs).category;
            end;
          end;
          if (p = 1) or (pcat = 20) or (pcat = 142) or (pcat = 857) or (pcat = 16) or (pcat = 636) or (pcat = 484) or (pcat = 418) or (pcat = 485) or (pcat = 18) or (pcat = 19) then
          begin
            pci := kp.Objects[j] as TPieceColorInfo;
            if pci <> nil then
            begin
              if pci.lastsetyear = y - 1 then
              begin
                lst.Add(pcs + ',' + itoa(i));
                inv.AddLoosePartFast(pcs, i, 1, pci);
              end;
            end;
          end;
        end;
    end;

  if y > 1931 then
    prevstr := '<a href=MinifigurePiecesDiscontinuedAtYear/' + itoa(y - 1) + '>Minifigure parts discontinued at year ' + itoa(y - 1) + '</a><br>'
  else
    prevstr := '';
  if y < atoi(FormatDateTime('yyyy', Now)) then
    nextstr := '<br><a href=MinifigurePiecesDiscontinuedAtYear/' + itoa(y + 1) + '>Minifigure parts discontinued at year ' + itoa(y + 1) + '</a><br>'
  else
    nextstr := '';

  lst.Sort;
  titstr := 'Minifigure parts discontinued at year ' + itoa(y);
  DrawPieceList(prevstr + titstr + nextstr, lst, SORT_NONE, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\MinifigurePiecesDiscontinuedAtYear\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'MinifigurePiecesDiscontinuedAtYear' + itoa(y);
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListNew(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListNew(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListNew(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListNew(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListNew(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListNew(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListNew(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListNew(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_030%.xml', 0.3);

  inv.Free;
end;

procedure TMainForm.FirstappearedMinifigureparts1Click(Sender: TObject);
var
  yyyy: integer;
  foo: boolean;
begin
  yyyy := atoi(FormatDateTime('yyyy', Now));
  if InputInteger('First appeared minifigure pieces', 'Year', yyyy) then
    HTMLClick('MinifigurePiecesDiscontinuedAtYear/' + itoa(yyyy), foo);
end;

procedure TMainForm.DiscontinuedMinifigureparts1Click(Sender: TObject);
var
  yyyy: integer;
  foo: boolean;
begin
  yyyy := atoi(FormatDateTime('yyyy', Now));
  if InputInteger('Discontinued minifigure pieces', 'Year', yyyy) then
    HTMLClick('MinifigurePiecesDiscontinuedAtYear/' + itoa(yyyy), foo);
end;

procedure TMainForm.Piece2Click(Sender: TObject);
var
  pieceid: string;
  foo: Boolean;
begin
  pieceid := '';
//  if GetExistingPieceID(pieceid) then
  if GetPieceID(pieceid) then
    if EditMold(pieceid) then
      HTMLClick('spiece/' + pieceid, foo);
end;

procedure TMainForm.Rebrickablecsv1Click(Sender: TObject);
var
  inv: TBrickInventory;
  foo: boolean;
begin
  if OpenDialog2.Execute then
  begin
    inv := TBrickInventory.Create;
    if inv.LoadFromRebrickableFile(OpenDialog2.FileName) then
    begin
      inventory.MergeWith(inv);
      btn_SaveClick(nil);
      HTMLClick('refresh', foo);
    end;
    inv.Free;
  end;
end;

procedure TMainForm.Newpieceswithpricegreaterthan50eurosoldandavailable1Click(
  Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('NewPiecesPriceAboveEvaluated/50', foo);
end;

procedure TMainForm.Usedpieceswithpricegreaterthan50eurosoldandavailable1Click(
  Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('UsedPiecesPriceAboveEvaluated/50', foo);
end;

procedure TMainForm.Exportunknownpieces1Click(Sender: TObject);
var
  lst: TStringList;
begin
  if SaveDialog1.Execute then
  begin
    Screen.Cursor := crHourglass;
    lst := db.GetUnknownPiecesFromCache;
    backupfile(SaveDialog1.FileName);
    lst.SaveToFile(SaveDialog1.FileName);
    Screen.Cursor := crDefault;
    lst.Free;
  end;
  ChDir(basedefault);
end;

procedure TMainForm.ClearStubEntries1Click(Sender: TObject);
var
  x: integer;
begin
  Screen.Cursor := crHourglass;
  x := db.RemoveUnknownPiecesFromCache;
  Screen.Cursor := crDefault;
  ShowMessage(Format('%d stub entries cleared', [x]));
end;

procedure TMainForm.Reorganize1Click(Sender: TObject);
var
  foo: boolean;
begin
  Screen.Cursor := crHourglass;
  ShowSplash;
  db.CacheDB.Reorganize;
  HideSplash;
  Screen.Cursor := crDefault;
  CheckCacheHashEfficiency1Click(Sender);
  HTMLClick('refresh', foo);
end;

procedure TMainForm.Sets3Click(Sender: TObject);
var
  sl: TStringList;
  i: integer;
  foo: boolean;
begin
  sl := TStringList.Create;
  if GetUpdateLinks1(sl) then
  begin
    Screen.Cursor := crHourglass;
    for i := 0 to sl.Count - 1 do
      HTMLClick(sl.Strings[i], foo);
    S_FlashFileSystem;
    Screen.Cursor := crDefault;
  end;
  sl.Free;
end;

procedure TMainForm.Partnamesrebrickable1Click(Sender: TObject);
var
  sl: TStringList;
  i: integer;
  foo: boolean;
begin
  sl := TStringList.Create;
  if UpdatePartNamesFromRebrickable(sl) then
  begin
    Screen.Cursor := crHourglass;
    for i := 0 to sl.Count - 1 do
      HTMLClick(sl.Strings[i], foo);
    S_FlashFileSystem;
    Screen.Cursor := crDefault;
  end;
  sl.Free;
end;

procedure TMainForm.Partswithoutknowncolors1Click(Sender: TObject);
var
  sl: TStringList;
  i: integer;
  foo: boolean;
begin
  sl := TStringList.Create;
  if UpdatePartColorsFromBricklink(sl) then
  begin
    Screen.Cursor := crHourglass;
    for i := 0 to sl.Count - 1 do
      HTMLClick(sl.Strings[i], foo);
    S_FlashFileSystem;
    Screen.Cursor := crDefault;
  end;
  sl.Free;
end;


procedure TMainForm.Newismuchmoreexpensivethanused1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('NewPiecesMuchMoreExpensiveThanUsed/20.000', foo);
end;

procedure TMainForm.NewPiecesMuchMoreExpensiveThanUsed(const factor: double);
var
  i, j: integer;
  lst: TStringList;
  pcs: string;
  pci: TPieceColorInfo;
  cost: double;
  cost2: double;
  inv: TBrickInventory;
  s1: string;
  ffmt: string;
  ffmt_prev: string;
  ffmt_next: string;
  title, titstr: string;
  kp: THashStringList;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
        for j := 0 to kp.Count - 1 do
        begin
          pcs := kp.Strings[j];
          pci := kp.Objects[j] as TPieceColorInfo;
          if pci <> nil then
          begin
            if (pci.priceguide.nTimesSold > 0) and (pci.priceguide.uTimesSold > 0) then
            begin
              cost := pci.EvaluatePriceNew;
              cost2 := pci.EvaluatePriceUsed;
              if cost >= cost2 * factor then
              begin
                lst.Add(pcs + ',' + itoa(i));
                inv.AddLoosePartFast(pcs, i, 1, pci);
              end;
            end;
          end;
        end;
    end;

  ffmt := Format('%2.3f', [factor]);
  ffmt_prev := Format('%2.3f', [factor * 0.8]);
  ffmt_next := Format('%2.3f', [factor * 1.25]);
  titstr := 'New pieces ' + ffmt + ' X more expensive than used';
  title := '<a href=NewPiecesMuchMoreExpensiveThanUsed/' + ffmt_prev + '>New pieces ' + ffmt_prev + ' X more expensive than used</a><br>' +
           titstr + '<br>' +
           '<a href=NewPiecesMuchMoreExpensiveThanUsed/' + ffmt_next + '>New pieces ' + ffmt_next + ' X more expensive than used</a>';

  lst.Sort;
  DrawPieceList(title, lst, SORT_NONE, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\NewPiecesMuchMoreExpensiveThanUsed\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'NewPiecesMuchMoreExpensiveThanUsed_' + ffmt;
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListNew(s1 + '_NEW_030%.xml', 0.3);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_USED_030%.xml', 0.3);

  inv.Free;
end;

procedure TMainForm.Newpartsbricklinkcom1Click(Sender: TObject);
var
  sl: TStringList;
  i: integer;
  foo: boolean;
begin
  sl := TStringList.Create;
  if UpdateNewPartsFromBricklink(sl) then
  begin
    Screen.Cursor := crHourglass;
    for i := 0 to sl.Count - 1 do
      HTMLClick(sl.Strings[i], foo);
    S_FlashFileSystem;
    Screen.Cursor := crDefault;
  end;
  sl.Free;
end;

function sortmoldlist_numcolors(List: TStringList; Index1, Index2: Integer): Integer;
var
  c1, c2: integer;
begin
  c1 := db.GetMoldNumColors(List.Strings[Index1]);
  c2 := db.GetMoldNumColors(List.Strings[Index2]);
  Result := c1 - c2;
end;

type
  ShowMoldsWithMoreThanColorsParams_t = record
    start, stop: integer;
    mincolors, maxcolors: integer;
    list: TStringList;
  end;
  ShowMoldsWithMoreThanColorsParams_p = ^ShowMoldsWithMoreThanColorsParams_t;

function ShowMoldsWithMoreThanColors_thr(p: pointer): integer; stdcall;
var
  parms: ShowMoldsWithMoreThanColorsParams_p;
  i: integer;
begin
  parms := p;
  for i := parms.start to parms.stop do
    if db.GetMoldNumColors(db.AllPieces.Strings[i]) >= parms.mincolors then
      if db.GetMoldNumColors(db.AllPieces.Strings[i]) <= parms.maxcolors then
        parms.list.Add(db.AllPieces.Strings[i]);
  Result := 1;
end;

procedure TMainForm.ShowMoldsWithNumColors(const ncolors: integer);
begin
  ShowMoldsWithNumColorsBetween(ncolors, ncolors);
end;

procedure TMainForm.ShowMoldsWithMoreThanColors(const ncolors: integer);
begin
  ShowMoldsWithNumColorsBetween(ncolors, MAXINT);
end;

procedure TMainForm.ShowMoldsWithNumColorsBetween(const mincolors, maxcolors: integer);
var
  i: integer;
  cmolds: TStringList;
  cmolds2: TStringList;
  cmolds3: TStringList;
  cmolds4: TStringList;
  params1: ShowMoldsWithMoreThanColorsParams_t;
  params2: ShowMoldsWithMoreThanColorsParams_t;
  params3: ShowMoldsWithMoreThanColorsParams_t;
  params4: ShowMoldsWithMoreThanColorsParams_t;
  tit: string;
  cnt: integer;
  step: integer;
  donumlinks: boolean;
  titstr: string;
begin
  Screen.Cursor := crHourglass;
  cmolds := TStringList.Create;
  try
    cnt := db.AllPieces.Count;
    if usemultithread and (cnt > 127) then
    begin
      cmolds2 := TStringList.Create;
      cmolds3 := TStringList.Create;
      cmolds4 := TStringList.Create;

      step := cnt div 4;

      params1.start := 0;
      params1.stop := step;
      params1.mincolors := mincolors;
      params1.maxcolors := maxcolors;
      params1.list := cmolds;

      params2.start := params1.stop + 1;
      params2.stop := params2.start + step;
      params2.mincolors := mincolors;
      params2.maxcolors := maxcolors;
      params2.list := cmolds2;

      params3.start := params2.stop + 1;
      params3.stop := params3.start + step;
      params3.mincolors := mincolors;
      params3.maxcolors := maxcolors;
      params3.list := cmolds3;

      params4.start := params3.stop + 1;
      params4.stop := cnt - 1;
      params4.mincolors := mincolors;
      params4.maxcolors := maxcolors;
      params4.list := cmolds4;

      MT_Execute(
        @ShowMoldsWithMoreThanColors_thr, @params1,
        @ShowMoldsWithMoreThanColors_thr, @params2,
        @ShowMoldsWithMoreThanColors_thr, @params3,
        @ShowMoldsWithMoreThanColors_thr, @params4);

      cmolds.AddStrings(cmolds2);
      cmolds.AddStrings(cmolds3);
      cmolds.AddStrings(cmolds4);

      cmolds2.Free;
      cmolds3.Free;
      cmolds4.Free;
    end
    else
    begin
      for i := 0 to cnt - 1 do
        if db.GetMoldNumColors(db.AllPieces.Strings[i]) >= mincolors then
          if db.GetMoldNumColors(db.AllPieces.Strings[i]) <= maxcolors then
            cmolds.Add(db.AllPieces.Strings[i]);
    end;

    tit := '';
    donumlinks := True;
    if maxcolors = MAXINT then
    begin
      titstr := 'Molds with more than ' + itoa(mincolors) + ' known colors';
      if mincolors > 1 then
        tit := tit + '<a href="ShowMoldsWithMoreThanColors/' + itoa(mincolors - 1) + '">' + titstr + ' known colors</a><br>';
      tit := tit + titstr + '<br>';
      if mincolors < db.maximumcolors then
        tit := tit + '<a href="ShowMoldsWithMoreThanColors/' + itoa(mincolors + 1) + '">Molds with more than ' + itoa(mincolors + 1) + ' known colors</a><br>';
    end
    else if mincolors = maxcolors then
    begin
      donumlinks := False;
      titstr := 'Molds with ' + itoa(mincolors) + ' known colors';
      if mincolors > 1 then
        tit := tit + '<a href="ShowMoldsWithNumColors/' + itoa(mincolors - 1) + '">Molds with ' + itoa(mincolors - 1) + ' known colors</a><br>';
      tit := tit + titstr + '<br>';
      if mincolors < db.maximumcolors then
        tit := tit + '<a href="ShowMoldsWithNumColors/' + itoa(mincolors + 1) + '">Molds with ' + itoa(mincolors + 1) + ' known colors</a><br>';
    end;

    if mincolors <> maxcolors then
      cmolds.CustomSort(sortmoldlist_numcolors);
    DrawMoldList(tit, cmolds, False, donumlinks, titstr);

  finally
    cmolds.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.ShowChildMolds(const basepcs: string);
var
  cmolds: TStringList;
  linkstr, titstr: string;
begin
  cmolds := db.ChildMolds(basepcs);
  linkstr := '<a href=spiece/' + basepcs + '>' + basepcs + '</a> ' + MakeThumbnailImage2(basepcs);
  titstr := 'Child molds for ' + basepcs;
  if cmolds = nil then
  begin
    cmolds := TStringList.Create;
    DrawMoldList('No child molds found for ' + linkstr, cmolds, False, False, titstr);
    cmolds.Free;
  end
  else
    DrawMoldList('Child molds for ' + linkstr, cmolds, False, False, titstr);
end;

procedure TMainForm.ShowFamilyMolds(const basepcs: string);
var
  cmolds: TStringList;
  linkstr, titstr: string;
begin
  cmolds := db.FamilyMolds(basepcs);
  linkstr := '<a href=spiece/' + basepcs + '>' + basepcs + '</a> ' + MakeThumbnailImage2(basepcs);
  titstr := 'Variations and/or other prints for ' + basepcs;
  if cmolds = nil then
  begin
    cmolds := TStringList.Create;
    DrawMoldList('No variations and/or other prints for ' + linkstr, cmolds, False, False, titstr);
    cmolds.Free;
  end
  else
    DrawMoldList('Variations and/or other prints for ' + linkstr, cmolds, False, False, titstr);
end;

procedure TMainForm.ShowMoldVariations(const basepcs: string);
var
  linkstr, titstr: string;
  pi: TPieceInfo;
begin
  pi := db.PieceInfo(basepcs);
  linkstr := '<a href=spiece/' + basepcs + '>' + basepcs + '</a> ' + MakeThumbnailImage2(basepcs);
  titstr := 'Mold variations for ' + basepcs;
  if pi.moldvariations.Count = 0 then
    DrawMoldList('No mold variations for ' + linkstr, pi.moldvariations, False, False, titstr)
  else
    DrawMoldList('Mold variations for ' + linkstr, pi.moldvariations, False, False, titstr);
end;

procedure TMainForm.ShowPieceAlternates(const basepcs: string);
var
  linkstr, titstr: string;
  pi: TPieceInfo;
begin
  pi := db.PieceInfo(basepcs);
  linkstr := '<a href=spiece/' + basepcs + '>' + basepcs + '</a> ' + MakeThumbnailImage2(basepcs);
  titstr := 'Alternates for ' + basepcs;
  if pi.alternates.Count = 0 then
    DrawMoldList('No alternates for ' + linkstr, pi.alternates, False, False, titstr)
  else
    DrawMoldList('Alternates for ' + linkstr, pi.alternates, False, False, titstr);
end;

procedure TMainForm.ShowPiecePatterns(const basepcs: string);
var
  linkstr, titstr: string;
  pi: TPieceInfo;
begin
  pi := db.PieceInfo(basepcs);
  linkstr := '<a href=spiece/' + basepcs + '>' + basepcs + '</a> ' + MakeThumbnailImage2(basepcs);
  titstr := 'Patterns for ' + basepcs;
  if pi.patterns.Count = 0 then
    DrawMoldList('No patterns for ' + linkstr, pi.patterns, False, False, titstr)
  else
    DrawMoldList('Patterns for ' + linkstr, pi.patterns, False, False, titstr);
end;

procedure TMainForm.ShowPiecePrints(const basepcs: string);
var
  linkstr, titstr: string;
  pi: TPieceInfo;
begin
  pi := db.PieceInfo(basepcs);
  linkstr := '<a href=spiece/' + basepcs + '>' + basepcs + '</a> ' + MakeThumbnailImage2(basepcs);
  titstr := 'Prints for ' + basepcs;
  if pi.prints.Count = 0 then
    DrawMoldList('No prints for ' + linkstr, pi.prints, False, False, titstr)
  else
    DrawMoldList('Prints for ' + linkstr, pi.prints, False, False, titstr);
end;

procedure TMainForm.Pieceswithmorethan30colors1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('ShowMoldsWithMoreThanColors/30', foo);
end;

type
  ShowNameswithbothpartandsetcolorindexesParams_t = record
    start, stop: integer;
    list: TStringList;
  end;
  ShowNameswithbothpartandsetcolorindexesParams_p = ^ShowNameswithbothpartandsetcolorindexesParams_t;

function ShowNameswithbothpartandsetcolorindexes_thr(p: pointer): integer; stdcall;
var
  parms: ShowNameswithbothpartandsetcolorindexesParams_p;
  i: integer;
begin
  parms := p;
  for i := parms.start to parms.stop do
    if (db.GetMoldColorsFlags(db.AllPieces.Strings[i]) and (COLORFLAG_SET or COLORFLAG_PART)) = (COLORFLAG_SET or COLORFLAG_PART) then
      parms.list.Add(db.AllPieces.Strings[i]);
  Result := 1;
end;

procedure TMainForm.ShowNameswithbothpartandsetcolorindexes;
var
  i: integer;
  cmolds: TStringList;
  cmolds2: TStringList;
  cmolds3: TStringList;
  cmolds4: TStringList;
  params1: ShowNameswithbothpartandsetcolorindexesParams_t;
  params2: ShowNameswithbothpartandsetcolorindexesParams_t;
  params3: ShowNameswithbothpartandsetcolorindexesParams_t;
  params4: ShowNameswithbothpartandsetcolorindexesParams_t;
  tit, titstr: string;
  cnt: integer;
  step: integer;
begin
  Screen.Cursor := crHourglass;
  cmolds := TStringList.Create;
  try
    cnt := db.AllPieces.Count;
    if usemultithread and (cnt > 127) then
    begin
      cmolds2 := TStringList.Create;
      cmolds3 := TStringList.Create;
      cmolds4 := TStringList.Create;

      step := cnt div 4;

      params1.start := 0;
      params1.stop := step;
      params1.list := cmolds;

      params2.start := params1.stop + 1;
      params2.stop := params2.start + step;
      params2.list := cmolds2;

      params3.start := params2.stop + 1;
      params3.stop := params3.start + step;
      params3.list := cmolds3;

      params4.start := params3.stop + 1;
      params4.stop := cnt - 1;
      params4.list := cmolds4;

      MT_Execute(
        @ShowNameswithbothpartandsetcolorindexes_thr, @params1,
        @ShowNameswithbothpartandsetcolorindexes_thr, @params2,
        @ShowNameswithbothpartandsetcolorindexes_thr, @params3,
        @ShowNameswithbothpartandsetcolorindexes_thr, @params4);

      cmolds.AddStrings(cmolds2);
      cmolds.AddStrings(cmolds3);
      cmolds.AddStrings(cmolds4);

      cmolds2.Free;
      cmolds3.Free;
      cmolds4.Free;
    end
    else
    begin
      for i := 0 to cnt - 1 do
        if (db.GetMoldColorsFlags(db.AllPieces.Strings[i]) and (COLORFLAG_SET or COLORFLAG_PART)) = (COLORFLAG_SET or COLORFLAG_PART) then
          cmolds.Add(db.AllPieces.Strings[i]);
    end;

    tit := 'Names with both part and set color indexes';
    titstr := tit;

    cmolds.CustomSort(sortmoldlist_numcolors);
    DrawMoldList(tit, cmolds, True, False, titstr);

  finally
    cmolds.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.Nameswithbothpartandsetcolorindexes1Click(
  Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('Nameswithbothpartandsetcolorindexes', foo);
end;

procedure TMainForm.Allitems1Click(Sender: TObject);
begin
  if SaveDialogDatabase.Execute then
  begin
    ShowSplash;
    progress_string := 'Saving Database...';
    SplashProgress(progress_string, 0.05);
    db.ExportDatabase(SaveDialogDatabase.Filename);
    HideSplash;
  end;
end;

procedure TMainForm.Multipagedisplay1Click(Sender: TObject);
var
  foo: boolean;
begin
  domultipagedocuments := not domultipagedocuments;
  HTMLClick(AddressEdit.Text, foo);
end;

procedure TMainForm.UsemultipleCPUcores1Click(Sender: TObject);
begin
  usemultithread := not usemultithread;
end;

procedure TMainForm.Options1Click(Sender: TObject);
var
  foo: boolean;
begin
  if BI_EditOptions then
    HTMLClick('refresh', foo);
end;

procedure TMainForm.IdleTimerTimer(Sender: TObject);
begin
  if document.needsidletime then
    Application.OnIdle := document.IdleEventHandler
  else
    Application.OnIdle := nil;
end;

function TMainForm.MakeThumbnailImageEx(const pcs1: string; const typof: char; const ncolor: integer = -1000): string;
  function _doMakeThumbnailImageEx(const pcs: string): string;
  var
    s: string;
    c: string;
    fin, fout: string;
    i: integer;
  begin
    if Trim(pcs1) = '' then
    begin
      Result := '';
      Exit;
    end;

    s := Trim(pcs);

    if s = '' then
    begin
      Result := '';
      Exit;
    end;

    if not (typof in ['g', 'c', 'o', 'i']) then // gray/color/original box/instructions
    begin
      Result := '';
      Exit;
    end;

    c := UpperCase(pcs1[1]);
    if not (c[1] in ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
                     'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
                     '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) then
      c := '9999';

    fout := 'th' + typof + '\' + c + '\' + Trim(pcs1) + '.png';
    if not fexists(basedefault + fout) then
    begin
      fin := '';
      if typof = 'i' then
      begin
        fin := basedefault + '9997\' + s + '.png';
        ResizePng2Png(fin, basedefault + fout, false);
      end
      else if typof = 'o' then
      begin
        fin := basedefault + '9998\' + s + '.png';
        ResizePng2Png(fin, basedefault + fout, false);
      end
      else if typof in ['c', 'g'] then
      begin
        if ncolor = -1 then
        begin
          fin := basedefault + '-1\' + s + '.png';
          if not ResizePng2Png(fin, basedefault + fout, typof = 'g') then
          begin
            fin := basedefault + 's\' + s + '.jpg';
            ResizeJpg2Png(fin, basedefault + fout, typof = 'g');
          end;
        end
        else if ncolor < -1 then
        begin
          for i := 0 to LASTNORMALCOLORINDEX - 1 do
            if db.Colors(i).id = i then
            begin
              fin := basedefault + itoa(i) +'\' + s + '.png';
              if ResizePng2Png(fin, basedefault + fout, typof = 'g') then
                Break;
            end;
          fin := basedefault + '-1\' + s + '.png';
          ResizePng2Png(fin, basedefault + fout, typof = 'g');
        end
        else
        begin
          fin := basedefault + itoa(ncolor) +'\' + s + '.png';
          if not ResizePng2Png(fin, basedefault + fout, typof = 'g') then
          begin
            for i := 0 to LASTNORMALCOLORINDEX - 1 do
              if db.Colors(i).id = i then
              begin
                fin := basedefault + itoa(i) +'\' + s + '.png';
                if ResizePng2Png(fin, basedefault + fout, typof = 'g') then
                  Break;
              end;
            fin := basedefault + '-1\' + s + '.png';
            ResizePng2Png(fin, basedefault + fout, typof = 'g');
          end;
        end;

        if not fexists(fout) then
          if db.AllSets.IndexOf(s) > 0 then
          begin
            fin := basedefault + 's\' + s + '.jpg';
            if not ResizeJpg2Png(fin, basedefault + fout, typof = 'g') then
            begin
              fin := basedefault + '-1\' + s + '.png';
              ResizePng2Png(fin, basedefault + fout, typof = 'g');
            end;
          end;
      end;

      if not fexists(fout) then
      begin
        Result := '';
        Exit;
      end;
    end;

    Result := '<img src="' + fout + '">'
  end;
begin
  Result := _doMakeThumbnailImageEx(pcs1);
  if Result = '' then
    Result := _doMakeThumbnailImageEx(db.RebrickablePart(pcs1));
end;

function TMainForm.MakeThumbnailImageExCache(const pcs: string; const typof: char; const ncolor: integer = -1000): string;
var
  s: string;
  c: string;
  fout, Ufout: string;
  idx: integer;
  b: boolean;
begin
  s := Trim(pcs);

  if s = '' then
    Exit;

  if not (typof in ['g', 'c', 'o', 'i']) then // gray/color/original box/instructions
    Exit;

  c := UpperCase(s[1]);
  if not (c[1] in ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
                   'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
                   '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) then
  begin
    c := '9999';
    idx := 0
  end
  else
    idx := Ord(c[1]);

  fout := 'th' + typof + '\' + c + '\' + s + '.png';
  Ufout := StrUpper(fout);

  if thumbnailfilesexist[idx].IndexOf(Ufout) >= 0 then
    b := True
  else
  begin
    b := fexists(basedefault + fout);
    if b then
      thumbnailfilesexist[idx].Add(Ufout);
  end;

  if not b then
    if thumbnailcache[idx].IndexOf(Ufout) < 0 then
      thumbnailcache[idx].AddObject(Ufout, TThumbnailCacheInfo.Create(pcs, typof, ncolor));

  Result := '<img src="' + fout + '">'
end;

function TMainForm.ConvertThumbnailImageExCache(const imgfile: string): boolean;
var
  i: integer;
  typ: string;
  c: string;
  pcs: string;
  idx: integer;
  idx2: integer;
  check: string;
  thc: TThumbnailCacheInfo;
begin
  Result := False;

  check := UpperCase(imgfile);
  for i := 1 to Length(check) do
    if check[i] = '/' then
      check[i] := '\';
  splitstring(check, typ, c, pcs, '\');
  c := Trim(c);
  if c = '' then
    Exit;

  if c = '9999' then
    idx := 0
  else
    idx := Ord(c[1]);

  idx2 := thumbnailcache[idx].IndexOf(check);
  if idx2 < 0 then
    Exit;

  thc := thumbnailcache[idx].Objects[idx2] as TThumbnailCacheInfo;

  Result := MakeThumbnailImageEx(thc.pcs, thc.typof, thc.ncolor) <> '';
  if Result then
  begin
    thc.Free;
    thumbnailcache[idx].Delete(idx2);
  end;
end;

function TMainForm.MakeThumbnailImage(const pcs: string; const ncolor: integer = -1000): string;
begin
  if generatethumbnailsondemand then
  begin
    if ncolor <= -1 then
    begin
      if db.AllSets.IndexOf(pcs) >= 0 then
        Result := MakeThumbnailImageExCache(pcs, 'c')
      else if db.IsBook(pcs) then
        Result := MakeThumbnailImageExCache(pcs, 'c')
      else
        Result := MakeThumbnailImageExCache(pcs, 'g');
      Exit;
    end;

    if ncolor = -1 then
      Result := MakeThumbnailImageExCache(pcs, 'c', ncolor)
    else if ncolor = 9997 then
      Result := MakeThumbnailImageExCache(pcs, 'i', ncolor)
    else if ncolor = 9998 then
      Result := MakeThumbnailImageExCache(pcs, 'o', ncolor)
    else if ncolor = 9996 then
      Result := MakeThumbnailImageExCache(pcs, 'c', ncolor)
    else
      Result := MakeThumbnailImageExCache(pcs, 'g', ncolor)
  end
  else
  begin
    if ncolor <= -1 then
    begin
      if db.AllSets.IndexOf(pcs) >= 0 then
        Result := MakeThumbnailImageEx(pcs, 'c')
      else if db.IsBook(pcs) then
        Result := MakeThumbnailImageEx(pcs, 'c')
      else
        Result := MakeThumbnailImageEx(pcs, 'g');
      Exit;
    end;

    if ncolor = -1 then
      Result := MakeThumbnailImageEx(pcs, 'c', ncolor)
    else if ncolor = 9997 then
      Result := MakeThumbnailImageEx(pcs, 'i', ncolor)
    else if ncolor = 9998 then
      Result := MakeThumbnailImageEx(pcs, 'o', ncolor)
    else if ncolor = 9996 then
      Result := MakeThumbnailImageEx(pcs, 'c', ncolor)
    else
      Result := MakeThumbnailImageEx(pcs, 'g', ncolor)
  end;
end;

function TMainForm.MakeThumbnailImage2(const pcs: string; const ncolor: integer = -1000): string;
begin
  Result := MakeThumbnailImage(pcs, ncolor);
  if Result = '' then
    Result := MakeThumbnailImage(db.RebrickablePart(pcs), ncolor);
  if Result = '' then
  begin
    if ncolor = -1 then
      Result := '<img width=64px src=s\' + pcs + '.jpg>'
    else if ncolor <> -1000 then
      Result := '<img width=64px src=' + itoa(color) + '\' + pcs + '.png>';
  end;
end;

function TMainForm.FindThumbnailImageFileName(const SRC: string): string;
var
  ssrc: string;
  i: integer;
  scolor, spart, stmp, sext: string;
  ncolor: integer;
begin
  ssrc := SRC;
  for i := 1 to Length(ssrc) do
    if ssrc[i] = '/' then
      ssrc[i] := '\';

  splitstring(SRC, scolor, stmp, '\');
  splitstring(stmp, spart, sext, '.');
  spart := Trim(spart);
  if Pos('\', spart) > 0 then
  begin
    Result := '';
    Exit;
  end;

  if UpperCase(scolor) = 'S' then
    ncolor := -1
  else
    ncolor := atoi(scolor, -1000);
  Result := MakeThumbnailImage(spart, ncolor);
  if Result <> '' then
  begin
    Result := stringreplace(Result, '<img src="', '', [rfReplaceAll, rfIgnoreCase]);
    Result := stringreplace(Result, '">', '', [rfReplaceAll, rfIgnoreCase]);
  end;
end;

function TMainForm.FindThumbnailImageFileNameForHtmlReq(const SRC: string): string;
var
  stmp: string;
begin
  stmp := FindThumbnailImageFileName(SRC);
  if Length(stmp) < 5 then
  begin
    Result := '';
    Exit;
  end;

  if UpperCase(stmp[1]) <> 'T' then
  begin
    Result := '';
    Exit;
  end;

  if UpperCase(stmp[2]) <> 'H' then
  begin
    Result := '';
    Exit;
  end;

  if UpperCase(stmp[4]) <> '\' then
  begin
    Result := '';
    Exit;
  end;

  Result := stmp;
  Result[3] := '0';
  if not fexists(basedefault + Result) then
    if fexists(basedefault + stmp) then
      CopyFile(basedefault + stmp, basedefault + Result);
end;

procedure TMainForm.btn_CatalogHomeClick(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('cataloghome', foo);
end;

procedure TMainForm.FindDialogFind(Sender: TObject);
begin
  with FindDialog do
    if not HTML.FindEx(FindText, frMatchCase in Options, not (frDown in Options)) then
      MessageDlg('Search string ''' + FindText + ''' not found', mtInformation, [mbOK], 0);
end;

procedure TMainForm.Edit1Click(Sender: TObject);
begin
  HtmlCopy1.Enabled := HTML.SelLength <> 0;
end;

procedure TMainForm.HtmlFindClick(Sender: TObject);
begin
  FindDialog.Execute;
end;

procedure TMainForm.HtmlCopyClick(Sender: TObject);
begin
  if HTML.SelLength <> 0 then
    HTML.CopyToClipboard;
end;

procedure TMainForm.HtmlSelectAllClick(Sender: TObject);
begin
  HTML.SelectAll;
end;

procedure TMainForm.PopupMenu1Popup(Sender: TObject);
begin
  HtmlCopy2.Enabled := HTML.SelLength <> 0;
  CloseTab1.Enabled := TabControl1.Tabs.Count > 1;
  Openlinkinnewtab1.Enabled := (newtabUrl <> '') and (TabControl1.Tabs.Count <= MAXNUMTABS) and (CanOpenInNewTab(newtabUrl));
  Openlink1.Enabled := newtabUrl <> '';

  OpenItemPU1.Visible := False;
  if db.AllPieces.IndexOf(Trim(HTML.SelText)) >= 0 then
  begin
    OpenItemPU1.Visible := True;
    OpenItemPU1.Caption := 'Open item "' + Trim(HTML.SelText) + '"';
  end;
end;

procedure TMainForm.CloseTabClick(Sender: TObject);
var
  idx: integer;
begin
  if TabControl1.Tabs.Count <= 1 then
    Exit;

  idx := TabControl1.TabIndex;
  if (idx < 0) or (idx >= TabControl1.Tabs.Count) then
    Exit;

  TabControl1.Tabs.Objects[idx].Free;
  TabControl1.Tabs.Delete(idx);
  dec(idx);
  if idx < 0 then
    idx := 0;
  if idx < TabControl1.Tabs.Count then
  begin
    TabControl1.TabIndex := idx;
    RestoreTab;
  end;
end;

procedure TMainForm.StoreTab;
var
  idx: integer;
  tb: TTabItem;
begin
  idx := TabControl1.TabIndex;
  if (idx < 0) or (idx >= TabControl1.Tabs.Count) then
    Exit;

  tb := TabControl1.Tabs.Objects[idx] as TTabItem;
  tb.Store(HTML, AddressEdit.Text, goback, gofwd);
end;

procedure TMainForm.RestoreTab;
var
  idx: integer;
  tb: TTabItem;
begin
  idx := TabControl1.TabIndex;
  if (idx < 0) or (idx >= TabControl1.Tabs.Count) then
    Exit;

  tb := TabControl1.Tabs.Objects[idx] as TTabItem;
  tb.Restore(HTML, AddressEdit, goback, gofwd);
  Caption := Application.Title + ' - ' + HTML.DocumentTitle;
end;

procedure TMainForm.TabControl1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
  StoreTab;
  AllowChange := True;
end;

procedure TMainForm.TabControl1Change(Sender: TObject);
begin
  RestoreTab;
end;

procedure TMainForm.HTMLRightClick(Sender: TObject;
  Parameters: TRightClickParameters);
var
  p: TPoint;
begin
  newtabUrl := Parameters.URL;
  GetCursorPos(p);
  PopupMenu1.Popup(p.X, p.Y);
end;

function TMainForm.DoOpenUrlInNewTable(const aUrl: string): boolean;
var
  foo: boolean;
  tb: TTabItem;
  idx: integer;
  i: integer;
begin
  Result := False;

  if aUrl = '' then
    Exit;

  if TabControl1.Tabs.Count > MAXNUMTABS then
    Exit;

  if not CanOpenInNewTab(aUrl) then
    Exit;

  StoreTab;

  tb := TTabItem.Create(TabControl1);
  idx := TabControl1.Tabs.AddObject('(loading...)', tb);
  if (idx < 0) or (idx >= TabControl1.Tabs.Count) then
    Exit;
  TabControl1.Tabs.Objects[idx] := tb;
  TabControl1.TabIndex := idx;

  for i := 0 to goback.Count - 1 do
    goback.Objects[i].Free;
  goback.Clear;
  goback.AddObject(newtabUrl, TScrollPos.Create(0, 0));

  for i := 0 to gofwd.Count - 1 do
    gofwd.Objects[i].Free;
  gofwd.Clear;

  HTMLClick(aUrl, foo);

  Result := True;
end;

procedure TMainForm.Openlinkinnewtab1Click(Sender: TObject);
begin
  DoOpenUrlInNewTable(newtabUrl);
end;

procedure TMainForm.Openlink1Click(Sender: TObject);
var
  foo: boolean;
begin
  if newtabUrl = '' then
    Exit;

  HTMLClick(newtabUrl, foo);
end;

procedure TMainForm.LugBulks1Click(Sender: TObject);
begin
  N20171.Visible := fexists(basedefault + 'lugbulks\2017.txt');
  N20181.Visible := fexists(basedefault + 'lugbulks\2018.txt');
  N20191.Visible := fexists(basedefault + 'lugbulks\2019.txt');
end;

procedure TMainForm.Setsbynumpieces1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('ShowSetStatsByNumPieces', foo);
end;

procedure TMainForm.Setsbynumlots1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('ShowSetStatsByNumLots', foo);
end;

procedure TMainForm.ShowSetsLotsBetween(const a, b: integer);
var
  lsets: TStringList;
  i: integer;
  aa: integer;
  tit: string;
  inv: TBrickInventory;
  n: integer;
  setstr: string;
begin
  Screen.Cursor := crHourGlass;
  ShowSplash;
  SplashProgress('Working...', 0);

  lsets := db.SetListWithInvLotsBetween(a, b);

  if domultipagedocuments then
    document.NewMultiPageDocument('SetListWithInvLotsBetween' + itoa(a) + itoa(b), itoa(lsets.count));

  document.write('<body background="splash.jpg">');
  tit := Format('Sets from %d to %d lots', [a, b]);
  document.title(tit);
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine(tit);

  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Set</b></th>');
  document.write('<th><b>Num lots</b></th>');
  document.write('</tr>');
  HideSplash;

  aa := 0;
  for i := 0 to lsets.Count - 1 do
  begin
    inc(aa);
    document.StartItemId(aa);
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%>' +
                    MakeThumbnailImage2(lsets.Strings[i], -1) + '<br>');
    document.write('<a href="sinv/' + lsets.Strings[i] + '">');
    document.write('<b>' + lsets.Strings[i] + '</b> - ' + db.SetDesc(lsets.Strings[i]));
//    if not db.IsMoc(lsets.Strings[i]) then
//      document.write(' <a href=UpdateSetAssetsFromBricklink/' + lsets.Strings[i] + '><img src="images\refresh.png"></a>');

    if lsets.Count > 1 then
    begin
      if lsets.Count < 20 then
        SplashProgress('Working...', i / (lsets.Count - 1))
      else
        if (i mod 5) = 0 then
          SplashProgress('Working...', i / (lsets.Count - 1));
    end;

    document.write('</td>');

    n := 0;
    setstr := '';
    inv := db.GetSetInventory(lsets.Strings[i]);
    if inv <> nil then
    begin
      n := inv.numlooseparts;
      if inv.numsets > 0 then
        setstr := itoa(inv.numsets) + ' sets';
    end;
    if n <> 0 then
    begin
      if setstr <> '' then
        document.write('<td><b>' + itoa(n) + ' + ' + setstr + '</b></td>')
      else
        document.write('<td><b>' + itoa(n) + '</b></td>');
    end
    else
      document.write('<td><b>' + setstr + '</b></td>');

    document.write('</tr>');
  end;
  document.EndNavigateSection;
  HideSplash;

  document.write('</table>');

  document.MarkBottomNavigateSection;

  document.write('</p><br><br></p></div></body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;

  lsets.Free;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowSetsPartsBetween(const a, b: integer);
var
  lsets: TStringList;
  i: integer;
  aa: integer;
  tit: string;
  inv: TBrickInventory;
  n: integer;
begin
  Screen.Cursor := crHourGlass;
  ShowSplash;
  SplashProgress('Working...', 0);

  lsets := db.SetListWithInvPartsBetween(a, b);

  if domultipagedocuments then
    document.NewMultiPageDocument('SetListWithInvPartsBetween' + itoa(a) + itoa(b), itoa(lsets.count));

  document.write('<body background="splash.jpg">');
  tit := Format('Sets from %d to %d parts', [a, b]);
  document.title(tit);
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine(tit);

  document.StartNavigateSection;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Set</b></th>');
  document.write('<th><b>Num parts</b></th>');
  document.write('</tr>');
  HideSplash;

  aa := 0;
  for i := 0 to lsets.Count - 1 do
  begin
    inc(aa);
    document.StartItemId(aa);
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%>' +
                    MakeThumbnailImage2(lsets.Strings[i], -1) + '<br>');
    document.write('<a href="sinv/' + lsets.Strings[i] + '">');
    document.write('<b>' + lsets.Strings[i] + '</b> - ' + db.SetDesc(lsets.Strings[i]));
//    if not db.IsMoc(lsets.Strings[i]) then
//      document.write(' <a href=UpdateSetAssetsFromBricklink/' + lsets.Strings[i] + '><img src="images\refresh.png"></a>');

    if lsets.Count > 1 then
    begin
      if lsets.Count < 20 then
        SplashProgress('Working...', i / (lsets.Count - 1))
      else
        if (i mod 5) = 0 then
          SplashProgress('Working...', i / (lsets.Count - 1));
    end;

    document.write('</td>');

    n := 0;
    inv := db.GetSetInventory(lsets.Strings[i]);
    if inv <> nil then
      n := inv.totallooseparts;
    document.write('<td><b>' + itoa(n) + '</b></td>');

    document.write('</tr>');
  end;
  document.EndNavigateSection;
  HideSplash;

  document.write('</table>');

  document.MarkBottomNavigateSection;

  document.write('</p><br><br></p></div></body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;

  lsets.Free;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.PiecesWithDaysToUpdate(const x: integer);
var
  i, j: integer;
  lst: TStringList;
  pcs: string;
  pci: TPieceColorInfo;
  inv: TBrickInventory;
  s1: string;
  nextstr, prevstr: string;
  nextx, prevx: integer;
  dn: TDateTime;
  ddays: integer;
  kp: THashStringList;
  titstr: string;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;
  dn := Now();

  for i := 0 to LASTNORMALCOLORINDEX do
    if db.Colors(i).id = i then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
        for j := 0 to kp.Count - 1 do
        begin
          pcs := kp.Strings[j];
          pci := kp.Objects[j] as TPieceColorInfo;
          if pci <> nil then
          begin
            ddays := DaysBetween(dn, pci.Date);
            if ddays >= x then
            begin
              lst.Add(pcs + ',' + itoa(i));
              inv.AddLoosePartFast(pcs, i, 1, pci);
            end;
          end;
        end;
    end;

  if x > 1 then
  begin
    if x <= 10 then
      prevx := x - 1
    else
      prevx := round(x * 0.8);
    prevstr := '<a href=PiecesWithDaysToUpdate/' + itoa(prevx) + '>Pieces without update the last ' + itoa(prevx) + ' days</a><br>';
  end
  else
    prevstr := '';
  if x < 5000 then
  begin
    nextx := round(x * 1.25);
    if nextx > 5000 then
      nextx := 5000;
    nextstr := '<br><a href=PiecesWithDaysToUpdate/' + itoa(nextx) + '>Pieces without update the last ' + itoa(nextx) + ' days</a>';
  end
  else
    nextstr := '';

  lst.Sort;
  titstr := 'Pieces without update the last ' + itoa(x) + ' days';
  DrawPieceList(prevstr + titstr + nextstr, lst, SORT_DATE_UPDATE, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\PiecesWithDaysToUpdate' + IntToStrzFill(4, x) + '\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'PiecesWithDaysToUpdate' + IntToStrzFill(4, x);
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListUsed(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListUsed(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListUsed(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListUsed(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListUsed(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListUsed(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListUsed(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListUsed(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_030%.xml', 0.3);

  inv.Free;

end;

procedure TMainForm.PiecesWithDaysToUpdateRange(const ax1, ax2: integer);
var
  i, j: integer;
  lst: TStringList;
  pcs: string;
  pci: TPieceColorInfo;
  inv: TBrickInventory;
  s1: string;
  nextstr, prevstr: string;
  nextx, prevx: integer;
  dn: TDateTime;
  ddays: integer;
  x, x2: integer;
  diff: integer;
  kp: THashStringList;
  titstr: string;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;
  dn := Now();

  if ax1 < ax2 then
  begin
    x := ax1;
    x2 := ax2;
  end
  else
  begin
    x := ax2;
    x2 := ax1;
  end;
  for i := 0 to LASTNORMALCOLORINDEX do
    if db.Colors(i).id = i then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
        for j := 0 to kp.Count - 1 do
        begin
          pcs := kp.Strings[j];
          pci := kp.Objects[j] as TPieceColorInfo;
          if pci <> nil then
          begin
            ddays := DaysBetween(dn, pci.Date);
            if ddays >= x then
              if ddays <= x2 then
              begin
                lst.Add(pcs + ',' + itoa(i));
                inv.AddLoosePartFast(pcs, i, 1, pci);
              end;
          end;
        end;
    end;

  diff := x2 - x;
  if x >= diff then
  begin
    prevx := x - diff;
    prevstr := '<a href=PiecesWithDaysToUpdateRange/' + itoa(prevx) + '/' + itoa(x) + '>Pieces without update between ' + itoa(prevx) + ' and ' + itoa(x) + ' days</a><br>';
  end
  else
    prevstr := '';
  if (x2 < 1000 * diff) and (int64(int64(x) + int64(diff)) < int64(MAXINT)) then
  begin
    nextx := x2 + diff;
    nextstr := '<br><a href=PiecesWithDaysToUpdateRange/' + itoa(x2) + '/' + itoa(nextx) + '>Pieces without update between ' + itoa(x2) + ' and ' + itoa(nextx) + ' days</a>';
  end
  else
    nextstr := '';

  lst.Sort;
  titstr := 'Pieces without update between ' + itoa(x) + ' and ' + itoa(x2) + ' days';
  DrawPieceList(prevstr + titstr + nextstr, lst, SORT_DATE_UPDATE, '', titstr);
  lst.Free;

  s1 := basedefault + 'out\PiecesWithDaysToUpdateRange_' + IntToStrzFill(4, x) + '_' + IntToStrzFill(4, x2) + '\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + 'PiecesWithDaysToUpdateRange_' + IntToStrzFill(4, x) + '_' + IntToStrzFill(4, x2);
  inv.SaveLooseParts(s1 + '.txt');
  s1 := s1 + '_wantedlist';
  inv.SaveLoosePartsWantedListUsed(s1 + '_200%.xml', 2.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_150%.xml', 1.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_140%.xml', 1.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_130%.xml', 1.3);
  inv.SaveLoosePartsWantedListUsed(s1 + '_120%.xml', 1.2);
  inv.SaveLoosePartsWantedListUsed(s1 + '_110%.xml', 1.1);
  inv.SaveLoosePartsWantedListUsed(s1 + '_100%.xml', 1.0);
  inv.SaveLoosePartsWantedListUsed(s1 + '_090%.xml', 0.9);
  inv.SaveLoosePartsWantedListUsed(s1 + '_080%.xml', 0.8);
  inv.SaveLoosePartsWantedListUsed(s1 + '_070%.xml', 0.7);
  inv.SaveLoosePartsWantedListUsed(s1 + '_060%.xml', 0.6);
  inv.SaveLoosePartsWantedListUsed(s1 + '_050%.xml', 0.5);
  inv.SaveLoosePartsWantedListUsed(s1 + '_040%.xml', 0.4);
  inv.SaveLoosePartsWantedListUsed(s1 + '_030%.xml', 0.3);

  inv.Free;

end;

procedure TMainForm.Pieceswithoutupdatethelast10days1Click(
  Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PiecesWithDaysToUpdate/10', foo);
end;

procedure TMainForm.Pieceswithoutupdatethelast30days1Click(
  Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PiecesWithDaysToUpdate/30', foo);
end;

procedure TMainForm.Pieceswithoutupdatethelast90days1Click(
  Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PiecesWithDaysToUpdate/90', foo);
end;

procedure TMainForm.Pieceswithoutupdatethelast365days1Click(
  Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PiecesWithDaysToUpdate/365', foo);
end;

var
  dnu1: integer = 10;
  dnu2: integer = 20;

procedure TMainForm.Pieceswithoutupdateinrange1Click(Sender: TObject);
var
  foo: Boolean;
begin
  if InputTwoIntegers('Pieces without update in range', 'From days: ', 'To days: ', dnu1, dnu2) then
    HTMLClick('PiecesWithDaysToUpdateRange/' + itoa(dnu1) + '/' + itoa(dnu2), foo);
end;

procedure TMainForm.Lotswith10ormoreparts1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('ShowBigInvLots/10', foo);
end;

procedure TMainForm.Lotswith50ormoreparts1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('ShowBigInvLots/50', foo);
end;

procedure TMainForm.Lotswith100ormoreparts1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('ShowBigInvLots/100', foo);
end;

procedure TMainForm.Lotswith500ormoreparts1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('ShowBigInvLots/500', foo);
end;

procedure TMainForm.Lotswith1000ormoreparts1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('ShowBigInvLots/1000', foo);
end;

procedure TMainForm.ErrorBeep;
begin
  if silentwarnings then
    Exit;
  MessageBeep(MB_ICONERROR)
end;

procedure TMainForm.UpdateUnknownYearFromDisk(const qry: string);
var
  i, j: integer;
  pci: TPieceColorInfo;
  kp: THashStringList;
  tot, cnt: integer;
  smsg: string;
begin
  Screen.Cursor := crHourglass;
  ShowSplash;
  tot := 0;
  smsg := 'Working...';
  SplashProgress(smsg, 0.0);
  for i := -1 to MAXINFOCOLOR do
    if (i = -1) or (db.Colors(i).id = i) then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
      begin
        for j := 0 to kp.Count - 1 do
        begin
          pci := kp.Objects[j] as TPieceColorInfo;
          if pci <> nil then
            if (pci.sparttype = qry) or (qry = '') then
              if pci.year = 0 then
                inc(tot);
        end;
      end;
    end;
  SplashProgress(smsg, 0.1);

  if tot > 0 then
  begin
    cnt := 0;
    for i := -1 to MAXINFOCOLOR do
      if (i = -1) or (db.Colors(i).id = i) then
      begin
        kp := db.Colors(i).knownpieces;
        if kp <> nil then
        begin
          for j := 0 to kp.Count - 1 do
          begin
            pci := kp.Objects[j] as TPieceColorInfo;
            if pci <> nil then
              if (pci.sparttype = qry) or (qry = '') then
                if pci.year = 0 then
                begin
                  db.UpdateItemYearFromDiskCache(pci.piece);
                  inc(cnt);
                  if cnt mod 100 = 0 then
                    SplashProgress(smsg, 0.1 + 0.9 * (cnt / tot));
                end;
          end;
        end;
      end;
  end;
  SplashProgress(smsg, 1.0);
  Screen.Cursor := crDefault;
end;

procedure TMainForm.UpdateAllPartsUnknownYear;
var
  i, j: integer;
  pci: TPieceColorInfo;
  kp: THashStringList;
  idx, tot: integer;
  smsg: string;
  lst: TStringList;
begin
  Screen.Cursor := crHourglass;
  ShowSplash;
  lst := TStringList.Create;
  smsg := 'Working...';
  SplashProgress(smsg, 0.0);
  for i := -1 to MAXINFOCOLOR do
    if (i = -1) or (db.Colors(i).id = i) then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
      begin
        for j := 0 to kp.Count - 1 do
        begin
          pci := kp.Objects[j] as TPieceColorInfo;
          if pci <> nil then
            if pci.sparttype = 'P' then
              if pci.year = 0 then
              begin
                idx := lst.IndexOf(pci.piece);
                if idx < 0 then
                begin
                  if i = -1 then
                    lst.AddObject(pci.piece, TCInteger.Create(0))
                  else
                    lst.AddObject(pci.piece, TCInteger.Create(1))
                end
                else
                  (lst.Objects[idx] as TCInteger).IncValue;
              end;
        end;
      end;
    end;

  for i := lst.Count - 1 downto 0 do
    if (lst.Objects[i] as TCInteger).value > 1 then
    begin
      (lst.Objects[i] as TCInteger).Free;
      lst.Delete(i);
    end;
  tot := lst.Count;

  SplashProgress(smsg, 0.01);

  for i := 0 to tot - 1 do
  begin
    db.UpdatePartKnownColorsFromBricklink(lst.Strings[i]);
    if i mod 100 = 0 then
      SplashProgress(smsg, 0.01 + 0.99 * (i / tot));
  end;

  SplashProgress(smsg, 1.0);
  Screen.Cursor := crDefault;
  FreeList(lst);
end;

procedure TMainForm.UpdateAllItemsUnknownDesc;
var
  i, j: integer;
  pci: TPieceColorInfo;
  kp: THashStringList;
  idx, tot: integer;
  smsg: string;
  lst: TStringList;
begin
  Screen.Cursor := crHourglass;
  ShowSplash;
  lst := TStringList.Create;
  smsg := 'Working...';
  SplashProgress(smsg, 0.0);
  for i := -1 to MAXINFOCOLOR do
    if (i = -1) or (db.Colors(i).id = i) then
    begin
      kp := db.Colors(i).knownpieces;
      if kp <> nil then
      begin
        for j := 0 to kp.Count - 1 do
        begin
          pci := kp.Objects[j] as TPieceColorInfo;
          if db.PieceDesc(pci.piece) = '' then
          begin
            idx := lst.IndexOf(pci.piece);
            if idx < 0 then
              lst.Add(pci.piece);
          end;
        end;
      end;
    end;

  tot := lst.Count;

  SplashProgress(smsg, 0.01);

  for i := 0 to tot - 1 do
  begin
    db.UpdatePartKnownColorsFromBricklink(lst.Strings[i]);
    if i mod 100 = 0 then
      SplashProgress(smsg, 0.01 + 0.99 * (i / tot));
  end;

  SplashProgress(smsg, 1.0);
  Screen.Cursor := crDefault;
  FreeList(lst);
end;

procedure TMainForm.UpdatePartFromDisk(const spart: string);
begin
  if not db.UpdatePartKnownColorsFromBricklink(spart, false) then
    if not db.UpdateGearKnownColorsFromBricklink(spart, false) then
      db.UpdateBookKnownColorsFromBricklink(spart, false);
end;

procedure TMainForm.LugBulk2019CheapParts1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2019/5.0', foo);
end;

procedure TMainForm.LugBulk2019CheapBricks1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2019/5.0/5', foo);
end;

procedure TMainForm.LugBulk2019CheapPlates1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2019/5.0/26', foo);
end;

procedure TMainForm.LugBulk2019CheapTiles1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2019/5.0/37', foo);
end;

procedure TMainForm.LugBulk2019CheapSlopes1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2019/5.0/31', foo);
end;

procedure TMainForm.LugBulk2019CheapInvertedSlopes1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2019/5.0/32', foo);
end;

procedure TMainForm.ExportPartsColors1Click(Sender: TObject);
begin
  if SaveDialogPartsColors.Execute then
  begin
    ShowSplash;
    progress_string := 'Saving part/color data...';
    SplashProgress(progress_string, 0.05);
    db.ExportPartColor(SaveDialogPartsColors.Filename);
    HideSplash;
  end;
end;

procedure TMainForm.MostitemssoldNew1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PriceGuideQry/P/10/1/0/0/0', foo);
end;

procedure TMainForm.MostitemssoldUsed1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PriceGuideQry/P/10/0/1/0/0', foo);
end;

procedure TMainForm.MostitemssoldNewUsed1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PriceGuideQry/P/10/1/1/0/0', foo);
end;

procedure TMainForm.MoreitemsavailableNew1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PriceGuideQry/P/10/0/0/1/0', foo);
end;

procedure TMainForm.MoreitemsavailableUsed1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PriceGuideQry/P/10/0/0/0/1', foo);
end;

procedure TMainForm.MoreitemsavailableNewUsed1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PriceGuideQry/P/10/0/0/1/1', foo);
end;

procedure TMainForm.MostsetssoldNew1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PriceGuideQry/S/10/1/0/0/0', foo);
end;

procedure TMainForm.MostsetssoldUsed1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PriceGuideQry/S/10/0/1/0/0', foo);
end;

procedure TMainForm.MostsetssoldNewUsed1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PriceGuideQry/S/10/1/1/0/0', foo);
end;

procedure TMainForm.MoresetsavailableNew1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PriceGuideQry/S/10/0/0/1/0', foo);
end;

procedure TMainForm.MoresetsavailableUsed1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PriceGuideQry/S/10/0/0/0/1', foo);
end;

procedure TMainForm.MoresetsavailableNewUsed1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PriceGuideQry/S/10/0/0/1/1', foo);
end;

procedure TMainForm.MostminifigssoldNew1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PriceGuideQry/M/10/1/0/0/0', foo);
end;

procedure TMainForm.MostminifigssoldUsed1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PriceGuideQry/M/10/0/1/0/0', foo);
end;

procedure TMainForm.MostminifigssoldNewUsed1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PriceGuideQry/M/10/1/1/0/0', foo);
end;

procedure TMainForm.MoreminifigsavailableNew1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PriceGuideQry/M/10/0/0/1/0', foo);
end;

procedure TMainForm.MoreminifigsavailableUsed1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PriceGuideQry/M/10/0/0/0/1', foo);
end;

procedure TMainForm.MoreminifigsavailableNewUsed1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('PriceGuideQry/M/10/0/0/1/1', foo);
end;

function MoldYear_thr(p: pointer): integer; stdcall;
var
  params: iterator_base_p;
  i, j: integer;
  pci: TPieceColorInfo;
  cp: colorinfo_p;
  kp: THashStringList;
  y: integer;
begin
  params := iterator_base_p(p);
  for i := 0 to MAXINFOCOLOR do
    if i mod params.numidxs = params.idx then
    begin
      cp := db.Colors(i);
      if cp.id = i then
      begin
        kp := cp.knownpieces;
        if kp <> nil then
        begin
          for j := 0 to kp.Count - 1 do
          begin
            pci := kp.Objects[j] as TPieceColorInfo;
            if pci <> nil then
            begin
              y := pci.firstsetyear;
              if y >= 1931 then
                if y <= 2050 then
                  params.list.AddObject(kp.Strings[j], TCInteger.Create(y));
            end;
          end;
        end;
      end;
    end;
  Result := 0;
end;

procedure TMainForm.MoldsYearQry(const y: integer; const firstappeared: boolean);
var
  i: integer;
  lst, molds: TStringList;
  prevstr, nextstr: string;
  N: TDNumberList;
  tit, titstr: string;
  sl: string;
begin
  lst := MT_Iterate_Base(@MoldYear_thr);
  lst.Sort;

  molds := TStringList.Create;
  for i := 0 to lst.Count - 1 do
  begin
    if i = 0 then
    begin
      N := TDNumberList.Create;
      N.Add((lst.Objects[0] as TCInteger).value);
      molds.AddObject(lst.Strings[0], N);
    end
    else
    begin
      if molds.Strings[molds.Count - 1] = lst.Strings[i] then
        (molds.Objects[molds.Count - 1] as TDNumberList).Add((lst.Objects[i] as TCInteger).value)
      else
      begin
        N := TDNumberList.Create;
        N.Add((lst.Objects[i] as TCInteger).value);
        molds.AddObject(lst.Strings[i], N);
      end;
    end;
  end;

  FreeList(lst);

  for i := 0 to molds.Count - 1 do
    (molds.Objects[i] as TDNumberList).Sort;

  if firstappeared then
  begin
    for i := molds.Count - 1 downto 0 do
    begin
      N := molds.Objects[i] as TDNumberList;
      if N.Numbers[0] <> y then
      begin
        N.Free;
        molds.Delete(i);
      end;
    end;
    if y > 1931 then
      prevstr := '<a href=MoldsFirstAppearedAtYear/' + itoa(y - 1) + '>Molds first appeared at year ' + itoa(y - 1) + '</a><br>'
    else
      prevstr := '';
    if y < atoi(FormatDateTime('yyyy', Now)) then
      nextstr := '<br><a href=MoldsFirstAppearedAtYear/' + itoa(y + 1) + '>Molds first appeared at year ' + itoa(y + 1) + '</a><br>'
    else
      nextstr := '';
    tit := 'Molds first appeared at year ' + itoa(y);
    titstr := tit;
  end
  else // discontinued
  begin
    for i := molds.Count - 1 downto 0 do
    begin
      N := molds.Objects[i] as TDNumberList;
      if N.Numbers[N.Count - 1] <> y - 1 then
      begin
        N.Free;
        molds.Delete(i);
      end;
    end;
    if y > 1931 then
      prevstr := '<a href=MoldsDiscontinuedAtYear/' + itoa(y - 1) + '>Molds discontinued at year ' + itoa(y - 1) + '</a><br>'
    else
      prevstr := '';
    if y < atoi(FormatDateTime('yyyy', Now)) then
      nextstr := '<br><a href=MoldsDiscontinuedAtYear/' + itoa(y + 1) + '>Molds discontinued at year ' + itoa(y + 1) + '</a><br>'
    else
      nextstr := '';
    tit := 'Molds discontinued at year ' + itoa(y);
    titstr := tit;
  end;

  DrawMoldList(tit, molds, False, True, titstr);

  sl := basedefault + 'out\MoldsYearQry\';
  if not DirectoryExists(sl) then
    ForceDirectories(sl);
  sl := sl + RemoveSpaces(tit) + '.txt';

  S_SaveToFile(molds, sl);

  FreeList(molds);
end;

procedure TMainForm.MoldsFirstAppeared1Click(Sender: TObject);
var
  yyyy: integer;
  foo: boolean;
begin
  yyyy := atoi(FormatDateTime('yyyy', Now));
  if InputInteger('First appeared molds', 'Year', yyyy) then
    HTMLClick('MoldsFirstAppearedAtYear/' + itoa(yyyy), foo);
end;

procedure TMainForm.Moldsdiscontinuedatyear1Click(Sender: TObject);
var
  yyyy: integer;
  foo: boolean;
begin
  yyyy := atoi(FormatDateTime('yyyy', Now));
  if InputInteger('Discontinued molds', 'Year', yyyy) then
    HTMLClick('MoldsDiscontinuedAtYear/' + itoa(yyyy), foo);
end;

procedure TMainForm.CollectionValue1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('ShowMyPiecesValue', foo);
end;

procedure TMainForm.NewTab1Click(Sender: TObject);
begin
  DoOpenUrlInNewTable('home');
end;

procedure TMainForm.Lugbulk2017items1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('ShowLugbulk/2017', foo);
end;

procedure TMainForm.Lugbulk2018items1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('ShowLugbulk/2018', foo);
end;

procedure TMainForm.Lugbulk2019items1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('ShowLugbulk/2019', foo);
end;

procedure TMainForm.Comparesets1Click(Sender: TObject);
var
  foo: Boolean;
  fn: string;
begin
  if GetMultipleSetsList(Comparesetslist) then
    HTMLClick('ShowSetsDataCompare/' + IntToStr(Integer(Comparesetslist)) + '/Compare Sets', foo);
  fn := basedefault + 'out\Comparesets\';
  if not DirectoryExists(fn) then
    MkDir(fn);
  fn := fn + 'Comparesetslist.txt';
  backupfile(fn);
  Comparesetslist.SaveToFile(fn);
end;


procedure TMainForm.SpeedButton2Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('refresh', foo);
end;

procedure TMainForm.MinifiguresIcanbuild1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('figsIcanbuild/9/10', foo);
end;

function TMainForm.GetRebrickableColorHtml(const cl: integer): string;
var
  rcolor: integer;
begin
  rcolor := db.colors(cl).RebrickableColor;
  if cl <> rcolor then
    Result := ' (RB=' + itoa(rcolor) + ')'
  else
    Result := '';
end;

procedure TMainForm.OpenItemPU1Click(Sender: TObject);
var
  pcs: string;
  foo: boolean;
begin
  pcs := Trim(HTML.SelText);
  if db.AllPieces.IndexOf(pcs) >= 0 then
    HTMLClick('spiece/' + pcs, foo);
end;

function AutoCorrectUnknownPieceYears_thr(p: pointer): integer; stdcall;
var
  params: iterator_base_p;
  i, j, k: integer;
  pci: TPieceColorInfo;
  cp: colorinfo_p;
  kp: THashStringList;
  y: integer;
  scheck: string;
  sset: string;
  idx: integer;
  desc: string;
begin
  params := iterator_base_p(p);

  scheck := ' SET ';
  for i := 0 to MAXINFOCOLOR do
    if i mod params.numidxs = params.idx then
    begin
      cp := db.Colors(i);
      if cp.id = i then
      begin
        kp := cp.knownpieces;
        if kp <> nil then
        begin
          for j := 0 to kp.Count - 1 do
          begin
            pci := kp.Objects[j] as TPieceColorInfo;
            if pci <> nil then
              if pci.year = 0 then
              begin
                desc := db.PieceDesc(pci.piece);
                idx := Pos(scheck, strupper(desc));
                if idx > 0 then
                begin
                  sset := '';
                  for k := idx + Length(scheck) to Length(desc) do
                  begin
                    if IsNumericC(desc[k]) or (desc[k] = '-') then
                      sset := sset + desc[k]
                    else
                      Break;
                  end;

                  if Pos('-', sset) = 0 then
                    sset := sset + '-1';

                  y := db.SetYear(sset);
                  if y >= 1931 then
                    if y <= 2050 then
                      params.list.AddObject(pci.piece, TCInteger.Create(y));
                end;
              end;
          end;
        end;
      end;
    end;

  scheck := 'STICKER FOR SET ';
  cp := db.Colors(-1);
  kp := cp.knownpieces;
  if kp <> nil then
    for j := 0 to kp.Count - 1 do
      if j mod params.numidxs = params.idx then
      begin
        pci := kp.Objects[j] as TPieceColorInfo;
        if pci <> nil then
          if pci.year = 0 then
          begin
            desc := db.PieceDesc(pci.piece);
            idx := Pos(scheck, strupper(desc));
            if idx = 1 then
            begin
              sset := '';
              for k := idx + Length(scheck) to Length(desc) do
              begin
                if IsNumericC(desc[k]) or (desc[k] = '-') then
                  sset := sset + desc[k]
                else
                  Break;
              end;

              if Pos('-', sset) = 0 then
                sset := sset + '-1';

              y := db.SetYear(sset);
              if y >= 1931 then
                if y <= 2050 then
                  params.list.AddObject(pci.piece, TCInteger.Create(y));
            end;
          end;
      end;

  Result := 0;
end;

function TMainForm.AutoCorrectUnknownPieceYears: boolean;
var
  i: integer;
  lst: TStringList;
begin
  Screen.Cursor := crHourglass;

  lst := MT_Iterate_Base(@AutoCorrectUnknownPieceYears_thr);
  lst.Sort;

  for i := lst.Count - 1 downto 1 do
    if lst.Strings[i] = lst.Strings[i - 1] then
    begin
      lst.Objects[i].Free;
      lst.Delete(i);
    end;

  for i := 0 to lst.Count - 1 do
    db.UpdateYearForAllColors(lst.Strings[i], (lst.Objects[i] as TCInteger).value);

  Result := lst.Count > 0;

  FreeList(lst);

  S_FlashFileSystem;

  Screen.Cursor := crDefault;
end;

procedure TMainForm.DoUpdateInstructionsFromNet(const sset: string);
var
  foo: boolean;
  ret: boolean;
begin
  Screen.Cursor := crHourglass;
  try
    ret := UpdateInstructionsFromNet(sset, True);
  finally
    Screen.Cursor := crDefault;
  end;
  if ret or InstructionsExist(sset) or (findfile(basedefault + InstructionDir(sset) + '*.pdf') <> '') then
    HTMLClick('instructions/' + sset, foo);
end;

procedure TMainForm.DoUpdateInstructionsFromNetHost(const sset: string; const host: string);
var
  foo: boolean;
  ret: boolean;
begin
  Screen.Cursor := crHourglass;
  try
    ret := UpdateInstructionsFromNet(sset, True, host);
  finally
    Screen.Cursor := crDefault;
  end;
  if ret or InstructionsExist(sset) or (findfile(basedefault + InstructionDir(sset) + '*.pdf') <> '') then
    HTMLClick('refresh', foo);
end;

procedure TMainForm.DoUpdateInstructionsFromPdf(const sset: string);
var
  foo: boolean;
begin
  if UpdateInstructionsFromPdf(sset) then
    HTMLClick('instructions/' + sset, foo);
end;

function TMainForm.RemoveImageFromCache(const simg: string): boolean;
var
  idx: integer;
  scheck: string;
begin
  scheck := strupper(simg);

  Result := False;

  idx := streams.IndexOf(scheck);
  if idx >= 0 then
  begin
    Result := True;
    streams.Objects[idx].Free;
    streams.Delete(idx);
  end;

  idx := imagerequests.IndexOf(scheck);
  if idx >= 0 then
  begin
    Result := True;
    imagerequests.Delete(idx);
  end;
end;

procedure TMainForm.Instructionswithunknownweight1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('InstructionsUnknownWeight', foo);
end;

procedure TMainForm.OriginalBoxeswithunknownweight1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('BoxesUnknownWeight', foo);
end;

procedure TMainForm.Lugbulk2020items1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('ShowLugbulk/2020', foo);
end;

procedure TMainForm.LugBulk2020CheapParts1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2020/5.0', foo);
end;

procedure TMainForm.LugBulk2020CheapBricks1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2020/5.0/5', foo);
end;

procedure TMainForm.LugBulk2020CheapPlates1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2020/5.0/26', foo);
end;

procedure TMainForm.LugBulk2020CheapTiles1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2020/5.0/37', foo);
end;

procedure TMainForm.LugBulk2020CheapInvertedSlopes1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2020/5.0/32', foo);
end;

procedure TMainForm.LugBulk2020CheapSlopes1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2020/5.0/31', foo);
end;

procedure TMainForm.Lugbulk2021items1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('ShowLugbulk/2021', foo);
end;

procedure TMainForm.LugBulk2021CheapParts1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2021/5.0', foo);
end;

procedure TMainForm.LugBulk2020CheapBricks2Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2021/5.0/5', foo);
end;

procedure TMainForm.LugBulk2021CheapPlates1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2021/5.0/26', foo);
end;

procedure TMainForm.LugBulk2021CheapTiles1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2021/5.0/37', foo);
end;

procedure TMainForm.LugBulk2021CheapSlopes1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2021/5.0/31', foo);
end;

procedure TMainForm.LugBulk2021CheapInvertedSlopes1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2021/5.0/32', foo);
end;

procedure TMainForm.DoUpdatePriceGuideFromDiskList(const fn: string; const days: integer);
var
  s: TStringList;
  s1, s2, s3: string;
  i: integer;
  pci: TPieceColorInfo;
begin
  if not fexists(fn) then
    Exit;

  Screen.Cursor := crHourglass;
  s := TStringList.Create;
  try
    s.LoadFromFile(fn);
    if s.Count > 1 then
    begin
      splitstring(s.Strings[0], s1, s2, s3, ',');
      if (strupper(s1) = 'PART') and (strupper(s2) = 'COLOR') then
      begin
        for i := 1 to s.Count - 1 do
        begin
          splitstring(s.Strings[i], s1, s2, s3, ',');
          pci := db.PieceColorInfo(s1, atoi(s2));
          if pci <> nil then
          begin
            if DaysBetween(Now(), pci.date) >= days then
              pci.InternetUpdate;
          end
        end;
      end;
    end;
  finally
    s.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.UpdatePriceGuideDisklist1Click(Sender: TObject);
begin
  if OpenDialog3.Execute then
    DoUpdatePriceGuideFromDiskList(OpenDialog3.FileName, 1);
end;

procedure TMainForm.SaveLugbulkDatabase(const fn: string; const year: integer);
var
  sL: TStringList;
  i: integer;
  pci: TPieceColorInfo;
  spart, scolor: string;
  lb: TLugBulk2017;
  slugbulkfile: string;
begin
  slugbulkfile := basedefault + 'lugbulks\' + itoa(year) + '.txt';
  if not fexists(slugbulkfile) then
  begin
    ShowMessage('File ' + slugbulkfile + #13#10 + 'not found!');
    Exit;
  end;

  lb := TLugBulk2017.Create;
  lb.LoadFromFile(slugbulkfile);

  sL := TStringList.Create;
  try
    sL.Add('Part,Color,Code,Lugbulk_Cost,Bricklink_Cost');
    for i := 0 to lb.List.Count - 1 do
    begin
      splitstring(lb.List.Strings[i], spart, scolor, ',');
      pci := db.PieceColorInfo(spart, atoi(scolor, -1));
      if pci <> nil then
        if pci.code <> '' then
          sL.Add(Format('%s,%s,%2.2f,%2.4f', [lb.List.Strings[i], pci.code, (lb.List.Objects[i] as TLugBulkDouble).value, pci.EvaluatePriceNew]));
    end;
    sL.SaveToFile(fn);
  finally
    sL.Free;
  end;

  lb.Free;
end;

procedure TMainForm.SaveLugbulk2021database1Click(Sender: TObject);
begin
  if SaveLugbulkBLCostDialog.Execute then
    SaveLugbulkDatabase(SaveLugbulkBLCostDialog.Filename, 2021);
end;

procedure TMainForm.SortInventory(const ainv: TBrickInventory);
begin
  case inventorysortmethod of
    1: ainv.SortPiecesByPartNumber;
    2: ainv.SortPiecesByPriceNew;
    3: ainv.SortPiecesByPriceUsed;
    4: ainv.SortPiecesByColor;
  else
    ainv.SortPieces;
  end;
end;

procedure TMainForm.Readlist1Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('sinv/' + S_READYLIST_01, foo);
end;

procedure TMainForm.Crawlerfile1Click(Sender: TObject);
begin
  if SaveDialogCrawlerInv.Execute then
  begin
    Screen.Cursor := crHourglass;
    inventory.SaveInventoryForCrawler(SaveDialogCrawlerInv.FileName);
    Screen.Cursor := crDefault;
  end;
  ChDir(basedefault);
end;

function TMainForm.taglink(const tag: string): string;
begin
  Result := '<a href="showtag/' + tag + '">' + tag + '</a>';
end;

function TMainForm.lugbulklinks(const pci: TPieceColorInfo): string;
var
  i: integer;
begin
  Result := '';
  if pci = nil then
    Exit;

  for i := 2015 to 2015 + 31 do
    if pci.GetLugbulk(i) then
    begin
      if Result = '' then
        Result := '<br><b>Lugbulks:</b> '
      else
        Result := Result + ', ';
      Result := Result + '<a href="ShowCatalogList/MC/' + itoa(i) + '/' + itoa(CATEGORYLUGBULK) + '">' + itoa(i) + '</a>'
    end;
end;

procedure TMainForm.Plates3x1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/plates3x', foo);
end;

procedure TMainForm.Plates16x1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/plates16x', foo);
end;

procedure TMainForm.Tiles3x1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/tiles3x', foo);
end;

procedure TMainForm.Tiles4x1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/tiles4x', foo);
end;

procedure TMainForm.Tiles6x1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/tiles6x', foo);
end;

procedure TMainForm.Tiles8x1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/tiles8x', foo);
end;

procedure TMainForm.Bricks4x1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/bricks4x', foo);
end;

procedure TMainForm.Bricks8x1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/bricks8x', foo);
end;

procedure TMainForm.AllBricks1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('dimentionsquery/bricksx', foo);
end;

procedure TMainForm.Alltiles1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('dimentionsquery/tilesx', foo);
end;

procedure TMainForm.Allplates1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('dimentionsquery/platesx', foo);
end;

function TMainForm.GetSetCostDbl(const setid: string): double;
var
  ocost: ordercost_t;
  costfname: string;
  sL: TStringList;
  i: integer;
  s1, s2: string;
  x: integer;
  dnum: integer;
  dcost: double;
begin
  ocost := orders.ItemCost(setid, -1);
  dnum := ocost.num;
  dcost := ocost.totcost;

  costfname := basedefault + 'out\' + setid + '\' + setid + '_cost.txt';
  if fexists(costfname) then
  begin
    sL := TStringList.Create;
    try
      sL.LoadFromFile(costfname);
      for i := 0 to sL.Count - 1 do
      begin
        splitstring(sL.Strings[i], s1, s2, ',');
        x := atoi(s1);
        dnum := dnum + x;
        dcost := dcost + x * atof(s2);
      end;
    finally
      sL.Free;
    end;
  end;

  if dnum = 0 then
    Result := 0.0
  else
    Result := dcost / dnum;
end;

function TMainForm.GetItemCostDbl(const apart: string; const acolor: integer): double;
begin
  if acolor = -1 then
    Result := GetSetCostDbl(apart)
  else
    Result := orders.ItemCostDbl(apart, acolor);
end;

procedure TMainForm.Commoninventoryof2sets1Click(Sender: TObject);
var
  set1, set2: string;
  foo: Boolean;
begin
  if Compare2SetsQuery('Common inventory of sets', set1, set2) then
    HTMLClick('common2sets/' + set1 + '/' + set2, foo);
end;

procedure TMainForm.Jumpers1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/jumpers', foo);
end;

procedure TMainForm.Connectors1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/connectors', foo);
end;

procedure TMainForm.Doorrail1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/rail', foo);
end;

end.

