//------------------------------------------------------------------------------
//
//  BrickInventory: A tool for managing your brick collection
//  Copyright (C) 2014-2018 by Jim Valavanis
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
  Dialogs, OleCtrls, SHDocVw, Readhtml, FramView, Htmlview, ExtCtrls,
  StdCtrls, Buttons, bi_docwriter, bi_db, bi_hash, bi_orders, Menus, jpeg,
  pngimage, bi_lugbulk2017, ComCtrls, bi_delphi, AppEvnts;

type
  TMainForm = class(TForm)
    HTML: THTMLViewer;
    Panel1: TPanel;
    OutputMemo: TMemo;
    Splitter1: TSplitter;
    Bevel1: TBevel;
    Timer1: TTimer;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Search1: TMenuItem;
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
    Lengthqueries1: TMenuItem;
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
    Edit1: TMenuItem;
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
    N20172: TMenuItem;
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
    N24: TMenuItem;
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
    procedure FormCreate(Sender: TObject);
    procedure HTMLImageRequest(Sender: TObject; const SRC: String;
      var Stream: TMemoryStream);
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
    procedure Queries1Click(Sender: TObject);
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
    Missingformultiplesetslist: TStringList;
    Missingfordismandaledsetslist: TStringList;
    orders: TOrders;
    lastset: string;
    dismantaledsetsinv: TBrickInventory;
    diskmirror: string;
    storagebinsupdatetime: double;
    thumbnailcache: array[0..127] of TStringList;
    function CheckAA(const AA, fromAA, toAA: integer): boolean;
    procedure Navigate(const akey: string; const pg: integer);
    procedure DrawColorCell(const cc: integer; const width: integer);
    procedure DrawColorCells(const colors: TDNumberList; const width: integer);
    procedure DrawColorCells2(const colors: TDNumberList; const width: integer);
    procedure doShowLengthQueryColor(inv: TBrickInventory; const id: string; const color: integer; inflst: TStringList);
    procedure doShowLengthQueryColorSlopes(inv: TBrickInventory; const id: string; const color: integer; inflst: TStringList);
    procedure doShowLengthQuery(inv: TBrickInventory; const id: string);
    procedure doShowLengthQuerySlopes(inv: TBrickInventory; const id: string);
    procedure dbloadprogress(const s: string; d : Double);
    procedure ShowLooseParts(inv: TBrickInventory; colormask: Integer = -1; partmask: string = ''; cat: Integer = -1;
      const fromAA: integer = -1; const toAA: integer = MAXINT);
    procedure ShowSetInventory(const setid: string; const lite: Boolean = False);
    procedure PreviewInventoryTable(inv: TBrickInventory);
    procedure PreviewSetInventory(const setid: string);
    procedure ShowColors;
    procedure ShowCategoryColors(const cat: integer);
    function HtmlDrawInvImgLink(const pcs: string; const color: integer; const pi: TPieceInfo): string;
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
    procedure DrawMoldList(const tit: string; const lst: TStringList; const splitcolorflags: boolean);
    procedure DrawMoldListCatalog(const tit: string; const lst: TStringList; const year: integer);
    procedure ShowCatalogList(const ltyp: string; const year: integer; const catid1: integer);
    procedure DrawPieceList(const tit: string; const lst: TStringList; const sortorder: integer = 0);
    procedure DrawPieceListSet(const tit: string; const settit: string; const lst: TStringList);
    procedure DrawPieceListLugbulk(const tit: string; const lst: TStringList);
    procedure DrawPieceListLugbulkKnownCost(const tit: string; const lb: TLugBulk2017;
      const over: double; const dobrickorederinfo: boolean; const catid: Integer = -1);
    procedure DrawSetAlternatePieceList(const tit: string; const lst: TStringList);
    procedure UsedPiecesbeloweuroKgr(const x: integer);
    procedure UsedPiecesaboveeuroKgr(const x: integer);
    procedure NewPiecesbeloweuroKgr(const x: integer);
    procedure NewPiecesaboveeuroKgr(const x: integer);
    procedure NewPiecesPriceAbove(const x: double);
    procedure UsedPiecesPriceAbove(const x: double);
    procedure NewPiecesPriceAboveEvaluated(const x: double);
    procedure UsedPiecesPriceAboveEvaluated(const x: double);
    procedure NewPiecesCheaperUsed;
    procedure NewPiecesMuchMoreExpensiveThanUsed(const factor: double);
    procedure PiecesDiscontinuedAtYear(const y: integer);
    procedure PiecesDiscontinuedAtYearExcludingVariations(const y: integer);
    procedure PiecesDiscontinuedAtYear_Minifigure(const y: integer);
    procedure PiecesNewAtYear(const y: integer);
    procedure PiecesNewAtYearExcludingVariations(const y: integer);
    procedure PiecesNewAtYear_Minifigure(const y: integer);
    procedure ShowPieceCInventory(const pcs: string; const color: integer);
    procedure ShowColorPiece(const pcs: string; const color: integer; const ayear: integer = -1);
    procedure ShowSetsICanBuild(const pct: double);
    procedure ShowSetsAtYear(const year: integer);
    procedure ShowSetsAtUnknownYear;
    procedure UpdateSetAssetsFromBricklink(const s: string);
    procedure ShowSetsForPartOutNew(const minyear, minavailablelots: integer;
                                    const mindemand, mincostmultiplier: double);
    procedure ShowSetsForPartOutUsed(const minyear, minavailablelots: integer;
                                    const mindemand, mincostmultiplier: double);
    procedure ShowSetsForPartInUsed(const posost: integer);
    procedure ShowSetsForPartOutWithMiniFigsNew(const minyear, minavailablelots, minminifignum: integer;
                                                const mindemand, minpartscostmultiplier, minminifigscostmultiplier, minpartoutmultiplier: double);
    procedure ShowSetsForPartOutWithMiniFigsUsed(const minyear, minavailablelots, minminifignum: integer;
                                                const mindemand, minpartscostmultiplier, minminifigscostmultiplier, minpartoutmultiplier: double);
    procedure ShowMissingFromStorageBins;
    procedure ShowCheckStorageReport;
    procedure ShowMissingToBuildSetInventory(const setid: string; const numsets: integer; legacyignore: boolean);
    procedure ShowExpensiveSetLotsNew(const setid: string; const numlots: integer);
    procedure ShowExpensiveSetLotsUsed(const setid: string; const numlots: integer);
    procedure ShowExpensiveInvNew(const atitle: string; const numlots: integer);
    procedure ShowExpensiveInvUsed(const atitle: string; const numlots: integer);
    procedure ShowExpensiveInvPartsNew(const atitle: string; const numlots: integer);
    procedure ShowExpensiveInvPartsUsed(const atitle: string; const numlots: integer);
    procedure ShowMissingToBuilMultipledSets(const setids: TStringList);
    procedure ShowInventoryForMultipledSets(const setids: TStringList);
    procedure ShowLugbulkSuggestions(const years: string; const demand, sold: integer; const price: double);
    procedure ShowCompare2Sets(const set1, set2: string);
    procedure ShowOrders(const seller: string = '');
    procedure ShowOrder(const orderid: string);
    procedure DrawOrderInf(const orderid: string);
    procedure ShowLugbulkBestPrice(const year: string; const over: double; const catid: integer  = -1);
    procedure ShowLugbulkBestPriceNoBrickOrder(const year: string; const over: double; const catid: integer  = -1);
    procedure ShowStorageBins;
    procedure ShowStorageInventory(const st: string);
    procedure ShowHomePage;
    function ShowInventorySets(const inv: TBrickInventory; const header_flash: boolean; const mocflag: integer): boolean;
    procedure ShowMySets;
    procedure ShowMyMocs;
    procedure ShowMySetsPieces;
    procedure ShowMyMocsPieces;
    procedure ShowLengthQuery(const id: string);
    procedure ShowLengthQuerySlopes(const id: string);
    procedure DrawNavigateBar;
    procedure DrawNavigateCatalog;
    procedure DrawHeadLine(const s: string);
    procedure DrawPartOutValue(inv: TBrickInventory; const setid: string = '');
    procedure DrawInventoryTable(inv: TBrickInventory; const lite: Boolean = False; const setid: string = ''; const dosort: boolean = True; const usepages: boolean = true);
    procedure DrawInventoryTableNoPages(inv: TBrickInventory; const lite: Boolean = False; const setid: string = ''; const dosort: boolean = True);
    procedure DrawBrickOrderInfo(const brick: brickpool_p; const setid: string = '');
    procedure UpdateDismantaledsetsinv;
    procedure DrawPriceguide(const part: string; const color: integer = -1);
    procedure HTMLClick(const SRC1: String; var Handled: Boolean);
    procedure StoreInventoryStatsRec(const piece: string; const color: string = '');
    procedure DoEditSet(const setid: string);
    procedure ShowUniquePiecesOfMyInventory(const ntimes: integer);
    procedure ShowMoldsWithMoreThanColors(const numcolors: integer);
    procedure ShowNameswithbothpartandsetcolorindexes;
    function MakeThumbnailImageEx(const pcs: string; const typof: char; const ncolor: integer = -1000): string;
    function MakeThumbnailImageExCache(const pcs: string; const typof: char; const ncolor: integer = -1000): string;
    function ConvertThumbnailImageExCache(const imgfile: string): boolean;
    function MakeThumbnailImage(const pcs: string; const ncolor: integer = -1000): string;
    function MakeThumbnailImage2(const pcs: string; const ncolor: integer = -1000): string;
    function FindThumbnailImageFileName(const SRC: string): string;
    function FindThumbnailImageFileNameForHtmlReq(const SRC: string): string;
  public
    { Public declarations }
    activebits: integer;
  end;

var
  MainForm: TMainForm;

implementation

uses
  bi_pak, bi_io, bi_system, bi_tmp, slpash, bi_utils, timing,
  searchset, searchpart, frm_multiplesets, PreviewForm, bl_orderxml,
  compare2sets, mosaicfrm, mosaicfrm_plates, mosaicfrm_tiles, editpiecefrm,
  removepiecefromstoragefrm, strutils, frm_diagrams, frm_lugbulksuggest,
  frm_selectsets, bi_script, frm_batch, bi_globals, frm_editsetastext,
  frm_setsforpartout_params, searchstorage, frm_setsminifigspartout_params,
  editmoldfrm, frm_update1, frm_update2, frm_update3, frm_update4,
  bi_multithread, bi_iterators, bi_defs, frm_options, bi_data;

{$R *.dfm}

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

  dismantaledsetsinv := nil;

  for i := 0 to 127 do
  begin
    thumbnailcache[i] := TStringList.Create;
    thumbnailcache[i].Sorted := True;
  end;
  streams := TStringList.Create;
  imagerequests := THashStringList.Create;
  entries := TStringList.Create;
  entriesHash := THashTable.Create;
  goback := TStringList.Create;
  gofwd := TStringList.Create;
  Missingformultiplesetslist := TStringList.Create;
  s1 := 'out\multisetsquery\missing_multisetsquery_sets.txt';
  if FileExists(s1) then
    Missingformultiplesetslist.LoadFromFile(s1);

  Missingfordismandaledsetslist := TStringList.Create;
  s1 := 'out\dismandaledsetsquery\missing_dismandaledsetsquery_sets.txt';
  if FileExists(s1) then
    Missingfordismandaledsetslist.LoadFromFile(s1);

  document := TDocument.Create(HTML);

  orders := TOrders.Create;

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
        document.Write('<td><table border=1 width=' + IntToStr(width) + ' bgcolor="#' + IntToHex(db.colors(cc).RGB, 6) + '"><tr><td><p align=center><font color=#' + IntToHex(db.colors(cc).RGB, 6) + '><b>.</b></font></p></td></tr></table></td>');
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
      document.Write('<table border=1 width=' + IntToStr(width) + ' bgcolor="#' + IntToHex(db.colors(cc).RGB, 6) + '"><tr><td><p align=center><font color=#FFFFFF><b>B</b></font></p></td></tr></table>');
    else
      document.BlancColorCell(db.colors(cc).RGB, width);
  end;
end;

procedure TMainForm.DrawNavigateBar;
begin
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2><tr>');
{  document.write('<td width=16%><a href="back">Back</a></td>');
  document.write('<td width=16%><a href="fwd">Forward</a></td>');}
  document.write('<td width=16%><a href="home">Home</a></td>');
  document.write('<td width=16%><a href="cataloghome">Catalog</a></td>');
  document.write('<td width=16%><a href="inv/0/C/-1">My loose parts</a></td>');
  document.write('<td width=16%><a href="mysets">My sets</a></td>');
  document.write('<td width=16%><a href="mymocs">My mocs</a></td>');
  document.write('<td width=16%><a href="colors">Colors</a></td>');
  document.write('<td width=16%><a href="categories">Categories</a></td>');
  document.write('<td width=16%><a href="orders">Orders</a></td>');
  document.write('<td width=16%><a href="ShowStorageBins">Storage Bins</a></td>');

  document.write('</tr></table></p></div><br><br>');

end;

procedure TMainForm.DrawNavigateCatalog;
begin
  document.write('<body background="splash.jpg">');
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
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="catalogsets">Sets</a></td></tr>');
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
  orderslist: TStringList;
  sorder: string;
begin
  Screen.Cursor := crHourglass;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowOrders', seller);

  document.write('<body background="splash.jpg">');
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

  orderslist := TStringList.Create;
  for i := 0 to orders.numorders - 1 do
    for j := 0 to orders.orders[i].Count - 1 do
      if (seller = '') or (orders.orders[i].ORDER[j].SELLER = seller) then
        orderslist.Add(itoa(orders.orders[i].ORDER[j].ORDERID));

  orderslist.Sort;

  for i := 0 to orderslist.Count - 1 do
  begin
    SplashProgress('Working...', i / orderslist.Count);
    inc(aa);
    document.StartItemId(aa);
    sorder := orderslist.Strings[i];
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
  inv.SortPieces;

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


procedure TMainForm.ShowOrder(const orderid: string);
var
  inv: TBrickInventory;
begin
  Screen.Cursor := crHourGlass;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowOrder', orderid);

  document.write('<body background="splash.jpg">');
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
  inv.SortPieces;
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
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  DrawHeadLine('Bricks Inventory');
  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');

  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>Quick links</b></th>');
  document.write('</tr>');

  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="inv/0/C/-1">My loose parts</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="mysets">My sets</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="mymocs">My mocs</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="colors">Colors</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="categories">Categories</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="orders">Orders</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="ShowStorageBins">Storage Bins</a></td></tr>');

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

  function BLCOLOR1: string;
  var
    p: integer;
    n: string;
  begin
    Result := '';
    p := Pos('\', SRC);
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
    p := Pos('\', SRC);
    if p < 1 then
      Exit;
    Result := Copy(SRC, 1, p - 1);
  end;


begin
  ChDir(basedefault);
  if Trim(SRC) = '' then
    Exit;

  if Pos('//static.bricklink', SRC) = 1 then
    Exit;

  scheck := Trim(SRC);
  if toupper(scheck[1]) = 'T' then
  begin
    if not fexists(basedefault + scheck) then
      ConvertThumbnailImageExCache(scheck);
    Exit;
  end;

  if fexists(basedefault + SRC) then
  begin
    if RightStr(SRC, 4) = '.jpg' then // set
    begin
      trydownload := not CheckValidImageDonwload(basedefault + 's\' + ExtractFileName(SRC));
      if not trydownload then
        Exit;
    end
    else
      Exit;
  end;
  idx := streams.IndexOf(strupper(SRC));
  idx2 := imagerequests.IndexOf(strupper(SRC));
  if idx2 < 0 then
    imagerequests.Add(strupper(SRC));
  if idx = -1 then
  begin
    ps := TPakStream.Create(SRC, pm_full);
    trydownload := (ps.IOResult <> 0) and (idx2 = -1);
    if not trydownload then
      if RightStr(SRC, 4) = '.jpg' then // set
        if FileExists(basedefault + 's\' + ExtractFileName(SRC)) then
          trydownload := not CheckValidImageDonwload(basedefault + 's\' + ExtractFileName(SRC));
    if trydownload then
    begin
      Screen.Cursor := crHourglass;
      ps.Free;
      if RightStr(SRC, 4) = '.jpg' then // set
      begin
        ForceDirectories(basedefault + 's\');
        jpgfilename := ExtractFileName(SRC);
        pngfilename := ChangeFileExt(jpgfilename, '.png');
        outfname := basedefault + 's\' + ExtractFileName(SRC);
        if db.IsBook(firstword(ExtractFileName(SRC), '.')) then
        begin
          if not DownloadFileImg('https://www.bricklink.com/BL/' + NewBlFileName(jpgfilename), outfname) then
            if not DownloadFileImg('http://www.bricklink.com/SL/' + NewBlFileName(jpgfilename), outfname) then
              if not DownloadFileImg('https://img.bricklink.com/ItemImage/SL/' + NewBlFileName(jpgfilename), outfname) then
                if not DownloadPngFileToJpg('https://img.bricklink.com/ItemImage/PN/0/' + NewBlFileName(pngfilename), outfname) then
                  if not DownloadPngFileToJpg('https://img.bricklink.com/ItemImage/SL/' + NewBlFileName(pngfilename), outfname) then
                    if not DownloadPngFileToJpg('https://img.bricklink.com/ItemImage/MN/0/' + NewBlFileName(pngfilename), outfname) then
                      if not DownloadPngFileToJpg('https://img.bricklink.com/ItemImage/ON/0/' + NewBlFileName(pngfilename), outfname) then
                        if not DownloadPngFileToJpg('https://img.bricklink.com/ItemImage/SN/0/' + NewBlFileName(pngfilename), outfname) then
                          if not DownloadFileImg('http://www.bricklink.com/ML/' + NewBlFileName(jpgfilename), outfname) then
                            if not DownloadFileImg('http://www.1000steine.com/brickset/images/' + NewBlFileName(jpgfilename), outfname) then
                              if not DownloadFileImg('https://images.brickset.com/sets/images/' + NewBlFileName(jpgfilename), outfname) then
                                if not DownloadFileImg('http://img.rebrickable.com/img/sets-b/' + NewBlFileName(jpgfilename), outfname) then;
        end
        else if db.IsGear(firstword(ExtractFileName(SRC), '.')) then
        begin
          if not DownloadPngFileToJpg('https://img.bricklink.com/ItemImage/GL/' + NewBlFileName(pngfilename), outfname) then
            if not DownloadPngFileToJpg('https://img.bricklink.com/ItemImage/GN/0/' + NewBlFileName(pngfilename), outfname) then
              if not DownloadFileImg('https://www.bricklink.com/BL/' + NewBlFileName(jpgfilename), outfname) then;
        end
        else
        begin
          if not DownloadFileImg('http://www.bricklink.com/SL/' + NewBlFileName(jpgfilename), outfname) then
            if not DownloadFileImg('https://www.bricklink.com/BL/' + NewBlFileName(jpgfilename), outfname) then
              if not DownloadFileImg('https://img.bricklink.com/ItemImage/SL/' + NewBlFileName(jpgfilename), outfname) then
                if not DownloadPngFileToJpg('https://img.bricklink.com/ItemImage/PN/0/' + NewBlFileName(pngfilename), outfname) then
                  if not DownloadPngFileToJpg('https://img.bricklink.com/ItemImage/SL/' + NewBlFileName(pngfilename), outfname) then
                    if not DownloadPngFileToJpg('https://img.bricklink.com/ItemImage/MN/0/' + NewBlFileName(pngfilename), outfname) then
                      if not DownloadPngFileToJpg('https://img.bricklink.com/ItemImage/ON/0/' + NewBlFileName(pngfilename), outfname) then
                        if not DownloadPngFileToJpg('https://img.bricklink.com/ItemImage/SN/0/' + NewBlFileName(pngfilename), outfname) then
                          if not DownloadFileImg('http://www.bricklink.com/ML/' + NewBlFileName(jpgfilename), outfname) then
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
          if not DownloadJpgFileToPNG('http://www.bricklink.com/SL/' + NewBlFileName(ChangeFileExt(ExtractFileName(SRC), '.jpg')), outfname) then
            DownloadJpgFileToPNG('http://www.1000steine.com/brickset/images/' + ChangeFileExt(ExtractFileName(SRC), '.jpg'), outfname);
        end
        else if RBCOLOR1 = itoa(CATALOGCOLORINDEX) then
        begin
          outfname := basedefault + itoa(CATALOGCOLORINDEX) + '\' + ExtractFileName(SRC);
          if not DownloadFileImg('https://img.bricklink.com/ItemImage/CN/0/' + NewBlFileName(ExtractFileName(SRC)), outfname) then
            if not DownloadFileImg('https://img.bricklink.com/ItemImage/CT/0/' + ChangeFileExt(NewBlFileName(ExtractFileName(SRC)), '.t1.png'), outfname) then
              if not DownloadGIFFileToPNG('http://img.bricklink.com/C/' + NewBlFileName(ChangeFileExt(ExtractFileName(SRC), '.gif')), outfname) then
                DownloadJpgFileToPNG('http://img.bricklink.com/C/' + NewBlFileName(ChangeFileExt(ExtractFileName(SRC), '.jpg')), outfname);
        end
        else if RBCOLOR1 = itoa(INSTRUCTIONCOLORINDEX) then
        begin
          outfname := basedefault + itoa(INSTRUCTIONCOLORINDEX) + '\' + ExtractFileName(SRC);
          if not DownloadFileImg('https://img.bricklink.com/ItemImage/IN/0/' + NewBlFileName(ExtractFileName(SRC)), outfname) then
            if not DownloadFileImg('https://img.bricklink.com/ItemImage/IT/0/' + ChangeFileExt(NewBlFileName(ExtractFileName(SRC)), '.t1.png'), outfname) then
              if not DownloadJpgFileToPNG('http://img.bricklink.com/I/' + NewBlFileName(ChangeFileExt(ExtractFileName(SRC), '.jpg')), outfname) then
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
                    if not DownloadJpgFileToPNG('http://lego.brickinstructions.com\' + iname2 + '\' + iname1 + '\001.jpg', outfname) then
                      DownloadJpgFileToPNG('http://lego.brickinstructions.com\' + iname2 + '\' + iname1 + '\main.jpg', outfname);
                  end;
              end;
        end
        else if RBCOLOR1 = itoa(BOXCOLORINDEX) then
        begin
          outfname := basedefault + itoa(BOXCOLORINDEX) + '\' + ExtractFileName(SRC);
          if not DownloadFileImg('https://img.bricklink.com/ItemImage/ON/0/' + NewBlFileName(ExtractFileName(SRC)), outfname) then
            DownloadFileImg('https://img.bricklink.com/ItemImage/OT/0/' + ChangeFileExt(NewBlFileName(ExtractFileName(SRC)), '.t1.png'), outfname);
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
                    imgfound := DownloadFileImg('https://img.bricklink.com/ItemImage/MN/0/' + NewBlFileName(ExtractFileName(SRC)), outfname);
                if not imgfound then
                  imgfound := DownloadFileImg('https://img.bricklink.com/ItemImage/SN/0/' + NewBlFileName(ExtractFileName(SRC)), outfname);
              end
              else
              begin
                imgfound := DownloadFileImg('https://img.bricklink.com/ItemImage/PT/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.t1.png', outfname);
              end;
            end;

            if not imgfound then
              if not DownloadJpgFileToPNG('http://img.bricklink.com/P/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.jpg', outfname) then
                if not DownloadJpgFileToPNG('http://img.bricklink.com/P/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(firstword(ExtractFileName(SRC), '.')) + '.jpg', outfname) then
                  if not DownloadJpgFileToPNG('http://www.bricklink.com/P/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.jpg', outfname) then
                    if not DownloadGIFFileToPNG('http://www.bricklink.com/P/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.gif', outfname) then
                      if not DownloadGIFFileToPNG('http://www.bricklink.com/M/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.gif', outfname) then
                        if not DownloadJpgFileToPNG('http://www.bricklink.com/M/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.jpg', outfname) then
                          if not DownloadGIFFileToPNG('https://www.bricklink.com/ML/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.gif', outfname) then
                            if not DownloadJpgFileToPNG('https://www.bricklink.com/ML/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.jpg', outfname) then
                              if not DownloadFileImg('https://img.bricklink.com/ItemImage/BN/' + BLCOLOR1 + '/' + NewBlFileName(ExtractFileName(SRC)), outfname) then
                                if not DownloadFileImg('https://img.bricklink.com/ItemImage/PN/' + BLCOLOR1 + '/' + NewBlFileName(ExtractFileName(SRC)), outfname) then
                                  DownloadFile('https://img.bricklink.com/ItemImage/GN/' + BLCOLOR1 + '/' + db.GetBLNetPieceName(db.BrickLinkPart(firstword(ExtractFileName(SRC), '.'))) + '.png', outfname)
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
        if fExists(basedefault + sTmp) then
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
  Streams.Exchange(0, idx);
  Stream := streams.Objects[0] as TMemoryStream;
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

  entries.Free;
  entriesHash.Free;
  for i := 0 to goback.Count - 1 do
    goback.Objects[i].Free;
  goback.Free;
  for i := 0 to gofwd.Count - 1 do
    gofwd.Objects[i].Free;
  gofwd.Free;
  Missingformultiplesetslist.Free;
  Missingfordismandaledsetslist.Free;

  if dismantaledsetsinv <> nil then
    dismantaledsetsinv.Free;

  SplashForm.Free;
  PAK_ShutDown;
{  I_ShutDownTempFiles;
  I_ShutDownIO;}
  MT_ShutDown;
  I_Quit;

  document.Free;

  BI_SaveDefaults(basedefault + 'bi4.ini');
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  i: integer;
  parts: TDStringList;
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
  entries.Sort;
  entriesHash.AssignStringList(entries);
  db := TSetsDatabase.Create;
  progress_string := 'Loading database...';
  db.progressfunc := dbloadprogress;
  db.InitCreate;
  db.LoadFromDisk(basedefault + 'db\db_set_pieces.txt');
  inventory := TBrickInventory.Create;
  inventory.CreateExtentedHashTable;
  if FileExists(basedefault + 'myparts.txt') then
    inventory.LoadLooseParts(basedefault + 'myparts.txt');
  if FileExists(basedefault + 'mysets.txt') then
    inventory.LoadSets(basedefault + 'mysets.txt');

  initialized := True;
  HideSplash;

  orders.LoadFilesDirectory(basedefault + 'orders');

  if not DirectoryExists(basedefault + 'out') then
    MkDir(basedefault + 'out');
  if not DirectoryExists(basedefault + 'out\navigate') then
    MkDir(basedefault + 'out\navigate');
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
  if not DirectoryExists(basedefault + 'orders') then
    MkDir(basedefault + 'orders');
  if not DirectoryExists(basedefault + 'storage') then
    MkDir(basedefault + 'storage');
  if not DirectoryExists(basedefault + 'images') then
    MkDir(basedefault + 'images');
  BI_CheckDefaultAssets(basedefault);

  document.savepath := basedefault + 'out\navigate\';

  goback.AddObject('home', TScrollPos.Create(0, 0));
  ShowHomePage;
{  db.CrawlerPriorityPart('15714', 15);
  db.CrawlerPriorityPart('15714', 41);
  db.CrawlerPriorityPart('15714', 14);
  db.CrawlerPriorityPart('15714', 4);
  db.CrawlerPriorityPart('15714', 1);}
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
  document.write('<b>Inventory (Part Out) Value</b>');
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
  dismantaledsetsinv.Free;
  dismantaledsetsinv := TBrickInventory.Create;

  for i := 0 to inventory.numsets - 1 do
    if inventory.sets[i].dismantaled > 0 then
    begin
      inv2 := db.GetSetInventory(inventory.sets[i].setid);
      dismantaledsetsinv.MergeWith(inv2);
    end;

end;

procedure TMainForm.DrawBrickOrderInfo(const brick: brickpool_p; const setid: string = '');
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
  removeforbuildsetstr: string;
begin
  if not dodraworderinfo then
    Exit;

  oinf := orders.ItemInfo(brick.part, brick.color);
  if oinf <> nil then
  begin
    for i := 0 to oinf.Count - 1 do
    begin
      oitem := oinf.Objects[i] as TOrderItemInfo;
      document.write('<tr bgcolor=#EEEEEE>');

      if oitem.orderstatus = 'NSS' then
        ss1 := '<font color="red">'
      else if (oitem.orderstatus = 'Completed') or (oitem.orderstatus = 'Received') then
        ss1 := '<font color="green">'
      else
        ss1 := '';
      if ss1 = '' then
        ss2 := ''
      else
        ss2 := '</font>';

      document.write('<td colspan="4">%s<b><a href="order/%d">%d</a></b> %s %s [%s] %s</td>',
          [ss1,
           oitem.orderid,
           oitem.orderid,
           oitem.orderdate,
           '<a href=sellerorders/' + oitem.orderseller + '>' + oitem.orderseller + '</a>',
           oitem.orderstatus,
           ss2]);

      document.write('<td align="right">%d</td>',
          [oitem.num]);

      document.write('<td align="right" colspan="3">(%s) %s %2.3f (%2.3f)<br>%2.3f (%2.3f)',
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


      if oitem.orderstatus = 'NSS' then
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
              document.write('<td colspan="4"><b><a href="sinv/%s">%d x %s</a></b> - %s ' + MakeThumbnailImage2(sid, -1) + '</td>',
                [sid, inventory.sets[i].dismantaled,
                 sid, db.SetDesc(sid)]);
              document.write('<td align="right">%d</td>', [num]);
              document.write('<td colspan="3"><br></td></tr>');
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
      if setid = '' then
        removeforbuildsetstr := ''
      else
        removeforbuildsetstr :=
          ' <a href=removepiecefromstorage/' + brick.part + '/' + itoa(brick.color) + '/' +
              setid + '/' + stor + '><img src="images\remove.png"></a>';

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
        if Pos('-', sset) = 0 then
          sset := sset + '-1'
        else if Pos('-', sset) = Length(sset) then
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
      document.write('<td colspan="8">%s</td></tr>',
                ['<b><a href=storage/' + stor + '>' + stor + '</a></b>:' + stmp3 + removeforbuildsetstr]);
    end;
  end;

  document.write('<tr bgcolor=' + TFGCOLOR + '><td colspan="8"></td></tr>');
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
  num: integer;
  accurstr: string;
  totalweight: double;
  totalcostwn: double;
  totalcostwu: double;
  scolor: string;
begin
  UpdateDismantaledsetsinv;

  if inv = nil then
    inv := inventory;

  Screen.Cursor := crHourGlass;

  inv.SortPieces;

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
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=35%><img src=' + scolor + '\' + brick.part + '.png><br><b>');
    document.write('<a href=spiece/' + brick.part + '>' + brick.part + '</a></b>');
    document.write(' - ' + db.PieceDesc(brick.part) + '</td><td width=20%>');
    DrawColorCell(brick.color, 25);
//    document.BlancColorCell(db.colors(brick.color).RGB, 25);
//    pci := db.PieceColorInfo(brick.part, brick.color);
    pci := db.PieceColorInfo(brick);
    document.write('<a href=spiecec/' + brick.part + '/' + scolor + '>' +  db.colors(brick.color).name + ' (' + scolor + ') (BL=' + IntToStr(db.colors(brick.color).BrickLingColor) + ')</td>');
    document.write('<td width=10% align=right>' + IntToStr(brick.num));
    document.write('</td>');
    pi := db.PieceInfo(pci);
    if pci <> nil then
    begin
      prn := pci.EvaluatePriceNew;
      pru := pci.EvaluatePriceUsed;
      document.write('<td width=15% align=right>' + Format('€ ' + accurstr + '<br>€ ' + accurstr + '<br>€ %2.2f / Krg', [prn, prn * brick.num, dbl_safe_div(prn, pi.weight) * 1000]) + '</td>');
      document.write('<td width=15% align=right>' + Format('€ ' + accurstr + '<br>€ ' + accurstr + '<br>€ %2.2f / Krg', [pru, pru * brick.num, dbl_safe_div(pru, pi.weight) * 1000]) + '</td>');
      prnt := prnt + prn * brick.num;
      prut := prut + pru * brick.num;
      if pi.weight > 0.0 then
      begin
        totalweight := totalweight + pi.weight * brick.num;
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
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f<br>€ %2.2f / Krg', [prnt, dbl_safe_div(totalcostwn, totalweight) * 1000]) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f<br>€ %2.2f / Krg', [prut, dbl_safe_div(totalcostwu, totalweight) * 1000]) + '</b></td>');
  document.write('</tr>');
  SplashProgress('Working...', 1);

  cl.Free;
  pl.Free;

  HideSplash;

  document.write('</tr></table>');

  Screen.Cursor := crDefault;
end;

procedure TMainForm.DrawInventoryTableNoPages(inv: TBrickInventory; const lite: Boolean = False; const setid: string = ''; const dosort: boolean = True);
begin
  DrawInventoryTable(inv, lite, setid, dosort, false);
end;

procedure TMainForm.DrawInventoryTable(inv: TBrickInventory; const lite: Boolean = False; const setid: string = ''; const dosort: boolean = True; const usepages: boolean = true);
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
  totalweight: double;
  totalcostwn: double;
  totalcostwu: double;
  scolor: string;
begin
  UpdateDismantaledsetsinv;

  ShowSplash;
  SplashProgress('Working...', 0);

  if inv = nil then
    inv := inventory;

  Screen.Cursor := crHourGlass;

  if dosort then
    inv.SortPieces;

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
  if not lite then
    document.write('<th>U</th>');
  if not lite then
    document.write('<th>Cost</th>');
  document.write('</tr>');

  brick := @inv.looseparts[0];
  aa := 0;
  prnt := 0;
  prut := 0;
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
  totalcostwn := 0.0;
  totalcostwu := 0.0;
  for i := 0 to inv.numlooseparts - 1 do
  begin
    inc(aa);
    if usepages then
      if not lite then
        document.StartItemId(aa);
    scolor := itoa(brick.Color);
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=35%><img src=' + scolor + '\' + brick.part + '.png><br><b>');
    document.write('<a href=spiece/' + brick.part + '>' + brick.part + '</a></b>');
    document.write(' - ' + db.PieceDesc(brick.part) + '</td><td width=20%>');
    DrawColorCell(brick.color, 25);
//    document.BlancColorCell(db.colors(brick.color).RGB, 25);
//    pci := db.PieceColorInfo(brick.part, brick.color);
    pci := db.PieceColorInfo(brick);
    pi := db.PieceInfo(pci);
    if pi = nil then
      pi := db.PieceInfo(brick.part);
    if lite or (pci = nil) then
      document.write('<a href=spiecec/' + brick.part + '/' + scolor + '>' +  db.colors(brick.color).name + ' (' + scolor + ') (BL=' + IntToStr(db.colors(brick.color).BrickLingColor) + ')<img src="images\details.png"></a>' + HtmlDrawInvImgLink(brick.part, brick.color, pi) + '</td>')
    else
      document.write('<a href=spiecec/' + brick.part + '/' + scolor + '>' +  db.colors(brick.color).name + ' (' + scolor + ') (BL=' + IntToStr(db.colors(brick.color).BrickLingColor) + ')<img src="images\details.png"></a>' + HtmlDrawInvImgLink(brick.part, brick.color, pi) +
        decide(pci.setmost='','', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + '</td>');
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
    if pci <> nil then
    begin
      prn := pci.EvaluatePriceNew;
      pru := pci.EvaluatePriceUsed;
      document.write('<td width=' + decide(lite, '15%', '10%') + ' align=right>' + Format('€ ' + accurstr + '<br>€ ' + accurstr + '<br>€ %2.2f / Krg', [prn, prn * brick.num, dbl_safe_div(prn, pi.weight) * 1000]) + '</td>');
      if not lite then
        document.write('<td width=10% align=right>' + Format('€ ' + accurstr + '<br>€ ' + accurstr + '<br>€ %2.2f / Krg', [pru, pru * brick.num, dbl_safe_div(pru, pi.weight) * 1000]) + '</td>');
      prnt := prnt + prn * brick.num;
      prut := prut + pru * brick.num;
      if pi.weight > 0.0 then
      begin
        totalweight := totalweight + pi.weight * brick.num;
        totalcostwn := totalcostwn + prn * brick.num;
        totalcostwu := totalcostwu + pru * brick.num;
      end;
    end;
    mycost := orders.ItemCost(brick.part, brick.color);
    if not lite then
      document.write('<td width=10% align=right>' + Format('€ %2.4f<br>€ %2.4f', [mycost, mycost * brick.num]) + '</td>');
    mycosttot := mycosttot + mycost * brick.num;
    if mycost > 0.0 then
      mytotcostpieces := mytotcostpieces + brick.num;

    document.write('</tr>');

    if cl.IndexOf(brick.color) < 0 then
      cl.Add(brick.color);
    if pl.IndexOf(brick.part) < 0 then
      pl.Add(brick.part);
    num := num + brick.num;

    if lite then
      DrawBrickOrderInfo(brick)
    else
      DrawBrickOrderInfo(brick, setid);
    Inc(brick);
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
  if usepages then
    if not lite then
      document.EndNavigateSection;

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
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f<br>€ %2.2f / Krg', [prnt, dbl_safe_div(totalcostwn, totalweight) * 1000]) + '</b></td>');
  if not lite then
  begin
    document.write('<td width=10% align=right><b>' + Format('€ %2.2f<br>€ %2.2f / Krg', [prut,  dbl_safe_div(totalcostwu, totalweight) * 1000]) + '</b></td>');
    if num = 0 then
      document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [mycosttot]) + '</b></td>')
    else
      document.write('<td width=10% align=right><b>' + Format('€ %2.2f<br>%2.3f%s', [mycosttot, 100 * mytotcostpieces / num, '%']) + '</b></td>');
  end;

  document.write('</tr></table>');

  cl.Free;
  pl.Free;

//  document.write('</tr></table>');

  HideSplash;

  Screen.Cursor := crDefault;
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
    DrawNavigateBar;
    document.write('<div style="color:' + DFGCOLOR + '">');
    document.write('<p align=center>');
    link := '<a href=dismantleallsets>Dismantle All Sets</a> - <a href=buildallsets>Build All Sets</a>';

    tot := 0;
    if mocflag = 3 then
      for i := 0 to inv.numsets - 1 do
        if inv.sets[i].num > 0 then
        begin
          sinv := db.GetSetInventory(inv.sets[i].setid);
          if sinv <> nil then
            tot := tot + sinv.totallooseparts * inv.sets[i].num;
        end;

    if mocflag = 1 then
      for i := 0 to inv.numsets - 1 do
        if not db.IsMoc(inv.sets[i].setid) then
          if inv.sets[i].num > 0 then
          begin
            sinv := db.GetSetInventory(inv.sets[i].setid);
            if sinv <> nil then
              tot := tot + sinv.totallooseparts * inv.sets[i].num;
          end;

    if mocflag = 2 then
      for i := 0 to inv.numsets - 1 do
        if db.IsMoc(inv.sets[i].setid) then
          if inv.sets[i].num > 0 then
          begin
            sinv := db.GetSetInventory(inv.sets[i].setid);
            if sinv <> nil then
              tot := tot + sinv.totallooseparts * inv.sets[i].num;
          end;

    if mocflag = 3 then
      DrawHeadLine('My Sets and Mocs (' + IntToStr(inv.numsets) + ' lots)<br>(' + IntToStr(inv.totalsetsbuilted) + ' builted - ' + IntToStr(inv.totalsetsdismantaled) + ' dismantaled)<br><br>' +
        'My builded sets have <a href="ShowMySetsPieces">' + IntToStr(tot) + ' parts</a><br><br>' +
        link);
    if mocflag = 2 then
      DrawHeadLine('My Mocs <br><br>' +
        'My builded mocs have <a href="ShowMyMocsPieces">' + IntToStr(tot) + ' parts</a><br><br>');
  end;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  if mocflag = 2 then
    document.write('<th><b>Moc</b></th>')
  else
    document.write('<th><b>Set</b></th>');
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

procedure TMainForm.ShowMySets;
begin
  ShowInventorySets(inventory, True, 3);
end;

procedure TMainForm.ShowMyMocs;
begin
  ShowInventorySets(inventory, True, 2);
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
begin
  Screen.Cursor := crHourGlass;

  lastset := setid;

{  if domultipagedocuments then
    if not lite then
      document.NewMultiPageDocument('ShowSetInventory', setid);}

  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  inv := db.GetSetInventory(setid);
  if inv = nil then
  begin
    if Trim(setid) <> '' then
      lnk := '<a href=downloadset/' + setid + '>Try to download the inventory from bricklink.com</a>'
    else
      lnk := '';
    DrawHeadLine('Can not find inventory for ' + setid + ' <a href=DoEditSet/' + setid + '><img src="images\edit.png"></a><br><br>' + lnk);
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

  if not db.IsMoc(setid) then
  begin
    lnk := '<a href=downloadsetandrefresh/' + setid + '>Refresh set inventory from bricklink.com</a>';
    DrawHeadLine(lnk);
  end;
  inv.DoUpdateCostValues;
  inv.SortPieces;

  s1 := basedefault + 'out\' + Trim(setid) + '\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s2 := s1 + Trim(setid) + '.txt';
  inv.SaveLooseParts(s2);

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
  ss1 := Format('Inventory for %s - %s <br>(%d lots, %d parts, %d sets)<br>You have %s%d%s builted and %s%d%s dismantaled<br><img width=360px src=s\' + setid + '.jpg>' +
      ' <a href=DoEditSet/' + setid + '><img src="images\edit.png"></a>' +
      ' <a href=PreviewSetInventory/' + setid + '><img src="images\print.png"></a>' +
      ' <a href=diagrampiece/' + setid + '/-1><img src="images\diagram.png"></a><br>' +

      '[Year: <a href=ShowSetsAtYear/%d>%d</a>]<br>',
    ['<a href=spiece/' + setid + '>' + setid + '</a>', db.SetDesc(setid), inv.numlooseparts, inv.totallooseparts, inv.totalsetsbuilted + inv.totalsetsdismantaled,
     sf2, st.num, sf1, sf4, st.dismantaled, sf3, db.SetYear(setid), db.SetYear(setid)]);
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
  if missing = 0 then
    DrawHeadLine('You can built this set! <a href=buildset/' + Trim(setid) + '>(Built it!)</a>')
  else
    DrawHeadLine(Format('%d part' + decide(missing = 1, ' is', 's are') + ' missing to build a copy of this set (%2.2f%s) (<a href="missingtobuildset/%s">show</a>)',
      [missing, dbl_safe_div(missing * 100, inv.totallooseparts), '%', Trim(setid)]));

  missing2 := inventory.MissingToBuildSetLegacyIgnore(Trim(setid));
  DrawHeadLine(Format('%d part' + decide(missing2 <= 1, ' is', 's are') + ' missing to build a copy of this set ignoring legacy colors (%2.2f%s) (<a href="missingtobuildsetLI/%s">show</a>)',
    [missing2, dbl_safe_div(missing2 * 100, inv.totallooseparts), '%', Trim(setid)]));

  DrawHeadLine(Format('<a href="missingtobuildset/%s/2">Check to build 2 sets</a>', [Trim(setid)]));

  numbricks := inv.totalloosepartsbycategory(5);
  numplates := inv.totalloosepartsbycategory(26);
  numtiles := inv.totalloosepartsbycategory(37) + inv.totalloosepartsbycategory(39);
  numslopes := inv.totalloosepartsbycategory(31) + inv.totalloosepartsbycategory(32) + inv.totalloosepartsbycategory(33);
  expensizestr := 'Most expensive lots: <a href="ShowExpensiveSetLotsNew/' + Trim(setid) + '/10">New</a> - <a href="ShowExpensiveSetLotsUsed/' + Trim(setid) + '/10">Used</a>';

  DrawHeadLine(Format('Bricks: %d<br>Slopes: %d<br>Plates: %d<br>Tiles: %d<br>Other %d<br>' + expensizestr, [numbricks, numslopes, numplates, numtiles, inv.totallooseparts - numbricks - numplates - numslopes - numtiles]));

  DrawPriceguide(setid);
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

  if FileExists(basedefault + 'db\sets\' + Trim(setid) + '.alternatives.txt') then
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
    DrawHeadLine('<a href=downloadsetaltparts/' + Trim(setid) + '>Try to download alternate parts from bricklink.com</a>');
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
  inv.SortPieces;

  s1 := basedefault + 'out\' + Trim(setid) + '\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s2 := s1 + Trim(setid) + '.txt';
  inv.SaveLooseParts(s2);

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
    document.write('<td width=50%>');
    DrawColorCell(i, 25);
//    document.BlancColorCell(cp.RGB, 25);
    document.write('<a href="inv/' + IntToStr(Integer(inv)) +'/C/' + IntToStr(decide(i = -1, 9999, i)) + '">');
    document.write('<b>' + cp.name + '</b> (' + IntToStr(cp.id) + ') (BL=' + IntToStr(cp.BrickLingColor) +  ')</a></td>');
    document.write('<td width=25% align=right>' + IntToStr(inv.numlotsbycolor(i)) + '</td>');
    document.write('<td width=25% align=right>' + IntToStr(inv.totalloosepartsbycolor(i)) + '</td>');
    document.write('<td width=25% align=right>' + Format('%2.3f', [inv.weightbycolor(i) / 1000]) + '</td>');

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
      document.write('<b>' + cp.name + '</b> (' + IntToStr(cp.id) + ') (BL=' + IntToStr(cp.BrickLingColor) +  ')</a></td>');
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


function TMainForm.HtmlDrawInvImgLink(const pcs: string; const color: integer; const pi: TPieceInfo): string;
var
  inv: TBrickInventory;
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
        Result := '';
    end;
    Exit;
  end;

  if (color = 9996) or (color = 9997) or (color = 9998) then
  begin
    Result := '';
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
      if upcs = strupper(db.AllPieces.Strings[i]) then
      begin
        idx := i;
        break;
      end;
  end;

  if idx < 0 then
  begin
    if db.GetPieceColorFromCode(pcs, spiece, scolor) then
    begin
      ShowColorPiece(spiece, scolor);
      Exit;
    end;

    document.write('<body background="splash.jpg">');
    DrawNavigateBar;
    document.write('<div style="color:' + DFGCOLOR + '">');
    document.write('<p align=center>');

    DrawHeadLine('Can not find piece ' + pcs + ' <a href=editmold/' + pcs + '><img src="images\edit.png"></a>' +
                                               ' <a href=refreshpiecefrombricklink/' + pcs + '><img src="images\refreshcolors.png"></a>');
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');

    tmpsets := TStringList.Create;
    for i := 0 to db.AllPieces.Count - 1 do
    begin
      desc := db.PieceDesc(db.AllPieces.Strings[i]);
      if Pos(strupper(pcs), strupper(desc)) > 0 then
        tmpsets.Add(db.AllPieces.Strings[i]);
    end;

    if tmpsets.Count = 0 then
      for i := 0 to db.AllPieces.Count - 1 do
      begin
        desc := db.AllPieces.Strings[i];
        if Pos(strupper(pcs), strupper(desc)) > 0 then
          tmpsets.Add(desc);
      end;

    tmpsets.Sort;

    for i := 0 to tmpsets.Count - 1 do
    begin
      DrawHeadLine(Format('<a href="spiece/%s">%s - %s</a>',
        [tmpsets.Strings[i], tmpsets.Strings[i], db.PieceDesc(tmpsets.Strings[i])]));
    end;
    tmpsets.Free;
    document.write('</body>');
    document.SaveBufferToFile(diskmirror);
    document.Flash;
    Screen.Cursor := crDefault;
    Exit;
  end;

  document.write('<body background="splash.jpg">');
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
    DrawHeadLine(pcs + ' - ' + db.PieceDesc(pcs) + ' (' + Format('%2.2f gr', [pi.weight]) + ')' + ' <a href=editmold/' + pcs + '><img src="images\edit.png"></a>' + ' <a href=refreshpiecefrombricklink/' + pcs + '><img src="images\refreshcolors.png"></a>' + refrhtml + '<br>' + cathtml)
  else
    DrawHeadLine(pcs + ' - ' + db.PieceDesc(pcs) + ' <a href=editmold/' + pcs + '><img src="images\edit.png"></a>' + ' <a href=refreshpiecefrombricklink/' + pcs + '><img src="images\refreshcolors.png"></a>' + refrhtml + '<br>' + cathtml);

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
    for i := 0 to pyears.Count - 1 do
    begin
      pyearsstr := pyearsstr + ' - ';
      if year = pyears.Numbers[i] then
        pyearsstr := pyearsstr + decide(pyears.Numbers[i] = 0, '(Unknown)', itoa(pyears.Numbers[i]))
      else
        pyearsstr := pyearsstr + '<a href=spiece/' + pcs + '/' + itoa(pyears.Numbers[i]) + '>' + decide(pyears.Numbers[i] = 0, '(Unknown)', itoa(pyears.Numbers[i])) + '</a>';
    end;
    if pyearsstr <> '' then
      DrawHeadLine(pyearsstr);
  end;

  pyears.Free;

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
              document.write('<td width=10%><img src=' + IntToStr(i) + '\' + pcs + '.png>');
              DrawColorCell(i, 25);
  //            document.BlancColorCell(db.colors(i).RGB, 25);
              document.write('<a href=spiecec/' + pcs + '/' + IntToStr(i) + '>' + db.colors(i).name + ' (' + IntToStr(i) + ') (BL=' + IntToStr(db.colors(i).BrickLingColor) + ')<img src="images\details.png"></a>' +
                  HtmlDrawInvImgLink(pcs, i, pi) + '</td>');
              document.write('<td width=10% align=right>' + Format('%d', [numpieces]) +
                '<br><a href=editpiece/' + pcs + '/' + itoa(i) + '><img src="images\edit.png"></a>' +
                '<br><a href=diagrampiece/' + pcs + '/' + itoa(i) + '><img src="images\diagram.png"></a>' +
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
            document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');
            document.write('<td width=35%><img src=' + IntToStr(i) + '\' + pcs + '.png></td>');
            document.write('<td width=20%>');
            DrawColorCell(i, 25);
            // document.BlancColorCell(db.colors(i).RGB, 25);
            if pci <> nil then
            begin
              document.write('<a href=spiecec/' + pcs + '/' + IntToStr(i) + '>' + db.colors(i).name + ' (' + IntToStr(i) + ') (BL=' + IntToStr(db.colors(i).BrickLingColor) + ')<img src="images\details.png"></a>' +
                HtmlDrawInvImgLink(pcs, i, pi) +
                decide(pci.setmost='','', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + '</td>');
              document.write('<td width=10% align=right>');
              document.write('N=%2.3f<br>U=%2.3f</td>', [pci.nDemand, pci.uDemand]);
            end
            else
            begin
              document.write('<a href=spiecec/' + pcs + '/' + IntToStr(i) + '>' + db.colors(i).name + ' (' + IntToStr(i) + ') (BL=' + IntToStr(db.colors(i).BrickLingColor) + ')<img src="images\details.png"></a>' +
                HtmlDrawInvImgLink(pcs, i, pi) + '</td>');
              document.write('<td width=10% align=right></td>');
            end;
            document.write('<td width=15% align=right>' + Format('%d', [numpieces]) +
              '<br><a href=editpiece/' + pcs + '/' + itoa(i) + '><img src="images\edit.png"></a>' +
              '<br><a href=diagrampiece/' + pcs + '/' + itoa(i) + '><img src="images\diagram.png"></a>' +
              '</td>');

            if pci <> nil then
            begin
              prn := pci.EvaluatePriceNew;
              pru := pci.EvaluatePriceUsed;
              document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Krg', [prn, dbl_safe_div(prn, pi.weight) * 1000]) + '</td>');
              document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Krg', [pru, dbl_safe_div(pru, pi.weight) * 1000]) + '</td>');
              prnt := prnt + prn * numpieces;
              prut := prut + pru * numpieces;

            end
            else
            begin
              document.write('<td width=15% align=right>-</td>');
              document.write('<td width=15% align=right>-</td>');
            end;
            mycost := orders.ItemCost(pcs, i);
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

procedure TMainForm.DrawMoldList(const tit: string; const lst: TStringList; const splitcolorflags: boolean);
var
  i: integer;
  pcs: string;
  aa: integer;
  ncolors: TDNumberList;
begin
  UpdateDismantaledsetsinv;
  ShowSplash;
  SplashProgress('Working...', 0);

  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine(tit);

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
    inc(aa);
    pcs := lst.Strings[i];
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');

    document.write('<td width=25%>');
    document.write(MakeThumbnailImage2(pcs) + '<b><a href=spiece/' + pcs + '>' + pcs + '</a></b>');
    document.write(' - ' + db.PieceDesc(pcs) + '</td>');

    ncolors := db.GetMoldKnownColors(pcs);
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


  document.write('</table><br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

end;

procedure TMainForm.DrawMoldListCatalog(const tit: string; const lst: TStringList; const year: integer);
var
  i: integer;
  pcs: string;
  aa: integer;
begin
  UpdateDismantaledsetsinv;
  ShowSplash;
  SplashProgress('Working...', 0);

  if domultipagedocuments then
    document.NewMultiPageDocument('DrawMoldListCatalog' + tit, lst.Text);

  document.write('<body background="splash.jpg">');
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
    inc(aa);
    document.StartItemId(aa);
    pcs := lst.Strings[i];
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');

    document.write('<td width=25%>');
    document.write(MakeThumbnailImage(pcs, (lst.Objects[i] as TDNumberList).Numbers[0]) + '<b><a href=spiece/' + pcs + '/' + itoa(year) + '>' + pcs + '</a></b>');
    document.write(' - ' + db.PieceDesc(pcs) + '</td>');

    document.write('<td width=10%><p align=center>' + itoa((lst.Objects[i] as TDNumberList).Count) + '</p></td>');
    document.write('<td width=65%>');
    DrawColorCells(lst.Objects[i] as TDNumberList, 25);

    document.write('</td></tr>');

    SplashProgress('Working...', i / lst.Count);
  end;

  document.EndNavigateSection;

  document.write('</table><br>');
  document.write('</p>');
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

procedure TMainForm.ShowCatalogList(const ltyp: string; const year: integer; const catid1: integer);
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
  okboxinstructions: boolean;
  cyears: TStringList;
  ccategories: TStringList;
  catid: integer;
  ccatid: integer;
  cyearid: integer;
  sdoc: TStringList;
  typ: string;
begin
  Screen.Cursor := crHourGlass;

  if catid1 >= MAXCATEGORIES - 1 then
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
  else if typ = 'S' then
  begin
    tit_typ := '<a href="catalogsets">Sets</a>';
    cranges.Add(-1);
    cranges.Add(INSTRUCTIONCOLORINDEX);
    cranges.Add(BOXCOLORINDEX);
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

          if okmoc or okboxinstructions or (pci.ItemType = typ) or (typ = '') then
            if (year < 0) or (pci.year = year) then
            begin
              pi := db.PieceInfo(pci);
              if ((catid < 0) or (pi.category = catid)) and (pci.piece <> '') then
              begin
                l := Ord(pci.piece[1]) div 2;
                idx := pilsts[l].IndexOf(pci.piece);
                if idx < 0 then
                  if not okboxinstructions then
                  begin
                    idx := pilsts[l].AddObject(pci.piece, TDNumberList.Create);
                    idx2 := ccategories.IndexOf(itoa(pi.category));
                    if idx2 < 0 then
                      idx2 := ccategories.AddObject(itoa(pi.category), TCounterObject.Create);
                    (ccategories.Objects[idx2] as TCounterObject).IncL;
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
                  (pilsts[l].Objects[idx] as TDNumberList).Add(i);
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
  if (year >= 0) or (catid >= 0) then
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

  if (year < 0) and (catid < 0) then
  begin
    document.write('<body background="splash.jpg">');
    DrawNavigateBar;
    document.write('<div style="color:' + DFGCOLOR + '">');
    document.write('<p align=center>');

    if tit_typ = '' then
      DrawHeadLine(tit + ' - All Items')
    else
      DrawHeadLine(tit + ' - ' + htmlstripstring(tit_typ));

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
          if ccatid < MAXCATEGORIES - 1 then
            if ccatid > 0 then
              sdoc.Add('<a href=ShowCatalogList/' + typ + '/-1/' + itoa(ccatid) + '>' + db.categories[ccatid].name + '</a> (' +
                itoa((ccategories.Objects[i] as TCounterObject).value) + ')<br>');
      end;
      sdoc.CustomSort(sortpiecelist_htmlstrip);
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
    DrawMoldListCatalog(tit, pilst, year);
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

const
  SORT_NONE = 0;
  SORT_PRICE_NEW = 1;
  SORT_PRICE_USED = 2;

procedure TMainForm.DrawPieceList(const tit: string; const lst: TStringList; const sortorder: integer = 0);
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
begin
  UpdateDismantaledsetsinv;
  ShowSplash;
  SplashProgress('Working...', 0);

  if domultipagedocuments then
    document.NewMultiPageDocument('DrawPieceList', tit + itoa(sortorder) + itoa(lst.count));

  document.write('<body background="splash.jpg">');
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
  document.write('<th>N</th>');
  document.write('<th>U</th>');
  document.write('<th>Cost</th>');
  document.write('</tr>');

  aa := 0;
  prnt := 0.0;
  prut := 0.0;
  totpieces := 0;
  mycosttot := 0.0;

  case sortorder of
    SORT_PRICE_NEW: lst.CustomSort(sortpiecelist_price_new);
    SORT_PRICE_USED: lst.CustomSort(sortpiecelist_price_used);
  end;

  for i := 0 to lst.Count - 1 do
  begin
    inc(aa);
    document.StartItemId(aa);
    splitstring(lst.Strings[i], pcs, col, ',');
    cl := atoi(col);
    numpieces := inventory.LoosePartCount(pcs, cl);
    totpieces := totpieces + numpieces;
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');

    document.write('<td width=35%>' + decide((cl = 9996) or (cl = 9997) or (cl = 9998), MakeThumbnailImage(pcs, cl),'<img src=' + col + '\' + pcs + '.png>'));
    document.write('<a href=spiece/' + pcs + '>' + pcs + '</a></b>');
    document.write(' - ' + db.PieceDesc(pcs) + '</td>');
    document.write('<td width=20%>');
    DrawColorCell(cl, 25);
//    document.BlancColorCell(db.colors(cl).RGB, 25);

    pci := db.PieceColorInfo(pcs, cl, lst.Objects[i]);
    pi := db.PieceInfo(pci);
    if pci = nil then
      document.write('<a href=spiecec/' + pcs + '/' + col + '>' + db.colors(cl).name + ' (' + col + ') (BL=' + IntToStr(db.colors(cl).BrickLingColor) + ')<img src="images\details.png"></a>' +
        HtmlDrawInvImgLink(pcs, cl, pi) + '</td>')
    else
      document.write('<a href=spiecec/' + pcs + '/' + col + '>' + db.colors(cl).name + ' (' + col + ') (BL=' + IntToStr(db.colors(cl).BrickLingColor) + ')<img src="images\details.png"></a>' +
        HtmlDrawInvImgLink(pcs, cl, pi) +
          decide(pci.setmost='','', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + '</td>');

    document.write('<td width=15% align=right>' + Format('%d', [numpieces]) +
            '<br><a href=editpiece/' + pcs + '/' + itoa(cl) + '><img src="images\edit.png"></a>' +
            '<br><a href=diagrampiece/' + pcs + '/' + itoa(cl) + '><img src="images\diagram.png"></a>' +
            '</td>');

    if pci <> nil then
    begin
      prn := pci.EvaluatePriceNew;
      pru := pci.EvaluatePriceUsed;
      document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Krg', [prn, dbl_safe_div(prn, pi.weight) * 1000]) + '</td>');
      document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Krg', [pru, dbl_safe_div(pru, pi.weight) * 1000]) + '</td>');
      prnt := prnt + prn * numpieces;
      prut := prut + pru * numpieces;

    end
    else
    begin
      document.write('<td width=15% align=right>-</td>');
      document.write('<td width=15% align=right>-</td>');
    end;
    mycost := orders.ItemCost(pcs, cl);
    document.write('<td width=10% align=right>' + Format('€ %2.4f<br>€ %2.4f', [mycost, mycost * numpieces]) + '</td>');
    mycosttot := mycosttot + mycost * numpieces;
    document.write('</tr>');

    bp.part := pcs;
    bp.color := cl;
    bp.num := numpieces;

    DrawBrickOrderInfo(@bp);
    if (lst.Count < 250) or (i mod 5 = 0) then
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

  document.write('<br></table></p></div></body>');
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
begin
  UpdateDismantaledsetsinv;
  ShowSplash;
  SplashProgress('Working...', 0);

  if domultipagedocuments then
    document.NewMultiPageDocument('DrawPieceListSet', tit + settit + itoa(lst.count));
  document.write('<body background="splash.jpg">');
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
    totpieces := totpieces + numpieces;
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');

    document.write('<td width=35%><img src=' + col + '\' + pcs + '.png>');
    document.write('<a href=spiece/' + pcs + '>' + pcs + '</a></b>');
    document.write(' - ' + db.PieceDesc(pcs) + '</td>');
    document.write('<td width=20%>');
    DrawColorCell(cl, 25);
    // document.BlancColorCell(db.colors(cl).RGB, 25);

    pci := db.PieceColorInfo(pcs, cl, lst.Objects[i]);
    pi := db.PieceInfo(pci);
    if pci = nil then
      document.write('<a href=spiecec/' + pcs + '/' + col + '>' + db.colors(cl).name + ' (' + col + ') (BL=' + IntToStr(db.colors(cl).BrickLingColor) + ')<img src="images\details.png"></a>' +
        HtmlDrawInvImgLink(pcs, cl, pi) + '</td>')
    else
      document.write('<a href=spiecec/' + pcs + '/' + col + '>' + db.colors(cl).name + ' (' + col + ') (BL=' + IntToStr(db.colors(cl).BrickLingColor) + ')<img src="images\details.png"></a>' +
        HtmlDrawInvImgLink(pcs, cl, pi) +
          decide(pci.setmost='','', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + '</td>');

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
      document.write('<td width=12% align=right>' + Format('€ %2.4f<br>€ %2.2f / Krg', [prn, dbl_safe_div(prn, pi.weight) * 1000]) + '</td>');
      document.write('<td width=12% align=right>' + Format('€ %2.4f<br>€ %2.2f / Krg', [pru, dbl_safe_div(pru, pi.weight) * 1000]) + '</td>');
      prnt := prnt + prn * numpieces;
      prut := prut + pru * numpieces;
    end
    else
    begin
      document.write('<td width=12% align=right>-</td>');
      document.write('<td width=12% align=right>-</td>');
    end;
    mycost := orders.ItemCost(pcs, cl);
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

  document.write('<br></table></p></div></body>');
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
      if lst.Strings[i] = 'Part,Color,Desc' then
        continue;
    inc(aa);
    splitstring(lst.Strings[i], pcs, col, foo, ',');
    if Pos('BL ', pcs) = 1 then
      pcs := Trim(db.RebrickablePart(Copy(pcs, 4, Length(pcs) - 3)))
    else
      pcs := db.RebrickablePart(Trim(pcs));

    if Pos('BL', col) = 1 then
    begin
      col := Trim(Copy(col, 3, Length(col) - 2));
      cl := db.BrickLinkColorToRebrickableColor(StrToIntDef(col, 0))
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

    document.write('<td width=35%><img src=' + col + '\' + pcs + '.png>');
    document.write('<a href=spiece/' + pcs + '>' + pcs + '</a></b>');
    document.write(' - ' + db.PieceDesc(pcs) + '</td>');
    document.write('<td width=20%>');
    DrawColorCell(cl, 25);
    // document.BlancColorCell(db.colors(cl).RGB, 25);

    pci := db.PieceColorInfo(pcs, cl);
    pi := db.PieceInfo(pci);
    if pci = nil then
      document.write('<a href=spiecec/' + pcs + '/' + col + '>' + db.colors(cl).name + ' (' + col + ') (BL=' + IntToStr(db.colors(cl).BrickLingColor) + ')<img src="images\details.png"></a>' +
        HtmlDrawInvImgLink(pcs, cl, pi) + '</td>')
    else
      document.write('<a href=spiecec/' + pcs + '/' + col + '>' + db.colors(cl).name + ' (' + col + ') (BL=' + IntToStr(db.colors(cl).BrickLingColor) + ')<img src="images\details.png"></a>' +
        HtmlDrawInvImgLink(pcs, cl, pi) +
          decide(pci.setmost='','', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + '</td>');

    document.write('<td width=15% align=right>' +
            '<br><a href=editpiece/' + pcs + '/' + itoa(cl) + '><img src="images\edit.png"></a>' +
            '<br><a href=diagrampiece/' + pcs + '/' + itoa(cl) + '><img src="images\diagram.png"></a>' +
            '</td>');

    if pci <> nil then
    begin
      prn := pci.EvaluatePriceNew;
      pru := pci.EvaluatePriceUsed;
      document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Krg', [prn, dbl_safe_div(prn, pi.weight) * 1000]) + '</td>');
      document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Krg', [pru, dbl_safe_div(pru, pi.weight) * 1000]) + '</td>');
      prnt := prnt + prn * numpieces;
      prut := prut + pru * numpieces;

    end
    else
    begin
      document.write('<td width=15% align=right>-</td>');
      document.write('<td width=15% align=right>-</td>');
    end;
    mycost := orders.ItemCost(pcs, cl);
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
begin
  UpdateDismantaledsetsinv;
  ShowSplash;
  SplashProgress('Working...', 0);

  if domultipagedocuments then
    document.NewMultiPageDocument('DrawPieceListLugbulk', tit + itoa(lst.count));

  document.write('<body background="splash.jpg">');
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

    document.write('<td width=35%><img src=' + col + '\' + pcs + '.png>');
    document.write('<a href=spiece/' + pcs + '>' + pcs + '</a></b>');
    document.write(' - ' + db.PieceDesc(pcs) + '</td>');
    document.write('<td width=20%>');
    DrawColorCell(cl, 25);
    // document.BlancColorCell(db.colors(cl).RGB, 25);
    if pci = nil then
      document.write('<a href=spiecec/' + pcs + '/' + col + '>' + db.colors(cl).name + ' (' + col + ') (BL=' + IntToStr(db.colors(cl).BrickLingColor) + ')<img src="images\details.png"></a>' +
        HtmlDrawInvImgLink(pcs, cl, pi) +
          '</td>')
    else
      document.write('<a href=spiecec/' + pcs + '/' + col + '>' + db.colors(cl).name + ' (' + col + ') (BL=' + IntToStr(db.colors(cl).BrickLingColor) + ')<img src="images\details.png"></a>' +
        HtmlDrawInvImgLink(pcs, cl, pi) +
         decide(pci.setmost='','', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + '</td>');
    document.write('<td width=15% align=right>' + Format('%2.3f', [decided(pci = nil, 0.0, pci.nDemand)]) +
            '<br><a href=editpiece/' + pcs + '/' + itoa(cl) + '><img src="images\edit.png"></a>' +
            '<br><a href=diagrampiece/' + pcs + '/' + itoa(cl) + '><img src="images\diagram.png"></a>' +
            '</td>');

    if pci <> nil then
    begin
      prn := pci.EvaluatePriceNew;
      pru := pci.EvaluatePriceUsed;
      document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Krg', [prn, dbl_safe_div(prn, pi.weight) * 1000]) + '</td>');
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

  document.write('<br></table></p></div></body>');
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
begin
  Screen.Cursor := crHourglass;
  UpdateDismantaledsetsinv;

  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  pci := db.PieceColorInfo(pcs, color);
  pi := db.PieceInfo(pci);
  DrawHeadLine('Inventory for <a href=spiece/' + pcs + '>' + pcs + '</a> - ' + db.Colors(color).name + ' ' + db.PieceDesc(pcs) +
    ' <a href=editpiece/' + pcs + '/' + itoa(color) + '><img src="images\edit.png"></a>' +
    ' <a href=spiecec/' + pcs + '/' + itoa(color) + '><img src="images\details.png"></a>' +
    '<br><a href=diagrampiece/' + pcs + '/' + itoa(color) + '><img src="images\diagram.png"></a>' +
    '<br><br><img src=' + IntToStr(color) + '\' + pcs + '.png>');


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
          document.write('<td width=35%><a href=spiece/' + pcs + '><img src=' + IntToStr(i) + '\' + pcs + '.png></a></td>');
          document.write('<td width=20%>');
          DrawColorCell(i, 25);
          // document.BlancColorCell(db.colors(i).RGB, 25);
          if pci = nil then
            document.write(db.colors(i).name + ' (' + IntToStr(i) + ') (BL=' + IntToStr(db.colors(i).BrickLingColor) + ')</td>')
          else
            document.write(db.colors(i).name + ' (' + IntToStr(i) + ') (BL=' + IntToStr(db.colors(i).BrickLingColor) + ')' +
              decide(pci.setmost='','', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + '</td>');
          document.write('<td width=15% align=right>' + Format('%d', [numpieces]) + '</td>');

          if pci <> nil then
          begin
            prn := pci.EvaluatePriceNew;
            pru := pci.EvaluatePriceUsed;
            document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Krg', [prn, dbl_safe_div(prn, pi.weight) * 1000]) + '</td>');
            document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Krg', [pru, dbl_safe_div(pru, pi.weight) * 1000]) + '</td>');
          end
          else
          begin
            document.write('<td width=15% align=right>-</td>');
            document.write('<td width=15% align=right>-</td>');
          end;
          mycost := orders.ItemCost(pcs, i);
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
    DrawHeadLine('Can not find inventory for ' + db.Colors(color).name + ' ' + db.PieceDesc(pcs));
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

  inv.SortPieces;

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

procedure TMainForm.ShowColorPiece(const pcs: string; const color: integer; const ayear: integer = -1);
var
  i, j: integer;
  idx: Integer;
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
begin
  UpdateDismantaledsetsinv;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowColorPiece', pcs + '_' + itoa(color) + '_' + itoa(ayear));
  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  ylist := TDNumberList.Create;

  pci := db.PieceColorInfo(pcs, color);
  pi := db.PieceInfo(pci);
  ayearcount := 0;
  stmp := '';
  if pci <> nil then
  begin
    if ayear < 0 then
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
      stmp := '<br>Appears in ' + IntToStr(ayearcount) + ' set' + decide(ayearcount = 1, '', 's') + ' in year ' + IntToStr(ayear);
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

  DrawHeadLine('<a href=spiece/' + pcs + '>' + pcs + '</a> - ' + db.Colors(color).name + ' ' + db.PieceDesc(pcs) +
    ' <a href=editpiece/' + pcs + '/' + itoa(color) + '><img src="images\edit.png"></a>' +
    HtmlDrawInvImgLink(pcs, color, pi) +
    '<br><a href=diagrampiece/' + pcs + '/' + itoa(color) + '><img src="images\diagram.png"></a>' +
    '<br><br><img src=' + IntToStr(color) + '\' + pcs + '.png>' + stmp);
  if ytext <> '' then
    DrawHeadLine(ytext);

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
          document.write('<td width=35%><a href=spiece/' + pcs + '><img src=' + IntToStr(i) + '\' + pcs + '.png></a></td>');
          document.write('<td width=20%>');
          DrawColorCell(i, 25);
          // document.BlancColorCell(db.colors(i).RGB, 25);
          if pci = nil then
            document.write(db.colors(i).name + ' (' + IntToStr(i) + ') (BL=' + IntToStr(db.colors(i).BrickLingColor) + ')</td>')
          else
            document.write(db.colors(i).name + ' (' + IntToStr(i) + ') (BL=' + IntToStr(db.colors(i).BrickLingColor) + ')' +
              decide(pci.setmost='','', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + '</td>');
          document.write('<td width=15% align=right>' + Format('%d', [numpieces]) + '</td>');

          if pci <> nil then
          begin
            prn := pci.EvaluatePriceNew;
            pru := pci.EvaluatePriceUsed;
            document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Krg', [prn, dbl_safe_div(prn, pi.weight) * 1000]) + '</td>');
            document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Krg', [pru, dbl_safe_div(pru, pi.weight) * 1000]) + '</td>');
          end
          else
          begin
            document.write('<td width=15% align=right>-</td>');
            document.write('<td width=15% align=right>-</td>');
          end;
          mycost := orders.ItemCost(pcs, i);
          document.write('<td width=10% align=right>' + Format('€ %2.4f<br>€ %2.4f', [mycost, mycost * numpieces]) + '</td>');
          document.write('</tr>');

          bp.part := pcs;
          bp.color := i;
          bp.num := numpieces;

          DrawBrickOrderInfo(@bp);

          if pci <> nil then
            for j := 0 to pci.sets.Count - 1 do
              if (ayear < 0) or (db.SetYear(pci.sets.Strings[j]) = ayear) then
              begin
                inc(aa);
                document.StartItemId(aa);
                inv := db.GetSetInventory(pci.sets.Strings[j]);
                if db.SetYear(pci.sets.Strings[j]) > 1931 then
                  y := Format('[Year: <a href=ShowSetsAtYear/%d>%d</a>]', [db.SetYear(pci.sets.Strings[j]), db.SetYear(pci.sets.Strings[j])])
                else
                  y := '';
                DrawHeadLine(Format('%d. <a href="sinv/%s">%s - %s</a> %s<br>(appears %d times)<br><img width=240px src=s\' + pci.sets.Strings[j] + '.jpg>',
                  [aa, pci.sets.Strings[j], pci.sets.Strings[j], db.SetDesc(pci.sets.Strings[j]), y, inv.LoosePartCount(pcs, color)]));
              end;

        end;
      end;

  document.EndNavigateSection;

  document.write('</table>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write(BLColorPieceInfo(pcs, color));
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;

end;

procedure TMainForm.ShowSetsICanBuild(const pct: double);
type
  struct1 = record
    setid: string[15];
    set_tot_pieces: integer;
    set_have_pieces: integer;
    set_pct: double;
  end;
  struct1A = array[0..$1FFFF] of struct1;
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
        end;
      end;
    end;
  end;

  SplashProgress('Working...', 1);
  if domultipagedocuments then
    document.NewMultiPageDocument('ShowSetsICanBuild', ftoa(pct));

  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  links := '';
  if not dbl_equal(pct, 0.7) then
    links := links + '<a href=setsIcanbuild/7/10>Check 70%</a><br>';
  if not dbl_equal(pct, 0.8) then
    links := links + '<a href=setsIcanbuild/8/10>Check 80%</a><br>';
  if not dbl_equal(pct, 0.9) then
    links := links + '<a href=setsIcanbuild/9/10>Check 90%</a><br>';

  DrawHeadLine(Format('Sets that I can build<br> (Loose parts are at least %2.3f%s of set inventory)<br>', [100 * pct, '%']) + links);

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

  document.write('</p>');
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
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine('Set with unknown release year');

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

  document.write('</p>');
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
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

  document.write('</p>');
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;

  lsets.Free;
  Screen.Cursor := crDefault;

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

  document.write('</p>');
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

  document.write('</p>');
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

  document.write('</p>');
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

  document.write('</p>');
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

  document.write('</p>');
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

  FreeList(list);
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
    document.write('<tr bgcolor=' + TBGCOLOR + '>');
    document.write('<td width=5% align=right>' + IntToStr(aa) + '.</td><td width=35%><img src=' + scolor + '\' + spart + '.png><br><b>');
    document.write('<a href=spiece/' + spart + '>' + spart + '</a></b>');
    document.write(' - ' + db.PieceDesc(spart) + '</td><td width=20%>');
    DrawColorCell(ncolor, 25);
//    document.BlancColorCell(db.colors(ncolor).RGB, 25);
    pci := db.PieceColorInfo(spart, ncolor, report.Objects[i]);
    pi := db.PieceInfo(pci);
    document.write('<a href=spiecec/' + spart + '/' + scolor + '>' +  db.colors(ncolor).name + ' (' + scolor + ') (BL=' + IntToStr(db.colors(ncolor).BrickLingColor) + ')<img src="images\details.png"></a>' +
      HtmlDrawInvImgLink(spart, ncolor, pi));
    if pci <> nil then
      document.write((decide(pci.setmost='','', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + '</td>'))
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
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  FreeList(report);

  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowMissingToBuildSetInventory(const setid: string; const numsets: integer; legacyignore: boolean);
var
  inv: TBrickInventory;
  missing: integer;
  s1: string;
  nparts: integer;
  legstr: string;
begin
  Screen.Cursor := crHourGlass;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowMissingToBuildSetInventory', setid + '_' + itoa(numsets) + '_' + itoa(intval(legacyignore)));

  document.write('<body background="splash.jpg">');
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

  DrawHeadLine(Format('<a href="sinv/%s">%s - %s</a><br><br><img width=360px src=s\' + setid + '.jpg>', [setid, setid, db.SetDesc(setid)]));
  if numsets <= 1 then
    DrawHeadLine(Format('%d part' + decide(missing = 1, '', 's') + ' in %d lots ' +
      decide(missing = 1, 'is', 'are') + ' missing to build a copy of this set %s (%2.2f%s)',
        [missing, inv.numlooseparts, setid, dbl_safe_div(100 * missing, db.GetSetInventory(setid).totallooseparts), '%']))
  else
  begin
    nparts := db.GetSetInventory(setid).totallooseparts;
    if nparts > 0 then
      DrawHeadLine(Format('%d part' + decide(missing = 1, '', 's') + ' in %d lots ' +
        decide(missing = 1, 'is', 'are') + ' missing to build %d copies of this set %s (%2.2f%s)', [missing, inv.numlooseparts, numsets, setid, missing / nparts / numsets * 100, '%']));
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
  if reallots < sinv.numlooseparts then
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

  minv := inventory.InventoryForMissingToBuildInventory(inv);
  DrawHeadLine(Format('<a href=multymissing/' + IntToStr(Integer(setids)) +
    '>%d parts in %d lots are missing to build this list</a>', [minv.totallooseparts, minv.numlooseparts]));
  minv.Free;

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

procedure TMainForm.ShowCompare2Sets(const set1, set2: string);
var
  inv1, inv2: TBrickInventory;
  miss1, miss2: integer;
  missinv1, missinv2: TBrickInventory;
  b: Boolean;
begin
  Screen.Cursor := crHourGlass;

  document.write('<body background="splash.jpg">');
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
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);
  document.Flash;
  missinv1.Free;
  missinv2.Free;
  dodraworderinfo := b;
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
  inv.SortPieces;

  if domultipagedocuments then
    document.NewMultiPageDocument('ShowLooseParts' + itoa(Integer(inv)), itoa(colormask) + '_' + partmask + '_' + itoa(cat));

  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:#' + DFGCOLOR + '">');
  document.write('<p align=center>');
  if (colormask <> -1) and (cat <> -1) then
    DrawHeadLine('Loose Parts - ' + IntToStr(inv.numlotsbycolor(colormask)) + ' lots, ' + IntToStr(inv.totalloosepartsbycatcolor(colormask, cat)) + ' parts (filtered)')
  else if colormask <> -1 then
    DrawHeadLine('Loose Parts - ' + IntToStr(inv.numlotsbycolor(colormask)) + ' lots, ' + IntToStr(inv.totalloosepartsbycolor(colormask)) + ' parts (filtered)')
  else if partmask <> '' then
    DrawHeadLine('Loose Parts - ' + IntToStr(inv.numlotsbypart(partmask)) + ' lots, ' + IntToStr(inv.totalloosepartsbypart(partmask)) + ' parts (filtered)')
  else if cat <> -1 then
    DrawHeadLine('Loose Parts - ' + IntToStr(inv.numlotsbycategory(cat)) + ' lots, ' + IntToStr(inv.totalloosepartsbycategory(cat)) + ' parts (filtered)')
  else
  begin
    DrawHeadLine('Loose Parts - ' + IntToStr(inv.numlooseparts) + ' lots, ' + IntToStr(inv.totallooseparts) + ' parts');
    if looseparts then
      DrawHeadLine('Inventory Statistics<a href="diagramstorage/Loose Parts"><img src="images\diagram.png"></a>');

    DrawPartOutValue(inv);
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
      document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=35%><img src=' + scolor + '\' + brick.part + '.png><br>');
      document.write('<b><a href=spiece/' + brick.part + '>' + brick.part + '</a></b>');
      document.write('<a href="inv/' + invs +'/P/' + decide(partmask='',brick.part,'') + '">' + ' - ' );
      document.write(db.PieceDesc(brick.part) + '</a> <a href=spiece/' + brick.part + '>...</a></td><td width=25%>');
      DrawColorCell(brick.color, 20);
  //    document.BlancColorCell(db.colors(brick.color).RGB, 20);
      document.write('<a href="inv/' + invs +'/C/' + IntToStr(decide(colormask=-1, brick.color, -1)) + '">');

      document.write(db.colors(brick.color).name + ' (' + scolor + ') (BL=' +
                     IntToStr(db.colors(brick.color).BrickLingColor) +  ')</a> <a href=spiecec/' +
                     brick.part + '/' + scolor + '><img src="images\details.png"></a>' +  HtmlDrawInvImgLink(brick.part, brick.color, pi));

//      pci := db.PieceColorInfo(brick.part, brick.color);
      if pci = nil then
        document.write('</td>')
      else
        document.write(decide(pci.setmost='','', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + '</td>');

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
        document.write('<td width=10% align=right>' + Format('€ %2.4f<br>€ %2.4f<br>€ %2.2f / Krg', [prn, prn * brick.num, dbl_safe_div(prn, pi.weight) * 1000]) + '</td>');
        document.write('<td width=10% align=right>' + Format('€ %2.4f<br>€ %2.4f<br>€ %2.2f / Krg', [pru, pru * brick.num, dbl_safe_div(pru, pi.weight) * 1000]) + '</td>');
        prnt := prnt + prn * brick.num;
        prut := prut + pru * brick.num;
        if pi.weight > 0.0 then
        begin
          totalweight := totalweight + pi.weight * brick.num;
          totalcostwn := totalcostwn + prn * brick.num;
          totalcostwu := totalcostwu + pru * brick.num;
        end;
      end;

      mycost := orders.ItemCost(brick.part, brick.color);
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
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f<br>€ %2.2f / Krg', [prnt, dbl_safe_div(totalcostwn, totalweight) * 1000]) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f<br>€ %2.2f / Krg', [prut, dbl_safe_div(totalcostwu, totalweight) * 1000]) + '</b></td>');
  if num = 0 then
    document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [mycosttot]) + '</b></td>')
  else
    document.write('<td width=10% align=right><b>' + Format('€ %2.2f<br>%2.3f%s', [mycosttot, 100 * mytotcostpieces / num, '%']) + '</b></td>');
  document.write('</tr>');

  cl.Free;
  pl.Free;

  document.write('</tr></table></p>');
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

procedure TMainForm.HTMLHotSpotClick(Sender: TObject; const SRC: String;
  var Handled: Boolean);
begin
  HTMLClick(SRC, Handled);
end;

procedure TMainForm.HTMLClick(const SRC1: String; var Handled: Boolean);
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
  splitstring(SRC1, SRC, PARAMS, '&');

  diskmirror := '';

  if streams.Count > 500 then
  begin
    for i := 0 to streams.Count - 1 do
      streams.Objects[i].Free;
    streams.Free;
    streams := TStringList.Create;
  end;

  scrollx := 0;
  scrolly := 0;
  if SRC <> 'refresh' then
  begin
    if Pos('editpiece/', SRC) = 1 then
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

    if Pos('DoEditSet/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      DoEditSet(s2);
      Handled := True;
      Exit;
    end;

    if Pos('editmold/', SRC) = 1 then
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

    if Pos('removepiecefromstorage/', SRC) = 1 then
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

    if Pos('StoreInventoryStatsRec/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, s3, '/');
      StoreInventoryStatsRec(s2, s3);
      Handled := True;
      Exit;
    end;

    if Pos('refreshset/', SRC) = 1 then
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

    if Pos('refreshsetlite/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      if db.RefreshSet(s2, True) then
        HTMLClick('refresh', Handled);
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos('refreshpiece/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if db.RefreshPart(s2) then
        HTMLClick('refresh', Handled);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos('refreshpiececat/', SRC) = 1 then
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

    if Pos('refreshpiecefrombricklink/', SRC) = 1 then
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

    if Pos('refreshpiecefrombricklinkalias/', SRC) = 1 then
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

    if Pos('refreshpiecefrombricklinknorefresh/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if not db.UpdatePartKnownColorsFromBricklink(s2) then
        MessageBeep(MB_ICONERROR);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos('refreshpiecefrombricklinkaliasnorefresh/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, s3, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      db.SetNewPieceName(s2, s3);
      if not db.UpdatePartKnownColorsFromBricklink(s2) then
        MessageBeep(MB_ICONERROR);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos('UpdateCatalogFromBricklink/', SRC) = 1 then
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

    if Pos('UpdateCatalogFromBricklinknorefresh/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if not db.UpdateCatalogFromBricklink(s2) then
        MessageBeep(MB_ICONERROR);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos('updatepartnamerebrickable/', SRC) = 1 then
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

    if Pos('updatepartnamerebrickablenorefresh/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if not db.UpdateNameFromRebrickable(s2) then
        MessageBeep(MB_ICONERROR);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos('UpdateSetAsPartFromBricklink/', SRC) = 1 then
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

    if Pos('UpdateSetAsPartFromBricklinknorefresh/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      Screen.Cursor := crHourglass;
      ShowSplash;
      if not db.UpdateSetAsPartFromBricklink(s2) then
        MessageBeep(MB_ICONERROR);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := True;
      Exit;
    end;

    if Pos('refreshminifigcat/', SRC) = 1 then
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

    if Pos('refreshpiece100/', SRC) = 1 then
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

    if Pos('refreshpiece1000/', SRC) = 1 then
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

    if Pos('addset/', SRC) = 1 then
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

    if Pos('AddNewSetAsPiece/', SRC) = 1 then
    begin
      Screen.Cursor := crHourglass;
      splitstring(SRC, s1, s2, s3, '/');
      AddNewSetAsPiece(s2, s3);
      Handled := True;
      Screen.Cursor := crDefault;
      Exit;
    end;

    if Pos('DoAddNewSetAsPiece/', SRC) = 1 then
    begin
      Screen.Cursor := crHourglass;
      splitstring(SRC, s1, s2, s3, '/');
      DoAddNewSetAsPiece(s2, s3);
      Handled := True;
      Screen.Cursor := crDefault;
      Exit;
    end;

    if Pos('RefreshUnKnownSetsCategoryAndWeight/', SRC) = 1 then
    begin
      Screen.Cursor := crHourglass;
      splitstring(SRC, s1, s2, '/');
      RefreshUnKnownSetsCategoryAndWeight(atoi(s2));
      HTMLClick('refresh', Handled);
      Handled := True;
      Screen.Cursor := crDefault;
      Exit;
    end;

    if Pos('RefreshUnKnownPiecesCategory/', SRC) = 1 then
    begin
      Screen.Cursor := crHourglass;
      splitstring(SRC, s1, s2, '/');
      RefreshUnKnownPiecesCategory(atoi(s2));
      HTMLClick('refresh', Handled);
      Handled := True;
      Screen.Cursor := crDefault;
      Exit;
    end;

    if Pos('RefreshUnKnownPiecesWeight/', SRC) = 1 then
    begin
      Screen.Cursor := crHourglass;
      splitstring(SRC, s1, s2, '/');
      RefreshUnKnownPiecesWeight(atoi(s2));
      HTMLClick('refresh', Handled);
      Handled := True;
      Screen.Cursor := crDefault;
      Exit;
    end;

    if Pos('RefreshUnKnownMinifigCategory/', SRC) = 1 then
    begin
      Screen.Cursor := crHourglass;
      splitstring(SRC, s1, s2, '/');
      RefreshUnKnownMinifigCategory(atoi(s2));
      HTMLClick('refresh', Handled);
      Handled := True;
      Screen.Cursor := crDefault;
      Exit;
    end;

    if Pos('RefreshUnKnownMinifigWeight/', SRC) = 1 then
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

    if Pos('downloadset/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      if db.DownloadSetFromBricklinkNew(s2) then
        HTMLClick('refresh', Handled)
      else
        MessageBeep(MB_ICONERROR);
      Handled := True;
      Exit;
    end;

    if Pos('downloadsetnorefresh/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      if not db.DownloadSetFromBricklinkNew(s2) then
        MessageBeep(MB_ICONERROR);
      Handled := True;
      Exit;
    end;

    if Pos('downloadsetaltparts/', SRC) = 1 then
    begin
      Screen.Cursor := crHourglass;
      splitstring(SRC, s1, s2, '/');
      if db.DownloadSetAlternatesFromBricklinkNew(s2) then
        HTMLClick('refresh', Handled)
      else
        MessageBeep(MB_ICONERROR);
      Handled := True;
      Screen.Cursor := crDefault;
      Exit;
    end;

    if Pos('downloadsetex/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, s3, s4, '/');
      Screen.Cursor := crHourglass;
      if db.DownloadSetFromBricklinkNew(s2) then
      begin
        HTMLClick('refresh', Handled);
        db.UpdateSetInfo(s2, s3, atoi(s4, -1), False);
      end
      else
        MessageBeep(MB_ICONERROR);
      Handled := True;
      Screen.Cursor := crDefault;
      Exit;
    end;

    if Pos('downloadsetandrefresh/', SRC) = 1 then
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
        MessageBeep(MB_ICONERROR);
      Handled := True;
      Exit;
    end;

    if Pos('downloadsetandrefreshex/', SRC) = 1 then
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
        MessageBeep(MB_ICONERROR);
      Handled := True;
      Exit;
    end;

    if Pos('addsetdismantaled/', SRC) = 1 then
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

    if Pos('UpdatePartInventory/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      if db.UpdatePartInventory(s2, False) then
        HTMLClick('refresh', Handled);
      Handled := True;
      Exit;
    end;

    if Pos('UpdatePartInventorynorefresh/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      db.UpdatePartInventory(s2, False);
      Handled := True;
      Exit;
    end;

    if Pos('DownloadPartInventorynorefresh/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      db.UpdatePartInventory(s2, True);
      Handled := True;
      Exit;
    end;

    if Pos('removeset/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      inventory.RemoveSet(s2, False);
      btn_SaveClick(nil);
      HTMLClick('refresh', Handled);
      Handled := True;
      Exit;
    end;

    if Pos('UpdateSetAssetsFromBricklink/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      UpdateSetAssetsFromBricklink(s2);
      HTMLClick('refresh', Handled);
      Handled := True;
      Exit;
    end;

    if Pos('removesetdismantaled/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      inventory.RemoveSet(s2, True);
      btn_SaveClick(nil);
      HTMLClick('refresh', Handled);
      Handled := True;
      Exit;
    end;

    if Pos('diagrampiece/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, s3, '/');
      DiagramPiece(s2, atoi(s3));
      Handled := True;
      Exit;
    end;

    if Pos('diagramstorage/', SRC) = 1 then
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

    if Pos('diagramorder/', SRC) = 1 then
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
  begin
    if tmpfname[i] = '\' then
      tmpfname[i] := '_'
    else if tmpfname[i] = '/' then
      tmpfname[i] := '_'
  end;
  diskmirror := basedefault + 'out\html\' + tmpfname + '.htm';

  Handled := True;
  AddressEdit.Text := slink;
  if Pos('navigate/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, '/');
    Navigate(s2, atoi(s3));
  end
  else if Pos('inv/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, s4, '/');
    if s3 = 'C' then
      ShowLooseParts(TBrickInventory(StrToInt(s2)), StrToInt(s4))
    else if s3 = 'P' then
      ShowLooseParts(TBrickInventory(StrToInt(s2)), -1, s4)
    else if s3 = 'CAT' then
      ShowLooseParts(TBrickInventory(StrToInt(s2)), -1, '', StrToInt(s4));
  end
  else if Pos('invex/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, s4, '/');
    ShowLooseParts(TBrickInventory(StrToInt(s2)), StrToInt(s3), '', StrToInt(s4));
  end
  else if Pos('lugbulksuggest/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, s4, s5, '/');
    ShowLugbulkSuggestions(s2, atoi(s3), atoi(s4), atof(s5));
  end
  else if Pos('lugbulkbstprice/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, s4, '/');
    ShowLugbulkBestPrice(s2, atof(s3, -1.0), atoi(s4, -1));
  end
  else if Pos('lugbulkbstpricelt/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, s4, '/');
    ShowLugbulkBestPriceNoBrickOrder(s2, atof(s3, -1.0), atoi(s4, -1));
  end
  else if Pos('ShowSetsAtYear/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    ShowSetsAtYear(atoi(s2));
  end
  else if Pos('ShowSetsAtUnknownYear/', slink) = 1 then
  begin
    ShowSetsAtUnknownYear;
  end
  else if Pos('multymissing/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    ShowMissingToBuilMultipledSets(TStringList(StrToInt(s2)));
  end
  else if Pos('multysetinv/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    ShowInventoryForMultipledSets(TStringList(StrToInt(s2)));
  end
  else if slink = 'ShowStorageBins' then
  begin
    ShowStorageBins;
  end
  else if Pos('storage/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    ShowStorageInventory(s2);
  end
  else if Pos('sinv/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    ShowSetInventory(s2);
  end
  else if slink = 'catalogparts' then
  begin
    ShowCatalogList('P', -1, -1);
  end
  else if slink = 'catalogsets' then
  begin
    ShowCatalogList('S', -1, -1);
  end
  else if slink = 'cataloginstructions' then
  begin
    ShowCatalogList('I', -1, -1);
  end
  else if slink = 'catalogboxes' then
  begin
    ShowCatalogList('O', -1, -1);
  end
  else if slink = 'catalogmocs' then
  begin
    ShowCatalogList('MC', -1, -1);
  end
  else if slink = 'cataloggears' then
  begin
    ShowCatalogList('G', -1, -1);
  end
  else if slink = 'catalogcatalogs' then
  begin
    ShowCatalogList('C', -1, -1);
  end
  else if slink = 'catalogminifigures' then
  begin
    ShowCatalogList('M', -1, -1);
  end
  else if slink = 'catalogbooks' then
  begin
    ShowCatalogList('B', -1, -1);
  end
  else if Pos('ShowCatalogList/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, s4, '/');
    ShowCatalogList(s2, atoi(s3, -1), atoi(s4, -1));
  end
  else if Pos('PreviewSetInventory/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    PreviewSetInventory(s2);
  end
  else if Pos('PiecesDiscontinuedAtYear/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    PiecesDiscontinuedAtYear(atoi(s2));
  end
  else if Pos('MinifigurePiecesDiscontinuedAtYear/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    PiecesDiscontinuedAtYear_Minifigure(atoi(s2));
  end
  else if Pos('PiecesDiscontinuedAtYearExcludingVariations/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    PiecesDiscontinuedAtYearExcludingVariations(atoi(s2));
  end
  else if Pos('PiecesNewAtYear/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    PiecesNewAtYear(atoi(s2));
  end
  else if Pos('MinifigurePiecesNewAtYear/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    PiecesNewAtYear_Minifigure(atoi(s2));
  end
  else if Pos('PiecesNewAtYearExcludingVariations/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    PiecesNewAtYearExcludingVariations(atoi(s2));
  end
  else if Pos('ShowUniquePiecesOfMyInventory/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    ShowUniquePiecesOfMyInventory(atoi(s2));
  end
  else if Pos('ShowMoldsWithMoreThanColors/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    ShowMoldsWithMoreThanColors(atoi(s2));
  end
  else if slink = 'Nameswithbothpartandsetcolorindexes' then
  begin
    ShowNameswithbothpartandsetcolorindexes;
  end
  else if Pos('ShowExpensiveSetLotsNew/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowExpensiveSetLotsNew(s2, atoi(s3));
  end
  else if Pos('ShowExpensiveSetLotsUsed/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowExpensiveSetLotsUsed(s2, atoi(s3));
  end
  else if Pos('ShowExpensiveInvNew/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowExpensiveInvNew(s2, atoi(s3));
  end
  else if Pos('ShowExpensiveInvUsed/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowExpensiveInvUsed(s2, atoi(s3));
  end
  else if Pos('ShowExpensiveInvPartsNew/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowExpensiveInvPartsNew(s2, atoi(s3));
  end
  else if Pos('ShowExpensiveInvPartsUsed/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowExpensiveInvPartsUsed(s2, atoi(s3));
  end
  else if Pos('sinvl/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    ShowSetInventory(s2, True);
  end
  else if Pos('spiece/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowPiece(s2, atoi(s3, -1));
  end
  else if Pos('spiecec/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, s4, '/');
    ShowColorPiece(s2, atoi(s3, 0), atoi(s4, -1));
  end
  else if Pos('spiececinv/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowPieceCInventory(s2, atoi(s3, 0));
  end
  else if Pos('buildset/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    inventory.BuildSet(s2);
    btn_SaveClick(nil);
    ShowSetInventory(s2);
  end
  else if Pos('dismantleset/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    inventory.DismandalSet(s2);
    btn_SaveClick(nil);
    ShowSetInventory(s2);
  end
  else if Pos('missingtobuildset/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowMissingToBuildSetInventory(s2, StrToIntDef(s3, 1), False);
  end
  else if Pos('missingtobuildsetLI/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowMissingToBuildSetInventory(s2, StrToIntDef(s3, 1), True);
  end
  else if Pos('UsedPiecesbeloweuroKgr/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    UsedPiecesbeloweuroKgr(atoi(s2));
  end
  else if Pos('NewPiecesbeloweuroKgr/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    NewPiecesbeloweuroKgr(atoi(s2));
  end
  else if Pos('UsedPiecesaboveeuroKgr/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    UsedPiecesaboveeuroKgr(atoi(s2));
  end
  else if Pos('NewPiecesaboveeuroKgr/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    NewPiecesaboveeuroKgr(atoi(s2));
  end
  else if Pos('NewPiecesPriceAbove/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    NewPiecesPriceAbove(atof(s2));
  end
  else if Pos('UsedPiecesPriceAbove/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    UsedPiecesPriceAbove(atof(s2));
  end
  else if Pos('NewPiecesPriceAboveEvaluated/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    NewPiecesPriceAboveEvaluated(atof(s2));
  end
  else if Pos('UsedPiecesPriceAboveEvaluated/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    UsedPiecesPriceAboveEvaluated(atof(s2));
  end
  else if slink = 'NewPiecesCheaperUsed' then
  begin
    NewPiecesCheaperUsed;
  end
  else if Pos('NewPiecesMuchMoreExpensiveThanUsed/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    NewPiecesMuchMoreExpensiveThanUsed(atof(s2));
  end
  else if slink = 'ShowMySetsPieces' then
  begin
    ShowMySetsPieces;
  end
  else if slink = 'ShowMyMocsPieces' then
  begin
    ShowMyMocsPieces;
  end

  else if Pos('order/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    ShowOrder(s2);
  end
  else if Pos('catcolors/', slink) = 1 then
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
  else if slink = 'categories' then
  begin
    ShowCategories;
  end
  else if slink = 'mysets' then
  begin
    ShowMySets;
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
    ShowMySets;
  end
  else if slink = 'buildallsets' then
  begin
    Screen.Cursor := crHourGlass;
    inventory.BuildAllSets;
    Screen.Cursor := crDefault;
    ShowMySets;
  end
  else if slink = 'orders' then
  begin
    ShowOrders;
  end
  else if Pos('sellerorders/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    ShowOrders(s2);
  end
  else if Pos('compare2sets/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowCompare2Sets(s2, s3);
  end
  else if Pos('setsIcanbuild/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowSetsICanBuild(atoi(s2) / atoi(s3));
  end
  else if Pos('ShowSetsForPartOutNew/', slink) = 1 then
  begin
    splitstring(slink, stmp, s1, s2, s3, s4, '/');
    ShowSetsForPartOutNew(atoi(s1), atoi(s2), atoi(s3) / 100, atoi(s4) / 100);
  end
  else if Pos('ShowSetsForPartOutUsed/', slink) = 1 then
  begin
    splitstring(slink, stmp, s1, s2, s3, s4, '/');
    ShowSetsForPartOutUsed(atoi(s1), atoi(s2), atoi(s3) / 100, atoi(s4) / 100);
  end
  else if Pos('ShowSetsForPartInUsed/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    ShowSetsForPartInUsed(atoi(s2));
  end
  else if Pos('ShowSetsForPartOutWithMiniFigsNew/', slink) = 1 then
  begin
    splitstring(slink, stmp, s1, s2, s3, s4, s5, s6, s7, '/');
    ShowSetsForPartOutWithMiniFigsNew(atoi(s1), atoi(s2), atoi(s3), atoi(s4) / 100, atoi(s5) / 100, atoi(s6) / 100, atoi(s7) / 100);
  end
  else if Pos('ShowSetsForPartOutWithMiniFigsUsed/', slink) = 1 then
  begin
    splitstring(slink, stmp, s1, s2, s3, s4, s5, s6, s7, '/');
    ShowSetsForPartOutWithMiniFigsUsed(atoi(s1), atoi(s2), atoi(s3), atoi(s4) / 100, atoi(s5) / 100, atoi(s6) / 100, atoi(s7) / 100);
  end
  else if Pos('lengthquery/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    ShowLengthQuery(s2);
  end
  else if Pos('lengthqueryslopes/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    ShowLengthQuerySlopes(s2);
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
  if Compare2SetsQuery(set1, set2) then
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
  inventory.SaveLooseParts(basedefault + 'myparts.txt');
  inventory.SaveSets(basedefault + 'mysets.txt');
  db.SaveStorage;
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
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to LASTNORMALCOLORINDEX do
    if db.Colors(i).id = i then
      if db.Colors(i).knownpieces <> nil then
        for j := 0 to db.Colors(i).knownpieces.Count - 1 do
        begin
          pcs := db.Colors(i).knownpieces.Strings[j];
          pci := db.Colors(i).knownpieces.Objects[j] as TPieceColorInfo;
          pi := db.PieceInfo(pci);
          weight := pi.Weight;
          if weight > 0 then
          begin
            if pci <> nil then
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
  DrawPieceList(prevstr + 'Used pieces with price below ' + itoa(x) + ' euro/Kgr' + nextstr, lst, SORT_PRICE_USED);
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
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to LASTNORMALCOLORINDEX do
    if db.Colors(i).id = i then
      if db.Colors(i).knownpieces <> nil then
        for j := 0 to db.Colors(i).knownpieces.Count - 1 do
        begin
          pcs := db.Colors(i).knownpieces.Strings[j];
          pi := db.PieceInfo(db.Colors(i).knownpieces.Objects[j] as TPieceColorInfo);
          weight := pi.Weight;
          if weight > 0 then
          begin
            pci := db.Colors(i).knownpieces.Objects[j] as TPieceColorInfo;
            if pci <> nil then
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
  DrawPieceList(prevstr + 'New pieces with price below ' + itoa(x) + ' euro/Kgr' + nextstr, lst, SORT_PRICE_NEW);
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
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  inv := TBrickInventory.Create;
  for i := 0 to inventory.numsets - 1 do
    for j := 1 to inventory.sets[i].num do
      inv.AddSet(inventory.sets[i].setid, False);
  inv.DismandalAllSets;
  inv.SortPieces;

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
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  inv := TBrickInventory.Create;
  for i := 0 to inventory.numsets - 1 do
    if db.IsMoc(inventory.sets[i].setid) then
      for j := 1 to inventory.sets[i].num do
        inv.AddSet(inventory.sets[i].setid, False);
  inv.DismandalAllSets;
  inv.SortPieces;

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

procedure TMainForm.doShowLengthQueryColor(inv: TBrickInventory; const id: string; const color: integer; inflst: TStringList);
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
  stmp := db.colors(color).name + ' (' + itoa(color) + ') (BL=' + IntToStr(db.colors(color).BrickLingColor) + ')' +
    '<table border=1 width=' + IntToStr(25) + ' bgcolor="#' + IntToHex(db.colors(color).RGB, 6) + '"><tr><td><br></td></tr></table>';
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

    document.write('<td width=25%><img src=' + scolor + '\' + spart + '.png><br><b>');
    document.write('<a href=spiece/' + spart + '>' + spart + '</a></b>');
    document.write(' - ' + db.PieceDesc(spart));
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
  stmp := db.colors(color).name + ' (' + itoa(color) + ') (BL=' + IntToStr(db.colors(color).BrickLingColor) + ')' +
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

    document.write('<td width=25%><img src=' + scolor + '\' + spart + '.png><br><b>');
    document.write('<a href=spiece/' + spart + '>' + spart + '</a></b>');
    document.write(' - ' + db.PieceDesc(spart));
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
  if FileExists(basedefault + 'db\db_cross_stats.txt') then
    lenlst.LoadFromFile(basedefault + 'db\db_cross_stats.txt');

  UpdateDismantaledsetsinv;

  if inv = nil then
    inv := inventory;

  Screen.Cursor := crHourglass;

  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  DrawHeadLine(id);

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');

  for i := 0 to NUMCROSSCOLORS - 1 do
  begin
    if not odd(i) then
      document.write('<tr bgcolor=' + THBGCOLOR + '>');

    document.write('<td>');
    doShowLengthQueryColor(inv, id, CROSSCOLOR[i], lenlst);
    SplashProgress('Working...', (i + 1) / 32);
    document.write('</td>');

    if odd(i) then
      document.write('</tr>');
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
  if FileExists(basedefault + 'db\db_cross_stats.txt') then
    lenlst.LoadFromFile(basedefault + 'db\db_cross_stats.txt');

  UpdateDismantaledsetsinv;

  if inv = nil then
    inv := inventory;

  Screen.Cursor := crHourglass;

  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  DrawHeadLine(id);

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');

  for i := 0 to NUMCROSSCOLORS - 1 do
  begin
    if not odd(i) then
      document.write('<tr bgcolor=' + THBGCOLOR + '>');

    document.write('<td>');
    doShowLengthQueryColorSlopes(inv, id, CROSSCOLOR[i], lenlst);
    SplashProgress('Working...', (i + 1) / 32);
    document.write('</td>');

    if odd(i) then
      document.write('</tr>');
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

procedure TMainForm.Missingfromstoragebins1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('ShowMissingFromStorageBins', foo);
end;

procedure TMainForm.Priceguide1Click(Sender: TObject);
begin
  ShowSplash;
  progress_string := 'Saving Priceguide...';
  SplashProgress(progress_string, 0.05);
  db.ExportPriceGuide(basedefault + 'dbexport_priceguide.txt');
  HideSplash;
end;

procedure TMainForm.Partoutguide1Click(Sender: TObject);
begin
  ShowSplash;
  progress_string := 'Saving Part Out guide...';
  SplashProgress(progress_string, 0.05);
  db.ExportPartOutGuide(basedefault + 'dbexport_partout.txt');
  HideSplash;
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
      if copyfile(fname, fname2) then
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
    weight := pi.Weight;
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
        inv.SaveLooseParts(s2);
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
  if EditSetAsTextForm(setid, data, desc, year, ismoc) then
  begin
    Screen.Cursor := crHourglass;
    db.UpdateSet(setid, data);
    db.UpdateSetInfo(setid, desc, year, ismoc);
    HTMLClick('sinv/' + setid, foo);
    Screen.Cursor := crDefault;
  end;
  lst.Free;
end;

procedure TMainForm.TechnicBricks1x1Click(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('lengthquery/technic1x', foo);
end;

procedure TMainForm.DrawPieceListLugbulkKnownCost(const tit: string; const lb: TLugBulk2017;
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
begin
  UpdateDismantaledsetsinv;
  ShowSplash;
  SplashProgress('Working...', 0);

  lst := TStringList.Create;
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

  if domultipagedocuments then
    document.NewMultiPageDocument('DrawPieceListLugbulkKnownCost', tit + itoa(lb.List.Count) + itoa(lst.Count) + ftoa(over) + itoa(Ord(dobrickorederinfo)) + itoa(catid));

  document.write('<body background="splash.jpg">');
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

    document.write('<td width=35%><img src=' + col + '\' + pcs + '.png>');
    document.write('<a href=spiece/' + pcs + '>' + pcs + '</a></b>');
    document.write(' - ' + db.PieceDesc(pcs) + '</td>');
    document.write('<td width=20%>');
    DrawColorCell(cl, 25);
//    document.BlancColorCell(db.colors(cl).RGB, 25);
    if pci = nil then
      document.write('<a href=spiecec/' + pcs + '/' + col + '>' + db.colors(cl).name + ' (' + col + ') (BL=' + IntToStr(db.colors(cl).BrickLingColor) + ')<img src="images\details.png"></a>' + HtmlDrawInvImgLink(pcs, cl, pi) + '</td>')
    else
      document.write('<a href=spiecec/' + pcs + '/' + col + '>' + db.colors(cl).name + ' (' + col + ') (BL=' + IntToStr(db.colors(cl).BrickLingColor) + ')<img src="images\details.png"></a>' +
        HtmlDrawInvImgLink(pcs, cl, pi) +
         decide(pci.setmost='','', '<br><a href=sinv/' + pci.setmost +'>Appears ' + itoa(pci.setmostnum) + ' times in ' + pci.setmost + '</a>') + '</td>');
    document.write('<td width=15% align=right>' + Format('%2.3f', [decided(pci = nil, 0.0, pci.nDemand)]) +
            '<br><a href=editpiece/' + pcs + '/' + itoa(cl) + '><img src="images\edit.png"></a>' +
            '<br><a href=diagrampiece/' + pcs + '/' + itoa(cl) + '><img src="images\diagram.png"></a>' +
            '</td>');

    lprice := lb.ItemCost(pcs, cl);
    document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Krg', [lprice, dbl_safe_div(lprice, pi.weight) * 1000]) + '</td>');
    if pci <> nil then
    begin
      prn := pci.EvaluatePriceNew;
      pru := pci.EvaluatePriceUsed;
      document.write('<td width=15% align=right>' + Format('€ %2.4f<br>€ %2.2f / Krg', [prn, dbl_safe_div(prn, pi.weight) * 1000]) + '</td>');
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

    if dobrickorederinfo then
      DrawBrickOrderInfo(@bp);
    SplashProgress('Working...', i / lst.Count);
  end;

  document.EndNavigateSection;
  document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right><b>Total</b></td>');
  document.write('<td width=35%><b> </b></td>');
  document.write('<td width=20%><b> </b></td>');
  document.write('<td width=10% align=right></td>');
  document.write('<td width=10% align=right><b>' + IntToStr(totpieces) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [prnt]) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [prut]) + '</b></td>');
  document.write('<td width=10% align=right><b> </b></td>');

  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.SaveBufferToFile(diskmirror);

  SplashProgress('Working...', 1);

  document.Flash;

  HideSplash;

  lst.Free;
end;

procedure TMainForm.LugBulk20171Click(Sender: TObject);
var
  foo: boolean;
begin
  HTMLClick('lugbulkbstprice/2017/5.0', foo);
end;

procedure TMainForm.ShowLugbulkBestPrice(const year: string; const over: double; const catid: integer  = -1);
var
  lb: TLugBulk2017;
  slugbulkfile: string;       
begin
  if over < 0 then
    Exit;

  slugbulkfile := basedefault + 'lugbulks\' + year + '.txt';
  if not FileExists(slugbulkfile) then
  begin
    ShowMessage('File ' + slugbulkfile + #13#10 + 'not found!');
    Exit;
  end;

  lb := TLugBulk2017.Create;
  lb.LoadFromFile(basedefault + 'lugbulks\' + year + '.txt');

  DrawPieceListLugbulkKnownCost('Lugbulk suggestions ' + year, lb, over, True, catid);

  lb.Free;
end;

procedure TMainForm.ShowLugbulkBestPriceNoBrickOrder(const year: string; const over: double; const catid: integer  = -1);
var
  lb: TLugBulk2017;
  slugbulkfile: string;
begin
  if over < 0 then
    Exit;

  slugbulkfile := basedefault + 'lugbulks\' + year + '.txt';
  if not FileExists(slugbulkfile) then
  begin
    ShowMessage('File ' + slugbulkfile + #13#10 + 'not found!');
    Exit;
  end;

  lb := TLugBulk2017.Create;
  lb.LoadFromFile(basedefault + 'lugbulks\' + year + '.txt');

  DrawPieceListLugbulkKnownCost('Lugbulk suggestions ' + year, lb, over, False, catid);

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
  HTMLClick('ShowSetsAtUnknownYear/', foo);
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
begin
  rsTitle := 'Bricks Inventory';
  MessageBox(
    Handle,
    PChar(Format('%s'#13#10'Version %s'#13#10#13#10'A tool for managing your brick collection.'#13#10'© 2014 - 2018, jvalavanis@gmail.com', [rsTitle, I_VersionBuilt])),
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

procedure TMainForm.Queries1Click(Sender: TObject);
begin
  if not DirectoryExists(basedefault + 'lugbulks') then
    LugBulks1.Visible := False
  else
    LugBulks1.Visible := True;
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
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
      if db.Colors(i).knownpieces <> nil then
        for j := 0 to db.Colors(i).knownpieces.Count - 1 do
        begin
          pcs := db.Colors(i).knownpieces.Strings[j];
          pi := db.PieceInfo(db.Colors(i).knownpieces.Objects[j] as TPieceColorInfo);
          weight := pi.Weight;
          if weight > 0 then
          begin
            pci := db.Colors(i).knownpieces.Objects[j] as TPieceColorInfo;
            if pci <> nil then
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
  DrawPieceList(prevstr + 'Used pieces with price above ' + itoa(x) + ' euro/Kgr' + nextstr, lst, SORT_PRICE_USED);
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
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
      if db.Colors(i).knownpieces <> nil then
        for j := 0 to db.Colors(i).knownpieces.Count - 1 do
        begin
          pcs := db.Colors(i).knownpieces.Strings[j];
          pi := db.PieceInfo(db.Colors(i).knownpieces.Objects[j] as TPieceColorInfo);
          weight := pi.Weight;
          if weight > 0 then
          begin
            pci := db.Colors(i).knownpieces.Objects[j] as TPieceColorInfo;
            if pci <> nil then
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

  DrawPieceList(prevstr + 'New pieces with price above ' + itoa(x) + ' euro/Kgr' + nextstr, lst, SORT_PRICE_NEW);
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
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
      if db.Colors(i).knownpieces <> nil then
        for j := 0 to db.Colors(i).knownpieces.Count - 1 do
        begin
          pcs := db.Colors(i).knownpieces.Strings[j];
          pci := db.Colors(i).knownpieces.Objects[j] as TPieceColorInfo;
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

  lst.Sort;
  DrawPieceList('New pieces cheaper than Used', lst);
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
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
      if db.Colors(i).knownpieces <> nil then
        for j := 0 to db.Colors(i).knownpieces.Count - 1 do
        begin
          pcs := db.Colors(i).knownpieces.Strings[j];
          pci := db.Colors(i).knownpieces.Objects[j] as TPieceColorInfo;
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

  sprev := '<a href="NewPiecesPriceAbove/' + Format('%2.4f', [x / 2]) + '">New pieces with price greater than ' + Format('%2.4f', [x / 2]) + ' euro</a>';
  snext := '<a href="NewPiecesPriceAbove/' + Format('%2.4f', [x * 2]) + '">New pieces with price greater than ' + Format('%2.4f', [x * 2]) + ' euro</a>';

  lst.Sort;
  DrawPieceList(sprev + '<br>' + 'New pieces with price greater than ' + Format('%2.4f', [x]) + ' euro' + '<br>' + snext, lst, SORT_PRICE_NEW);
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
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
      if db.Colors(i).knownpieces <> nil then
        for j := 0 to db.Colors(i).knownpieces.Count - 1 do
        begin
          pcs := db.Colors(i).knownpieces.Strings[j];
          pci := db.Colors(i).knownpieces.Objects[j] as TPieceColorInfo;
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

  sprev := '<a href="UsedPiecesPriceAbove/' + Format('%2.4f', [x / 2]) + '">Used pieces with price greater than ' + Format('%2.4f', [x / 2]) + ' euro</a>';
  snext := '<a href="UsedPiecesPriceAbove/' + Format('%2.4f', [x * 2]) + '">Used pieces with price greater than ' + Format('%2.4f', [x * 2]) + ' euro</a>';

  lst.Sort;
  DrawPieceList(sprev + '<br>' + 'Used pieces with price greater than ' + Format('%2.4f', [x]) + ' euro' + '<br>' + snext, lst, SORT_PRICE_USED);
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
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
      if db.Colors(i).knownpieces <> nil then
        for j := 0 to db.Colors(i).knownpieces.Count - 1 do
        begin
          pcs := db.Colors(i).knownpieces.Strings[j];
          pci := db.Colors(i).knownpieces.Objects[j] as TPieceColorInfo;
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

  sprev := '<a href="NewPiecesPriceAboveEvaluated/' + Format('%2.4f', [x / 2]) + '">New pieces with evaluated price greater than ' + Format('%2.4f', [x / 2]) + ' euro</a>';
  snext := '<a href="NewPiecesPriceAboveEvaluated/' + Format('%2.4f', [x * 2]) + '">New pieces with evaluated price greater than ' + Format('%2.4f', [x * 2]) + ' euro</a>';

  lst.Sort;
  DrawPieceList(sprev + '<br>' + 'New pieces with evaluated price greater than ' + Format('%2.4f', [x]) + ' euro' + '<br>' + snext, lst, SORT_PRICE_NEW);
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
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
      if db.Colors(i).knownpieces <> nil then
        for j := 0 to db.Colors(i).knownpieces.Count - 1 do
        begin
          pcs := db.Colors(i).knownpieces.Strings[j];
          pci := db.Colors(i).knownpieces.Objects[j] as TPieceColorInfo;
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

  sprev := '<a href="UsedPiecesPriceAboveEvaluated/' + Format('%2.4f', [x / 2]) + '">Used pieces with evaluated price greater than ' + Format('%2.4f', [x / 2]) + ' euro</a>';
  snext := '<a href="UsedPiecesPriceAboveEvaluated/' + Format('%2.4f', [x * 2]) + '">Used pieces with evaluated price greater than ' + Format('%2.4f', [x * 2]) + ' euro</a>';

  lst.Sort;
  DrawPieceList(sprev + '<br>' + 'Used pieces with evaluated price greater than ' + Format('%2.4f', [x]) + ' euro' + '<br>' + snext, lst, SORT_PRICE_USED);
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
  for i := 0 to MAXINFOCOLOR do
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
        if db.Colors(i).knownpieces <> nil then
          for j := 0 to db.Colors(i).knownpieces.Count - 1 do
          begin
            pcs := db.Colors(i).knownpieces.Strings[j];
            pci := db.Colors(i).knownpieces.Objects[j] as TPieceColorInfo;
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

  if y > 1931 then
    prevstr := '<a href=PiecesDiscontinuedAtYear/' + itoa(y - 1) + '>Pieces discontinued at year ' + itoa(y - 1) + '</a><br>'
  else
    prevstr := '';
  if y < atoi(FormatDateTime('yyyy', Now)) then
    nextstr := '<br><a href=PiecesDiscontinuedAtYear/' + itoa(y + 1) + '>Pieces discontinued at year ' + itoa(y + 1) + '</a><br>'
  else
    nextstr := '';

  lst.Sort;
  DrawPieceList(prevstr + 'Pieces discontinued at year ' + itoa(y) + nextstr, lst);
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
        if db.Colors(i).knownpieces <> nil then
          for j := 0 to db.Colors(i).knownpieces.Count - 1 do
          begin
            pcs := db.Colors(i).knownpieces.Strings[j];
            pci := db.Colors(i).knownpieces.Objects[j] as TPieceColorInfo;
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

  if y > 1931 then
    prevstr := '<a href=PiecesNewAtYear/' + itoa(y - 1) + '>Pieces first appeared at year ' + itoa(y - 1) + '</a><br>'
  else
    prevstr := '';
  if y < atoi(FormatDateTime('yyyy', Now)) then
    nextstr := '<br><a href=PiecesNewAtYear/' + itoa(y + 1) + '>Pieces first appeared at year ' + itoa(y + 1) + '</a><br>'
  else
    nextstr := '';

  lst.Sort;
  DrawPieceList(prevstr + 'Pieces first appeared at year ' + itoa(y) + nextstr, lst);
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
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
      if db.Colors(i).knownpieces <> nil then
        for j := 0 to db.Colors(i).knownpieces.Count - 1 do
        begin
          pcs := db.Colors(i).knownpieces.Strings[j];
          pci := db.Colors(i).knownpieces.Objects[j] as TPieceColorInfo;
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

  if y > 1931 then
    prevstr := '<a href=PiecesDiscontinuedAtYearExcludingVariations/' + itoa(y - 1) + '>Pieces discontinued at year ' + itoa(y - 1) + ' (Excluding variations)</a><br>'
  else
    prevstr := '';
  if y < atoi(FormatDateTime('yyyy', Now)) then
    nextstr := '<br><a href=PiecesDiscontinuedAtYearExcludingVariations/' + itoa(y + 1) + '>Pieces discontinued at year ' + itoa(y + 1) + ' (Excluding variations)</a><br>'
  else
    nextstr := '';

  lst.Sort;
  DrawPieceList(prevstr + 'Pieces discontinued at year ' + itoa(y) + ' (Excluding variations)' + nextstr, lst);
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
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
      if db.Colors(i).knownpieces <> nil then
        for j := 0 to db.Colors(i).knownpieces.Count - 1 do
        begin
          pcs := db.Colors(i).knownpieces.Strings[j];
          pci := db.Colors(i).knownpieces.Objects[j] as TPieceColorInfo;
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

  if y > 1931 then
    prevstr := '<a href=PiecesNewAtYearExcludingVariations/' + itoa(y - 1) + '>Pieces first appeared at year ' + itoa(y - 1) + ' (Excluding variations)</a><br>'
  else
    prevstr := '';
  if y < atoi(FormatDateTime('yyyy', Now)) then
    nextstr := '<br><a href=PiecesNewAtYearExcludingVariations/' + itoa(y + 1) + '>Pieces first appeared at year ' + itoa(y + 1) + ' (Excluding variations)</a><br>'
  else
    nextstr := '';

  lst.Sort;
  DrawPieceList(prevstr + 'Pieces first appeared at year ' + itoa(y) + ' (Excluding variations)' + nextstr, lst);
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
    DrawPieceList('Pieces of my inventory that appear only in ' + itoa(ntimes) + ' official sets', lst);
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
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
      if db.Colors(i).knownpieces <> nil then
        for j := 0 to db.Colors(i).knownpieces.Count - 1 do
        begin
          pcs := db.Colors(i).knownpieces.Strings[j];
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
            pci := db.Colors(i).knownpieces.Objects[j] as TPieceColorInfo;
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
    prevstr := '<a href=MinifigurePiecesNewAtYear/' + itoa(y - 1) + '>Minifigure parts first appeared at year ' + itoa(y - 1) + '</a><br>'
  else
    prevstr := '';
  if y < atoi(FormatDateTime('yyyy', Now)) then
    nextstr := '<br><a href=MinifigurePiecesNewAtYear/' + itoa(y + 1) + '>Minifigure parts first appeared at year ' + itoa(y + 1) + '</a><br>'
  else
    nextstr := '';

  lst.Sort;
  DrawPieceList(prevstr + 'Minifigure parts first appeared at year ' + itoa(y) + nextstr, lst);
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
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
      if db.Colors(i).knownpieces <> nil then
        for j := 0 to db.Colors(i).knownpieces.Count - 1 do
        begin
          pcs := db.Colors(i).knownpieces.Strings[j];
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
            pci := db.Colors(i).knownpieces.Objects[j] as TPieceColorInfo;
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

  if y > 1931 then
    prevstr := '<a href=MinifigurePiecesDiscontinuedAtYear/' + itoa(y - 1) + '>Minifigure parts discontinued at year ' + itoa(y - 1) + '</a><br>'
  else
    prevstr := '';
  if y < atoi(FormatDateTime('yyyy', Now)) then
    nextstr := '<br><a href=MinifigurePiecesDiscontinuedAtYear/' + itoa(y + 1) + '>Minifigure parts discontinued at year ' + itoa(y + 1) + '</a><br>'
  else
    nextstr := '';

  lst.Sort;
  DrawPieceList(prevstr + 'Minifigure parts discontinued at year ' + itoa(y) + nextstr, lst);
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
  title: string;
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
      if db.Colors(i).knownpieces <> nil then
        for j := 0 to db.Colors(i).knownpieces.Count - 1 do
        begin
          pcs := db.Colors(i).knownpieces.Strings[j];
          pci := db.Colors(i).knownpieces.Objects[j] as TPieceColorInfo;
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

  ffmt := Format('%2.3f', [factor]);
  ffmt_prev := Format('%2.3f', [factor * 0.8]);
  ffmt_next := Format('%2.3f', [factor * 1.25]);
  title := '<a href=NewPiecesMuchMoreExpensiveThanUsed/' + ffmt_prev + '>New pieces ' + ffmt_prev + ' X more expensive than used</a><br>' +
           'New pieces ' + ffmt + ' X more expensive than used<br>' +
           '<a href=NewPiecesMuchMoreExpensiveThanUsed/' + ffmt_next + '>New pieces ' + ffmt_next + ' X more expensive than used</a>';

  lst.Sort;
  DrawPieceList(title, lst);
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
    start, stop, numcolors: integer;
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
    if db.GetMoldNumColors(db.AllPieces.Strings[i]) >= parms.numcolors then
      parms.list.Add(db.AllPieces.Strings[i]);
  Result := 1;
end;

procedure TMainForm.ShowMoldsWithMoreThanColors(const numcolors: integer);
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
  oldm: boolean;
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
      params1.numcolors := numcolors;
      params1.list := cmolds;

      params2.start := params1.stop + 1;
      params2.stop := params2.start + step;
      params2.numcolors := numcolors;
      params2.list := cmolds2;

      params3.start := params2.stop + 1;
      params3.stop := params3.start + step;
      params3.numcolors := numcolors;
      params3.list := cmolds3;

      params4.start := params3.stop + 1;
      params4.stop := cnt - 1;
      params4.numcolors := numcolors;
      params4.list := cmolds4;

      oldm := ismultithread;
      ismultithread := True;
      MT_Execute(
        @ShowMoldsWithMoreThanColors_thr, @params1,
        @ShowMoldsWithMoreThanColors_thr, @params2,
        @ShowMoldsWithMoreThanColors_thr, @params3,
        @ShowMoldsWithMoreThanColors_thr, @params4);
      ismultithread := oldm;

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
        if db.GetMoldNumColors(db.AllPieces.Strings[i]) >= numcolors then
          cmolds.Add(db.AllPieces.Strings[i]);
    end;

    tit := '';
    if numcolors > 20 then
      tit := tit + '<a href="ShowMoldsWithMoreThanColors/' + itoa(numcolors - 1) + '">Molds with more than ' + itoa(numcolors - 1) + ' known colors</a><br>';
    tit := tit + 'Molds with more than ' + itoa(numcolors) + ' known colors<br>';
    if numcolors < db.maximumcolors then
      tit := tit + '<a href="ShowMoldsWithMoreThanColors/' + itoa(numcolors + 1) + '">Molds with more than ' + itoa(numcolors + 1) + ' known colors</a><br>';

    cmolds.CustomSort(sortmoldlist_numcolors);
    DrawMoldList(tit, cmolds, False);

  finally
    cmolds.Free;
    Screen.Cursor := crDefault;
  end;
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
  tit: string;
  cnt: integer;
  step: integer;
  oldm: boolean;
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

      oldm := ismultithread;
      ismultithread := True;
      MT_Execute(
        @ShowNameswithbothpartandsetcolorindexes_thr, @params1,
        @ShowNameswithbothpartandsetcolorindexes_thr, @params2,
        @ShowNameswithbothpartandsetcolorindexes_thr, @params3,
        @ShowNameswithbothpartandsetcolorindexes_thr, @params4);
      ismultithread := oldm;

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

    cmolds.CustomSort(sortmoldlist_numcolors);
    DrawMoldList(tit, cmolds, True);

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
  ShowSplash;
  progress_string := 'Saving Database...';
  SplashProgress(progress_string, 0.05);
  db.ExportDatabase(basedefault + 'dbexport_database.txt');
  HideSplash;
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

function TMainForm.MakeThumbnailImageEx(const pcs: string; const typof: char; const ncolor: integer = -1000): string;
var
  s: string;
  c: string;
  fin, fout: string;
  i: integer;
begin
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

  c := UpperCase(s[1]);
  if not (c[1] in ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
                   'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
                   '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) then
    c := '9999';

  fout := 'th' + typof + '\' + c + '\' + s + '.png';
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

function TMainForm.MakeThumbnailImageExCache(const pcs: string; const typof: char; const ncolor: integer = -1000): string;
var
  s: string;
  c: string;
  fout: string;
  idx: integer;
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
  if not fexists(basedefault + fout) then
    if thumbnailcache[idx].IndexOf(StrUpper(fout)) < 0 then
      thumbnailcache[idx].AddObject(StrUpper(fout), TThumbnailCacheInfo.Create(pcs, typof, ncolor));

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
  adir: string;
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
      copyfile(basedefault + stmp, basedefault + Result);
end;

procedure TMainForm.btn_CatalogHomeClick(Sender: TObject);
var
  foo: Boolean;
begin
  HTMLClick('cataloghome', foo);
end;

end.

