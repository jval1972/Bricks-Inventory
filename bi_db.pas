//------------------------------------------------------------------------------
//
//  BrickInventory: A tool for managing your brick collection
//  Copyright (C) 2014-2019 by Jim Valavanis
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
//    Database and invenory objects
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_db;

interface

uses
  SysUtils, Classes, bi_hash, bi_hash512, bi_threads, bi_currency, bi_delphi,
  bi_binaryset, bi_binarypart;

type
  progressfunc_t = procedure (const s: string; a: double) of object;

  brickpool_t = record
    part: string[15];
    color: integer;
    num: integer;
    pci: TObject;
  end;

  brickpool_p = ^brickpool_t;
  brickpool_a = array[0..$FFFF] of brickpool_t;
  brickpool_pa = ^brickpool_a;

  sortbrickpool_t = record
    part: string[15];
    color: integer;
    num: integer;
    pci: TObject;
    {$IFNDEF CRAWLER}
    sortvalue: double;
    {$ENDIF}
  end;

  sortbrickpool_p = ^sortbrickpool_t;
  sortbrickpool_a = array[0..$FFFF] of sortbrickpool_t;
  sortbrickpool_pa = ^sortbrickpool_a;

  set_t = record
    setid: string[15];
    num: integer;
    dismantaled: integer;
  end;

  set_p = ^set_t;
  set_a = array[0..$FFFF] of set_t;
  set_pa = ^set_a;

type
  partout_t = record
    percentage: Double;
    value: Double;
  end;

const
  B_HASHSIZE = $FF;

type
  bhashitem_t = packed record
    position: smallint;
  end;
  bhashtable_t = array[0..B_HASHSIZE] of bhashitem_t;

const
  C_HASHSIZE = $FFFF;

const
  MAXCATEGORIES = 2000;
  CATEGORYLUGBULK = 1998;
  CATEGORYCUSTOMMINIFIGS = 1999;

type
  categorysum_t = array[-1..MAXCATEGORIES - 1] of integer;
  categorysum_p = ^categorysum_t;
  categorydouble_t = array[-1..MAXCATEGORIES - 1] of double;
  categorydouble_p = ^categorydouble_t;

type
  chashitem_t = record
    position: integer;
  end;
  chashtable_t = array[0..C_HASHSIZE] of chashitem_t;
  chashtable_p = ^chashtable_t;

type
  brickstatshistory_t = record
    time: TDateTime;
    Sold_nAvg: partout_t;
    Sold_nQtyAvg: partout_t;
    Sold_uAvg: partout_t;
    Sold_uQtyAvg: partout_t;
    Avail_nAvg: partout_t;
    Avail_nQtyAvg: partout_t;
    Avail_uAvg: partout_t;
    Avail_uQtyAvg: partout_t;
    nDemand: partout_t;
    uDemand: partout_t;
  end;

  brickevalhistory_t = record
    time: TDateTime;
    Eval_nAvg: partout_t;
    Eval_nQtyAvg: partout_t;
    Eval_uAvg: partout_t;
    Eval_uQtyAvg: partout_t;
  end;

type
  pieceinventoryhistory_t = record
    time: TDateTime;
    nnew: integer;
    nused: integer;
    nbuilded: integer;
    ndismantaled: integer;
  end;

type
  TBrickInventory = class(TObject)
  private
    flooseparts: brickpool_pa;
    bhash: bhashtable_t;
    chash: chashtable_p;
    fsets: set_pa;
    fneedsReorganize: boolean;
    fnumlooseparts: integer;
    fnumsets: integer;
    frealnumlooseparts: integer;
    frealnumsets: integer;
    fupdatetime: TDateTime;
    fupdatetimeout: double;
    fSoldPartOutValue_nAvg: partout_t;
    fSoldPartOutValue_nQtyAvg: partout_t;
    fSoldPartOutValue_uAvg: partout_t;
    fSoldPartOutValue_uQtyAvg: partout_t;
    fAvailablePartOutValue_nAvg: partout_t;
    fAvailablePartOutValue_nQtyAvg: partout_t;
    fAvailablePartOutValue_uAvg: partout_t;
    fAvailablePartOutValue_uQtyAvg: partout_t;
    fEvaluatedPartOutValue_nAvg: partout_t;
    fEvaluatedPartOutValue_nQtyAvg: partout_t;
    fEvaluatedPartOutValue_uAvg: partout_t;
    fEvaluatedPartOutValue_uQtyAvg: partout_t;
    procedure _growparts;
    procedure _growsets;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure CreateExtentedHashTable;
    function LoadLooseParts(const fname: string): boolean; overload;
    function LoadLooseParts(const S: TStringList): boolean; overload;
    function LoadSets(const fname: string): boolean;
    function LoadFromRebrickableFile(const fname: string): boolean;
    procedure SaveLooseParts(const fname: string);
    procedure SaveLoosePartsAndSets(const fname: string);
    procedure GetLoosePartsInStringList(const s: TStringList; const rbconvert: boolean);
    procedure GetSetsInStringList(const s: TStringList);
    function AsText: string;
    procedure SaveLoosePartsForRebrickable(const fname: string);
    procedure SaveLooseSetsForRebrickable(const fname: string);
    procedure SaveLoosePartsWantedListNew(const fname: string; const pricefactor: Double = 1.0; const wl: Integer = 0);
    procedure SaveLoosePartsWantedListUsed(const fname: string; const pricefactor: Double = 1.0; const wl: Integer = 0);
    procedure SavePartsInventoryPriceguide(const fname: string);
    procedure SaveSets(const fname: string);
    procedure AddLoosePart(const part: string; color: integer; const num: integer; const pci: TObject = nil);
    procedure AddLoosePartFast(const part: string; color: integer; const num: integer; const pci: TObject = nil);
    function RemoveLoosePart(const part: string; color: integer; num: integer): boolean;
    procedure RemoveAllSets;
    function LoosePartCount(const part: string; color: integer): integer;
    procedure AddSet(const setid: string; dismantaled: boolean);
    procedure GetSetInfo(const setid: string; const s: set_p);
    function RemoveSet(const setid: string; dismantaled: boolean): boolean;
    function DismandalSet(const setid: string): boolean;
    function DismandalAllSets: boolean;
    function BuildSet(const setid: string): boolean;
    function BuildAllSets: boolean;
    function CanBuildSet(const setid: string): boolean;
    procedure LegacyColorMerge;
    function MissingToBuildSet(const setid: string): integer;
    function MissingToBuildSetLegacyIgnore(const setid: string): integer;
    function Minifigures: TBrickInventory;
    function InventoryForMissingToBuildSet(const setid: string; const nsets: integer = 1): TBrickInventory;
    function InventoryForMissingToBuildSetLegacyIgnore(const setid: string; const nsets: integer = 1): TBrickInventory;
    {$IFNDEF CRAWLER}
    function InventoryForExpensiveLotsNew(const nlots: integer = 1): TBrickInventory;
    function InventoryForExpensiveLotsUsed(const nlots: integer = 1): TBrickInventory;
    function InventoryForExpensivePartsNew(const nlots: integer = 1): TBrickInventory;
    function InventoryForExpensivePartsUsed(const nlots: integer = 1): TBrickInventory;
    {$ENDIF}
    function InventoryForBigLots(const nitems: integer = 1): TBrickInventory;
    function CanBuildInventory(const inv: TBrickInventory): boolean;
    function CanBuildInventoryLegacyIgnore(const inv: TBrickInventory): boolean;
    function MissingToBuildInventory(const inv: TBrickInventory): integer;
    function MissingToBuildInventoryLegacyIgnore(const inv: TBrickInventory): integer;
    function InventoryForMissingToBuildInventory(const inv: TBrickInventory): TBrickInventory;
    function InventoryForMissingToBuildInventoryLegacyIgnore(const inv: TBrickInventory): TBrickInventory;
    function LoosePartsWeight: double;
    procedure UpdateCostValues;
    procedure DoUpdateCostValues;
    function SoldPartOutValue_nAvg: partout_t;
    function SoldPartOutValue_nQtyAvg: partout_t;
    function SoldPartOutValue_uAvg: partout_t;
    function SoldPartOutValue_uQtyAvg: partout_t;
    function AvailablePartOutValue_nAvg: partout_t;
    function AvailablePartOutValue_nQtyAvg: partout_t;
    function AvailablePartOutValue_uAvg: partout_t;
    function AvailablePartOutValue_uQtyAvg: partout_t;
    function EvaluatedPartOutValue_nAvg: partout_t;
    function EvaluatedPartOutValue_nQtyAvg: partout_t;
    function EvaluatedPartOutValue_uAvg: partout_t;
    function EvaluatedPartOutValue_uQtyAvg: partout_t;
    function nDemand: partout_t;
    function uDemand: partout_t;
    procedure Reorganize;
    procedure DoReorganize;
    procedure Clear;
    procedure MergeWith(const inv: TBrickInventory);
    function Clone: TBrickInventory;
    function numlotsbycolor(const col: integer): integer;
    function numlotsbycatcolor(const col: integer; const cat: integer): integer;
    function numlotsbypart(const pt: string): integer;
    function numlotsbycategory(const cat: integer): integer;
    function totalloosepartsbycolor(const col: integer): integer;
    function totalloosepartsbycatcolor(const col: integer; const cat: integer): integer;
    function totalloosepartsbypart(const pt: string): integer;
    function totalloosepartsbycategory(const cat: integer): integer;
    procedure partscategorysum(const c: categorysum_p);
    procedure lotscategorysum(const c: categorysum_p);
    function weightbycategory(const cat: integer): double;
    procedure weightcategorysum(const c: categorydouble_p);
    function weightbycolor(const col: integer): double;
    function weightbycatcolor(const col: integer; const cat: integer): double;
    function totallooseparts: integer;
    function totalsetsbuilted: integer;
    function totalsetsdismantaled: integer;
    function GetMoldList: TStringList;
    function GetDismandaledSets: TStringList;
    function GetHistoryStatsRec: brickstatshistory_t;
    function GetHistoryEvalRec: brickevalhistory_t;
    procedure StoreHistoryStatsRec(const fn: string; const pl: integer = 0);
    procedure StoreHistoryEvalRec(const fn: string; const pl: integer = 0);
    function GetPieceInventoryStatsRec(const piece: string; const color: integer): pieceinventoryhistory_t;
    procedure StorePieceInventoryStatsRec(const fn: string; const piece: string; const color: integer; const pl: integer = 0);
    {$IFNDEF CRAWLER}
    procedure SortPieces;
    procedure SortPiecesByPartNumber;
    procedure SortPiecesByPriceNew;
    procedure SortPiecesByPriceUsed;
    {$ENDIF}
    property numlooseparts: integer read fnumlooseparts;
    property looseparts: brickpool_pa read flooseparts;
    property numsets: integer read fnumsets;
    property sets: set_pa read fsets;
    property updatetimeout: double read fupdatetimeout write fupdatetimeout;
  end;

const
  MAXBRICKLINKCOLOR = 256;
  MAXREBRICKABLECOLOR = 2048;

type
  colorinfo_t = record
    id: integer;
    {$IFNDEF CRAWLER}
    name: string[32];
    RGB: LongWord;
    nParts: integer;
    nSets: integer;
    fYear: integer;
    yYear: integer;
    legoColor: string[24];
    ldrawColor: integer;
    {$ENDIF}
    BrickLingColor: integer;
    RebrickableColor: integer;
    {$IFNDEF CRAWLER}
    PeeronColor: string[24];
    alternateid: integer;
    {$ENDIF}
    knownpieces: THashStringList;
  end;
  colorinfo_p = ^colorinfo_t;

const
  LASTNORMALCOLORINDEX = 9000;
  CATALOGCOLORINDEX = 9996;
  INSTRUCTIONCOLORINDEX = 9997;
  BOXCOLORINDEX = 9998;
  MAXINFOCOLOR = 9999;

type
  colorinfoarray_t = array[-1..MAXINFOCOLOR] of colorinfo_t;
  colorinfoarray_p = ^colorinfoarray_t;

type
  categoryinfo_t = record
    name: string[48];
    knownpieces: THashStringList;
    {$IFNDEF CRAWLER}
    fetched: boolean;
    {$ENDIF}
  end;
  categoryinfo_p = ^categoryinfo_t;

type
  categoryinfoarray_t = array[0..MAXCATEGORIES - 1] of categoryinfo_t;
  categoryinfoarray_p = ^categoryinfoarray_t;

type
  TPieceInfo = class(TObject)
  private
    fname: string;
    flname: string;
    {$IFNDEF CRAWLER}
    fdesc: string;
    {$ENDIF}
    fcategory: integer;
    fweight: Double;
    {$IFNDEF CRAWLER}
    fmoldvariations: TStringList;
    falternates: TStringList;
    fpatternof: string;
    fprintof: string;
    fpatterns: TStringList;
    fprints: TStringList;
    {$ENDIF}
    {$IFNDEF CRAWLER}
    fdimentionx: Double;
    fdimentiony: Double;
    fdimentionz: Double;
    {$ENDIF}
    finventoryfound: boolean;
    fpartsinventoriesvalidcount: integer;
    finventoryname: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function hasinventory: boolean;
    function Inventory(const color: integer): TBrickInventory;
    function InventoryAsText(const color: integer): string;
    property name: string read fname write fname;
    property lname: string read flname write flname;
    {$IFNDEF CRAWLER}
    property desc: string read fdesc write fdesc;
    {$ENDIF}
    property category: integer read fcategory write fcategory;
    property weight: Double read fweight write fweight;
    {$IFNDEF CRAWLER}
    property moldvariations: TStringList read fmoldvariations write fmoldvariations;
    property alternates: TStringList read falternates write falternates;
    property patternof: string read fpatternof write fpatternof;
    property printof: string read fprintof write fprintof;
    property patterns: TStringList read fpatterns write fpatterns;
    property prints: TStringList read fprints write fprints;
    {$ENDIF}
    {$IFNDEF CRAWLER}
    property dimentionx: Double read fdimentionx write fdimentionx;
    property dimentiony: Double read fdimentiony write fdimentiony;
    property dimentionz: Double read fdimentionz write fdimentionz;
    {$ENDIF}
    property inventoryfound: boolean read finventoryfound write finventoryfound;
    property inventoryname: string read finventoryname write finventoryname;
  end;

type
  TSetExtraInfo = class(TObject)
  public
    moc: boolean;
    hasinstructions: boolean;
    hasoriginalbox: boolean;
    {$IFNDEF CRAWLER}
    text: string;
    year: integer;
    fixedinstructions: boolean;
    instructionsdimentionx: double;
    instructionsdimentiony: double;
    instructionsdimentionz: double;
    instructionsweight: double;
    fixedoriginalbox: boolean;
    originalboxinstructionx: double;
    originalboxinstructiony: double;
    originalboxinstructionz: double;
    originalboxweight: double;
    {$ENDIF}
    constructor Create; virtual;
  end;

const
  TYPE_PART = 0;
  TYPE_MINIFIGURE = 1;
  TYPE_SET = 2;
  TYPE_STICKER = 3;
  TYPE_BOOK = 4;
  TYPE_GEAR = 5;
  TYPE_INSTRUCTIONS = 6;
  TYPE_BOX = 7;
  TYPE_CATALOG = 8;

type
  priceguide_t = record
    nTimesSold: integer;
    nTotalQty: integer;
    nMinPrice: double;
    nAvgPrice: double;
    nQtyAvgPrice: double;
    nMaxPrice: double;
    uTimesSold: integer;
    uTotalQty: integer;
    uMinPrice: double;
    uAvgPrice: double;
    uQtyAvgPrice: double;
    uMaxPrice: double;
  end;
  priceguide_p = ^priceguide_t;

  availability_t = record
    nTotalLots: integer;
    nTotalQty: integer;
    nMinPrice: double;
    nAvgPrice: double;
    nQtyAvgPrice: double;
    nMaxPrice: double;
    uTotalLots: integer;
    uTotalQty: integer;
    uMinPrice: double;
    uAvgPrice: double;
    uQtyAvgPrice: double;
    uMaxPrice: double;
  end;
  availability_p = ^availability_t;

  parec_t = record
    priceguide: priceguide_t;
    availability: availability_t;
  end;

  parecdate_t = record
    priceguide: priceguide_t;
    availability: availability_t;
    date: TDateTime;
  end;

  parecdate_p = ^parecdate_t;

  parecdate2_t = record
    priceguide: priceguide_t;
    availability: availability_t;
    date: TDateTime;
    currency: string[3];
    url: string[127];
  end;

  parecdate2_p = ^parecdate2_t;

  parecdate3_t = record
    priceguide: priceguide_t;
    availability: availability_t;
    date: TDateTime;
    currency: string[3];
    url: string[127];
    vat: boolean;
  end;

  parecdate3_p = ^parecdate3_t;

  TPieceColorInfo = class(TObject)
  private
    fneedssave: boolean;
    fhasloaded: boolean;
    fhash: LongWord;
    fpriceguide: priceguide_t;
    favailability: availability_t;
    {$IFNDEF CRAWLER}
    fappearsinsets: integer;        // In how many sets appears?
    fappearsinsetstotal: integer;   // How many pieces in all set?
    fappearsinparts: integer;       // In how many parts appears?
    fappearsinpartstotal: integer;  // How many pieces in all parts?
    {$ENDIF}
    fpiece: string;
    fcolor: integer;
    fcolorstr: string;
    fsets: TStringList;
    fparts: TStringList;
    {$IFNDEF CRAWLER}
    fstorage: TStringList;
    {$ENDIF}
    fdate: TDateTime;
    fcrawlerlink: string;
    {$IFNDEF CRAWLER}
    fsetmost: string;
    fsetmostnum: integer;
    fpartmost: string;
    fpartmostnum: integer;
    {$ENDIF}
    fcacheread: boolean;
    fcode: string;
    {$IFNDEF CRAWLER}
    ffirstsetyear, flastsetyear, fyear: integer;
    fcanedityear: boolean;
    {$ENDIF}
    fparttype: integer;
    fsparttype: char;
    flastinternetupdate: double;
    fpieceinfo: TObject;
  protected
    {$IFNDEF CRAWLER}
    procedure SetYear(const y: integer);
    {$ENDIF}
    procedure SetSPartType(const t: char);
  public
    constructor Create(const apiece: string; const acolor: integer); virtual;
    destructor Destroy; override;
    procedure Assign(const pg: priceguide_t); overload;
    procedure Assign(const av: availability_t); overload;
    function Check: boolean;
    procedure AddSetReference(const aset: string; const numpieces: integer);
    procedure UpdateSetReference(const aset: string; const numpieces: integer);
    procedure AddPartReference(const apart: string; const numpieces: integer);
    procedure UpdatePartReference(const apart: string; const numpieces: integer);
    function LoadFromDisk: boolean;
    procedure Load;
    procedure SaveToDisk;
    procedure DoSaveToDisk;
    procedure InternetUpdate;
    function EvaluatePriceNew: double;
    function EvaluatePriceNewAvg: double;
    function EvaluatePriceUsed: double;
    function EvaluatePriceUsedAvg: double;
    function dbExportStringPG: string;
    {$IFNDEF CRAWLER}
    function dbExportStringDB: string;
    {$ENDIF}
    function nDemand: double;
    function uDemand: double;
    function ItemType: string;
    {$IFNDEF CRAWLER}
    procedure UpdateSetYears(const setid: string; const y: integer = -1);
    procedure UpdatePartYears(const y: integer = -1);
    {$ENDIF}
    {$IFNDEF CRAWLER}
    function PG_nTimesSoldAt(const at: TDateTime): integer;
    function PG_nTotalQtyAt(const at: TDateTime): integer;
    function PG_nMinPriceAt(const at: TDateTime): double;
    function PG_nAvgPriceAt(const at: TDateTime): double;
    function PG_nQtyAvgPriceAt(const at: TDateTime): double;
    function PG_nMaxPriceAt(const at: TDateTime): double;

    function PG_uTimesSoldAt(const at: TDateTime): integer;
    function PG_uTotalQtyAt(const at: TDateTime): integer;
    function PG_uMinPriceAt(const at: TDateTime): double;
    function PG_uAvgPriceAt(const at: TDateTime): double;
    function PG_uQtyAvgPriceAt(const at: TDateTime): double;
    function PG_uMaxPriceAt(const at: TDateTime): double;

    function AV_nTotalLotsAt(const at: TDateTime): integer;
    function AV_nTotalQtyAt(const at: TDateTime): integer;
    function AV_nMinPriceAt(const at: TDateTime): double;
    function AV_nAvgPriceAt(const at: TDateTime): double;
    function AV_nQtyAvgPriceAt(const at: TDateTime): double;
    function AV_nMaxPriceAt(const at: TDateTime): double;

    function AV_uTotalLotsAt(const at: TDateTime): integer;
    function AV_uTotalQtyAt(const at: TDateTime): integer;
    function AV_uMinPriceAt(const at: TDateTime): double;
    function AV_uAvgPriceAt(const at: TDateTime): double;
    function AV_uQtyAvgPriceAt(const at: TDateTime): double;
    function AV_uMaxPriceAt(const at: TDateTime): double;
    {$ENDIF}

    property priceguide: priceguide_t read fpriceguide;
    property availability: availability_t read favailability;
    {$IFNDEF CRAWLER}
    property appearsinsets: integer read fappearsinsets;
    property appearsinsetstotal: integer read fappearsinsetstotal;
    property appearsinparts: integer read fappearsinparts;
    property appearsinpartstotal: integer read fappearsinpartstotal;
    {$ENDIF}
    property piece: string read fpiece;
    property color: integer read fcolor;
    property hasloaded: boolean read fhasloaded;
    property sets: TStringList read fsets;
    property parts: TStringList read fparts;
    {$IFNDEF CRAWLER}
    property storage: TStringList read fstorage;
    {$ENDIF}
    property Hash: LongWord read fhash;
    {$IFNDEF CRAWLER}
    property setmost: string read fsetmost;
    property setmostnum: integer read fsetmostnum;
    property partmost: string read fpartmost;
    property partmostnum: integer read fpartmostnum;
    {$ENDIF}
    property cacheread: boolean read fcacheread;
    {$IFNDEF CRAWLER}
    property firstsetyear: integer read ffirstsetyear;
    property lastsetyear: integer read flastsetyear;
    property year: integer read fyear write SetYear;
    property canedityear: boolean read fcanedityear write fcanedityear;
    {$ENDIF}
    function invalid: boolean;
    property code: string read fcode write fcode;
    property parttype: integer read fparttype write fparttype;
    property sparttype: char read fsparttype write SetSPartType;
    property pieceinfo: TObject read fpieceinfo write fpieceinfo;
    property date: TDateTime read fdate;
  end;

const
  SPT_PART = 'Part';
  SPT_SET = 'Set';
  SPT_MINIFIG = 'Minifigure';
  SPT_CATALOG = 'Catalog';
  SPT_BOOK = 'Book';
  SPT_INSTRUCTIONS = 'Instructions';
  SPT_BOX = 'Original Box';
  SPT_GEAR = 'Gear';

function PartTypeToPartTypeName(const pt: char): string;

function PartTypeNameToPartType(const pn: string): char;

const
  CACHEDBHASHSIZE = $40000;
  CACHEDBSTRINGSIZE = 16;

type
  cachedbitem_t = record
    partid: array[0..CACHEDBSTRINGSIZE - 1] of char;
    color: integer;
    parec: parecdate_t;
  end;
  cachedbitem_p = ^cachedbitem_t;

  cachedbparec_t = array[0..CACHEDBHASHSIZE - 1] of cachedbitem_t;
  cachedbparec_p = ^cachedbparec_t;

type
  setasset_t = record
    year: integer;
    hasinstructions: boolean;
    hasoriginalbox: boolean;
  end;
  setasset_p = ^setasset_t;

const
  COLORFLAG_SET = 1;
  COLORFLAG_PART = 2;

type
  TSetsDatabase = class;

  TCacheDB = class(TObject)
  private
    fname: string;
    fstream: TFileStream;
    waitlist: TDNumberList;
    {$IFNDEF CRAWLER}
    st_pcihitcnt: int64;
    st_pcihitlevel: int64;
    {$ENDIF}
    function OpenDB1(const mode: char): TFileStream;
  protected
    parecs: cachedbparec_p;
    function TryOpenDB(const mode: char; const maxretry: integer = 100): TFileStream;
    function apart(const it: cachedbitem_p): string;
    procedure Flash; virtual;
    procedure FlashAll; virtual;
  public
    constructor Create(const aname: string); virtual;
    destructor Destroy; override;
    procedure OpenDB(const mode: char); virtual;
    procedure CloseDB; virtual;
    function LoadPCI(const p: TPieceColorInfo): boolean; virtual;
    function SavePCI(const p: TPieceColorInfo): boolean; virtual;
    procedure Reorganize;
    {$IFNDEF CRAWLER}
    function GetHitLevel: double;
    {$ENDIF}
  end;


  fpciloaderparams_t = record
    db: TSetsDatabase;
  end;
  fpciloaderparams_p = ^fpciloaderparams_t;

  TSetsDatabase = class(TObject)
  private
    floaded: boolean;
    fallsets: THashStringList;
    fallsetswithoutextra: THashStringList;
    fallbooks: THashStringList;
    fcolors: colorinfoarray_t;
    fpartsinventories: TStringList;
    fpartsinventoriesvalidcount: integer;
    fcategories: categoryinfoarray_t;
    fpieces: TStringList;
    fpieceshash: THashTable;
    ffixedblcolors: TStringList;
    fsets: TStringList;
    fsetshash: THashTable;
    fpiecenewnames: TStringList;
    fpiecesalias: THashStringList;
    fpiecesaliasBL: THashStringList;
    fpiecesaliasRB: THashStringList;
    fCrawlerLinks: TStringList;
    fcolorpieces: TStringList;
    fcrawlerpriority: TStringList;
    fcrawlerhistory: TStringList;
    flastcrawlpiece: string;
    {$IFNDEF CRAWLER}
    fstorage: TStringList;
    fbasemolds: TStringList;
    {$ENDIF}
    fstubpieceinfo: TPieceInfo;
    fcurrencies: TCurrency;
    fcurrencyconvert: TCurrencyConvert;
    fcrawlerfilename: string;
    fbricklinkcolortosystemcolor: array[0..MAXBRICKLINKCOLOR] of integer;
    frebrickablecolortosystemcolor: array[0..MAXREBRICKABLECOLOR] of integer;
    fCacheDB: TCacheDB;
    fbinarysets: TBinarySetCollection;
    fbinaryparts: TBinaryPartCollection;
    st_pciloads: integer;
    st_pciloadscache: integer;
    fpiececodes: TStringList;
    fCrawlerCache: TStringList;
    {$IFNDEF CRAWLER}
    fStorageBinsCache: TStringList;
    {$ENDIF}
    fmaximumcolors: integer;
    fPartTypeList: TStringList;
    fcrawlerrandom: byte;
//    fpciloader: TDThread;
//    fpciloaderparams: fpciloaderparams_t;
    procedure InitColors;
    procedure InitCategories;
    procedure InitWeightTable;
    {$IFNDEF CRAWLER}
    procedure InitYearTable;
    procedure InitBaseMolds;
    procedure InitRelationShips;
    {$ENDIF}
    procedure InitPieces;
    procedure InitPiecesAlias;
    procedure InitNewNames;
    procedure SaveNewNames;
    procedure InitCrawlerLinks;
    procedure InitSets;
    procedure InitBooks;
    procedure InitCatalogs;
    procedure InitGears;
    procedure InitPiecesInventories;
    procedure InitSetReferences;
    procedure InitPartReferences;
    procedure MarkInventoriedPart(const pcs: string); overload;
    procedure MarkInventoriedPart(const pi: TPieceInfo; const pcs: string); overload;
    procedure MarkUnInventoriedPart(const pcs: string); overload;
    procedure MarkUnInventoriedPart(const pi: TPieceInfo; const pcs: string); overload;
    procedure LoadPieceCodes;
    {$IFNDEF CRAWLER}
    procedure SavePieceCodes;
    {$ENDIF}
    function smallparsepartname(const p: string; const data: string): string;
    procedure TmpSaveCrawler;
    procedure RemoveDoublesFromList1(const lst: TStringList); overload;
    procedure RemoveDoublesFromList1(const fname1: string); overload;
    function RemoveDoublesFromList2(const lst: TStringList): boolean; overload;
    function RemoveDoublesFromList2(const fname1: string): boolean; overload;
    procedure FixKnownPieces;
  public
    progressfunc: progressfunc_t;
    constructor Create; virtual;
    procedure InitCreate(const app: string = ''); virtual;
    destructor Destroy; override;
    function LoadFromDisk(const fname: string): boolean;
    procedure AddSetPiece(const setid: string; const part: string; const typ: string;
      const color: integer; const num: integer; const pci: TObject = nil);
    procedure AddPieceAlias(const bl, rb: string);
    procedure AddCrawlerLink(const part: string; const color: integer; const link: string);
    function CrawlerLink(const part: string; const color: integer): string;
    function GetSetInventory(const setid: string): TBrickInventory;
    function GetSetInventoryWithOutExtra(const setid: string): TBrickInventory;
    procedure ReloadCache;
    function GetBLNetPieceName(const pcs: string): string;
    function GetNewPieceName(const pcs: string): string;
    procedure SetNewPieceName(const pcs: string; const newname: string);
    procedure UnSetNewPieceName(const pcs: string; const newname: string);
    {$IFNDEF CRAWLER}
    procedure SetPieceCode(const pci: TPieceColorInfo; const newcode: string);
    function GetUnknownPiecesFromCache: TStringList;
    function RemoveUnknownPiecesFromCache: integer;
    {$ENDIF}
    procedure GetCacheHashEfficiency(var hits, total: integer);
    function Colors(const i: Integer): colorinfo_p;
    {$IFNDEF CRAWLER}
    function PieceDesc(const s: string): string; overload;
    function PieceDesc(const pi: TPieceInfo): string; overload;
    {$ENDIF}
    function BrickLinkPart(const s: string): string;
    function BrickLinkColorToSystemColor(const c: integer): integer;
    function RebrickableColorToSystemColor(const c: integer): integer;
    function RebrickablePart(const s: string): string;
    {$IFNDEF CRAWLER}
    function SetDesc(const s: string): string;
    function SetYear(const s: string): integer;
    {$ENDIF}
    function IsMoc(const s: string): boolean;
    {$IFNDEF CRAWLER}
    function SetListAtYear(const y: integer): TStringList;
    {$ENDIF}
    function SetListWithInvLotsBetween(const a, b: integer): TStringList;
    function SetListWithInvPartsBetween(const a, b: integer): TStringList;
    function PieceListForSets(const slist: TStringList): TStringList;
    {$IFNDEF CRAWLER}
    function PieceListForYear(const y: integer): TStringList;
    {$ENDIF}
    function PieceInfo(const piece: string): TPieceInfo; overload;
    function PieceInfo(const pci: TPieceColorInfo): TPieceInfo; overload;
    function PieceInfo(const brick: brickpool_p): TPieceInfo; overload;
    function IsValidPieceInfo(const pi: TPieceInfo): boolean;
    function PieceColorInfo(const brick: brickpool_p): TPieceColorInfo; overload;
    function PieceColorInfo(const piece: string; const color: integer; const suspect: TObject = nil): TPieceColorInfo; overload;
    function Priceguide(const brick: brickpool_p): priceguide_t; overload;
    function Priceguide(const piece: string; const color: integer = -1): priceguide_t; overload;
    function Availability(const brick: brickpool_p): availability_t; overload;
    function Availability(const piece: string; const color: integer = -1): availability_t; overload;
    {$IFNDEF CRAWLER}
    function PArecAt(const piece: string; const color: integer; const at: TDateTime): parecdate_t;
    function GetPartLastUpdateDate(const piece: string; const color: integer): TDateTime;
    {$ENDIF}
    function ConvertCurrency(const cur: string): double;
    function ConvertCurrencyAt(const cur: string; const dd: TDateTime): double;
    function RecentCrawlerPart(const cpiece: string): boolean;
    procedure CrawlerPriorityPart(const piece: string; const color: integer = -1);
    procedure CrawlerPriorityPartColor(const cpiece: string);
    procedure ExportPriceGuide(const fname: string);
    procedure ExportPartOutGuide(const fname: string);
    {$IFNDEF CRAWLER}
    procedure ExportDatabase(const fname: string);
    procedure ExportPartColor(const fname: string);
    {$ENDIF}
    procedure Crawler(const rlevel: integer = 0);
    {$IFNDEF CRAWLER}
    procedure SaveStorage;
    procedure LoadStorage;
    function CheckStorageReport: TStringList;
    function StorageBins: TStringlist;
    function StorageBinsForMold(const mld: string): TStringlist;
    function StorageBinsForPart(const pcs: string; const color: integer): TStringlist;
    function InventoryForStorageBin(const st: string): TBrickInventory;
    function InventoryForStorageBinCache(const st: string): TBrickInventory;
    procedure FetchStorageBinsCache;
    function InventoryForAllStorageBins: TBrickInventory;
    procedure SetPieceStorage(const piece: string; const color: integer; const st: TStringList);
    {$ENDIF}
    {$IFNDEF CRAWLER}
    function BaseMold(const pcs: string): string;
    function ChildMolds(const pcs: string): TStringList;
    function FamilyMolds(const pcs: string): TStringList;
    {$ENDIF}
    {$IFNDEF CRAWLER}
    procedure RefreshAllSetsYears;
    {$ENDIF}
    procedure RefreshAllSetsAssets;
    {$IFNDEF CRAWLER}
    function GetItemWeight(const pcs: string; const color: integer; pi: TPieceInfo = nil): double;
    function GetAssetWeight(const aset: string; const aasset: char): double; overload;
    function GetAssetWeight(const aset: string; const aasset: integer): double; overload;
    procedure SetAssetWeight(const aset: string; const aasset: char; const ww: double); overload;
    procedure SetAssetWeight(const aset: string; const aasset: integer; const ww: double); overload;
    function UpdateAssetWeightFromNET(const aset: string; const casset: char): boolean; overload;
    function UpdateAssetWeightFromNET(const aset: string; const aasset: integer): boolean; overload;
    function GetAssetWeightFromNET(const aset: string; const casset: char): double; overload;
    function GetAssetWeightFromNET(const aset: string; const aasset: integer): double; overload;
    {$ENDIF}
    {$IFNDEF CRAWLER}
    procedure RefreshSetYears(const setid: string);
    function RefreshPart(const s: string): boolean;
    {$ENDIF}
    function RefreshInv(const inv: TBrickInventory): boolean;
    function RefreshSet(const s: string; const lite: boolean = False): boolean;
    function SetPartCategory(const s: string; const newcat: integer): boolean;
    function RefreshPartCategory(const s: string): boolean;
    procedure RefreshPartsCategory(const L: TStringList);
    function RefreshMinifigCategory(const s: string): boolean;
    function RefreshPartWeight(const s: string): boolean;
    function RefreshMinifigWeight(const s: string): boolean;
    function ParseKnownPiecesFromHTML(const shtml: string): TStringList;
    {$IFNDEF CRAWLER}
    procedure AddKnownPieces(const sl: TStringList);
    function AddKnownPiece(const spart: string; const color: integer; const desc: string): boolean; overload;
    function AddKnownPiece(const spart: string; const color: integer; const desc: string; var pci: TPieceColorInfo): boolean; overload;
    function AddKnownPiecePINV(const spart: string; const color: integer; const desc: string): boolean;
    {$ENDIF}
    procedure LoadKnownPieces;
    procedure LoadKnownPiece(const spart: string; const color: integer; const desc: string);
    {$IFNDEF CRAWLER}
    function UpdateNameFromRebrickable(const pid: string): boolean;
    function UpdatePartNameFromRebrickable(const pid: string): boolean;
    function UpdateSetNameFromRebrickable(const pid: string): boolean;
    function SetMoldName(const spart: string; const adesc: string): boolean;
    function AddMoldColor(const spart: string; const color: integer): boolean;
    function AddMoldSet(const spart: string): boolean;
    {$ENDIF}
    function GetMoldKnownColors(const spart: string): TDNumberList;
    function GetMoldNumColors(const spart: string): integer;
    function GetMoldNumNormalPartColors(const spart: string): integer;
    function HasSetColorsOnly(const spart: string): boolean;
    function HasMinifigColorsOnly(const spart: string): boolean;
    function GetMoldColorsFlags(const spart: string): integer;
    function MoldHasNoColors(const spart: string): boolean;
    procedure SetSetIsGear(const sid: string; const value: boolean);
    {$IFNDEF CRAWLER}
    procedure SetSetIsBook(const sid: string);
    {$ENDIF}
    function IsGear(const sid: string): boolean;
    function IsMinifigure(const mid: string): boolean;
    {$IFNDEF CRAWLER}
    function AutoFixSticker(const stk: string): boolean;
    function DownloadPartInventory(const s: string): boolean;
    function UpdatePartInventory(const s: string; const forcedownload: boolean): boolean;
    function SetPartInventory(const s: string; const stext: string): boolean; overload;
    function SetPartInventory(const s: string; const sout: TStringList): boolean; overload;
    function DownloadSetFromBricklinkNew(const s: string; const typ1: char = ' '): boolean;
    function DownloadSetAlternatesFromBricklinkNew(const s: string): boolean;
    function DownloadSetAssetsFromBricklink(const s: string; const typ: string; const def: integer): setasset_t;
    function UpdatePartKnownColorsFromBricklink(const pid: string; const donet: boolean = True): boolean;
    function UpdateGearKnownColorsFromBricklink(const pid: string; const donet: boolean = True): boolean;
    function UpdateBookKnownColorsFromBricklink(const pid: string; const donet: boolean = True): boolean;
    function UpdateItemYearFromDiskCache(const pid: string): boolean;
    function UpdatePartYearFromNet(const pid: string; const cl: integer): boolean; overload;
    function UpdatePartYearFromNet(const pci: TPieceColorInfo): boolean; overload;
    function GetPartYearFromNet(const pci: TPieceColorInfo; var yyyy: integer): boolean; overload;
    function GetPartYearFromNet(const pid: string; const cl: integer; var yyyy: integer): boolean; overload;
    function UpdateMoldYearsFromNet(const pid: string): boolean;
    function UpdateSetYearFromNet(const pid: string): boolean;
    function UpdateSetYearFromDiskCache(const pid: string; const fname: string): boolean;
    function UpdateMinifigYearFromDiskCache(const pid: string; const fname: string): boolean;
    function GetSetYearFromDiskCache(const fname: string): integer;
    function UpdateYearForAllColors(const pid: string; const yearnum: integer): boolean;
    procedure SaveItemYearInfo(const pid: string; const cl: integer; const yyyy: integer);
    procedure SetItemYear(const pci: TPieceColorInfo; const yyyy: integer);
    function UpdateCatalogFromBricklink(const pid: string): boolean;
    function UpdateSetAsPartFromBricklink(const pid: string; const donet: boolean = True): boolean;
    function UpdateMinifigAsPartFromBricklink(const pid: string; const donet: boolean = True): boolean;
    function QryNewInventoriesFromBricklink(const path: string; const check: string): TStringList;
    function QryNewSetAsPartFromBricklink(const path: string; const check: string): TStringList;
    function QryNewCatalogsFromBricklink(const path: string; const check: string): TStringList;
    function QryNewPartsFromBricklink(const path: string; const check: string; const savelink: string = ''): TStringList;
    function QryNewPartsFromFile(const fname: string; const check: string): TStringList;
    function QryPartsFromBricklink(const path: string; const check: string): TStringList;
    function UpdateSetAssetsFromBricklink(const s: string): boolean;
    function DownloadSetFromBricklink(const s: string; const typ: string = ''): boolean;
    function UpdateSet(const s: string; const data: string = ''): boolean;
    function UpdateSetInfo(const s: string; const desc: string; const year: integer; const ismoc: boolean): boolean;
    function UpdateSetInfoEx(const s: string; const desc: string; const year: integer; const ismoc: boolean; const hasI, hasB: boolean): boolean;
    {$ENDIF}
    function IsBook(const s: string): boolean;
    function GetPieceColorFromCode(const code: string; var spiece: string; var scolor: integer): boolean; overload;
    function GetPieceColorFromCode(const code: string; var spiece: string; var scolor: string): boolean; overload;
    function GetCodeFromPieceColor(const spiece: string; const scolor: integer): string;
    {$IFNDEF CRAWLER}
    function GetColorIdFromName(const cs: string; var cc: integer): boolean;
    {$ENDIF}
    function UpdatePartWeight(const pcs: string; const w: double): boolean; overload;
    function UpdatePartWeight(const pi: TPieceInfo; const w: double): boolean; overload;
    function GetPartWeight(const pcs: string): double;
    procedure SaveCrawlerData;
    function SearchGlobalPieceAlias(const pcs: string): TStringList;
    function PieceAlias(const pcs: string): string;
    procedure InitPartTypes;
    function SetPartType(const pci: TPieceColorInfo; const pt: char = ' '): boolean; overload;
    function SetPartType(const pcs: string; const cl: integer; const pt: char = ' '): boolean; overload;
    function SetPartTypeIndirect(const pcs: string; const cl: integer; const pt: char): boolean;
    procedure FlashPartTypes;
    property lastcrawlpiece: string read flastcrawlpiece;
    property loaded: boolean read floaded;
    property categories: categoryinfoarray_t read fcategories;
    property Sets: TStringList read fsets;
    property AllSets: THashStringList read fallsets;
    property AllSetsWithOutExtra: THashStringList read fallsetswithoutextra;
    property AllPieces: TStringList read fpieces;
    property crawlerfilename: string read fcrawlerfilename write fcrawlerfilename;
    property CacheDB: TCacheDB read fCacheDB;
    property pciloads: integer read st_pciloads;
    property pciloadscache: integer read st_pciloadscache;
    property binarysets: TBinarySetCollection read fbinarysets;
    property binaryparts: TBinaryPartCollection read fbinaryparts;
    property partsinventories: TStringList read fpartsinventories;
    property partsinventoriesvalidcount: integer read fpartsinventoriesvalidcount;
    {$IFNDEF CRAWLER}
    property maximumcolors: integer read fmaximumcolors;
    property basemolds: TStringList read fbasemolds;
    {$ENDIF}
    {$IFDEF CRAWLER}
    property crawlerpriority: TStringList read fcrawlerpriority;
    {$ENDIF}
    property crawlerrandom: byte read fcrawlerrandom write fcrawlerrandom;
  end;

function PieceColorCacheDir(const piece, color: string): string;
function PieceColorCacheFName(const piece, color: string): string;
function PieceColorCacheFName2(const piece, color: string): string;
function PieceColorCacheFName3(const piece, color: string): string;

var
  basedefault: string = '';
  sbricklink: string = 'https://www.bricklink.com/';

function F_nDemand(const favailability: availability_t; const fpriceguide: priceguide_t): double;
function F_uDemand(const favailability: availability_t; const fpriceguide: priceguide_t): double;

function fixpartname(const spart: string): string;

implementation

uses
  bi_system, bi_utils, bi_crawler, StrUtils, bi_priceadjust, bi_tmp, bi_globals,
  UrlMon, bi_multithread, bi_cachefile{$IFNDEF CRAWLER}, DateUtils, bi_pghistory{$ENDIF};

function fixpartname(const spart: string): string;
var
  i, k: integer;
begin
  Result := '';
  k := 1;

  if Length(spart) > 3 then
    if spart[3] = ' ' then
      if spart[1] in ['B', 'b'] then
        if spart[2] in ['L', 'l'] then
        begin
          Result := 'BL ';
          k := 4;
        end;

  for i := k to Length(spart) do
    if not (spart[i] in [' ', Chr(160), Chr($A0), #13, #10]) then
      Result := Result + spart[i];

  if Result = '6141' then
    Result := '4073'
  else if Pos('Mx', Result) = 1 then
    Result := LowerCase(Result)
  else if Result = '10830pat0001pr0001' then
    Result := '10830pat01pr01'
  else if Result = 'ClermontFerrand-1' then
    Result := 'Ferrand-1'
  else if Result = 'ISBN1338112120-1' then
    Result := 'ISBN1338112120'
  else if Pos('scalaupn00', Result) = 1 then
  begin
    if Result = 'scalaupn0024pr0001' then Result := 'scalaupn024pr01'
    else if Result = 'scalaupn0022pr0001' then Result := 'scalaupn022pr01'
    else if Result = 'scalaupn0027pr0001' then Result := 'scalaupn027pr01'
    else if Result = 'scalaupn0023pr0001' then Result := 'scalaupn023pr01'
    else if Result = 'scalaupn0024pr0001' then Result := 'scalaupn024pr01'
    else if Result = 'scalaupn0024pr0001' then Result := 'scalaupn024pr01';
  end
  else if Pos('dupupn00', Result) = 1 then
  begin
    if Result = 'dupupn0013c02pr0001a' then Result := 'dupupn13c02pr1a'
    else if Result = 'dupupn0049c01pr0001' then Result := 'dupupn49c01pr01'
    else if Result = 'dupupn0013c02pr0001' then Result := 'dupupn13c02pr01'
    else if Result = 'dupupn0049c07pr0002' then Result := 'dupupn49c07pr02'
    else if Result = 'dupupn0049c07pr0001' then Result := 'dupupn49c07pr01'
    else if Result = 'dupupn0049c03pr0001' then Result := 'dupupn49c03pr01'
    else if Result = 'dupupn0027c02pr0001' then Result := 'dupupn27c02pr01'
    else if Result = 'dupupn0049c01pr0002' then Result := 'dupupn49c01pr02'
    else if Result = 'dupupn0049c03pr0002' then Result := 'dupupn49c03pr02'
    else if Result = 'dupupn0028c02pr0001' then Result := 'dupupn28c02pr01'
    else if Result = 'dupupn0013c01pr0001' then Result := 'dupupn13c01pr01'
    else if Result = 'dupupn0013c03pr0001' then Result := 'dupupn13c03pr01'
    else if Result = 'dupupn0013c01pr0001' then Result := 'dupupn13c01pr01'
    else if Result = 'dupupn0013c02pr0001' then Result := 'dupupn13c02pr01'
    else if Result = 'dupupn0013c03pr0001' then Result := 'dupupn13c03pr01'
    else if Result = 'dupupn0049c02pr0001' then Result := 'dupupn49c02pr01'
    else if Result = 'dupupn0028c03pr0001' then Result := 'dupupn28c03pr01'
    else if Result = 'dupupn0049c02pr0002' then Result := 'dupupn49c02pr02'
    else if Result = 'dupupn0049c02pr0002' then Result := 'dupupn49c02pr02'
    else if Result = 'dupupn0027c01pr0001' then Result := 'dupupn27c01pr01'
    else if Result = 'dupupn0027c02pr0001' then Result := 'dupupn27c02pr01'
    else if Result = 'dupupn0049c02pr0001' then Result := 'dupupn49c02pr01'
    else if Result = 'dupupn0002pr0001' then Result := 'dupupn02pr01'
    else if Result = 'dupupn0017pr0003' then Result := 'dupupn17pr03'
    else if Result = 'dupupn0071pr0001' then Result := 'dupupn71pr01'
    else if Result = 'dupupn0019pr0001' then Result := 'dupupn19pr01'
    else if Result = 'dupupn0071pr0002' then Result := 'dupupn71pr02'
    else if Result = 'dupupn0063pr0001' then Result := 'dupupn63pr01'
    else if Result = 'dupupn0028pr0001' then Result := 'dupupn28pr01'
    else if Result = 'dupupn0017pr0001' then Result := 'dupupn17pr01'
    else if Result = 'dupupn0028pr0001' then Result := 'dupupn28pr01'
    else if Result = 'dupupn0028pr0001' then Result := 'dupupn28pr01'
    else if Result = 'dupupn0088pr0001' then Result := 'dupupn88pr01'
    else if Result = 'dupupn0021pr0001' then Result := 'dupupn21pr01'
    else if Result = 'dupupn0022pr0001' then Result := 'dupupn22pr01'
    else if Result = 'dupupn0008pr0002' then Result := 'dupupn08pr02'
    else if Result = 'dupupn0022pr0002' then Result := 'dupupn22pr02'
    else if Result = 'dupupn0016pr0002' then Result := 'dupupn16pr02'
    else if Result = 'dupupn0028pr0001' then Result := 'dupupn28pr01'
    else if Result = 'dupupn0022pr0003' then Result := 'dupupn22pr03'
    else if Result = 'dupupn0017pr0001' then Result := 'dupupn17pr01'
    else if Result = 'dupupn0008pr0007' then Result := 'dupupn08pr07'
    else if Result = 'dupupn0017pr0002' then Result := 'dupupn17pr02'
    else if Result = 'dupupn0018pr0001' then Result := 'dupupn18pr01'
    else if Result = 'dupupn0028pr0003' then Result := 'dupupn28pr03'
    else if Result = 'dupupn0086pr0001' then Result := 'dupupn86pr01'
    else if Result = 'dupupn0033pr0001' then Result := 'dupupn33pr01'
    else if Result = 'dupupn0073pr0001' then Result := 'dupupn73pr01'
    else if Result = 'dupupn0087pr0001' then Result := 'dupupn87pr01'
    else if Result = 'dupupn0087pr0002' then Result := 'dupupn87pr02'
    else if Result = 'dupupn0028pr0002' then Result := 'dupupn28pr02'
    else if Result = 'dupupn0008pr0001' then Result := 'dupupn08pr01'
    else if Result = 'dupupn0008pr0003' then Result := 'dupupn08pr03'
    else if Result = 'dupupn0016pr0002' then Result := 'dupupn16pr02'
    else if Result = 'dupupn0016pr0001' then Result := 'dupupn16pr01'
    else if Result = 'dupupn0008pr0006' then Result := 'dupupn08pr06'
    else if Result = 'dupupn0008pr0004' then Result := 'dupupn08pr04'
    else if Result = 'dupupn0008pr0004' then Result := 'dupupn08pr04'
    else if Result = 'dupupn0008pr0005' then Result := 'dupupn08pr05'
    else if Result = 'dupupn0008pr0007' then Result := 'dupupn08pr07';
  end;
end;

function fixdescname(const spart: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(spart) do
    if not (spart[i] in [Chr(160), Chr($A0), #13, #10]) then
      Result := Result + spart[i];
end;

{$IFNDEF CRAWLER}
procedure QSortBrickPool(const A: sortbrickpool_pa; const Len: integer);

  procedure QuickSortBP(iLo, iHi: Integer);
  var
    Lo, Hi: integer;
    Pivot: double;
    T: sortbrickpool_t;
  begin
    Lo := iLo;
    Hi := iHi;
    Pivot := A[(Lo + Hi) div 2].sortvalue;
    repeat
      while A[Lo].sortvalue < Pivot do Inc(Lo);
      while A[Hi].sortvalue > Pivot do Dec(Hi);
      if Lo <= Hi then
      begin
        T := A[Lo];
        A[Lo] := A[Hi];
        A[Hi] := T;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then QuickSortBP(iLo, Hi) ;
    if Lo < iHi then QuickSortBP(Lo, iHi) ;
  end;

begin
  if Len > 1 then
    QuickSortBP(0, Len - 1);
end;
{$ENDIF}

function MkBHash(const part: string; color: integer): LongWord;
var
  i: integer;
  check: string;
  b: byte;
begin
  check := strupper(part) + ',' + itoa(color);

  b := Ord(check[1]);

  Result := 5381 * 33 + b;

  for i := 2 to Length(check) do
  begin
    b := Ord(check[i]);
    Result := Result * 33 + b;
  end;

  Result := Result and B_HASHSIZE;
end;

function MkCHash(const part: string; color: integer): LongWord;
var
  i: integer;
  check: string;
  b: byte;
begin
  check := strupper(part) + ',' + itoa(color);

  b := Ord(check[1]);

  Result := 5381 * 33 + b;

  for i := 2 to Length(check) do
  begin
    b := Ord(check[i]);
    Result := Result * 33 + b;
  end;

  Result := Result and C_HASHSIZE;
end;

function MkPCIHash(const part: string; color: integer): LongWord;
var
  i: integer;
  check: string;
  b: byte;
begin
  check := strupper(part) + ',' + itoa(color);

  b := Ord(check[1]);

  Result := 5381 * 33 + b;

  for i := 2 to Length(check) do
  begin
    b := Ord(check[i]);
    Result := Result * 33 + b;
  end;

  Result := Result mod CACHEDBHASHSIZE;
end;

constructor TBrickInventory.Create;
begin
  inherited Create;

  fupdatetime := 0.0;
  fupdatetimeout := 1 / 48; // half an hour
  chash := nil;
  ZeroMemory(@bhash, SizeOf(bhashtable_t));
  ZeroMemory(@fSoldPartOutValue_nAvg, SizeOf(partout_t));
  ZeroMemory(@fSoldPartOutValue_nQtyAvg, SizeOf(partout_t));
  ZeroMemory(@fSoldPartOutValue_uAvg, SizeOf(partout_t));
  ZeroMemory(@fSoldPartOutValue_uQtyAvg, SizeOf(partout_t));
  ZeroMemory(@fAvailablePartOutValue_nAvg, SizeOf(partout_t));
  ZeroMemory(@fAvailablePartOutValue_nQtyAvg, SizeOf(partout_t));
  ZeroMemory(@fAvailablePartOutValue_uAvg, SizeOf(partout_t));
  ZeroMemory(@fAvailablePartOutValue_uQtyAvg, SizeOf(partout_t));
  ZeroMemory(@fEvaluatedPartOutValue_nAvg, SizeOf(partout_t));
  ZeroMemory(@fEvaluatedPartOutValue_nQtyAvg, SizeOf(partout_t));
  ZeroMemory(@fEvaluatedPartOutValue_uAvg, SizeOf(partout_t));
  ZeroMemory(@fEvaluatedPartOutValue_uQtyAvg, SizeOf(partout_t));

  fnumlooseparts := 0;
  fnumsets := 0;
  frealnumlooseparts := 0;
  frealnumsets := 0;
  flooseparts := nil;
  fsets := nil;
  fneedsReorganize := False;
end;

procedure TBrickInventory._growparts;
var
  delta: integer;
begin
  if fnumlooseparts >= frealnumlooseparts then
  begin
    if frealnumlooseparts < 32 then
      delta := 4
    else if frealnumlooseparts < 128 then
      delta := 8
    else
      delta := 16;
    frealnumlooseparts := frealnumlooseparts + delta;
    ReallocMem(flooseparts, frealnumlooseparts * SizeOf(brickpool_t));
  end;
end;

procedure TBrickInventory._growsets;
begin
  if fnumsets >= frealnumsets then
  begin
    frealnumsets := frealnumsets + 4;
    ReallocMem(fsets, frealnumsets * SizeOf(set_t));
  end;
end;

destructor TBrickInventory.Destroy;
begin
  Clear;
  if chash <> nil then
    FreeMem(chash, SizeOf(chashtable_t));
  Inherited;
end;

procedure TBrickInventory.RemoveAllSets;
begin
  fnumsets := 0;
  frealnumsets := 0;
  ReallocMem(fsets, 0);
  fsets := nil;
end;

procedure TBrickInventory.CreateExtentedHashTable;
var
  i: integer;
begin
  if chash = nil then
  begin
    GetMem(chash, SizeOf(chashtable_t));
    for i := 0 to C_HASHSIZE - 1 do
      chash[i].position := 0;
  end;
end;

function TBrickInventory.LoadFromRebrickableFile(const fname: string): boolean;
var
  s: TStringList;
  check: string;
  i: integer;
  spart, scolor, snum, scost: string;
begin
  Result := False;
  if not fexists(fname) then
    Exit;

  s := TStringList.Create;
  try
    S_LoadFromFile(s, fname);

    if s.Count > 0 then
    begin
      check := s.Strings[0];

      if (check = 'Set Number,Quantity') or (check = 'Set,Num,Dismantaled') then
        Result := LoadSets(fname)
      else if (check = 'Part,Color,Num') or (check = 'Part,Color,Quantity') then
      begin
        for i := 1 to s.Count - 1 do
        begin
          splitstring(s.strings[i], spart, scolor, snum, ',');
          if Pos('RB', scolor) < 1 then
            s.Strings[i] := spart + ',' + 'RB ' + scolor + ',' + snum;
        end;
        Result := LoadLooseParts(s);
      end
      else if check = 'Part,Color,Num,Cost' then
      begin
        for i := 1 to s.Count - 1 do
        begin
          splitstring(s.strings[i], spart, scolor, snum, scost, ',');
          if Pos('RB', scolor) < 1 then
            s.Strings[i] := spart + ',' + 'RB ' + scolor + ',' + snum + ',' + scost
        end;
        Result := LoadLooseParts(s);
      end
      else if check = 'Code,Num' then
      begin
        Result := LoadLooseParts(s);
      end;
    end;

  finally
    s.Free;
  end;
end;

function TBrickInventory.LoadLooseParts(const S: TStringList): boolean;
var
  i: integer;
  spart, scolor, snum, scost, scode: string;
  cc, np: integer;

  procedure _load_one_item;
  begin
    np := StrToIntDef(snum, 0);
    if Pos('BL ', spart) = 1 then
      spart := db.RebrickablePart(Copy(spart, 4, Length(spart) - 3))
    else
      spart := db.RebrickablePart(spart);
    if Pos('BL', scolor) = 1 then
    begin
      scolor := Trim(Copy(scolor, 3, Length(scolor) - 2));

      if np > 0 then
        AddLoosePartFast(spart, db.BrickLinkColorToSystemColor(StrToIntDef(scolor, 0)), np)
      else
        AddLoosePart(spart, db.BrickLinkColorToSystemColor(StrToIntDef(scolor, 0)), np);
    end
    else if Pos('RB', scolor) = 1 then
    begin
      scolor := Trim(Copy(scolor, 3, Length(scolor) - 2));

      if np > 0 then
        AddLoosePartFast(spart, db.RebrickableColorToSystemColor(StrToIntDef(scolor, 0)), np)
      else
        AddLoosePart(spart, db.RebrickableColorToSystemColor(StrToIntDef(scolor, 0)), np);
    end
    else
    begin
      if np > 0 then
        AddLoosePartFast(spart, StrToIntDef(scolor, 0), np)
      else
        AddLoosePart(spart, StrToIntDef(scolor, 0), np);
    end;
  end;

begin
  if s.Count = 0 then
  begin
    Result := False;
    Exit;
  end;

  if (s.Strings[0] = 'Part,Color,Num') or (s.Strings[0] = 'Part,Color,Quantity') then
  begin
    scost := '0';
    for i := 1 to s.Count - 1 do
    begin
      splitstring(s.Strings[i], spart, scolor, snum, ',');
      _load_one_item;
    end;
    Reorganize;
    Result := True;
  end
  else if s.Strings[0] = 'Part,Color,Num,Cost' then
  begin
    for i := 1 to s.Count - 1 do
    begin
      splitstring(s.Strings[i], spart, scolor, snum, scost, ',');
      _load_one_item;
    end;
    Reorganize;
    Result := True;
  end
  else if s.Strings[0] = 'Code,Num' then
  begin
    for i := 1 to s.Count - 1 do
    begin
      splitstring(s.Strings[i], scode, snum, ',');
      if db.GetPieceColorFromCode(scode, spart, cc) then
      begin
        scolor := itoa(cc);
        _load_one_item;
      end;
    end;
    Reorganize;
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

function TBrickInventory.LoadLooseParts(const fname: string): boolean;
var
  S: TStringList;
begin
  S := TStringList.Create;
  S_LoadFromFile(S, fname);
  if S.Count = 0 then
  begin
    S.Free;
    Result := False;
    Exit;
  end;

  Result := LoadLooseParts(S);
  S.Free;
end;

function TBrickInventory.LoadSets(const fname: string): boolean;
var
  s: TStringList;
  i, j: integer;
  sset, snum, sdismantaled: string;
begin
  Result := False;
  if not fexists(fname) then
    Exit;

  s := TStringList.Create;
  S_LoadFromFile(s, fname);
  if s.Count = 0 then
  begin
    s.Free;
    Exit;
  end;

  if s.Strings[0] = 'Set,Num,Dismantaled' then
  begin
    for i := 1 to s.Count - 1 do
    begin
      splitstring(s.Strings[i], sset, snum, sdismantaled, ',');
      for j := 0 to StrToIntDef(snum, 0) - 1 do
        AddSet(sset, False);
      for j := 0 to StrToIntDef(sdismantaled, 0) - 1 do
        AddSet(sset, True);
    end;
    Result := True;
  end
  else if s.Strings[0] = 'Set Number,Quantity' then
  begin
    for i := 1 to s.Count - 1 do
    begin
      splitstring(s.Strings[i], sset, snum, ',');
      for j := 0 to StrToIntDef(snum, 0) - 1 do
        AddSet(sset, False);
    end;
    Result := True;
  end;

  s.Free;
end;

procedure TBrickInventory.SaveLooseParts(const fname: string);
var
  s: TStringList;
  i: integer;
begin
  s := TStringList.Create;
  s.Add('Part,Color,Num');
  for i := 0 to fnumlooseparts - 1 do
    s.Add(Format('%s,%d,%d', [flooseparts[i].part, flooseparts[i].color, flooseparts[i].num]));
  try
    S_BackupFile(fname);
    S_SaveToFile(s, fname);
  except
    I_Warning('TBrickInventory.SaveLooseParts(): Can not save file %s'#13#10, [fname]);
  end;
  s.Free;
end;

procedure TBrickInventory.SaveLoosePartsAndSets(const fname: string);
var
  s: TStringList;
  i: integer;
begin
  s := TStringList.Create;
  s.Add('Part,Color,Num');
  for i := 0 to fnumlooseparts - 1 do
    s.Add(Format('%s,%d,%d', [flooseparts[i].part, flooseparts[i].color, flooseparts[i].num]));
  for i := 0 to fnumsets - 1 do
    if fsets[i].num > 0 then
      s.Add(Format('%s,%d,%d', [fsets[i].setid, -1, fsets[i].num]));
  try
    S_BackupFile(fname);
    S_SaveToFile(s, fname);
  except
    I_Warning('TBrickInventory.SaveLooseParts(): Can not save file %s'#13#10, [fname]);
  end;
  s.Free;
end;

procedure TBrickInventory.GetLoosePartsInStringList(const s: TStringList; const rbconvert: boolean);
var
  i, j: integer;
  bp: brickpool_t;
  sinv: TBrickInventory;
begin
  if rbconvert then
  begin
    for i := 0 to fnumlooseparts - 1 do
    begin
      bp := flooseparts[i];
      sinv := nil;
      if (bp.color = -1) or (bp.color = 9999) or (bp.color = 89) then
      begin
        sinv := db.GetSetInventory(bp.part);
        if sinv = nil then
          if bp.pci <> nil then
            if (bp.pci as TPieceColorInfo).pieceinfo as TPieceInfo <> nil then
              if ((bp.pci as TPieceColorInfo).pieceinfo as TPieceInfo).category = CATEGORYCUSTOMMINIFIGS then
                Continue;
      end
      else
        bp.color := db.colors(bp.color).RebrickableColor;
      if sinv <> nil then
      begin
        for j := 0 to bp.num - 1 do
          sinv.GetLoosePartsInStringList(s, rbconvert);
      end
      else if bp.part = '30390' then
      begin
        s.Add(Format('%s,%d,%d', ['30390b', bp.color, bp.num]));
      end
      else if bp.part = '4460' then
      begin
        s.Add(Format('%s,%d,%d', ['4460b', bp.color, bp.num]));
      end
      else if bp.part = '3070bpr0002' then
      begin
        s.Add(Format('%s,%d,%d', ['3070bp02', bp.color, bp.num]));
      end
      else if bp.part = '4476' then
      begin
        s.Add(Format('%s,%d,%d', ['4476b', bp.color, bp.num]));
      end
      else if bp.part = '7026ac01' then
      begin
        s.Add(Format('%s,%d,%d', ['3081ac01', bp.color, bp.num]));
      end
      else if bp.part = '14518c01pb01' then
      begin
        s.Add(Format('%s,%d,%d', ['14518', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['87587pr0001', bp.color, bp.num]));
      end
      else if bp.part = '15068pb046' then
      begin
        s.Add(Format('%s,%d,%d', ['15068pr0015', bp.color, bp.num]));
      end
      else if bp.part = '44658c01' then
      begin
        s.Add(Format('%s,%d,%d', ['37', bp.color, 2 * bp.num]));
      end
      else if bp.part = '4465c02' then
      begin
        s.Add(Format('%s,%d,%d', ['4465body', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['30456', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['30460', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['30462', 484, bp.num]));
        s.Add(Format('%s,%d,%d', ['87695', 15, bp.num]));
      end
      else if bp.part = '4624c05' then
      begin
        s.Add(Format('%s,%d,%d', ['4624', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['59895', 0, bp.num]));
      end
      else if bp.part = '6246c01' then
      begin
        s.Add(Format('%s,%d,%d', ['55295', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['55296', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['55297', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['55298', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['55299', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['55300', bp.color, bp.num]));
      end
      else if bp.part = '93594c01' then
      begin
        s.Add(Format('%s,%d,%d', ['93594', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['50951', 0, bp.num]));
      end
      else if (bp.part = '970c00pr0400') and (bp.color = 379) then
      begin
        s.Add(Format('%s,%d,%d', ['970c23pr0400', 326, bp.num]));
      end
      else if (bp.part = '970c00pr0476') and (bp.color = 71) then
      begin
        s.Add(Format('%s,%d,%d', ['970x194pr0476', 25, bp.num]));
      end
      else if bp.part = '2599' then
      begin
        s.Add(Format('%s,%d,%d', ['59275', bp.color, bp.num]));
      end
      else if bp.part = '15625pb018' then
      begin
        s.Add(Format('%s,%d,%d', ['15625pr0008', bp.color, bp.num]));
      end
      else if bp.part = '15625pb019' then
      begin
        s.Add(Format('%s,%d,%d', ['15625pr0013', bp.color, bp.num]));
      end
      else if bp.part = '15714' then
      begin
        s.Add(Format('%s,%d,%d', ['4865b', bp.color, bp.num]));
      end
      else if bp.part = '18052pb002' then
      begin
        s.Add(Format('%s,%d,%d', ['18052pr0002', bp.color, bp.num]));
      end
      else if bp.part = '2431pb415' then
      begin
        s.Add(Format('%s,%d,%d', ['2431pr0081', bp.color, bp.num]));
      end
      else if bp.part = '2431pb477' then
      begin
        s.Add(Format('%s,%d,%d', ['2431pr0099', bp.color, bp.num]));
      end
      else if bp.part = '2431pb478' then
      begin
        s.Add(Format('%s,%d,%d', ['2431pr0098', bp.color, bp.num]));
      end
      else if bp.part = '2431pb484' then
      begin
        s.Add(Format('%s,%d,%d', ['2431pr0092', bp.color, bp.num]));
      end
      else if bp.part = '2436a' then
      begin
        s.Add(Format('%s,%d,%d', ['2436', bp.color, bp.num]));
      end
      else if bp.part = '2546p02' then
      begin
        s.Add(Format('%s,%d,%d', ['2546pat0001', bp.color, bp.num]));
      end
      else if bp.part = '26603pb004' then
      begin
        s.Add(Format('%s,%d,%d', ['26603pr0003', bp.color, bp.num]));
      end
      else if bp.part = '3004pb159' then
      begin
        s.Add(Format('%s,%d,%d', ['3004pr0024', bp.color, bp.num]));
      end
      else if bp.part = '3010pb009' then
      begin
        s.Add(Format('%s,%d,%d', ['3010p70', bp.color, bp.num]));
      end
      else if bp.part = '30261pb034' then
      begin
        s.Add(Format('%s,%d,%d', ['30261pr0002', bp.color, bp.num]));
      end
      else if bp.part = '30387p02' then
      begin
        s.Add(Format('%s,%d,%d', ['30387pr0002', bp.color, bp.num]));
      end
      else if bp.part = '3068bpb0381' then
      begin
        s.Add(Format('%s,%d,%d', ['3068bpr0137', bp.color, bp.num]));
      end
      else if bp.part = '3068bpb0408' then
      begin
        s.Add(Format('%s,%d,%d', ['3068bpr0197', bp.color, bp.num]));
      end
      else if bp.part = '3068bpb0819' then
      begin
        s.Add(Format('%s,%d,%d', ['3068bpr0225', bp.color, bp.num]));
      end
      else if bp.part = '3068bpb0933' then
      begin
        s.Add(Format('%s,%d,%d', ['3068bpr0255', bp.color, bp.num]));
      end
      else if bp.part = '3068bpb1102' then
      begin
        s.Add(Format('%s,%d,%d', ['3068bpr0333', bp.color, bp.num]));
      end
      else if bp.part = '30839pb001' then
      begin
        s.Add(Format('%s,%d,%d', ['30839pr0001', bp.color, bp.num]));
      end
      else if bp.part = '30841pb02' then
      begin
        s.Add(Format('%s,%d,%d', ['30841pr0008', bp.color, bp.num]));
      end
      else if bp.part = '30841pb03' then
      begin
        s.Add(Format('%s,%d,%d', ['30841pr0009', bp.color, bp.num]));
      end
      else if bp.part = '30925pb02' then
      begin
        s.Add(Format('%s,%d,%d', ['30925pr0003', bp.color, bp.num]));
      end
      else if bp.part = '3626bpb0121' then
      begin
        s.Add(Format('%s,%d,%d', ['3626bpr0387', bp.color, bp.num]));
      end
      else if bp.part = '3626bpb479' then
      begin
        s.Add(Format('%s,%d,%d', ['3626bpr0680', bp.color, bp.num]));
      end
      else if bp.part = '3626cpb0273' then
      begin
        s.Add(Format('%s,%d,%d', ['3626cpr0498', bp.color, bp.num]));
      end
      else if bp.part = '3626cpb0652' then
      begin
        s.Add(Format('%s,%d,%d', ['3626cpr0929', bp.color, bp.num]));
      end
      else if bp.part = '3626cpb0803' then
      begin
        s.Add(Format('%s,%d,%d', ['3626cpr0955', bp.color, bp.num]));
      end
      else if bp.part = '3626cpb1132' then
      begin
        s.Add(Format('%s,%d,%d', ['3626cpr1441', bp.color, bp.num]));
      end
      else if bp.part = '3626cpb1314' then
      begin
        s.Add(Format('%s,%d,%d', ['3626cpr1769', bp.color, bp.num]));
      end
      else if bp.part = '3626cpb1535' then
      begin
        s.Add(Format('%s,%d,%d', ['3626cpr1849', bp.color, bp.num]));
      end
      else if bp.part = '3626cpb1746' then
      begin
        s.Add(Format('%s,%d,%d', ['3626cpr2113', bp.color, bp.num]));
      end
      else if bp.part = '3678bpb103' then
      begin
        s.Add(Format('%s,%d,%d', ['3678bpr0059', bp.color, bp.num]));
      end
      else if bp.part = '4162p06' then
      begin
        s.Add(Format('%s,%d,%d', ['4162pr0012', bp.color, bp.num]));
      end
      else if bp.part = '4162p07' then
      begin
        s.Add(Format('%s,%d,%d', ['4162pr0014', bp.color, bp.num]));
      end
      else if bp.part = '60017stk01' then
      begin
        s.Add(Format('%s,%d,%d', ['11846', bp.color, bp.num]));
      end
      else if bp.part = '60054stk01a' then
      begin
        s.Add(Format('%s,%d,%d', ['16535', bp.color, bp.num]));
      end
      else if bp.part = '60059stk01a' then
      begin
        s.Add(Format('%s,%d,%d', ['17105', bp.color, bp.num]));
      end
      else if bp.part = '60119stk01a' then
      begin
        s.Add(Format('%s,%d,%d', ['60119stk01', bp.color, bp.num]));
      end
      else if bp.part = '60119stk01b' then begin s.Add(Format('%s,%d,%d', ['60119stk01', bp.color, bp.num])); end
      else if bp.part = '60147stk01a' then begin s.Add(Format('%s,%d,%d', ['29462', bp.color, bp.num])); end
      else if bp.part = '60147stk01b' then begin s.Add(Format('%s,%d,%d', ['29462', bp.color, bp.num])); end
      else if bp.part = '60581pb081' then begin s.Add(Format('%s,%d,%d', ['60581pr0003', bp.color, bp.num])); end
      else if bp.part = '60581pb082' then begin s.Add(Format('%s,%d,%d', ['60581pr0001', bp.color, bp.num])); end
      else if bp.part = '60581pb083' then begin s.Add(Format('%s,%d,%d', ['60581pr0005', bp.color, bp.num])); end
      else if bp.part = '60581pb084' then begin s.Add(Format('%s,%d,%d', ['60581pr0006', bp.color, bp.num])); end
      else if bp.part = '60581pb086' then begin s.Add(Format('%s,%d,%d', ['60581pr0004', bp.color, bp.num])); end
      else if bp.part = '75049stk01a' then begin s.Add(Format('%s,%d,%d', ['17620', bp.color, bp.num])); end
      else if bp.part = '75049stk01b' then begin s.Add(Format('%s,%d,%d', ['17620', bp.color, bp.num])); end
      else if bp.part = '83784' then begin s.Add(Format('%s,%d,%d', ['30370ps3', bp.color, bp.num])); end
      else if bp.part = '85984pb100' then begin s.Add(Format('%s,%d,%d', ['85984pr0143', bp.color, bp.num])); end
      else if bp.part = '87079pb193' then begin s.Add(Format('%s,%d,%d', ['87079pr0073', bp.color, bp.num])); end
      else if bp.part = '87079pb194' then begin s.Add(Format('%s,%d,%d', ['87079pr0074', bp.color, bp.num])); end
      else if bp.part = '87079pb195' then begin s.Add(Format('%s,%d,%d', ['87079pr0075', bp.color, bp.num])); end
      else if bp.part = '87079pb422l' then begin s.Add(Format('%s,%d,%d', ['87079pr0119', bp.color, bp.num])); end
      else if bp.part = '87079pb422L' then begin s.Add(Format('%s,%d,%d', ['87079pr0119', bp.color, bp.num])); end
      else if bp.part = '87079pb422r' then begin s.Add(Format('%s,%d,%d', ['87079pr0120', bp.color, bp.num])); end
      else if bp.part = '87079pb422R' then begin s.Add(Format('%s,%d,%d', ['87079pr0120', bp.color, bp.num])); end
      else if bp.part = '87079pb423l' then begin s.Add(Format('%s,%d,%d', ['87079pr0117', bp.color, bp.num])); end
      else if bp.part = '87079pb423L' then begin s.Add(Format('%s,%d,%d', ['87079pr0117', bp.color, bp.num])); end
      else if bp.part = '87079pb423r' then begin s.Add(Format('%s,%d,%d', ['87079pr0118', bp.color, bp.num])); end
      else if bp.part = '87079pb423R' then begin s.Add(Format('%s,%d,%d', ['87079pr0118', bp.color, bp.num])); end
      else if bp.part = '87079pb426' then begin s.Add(Format('%s,%d,%d', ['87079pr0123', bp.color, bp.num])); end
      else if bp.part = '87079pb431l' then begin s.Add(Format('%s,%d,%d', ['87079pr0128', bp.color, bp.num])); end
      else if bp.part = '87079pb431L' then begin s.Add(Format('%s,%d,%d', ['87079pr0128', bp.color, bp.num])); end
      else if bp.part = '87079pb431r' then begin s.Add(Format('%s,%d,%d', ['87079pr0129', bp.color, bp.num])); end
      else if bp.part = '87079pb431R' then begin s.Add(Format('%s,%d,%d', ['87079pr0129', bp.color, bp.num])); end
      else if bp.part = '87079pb432' then begin s.Add(Format('%s,%d,%d', ['87079pr0125', bp.color, bp.num])); end
      else if bp.part = '87079pb433' then begin s.Add(Format('%s,%d,%d', ['87079pr0127', bp.color, bp.num])); end
      else if bp.part = '90463c01' then begin s.Add(Format('%s,%d,%d', ['90463', bp.color, bp.num])); end
      else if bp.part = '91599' then begin s.Add(Format('%s,%d,%d', ['30370pr04', bp.color, bp.num])); end
      else if bp.part = '93273pb066' then begin s.Add(Format('%s,%d,%d', ['93273pr0009', bp.color, bp.num])); end
      else if bp.part = '93273pb067' then begin s.Add(Format('%s,%d,%d', ['93273pr0010', bp.color, bp.num])); end
      else if bp.part = '93273pb074' then begin s.Add(Format('%s,%d,%d', ['93273pr0011', bp.color, bp.num])); end
      else if bp.part = '93273pb075' then begin s.Add(Format('%s,%d,%d', ['93273pr0012', bp.color, bp.num])); end
      else if bp.part = '93552pb01' then begin s.Add(Format('%s,%d,%d', ['93552pr0001', bp.color, bp.num])); end
      else if bp.part = '93587pb11' then begin s.Add(Format('%s,%d,%d', ['93587pr0009', bp.color, bp.num])); end
      else if bp.part = '93587pb12' then begin s.Add(Format('%s,%d,%d', ['93587pr0010', bp.color, bp.num])); end
      else if bp.part = '93587pb13' then begin s.Add(Format('%s,%d,%d', ['93587pr0011', bp.color, bp.num])); end
      else if bp.part = '93587pb14' then begin s.Add(Format('%s,%d,%d', ['93587pr0012', bp.color, bp.num])); end
      else if bp.part = '93591pb18' then begin s.Add(Format('%s,%d,%d', ['93591pr0021', bp.color, bp.num])); end
      else if bp.part = '93591pb19' then begin s.Add(Format('%s,%d,%d', ['93591pr0023', bp.color, bp.num])); end
      else if bp.part = '93598pb08' then begin s.Add(Format('%s,%d,%d', ['93598pr0011', bp.color, bp.num])); end
      else if bp.part = '93598pb09' then begin s.Add(Format('%s,%d,%d', ['93598pr0009', bp.color, bp.num])); end
      else if bp.part = '93598pb10' then begin s.Add(Format('%s,%d,%d', ['93598pr0010', bp.color, bp.num])); end
      else if bp.part = '970c00pb625' then begin s.Add(Format('%s,%d,%d', ['970cpr1207', bp.color, bp.num])); end
      else if bp.part = '970c09' then begin s.Add(Format('%s,%d,%d', ['970x021', bp.color, bp.num])); end
      else if bp.part = '973pb0086c01' then begin s.Add(Format('%s,%d,%d', ['973pr1163c01', bp.color, bp.num])); end
      else if bp.part = '973pb0263c02' then begin s.Add(Format('%s,%d,%d', ['973pr1182c01', bp.color, bp.num])); end
      else if bp.part = '973pb0278c01' then begin s.Add(Format('%s,%d,%d', ['973pr1183c01', bp.color, bp.num])); end
      else if bp.part = '973pb0410c01' then begin s.Add(Format('%s,%d,%d', ['973pr1244c01', bp.color, bp.num])); end
      else if bp.part = '973pb0549c01' then begin s.Add(Format('%s,%d,%d', ['973pr1479c01', bp.color, bp.num])); end
      else if bp.part = '973pb0631c01' then begin s.Add(Format('%s,%d,%d', ['973pr1573c01', bp.color, bp.num])); end
      else if bp.part = '973pb0638c01' then begin s.Add(Format('%s,%d,%d', ['973pr1585c01', bp.color, bp.num])); end
      else if bp.part = '973pb0649c01' then begin s.Add(Format('%s,%d,%d', ['973pr1580c01', bp.color, bp.num])); end
      else if bp.part = '973pb0708c01' then begin s.Add(Format('%s,%d,%d', ['973pr1617c01', bp.color, bp.num])); end
      else if bp.part = '973pb0997c01' then begin s.Add(Format('%s,%d,%d', ['973pr1720c01', bp.color, bp.num])); end
      else if bp.part = '973pb1978c01' then begin s.Add(Format('%s,%d,%d', ['973pr2923c01', bp.color, bp.num])); end
      else if bp.part = '973pb2017c01' then begin s.Add(Format('%s,%d,%d', ['973pr2998c01', bp.color, bp.num])); end
      else if bp.part = '973pb2060c01' then begin s.Add(Format('%s,%d,%d', ['973pr3018c01a', bp.color, bp.num])); end
      else if bp.part = '973pb2346c01' then begin s.Add(Format('%s,%d,%d', ['973pr3431c01', bp.color, bp.num])); end
      else if bp.part = '973pb2367c01' then begin s.Add(Format('%s,%d,%d', ['973pr3370c01', bp.color, bp.num])); end
      else if bp.part = '973pb2600c01' then begin s.Add(Format('%s,%d,%d', ['973pr3627', bp.color, bp.num])); end
      else if bp.part = '973pb2601c01' then begin s.Add(Format('%s,%d,%d', ['973pr3700', bp.color, bp.num])); end
      else if bp.part = '973pb2607c01' then begin s.Add(Format('%s,%d,%d', ['973pr3655c01', bp.color, bp.num])); end
      else if bp.part = '973pb2663c01' then begin s.Add(Format('%s,%d,%d', ['973pr3699c01', bp.color, bp.num])); end
      else if bp.part = '973pb2664c01' then begin s.Add(Format('%s,%d,%d', ['973pr3732', bp.color, bp.num])); end
      else if bp.part = '973pr0270c01' then begin s.Add(Format('%s,%d,%d', ['973pr2189c01', bp.color, bp.num])); end
      else if bp.part = '98603pb009' then begin s.Add(Format('%s,%d,%d', ['98603s01pr0006', bp.color, bp.num])); end
      else if bp.part = '98613c01' then begin s.Add(Format('%s,%d,%d', ['74261', bp.color, bp.num])); end
      else if bp.part = 'ftfpb037c01' then begin s.Add(Format('%s,%d,%d', ['92456pr0036c01', bp.color, bp.num])); end
      else if bp.part = 'bb564c01pb01' then begin s.Add(Format('%s,%d,%d', ['98611pr01', bp.color, bp.num])); end
      else if bp.part = '60470' then begin s.Add(Format('%s,%d,%d', ['60470a', bp.color, bp.num])); end
      else if bp.part = '30027bc01' then
      begin
        s.Add(Format('%s,%d,%d', ['30027b', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['30028', 0, bp.num]));
      end
      else if (bp.part = '54701c03') and (bp.color = 15) then
      begin
        s.Add(Format('%s,%d,%d', ['54701c03', 25, bp.num]));
      end
      else if (bp.part = '3742c01') then
      begin
        s.Add(Format('%s,%d,%d', ['3742', bp.color, 4 * bp.num]));
      end
      else if (bp.part = '2350apb01') then
      begin
        s.Add(Format('%s,%d,%d', ['2350ap01', bp.color, bp.num]));
      end
      else if (bp.part = '6216m2') then
      begin
        s.Add(Format('%s,%d,%d', ['6216b', bp.color, bp.num]));
      end
      else if (bp.part = '700ed2') then
      begin
        s.Add(Format('%s,%d,%d', ['700ed', bp.color, bp.num]));
      end
      else if (bp.part = '4870c07') then
      begin
        s.Add(Format('%s,%d,%d', ['4870', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['59895', 0, bp.num * 2]));
        s.Add(Format('%s,%d,%d', ['4624', 71, bp.num * 2]));
      end
      else if (bp.part = '2415c01') then
      begin
        s.Add(Format('%s,%d,%d', ['2415', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['3139', 0, bp.num * 2]));
        s.Add(Format('%s,%d,%d', ['3464', 47, bp.num * 2]));
      end
      else if (bp.part = '30366pb03') then
      begin
        s.Add(Format('%s,%d,%d', ['30366ps1', bp.color, bp.num]));
      end
      else if (bp.part = '50747pr0002') then
      begin
        s.Add(Format('%s,%d,%d', ['50747pr02', bp.color, bp.num]));
      end
      else if (bp.part = '42611') then
      begin
        s.Add(Format('%s,%d,%d', ['51011', bp.color, bp.num]));
      end
      else if (bp.part = '132old') then
      begin
        s.Add(Format('%s,%d,%d', ['132a', bp.color, bp.num]));
      end
      else if (bp.part = '132-teeth') then
      begin
        s.Add(Format('%s,%d,%d', ['132c', bp.color, bp.num]));
      end
      else if (bp.part = '132-hollow') then
      begin
        s.Add(Format('%s,%d,%d', ['132b', bp.color, bp.num]));
      end
      else if (bp.part = '2440pb002') then
      begin
        s.Add(Format('%s,%d,%d', ['2440p01', bp.color, bp.num]));
      end
      else if (bp.part = '76041') and (bp.color = 0) then
      begin
        s.Add(Format('%s,%d,%d', ['76041c01', bp.color, bp.num]));
      end
      else if (bp.part = '2440pb003') then
      begin
        s.Add(Format('%s,%d,%d', ['2440', bp.color, bp.num]));
      end
      else if (bp.part = '3004pb024') then
      begin
        s.Add(Format('%s,%d,%d', ['3004', bp.color, bp.num]));
      end
      else if (bp.part = '11820') and (bp.color = -1) then
      begin
        s.Add(Format('%s,%d,%d', ['93088pr0001a', 92, bp.num]));
      end
      else if bp.part = '167' then
      begin
        s.Add(Format('%s,%d,%d', ['6190', bp.color, bp.num]));
      end
      else if bp.part = '16723' then
      begin
        s.Add(Format('%s,%d,%d', ['95342pr0001', bp.color, bp.num]));
      end
      else if bp.part = '17514' then
      begin
        s.Add(Format('%s,%d,%d', ['88000', bp.color, bp.num]));
      end
      else if bp.part = '18227' then
      begin
        s.Add(Format('%s,%d,%d', ['87990', bp.color, bp.num]));
      end
      else if bp.part = '19995' then
      begin
        s.Add(Format('%s,%d,%d', ['12888pr0001', bp.color, bp.num]));
      end
      else if bp.part = '2335' then
      begin
        s.Add(Format('%s,%d,%d', ['11055', bp.color, bp.num]));
      end
      else if bp.part = '2335pb107' then
      begin
        s.Add(Format('%s,%d,%d', ['11055pr0007', bp.color, bp.num]));
      end
      else if bp.part = '2446pb07' then
      begin
        s.Add(Format('%s,%d,%d', ['2446p50', bp.color, bp.num]));
      end
      else if bp.part = '2446pb28' then
      begin
        s.Add(Format('%s,%d,%d', ['2446p50', bp.color, bp.num]));
      end
      else if bp.part = '2446pb32' then
      begin
        s.Add(Format('%s,%d,%d', ['2446pr0001', bp.color, bp.num]));
      end
      else if bp.part = '2446pr0023' then
      begin
        s.Add(Format('%s,%d,%d', ['2446pr23', bp.color, bp.num]));
      end
      else if bp.part = '2446px4' then
      begin
        s.Add(Format('%s,%d,%d', ['2446p51', bp.color, bp.num]));
      end
      else if bp.part = '2466pb01' then
      begin
        s.Add(Format('%s,%d,%d', ['2466', bp.color, bp.num]));
      end
      else if bp.part = '3001pe1' then
      begin
        s.Add(Format('%s,%d,%d', ['3001pr1', bp.color, bp.num]));
      end
      else if bp.part = '3005ptA' then
      begin
        s.Add(Format('%s,%d,%d', ['3005pta', bp.color, bp.num]));
      end
      else if bp.part = '3005ptG' then
      begin
        s.Add(Format('%s,%d,%d', ['3005ptg', bp.color, bp.num]));
      end
      else if bp.part = '3010p16' then
      begin
        s.Add(Format('%s,%d,%d', ['3010pr0016', bp.color, bp.num]));
      end
      else if bp.part = '3010pb014' then
      begin
        s.Add(Format('%s,%d,%d', ['3010apr014', bp.color, bp.num]));
      end
      else if bp.part = '3010px5' then
      begin
        s.Add(Format('%s,%d,%d', ['3010apr0001', bp.color, bp.num]));
      end
      else if bp.part = '3037pb004' then
      begin
        s.Add(Format('%s,%d,%d', ['3037pc0', bp.color, bp.num]));
      end
      else if (bp.part = '3037pb06') or (bp.part = '3037px9') then
      begin
        s.Add(Format('%s,%d,%d', ['3037pb006', bp.color, bp.num]));
      end
      else if bp.part = '3039pr0062' then
      begin
        s.Add(Format('%s,%d,%d', ['3039pr62', bp.color, bp.num]));
      end
      else if bp.part = '3040pr0003' then
      begin
        s.Add(Format('%s,%d,%d', ['3040bpr0003', bp.color, bp.num]));
      end
      else if bp.part = '3041p01' then
      begin
        s.Add(Format('%s,%d,%d', ['3041pb001', bp.color, bp.num]));
      end
      else if bp.part = '30602pb03' then
      begin
        s.Add(Format('%s,%d,%d', ['30602pb003', bp.color, bp.num]));
      end
      else if bp.part = '30602pb15' then
      begin
        s.Add(Format('%s,%d,%d', ['30602pb015', bp.color, bp.num]));
      end
      else if bp.part = '3062bpb038' then
      begin
        s.Add(Format('%s,%d,%d', ['3062bpr0002', bp.color, bp.num]));
      end
      else if bp.part = '3192a' then
      begin
        s.Add(Format('%s,%d,%d', ['3192', bp.color, bp.num]));
      end
      else if bp.part = '3298pb008' then
      begin
        s.Add(Format('%s,%d,%d', ['3298p75', bp.color, bp.num]));
      end
      else if bp.part = '3298pb40' then
      begin
        s.Add(Format('%s,%d,%d', ['3298pr0029', bp.color, bp.num]));
      end
      else if bp.part = '3298pb41' then
      begin
        s.Add(Format('%s,%d,%d', ['3298pr0030', bp.color, bp.num]));
      end
      else if bp.part = '3298pr0005' then
      begin
        s.Add(Format('%s,%d,%d', ['3298p19', bp.color, bp.num]));
      end
      else if bp.part = '3298pr0008' then
      begin
        s.Add(Format('%s,%d,%d', ['3298p53', bp.color, bp.num]));
      end
      else if bp.part = '3622pb003' then
      begin
        s.Add(Format('%s,%d,%d', ['3622pf1', bp.color, bp.num]));
      end
      else if bp.part = '3193a' then
      begin
        s.Add(Format('%s,%d,%d', ['3193', bp.color, bp.num]));
      end
      else if bp.part = '41681c01' then
      begin
        s.Add(Format('%s,%d,%d', ['41679', bp.color, 2 * bp.num]));
        s.Add(Format('%s,%d,%d', ['41681', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['41680', 0, bp.num]));
      end
      else if bp.part = '41681c02' then
      begin
        s.Add(Format('%s,%d,%d', ['41679', bp.color, 2 * bp.num]));
        s.Add(Format('%s,%d,%d', ['41681', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['41680', 7, bp.num]));
      end
      else if (bp.part = '4286pb01') or (bp.part = '4286pb04') then
      begin
        s.Add(Format('%s,%d,%d', ['4286', bp.color, bp.num]));
      end
      else if bp.part = '98138pb036' then
      begin
        s.Add(Format('%s,%d,%d', ['98138pr0038', bp.color, bp.num]));
      end
      else if bp.part = '87079pb465' then
      begin
        s.Add(Format('%s,%d,%d', ['87079pr9995', bp.color, bp.num]));
      end
      else if bp.part = '43702pb02' then
      begin
        s.Add(Format('%s,%d,%d', ['43702pr0001', bp.color, bp.num]));
      end
      else if bp.part = '43898pb001' then
      begin
        s.Add(Format('%s,%d,%d', ['43702pr0001', bp.color, bp.num]));
      end
      else if bp.part = '445' then
      begin
        s.Add(Format('%s,%d,%d', ['3193', bp.color, bp.num]));
      end
      else if bp.part = '446' then
      begin
        s.Add(Format('%s,%d,%d', ['3192', bp.color, bp.num]));
      end
      else if bp.part = '45729' then
      begin
        s.Add(Format('%s,%d,%d', ['44375a', bp.color, bp.num]));
      end
      else if bp.part = '47326c03' then
      begin
        s.Add(Format('%s,%d,%d', ['47326pat03', bp.color, bp.num]));
      end
      else if bp.part = '4740pr0004' then
      begin
        s.Add(Format('%s,%d,%d', ['4740pr0001a', bp.color, bp.num]));
      end
      else if (bp.part = '47847pb003U') or (bp.part = '47847pb003u') then
      begin
        s.Add(Format('%s,%d,%d', ['47847pat0003', bp.color, bp.num]));
      end
      else if bp.part = '4864apx4' then
      begin
        s.Add(Format('%s,%d,%d', ['4864apt4', bp.color, bp.num]));
      end
      else if bp.part = '48958' then
      begin
        s.Add(Format('%s,%d,%d', ['3070bpr007', bp.color, bp.num]));
      end
      else if bp.part = '50860c02' then
      begin
        s.Add(Format('%s,%d,%d', ['50860', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['50859b', 0, bp.num]));
        s.Add(Format('%s,%d,%d', ['50861', 0, 2 * bp.num]));
        s.Add(Format('%s,%d,%d', ['50862', 71, 2 * bp.num]));
      end
      else if bp.part = '6156pb01' then
      begin
        s.Add(Format('%s,%d,%d', ['6156', bp.color, bp.num]));
      end
      else if bp.part = '6469a' then
      begin
        s.Add(Format('%s,%d,%d', ['6469', bp.color, bp.num]));
      end
      else if bp.part = '700ed2' then
      begin
        s.Add(Format('%s,%d,%d', ['700ed', bp.color, bp.num]));
      end
      else if bp.part = '86210' then
      begin
        s.Add(Format('%s,%d,%d', ['60603', bp.color, bp.num]));
      end
      else if bp.part = '87621pb01' then
      begin
        s.Add(Format('%s,%d,%d', ['87621pr01', bp.color, bp.num]));
      end
      else if bp.part = '88410' then
      begin
        s.Add(Format('%s,%d,%d', ['61506', bp.color, bp.num]));
      end
      else if bp.part = '93218pb01' then
      begin
        s.Add(Format('%s,%d,%d', ['18746pr0001', bp.color, bp.num]));
      end
      else if bp.part = '94638' then
      begin
        s.Add(Format('%s,%d,%d', ['87552', bp.color, bp.num]));
      end
      else if bp.part = '95820' then
      begin
        s.Add(Format('%s,%d,%d', ['30237b', bp.color, bp.num]));
      end
      else if bp.part = '4624c02' then
      begin
        s.Add(Format('%s,%d,%d', ['4624', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['3641', 0, bp.num]));
      end
      else if bp.part = '4532a' then
      begin
        s.Add(Format('%s,%d,%d', ['92410', bp.color, bp.num]));
      end
      else if bp.part = '6218' then
      begin
        s.Add(Format('%s,%d,%d', ['6259', bp.color, bp.num]));
      end
      else if bp.part = '61627pb01' then
      begin
        s.Add(Format('%s,%d,%d', ['sailbb41', bp.color, bp.num]));
      end
      else if bp.part = 'FTFpb104c01' then
      begin
        s.Add(Format('%s,%d,%d', ['92456c01pr0099', bp.color, bp.num]));
      end
      else if bp.part = '970c85' then
      begin
        s.Add(Format('%s,%d,%d', ['970', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['971', 72, bp.num]));
        s.Add(Format('%s,%d,%d', ['972', 72, bp.num]));
      end
      else if bp.part = '98288c01' then
      begin
        s.Add(Format('%s,%d,%d', ['98288', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['59895', 0, bp.num]));
        s.Add(Format('%s,%d,%d', ['3464', 15, bp.num]));
      end
      else if bp.part = '970c01' then
      begin
        s.Add(Format('%s,%d,%d', ['970', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['971', 15, bp.num]));
        s.Add(Format('%s,%d,%d', ['972', 15, bp.num]));
      end
      else if bp.part = '970c03' then
      begin
        s.Add(Format('%s,%d,%d', ['970', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['971', 14, bp.num]));
        s.Add(Format('%s,%d,%d', ['972', 14, bp.num]));
      end
      else if bp.part = '6026c01' then
      begin
        s.Add(Format('%s,%d,%d', ['6026', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['6027', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['6028', bp.color, bp.num]));
      end
      else if bp.part = '3937c01' then
      begin
        s.Add(Format('%s,%d,%d', ['3937', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['3938', bp.color, bp.num]));
      end
      else if bp.part = '4032' then
      begin
        s.Add(Format('%s,%d,%d', ['4032a', bp.color, bp.num]));
      end
      else if bp.part = '970c11' then
      begin
        s.Add(Format('%s,%d,%d', ['970', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['971', 0, bp.num]));
        s.Add(Format('%s,%d,%d', ['972', 0, bp.num]));
      end
      else if bp.part = '4719c01' then
      begin
        s.Add(Format('%s,%d,%d', ['4719', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['2807', 0, 2 * bp.num]));
        s.Add(Format('%s,%d,%d', ['4720', 47, 2 * bp.num]));
      end
      else if bp.part = '48002' then
      begin
        s.Add(Format('%s,%d,%d', ['48002a', bp.color, bp.num]));
      end
      else if bp.part = '2547c01' then
      begin
        s.Add(Format('%s,%d,%d', ['2547', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['2548', bp.color, bp.num]));
      end
      else if bp.part = '2495c01' then
      begin
        s.Add(Format('%s,%d,%d', ['2495', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['2496', 0, bp.num]));
      end
      else if bp.part = '3680c01' then
      begin
        s.Add(Format('%s,%d,%d', ['3680', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['3679', 7, bp.num]));
      end
      else if bp.part = '3680c02' then
      begin
        s.Add(Format('%s,%d,%d', ['3680', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['3679', 71, bp.num]));
      end
      else if bp.part = '3068bpb21' then
      begin
        s.Add(Format('%s,%d,%d', ['3068bpb0021', bp.color, bp.num]));
      end
      else if bp.part = '3069bpb084' then
      begin
        s.Add(Format('%s,%d,%d', ['3069bpr0002', bp.color, bp.num]));
      end
      else if bp.part = '3070bpr0001a' then
      begin
        s.Add(Format('%s,%d,%d', ['3070bp01', bp.color, bp.num]));
      end
      else if bp.part = '3070bpr0003' then
      begin
        s.Add(Format('%s,%d,%d', ['3070bp03', bp.color, bp.num]));
      end
      else if bp.part = '3070pr0149' then
      begin
        s.Add(Format('%s,%d,%d', ['3070bpr0149', bp.color, bp.num]));
      end
      else if bp.part = '4150px16' then
      begin
        s.Add(Format('%s,%d,%d', ['4150pa0', bp.color, bp.num]));
      end
      else if bp.part = '41747pb09' then
      begin
        s.Add(Format('%s,%d,%d', ['41747pb009', bp.color, bp.num]));
      end
      else if bp.part = '41748pb09' then
      begin
        s.Add(Format('%s,%d,%d', ['41748pb009', bp.color, bp.num]));
      end
      else if bp.part = '50944pb01c02' then
      begin
        s.Add(Format('%s,%d,%d', ['50944pr0001', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['51011', 0, bp.num]));
      end
      else if bp.part = '57877' then
      begin
        s.Add(Format('%s,%d,%d', ['57051', bp.color, bp.num]));
      end
      else if bp.part = '6014ac01' then
      begin
        s.Add(Format('%s,%d,%d', ['6014a', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['6015', 0, bp.num]));
      end
      else if bp.part = '6159c01' then
      begin
        s.Add(Format('%s,%d,%d', ['6159', bp.color, bp.num]));
      end
      else if bp.part = '2352' then
      begin
        s.Add(Format('%s,%d,%d', ['2352a', bp.color, bp.num]));
      end
      else if bp.part = '6584a' then
      begin
        s.Add(Format('%s,%d,%d', ['6584', bp.color, bp.num]));
      end
      else if bp.part = '4287' then
      begin
        s.Add(Format('%s,%d,%d', ['4287a', bp.color, bp.num]));
      end
      else if bp.part = '71137b' then
      begin
        s.Add(Format('%s,%d,%d', ['40620', bp.color, bp.num]));
      end
      else if bp.part = '92851c01' then
      begin
        s.Add(Format('%s,%d,%d', ['92851', bp.color, bp.num]));
      end
      else if (bp.part = '87079pb029r') or (bp.part = '87079pb029R') then
      begin
        s.Add(Format('%s,%d,%d', ['87079pr0017', bp.color, bp.num]));
      end
      else if (bp.part = '87079pb029l') or (bp.part = '87079pb029L') then
      begin
        s.Add(Format('%s,%d,%d', ['87079pr0018', bp.color, bp.num]));
      end
      else if (bp.part = '87079pb031l') or (bp.part = '87079pb031L') then
      begin
        s.Add(Format('%s,%d,%d', ['87079pr0020', bp.color, bp.num]));
      end
      else if (bp.part = '87079pb031r') or (bp.part = '87079pb031R') then
      begin
        s.Add(Format('%s,%d,%d', ['87079pr0019', bp.color, bp.num]));
      end
      else if (bp.part = '87079pb052l') or (bp.part = '87079pb052L') then
      begin
        s.Add(Format('%s,%d,%d', ['87079pr0035', bp.color, bp.num]));
      end
      else if (bp.part = '87079pb052r') or (bp.part = '87079pb052R') then
      begin
        s.Add(Format('%s,%d,%d', ['87079pr0034', bp.color, bp.num]));
      end
      else if bp.part = '2437pb04' then
      begin
        s.Add(Format('%s,%d,%d', ['2437pr0004', bp.color, bp.num]));
      end
      else if bp.part = '3060bpr0117' then
      begin
        s.Add(Format('%s,%d,%d', ['3069bpr0117', bp.color, bp.num]));
      end
      else if bp.part = '87079pb400' then
      begin
        s.Add(Format('%s,%d,%d', ['87079pr0097', bp.color, bp.num]));
      end
      else if bp.part = '93587pb01' then
      begin
        s.Add(Format('%s,%d,%d', ['93587pr0004', bp.color, bp.num]));
      end
      else if bp.part = '93591pb01' then
      begin
        s.Add(Format('%s,%d,%d', ['93591pr0002', bp.color, bp.num]));
      end
      else if bp.part = '93593c01' then
      begin
        s.Add(Format('%s,%d,%d', ['93593', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['50951', 0, bp.num]));
      end
      else if bp.part = '93595c01' then
      begin
        s.Add(Format('%s,%d,%d', ['93595', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['50951', 0, bp.num]));
      end
      else if bp.part = '973pr0655c01' then
      begin
        s.Add(Format('%s,%d,%d', ['973pr1192c01', bp.color, bp.num]));
      end
      else if bp.part = '3678bpb030' then
      begin
        s.Add(Format('%s,%d,%d', ['3678bpr0014a', bp.color, bp.num]));
      end
      else if bp.part = '3678bpb031' then
      begin
        s.Add(Format('%s,%d,%d', ['3678bpr0023a', bp.color, bp.num]));
      end
      else if bp.part = '93587pb06' then
      begin
        s.Add(Format('%s,%d,%d', ['93587pr0001', bp.color, bp.num]));
      end
      else if (bp.part = '87079pb187r') or (bp.part = '87079pb187R') then
      begin
        s.Add(Format('%s,%d,%d', ['87079pr0014', bp.color, bp.num]));
      end
      else if (bp.part = '87079pb187l') or (bp.part = '87079pb187L') then
      begin
        s.Add(Format('%s,%d,%d', ['87079pr0015', bp.color, bp.num]));
      end
      else if bp.part = '2431pb162' then
      begin
        s.Add(Format('%s,%d,%d', ['2431pr0033', bp.color, bp.num]));
      end
      else if (bp.part = '3678bpb032l') or (bp.part = '3678bpb032L') then
      begin
        s.Add(Format('%s,%d,%d', ['3678bpr0020', bp.color, bp.num]));
      end
      else if (bp.part = '3678bpb032r') or (bp.part = '3678bpb032R') then
      begin
        s.Add(Format('%s,%d,%d', ['3678bpr0016a', bp.color, bp.num]));
      end
      else if bp.part = '3678bpb044' then
      begin
        s.Add(Format('%s,%d,%d', ['3678bpr0027a', bp.color, bp.num]));
      end
      else if bp.part = '46220' then
      begin
        s.Add(Format('%s,%d,%d', ['46224c01', bp.color, bp.num]));
      end
      else if bp.part = '4738ac01' then
      begin
        s.Add(Format('%s,%d,%d', ['4738a', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['4739a', bp.color, bp.num]));
      end
      else if bp.part = '61485c01' then
      begin
        s.Add(Format('%s,%d,%d', ['60474', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['61485', bp.color, bp.num]));
      end
      else if bp.part = '10174stk01' then
      begin
        s.Add(Format('%s,%d,%d', ['56143', bp.color, bp.num]));
      end
      else if bp.part = '85777pb01' then
      begin
        s.Add(Format('%s,%d,%d', ['85777', bp.color, bp.num]));
      end
      else if bp.part = '93597pb001' then
      begin
        s.Add(Format('%s,%d,%d', ['93597pr0001', bp.color, bp.num]));
      end
      else if bp.part = '93597pb002' then
      begin
        s.Add(Format('%s,%d,%d', ['93597pr0002', bp.color, bp.num]));
      end
      else if bp.part = '93597pb004' then
      begin
        s.Add(Format('%s,%d,%d', ['93597pr0007', bp.color, bp.num]));
      end
      else if (bp.part = '93606pb001r') or (bp.part = '93606pb001R') then
      begin
        s.Add(Format('%s,%d,%d', ['93606pr0001', bp.color, bp.num]));
      end
      else if (bp.part = '93606pb001l') or (bp.part = '93606pb001L') then
      begin
        s.Add(Format('%s,%d,%d', ['93606pr0002', bp.color, bp.num]));
      end
      else if bp.part = '973pb2740c01' then
      begin
        s.Add(Format('%s,%d,%d', ['973pr3809c01', bp.color, bp.num]));
      end
      else if bp.part = '98834pb01a' then
      begin
        s.Add(Format('%s,%d,%d', ['98834pr0002', bp.color, bp.num]));
      end
      else if bp.part = '98835pb002' then
      begin
        s.Add(Format('%s,%d,%d', ['98835pr0002', bp.color, bp.num]));
      end
      else if bp.part = '98835pb003' then
      begin
        s.Add(Format('%s,%d,%d', ['98835pr0001', bp.color, bp.num]));
      end
      else if bp.part = 'bb661' then
      begin
        s.Add(Format('%s,%d,%d', ['71965', bp.color, bp.num]));
      end
      else if bp.part = '12247' then
      begin
        s.Add(Format('%s,%d,%d', ['54094pr01', bp.color, bp.num]));
      end
      else if bp.part = '3678bpb032l' then
      begin
        s.Add(Format('%s,%d,%d', ['3678bpr0020', bp.color, bp.num]));
      end
      else if bp.part = '3678bpb032r' then
      begin
        s.Add(Format('%s,%d,%d', ['3678bpr0016a', bp.color, bp.num]));
      end
      else if bp.part = '93595pb02c01' then
      begin
        s.Add(Format('%s,%d,%d', ['93595pr0001', bp.color, bp.num]));
        s.Add(Format('%s,%d,%d', ['50951', 0, bp.num]));
      end
      else
        s.Add(Format('%s,%d,%d', [bp.part, bp.color, bp.num]));
    end;
  end
  else
  begin
    for i := 0 to fnumlooseparts - 1 do
    begin
      bp := flooseparts[i];
      sinv := nil;
      if (bp.color = -1) or (bp.color = 9999) or (bp.color = 89) then
      begin
        sinv := db.GetSetInventory(bp.part);
        if sinv = nil then
          if bp.pci <> nil then
            if (bp.pci as TPieceColorInfo).pieceinfo as TPieceInfo <> nil then
              if ((bp.pci as TPieceColorInfo).pieceinfo as TPieceInfo).category = CATEGORYCUSTOMMINIFIGS then
                Continue;
      end;
      if sinv <> nil then
      begin
        for j := 0 to bp.num - 1 do
          sinv.GetLoosePartsInStringList(s, rbconvert);
      end
      else
        s.Add(Format('%s,%d,%d', [bp.part, bp.color, bp.num]));
    end;
  end;
end;

procedure TBrickInventory.GetSetsInStringList(const s: TStringList);
var
  i: integer;
begin
  for i := 0 to fnumsets - 1 do
    if fsets[i].num > 0 then
      s.Add(fsets[i].setid + ',-1,' + itoa(fsets[i].num));
end;

function TBrickInventory.AsText: string;
var
  s: TStringList;
begin
  s := TStringList.Create;
  try
    s.Add('Part,Color,Quantity');
    GetLoosePartsInStringList(s, False);
    GetSetsInStringList(s);
    Result := s.Text;
  finally
    s.Free;
  end;
end;

procedure TBrickInventory.SaveLoosePartsForRebrickable(const fname: string);
var
  s: TStringList;
//  i: integer;
//  sold, snew: string;
//  p, c, n: string;
begin
  s := TStringList.Create;
  try
    s.Add('Part,Color,Quantity');
    try
      GetLoosePartsInStringList(s, True);
      {for i := 1 to s.Count - 1 do
      begin
        sold := s.Strings[i];
        splitstring(sold, p, c, n, ',');
        snew := p + ',' + itoa(db.Colors(atoi(c)).RebrickableColor) + ',' + n;
        if snew <> sold then
          s.Strings[i] := snew;
      end;}
      S_BackupFile(fname);
      S_SaveToFile(s, fname);
    except
      I_Warning('TBrickInventory.SaveLoosePartsForRebrickable(): Can not save file %s'#13#10, [fname]);
    end;
  finally
    s.Free;
  end;
end;

procedure TBrickInventory.SaveLooseSetsForRebrickable(const fname: string);
var
  s: TStringList;
  i: integer;
begin
  s := TStringList.Create;
  try
    s.Add('set number,quantity');
    for i := 0 to fnumsets - 1 do
      if fsets[i].num > 0 then
        if not db.IsMoc(fsets[i].setid) then
          s.Add(Format('%s,%d', [fsets[i].setid, fsets[i].num]));

    try
      S_BackupFile(fname);
      S_SaveToFile(s, fname);
    except
      I_Warning('TBrickInventory.SaveLooseSetsForRebrickable(): Can not save file %s'#13#10, [fname]);
    end;
  finally
    s.Free;
  end;
end;

procedure TBrickInventory.SaveLoosePartsWantedListNew(const fname: string;
  const pricefactor: Double = 1.0; const wl: Integer = 0);
var
  s: TStringList;
  i: integer;
  pci: TPieceColorInfo;
begin
  s := TStringList.Create;
  s.Add('<INVENTORY>');
  for i := 0 to fnumlooseparts - 1 do
  begin
    pci := db.PieceColorInfo(flooseparts[i].part, flooseparts[i].color);
    if pci <> nil then
    begin
      if pci.EvaluatePriceNew > 0 then
        s.Add(
          Format(
            '<ITEM><ITEMTYPE>%s</ITEMTYPE><ITEMID>%s</ITEMID><COLOR>%d</COLOR><MAXPRICE>%2.4f</MAXPRICE><MINQTY>%d</MINQTY><NOTIFY>N</NOTIFY><WANTEDLISTID>%d</WANTEDLISTID></ITEM>',
            [pci.ItemType, db.BrickLinkPart(flooseparts[i].part), db.colors(flooseparts[i].color).BrickLingColor, pci.EvaluatePriceNew * pricefactor, flooseparts[i].num, wl]))
      else
        s.Add(
          Format(
            '<ITEM><ITEMTYPE>%s</ITEMTYPE><ITEMID>%s</ITEMID><COLOR>%d</COLOR><MINQTY>%d</MINQTY><NOTIFY>N</NOTIFY><WANTEDLISTID>%d</WANTEDLISTID></ITEM>',
            [pci.ItemType, db.BrickLinkPart(flooseparts[i].part), db.colors(flooseparts[i].color).BrickLingColor, flooseparts[i].num, wl]));
    end
    else
    begin
      s.Add(
        Format(
          '<ITEM><ITEMTYPE>P</ITEMTYPE><ITEMID>%s</ITEMID><COLOR>%d</COLOR><MINQTY>%d</MINQTY><NOTIFY>N</NOTIFY><WANTEDLISTID>%d</WANTEDLISTID></ITEM>',
          [db.BrickLinkPart(flooseparts[i].part), db.colors(flooseparts[i].color).BrickLingColor, flooseparts[i].num, wl]));
    end;
  end;
  s.Add('</INVENTORY>');
  try
    S_SaveToFile(s, fname);
  except
    I_Warning('TBrickInventory.SaveLoosePartsWantedList(): Can not save file %s'#13#10, [fname]);
  end;
  s.Free;
end;

procedure TBrickInventory.SaveLoosePartsWantedListUsed(const fname: string;
  const pricefactor: Double = 1.0; const wl: Integer = 0);
var
  s: TStringList;
  i: integer;
  pci: TPieceColorInfo;
begin
  s := TStringList.Create;
  s.Add('<INVENTORY>');
  for i := 0 to fnumlooseparts - 1 do
  begin
    pci := db.PieceColorInfo(@flooseparts[i]);
    if pci <> nil then
    begin
      if flooseparts[i].color = INSTRUCTIONCOLORINDEX then
      begin
        if pci.EvaluatePriceUsed > 0 then
          s.Add(
            Format(
              '<ITEM><ITEMTYPE>I</ITEMTYPE><ITEMID>%s</ITEMID><MAXPRICE>%2.4f</MAXPRICE><MINQTY>%d</MINQTY><NOTIFY>N</NOTIFY><WANTEDLISTID>%d</WANTEDLISTID></ITEM>',
              [db.BrickLinkPart(flooseparts[i].part), pci.EvaluatePriceUsed * pricefactor, flooseparts[i].num, wl]))
        else
          s.Add(
            Format(
              '<ITEM><ITEMTYPE>I</ITEMTYPE><ITEMID>%s</ITEMID><MINQTY>%d</MINQTY><NOTIFY>N</NOTIFY><WANTEDLISTID>%d</WANTEDLISTID></ITEM>',
              [db.BrickLinkPart(flooseparts[i].part), flooseparts[i].num, wl]));
      end
      else if flooseparts[i].color = BOXCOLORINDEX then
      begin
        if pci.EvaluatePriceUsed > 0 then
          s.Add(
            Format(
              '<ITEM><ITEMTYPE>B</ITEMTYPE><ITEMID>%s</ITEMID><MAXPRICE>%2.4f</MAXPRICE><MINQTY>%d</MINQTY><NOTIFY>N</NOTIFY><WANTEDLISTID>%d</WANTEDLISTID></ITEM>',
              [db.BrickLinkPart(flooseparts[i].part), pci.EvaluatePriceUsed * pricefactor, flooseparts[i].num, wl]))
        else
          s.Add(
            Format(
              '<ITEM><ITEMTYPE>B</ITEMTYPE><ITEMID>%s</ITEMID><MINQTY>%d</MINQTY><NOTIFY>N</NOTIFY><WANTEDLISTID>%d</WANTEDLISTID></ITEM>',
              [db.BrickLinkPart(flooseparts[i].part), flooseparts[i].num, wl]));
      end
      else
      begin
        if pci.EvaluatePriceUsed > 0 then
          s.Add(
            Format(
              '<ITEM><ITEMTYPE>%s</ITEMTYPE><ITEMID>%s</ITEMID><COLOR>%d</COLOR><MAXPRICE>%2.4f</MAXPRICE><MINQTY>%d</MINQTY><NOTIFY>N</NOTIFY><WANTEDLISTID>%d</WANTEDLISTID></ITEM>',
              [pci.ItemType, db.BrickLinkPart(flooseparts[i].part), db.colors(flooseparts[i].color).BrickLingColor, pci.EvaluatePriceUsed * pricefactor, flooseparts[i].num, wl]))
        else
          s.Add(
            Format(
              '<ITEM><ITEMTYPE>%s</ITEMTYPE><ITEMID>%s</ITEMID><COLOR>%d</COLOR><MINQTY>%d</MINQTY><NOTIFY>N</NOTIFY><WANTEDLISTID>%d</WANTEDLISTID></ITEM>',
              [pci.ItemType, db.BrickLinkPart(flooseparts[i].part), db.colors(flooseparts[i].color).BrickLingColor, flooseparts[i].num, wl]));
      end;
    end
    else
    begin
      s.Add(
        Format(
          '<ITEM><ITEMTYPE>P</ITEMTYPE><ITEMID>%s</ITEMID><COLOR>%d</COLOR><MINQTY>%d</MINQTY><NOTIFY>N</NOTIFY><WANTEDLISTID>%d</WANTEDLISTID></ITEM>',
          [db.BrickLinkPart(flooseparts[i].part), db.colors(flooseparts[i].color).BrickLingColor, flooseparts[i].num, wl]));
    end;
  end;
  s.Add('</INVENTORY>');
  try
    S_SaveToFile(s, fname);
  except
    I_Warning('TBrickInventory.SaveLoosePartsWantedList(): Can not save file %s'#13#10, [fname]);
  end;
  s.Free;
end;

procedure TBrickInventory.SavePartsInventoryPriceguide(const fname: string);
var
  s: TStringList;
  i: integer;
  pci: TPieceColorInfo;
begin
  s := TStringList.Create;
  s.Add('Part;Color;Num;pg_nTimesSold;pg_nTotalQty;pg_nMinPrice;pg_nAvgPrice;' +
        'pg_nQtyAvgPrice;pg_nMaxPrice;pg_uTimesSold;pg_uTotalQty;pg_uMinPrice;' +
        'pg_uAvgPrice;pg_uQtyAvgPrice;pg_uMaxPrice;av_nTotalLots;av_nTotalQty;' +
        'av_nMinPrice;av_nAvgPrice;av_nQtyAvgPrice;av_nMaxPrice;av_uTotalLots;' +
        'av_uTotalQty;av_uMinPrice;av_uAvgPrice;av_uQtyAvgPrice;av_uMaxPrice;' +
        'EvaluatePriceNew;EvaluatePriceUsed');
  for i := 0 to fnumlooseparts - 1 do
  begin
    pci := db.PieceColorInfo(@flooseparts[i]);
    if pci <> nil then
    begin
      if not pci.hasloaded then
        pci.Load;
      s.Add(Format('%s;%d;%d;%d;%d;%2.5f;%2.5f;%2.5f;%2.5f;%d;%d;%2.5f;%2.5f;%2.5f;%2.5f'+
      ';%d;%d;%2.5f;%2.5f;%2.5f;%2.5f;%d;%d;%2.5f;%2.5f;%2.5f;%2.5f;%2.5f;%2.5f', [
        flooseparts[i].part,
        flooseparts[i].color,
        flooseparts[i].num,
        pci.priceguide.nTimesSold,
        pci.priceguide.nTotalQty,
        pci.priceguide.nMinPrice,
        pci.priceguide.nAvgPrice,
        pci.priceguide.nQtyAvgPrice,
        pci.priceguide.nMaxPrice,
        pci.priceguide.uTimesSold,
        pci.priceguide.uTotalQty,
        pci.priceguide.uMinPrice,
        pci.priceguide.uAvgPrice,
        pci.priceguide.uQtyAvgPrice,
        pci.priceguide.uMaxPrice,
        pci.availability.nTotalLots,
        pci.availability.nTotalQty,
        pci.availability.nMinPrice,
        pci.availability.nAvgPrice,
        pci.availability.nQtyAvgPrice,
        pci.availability.nMaxPrice,
        pci.availability.uTotalLots,
        pci.availability.uTotalQty,
        pci.availability.uMinPrice,
        pci.availability.uAvgPrice,
        pci.availability.uQtyAvgPrice,
        pci.availability.uMaxPrice,
        pci.EvaluatePriceNew,
        pci.EvaluatePriceUsed
      ]));
    end
    else
      s.Add(Format('%s,%d,%d', [flooseparts[i].part, flooseparts[i].color, flooseparts[i].num]));
  end;
  try
    S_BackupFile(fname);
    S_SaveToFile(s, fname);
  except
    I_Warning('TBrickInventory.SavePartsInventoryPriceguide(): Can not save file %s'#13#10, [fname]);
  end;
  s.Free;
end;

procedure TBrickInventory.SaveSets(const fname: string);
var
  s: TStringList;
  i: integer;
begin
  s := TStringList.Create;
  s.Add('Set,Num,Dismantaled');
  for i := 0 to fnumsets - 1 do
    s.Add(Format('%s,%d,%d', [fsets[i].setid, fsets[i].num, fsets[i].dismantaled]));
  try
    S_BackupFile(fname);
    S_SaveToFile(s, fname);
  except
    I_Warning('TBrickInventory.SaveSets(): Can not save file %s'#13#10, [fname]);
  end;
  s.Free;
end;

procedure TBrickInventory.Reorganize;
begin
  if not fneedsReorganize then
    Exit;
  DoReorganize;
end;

procedure TBrickInventory.DoReorganize;
var
  nparts: brickpool_pa;
  nnum: integer;
  i: integer;
  pci: TPieceColorInfo;
  cl: integer;
begin
  if fnumlooseparts = 0 then
    Exit;
  nnum := fnumlooseparts;
  GetMem(nparts, nnum * SizeOf(brickpool_t));
  for i := 0 to nnum - 1 do
    nparts[i] := flooseparts[i];
  FreeMem(flooseparts);
  flooseparts := nil;
  fnumlooseparts := 0;
  frealnumlooseparts := 0;
  for i := 0 to nnum - 1 do
  begin
    cl := nparts[i].color;
    pci := db.PieceColorInfo(@nparts[i]);
    if pci = nil then
    begin
      pci := TPieceColorInfo.Create(nparts[i].part, cl);
      if db.Colors(cl).knownpieces = nil then
        db.Colors(cl).knownpieces := THashStringList.Create;
      db.Colors(cl).knownpieces.AddObject(nparts[i].part, pci);
    end;
    AddLoosePart(nparts[i].part, cl, nparts[i].num, pci);
  end;
  FreeMem(nparts, nnum * SizeOf(brickpool_t));
  fneedsReorganize := False;
end;

procedure TBrickInventory.Clear;
begin
  if flooseparts <> nil then
  begin
    FreeMem(flooseparts);
    flooseparts := nil;
  end;
  fnumlooseparts := 0;
  frealnumlooseparts := 0;

  if fsets <> nil then
  begin
    FreeMem(fsets);
    fsets := nil;
  end;
  fnumsets := 0;
  frealnumsets := 0;
end;

procedure TBrickInventory.MergeWith(const inv: TBrickInventory);
var
  i, j: integer;
  brick: brickpool_p;
  stoppnt: LongWord;
begin
  if inv = nil then
    Exit;

  for i := 0 to inv.fnumsets - 1 do
  begin
    for j := 0 to inv.fsets[i].num - 1 do
      AddSet(inv.fsets[i].setid, False);
    for j := 0 to inv.fsets[i].dismantaled - 1 do
      AddSet(inv.fsets[i].setid, True);
  end;

  brick := @inv.flooseparts[0];
  if brick <> nil then
  begin
    stoppnt := LongWord(@inv.flooseparts[inv.fnumlooseparts - 1]);
    while LongWord(brick) <= stoppnt do
    begin
      AddLoosePart(brick.part, brick.color, brick.num, brick.pci);
      inc(brick);
    end;
  end;
end;

function TBrickInventory.Clone: TBrickInventory;
begin
  Result := TBrickInventory.Create;
  Result.MergeWith(self);
end;

function TBrickInventory.numlotsbycolor(const col: integer): integer;
var
  brick: brickpool_p;
  stoppnt: LongWord;
begin
  if col = -1 then
  begin
    Result := fnumlooseparts;
    Exit;
  end;

  Result := 0;
  brick := @flooseparts[0];
  if brick <> nil then
  begin
    stoppnt := LongWord(@flooseparts[fnumlooseparts - 1]);
    while LongWord(brick) <= stoppnt do
    begin
      if brick.color = col then
        inc(Result);
      inc(brick);
    end;
  end;
end;

function TBrickInventory.numlotsbypart(const pt: string): integer;
var
  brick: brickpool_p;
  stoppnt: LongWord;
begin
  if pt = '' then
  begin
    Result := fnumlooseparts;
    Exit;
  end;

  Result := 0;
  brick := @flooseparts[0];
  if brick <> nil then
  begin
    stoppnt := LongWord(@flooseparts[fnumlooseparts - 1]);
    while LongWord(brick) <= stoppnt do
    begin
      if brick.part = pt then
        inc(Result);
      inc(brick);
    end;
  end;
end;

function TBrickInventory.numlotsbycatcolor(const col: integer; const cat: integer): integer;
var
  i: integer;
begin
  if cat = -1 then
  begin
    Result := totallooseparts;
    Exit;
  end;
  Result := 0;
  for i := 0 to fnumlooseparts - 1 do
    if flooseparts[i].color = col then
      if db.PieceInfo(@flooseparts[i]).category = cat then
        inc(Result);
end;

function TBrickInventory.numlotsbycategory(const cat: integer): integer;
var
  i: integer;
begin
  if cat = -1 then
  begin
    Result := totallooseparts;
    Exit;
  end;
  Result := 0;
  for i := 0 to fnumlooseparts - 1 do
    if db.PieceInfo(@flooseparts[i]).category = cat then
      inc(Result);
end;

function TBrickInventory.totalloosepartsbycolor(const col: integer): integer;
var
  i: integer;
begin
  if col = -1 then
  begin
    Result := totallooseparts;
    Exit;
  end;
  Result := 0;
  for i := 0 to fnumlooseparts - 1 do
    if flooseparts[i].color = col then
      inc(Result, flooseparts[i].num);
end;

function TBrickInventory.totalloosepartsbycatcolor(const col: integer; const cat: integer): integer;
var
  i: integer;
begin
  if col = -1 then
  begin
    Result := totallooseparts;
    Exit;
  end;
  Result := 0;
  for i := 0 to fnumlooseparts - 1 do
    if flooseparts[i].color = col then
      if db.PieceInfo(@flooseparts[i]).category = cat then
        inc(Result, flooseparts[i].num);
end;

function TBrickInventory.totalloosepartsbypart(const pt: string): integer;
var
  i: integer;
begin
  if pt = '' then
  begin
    Result := totallooseparts;
    Exit;
  end;
  Result := 0;
  for i := 0 to fnumlooseparts - 1 do
    if flooseparts[i].part = pt then
      inc(Result, flooseparts[i].num);
end;

function TBrickInventory.totalloosepartsbycategory(const cat: integer): integer;
var
  i: integer;
begin
  if cat = -1 then
  begin
    Result := totallooseparts;
    Exit;
  end;
  Result := 0;
  for i := 0 to fnumlooseparts - 1 do
    if db.PieceInfo(@flooseparts[i]).category = cat then
      inc(Result, flooseparts[i].num);
end;

procedure TBrickInventory.partscategorysum(const c: categorysum_p);
var
  i: integer;
  cat: integer;
begin
  ZeroMemory(c, SizeOf(categorysum_t));
  c[-1] := totallooseparts;
  for i := 0 to fnumlooseparts - 1 do
  begin
    cat := db.PieceInfo(@flooseparts[i]).category;
    if cat >= 0 then
      if cat < MAXCATEGORIES then
        inc(c[cat], flooseparts[i].num);
  end;
end;

procedure TBrickInventory.lotscategorysum(const c: categorysum_p);
var
  i: integer;
  cat: integer;
begin
  ZeroMemory(c, SizeOf(categorysum_t));
  c[-1] := totallooseparts;
  for i := 0 to fnumlooseparts - 1 do
  begin
    cat := db.PieceInfo(@flooseparts[i]).category;
    if cat >= 0 then
      if cat < MAXCATEGORIES then
        inc(c[cat]);
  end;
end;

function TBrickInventory.weightbycategory(const cat: integer): double;
var
  i: integer;
  pi: TPieceInfo;
begin
  Result := 0.0;
  if cat = -1 then
  begin
    for i := 0 to fnumlooseparts - 1 do
      Result := Result + flooseparts[i].num * db.PieceInfo(@flooseparts[i]).Weight;
    Exit;
  end;
  for i := 0 to fnumlooseparts - 1 do
  begin
    pi := db.PieceInfo(@flooseparts[i]);
    if pi.category = cat then
      Result := Result + flooseparts[i].num * pi.Weight;
  end;
end;

procedure TBrickInventory.weightcategorysum(const c: categorydouble_p);
var
  i: integer;
  cat: integer;
  pi: TPieceInfo;
  w: double;
begin
  for i := -1 to MAXCATEGORIES - 1 do
    c[i] := 0.0;
  for i := 0 to fnumlooseparts - 1 do
  begin
    pi := db.PieceInfo(@flooseparts[i]);
    w := pi.weight;
    if w > 0 then
    begin
      cat := pi.category;
      w := w * flooseparts[i].num;
      c[cat] := c[cat] + w;
      c[-1] := c[-1] + w;
    end;
  end;
end;

function TBrickInventory.weightbycatcolor(const col: integer; const cat: integer): double;
var
  i: integer;
  pi: TPieceInfo;
begin
  Result := 0.0;
  for i := 0 to fnumlooseparts - 1 do
  begin
    pi := db.PieceInfo(@flooseparts[i]);
    if (cat = -1) or (pi.category = cat) then
      if (col = -1) or (flooseparts[i].color = col) then
        Result := Result + flooseparts[i].num * pi.Weight;
  end;
end;

function TBrickInventory.weightbycolor(const col: integer): double;
var
  i: integer;
begin
  Result := 0.0;
  if col = -1 then
  begin
    for i := 0 to fnumlooseparts - 1 do
      Result := Result + flooseparts[i].num * db.PieceInfo(@flooseparts[i]).Weight;
    Exit;
  end;
  for i := 0 to fnumlooseparts - 1 do
    if flooseparts[i].color = col then
      Result := Result + flooseparts[i].num * db.PieceInfo(@flooseparts[i]).Weight;
end;


function TBrickInventory.totallooseparts: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to fnumlooseparts - 1 do
    inc(Result, flooseparts[i].num);
end;

function TBrickInventory.totalsetsbuilted: integer;
var
  i: integer;
begin
  Result := 0;
  for i := fnumsets - 1 downto 0 do
    Result := Result + fsets[i].num;
end;

function TBrickInventory.totalsetsdismantaled: integer;
var
  i: integer;
begin
  Result := 0;
  for i := fnumsets - 1 downto 0  do
    Result := Result + fsets[i].dismantaled;
end;

function TBrickInventory.GetMoldList: TStringList;
var
  i: integer;
begin
  Result := TStringList.Create;
  for i := 0 to fnumlooseparts - 1 do
    if flooseparts[i].num > 0 then
      if Result.IndexOf(flooseparts[i].part) < 0 then
        Result.Add(flooseparts[i].part)
end;

function TBrickInventory.GetDismandaledSets: TStringList;
var
  i, j: integer;
begin
  Result := TStringList.Create;
  for i := 0 to fnumsets - 1 do
    for j := 0 to fsets[i].dismantaled - 1 do
      Result.Add(fsets[i].setid)
end;

function TBrickInventory.GetHistoryEvalRec: brickevalhistory_t;
begin
  Result.Eval_nAvg := EvaluatedPartOutValue_nAvg;
  Result.Eval_nQtyAvg := EvaluatedPartOutValue_nQtyAvg;
  Result.Eval_uAvg := EvaluatedPartOutValue_uAvg;
  Result.Eval_uQtyAvg := EvaluatedPartOutValue_uQtyAvg;
  Result.time := Now;
end;

function TBrickInventory.GetHistoryStatsRec: brickstatshistory_t;
begin
  Result.Sold_nAvg := SoldPartOutValue_nAvg;
  Result.Sold_nQtyAvg := SoldPartOutValue_nQtyAvg;
  Result.Sold_uAvg := SoldPartOutValue_uAvg;
  Result.Sold_uQtyAvg := SoldPartOutValue_uQtyAvg;
  Result.Avail_nAvg := AvailablePartOutValue_nAvg;
  Result.Avail_nQtyAvg := AvailablePartOutValue_nQtyAvg;
  Result.Avail_uAvg := AvailablePartOutValue_uAvg;
  Result.Avail_uQtyAvg := AvailablePartOutValue_uQtyAvg;
  Result.nDemand := nDemand;
  Result.uDemand := uDemand;
  Result.time := Now;
end;

procedure TBrickInventory.StoreHistoryStatsRec(const fn: string; const pl: integer = 0);
var
  h: brickstatshistory_t;
  f: TFileStream;
begin
  if pl > 3 then
    Exit;

  h := GetHistoryStatsRec;

  try
    if fexists(fn) then
    begin
      f := TFileStream.Create(fn, fmOpenReadWrite or fmShareDenyWrite);
      f.Position := f.Size;
    end
    else
    begin
      ForceDirectories(ExtractFilePath(fn));
      f := TFileStream.Create(fn, fmCreate or fmShareDenyWrite);
    end;

    f.Write(h, SizeOf(h));
    f.Free;
  except
    Sleep(150);
    StoreHistoryStatsRec(fn, pl + 1);
  end;
end;

procedure TBrickInventory.StoreHistoryEvalRec(const fn: string; const pl: integer = 0);
var
  h: brickevalhistory_t;
  f: TFileStream;
begin
  if pl > 3 then
    Exit;

  h := GetHistoryEvalRec;

  try
    if fexists(fn) then
    begin
      f := TFileStream.Create(fn, fmOpenReadWrite or fmShareDenyWrite);
      f.Position := f.Size;
    end
    else
    begin
      ForceDirectories(ExtractFilePath(fn));
      f := TFileStream.Create(fn, fmCreate or fmShareDenyWrite);
    end;

    f.Write(h, SizeOf(h));
    f.Free;
  except
    Sleep(150);
    StoreHistoryEvalRec(fn, pl + 1);
  end;
end;

function TBrickInventory.GetPieceInventoryStatsRec(const piece: string; const color: integer): pieceinventoryhistory_t;
var
  st: set_t;
begin
  if color = -1 then
  begin
    GetSetInfo(piece, @st);
    Result.nnew := 0;
    Result.nused := 0;
    Result.nbuilded := st.num + LoosePartCount(piece, -1);
    Result.ndismantaled := st.dismantaled;
  end
  else
  begin
    Result.nnew := 0;
    Result.nused := LoosePartCount(piece, color);
    Result.nbuilded := 0;
    Result.ndismantaled := 0;
  end;
  Result.time := Now;
end;

procedure TBrickInventory.StorePieceInventoryStatsRec(const fn: string; const piece: string; const color: integer; const pl: integer = 0);
var
  h: pieceinventoryhistory_t;
  f: TFileStream;
begin
  if pl > 3 then
    Exit;

  h := GetPieceInventoryStatsRec(piece, color);

  try
    if fexists(fn) then
    begin
      f := TFileStream.Create(fn, fmOpenReadWrite or fmShareDenyWrite);
      f.Position := f.Size;
    end
    else
    begin
      ForceDirectories(ExtractFilePath(fn));
      f := TFileStream.Create(fn, fmCreate or fmShareDenyWrite);
    end;

    f.Write(h, SizeOf(h));
    f.Free;
  except
    Sleep(150);
    StoreHistoryStatsRec(fn, pl + 1);
  end;
end;

{$IFNDEF CRAWLER}
procedure TBrickInventory.SortPieces;

  procedure QuickSortN(const A: brickpool_pa; iLo, iHi: Integer);
  var
     Lo, Hi: integer;
     Pivot: string;
     T: brickpool_t;
  begin
    Lo := iLo;
    Hi := iHi;
    Pivot := db.PieceDesc(A[(Lo + Hi) div 2].part);
    repeat
      while db.PieceDesc(A[Lo].part) < Pivot do Inc(Lo);
      while db.PieceDesc(A[Hi].part) > Pivot do Dec(Hi);
      if Lo <= Hi then
      begin
        T := A[Lo];
        A[Lo] := A[Hi];
        A[Hi] := T;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then QuickSortN(A, iLo, Hi);
    if Lo < iHi then QuickSortN(A, Lo, iHi);
  end;

begin
  Reorganize;
  if fnumlooseparts > 0 then
    QuickSortN(flooseparts, 0, fnumlooseparts - 1);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
procedure TBrickInventory.SortPiecesByPartNumber;

  procedure QuickSortPN(const A: brickpool_pa; iLo, iHi: Integer);
  var
     Lo, Hi: integer;
     Pivot: string;
     T: brickpool_t;
  begin
    Lo := iLo;
    Hi := iHi;
    Pivot := A[(Lo + Hi) div 2].part;
    repeat
      while A[Lo].part < Pivot do Inc(Lo);
      while A[Hi].part > Pivot do Dec(Hi);
      if Lo <= Hi then
      begin
        T := A[Lo];
        A[Lo] := A[Hi];
        A[Hi] := T;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then QuickSortPN(A, iLo, Hi);
    if Lo < iHi then QuickSortPN(A, Lo, iHi);
  end;

begin
  Reorganize;
  if fnumlooseparts > 0 then
    QuickSortPN(flooseparts, 0, fnumlooseparts - 1);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
procedure TBrickInventory.SortPiecesByPriceNew;

  function sortvalue_price_new(const b: brickpool_p): double;
  var
    pci: TPieceColorInfo;
  begin
    pci := db.PieceColorInfo(b);
    if pci <> nil then
      Result := pci.EvaluatePriceNew
    else
      Result := 0.0;
  end;

  procedure QuickSortPN(const A: brickpool_pa; iLo, iHi: Integer);
  var
     Lo, Hi: integer;
     Pivot: double;
     T: brickpool_t;
  begin
    Lo := iLo;
    Hi := iHi;
    Pivot := sortvalue_price_new(@A[(Lo + Hi) div 2]);
    repeat
      while sortvalue_price_new(@A[Lo]) < Pivot do Inc(Lo);
      while sortvalue_price_new(@A[Hi]) > Pivot do Dec(Hi);
      if Lo <= Hi then
      begin
        T := A[Lo];
        A[Lo] := A[Hi];
        A[Hi] := T;
        Inc(Lo) ;
        Dec(Hi) ;
      end;
    until Lo > Hi;
    if Hi > iLo then QuickSortPN(A, iLo, Hi);
    if Lo < iHi then QuickSortPN(A, Lo, iHi);
  end;

begin
  Reorganize;
  if fnumlooseparts > 0 then
    QuickSortPN(flooseparts, 0, fnumlooseparts - 1);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
procedure TBrickInventory.SortPiecesByPriceUsed;

  function sortvalue_price_used(const b: brickpool_p): double;
  var
    pci: TPieceColorInfo;
  begin
    pci := db.PieceColorInfo(b);
    if pci <> nil then
      Result := pci.EvaluatePriceUsed
    else
      Result := 0.0;
  end;

  procedure QuickSortPU(const A: brickpool_pa; iLo, iHi: Integer);
  var
     Lo, Hi: integer;
     Pivot: double;
     T: brickpool_t;
  begin
    Lo := iLo;
    Hi := iHi;
    Pivot := sortvalue_price_used(@A[(Lo + Hi) div 2]);
    repeat
      while sortvalue_price_used(@A[Lo]) < Pivot do Inc(Lo);
      while sortvalue_price_used(@A[Hi]) > Pivot do Dec(Hi);
      if Lo <= Hi then
      begin
        T := A[Lo];
        A[Lo] := A[Hi];
        A[Hi] := T;
        Inc(Lo) ;
        Dec(Hi) ;
      end;
    until Lo > Hi;
    if Hi > iLo then QuickSortPU(A, iLo, Hi);
    if Lo < iHi then QuickSortPU(A, Lo, iHi);
  end;

begin
  Reorganize;
  if fnumlooseparts > 0 then
    QuickSortPU(flooseparts, 0, fnumlooseparts - 1);
end;
{$ENDIF}

procedure TBrickInventory.AddLoosePartFast(const part: string; color: integer;
  const num: integer; const pci: TObject = nil);
var
  bp: brickpool_p;
  p: integer;
  i: integer;
  h: integer;
begin
  if color = -1 then
  begin
    p := Pos('-', part);
    if (p > 1) and (p < Length(part)) then
    begin
      if num > 0 then
      begin
        for i := 0 to num - 1 do
          AddSet(part, False);
      end
      else if num < 0 then
      begin
        for i := 0 to -num - 1 do
          RemoveSet(part, False);
      end;
      Exit;
    end;
  end;

  if num > 0 then
  begin
    _growparts;
    bp := @flooseparts[fnumlooseparts];
    bp.part := part;
    bp.color := color;
    bp.pci := pci;
    h := MkBHash(part, color);
    bhash[h].position := fnumlooseparts;
    if chash <> nil then
    begin
      h := MkCHash(part, color);
      chash[h].position := fnumlooseparts;
    end;
    bp.num := num;
    Inc(fnumlooseparts);
    fneedsReorganize := True;
    fupdatetime := 0;
  end
  else if num < 0 then
    RemoveLoosePart(part, color, -num);
end;

procedure TBrickInventory.AddLoosePart(const part: string; color: integer;
  const num: integer; const pci: TObject = nil);
var
  i: Integer;
  p: integer;
  h: integer;
  hl: LongWord;
  h2: LongWord;
begin
  if color = -1 then
  begin
    p := Pos('-', part);
    if (p > 1) and (p < Length(part)) then
    begin
      if num > 0 then
      begin
        for i := 0 to num - 1 do
          AddSet(part, False);
      end
      else if num < 0 then
      begin
        for i := 0 to num - 1 do
          RemoveSet(part, False);
      end;
      Exit;
    end;
  end;

  if num <= 0 then
  begin
    if num < 0 then
      RemoveLoosePart(part, color, -num);
    Exit;
  end;

  if chash <> nil then
  begin
    h2 := MkCHash(part, color);
    h := chash[h2].position;
    if h >= 0 then
      if h < fnumlooseparts then
        if flooseparts[h].color = color then
          if flooseparts[h].part = part then
          begin
            flooseparts[h].num := flooseparts[h].num + num;
            if flooseparts[h].pci = nil then
              flooseparts[h].pci := pci;
            fupdatetime := 0;
            Exit;
          end;
  end
  else
    h2 := 0;

  hl := MkBHash(part, color);
  h := bhash[hl].position;
  if h < fnumlooseparts then
    if flooseparts[h].color = color then
      if flooseparts[h].part = part then
      begin
        flooseparts[h].num := flooseparts[h].num + num;
        fupdatetime := 0;
        Exit;
      end;

  for i := 0 to fnumlooseparts - 1 do
    if  flooseparts[i].color = color then
      if flooseparts[i].part = part then
      begin
        flooseparts[i].num := flooseparts[i].num + num;
        fupdatetime := 0;
        Exit;
      end;

  _growparts;
  flooseparts[fnumlooseparts].part := part;
  flooseparts[fnumlooseparts].color := color;
  flooseparts[fnumlooseparts].num := num;
  flooseparts[fnumlooseparts].pci := pci;
  bhash[hl].position := fnumlooseparts;
  if chash <> nil then
    chash[h2].position := fnumlooseparts;
  Inc(fnumlooseparts);
  fupdatetime := 0;
end;

function TBrickInventory.RemoveLoosePart(const part: string; color: integer; num: integer): boolean;
var
  i, h, h2: integer;
  p: integer;
begin
  if color = -1 then
  begin
    p := Pos('-', part);
    if (p > 1) and (p < Length(part)) then
    begin
      if num > 0 then
      begin
        for i := 0 to num - 1 do
          RemoveSet(part, False);
      end
      else if num < 0 then
      begin
        for i := 0 to -num - 1 do
          AddSet(part, False);
      end;
      Result := True;
      Exit;
    end;
  end;

  if num <= 0 then
  begin
    if num < 0 then
      AddLoosePart(part, color, num);
    Result := True;
    Exit;
  end;

  if chash <> nil then
  begin
    h2 := chash[MkCHash(part, color)].position;
    if h2 >= 0 then
      if h2 < fnumlooseparts then
        if flooseparts[h2].color = color then
          if flooseparts[h2].part = part then
          begin
            if flooseparts[h2].num >= num then
            begin
              flooseparts[h2].num := flooseparts[h2].num - num;
              Result := True;
              fneedsReorganize := flooseparts[h2].num = 0;
              fupdatetime := 0;
            end
            else
              Result := False;
            Exit;
          end;
  end;

  h := bhash[MkBHash(part, color)].position;
  if h < fnumlooseparts then
    if flooseparts[h].color = color then
      if flooseparts[h].part = part then
      begin
        if flooseparts[h].num >= num then
        begin
          flooseparts[h].num := flooseparts[h].num - num;
          Result := True;
          fneedsReorganize := flooseparts[h].num = 0;
          fupdatetime := 0;
        end
        else
          Result := False;
        Exit;
      end;

  for i := 0 to fnumlooseparts - 1 do
    if flooseparts[i].color = color then
      if flooseparts[i].part = part then
      begin
        if flooseparts[i].num >= num then
        begin
          flooseparts[i].num := flooseparts[i].num - num;
          Result := True;
          fneedsReorganize := flooseparts[i].num = 0;
          fupdatetime := 0;
        end
        else
          Result := False;
        Exit;
      end;
  Result := False;
end;

function TBrickInventory.LoosePartCount(const part: string; color: integer): integer;
var
  h, h2: integer;
  brick: brickpool_p;
  stoppnt: LongWord;
begin
  if not fneedsReorganize then
  begin
    // Check Extended hash table first
    if chash <> nil then
    begin
      h2 := chash[MkCHash(part, color)].position;
      if h2 >= 0 then
        if h2 < fnumlooseparts then
          if flooseparts[h2].color = color then
            if flooseparts[h2].part = part then
            begin
              Result := flooseparts[h2].num;
              Exit;
            end;
    end;

    h := bhash[MkBHash(part, color)].position;
    if h < fnumlooseparts then
      if flooseparts[h].color = color then
        if flooseparts[h].part = part then
        begin
          Result := flooseparts[h].num;
          Exit;
        end;
  end;


  Result := 0;
  brick := @flooseparts[0];
  if brick <> nil then
  begin
    stoppnt := LongWord(@flooseparts[fnumlooseparts - 1]);
    while LongWord(brick) <= stoppnt do
    begin
      if brick.color = color then
        if brick.part = part then
        begin
          Result := Result + brick.num; // SOS maybe Exit here!!
          if not fneedsReorganize then
            Exit;
        end;
      inc(brick);
    end;
  end;
end;

procedure TBrickInventory.AddSet(const setid: string; dismantaled: boolean);
var
  i: Integer;
begin
  for i := fnumsets - 1 downto 0  do
    if  fsets[i].setid = setid then
    begin
      if dismantaled then
        Inc(fsets[i].dismantaled)
      else
        Inc(fsets[i].num);
      fupdatetime := 0;
      Exit;
    end;

  _growsets;
  fsets[fnumsets].setid := setid;
  if dismantaled then
  begin
    fsets[fnumsets].num := 0;
    fsets[fnumsets].dismantaled := 1;
  end
  else
  begin
    fsets[fnumsets].num := 1;
    fsets[fnumsets].dismantaled := 0;
  end;
  Inc(fnumsets);
  fupdatetime := 0;
end;

procedure TBrickInventory.GetSetInfo(const setid: string; const s: set_p);
var
  i: integer;
begin
  s.setid := setid;
  s.num := 0;
  s.dismantaled := 0;
  for i := 0 to fnumsets - 1 do
    if fsets[i].setid = setid then
    begin
      s.num := s.num + fsets[i].num;
      s.dismantaled := s.dismantaled + fsets[i].dismantaled;
    end;
end;

function TBrickInventory.RemoveSet(const setid: string; dismantaled: boolean): boolean;
var
  i{, j}: integer;
  inv: TBrickInventory;
begin
  Result := True;
  for i := 0 to fnumsets - 1 do
    if  fsets[i].setid = setid then
    begin
      if dismantaled then
      begin
        if fsets[i].dismantaled = 0 then
        begin
          Result := False;
          Exit;
        end;
        dec(fsets[i].dismantaled);
        inv := db.GetSetInventory(setid);
        if inv <> nil then
        begin
{ jval: SOS -> Do not remove pieces from inventory!
          for j := 0 to inv.numlooseparts - 1 do
            RemoveLoosePart(inv.flooseparts[j].part, inv.flooseparts[j].color, inv.flooseparts[j].num);}
        end;
      end
      else
      begin
        if fsets[i].num = 0 then
        begin
          Result := False;
          Exit;
        end;
        dec(fsets[i].num);
      end;
      fupdatetime := 0;
      Exit;
    end;
  Result := False;
end;

function TBrickInventory.DismandalSet(const setid: string): boolean;
var
  i, j, k: integer;
  inv: TBrickInventory;
begin
  Result := True;
  for i := 0 to fnumsets - 1 do
    if  fsets[i].setid = setid then
    begin
      if fsets[i].num = 0 then
      begin
        Result := False;
        Exit;
      end;
      dec(fsets[i].num);
      inc(fsets[i].dismantaled);
      inv := db.GetSetInventory(setid);
      if inv <> nil then
      begin
        for j := 0 to inv.numlooseparts - 1 do
          AddLoosePart(inv.flooseparts[j].part, inv.flooseparts[j].color, inv.flooseparts[j].num, inv.flooseparts[j].pci);
        for j := 0 to inv.fnumsets - 1 do
          for k := 0 to inv.fsets[j].num - 1 do
            AddSet(inv.fsets[j].setid, False);
      end;
      Exit;
    end;
  Result := False;
end;

function TBrickInventory.DismandalAllSets: boolean;
var
  i, j: integer;
  ret: boolean;
begin
  Result := True;
  for i := 0 to fnumsets - 1 do
    for j := 0 to fsets[i].num - 1 do
    begin
      ret := DismandalSet(fsets[i].setid);
      Result := Result and ret;
    end;
end;

function TBrickInventory.BuildSet(const setid: string): boolean;
var
  i: integer;
  inv: TBrickInventory;
begin
  Result := CanBuildSet(setid);
  if not Result then
    Exit;

  inv := db.GetSetInventory(setid);
  if inv <> nil then
  begin
    AddSet(setid, False);
    for i := 0 to inv.numlooseparts - 1 do
      RemoveLoosePart(inv.flooseparts[i].part, inv.flooseparts[i].color, inv.flooseparts[i].num);
    Reorganize;

    for i := 0 to fnumsets - 1 do
      if  fsets[i].setid = setid then
        if fsets[i].dismantaled > 0 then
        begin
          dec(fsets[i].dismantaled);
          break;
        end;
  end;
end;

function TBrickInventory.BuildAllSets: boolean;
var
  i, j: integer;
  ret: boolean;
begin
  Result := True;
  for i := 0 to fnumsets - 1 do
    for j := 0 to fsets[i].dismantaled - 1 do
    begin
      ret := BuildSet(fsets[i].setid);
      Result := Result and ret;
    end;
end;

function TBrickInventory.CanBuildSet(const setid: string): boolean;
begin
  Result := MissingToBuildSet(setid) = 0;
end;

procedure TBrickInventory.LegacyColorMerge;
var
  i: integer;
begin
  for i := 0 to fnumlooseparts - 1 do
  begin
    if flooseparts[i].color = 6 then
      flooseparts[i].color := 70
    else if flooseparts[i].color = 7 then
      flooseparts[i].color := 71
    else if flooseparts[i].color = 8 then
      flooseparts[i].color := 72;
  end;
  DoReorganize;
end;

function TBrickInventory.MissingToBuildSet(const setid: string): integer;
begin
  Result := MissingToBuildInventory(db.GetSetInventory(setid));
end;

function TBrickInventory.MissingToBuildSetLegacyIgnore(const setid: string): integer;
begin
  Result := MissingToBuildInventoryLegacyIgnore(db.GetSetInventory(setid));
end;

function TBrickInventory.Minifigures: TBrickInventory;
var
  i: integer;
  pci: TPieceColorInfo;
  cl: integer;
begin
  Reorganize;
  Result := TBrickInventory.Create;
  if Result <> nil then
  begin
    for i := 0 to fnumlooseparts - 1 do
    begin
      cl := flooseparts[i].color;
      if (cl = -1) or (cl = 89) or (cl = 9999) then
      begin
        pci := db.PieceColorInfo(@flooseparts[i]);
        if pci <> nil then
          if pci.sparttype = 'M' then
            Result.AddLoosePart(flooseparts[i].part, cl, flooseparts[i].num, pci);
      end;
    end;
    for i := 0 to fnumsets - 1 do
    begin
      if fsets[i].num > 0 then
      begin
        pci := db.PieceColorInfo(fsets[i].setid, -1);
        if pci <> nil then
          if pci.sparttype = 'M' then
            Result.AddLoosePart(fsets[i].setid, -1, fsets[i].num, pci);
      end;
    end;
  end;
end;

function TBrickInventory.InventoryForMissingToBuildSet(
  const setid: string; const nsets: integer = 1): TBrickInventory;
var
  inv: TBrickInventory;
  j, n: integer;
begin
  Reorganize;
  Result := TBrickInventory.Create;
  inv := db.GetSetInventory(setid);
  if inv <> nil then
  begin
    for j := 0 to inv.numlooseparts - 1 do
    begin
      n := LoosePartCount(inv.flooseparts[j].part, inv.flooseparts[j].color);
      if n < nsets * inv.flooseparts[j].num then
        Result.AddLoosePart(inv.flooseparts[j].part, inv.flooseparts[j].color, nsets * inv.flooseparts[j].num - n, inv.flooseparts[j].pci);
    end;
  end;
end;

// Ignore color variations
// 6 -> 70 (old brown and reddish brown)
// 7 -> 71 (old light gray and light bluish gray)
// 8 -> 72 (old dark gray and dark bluish gray)
function TBrickInventory.InventoryForMissingToBuildSetLegacyIgnore(const setid: string; const nsets: integer = 1): TBrickInventory;
var
  cloneinv, setinv: TBrickInventory;
  n, j: integer;
begin
  Reorganize;
  cloneinv := Clone;
  cloneinv.LegacyColorMerge;
  Result := TBrickInventory.Create;
  setinv := db.GetSetInventory(setid).Clone;
  if setinv <> nil then
  begin
    setinv.LegacyColorMerge;
    for j := 0 to setinv.numlooseparts - 1 do
    begin
      n := cloneinv.LoosePartCount(setinv.flooseparts[j].part, setinv.flooseparts[j].color);
      if n < nsets * setinv.flooseparts[j].num then
        Result.AddLoosePart(setinv.flooseparts[j].part, setinv.flooseparts[j].color, nsets * setinv.flooseparts[j].num - n, setinv.flooseparts[j].pci);
    end;
    setinv.Free;
  end;
  cloneinv.Free;
end;

{$IFNDEF CRAWLER}
function TBrickInventory.InventoryForExpensiveLotsNew(const nlots: integer = 1): TBrickInventory;
var
  allocsize1: integer;
  sortpool: sortbrickpool_pa;
  sp: sortbrickpool_p;
  i, idx: integer;
  pci: TPieceColorInfo;
begin
  Reorganize;
  allocsize1 := fnumlooseparts * SizeOf(sortbrickpool_t);
  sortpool := malloc(allocsize1);
  for i := 0 to fnumlooseparts - 1 do
  begin
    sortpool[i].part := flooseparts[i].part;
    sortpool[i].color := flooseparts[i].color;
    sortpool[i].num := flooseparts[i].num;
//    pci := db.PieceColorInfo(sortpool[i].part, sortpool[i].color);
    pci := db.PieceColorInfo(@flooseparts[i]);
    sortpool[i].pci := pci;
    if pci <> nil then
      sortpool[i].sortvalue := pci.EvaluatePriceNew * sortpool[i].num
    else
      sortpool[i].sortvalue := 0.0;
  end;

  QSortBrickPool(sortpool, fnumlooseparts);

  Result := TBrickInventory.Create;

  idx := fnumlooseparts;
  for i := 0 to nlots - 1 do
  begin
    dec(idx);
    if idx < 0 then
      Break;
    sp := @sortpool[idx];
    Result.AddLoosePart(sp.part, sp.color, sp.num, sp.pci);
  end;

  memfree(pointer(sortpool), allocsize1);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TBrickInventory.InventoryForExpensiveLotsUsed(const nlots: integer = 1): TBrickInventory;
var
  allocsize1: integer;
  sortpool: sortbrickpool_pa;
  sp: sortbrickpool_p;
  i, idx: integer;
  pci: TPieceColorInfo;
begin
  Reorganize;
  allocsize1 := fnumlooseparts * SizeOf(sortbrickpool_t);
  sortpool := malloc(allocsize1);
  for i := 0 to fnumlooseparts - 1 do
  begin
    sortpool[i].part := flooseparts[i].part;
    sortpool[i].color := flooseparts[i].color;
    sortpool[i].num := flooseparts[i].num;
    pci := db.PieceColorInfo(@flooseparts[i]);
    sortpool[i].pci := pci;
    if pci <> nil then
      sortpool[i].sortvalue := pci.EvaluatePriceUsed * sortpool[i].num
    else
      sortpool[i].sortvalue := 0.0;
  end;

  QSortBrickPool(sortpool, fnumlooseparts);

  Result := TBrickInventory.Create;

  idx := fnumlooseparts;
  for i := 0 to nlots - 1 do
  begin
    dec(idx);
    if idx < 0 then
      Break;
    sp := @sortpool[idx];
    Result.AddLoosePart(sp.part, sp.color, sp.num, sp.pci);
  end;

  memfree(pointer(sortpool), allocsize1);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TBrickInventory.InventoryForExpensivePartsNew(const nlots: integer = 1): TBrickInventory;
var
  allocsize1: integer;
  sortpool: sortbrickpool_pa;
  sp: sortbrickpool_p;
  i, idx: integer;
  pci: TPieceColorInfo;
begin
  Reorganize;
  allocsize1 := fnumlooseparts * SizeOf(sortbrickpool_t);
  sortpool := malloc(allocsize1);
  for i := 0 to fnumlooseparts - 1 do
  begin
    sortpool[i].part := flooseparts[i].part;
    sortpool[i].color := flooseparts[i].color;
    sortpool[i].num := flooseparts[i].num;
    pci := db.PieceColorInfo(@flooseparts[i]);
    sortpool[i].pci := pci;
//    pci := db.PieceColorInfo(sortpool[i].part, sortpool[i].color);
    if pci <> nil then
      sortpool[i].sortvalue := pci.EvaluatePriceNew
    else
      sortpool[i].sortvalue := 0.0;
  end;

  QSortBrickPool(sortpool, fnumlooseparts);

  Result := TBrickInventory.Create;

  idx := fnumlooseparts;
  for i := 0 to nlots - 1 do
  begin
    dec(idx);
    if idx < 0 then
      Break;
    sp := @sortpool[idx];
    Result.AddLoosePart(sp.part, sp.color, sp.num, sp.pci);
  end;

  memfree(pointer(sortpool), allocsize1);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TBrickInventory.InventoryForExpensivePartsUsed(const nlots: integer = 1): TBrickInventory;
var
  allocsize1: integer;
  sortpool: sortbrickpool_pa;
  sp: sortbrickpool_p;
  i, idx: integer;
  pci: TPieceColorInfo;
begin
  Reorganize;
  allocsize1 := fnumlooseparts * SizeOf(sortbrickpool_t);
  sortpool := malloc(allocsize1);
  for i := 0 to fnumlooseparts - 1 do
  begin
    sortpool[i].part := flooseparts[i].part;
    sortpool[i].color := flooseparts[i].color;
    sortpool[i].num := flooseparts[i].num;
//    pci := db.PieceColorInfo(sortpool[i].part, sortpool[i].color);
    pci := db.PieceColorInfo(@flooseparts[i]);
    sortpool[i].pci := pci;
    if pci <> nil then
      sortpool[i].sortvalue := pci.EvaluatePriceUsed
    else
      sortpool[i].sortvalue := 0.0;
  end;

  QSortBrickPool(sortpool, fnumlooseparts);

  Result := TBrickInventory.Create;

  idx := fnumlooseparts;
  for i := 0 to nlots - 1 do
  begin
    dec(idx);
    if idx < 0 then
      Break;
    sp := @sortpool[idx];
    Result.AddLoosePart(sp.part, sp.color, sp.num, sp.pci);
  end;

  memfree(pointer(sortpool), allocsize1);
end;
{$ENDIF}

function TBrickInventory.InventoryForBigLots(const nitems: integer = 1): TBrickInventory;
var
  i: integer;
begin
  Reorganize;
  Result := TBrickInventory.Create;
  for i := 0 to fnumlooseparts - 1 do
    if flooseparts[i].num >= nitems then
      Result.AddLoosePart(flooseparts[i].part, flooseparts[i].color, flooseparts[i].num, flooseparts[i].pci);
end;

function TBrickInventory.CanBuildInventory(const inv: TBrickInventory): boolean;
var
  j, n: integer;
begin
  Reorganize;
  if inv <> nil then
  begin
    for j := 0 to inv.numlooseparts - 1 do
    begin
      n := LoosePartCount(inv.flooseparts[j].part, inv.flooseparts[j].color);
      if n < inv.flooseparts[j].num then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
  Result := True;
end;

function TBrickInventory.CanBuildInventoryLegacyIgnore(const inv: TBrickInventory): boolean;
begin
  Result := MissingToBuildInventoryLegacyIgnore(inv) = 0;
end;

function TBrickInventory.MissingToBuildInventory(const inv: TBrickInventory): integer;
var
  j, n: integer;
begin
  Reorganize;
  Result := 0;
  if inv <> nil then
  begin
    for j := 0 to inv.numlooseparts - 1 do
    begin
      n := LoosePartCount(inv.flooseparts[j].part, inv.flooseparts[j].color);
      if n < inv.flooseparts[j].num then
        Result := Result + (inv.flooseparts[j].num - n);
    end;
  end;
end;

function TBrickInventory.MissingToBuildInventoryLegacyIgnore(const inv: TBrickInventory): integer;
var
  clone1, clone2: TBrickInventory;
begin
  Reorganize;
  clone1 := Clone;
  clone1.LegacyColorMerge;
  clone2 := inv.Clone;
  clone2.LegacyColorMerge;
  Result := clone1.MissingToBuildInventory(clone2);
  clone1.Free;
  clone2.Free;
end;

function TBrickInventory.InventoryForMissingToBuildInventory(const inv: TBrickInventory): TBrickInventory;
var
  j, n: integer;
begin
  Reorganize;
  Result := TBrickInventory.Create;
  if inv <> nil then
  begin
    for j := 0 to inv.numlooseparts - 1 do
    begin
      n := LoosePartCount(inv.flooseparts[j].part, inv.flooseparts[j].color);
      if n < inv.flooseparts[j].num then
        Result.AddLoosePart(inv.flooseparts[j].part, inv.flooseparts[j].color, inv.flooseparts[j].num - n, inv.flooseparts[j].pci);
    end;
  end;
end;

function TBrickInventory.InventoryForMissingToBuildInventoryLegacyIgnore(const inv: TBrickInventory): TBrickInventory;
var
  clone1, clone2: TBrickInventory;
begin
  Reorganize;
  clone1 := Clone;
  clone1.LegacyColorMerge;
  clone2 := inv.Clone;
  clone2.LegacyColorMerge;
  Result := clone1.InventoryForMissingToBuildInventory(clone2);
  clone1.Free;
  clone2.Free;
end;

function TBrickInventory.LoosePartsWeight: double;
var
  i: integer;
begin
  Result := 0.0;
  for i := 0 to numlooseparts - 1 do
    Result := Result + db.PieceInfo(@flooseparts[i]).weight * flooseparts[i].num;
end;

procedure TBrickInventory.UpdateCostValues;
begin
  if Now() - fupdatetime < fupdatetimeout then
    Exit;
  DoUpdateCostValues;
end;

procedure TBrickInventory.DoUpdateCostValues;
var
  i: integer;
  brick: brickpool_p;
  pg: priceguide_t;
  av: availability_t;
  miss: array[0..11] of integer;
  tot: integer;
  inv: TBrickInventory;
  pci: TPieceColorInfo;
  price: double;
begin
  fupdatetime := Now();

  ZeroMemory(@fSoldPartOutValue_nAvg, SizeOf(partout_t));
  ZeroMemory(@fSoldPartOutValue_nQtyAvg, SizeOf(partout_t));
  ZeroMemory(@fSoldPartOutValue_uAvg, SizeOf(partout_t));
  ZeroMemory(@fSoldPartOutValue_uQtyAvg, SizeOf(partout_t));
  ZeroMemory(@fAvailablePartOutValue_nAvg, SizeOf(partout_t));
  ZeroMemory(@fAvailablePartOutValue_nQtyAvg, SizeOf(partout_t));
  ZeroMemory(@fAvailablePartOutValue_uAvg, SizeOf(partout_t));
  ZeroMemory(@fAvailablePartOutValue_uQtyAvg, SizeOf(partout_t));
  ZeroMemory(@fEvaluatedPartOutValue_nAvg, SizeOf(partout_t));
  ZeroMemory(@fEvaluatedPartOutValue_nQtyAvg, SizeOf(partout_t));
  ZeroMemory(@fEvaluatedPartOutValue_uAvg, SizeOf(partout_t));
  ZeroMemory(@fEvaluatedPartOutValue_uQtyAvg, SizeOf(partout_t));
  if (fnumlooseparts = 0) and (totalsetsbuilted = 0) then
    Exit;

  if totalsetsbuilted > 0 then
  begin
    inv := Clone;
    try
      inv.DismandalAllSets;
      inv.DoUpdateCostValues;
      fSoldPartOutValue_nAvg := inv.fSoldPartOutValue_nAvg;
      fSoldPartOutValue_nQtyAvg := inv.fSoldPartOutValue_nQtyAvg;
      fSoldPartOutValue_uAvg := inv.fSoldPartOutValue_uAvg;
      fSoldPartOutValue_uQtyAvg := inv.fSoldPartOutValue_uQtyAvg;
      fAvailablePartOutValue_nAvg := inv.fAvailablePartOutValue_nAvg;
      fAvailablePartOutValue_nQtyAvg := inv.fAvailablePartOutValue_nQtyAvg;
      fAvailablePartOutValue_uAvg := inv.fAvailablePartOutValue_uAvg;
      fAvailablePartOutValue_uQtyAvg := inv.fAvailablePartOutValue_uQtyAvg;
      fEvaluatedPartOutValue_nAvg := inv.fEvaluatedPartOutValue_nAvg;
      fEvaluatedPartOutValue_nQtyAvg := inv.fEvaluatedPartOutValue_nQtyAvg;
      fEvaluatedPartOutValue_uAvg := inv.fEvaluatedPartOutValue_uAvg;
      fEvaluatedPartOutValue_uQtyAvg := inv.fEvaluatedPartOutValue_uQtyAvg;
    finally
      inv.Free;
    end;
    Exit;
  end;

  ZeroMemory(@miss, SizeOf(miss));
  tot := 0;

  brick := @flooseparts[0];
  for i := 0 to fnumlooseparts - 1 do
  begin
//    pg := db.Priceguide(brick.part, brick.color);
    pg := db.Priceguide(brick);
    if pg.nTimesSold = 0 then
    begin
      if pg.uTimesSold = 0 then
        db.CrawlerPriorityPart(brick.part, brick.color);
      Inc(miss[0], brick.num);
      Inc(miss[1], brick.num);
    end
    else
    begin
      fSoldPartOutValue_nAvg.value := fSoldPartOutValue_nAvg.value + pg.nAvgPrice * brick.num;
      fSoldPartOutValue_nQtyAvg.value := fSoldPartOutValue_nQtyAvg.value + pg.nQtyAvgPrice * brick.num;
    end;

    if pg.uTimesSold = 0 then
    begin
      Inc(miss[2], brick.num);
      Inc(miss[3], brick.num);
    end
    else
    begin
      fSoldPartOutValue_uAvg.value := fSoldPartOutValue_uAvg.value + pg.uAvgPrice * brick.num;
      fSoldPartOutValue_uQtyAvg.value := fSoldPartOutValue_uQtyAvg.value + pg.uQtyAvgPrice * brick.num;
    end;

//    av := db.Availability(brick.part, brick.color);
    av := db.Availability(brick);
    if av.nTotalLots = 0 then
    begin
      Inc(miss[4], brick.num);
      Inc(miss[5], brick.num);
    end
    else
    begin
      fAvailablePartOutValue_nAvg.value := fAvailablePartOutValue_nAvg.value + av.nAvgPrice * brick.num;
      fAvailablePartOutValue_nQtyAvg.value := fAvailablePartOutValue_nQtyAvg.value + av.nQtyAvgPrice * brick.num;
    end;

    if av.uTotalLots = 0 then
    begin
      Inc(miss[6], brick.num);
      Inc(miss[7], brick.num);
    end
    else
    begin
      fAvailablePartOutValue_uAvg.value := fAvailablePartOutValue_uAvg.value + av.uAvgPrice * brick.num;
      fAvailablePartOutValue_uQtyAvg.value := fAvailablePartOutValue_uQtyAvg.value + av.uQtyAvgPrice * brick.num;
    end;

//    pci := db.PieceColorInfo(brick.part, brick.color);
    pci := db.PieceColorInfo(brick);
    if pci = nil then
    begin
      Inc(miss[8], brick.num);
      Inc(miss[9], brick.num);
      Inc(miss[10], brick.num);
      Inc(miss[11], brick.num);
    end
    else
    begin
      price := pci.EvaluatePriceNewAvg;
      if price > 0 then
        fEvaluatedPartOutValue_nAvg.value := fEvaluatedPartOutValue_nAvg.value + price * brick.num
      else
        Inc(miss[8], brick.num);

      price := pci.EvaluatePriceNew;
      if price > 0 then
        fEvaluatedPartOutValue_nQtyAvg.value := fEvaluatedPartOutValue_nQtyAvg.value + price * brick.num
      else
        Inc(miss[9], brick.num);

      price := pci.EvaluatePriceUsedAvg;
      if price > 0 then
        fEvaluatedPartOutValue_uAvg.value := fEvaluatedPartOutValue_uAvg.value + price * brick.num
      else
        Inc(miss[10], brick.num);

      price := pci.EvaluatePriceUsed;
      if price > 0 then
        fEvaluatedPartOutValue_uQtyAvg.value := fEvaluatedPartOutValue_uQtyAvg.value + price * brick.num
      else
        Inc(miss[11], brick.num);
    end;
    Inc(tot, brick.num);
    Inc(brick);
  end;

  if tot = 0 then
    Exit;

  fSoldPartOutValue_nAvg.percentage := 1.0 - miss[0] / tot;
  fSoldPartOutValue_nQtyAvg.percentage := 1.0 - miss[1] / tot;
  fSoldPartOutValue_uAvg.percentage := 1.0 - miss[2] / tot;
  fSoldPartOutValue_uQtyAvg.percentage := 1.0 - miss[3] / tot;

  fAvailablePartOutValue_nAvg.percentage := 1.0 - miss[4] / tot;
  fAvailablePartOutValue_nQtyAvg.percentage := 1.0 - miss[5] / tot;
  fAvailablePartOutValue_uAvg.percentage := 1.0 - miss[6] / tot;
  fAvailablePartOutValue_uQtyAvg.percentage := 1.0 - miss[7] / tot;

  fEvaluatedPartOutValue_nAvg.percentage := 1.0 - miss[8] / tot;
  fEvaluatedPartOutValue_nQtyAvg.percentage := 1.0 - miss[9] / tot;
  fEvaluatedPartOutValue_uAvg.percentage := 1.0 - miss[10] / tot;
  fEvaluatedPartOutValue_uQtyAvg.percentage := 1.0 - miss[11] / tot;
end;

function TBrickInventory.SoldPartOutValue_nAvg: partout_t;
var
  i: integer;
  brick: brickpool_p;
  pg: priceguide_t;
  tot, miss: integer;
  inv: TBrickInventory;
begin
  FillChar(Result, SizeOf(partout_t), 0);
  if (fnumlooseparts = 0) and (totalsetsbuilted = 0) then
    Exit;

  if Now() - fupdatetime < fupdatetimeout then
  begin
    Result := fSoldPartOutValue_nAvg;
    Exit;
  end;

  if totalsetsbuilted > 0 then
  begin
    inv := Clone;
    inv.DismandalAllSets;
    Result := inv.SoldPartOutValue_nAvg;
    inv.Free;
    Exit;
  end;

  miss := 0;
  tot := 0;
  brick := @flooseparts[0];
  for i := 0 to fnumlooseparts - 1 do
  begin
//    pg := db.Priceguide(brick.part, brick.color);
    pg := db.Priceguide(brick);
    if pg.nTimesSold = 0 then
    begin
      if pg.uTimesSold = 0 then
        db.CrawlerPriorityPart(brick.part, brick.color);
      Inc(miss, brick.num);
    end
    else
      Result.value := Result.value + pg.nAvgPrice * brick.num;
    Inc(tot, brick.num);
    Inc(brick);
  end;
  if tot = 0 then
    Exit;
  Result.percentage := 1.0 - miss / tot;
end;

function TBrickInventory.SoldPartOutValue_nQtyAvg: partout_t;
var
  i: integer;
  brick: brickpool_p;
  pg: priceguide_t;
  tot, miss: integer;
  inv: TBrickInventory;
begin
  FillChar(Result, SizeOf(partout_t), 0);
  if (fnumlooseparts = 0) and (totalsetsbuilted = 0) then
    Exit;

  if Now() - fupdatetime < fupdatetimeout then
  begin
    Result := fSoldPartOutValue_nQtyAvg;
    Exit;
  end;

  if totalsetsbuilted > 0 then
  begin
    inv := Clone;
    inv.DismandalAllSets;
    Result := inv.SoldPartOutValue_nQtyAvg;
    inv.Free;
    Exit;
  end;

  miss := 0;
  tot := 0;
  brick := @flooseparts[0];
  for i := 0 to fnumlooseparts - 1 do
  begin
//    pg := db.Priceguide(brick.part, brick.color);
    pg := db.Priceguide(brick);
    if pg.nTimesSold = 0 then
    begin
      if pg.uTimesSold = 0 then
        db.CrawlerPriorityPart(brick.part, brick.color);
      Inc(miss, brick.num);
    end
    else
      Result.value := Result.value + pg.nQtyAvgPrice * brick.num;
    Inc(tot, brick.num);
    Inc(brick);
  end;
  if tot = 0 then
    Exit;
  Result.percentage := 1.0 - miss / tot;
end;

function TBrickInventory.SoldPartOutValue_uAvg: partout_t;
var
  i: integer;
  brick: brickpool_p;
  pg: priceguide_t;
  tot, miss: integer;
  inv: TBrickInventory;
begin
  FillChar(Result, SizeOf(partout_t), 0);
  if (fnumlooseparts = 0) and (totalsetsbuilted = 0) then
    Exit;

  if Now() - fupdatetime < fupdatetimeout then
  begin
    Result := fSoldPartOutValue_uAvg;
    Exit;
  end;

  if totalsetsbuilted > 0 then
  begin
    inv := Clone;
    inv.DismandalAllSets;
    Result := inv.SoldPartOutValue_uAvg;
    inv.Free;
    Exit;
  end;

  miss := 0;
  tot := 0;
  brick := @flooseparts[0];
  for i := 0 to fnumlooseparts - 1 do
  begin
//    pg := db.Priceguide(brick.part, brick.color);
    pg := db.Priceguide(brick);
    if pg.uTimesSold = 0 then
    begin
      if pg.nTimesSold = 0 then
        db.CrawlerPriorityPart(brick.part, brick.color);
      Inc(miss, brick.num);
    end
    else
      Result.value := Result.value + pg.uAvgPrice * brick.num;
    Inc(tot, brick.num);
    Inc(brick);
  end;
  if tot = 0 then
    Exit;
  Result.percentage := 1.0 - miss / tot;
end;

function TBrickInventory.SoldPartOutValue_uQtyAvg: partout_t;
var
  i: integer;
  brick: brickpool_p;
  pg: priceguide_t;
  tot, miss: integer;
  inv: TBrickInventory;
begin
  FillChar(Result, SizeOf(partout_t), 0);
  if (fnumlooseparts = 0) and (totalsetsbuilted = 0) then
    Exit;

  if Now() - fupdatetime < fupdatetimeout then
  begin
    Result := fSoldPartOutValue_uQtyAvg;
    Exit;
  end;

  if totalsetsbuilted > 0 then
  begin
    inv := Clone;
    inv.DismandalAllSets;
    Result := inv.SoldPartOutValue_uQtyAvg;
    inv.Free;
    Exit;
  end;

  miss := 0;
  tot := 0;
  brick := @flooseparts[0];
  for i := 0 to fnumlooseparts - 1 do
  begin
//    pg := db.Priceguide(brick.part, brick.color);
    pg := db.Priceguide(brick);
    if pg.uTimesSold = 0 then
    begin
      if pg.nTimesSold = 0 then
        db.CrawlerPriorityPart(brick.part, brick.color);
      Inc(miss, brick.num);
    end
    else
      Result.value := Result.value + pg.uQtyAvgPrice * brick.num;
    Inc(tot, brick.num);
    Inc(brick);
  end;
  if tot = 0 then
    Exit;
  Result.percentage := 1.0 - miss / tot;
end;

function TBrickInventory.AvailablePartOutValue_nAvg: partout_t;
var
  i: integer;
  brick: brickpool_p;
  av: availability_t;
  tot, miss: integer;
  inv: TBrickInventory;
begin
  FillChar(Result, SizeOf(partout_t), 0);
  if (fnumlooseparts = 0) and (totalsetsbuilted = 0) then
    Exit;

  if Now() - fupdatetime < fupdatetimeout then
  begin
    Result := fAvailablePartOutValue_nAvg;
    Exit;
  end;

  if totalsetsbuilted > 0 then
  begin
    inv := Clone;
    inv.DismandalAllSets;
    Result := inv.AvailablePartOutValue_nAvg;
    inv.Free;
    Exit;
  end;

  miss := 0;
  tot := 0;
  brick := @flooseparts[0];
  for i := 0 to fnumlooseparts - 1 do
  begin
//    av := db.Availability(brick.part, brick.color);
    av := db.Availability(brick);
    if av.nTotalLots = 0 then
      Inc(miss, brick.num)
    else
      Result.value := Result.value + av.nAvgPrice * brick.num;
    Inc(tot, brick.num);
    Inc(brick);
  end;
  if tot = 0 then
    Exit;
  Result.percentage := 1.0 - miss / tot;
end;

function TBrickInventory.AvailablePartOutValue_nQtyAvg: partout_t;
var
  i: integer;
  brick: brickpool_p;
  av: availability_t;
  tot, miss: integer;
  inv: TBrickInventory;
begin
  FillChar(Result, SizeOf(partout_t), 0);
  if (fnumlooseparts = 0) and (totalsetsbuilted = 0) then
    Exit;

  if Now() - fupdatetime < fupdatetimeout then
  begin
    Result := fAvailablePartOutValue_nQtyAvg;
    Exit;
  end;

  if totalsetsbuilted > 0 then
  begin
    inv := Clone;
    inv.DismandalAllSets;
    Result := inv.AvailablePartOutValue_nQtyAvg;
    inv.Free;
    Exit;
  end;

  miss := 0;
  tot := 0;
  brick := @flooseparts[0];
  for i := 0 to fnumlooseparts - 1 do
  begin
//    av := db.Availability(brick.part, brick.color);
    av := db.Availability(brick);
    if av.nTotalLots = 0 then
      Inc(miss, brick.num)
    else
      Result.value := Result.value + av.nQtyAvgPrice * brick.num;
    Inc(tot, brick.num);
    Inc(brick);
  end;
  if tot = 0 then
    Exit;
  Result.percentage := 1.0 - miss / tot;
end;

function TBrickInventory.AvailablePartOutValue_uAvg: partout_t;
var
  i: integer;
  brick: brickpool_p;
  av: availability_t;
  tot, miss: integer;
  inv: TBrickInventory;
begin
  FillChar(Result, SizeOf(partout_t), 0);
  if (fnumlooseparts = 0) and (totalsetsbuilted = 0) then
    Exit;

  if Now() - fupdatetime < fupdatetimeout then
  begin
    Result := fAvailablePartOutValue_uAvg;
    Exit;
  end;

  if totalsetsbuilted > 0 then
  begin
    inv := Clone;
    inv.DismandalAllSets;
    Result := inv.AvailablePartOutValue_uAvg;
    inv.Free;
    Exit;
  end;

  miss := 0;
  tot := 0;
  brick := @flooseparts[0];
  for i := 0 to fnumlooseparts - 1 do
  begin
//    av := db.Availability(brick.part, brick.color);
    av := db.Availability(brick);
    if av.uTotalLots = 0 then
      Inc(miss, brick.num)
    else
      Result.value := Result.value + av.uAvgPrice * brick.num;
    if av.uAvgPrice > 1000 then
      av.uAvgPrice := 1;
    Inc(tot, brick.num);
    Inc(brick);
  end;
  if tot = 0 then
    Exit;
  Result.percentage := 1.0 - miss / tot;
end;

function TBrickInventory.AvailablePartOutValue_uQtyAvg: partout_t;
var
  i: integer;
  brick: brickpool_p;
  av: availability_t;
  tot, miss: integer;
  inv: TBrickInventory;
begin
  FillChar(Result, SizeOf(partout_t), 0);
  if (fnumlooseparts = 0) and (totalsetsbuilted = 0) then
    Exit;

  if Now() - fupdatetime < fupdatetimeout then
  begin
    Result := fAvailablePartOutValue_uQtyAvg;
    Exit;
  end;

  if totalsetsbuilted > 0 then
  begin
    inv := Clone;
    inv.DismandalAllSets;
    Result := inv.SoldPartOutValue_nAvg;
    inv.Free;
    Exit;
  end;

  miss := 0;
  tot := 0;
  brick := @flooseparts[0];
  for i := 0 to fnumlooseparts - 1 do
  begin
//    av := db.Availability(brick.part, brick.color);
    av := db.Availability(brick);
    if av.uTotalLots = 0 then
      Inc(miss, brick.num)
    else
      Result.value := Result.value + av.uQtyAvgPrice * brick.num;
    Inc(tot, brick.num);
    Inc(brick);
  end;
  if tot = 0 then
    Exit;
  Result.percentage := 1.0 - miss / tot;
end;

function TBrickInventory.EvaluatedPartOutValue_nAvg: partout_t;
var
  i: integer;
  brick: brickpool_p;
  pci: TPieceColorInfo;
  tot, miss: integer;
  inv: TBrickInventory;
  price: double;
begin
  FillChar(Result, SizeOf(partout_t), 0);
  if (fnumlooseparts = 0) and (totalsetsbuilted = 0) then
    Exit;

  if Now() - fupdatetime < fupdatetimeout then
  begin
    Result := fEvaluatedPartOutValue_nAvg;
    Exit;
  end;

  if totalsetsbuilted > 0 then
  begin
    inv := Clone;
    inv.DismandalAllSets;
    Result := inv.EvaluatedPartOutValue_nAvg;
    inv.Free;
    Exit;
  end;

  miss := 0;
  tot := 0;
  brick := @flooseparts[0];
  for i := 0 to fnumlooseparts - 1 do
  begin
//    pci := db.PieceColorInfo(brick.part, brick.color);
    pci := db.PieceColorInfo(brick);
    if pci = nil then
      Inc(miss, brick.num)
    else
    begin
      price := pci.EvaluatePriceNewAvg;
      if price <= 0 then
        Inc(miss, brick.num)
      else
        Result.value := Result.value + price * brick.num;
    end;
    Inc(tot, brick.num);
    Inc(brick);
  end;
  if tot = 0 then
    Exit;
  Result.percentage := 1.0 - miss / tot;
end;

function TBrickInventory.EvaluatedPartOutValue_nQtyAvg: partout_t;
var
  i: integer;
  brick: brickpool_p;
  pci: TPieceColorInfo;
  tot, miss: integer;
  inv: TBrickInventory;
  price: double;
begin
  FillChar(Result, SizeOf(partout_t), 0);
  if (fnumlooseparts = 0) and (totalsetsbuilted = 0) then
    Exit;

  if Now() - fupdatetime < fupdatetimeout then
  begin
    Result := fEvaluatedPartOutValue_nQtyAvg;
    Exit;
  end;

  if totalsetsbuilted > 0 then
  begin
    inv := Clone;
    inv.DismandalAllSets;
    Result := inv.EvaluatedPartOutValue_nQtyAvg;
    inv.Free;
    Exit;
  end;

  miss := 0;
  tot := 0;
  brick := @flooseparts[0];
  for i := 0 to fnumlooseparts - 1 do
  begin
//    pci := db.PieceColorInfo(brick.part, brick.color);
    pci := db.PieceColorInfo(brick);
    if pci = nil then
      Inc(miss, brick.num)
    else
    begin
      price := pci.EvaluatePriceNew;
      if price <= 0 then
        Inc(miss, brick.num)
      else
        Result.value := Result.value + price * brick.num;
    end;
    Inc(tot, brick.num);
    Inc(brick);
  end;
  if tot = 0 then
    Exit;
  Result.percentage := 1.0 - miss / tot;
end;

function TBrickInventory.EvaluatedPartOutValue_uAvg: partout_t;
var
  i: integer;
  brick: brickpool_p;
  pci: TPieceColorInfo;
  tot, miss: integer;
  inv: TBrickInventory;
  price: double;
begin
  FillChar(Result, SizeOf(partout_t), 0);
  if (fnumlooseparts = 0) and (totalsetsbuilted = 0) then
    Exit;

  if Now() - fupdatetime < fupdatetimeout then
  begin
    Result := fEvaluatedPartOutValue_uAvg;
    Exit;
  end;

  if totalsetsbuilted > 0 then
  begin
    inv := Clone;
    inv.DismandalAllSets;
    Result := inv.EvaluatedPartOutValue_uAvg;
    inv.Free;
    Exit;
  end;

  miss := 0;
  tot := 0;
  brick := @flooseparts[0];
  for i := 0 to fnumlooseparts - 1 do
  begin
//    pci := db.PieceColorInfo(brick.part, brick.color);
    pci := db.PieceColorInfo(brick);
    if pci = nil then
      Inc(miss, brick.num)
    else
    begin
      price := pci.EvaluatePriceUsedAvg;
      if price <= 0 then
        Inc(miss, brick.num)
      else
        Result.value := Result.value + price * brick.num;
    end;
    Inc(tot, brick.num);
    Inc(brick);
  end;
  if tot = 0 then
    Exit;
  Result.percentage := 1.0 - miss / tot;
end;

function TBrickInventory.EvaluatedPartOutValue_uQtyAvg: partout_t;
var
  i: integer;
  brick: brickpool_p;
  pci: TPieceColorInfo;
  tot, miss: integer;
  inv: TBrickInventory;
  price: double;
begin
  FillChar(Result, SizeOf(partout_t), 0);
  if (fnumlooseparts = 0) and (totalsetsbuilted = 0) then
    Exit;

  if Now() - fupdatetime < fupdatetimeout then
  begin
    Result := fEvaluatedPartOutValue_uQtyAvg;
    Exit;
  end;

  if totalsetsbuilted > 0 then
  begin
    inv := Clone;
    inv.DismandalAllSets;
    Result := inv.EvaluatedPartOutValue_uQtyAvg;
    inv.Free;
    Exit;
  end;

  miss := 0;
  tot := 0;
  brick := @flooseparts[0];
  for i := 0 to fnumlooseparts - 1 do
  begin
//    pci := db.PieceColorInfo(brick.part, brick.color);
    pci := db.PieceColorInfo(brick);
    if pci = nil then
      Inc(miss, brick.num)
    else
    begin
      price := pci.EvaluatePriceUsed;
      if price <= 0 then
        Inc(miss, brick.num)
      else
        Result.value := Result.value + price * brick.num;
    end;
    Inc(tot, brick.num);
    Inc(brick);
  end;
  if tot = 0 then
    Exit;
  Result.percentage := 1.0 - miss / tot;
end;

function TBrickInventory.nDemand: partout_t;
var
  i: integer;
  tot, miss: integer;
  pci: TPieceColorInfo;
  d1, d2: double;
  brick: brickpool_p;
  price: double;
  inv: TBrickInventory;
begin
  FillChar(Result, SizeOf(partout_t), 0);
  if (fnumlooseparts = 0) and (totalsetsbuilted = 0) then
    Exit;

  if totalsetsbuilted > 0 then
  begin
    inv := Clone;
    inv.DismandalAllSets;
    Result := inv.nDemand;
    inv.Free;
    Exit;
  end;

  d1 := 0.0;
  d2 := 0.0;
  tot := 0;
  miss := 0;
  brick := @flooseparts[0];
  for i := 0 to fnumlooseparts - 1 do
  begin
    Inc(tot, brick.num);
//    pci := db.PieceColorInfo(brick.part, brick.color);
    pci := db.PieceColorInfo(brick);
    if pci <> nil then
    begin
      price := pci.priceguide.nQtyAvgPrice;
      d1 := d1 + pci.nDemand * price * brick.num;
      d2 := d2 + price * brick.num;
      if price = 0.0 then
        Inc(miss, brick.num);
    end;
    Inc(brick);
  end;

  if d2 > 0.0 then
    Result.value := d1 / d2
  else
    Result.value := 0.0;
  if tot = 0 then
    Exit;
  Result.percentage := 1.0 - miss / tot;
end;

function TBrickInventory.uDemand: partout_t;
var
  i: integer;
  tot, miss: integer;
  pci: TPieceColorInfo;
  d1, d2: double;
  brick: brickpool_p;
  price: double;
  inv: TBrickInventory;
begin
  FillChar(Result, SizeOf(partout_t), 0);
  if (fnumlooseparts = 0) and (totalsetsbuilted = 0) then
    Exit;

  if totalsetsbuilted > 0 then
  begin
    inv := Clone;
    inv.DismandalAllSets;
    Result := inv.uDemand;
    inv.Free;
    Exit;
  end;

  d1 := 0.0;
  d2 := 0.0;
  tot := 0;
  miss := 0;
  brick := @flooseparts[0];
  for i := 0 to fnumlooseparts - 1 do
  begin
    Inc(tot, brick.num);
//    pci := db.PieceColorInfo(brick.part, brick.color);
    pci := db.PieceColorInfo(brick);
    if pci <> nil then
    begin
      price := pci.priceguide.uQtyAvgPrice;
      d1 := d1 + pci.uDemand * price * brick.num;
      d2 := d2 + price * brick.num;
      if price = 0.0 then
        Inc(miss, brick.num);
    end;
    Inc(brick);
  end;

  if d2 > 0.0 then
    Result.value := d1 / d2
  else
    Result.value := 0.0;
  if tot = 0 then
    Exit;
  Result.percentage := 1.0 - miss / tot;
end;

function fpciloaderworker(parms: fpciloaderparams_p): integer; stdcall;
var
  i, j: integer;
begin
  for i := -1 to MAXINFOCOLOR do
    if (parms.db.fcolors[i].id = i) or (i = -1) then
    begin
      if parms.db.fcolors[i].knownpieces <> nil then
        for j := 0 to parms.db.fcolors[i].knownpieces.Count - 1 do
          (parms.db.fcolors[i].knownpieces.Objects[j] as TPieceColorInfo).Load;
    end;
  Result := 0;
end;

//------------------------------------------------------------------------------
const
  CACHEDBSPREAD = 128;

constructor TCacheDB.Create(const aname: string);
var
  item: cachedbitem_t;
  i, j, idx: integer;
  h: LongWord;
  spart: string;
begin
  {$IFNDEF CRAWLER}
  st_pcihitcnt := 0;
  st_pcihitlevel := 0;
  {$ENDIF}
  waitlist := TDNumberList.Create;
  parecs := malloc(SizeOf(cachedbparec_t));
  fname := aname;
  fstream := nil;
  if fexists(aname) then
  begin
    OpenDB('r');
    if fstream <> nil then
    begin
      if fstream.Size <> SizeOf(cachedbparec_t) then
      begin
        MT_ZeroMemory(parecs, SizeOf(cachedbparec_t));
        for i := 0 to fstream.Size div SizeOf(cachedbitem_t) - 1 do
        begin
          if i = CACHEDBHASHSIZE then
            break;
          fstream.Read(item, SizeOf(cachedbitem_t));
          spart := Trim(apart(@item));
          if spart <> '' then
          begin
            h := MkPCIHash(spart, item.color);
            for j := h to h + CACHEDBSPREAD - 1 do
            begin
              idx := j mod CACHEDBHASHSIZE;
              if apart(@parecs[idx]) = '' then
              begin
                parecs[idx] := item;
                break;
              end;
            end;
          end;
        end;
        CloseDB;
        OpenDB('c');
        if fstream <> nil then
          fstream.Write(parecs^, SizeOf(cachedbparec_t));
      end
      else
        fstream.Read(parecs^, SizeOf(cachedbparec_t));
      CloseDB;
    end
    else
      MT_ZeroMemory(parecs, SizeOf(cachedbparec_t));
  end
  else
  begin
    MT_ZeroMemory(parecs, SizeOf(cachedbparec_t));
    OpenDB('c');
    if fstream <> nil then
    begin
      fstream.Write(parecs^, SizeOf(cachedbparec_t));
      CloseDB;
    end;
  end;
  Inherited Create;
end;

procedure TCacheDB.Reorganize;
var
  item: cachedbitem_t;
  i, j, idx: integer;
  h: LongWord;
  ppart, spart: string;
begin
  if not fexists(fname) then
    Exit;
  S_BackupFile(fname);
  OpenDB('c');
  if fstream <> nil then
    fstream.Write(parecs^, SizeOf(cachedbparec_t));
  CloseDB;

  OpenDB('r');
  MT_ZeroMemory(parecs, SizeOf(cachedbparec_t));

  // First pass
  fstream.Position := 0;
  for i := 0 to CACHEDBHASHSIZE - 1 do
  begin
    fstream.Read(item, SizeOf(cachedbitem_t));
    spart := Trim(apart(@item));
    if spart <> '' then
    begin
      h := MkPCIHash(spart, item.color);
      parecs[h] := item;
    end;
  end;

  fstream.Position := 0;
  for i := 0 to CACHEDBHASHSIZE - 1 do
  begin
    fstream.Read(item, SizeOf(cachedbitem_t));
    spart := Trim(apart(@item));
    if spart <> '' then
    begin
      h := MkPCIHash(spart, item.color);
      ppart := apart(@parecs[h]);
      if (ppart <> spart) or (parecs[h].Color <> item.color) then
      begin
        for j := h + 1 to h + CACHEDBSPREAD - 1 do
        begin
          idx := j mod CACHEDBHASHSIZE;
          ppart := apart(@parecs[idx]);
          if ppart = '' then
          begin
            parecs[idx] := item;
            break;
          end
          else if (ppart = apart(@parecs[idx])) and (parecs[idx].color = item.color) then
          begin
            if parecs[idx].parec.date < item.parec.date then
            begin
              parecs[idx] := item;
              break;
            end;
          end;
        end;
      end;
    end;
  end;
  CloseDB;

  OpenDB('c');
  if fstream <> nil then
    fstream.Write(parecs^, SizeOf(cachedbparec_t));
  CloseDB;
end;

{$IFNDEF CRAWLER}
function TCacheDB.GetHitLevel: double;
begin
  if st_pcihitcnt = 0 then
    Result := 0.0
  else
    Result := st_pcihitlevel / st_pcihitcnt;
end;
{$ENDIF}

function TCacheDB.apart(const it: cachedbitem_p): string;
var
  i: integer;
  c: char;
begin
  Result := '';
  for i := 0 to CACHEDBSTRINGSIZE - 1 do
  begin
    c := it.partid[i];
    if c <> #0 then
      Result := Result + c
    else
      break;
  end;
  if Pos('BL ', Result) = 1 then
  begin
    Result[1] := ' ';
    Result[2] := ' ';
    Result := db.RebrickablePart(Trim(Result));
  end;
end;

function TCacheDB.OpenDB1(const mode: char): TFileStream;
begin
  if mode = 'w' then
  begin
    try
      Result := TFileStream.Create(fname, fmOpenReadWrite {or fmShareDenyWrite});
    except
      Result := nil;
    end;
    Exit;
  end
  else if mode = 'r' then
  begin
    try
      Result := TFileStream.Create(fname, fmOpenRead{ or fmShareDenyWrite});
    except
      Result := nil;
    end;
    Exit;
  end
  else if mode = 'c' then
  begin
    try
      Result := TFileStream.Create(fname, fmCreate {or fmShareDenyWrite});
    except
      Result := nil;
    end;
    Exit;
  end
  else
    Result := nil;
end;

function TCacheDB.TryOpenDB(const mode: char; const maxretry: integer = 100): TFileStream;
var
  i: integer;
  limit: integer;
begin
  Result := nil;
  limit := round(maxretry * 0.8999);
  for i := 0 to maxretry - 1 do
  begin
    Result := OpenDB1(mode);
    if Result <> nil then
      Exit;
    if i >= limit then
      Sleep(1000)
    else
      Sleep(200);
  end;
end;

procedure TCacheDB.OpenDB(const mode: char);
begin
  CloseDB;
  fstream := TryOpenDB(mode);
end;

procedure TCacheDB.CloseDB;
begin
  if fstream <> nil then
  begin
    fstream.Free;
    fstream := nil;
  end;
end;

function TCacheDB.LoadPCI(const p: TPieceColorInfo): boolean;
var
  idx: integer;
  pc: cachedbitem_p;
  i: integer;
begin
  idx := p.fhash;
  for i := idx to idx + CACHEDBSPREAD do
  begin
    pc := @parecs[i mod CACHEDBHASHSIZE];
    if pc.color = p.color then
      if apart(pc) = p.piece then
      begin
        p.Assign(pc.parec.priceguide);
        p.Assign(pc.parec.availability);
        p.fdate := pc.parec.date;
        {$IFNDEF CRAWLER}
        inc(st_pcihitcnt);
        inc(st_pcihitlevel, i - idx + 1);
        {$ENDIF}
        Result := True;
        Exit;
      end;
  end;
  Result := False;
end;

function TCacheDB.SavePCI(const p: TPieceColorInfo): boolean;
var
  idx, idx2: integer;
  pc: cachedbitem_p;
  i, len: integer;
  spiece: string;
begin
  Result := False;

  idx := p.Hash;
  idx2 := idx;
  spiece := p.piece;
  pc := @parecs[idx];
  for i := idx to idx + CACHEDBSPREAD do
  begin
    pc := @parecs[i mod CACHEDBHASHSIZE];
    if pc.color = p.color then
      if apart(pc) = p.piece then
      begin
        idx2 := i mod CACHEDBHASHSIZE;
        Break;
      end;
    if pc.color = 0 then
      if apart(pc) = '' then
      begin
        idx2 := i mod CACHEDBHASHSIZE;
        Break;
      end;
  end;
  pc := @parecs[idx2];

  len := length(spiece);
  if len > CACHEDBSTRINGSIZE then
    Exit;

  for i := 1 to len do
    pc.partid[i - 1] := spiece[i];
  for i := len to CACHEDBSTRINGSIZE - 1 do
    pc.partid[i] := #0;
  pc.color := p.color;
  pc.parec.priceguide := p.priceguide;
  pc.parec.availability := p.availability;
  pc.parec.date := p.fdate;
  waitlist.Add(idx2);
  if waitlist.Count > {$IFDEF CRAWLER}25{$ELSE}5{$ENDIF} then
    Flash;
  Result := True;
end;

procedure TCacheDB.Flash;
var
  i, idx: integer;
begin
  OpenDB('w');
  if fstream <> nil then
  begin
    waitlist.Sort;
    for i := 0 to waitlist.Count - 1 do
    begin
      idx := waitlist.Numbers[i];
      fstream.Position := idx * SizeOf(cachedbitem_t);
      fstream.Write(parecs[idx], SizeOf(cachedbitem_t));
    end;
    CloseDB;
    waitlist.Clear;
  end;
end;

procedure TCacheDB.FlashAll;
begin
  OpenDB('w');
  if fstream <> nil then
  begin
    fstream.Write(parecs^, SizeOf(cachedbparec_t));
    CloseDB;
  end;
end;

destructor TCacheDB.Destroy;
begin
  Flash;
  CloseDB;
  waitlist.Free;
  memfree(pointer(parecs), SizeOf(cachedbparec_t));
  Inherited;
end;

//------------------------------------------------------------------------------
constructor TSetsDatabase.Create;
begin
  randomize;
  db := self;
  fcrawlerrandom := 0;
  {$IFNDEF CRAWLER}
  S_InitFileSystem;
  {$ENDIF}
  inherited Create;
  fPartTypeList := TStringList.Create;
  fpartsinventoriesvalidcount := 0;
  fmaximumcolors := 0;
  fCrawlerCache := TStringList.Create;
  {$IFNDEF CRAWLER}
  fStorageBinsCache := TStringList.Create;
  fbasemolds := TStringList.Create;
  {$ENDIF}
  ffixedblcolors := TStringList.Create;
  ffixedblcolors.Add('Aqua');
  ffixedblcolors.Add('Black');
  ffixedblcolors.Add('Blue');
  ffixedblcolors.Add('Blue-Violet');
  ffixedblcolors.Add('Bright Green');
  ffixedblcolors.Add('Bright Light Blue');
  ffixedblcolors.Add('Bright Light Orange');
  ffixedblcolors.Add('Bright Light Yellow');
  ffixedblcolors.Add('Bright Pink');
  ffixedblcolors.Add('Brown');
  ffixedblcolors.Add('Coral');
  ffixedblcolors.Add('Dark Azure');
  ffixedblcolors.Add('Dark Blue');
  ffixedblcolors.Add('Dark Blue-Violet');
  ffixedblcolors.Add('Dark Bluish Gray');
  ffixedblcolors.Add('Dark Brown');
  ffixedblcolors.Add('Dark Flesh');
  ffixedblcolors.Add('Dark Gray');
  ffixedblcolors.Add('Dark Green');
  ffixedblcolors.Add('Dark Orange');
  ffixedblcolors.Add('Dark Pink');
  ffixedblcolors.Add('Dark Purple');
  ffixedblcolors.Add('Dark Red');
  ffixedblcolors.Add('Dark Tan');
  ffixedblcolors.Add('Dark Turquoise');
  ffixedblcolors.Add('Dark Yellow');
  ffixedblcolors.Add('Earth Orange');
  ffixedblcolors.Add('Fabuland Brown');
  ffixedblcolors.Add('Fabuland Orange');
  ffixedblcolors.Add('Flesh');
  ffixedblcolors.Add('Green');
  ffixedblcolors.Add('Lavender');
  ffixedblcolors.Add('Light Aqua');
  ffixedblcolors.Add('Light Blue');
  ffixedblcolors.Add('Light Bluish Gray');
  ffixedblcolors.Add('Light Flesh');
  ffixedblcolors.Add('Light Gray');
  ffixedblcolors.Add('Light Green');
  ffixedblcolors.Add('Light Lime');
  ffixedblcolors.Add('Light Orange');
  ffixedblcolors.Add('Light Pink');
  ffixedblcolors.Add('Light Purple');
  ffixedblcolors.Add('Light Salmon');
  ffixedblcolors.Add('Light Turquoise');
  ffixedblcolors.Add('Light Violet');
  ffixedblcolors.Add('Light Yellow');
  ffixedblcolors.Add('Lime');
  ffixedblcolors.Add('Maersk Blue');
  ffixedblcolors.Add('Magenta');
  ffixedblcolors.Add('Medium Azure');
  ffixedblcolors.Add('Medium Blue');
  ffixedblcolors.Add('Medium Dark Flesh');
  ffixedblcolors.Add('Medium Dark Pink');
  ffixedblcolors.Add('Medium Green');
  ffixedblcolors.Add('Medium Lavender');
  ffixedblcolors.Add('Medium Lime');
  ffixedblcolors.Add('Medium Orange');
  ffixedblcolors.Add('Medium Violet');
  ffixedblcolors.Add('Neon Green');
  ffixedblcolors.Add('Neon Orange');
  ffixedblcolors.Add('Olive Green');
  ffixedblcolors.Add('Orange');
  ffixedblcolors.Add('Pink');
  ffixedblcolors.Add('Purple');
  ffixedblcolors.Add('Red');
  ffixedblcolors.Add('Reddish Brown');
  ffixedblcolors.Add('Rust');
  ffixedblcolors.Add('Salmon');
  ffixedblcolors.Add('Sand Blue');
  ffixedblcolors.Add('Sand Green');
  ffixedblcolors.Add('Sand Purple');
  ffixedblcolors.Add('Sand Red');
  ffixedblcolors.Add('Sky Blue');
  ffixedblcolors.Add('Tan');
  ffixedblcolors.Add('Very Light Bluish Gray');
  ffixedblcolors.Add('Very Light Gray');
  ffixedblcolors.Add('Very Light Orange');
  ffixedblcolors.Add('Violet');
  ffixedblcolors.Add('White');
  ffixedblcolors.Add('Yellow');
  ffixedblcolors.Add('Yellowish Green');
  ffixedblcolors.Add('Trans-Black');
  ffixedblcolors.Add('Trans-Bright Green');
  ffixedblcolors.Add('Trans-Clear');
  ffixedblcolors.Add('Trans-Dark Blue');
  ffixedblcolors.Add('Trans-Dark Pink');
  ffixedblcolors.Add('Trans-Green');
  ffixedblcolors.Add('Trans-Light Blue');
  ffixedblcolors.Add('Trans-Light Orange');
  ffixedblcolors.Add('Trans-Light Purple');
  ffixedblcolors.Add('Trans-Medium Blue');
  ffixedblcolors.Add('Trans-Neon Green');
  ffixedblcolors.Add('Trans-Neon Orange');
  ffixedblcolors.Add('Trans-Neon Yellow');
  ffixedblcolors.Add('Trans-Orange');
  ffixedblcolors.Add('Trans-Pink');
  ffixedblcolors.Add('Trans-Purple');
  ffixedblcolors.Add('Trans-Red');
  ffixedblcolors.Add('Trans-Very Lt Blue');
  ffixedblcolors.Add('Trans-Yellow');
  ffixedblcolors.Add('Chrome Antique Brass');
  ffixedblcolors.Add('Chrome Black');
  ffixedblcolors.Add('Chrome Blue');
  ffixedblcolors.Add('Chrome Gold');
  ffixedblcolors.Add('Chrome Green');
  ffixedblcolors.Add('Chrome Pink');
  ffixedblcolors.Add('Chrome Silver');
  ffixedblcolors.Add('Copper');
  ffixedblcolors.Add('Flat Dark Gold');
  ffixedblcolors.Add('Flat Silver');
  ffixedblcolors.Add('Metal Blue');
  ffixedblcolors.Add('Pearl Dark Gray');
  ffixedblcolors.Add('Pearl Gold');
  ffixedblcolors.Add('Pearl Light Gold');
  ffixedblcolors.Add('Pearl Light Gray');
  ffixedblcolors.Add('Pearl Very Light Gray');
  ffixedblcolors.Add('Pearl White');
  ffixedblcolors.Add('Metallic Gold');
  ffixedblcolors.Add('Metallic Green');
  ffixedblcolors.Add('Metallic Silver');
  ffixedblcolors.Add('Glow In Dark Opaque');
  ffixedblcolors.Add('Glow In Dark Trans');
  ffixedblcolors.Add('Glow In Dark White');
  ffixedblcolors.Add('Milky White');
  ffixedblcolors.Add('Glitter Trans-Clear');
  ffixedblcolors.Add('Glitter Trans-Dark Pink');
  ffixedblcolors.Add('Glitter Trans-Light Blue');
  ffixedblcolors.Add('Glitter Trans-Neon Green');
  ffixedblcolors.Add('Glitter Trans-Purple');
  ffixedblcolors.Add('Speckle Black-Copper');
  ffixedblcolors.Add('Speckle Black-Gold');
  ffixedblcolors.Add('Speckle Black-Silver');
  ffixedblcolors.Add('Speckle DBGray-Silver');
  ffixedblcolors.Add('Mx Aqua Green');
  ffixedblcolors.Add('Mx Black');
  ffixedblcolors.Add('Mx Brown');
  ffixedblcolors.Add('Mx Buff');
  ffixedblcolors.Add('Mx Charcoal Gray');
  ffixedblcolors.Add('Mx Clear');
  ffixedblcolors.Add('Mx Lemon');
  ffixedblcolors.Add('Mx Light Bluish Gray');
  ffixedblcolors.Add('Mx Light Gray');
  ffixedblcolors.Add('Mx Light Orange');
  ffixedblcolors.Add('Mx Light Yellow');
  ffixedblcolors.Add('Mx Medium Blue');
  ffixedblcolors.Add('Mx Ochre Yellow');
  ffixedblcolors.Add('Mx Olive Green');
  ffixedblcolors.Add('Mx Orange');
  ffixedblcolors.Add('Mx Pastel Blue');
  ffixedblcolors.Add('Mx Pastel Green');
  ffixedblcolors.Add('Mx Pink');
  ffixedblcolors.Add('Mx Pink Red');
  ffixedblcolors.Add('Mx Red');
  ffixedblcolors.Add('Mx Teal Blue');
  ffixedblcolors.Add('Mx Terracotta');
  ffixedblcolors.Add('Mx Tile Blue');
  ffixedblcolors.Add('Mx Tile Brown');
  ffixedblcolors.Add('Mx Tile Gray');
  ffixedblcolors.Add('Mx Violet');
  ffixedblcolors.Add('Mx White');
  flastcrawlpiece := '';
  progressfunc := nil;
end;

procedure TSetsDatabase.FixKnownPieces;
var
  fname: string;
  s: TStringList;
  buf: THashStringList;
  dosave: boolean;
  i: integer;
  spart, scolor, sdesc: string;
  check: string;
  ts: TString;
begin
  fname := basedefault + 'db\db_knownpieces.txt';
  if not fexists(fname) then
    Exit;

  s := TStringList.Create;
  try
    S_LoadFromFile(s, fname);
    RemoveBlancLines(s);
    if s.Count > 1 then
      if s.Strings[0] = 'Part,Color,Desc' then
      begin
        dosave := False;
        buf := THashStringList.Create;
        try
          for i := 1 to s.Count - 1 do
          begin
            splitstring(s.Strings[i], spart, scolor, sdesc, ',');
            check := UpperCase(spart + ',' + scolor);
            if (Trim(spart) <> '') and (buf.IndexOf(check) < 0) then
            begin
              ts := TString.Create;
              ts.text := s.Strings[i];
              buf.AddObject(check, ts);
            end
            else
              dosave := True;
          end;
          if dosave then
          begin
            s.Clear;
            s.Add('Part,Color,Desc');
            for i := 0 to buf.Count - 1 do
            begin
              ts := buf.Objects[i] as TString;
              s.Add(ts.text);
            end;
            S_BackupFile(fname);
            RemoveBlancLines(s);
            S_SaveToFile(s, fname);
          end;
        finally
          FreeHashList(buf);
        end;
      end;
  finally
    s.Free;
  end;
end;

procedure TSetsDatabase.InitCreate(const app: string = '');
begin
//  fpciloaderparams.db := self;
  if app = '' then
    fcrawlerfilename := 'crawler.tmp'
  else
    fcrawlerfilename := app + '.tmp';
  if not DirectoryExists(basedefault + 'cache') then
    MkDir(basedefault + 'cache');
  {$IFNDEF CRAWLER}
  if not DirectoryExists(basedefault + 'storage') then
    MkDir(basedefault + 'storage');
  {$ENDIF}
  if not DirectoryExists(basedefault + 'out\') then
    MkDir(basedefault + 'out\');

  {$IFNDEF CRAWLER}
  S_SetCacheFile(basedefault + 'cache\' + fcrawlerfilename);
  S_SetCacheFile(basedefault + 'db\db_books.txt');
  S_SetCacheFile(basedefault + 'db\db_boxes.txt');
  S_SetCacheFile(basedefault + 'db\db_catalogs.txt');
  S_SetCacheFile(basedefault + 'db\db_categories.txt');
  S_SetCacheFile(basedefault + 'db\db_codes.txt');
  S_SetCacheFile(basedefault + 'db\db_gears.txt');
  S_SetCacheFile(basedefault + 'db\db_knownpieces.txt');
  S_SetCacheFile(basedefault + 'db\db_mocs.txt');
  S_SetCacheFile(basedefault + 'db\db_newnames.txt');
  S_SetCacheFile(basedefault + 'db\db_pieces_alias.txt');
  S_SetCacheFile(basedefault + 'db\db_pieces_categories.txt');
  S_SetCacheFile(basedefault + 'db\db_pieces_inventories.txt');
  S_SetCacheFile(basedefault + 'db\db_pieces_weight.txt');
  S_SetCacheFile(basedefault + 'db\db_pieces_years.txt');
  S_SetCacheFile(basedefault + 'db\db_set_assets.txt');
  S_SetCacheFile(basedefault + 'db\db_sets.txt');
  {$ENDIF}

{  RemoveDoublesFromList1(basedefault + 'db\db_pieces.extra.txt');
  RemoveDoublesFromList2(basedefault + 'db\db_knownpieces.txt');}

  FixKnownPieces;

  fCacheDB := TCacheDB.Create(basedefault + 'cache\cache.db');
  fbinarysets := TBinarySetCollection.Create(basedefault + 'db\sets1.db', basedefault + 'db\sets2.db', basedefault + 'db\sets3.db');
  fbinaryparts := TBinaryPartCollection.Create(basedefault + 'db\parts.db');

  InitPiecesInventories;

  st_pciloads := 0;
  st_pciloadscache := 0;

  ZeroMemory(@fbricklinkcolortosystemcolor, SizeOf(fbricklinkcolortosystemcolor));
  ZeroMemory(@frebrickablecolortosystemcolor, SizeOf(frebrickablecolortosystemcolor));

  fstubpieceinfo := TPieceInfo.Create;
  {$IFNDEF CRAWLER}
  fstubpieceinfo.desc := '(Unknown)';
  {$ENDIF}
  floaded := False;
  fallsets := THashStringList.Create;
  fallsetswithoutextra := THashStringList.Create;
  fallbooks := THashStringList.Create;
  fcolorpieces := TStringList.Create;
  fcrawlerpriority := TStringList.Create;
  fcrawlerhistory := TStringList.Create;
  {$IFNDEF CRAWLER}
  fstorage := TStringList.Create;
  {$ENDIF}
  fpiececodes := TStringList.Create;
  InitColors;
  InitPieces;
  InitSets;
  InitBooks;
  InitCatalogs;
  InitGears;
  InitCategories;
  InitWeightTable;
  InitPartReferences;
//  fpciloader := TDThread.Create(@fpciloaderworker);
end;

{$IFNDEF CRAWLER}
function FindAproxColorIndex(const colors: colorinfoarray_p; id: integer): integer;
var
  r, g, b: integer;
  rc, gc, bc: integer;
  dr, dg, db1: integer;
  i: integer;
  c, cc: LongWord;
  dist: LongWord;
  mindist: LongWord;
begin
  c := colors[id].RGB;
  r := c and $FF;
  g := (c shr 8) and $FF;
  b := (c shr 16) and $FF;
  Result := -1;
  mindist := LongWord($ffffffff);
  for i := 0 to MAXINFOCOLOR do
    if i <> id then
    begin
      cc := colors[i].RGB;
      rc := cc and $FF;
      gc := (cc shr 8) and $FF;
      bc := (cc shr 16) and $FF;
      dr := r - rc;
      dg := g - gc;
      db1 := b - bc;
      dist := dr * dr + dg * dg + db1 * db1;
      if dist < mindist then
      begin
        Result := i;
        mindist := dist;
      end;
    end;
end;
{$ENDIF}

procedure TSetsDatabase.InitColors;
var
  s: TStringList;
  i: integer;
  id: integer;
  s1: TStringList;
  fc: colorinfo_p;
begin
  MT_ZeroMemory(@fcolors, SizeOf(colorinfoarray_t));
  fcolors[MAXINFOCOLOR].id := MAXINFOCOLOR;

  fcolors[CATALOGCOLORINDEX].id := CATALOGCOLORINDEX;
  fcolors[CATALOGCOLORINDEX].BrickLingColor := CATALOGCOLORINDEX;
  fcolors[CATALOGCOLORINDEX].RebrickableColor := CATALOGCOLORINDEX;
  {$IFNDEF CRAWLER}
  fcolors[CATALOGCOLORINDEX].name := 'Catalog';
  {$ENDIF}

  fcolors[INSTRUCTIONCOLORINDEX].id := INSTRUCTIONCOLORINDEX;
  fcolors[INSTRUCTIONCOLORINDEX].BrickLingColor := INSTRUCTIONCOLORINDEX;
  fcolors[INSTRUCTIONCOLORINDEX].RebrickableColor := INSTRUCTIONCOLORINDEX;
  {$IFNDEF CRAWLER}
  fcolors[INSTRUCTIONCOLORINDEX].name := 'Instructions';
  {$ENDIF}

  fcolors[BOXCOLORINDEX].id := BOXCOLORINDEX;
  fcolors[BOXCOLORINDEX].BrickLingColor := BOXCOLORINDEX;
  fcolors[BOXCOLORINDEX].RebrickableColor := BOXCOLORINDEX;
  {$IFNDEF CRAWLER}
  fcolors[BOXCOLORINDEX].name := 'Original Box';
  {$ENDIF}

  s := TStringList.Create;
  s1 := TStringList.Create;
  S_LoadFromFile(s, basedefault + 'db\db_colors.txt');
  if s.Count > 0 then
  begin
    if s.Strings[0] = 'ID,Name,RGB,Num Parts,Num Sets,From Year,To Year,LEGO Color,LDraw Color,Bricklink Color,Peeron Color' then
    begin
      for i := 1 to s.Count - 1 do
      begin
        s1.Text := StringReplace(s.Strings[i], ',', #13#10, [rfReplaceAll]);
        if s1.Count >= 11 then
        begin
          id := StrToIntDef(s1.Strings[0], -1);
          if (id >= 0) and (id <= MAXINFOCOLOR) then
          begin
            fc := @fcolors[id];
            fc.id := id;
            {$IFNDEF CRAWLER}
            fc.name := s1.Strings[1];
            fc.RGB := HexToInt(s1.Strings[2]);
            fc.nParts := StrToIntDef(s1.Strings[3], 0);
            fc.nSets := StrToIntDef(s1.Strings[4], 0);
            fc.fYear := StrToIntDef(s1.Strings[5], 0);
            fc.yYear := StrToIntDef(s1.Strings[6], 0);
            fc.legoColor := s1.Strings[7];
            fc.ldrawColor := StrToIntDef(s1.Strings[8], 0);
            {$ENDIF}
            fc.BrickLingColor := StrToIntDef(s1.Strings[9], 0);
            fc.RebrickableColor := id;
            {$IFNDEF CRAWLER}
            fc.PeeronColor := s1.Strings[10];
            {$ENDIF}
            fbricklinkcolortosystemcolor[fc.BrickLingColor] := id;
            frebrickablecolortosystemcolor[fc.RebrickableColor] := id;
          end;
        end;

      end;
    end
    else if s.Strings[0] = 'ID,Rebrickable,Name,RGB,Num Parts,Num Sets,From Year,To Year,LEGO Color,LDraw Color,Bricklink Color,Peeron Color' then
    begin
      for i := 1 to s.Count - 1 do
      begin
        s1.Text := StringReplace(s.Strings[i], ',', #13#10, [rfReplaceAll]);
        if s1.Count >= 12 then
        begin
          id := StrToIntDef(s1.Strings[0], -1);
          if (id >= 0) and (id <= MAXINFOCOLOR) then
          begin
            fc := @fcolors[id];
            fc.id := id;
            fc.RebrickableColor := StrToIntDef(s1.Strings[1], 0);
            {$IFNDEF CRAWLER}
            fc.name := s1.Strings[2];
            fc.RGB := HexToInt(s1.Strings[3]);
            fc.nParts := StrToIntDef(s1.Strings[4], 0);
            fc.nSets := StrToIntDef(s1.Strings[5], 0);
            fc.fYear := StrToIntDef(s1.Strings[6], 0);
            fc.yYear := StrToIntDef(s1.Strings[7], 0);
            fc.legoColor := s1.Strings[8];
            fc.ldrawColor := StrToIntDef(s1.Strings[9], 0);
            {$ENDIF}
            fc.BrickLingColor := StrToIntDef(s1.Strings[10], 0);
            {$IFNDEF CRAWLER}
            fc.PeeronColor := s1.Strings[11];
            {$ENDIF}
            fbricklinkcolortosystemcolor[fc.BrickLingColor] := id;
            frebrickablecolortosystemcolor[fc.RebrickableColor] := id;
          end;
        end;

      end;
    end;
  end;

  fbricklinkcolortosystemcolor[0] := -1;
  frebrickablecolortosystemcolor[0] := 0;

  {$IFNDEF CRAWLER}
  fcolors[-1].alternateid := -1;
  fmaximumcolors := 1;
  for i := 0 to MAXINFOCOLOR do
    if fcolors[i].id <> 0 then
    begin
      fcolors[i].alternateid := FindAproxColorIndex(@fcolors, i);
      inc(fmaximumcolors);
    end;
  {$ENDIF}

  fcolors[-1].knownpieces := THashStringList.Create;
  fcolors[CATALOGCOLORINDEX].knownpieces := THashStringList.Create;
  fcolors[INSTRUCTIONCOLORINDEX].knownpieces := THashStringList.Create;
  fcolors[BOXCOLORINDEX].knownpieces := THashStringList.Create;

  s.Free;
  s1.Free;
end;

type
  TCodePieceInfo = class(TObject)
    piece: string;
    color: integer;
    constructor Create(const apiece: string; const acolor: integer);
  end;

constructor TCodePieceInfo.Create(const apiece: string; const acolor: integer);
begin
  Inherited Create;
  piece := apiece;
  color := acolor;
end;

{$IFNDEF CRAWLER}
procedure TSetsDatabase.SetPieceCode(const pci: TPieceColorInfo; const newcode: string);
var
  i: integer;
  ccode: TCodePieceInfo;
begin
  if pci.code = newcode then
    Exit;

  if pci.code = '' then
  begin
    pci.code := newcode;
    fpiececodes.Sorted := False;
    fpiececodes.AddObject(newcode, TCodePieceInfo.Create(pci.piece, pci.color));
    fpiececodes.Sorted := True;
    SavePieceCodes;
    Exit;
  end;

  for i := 0 to fpiececodes.Count - 1 do
  begin
    ccode := fpiececodes.Objects[i] as TCodePieceInfo;
    if ccode.piece = pci.piece then
      if ccode.color = pci.color then
      begin
        fpiececodes.Sorted := False;
        if newcode = '' then
        begin
          fpiececodes.Delete(i);
          pci.code := '';
        end
        else
        begin
          fpiececodes.Strings[i] := newcode;
          pci.code := newcode;
        end;
        fpiececodes.Sorted := True;
        SavePieceCodes;
        Exit;
      end;
  end;

  pci.code := newcode;
  fpiececodes.Sorted := False;
  fpiececodes.AddObject(newcode, TCodePieceInfo.Create(pci.piece, pci.color));
  fpiececodes.Sorted := True;
  SavePieceCodes;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
procedure TSetsDatabase.SavePieceCodes;
var
  sl: TStringList;
  i: integer;
  ccode: TCodePieceInfo;
  fname: string;
begin
  sl := TStringList.Create;
  sl.Add('Part,Color,Code');

  for i := 0 to fpiececodes.Count - 1 do
  begin
    ccode := fpiececodes.Objects[i] as TCodePieceInfo;
    sl.Add(ccode.piece + ',' + itoa(ccode.color) + ',' + fpiececodes.Strings[i]);
  end;

  fname := basedefault + 'db\db_codes.txt';
  S_BackupFile(fname);
  S_SaveToFile(sl, fname);
  sl.Free;
end;
{$ENDIF}

procedure TSetsDatabase.LoadPieceCodes;
var
  sl: TStringList;
  fname: string;
  s1, s2, s3: string;
  spiece: string;
  scolor: string;
  ncolor: integer;
  i: integer;
  pci: TPieceColorInfo;
  progressstring: string;
begin
  progressstring := 'Initializing codes...';
  fpiececodes.Sorted := False;
  for i := 0 to fpiececodes.Count - 1 do
    fpiececodes.Objects[i].Free;
  fpiececodes.Clear;

  fname := basedefault + 'db\db_codes.txt';
  if not fexists(fname) then
    Exit;

  sl := TStringList.Create;
  try
    S_LoadFromFile(sl, fname);
    if sl.Count > 0 then
      if Trim(sl.Strings[0]) = 'Part,Color,Code' then
      begin
        if Assigned(progressfunc) then
          progressfunc(progressstring, 0.0);
        for i := 1 to sl.Count - 1 do
        begin
          if i mod 500 = 0 then
            if Assigned(progressfunc) then
              progressfunc(progressstring, i / sl.Count);

          splitstring(sl.Strings[i], s1, s2, s3, ',');

          if Pos('BL ', s1) = 1 then
            spiece := RebrickablePart(Trim(Copy(s1, 4, Length(s1) - 3)))
          else
            spiece := RebrickablePart(Trim(s1));

          if Pos('BL', s2) = 1 then
          begin
            scolor := Trim(Copy(s2, 3, Length(s2) - 2));
            ncolor := BrickLinkColorToSystemColor(StrToIntDef(scolor, 0))
          end
          else if Pos('RB', s2) = 1 then
          begin
            scolor := Trim(Copy(s2, 3, Length(s2) - 2));
            ncolor := RebrickableColorToSystemColor(StrToIntDef(scolor, 0))
          end
          else
          begin
            scolor := Trim(s2);
            ncolor := StrToIntDef(scolor, 0);
          end;

          pci := PieceColorInfo(spiece, ncolor);
          if pci <> nil then
          begin
            fpiececodes.AddObject(s3, TCodePieceInfo.Create(spiece, ncolor));
            pci.code := s3;
          end;
        end;
        if Assigned(progressfunc) then
          progressfunc(progressstring, 1.0);
      end;
  finally
    sl.Free;
  end;

  fpiececodes.Sort;
  for i := fpiececodes.Count - 1 downto 1 do
    if fpiececodes.Strings[i] = fpiececodes.Strings[i - 1] then
    begin
      fpiececodes.Objects[i].Free;
      fpiececodes.Delete(i);
    end;

  fpiececodes.Sorted := True;
end;

procedure TSetsDatabase.InitCategories;
var
  sl: TStringList;
  s: string;
  s1, s2: string;
  idx: integer;
  i: integer;
  pinf: TPieceInfo;
  tmpcat: integer;
  scat, spiece, sname, syear, sweight, sdim: string;
  {$IFNDEF CRAWLER}
  sdimx, sdimy, sdimz: string;
  {$ENDIF}
  fn: string;
begin
  MT_ZeroMemory(@fcategories, SizeOf(categoryinfoarray_t));
  for i := 0 to MAXCATEGORIES - 1 do
    fcategories[i].knownpieces := THashStringList.Create;
  for i := 1 to MAXCATEGORIES - 1 do
    fcategories[i].name := 'Category #' + itoa(i);

  fn := basedefault + 'db\db_categories.txt';
  if fexists(fn) then
  begin
    sl := TStringList.Create;
    S_LoadFromFile(sl, fn);
    if sl.Count > 0 then
      if sl.Strings[0] = 'Category_ID,Category_Name' then
        for i := 1 to sl.Count - 1 do
        begin
          s := sl.Strings[i];
          splitstring(s, s1, s2, ',');
          idx := StrToIntDef(s1, -1);
          if (idx >= 0) and (idx < MAXCATEGORIES) then
          begin
            fcategories[idx].name := s2;
            {$IFNDEF CRAWLER}
            fcategories[idx].fetched := True;
            {$ENDIF}
          end;
        end;
    sl.Free;
  end;

  fn := basedefault + 'db\db_pieces_bl.txt';
  if fexists(fn) then
  begin
    sl := TStringList.Create;
    S_LoadFromFile(sl, fn);
    if sl.Count > 0 then
      if sl.Strings[0] = 'Category_ID,Part,Weight,Dimensions' then
        for i := 1 to sl.Count - 1 do
        begin
          s := sl.Strings[i];
          splitstring(s, scat, spiece, sweight, sdim, ',');
          spiece := fixpartname(spiece);
          {$IFNDEF CRAWLER}
          splitstring(sdim, sdimx, sdimy, sdimz, 'x');
          sdimx := Trim(sdimx);
          sdimy := Trim(sdimy);
          sdimz := Trim(sdimz);
          {$ENDIF}
          spiece := RebrickablePart(spiece);
          idx := IndexOfString(fpieceshash, spiece);
          if idx < 0 then
            idx := fpieces.IndexOf(spiece);
          if idx > -1 then
          begin
            pinf := fpieces.Objects[idx] as TPieceInfo;
            pinf.weight := atof(sweight, 0.0);
            {$IFNDEF CRAWLER}
            pinf.dimentionx := atof(sdimx, 0.0);
            pinf.dimentiony := atof(sdimy, 0.0);
            pinf.dimentionz := atof(sdimz, 0.0);
            {$ENDIF}
            idx := StrToIntDef(scat, 0);
            if (idx >= 0) and (idx < MAXCATEGORIES) then
            begin
              pinf.category := idx;
//              fcategories[idx].knownpieces.AddObject(pinf.name, pinf);
            end;
          end;
        end;
    sl.Free;
  end;

  fn := basedefault + 'db\db_pieces_categories.txt';
  if fexists(fn) then
  begin
    sl := TStringList.Create;
    S_LoadFromFile(sl, fn);
    if sl.Count > 0 then
      if sl.Strings[0] = 'Category,Part' then
        for i := 1 to sl.Count - 1 do
        begin
          s := sl.Strings[i];
          splitstring(s, scat, spiece, ',');
          spiece := fixpartname(spiece);
          spiece := RebrickablePart(spiece);
          idx := IndexOfString(fpieceshash, spiece);
          if idx < 0 then
            idx := fpieces.IndexOf(spiece);
          if idx > -1 then
          begin
            pinf := fpieces.Objects[idx] as TPieceInfo;
            idx := StrToIntDef(scat, 0);
            if (idx >= 0) and (idx < MAXCATEGORIES) then
            begin
              tmpcat := pinf.category;
              if tmpcat <> idx then
              begin
                pinf.category := idx;
//                fcategories[idx].knownpieces.AddObject(pinf.name, pinf);
//                idx := fcategories[oldcat].knownpieces.IndexOf(pinf.name);
//                if idx >= oldcat then
//                  fcategories[oldcat].knownpieces.Delete(idx);
              end;
            end;
          end;
        end;
    sl.Free;
  end;

  fn := basedefault + 'db\db_catalogs.txt';
  if fexists(fn) then
  begin
    sl := TStringList.Create;
    S_LoadFromFile(sl, fn);
    if sl.Count > 0 then
      if sl.Strings[0] = 'Category,Number,Name,Year,Weight' then
        for i := 1 to sl.Count - 1 do
        begin
          s := sl.Strings[i];
          splitstring(s, scat, spiece, sname, syear, sweight, ',');
          spiece := fixpartname(spiece);
          spiece := RebrickablePart(spiece);
          idx := IndexOfString(fpieceshash, spiece);
          if idx < 0 then
            idx := fpieces.IndexOf(spiece);
          if idx > -1 then
          begin
            pinf := fpieces.Objects[idx] as TPieceInfo;
            if pinf.category <= 0 then
            begin
              idx := StrToIntDef(scat, 0);
              if (idx >= 0) and (idx < MAXCATEGORIES) then
              begin
                pinf.category := idx;
//                fcategories[idx].knownpieces.AddObject(pinf.name, pinf);
//                idx := fcategories[0].knownpieces.IndexOf(pinf.name);
//                if idx >= 0 then
//                  fcategories[0].knownpieces.Delete(idx);
              end;
            end;
          end;
        end;
    sl.Free;
  end;

  for i := 0 to fpieces.Count - 1 do
  begin
    pinf := fpieces.Objects[i] as TPieceInfo;
    tmpcat := pinf.category;
    if (tmpcat >= 0) and (tmpcat < MAXCATEGORIES) then
      fcategories[tmpcat].knownpieces.AddObject(pinf.name, pinf);
  end;

end;

procedure TSetsDatabase.InitWeightTable;
var
  sl: TStringList;
  s: string;
  idx: integer;
  i: integer;
  pinf: TPieceInfo;
  spiece, sweight: string;
  fn: string;
begin
  fn := basedefault + 'db\db_pieces_weight.txt';
  if fexists(fn) then
  begin
    sl := TStringList.Create;
    S_LoadFromFile(sl, fn);
    if sl.Count > 0 then
      if sl.Strings[0] = 'Part,Weight' then
        for i := 1 to sl.Count - 1 do
        begin
          s := sl.Strings[i];
          splitstring(s, spiece, sweight, ',');
          spiece := fixpartname(spiece);
          spiece := RebrickablePart(spiece);
          idx := IndexOfString(fpieceshash, spiece);
          if idx > -1 then
          begin
            pinf := fpieces.Objects[idx] as TPieceInfo;
            if pinf <> nil then
              pinf.weight := atof(sweight);
          end;
        end;
    sl.Free;
  end;
end;

{$IFNDEF CRAWLER}
procedure TSetsDatabase.InitYearTable;
var
  sl: TStringList;
  s: string;
  i: integer;
  pci: TPieceColorInfo;
  spiece, scolor, syear: string;
  cl: integer;
  idx: integer;
  yyyy: integer;
  se: TSetExtraInfo;
  fn: string;
begin
  fn := basedefault + 'db\db_pieces_years.txt';
  if fexists(fn) then
  begin
    sl := TStringList.Create;
    S_LoadFromFile(sl, fn);
    if sl.Count > 0 then
      if sl.Strings[0] = 'Part,Color,Year' then
        for i := 1 to sl.Count - 1 do
        begin
          s := sl.Strings[i];
          splitstring(s, spiece, scolor, syear, ',');

          yyyy := atoi(syear);
          if (yyyy > 1931) and (yyyy <= 2050) then
          begin
            spiece := fixpartname(spiece);
            spiece := RebrickablePart(spiece);

            if Pos('BL', scolor) = 1 then
            begin
              scolor := Copy(scolor, 3, Length(scolor) - 2);
              cl := db.BrickLinkColorToSystemColor(StrToIntDef(scolor, 0));
            end
            else if Pos('RB', scolor) = 1 then
            begin
              scolor := Copy(scolor, 3, Length(scolor) - 2);
              cl := db.RebrickableColorToSystemColor(StrToIntDef(scolor, 0));
            end
            else
              cl := atoi(scolor);

            pci := PieceColorInfo(spiece, cl);
            if pci <> nil then
              if pci.year = 0 then
              begin
                pci.year := yyyy;
                pci.canedityear := True;
                pci.UpdatePartYears;
              end;
              
            if cl = -1 then
            begin
              idx := fsets.IndexOf(spiece);
              if idx >= 0 then
              begin
                se := (fsets.Objects[idx] as TSetExtraInfo);
                if se.year = 0 then
                  se.year := yyyy;
              end;
            end;
          end;
        end;
    sl.Free;
  end;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
procedure TSetsDatabase.InitBaseMolds;
var
  sl, sall: TStringList;
  i, j, len: integer;
  base, check, stmp: string;
begin
  fbasemolds.Sorted := False;
  ClearList(fbasemolds);
  fbasemolds.AddObject('', TStringList.Create);

  sall := TStringList.Create;
  try
    for i := 0 to fpieces.Count - 1 do
    begin
      check := fpieces.Strings[i];
      if check <> '' then
        if IsNumericC(check[1]) then
          if Pos('-', check) <= 0 then
            if Pos('stk', check) <= 0 then
              sall.Add(fpieces.Strings[i]);
    end;

    sall.Sort;

    for i := 0 to sall.Count - 1 do
    begin
      stmp := sall.Strings[i];
      base := '';
      for j := 1 to Length(stmp) do
        if IsNumericC(stmp[j]) then
          base := base + stmp[j]
        else
          Break;

      if base = '' then
        Continue;

      if fbasemolds.Strings[fbasemolds.Count - 1] <> base then
      begin
        sl := TStringList.Create;
        sl.Add(stmp);
        fbasemolds.AddObject(base, sl);
      end
      else
        Continue;

      len := Length(base);
      for j := i + 1 to sall.Count - 1 do
      begin
        check := sall.Strings[j];
        if not IsNumeric(check) then
          if Pos(base, check) = 1 then
            if len < Length(check) then
              if not IsNumericC(check[len + 1]) then
              begin
                sl := fbasemolds.Objects[basemolds.Count - 1] as TStringList;
                sl.Add(check);
                Continue;
              end;
        Break;
      end;
    end;
  finally
    sall.Free;
  end;

  fbasemolds.Objects[0].Free;
  fbasemolds.Delete(0);
  for i := fbasemolds.Count - 1 downto 0 do
  begin
    sl := fbasemolds.Objects[i] as TStringList;
    if sl.Count <= 1 then
    begin
      sl.Free;
      fbasemolds.Delete(i);
    end;
  end;

  fbasemolds.Sorted := True;
end;

function TSetsDatabase.BaseMold(const pcs: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(pcs) do
    if IsNumericC(pcs[i]) then
      Result := Result + pcs[i]
    else
      Break;
  if fbasemolds.IndexOf(Result) < 0 then
    Result := '';
end;

function TSetsDatabase.ChildMolds(const pcs: string): TStringList;
var
  idx: integer;
begin
  idx := fbasemolds.IndexOf(pcs);
  if idx < 0 then
  begin
    idx := fbasemolds.IndexOf(RebrickablePart(pcs));
    if idx < 0 then
      idx := fbasemolds.IndexOf(BricklinkPart(pcs));
  end;
  if idx >= 0 then
    Result := fbasemolds.Objects[idx] as TStringList
  else
    Result := nil;
end;

function TSetsDatabase.FamilyMolds(const pcs: string): TStringList;
var
  base: string;
begin
  base := BaseMold(pcs);
  if base = '' then
  begin
    Result := nil;
    Exit;
  end;
  Result := ChildMolds(base);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
procedure TSetsDatabase.InitRelationShips;
var
  fname: string;
  sl: TStringList;
  i: integer;
  styp, schild, sparent: string;
  pic, pip: TPieceInfo;
  progressstring: string;
begin
  fname := basedefault + 'db\db_relationships.txt';
  if not fexists(fname) then
    Exit;

  progressstring := 'Initializing relationships...';

  if Assigned(progressfunc) then
    progressfunc(progressstring, 0.0);

  sl := TStringList.Create;
  try
    S_LoadFromFile(sl, fname);
    if sl.Count > 1 then
      if sl.Strings[0] = 'type,child,parent' then
        for i := 1 to sl.Count - 1 do
        begin
          if i mod 100 = 0 then
            if Assigned(progressfunc) then
              progressfunc(progressstring, i / sl.Count);

          splitstring(sl.Strings[i], styp, schild, sparent, ',');
          if Length(styp) = 1 then
            if styp[1] in ['A', 'M', 'P', 'T'] then
            begin
              pic := PieceInfo(schild);
              if (pic <> nil) and (pic <> fstubpieceinfo) then
              begin
                pip := PieceInfo(sparent);
                if (pip <> nil) and (pip <> fstubpieceinfo) and (pip <> pic) then
                  if pip.name <> pic.name then
                  begin
                    case styp[1] of
                      'A':
                        begin
                          if pic.alternates.IndexOf(pip.name) < 0 then
                            pic.alternates.Add(pip.name);
                          if pip.alternates.IndexOf(pic.name) < 0 then
                            pip.alternates.Add(pic.name);
                        end;
                      'M':
                        begin
                          if pic.moldvariations.IndexOf(pip.name) < 0 then
                            pic.moldvariations.Add(pip.name);
                          if pip.moldvariations.IndexOf(pic.name) < 0 then
                            pip.moldvariations.Add(pic.name);
                        end;
                      'P':
                        begin
                          pic.printof := pip.name;
                          pip.prints.Add(pic.name);
                        end;
                      'T':
                        begin
                          pic.patternof := pip.name;
                          pip.patterns.Add(pic.name);
                        end;
                    end;
                  end;
              end;
            end;
        end;
  finally
    sl.Free;
  end;

  if Assigned(progressfunc) then
    progressfunc(progressstring, 1.0);
end;
{$ENDIF}

function TSetsDatabase.Colors(const i: Integer): colorinfo_p;
begin
  if (i >= 0) and (i <= MAXINFOCOLOR) then
    Result := @fcolors[i]
  else
    Result := @fcolors[-1];
end;

constructor TSetExtraInfo.Create;
begin
  moc := False;
  hasinstructions := False;
  hasoriginalbox := False;
  {$IFNDEF CRAWLER}
  text := '';
  year := 0;
  fixedinstructions := False;
  instructionsdimentionx := 0.0;
  instructionsdimentiony := 0.0;
  instructionsdimentionz := 0.0;
  instructionsweight := 0.0;
  fixedoriginalbox := False;
  originalboxinstructionx := 0.0;
  originalboxinstructiony := 0.0;
  originalboxinstructionz := 0.0;
  originalboxweight := 0.0;
  {$ENDIF}
  Inherited;
end;

procedure TSetsDatabase.InitSets;
var
  i: integer;
  pci: TPieceColorInfo;
  setname: string;

  procedure _loadsets(const fn: string; const ismoc: boolean);
  var
    s: TStringList;
    ss: TSetExtraInfo;
    stmp: string;
    stmp2: string;
    i, p: integer;
    {$IFNDEF CRAWLER}
    idx: integer;
    tx: string;
    tx2: string;
    year: integer;
    {$ENDIF}
    sname: string;
    progressstring: string;
  begin
    if not fexists(fn) then
      Exit;

    if ismoc then
      progressstring := 'Initializing mocs...'
    else
      progressstring := 'Initializing sets...';

    if Assigned(progressfunc) then
      progressfunc(progressstring, 0.0);

    s := TStringList.Create;
    if not fexists(fn) then
    begin
      s.Add('set_id,descr,year');
      S_SaveToFile(s, fn);
    end
    else
      S_LoadFromFile(s, fn);
    stmp := s.Text;
    SetLength(stmp2, Length(stmp));
    for i := 1 to Length(stmp) do
      if stmp[i] = '"' then
        stmp2[i] := ' '
      else
        stmp2[i] := stmp[i];

    s.Text := stmp2;
    if s.Count > 0 then
      if Trim(s.Strings[0]) = 'set_id,descr,year' then
      begin
        for i := 1 to s.Count - 1 do
        begin
          if i mod 200 = 0 then
            if Assigned(progressfunc) then
              progressfunc(progressstring, i / s.Count);

          stmp := s.Strings[i];
          p := Pos(',', stmp);
          if p > 0 then
          begin
            sname := Trim(Copy(stmp, 1, p - 1));
            if fsets.IndexOf(sname) < 0 then
            begin
              ss := TSetExtraInfo.Create;
              ss.Moc := ismoc;
              {$IFNDEF CRAWLER}
              splitstring(Trim(Copy(stmp, p + 1, Length(stmp) - p)), tx, tx2, ',');
              year := atoi(tx2);
              idx := fsets.AddObject(sname, ss);
              (fsets.Objects[idx] as TSetExtraInfo).Text := tx;
              (fsets.Objects[idx] as TSetExtraInfo).Year := year;
              {$ELSE}
              fsets.AddObject(sname, ss);
              {$ENDIF}
            end;
          end;
        end;
      end;
    s.Free;

    if Assigned(progressfunc) then
      progressfunc(progressstring, 1.0);
  end;

  procedure _loadsetasset(const fn: string; const colorcode: integer);
  var
    s: TStringList;
    ss: TSetExtraInfo;
    stmp: string;
    stmp2: string;
    i, idx: integer;
    sCategory: string;
    sNumber: string;
    sName: string;
    sYear: string;
    sWeight: string;
    sDimentions: string;
    {$IFNDEF CRAWLER}
    sDimentionx: string;
    sDimentiony: string;
    sDimentionz: string;
    {$ENDIF}
    progressstring: string;
  begin
    if not fexists(fn) then
      Exit;

    if colorcode = INSTRUCTIONCOLORINDEX then
      progressstring := 'Initializing instructions...'
    else if colorcode = BOXCOLORINDEX then
      progressstring := 'Initializing original boxes...';

    if Assigned(progressfunc) then
      progressfunc(progressstring, 0.0);

    s := TStringList.Create;
    S_LoadFromFile(s, fn);
    stmp := s.Text;
    SetLength(stmp2, Length(stmp));
    for i := 1 to Length(stmp) do
      if stmp[i] = '"' then
        stmp2[i] := ' '
      else
        stmp2[i] := stmp[i];

    s.Text := stmp2;
    if s.Count > 0 then
      if Trim(s.Strings[0]) = 'Category,Number,Name,Year,Weight,Dimensions' then
      begin
        for i := 1 to s.Count - 1 do
        begin
          if i mod 200 = 0 then
            if Assigned(progressfunc) then
              progressfunc(progressstring, i / s.Count);

          stmp := s.Strings[i];
          splitstring(stmp, sCategory, sNumber, sName, sYear, sWeight, sDimentions, ',');

          sNumber := Trim(sNumber);
          idx := fsets.IndexOf(sNumber);
          if idx >= 0 then
          begin
            ss := fsets.Objects[idx] as TSetExtraInfo;
            {$IFNDEF CRAWLER}
            if ss.year = 0 then
              ss.year := atoi(sYear);
            if ss.text = sNumber then
              ss.text := sName;
            splitstring(sDimentions, sDimentionx, sDimentiony, sDimentionz, 'x');
            {$ENDIF}
            if colorcode = INSTRUCTIONCOLORINDEX then
            begin
              ss.hasinstructions := True;
              {$IFNDEF CRAWLER}
              ss.fixedinstructions := True;
              ss.instructionsdimentionx := atof(sDimentionx);
              ss.instructionsdimentiony := atof(sDimentiony);
              ss.instructionsdimentionz := atof(sDimentionz);
              ss.instructionsweight := atof(sWeight);
              {$ENDIF}
            end
            else if colorcode = BOXCOLORINDEX then
            begin
              ss.hasoriginalbox := True;
              {$IFNDEF CRAWLER}
              ss.fixedoriginalbox := True;
              ss.originalboxinstructionx := atof(sDimentionx);
              ss.originalboxinstructiony := atof(sDimentiony);
              ss.originalboxinstructionz := atof(sDimentionz);
              ss.originalboxweight := atof(sWeight);
              {$ENDIF}
            end;
          end;
        end;
      end;
    s.Free;

    if Assigned(progressfunc) then
      progressfunc(progressstring, 1.0);
  end;

  procedure _loadsetextraassets(const fn: string);
  var
    s: TStringList;
    ss: TSetExtraInfo;
    stmp: string;
    stmp2: string;
    i, idx: integer;
    sNumber: string;
    sAsset: string;
    sWeight: string;
    progressstring: string;
  begin
    if not fexists(fn) then
      Exit;

    progressstring := 'Initializing extra assets...';

    if Assigned(progressfunc) then
      progressfunc(progressstring, 0.0);

    s := TStringList.Create;
    S_LoadFromFile(s, fn);
    stmp := s.Text;
    SetLength(stmp2, Length(stmp));
    for i := 1 to Length(stmp) do
      if stmp[i] = '"' then
        stmp2[i] := ' '
      else
        stmp2[i] := stmp[i];

    s.Text := stmp2;
    if s.Count > 0 then
    begin
      if Trim(s.Strings[0]) = 'Set,Asset' then
      begin
        for i := 1 to s.Count - 1 do
        begin
          if i mod 200 = 0 then
            if Assigned(progressfunc) then
              progressfunc(progressstring, i / s.Count);

          stmp := s.Strings[i];
          splitstring(stmp, sNumber, sAsset, ',');

          sNumber := Trim(sNumber);
          idx := fsets.IndexOf(sNumber);
          if idx >= 0 then
          begin
            if sAsset = 'I' then
            begin
              ss := fsets.Objects[idx] as TSetExtraInfo;
              ss.hasinstructions := True;
            end
            else if sAsset = 'B' then
            begin
              ss := fsets.Objects[idx] as TSetExtraInfo;
              ss.hasoriginalbox := True;
            end;
          end;
        end;
      end
      else if Trim(s.Strings[0]) = 'Set,Asset,Weight' then
      begin
        for i := 1 to s.Count - 1 do
        begin
          if i mod 200 = 0 then
            if Assigned(progressfunc) then
              progressfunc(progressstring, i / s.Count);

          stmp := s.Strings[i];
          splitstring(stmp, sNumber, sAsset, sWeight, ',');

          sNumber := Trim(sNumber);
          idx := fsets.IndexOf(sNumber);
          if idx >= 0 then
          begin
            if sAsset = 'I' then
            begin
              ss := fsets.Objects[idx] as TSetExtraInfo;
              ss.hasinstructions := True;
              {$IFNDEF CRAWLER}
              sWeight := Trim(sWeight);
              if sWeight <> '' then
                ss.instructionsweight := atof(sWeight);
              {$ENDIF}
            end
            else if sAsset = 'B' then
            begin
              ss := fsets.Objects[idx] as TSetExtraInfo;
              ss.hasoriginalbox := True;
              {$IFNDEF CRAWLER}
              sWeight := Trim(sWeight);
              if sWeight <> '' then
                ss.originalboxweight := atof(sWeight);
              {$ENDIF}
            end;
          end;
        end;
      end;
    end;
    s.Free;

    if Assigned(progressfunc) then
      progressfunc(progressstring, 1.0);
  end;

begin
  fsets := TStringList.Create;
  fsetshash := THashTable.Create;
  fsets.Sorted := True;

  _loadsets(basedefault + 'db\db_sets.txt', False);
  _loadsets(basedefault + 'db\db_mocs.txt', True);

  _loadsetasset(basedefault + 'db\db_instructions.txt', INSTRUCTIONCOLORINDEX);
  _loadsetasset(basedefault + 'db\db_boxes.txt', BOXCOLORINDEX);

  _loadsetextraassets(basedefault + 'db\db_set_assets.txt');

  fsetshash.AssignStringList(fsets);
  for i := 0 to fsets.Count - 1 do
  begin
    setname := fsets.Strings[i];
    pci := TPieceColorInfo.Create(setname, -1);
    if Pos('-', setname) > 0 then
      pci.parttype := TYPE_SET
    else
      pci.parttype := TYPE_MINIFIGURE;
    fcolors[-1].knownpieces.AddObject(setname, pci);
    if (fsets.Objects[i] as TSetExtraInfo).hasinstructions then
      if fcolors[INSTRUCTIONCOLORINDEX].knownpieces.IndexOf(setname) < 0 then
      begin
        pci := TPieceColorInfo.Create(setname, INSTRUCTIONCOLORINDEX);
        pci.parttype := TYPE_INSTRUCTIONS;
        fcolors[INSTRUCTIONCOLORINDEX].knownpieces.AddObject(setname, pci);
      end;
    if (fsets.Objects[i] as TSetExtraInfo).hasoriginalbox then
      if fcolors[BOXCOLORINDEX].knownpieces.IndexOf(setname) < 0 then
      begin
        pci := TPieceColorInfo.Create(setname, BOXCOLORINDEX);
        pci.parttype := TYPE_BOX;
        fcolors[BOXCOLORINDEX].knownpieces.AddObject(setname, pci);
      end;
  end;
end;

procedure TSetsDatabase.InitGears;
var
  i: integer;
  s: TStringList;
  fname: string;
  pci: TPieceColorInfo;
begin
  fname := basedefault + 'db\db_gears.txt';
  if fexists(fname) then
  begin
    s := TStringList.Create;
    try
      S_LoadFromFile(s, fname);
      for i := 0 to s.Count - 1 do
      begin
        pci := PieceColorInfo(s.Strings[i], -1);
        if pci <> nil then
          pci.parttype := TYPE_GEAR;
      end;
    finally
      s.Free;
    end;
  end;
end;

procedure TSetsDatabase.InitBooks;
var
  i: integer;
  s: TStringList;
  s1, s2, s3, stmp: string;
  fname: string;
  sbook: string;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  {$IFNDEF CRAWLER}
  yyyy: integer;
  {$ENDIF}
  dosave: boolean;
  tmplist: THashStringList;
  idx: integer;
  ts: TString;
  progressstring: string;
begin
  fname := basedefault + 'db\db_books.txt';
  if not fexists(fname) then
    Exit;

  progressstring := 'Loading Books...';
  if Assigned(progressfunc) then
    progressfunc(progressstring, 0);

  dosave := False;
  s := TStringList.Create;
  try
    S_LoadFromFile(s, fname);
    if s.Count > 1 then
    begin
      stmp := s.Strings[0];
      if Trim(stmp) = 'Number,Name,Year' then
      begin
        tmplist := THashStringList.Create;
        try
          for i := 1 to s.Count - 1 do
          begin
            if i mod 50 = 0 then
              if Assigned(progressfunc) then
                progressfunc(progressstring, (i / s.Count) / 2);
            stmp := Trim(s.Strings[i]);
            splitstring(stmp, s1, s2, s3, ',');
            sbook := fixpartname(s1);
            idx := tmplist.IndexOf(sbook);
            if idx < 0 then
            begin
              idx := tmplist.Add(sbook);
              ts := TString.Create;
              ts.text := stmp;
              tmplist.Objects[idx] := ts;
            end
            else
              dosave := True;
          end;
          if dosave then
          begin
            s.Clear;
            s.Add('Number,Name,Year');
            for i := 0 to tmplist.Count - 1 do
              s.Add((tmplist.Objects[i] as TString).text);
            S_BackupFile(fname);
            S_SaveToFile(s, fname);
          end;
        finally
          FreeHashList(tmplist);
        end;
      end;
    end;
  finally
    s.Free;
  end;

  s := TStringList.Create;
  try
    S_LoadFromFile(s, fname);
    if s.Count > 1 then
    begin
      stmp := s.Strings[0];
      if Trim(stmp) = 'Number,Name,Year' then
      begin
        for i := 1 to s.Count - 1 do
        begin
          if i mod 50 = 0 then
            if Assigned(progressfunc) then
              progressfunc(progressstring, (i / s.Count) / 2 + 0.5);
          stmp := Trim(s.Strings[i]);
          splitstring(stmp, s1, s2, s3, ',');
          sbook := fixpartname(s1);
          fallbooks.Add(sbook);
          pi := PieceInfo(sbook);
          if pi <> nil then
          begin
            pci := PieceColorInfo(sbook, -1);
            if pci = nil then
            begin
              pci := TPieceColorInfo.Create(sbook, -1);
              pci.pieceinfo := pi;
              pci.parttype := TYPE_BOOK;
              {$IFNDEF CRAWLER}
              yyyy := atoi(s3);
              pci.year := yyyy;
              {$ENDIF}

              if fColors[-1].knownpieces = nil then
                fColors[-1].knownpieces := THashStringList.Create;
              fColors[-1].knownpieces.AddObject(sbook, pci);
            end
            else
              pci.parttype := TYPE_BOOK;
          end;
        end;
      end;
    end;
  finally
    s.Free;
  end;
  if Assigned(progressfunc) then
    progressfunc(progressstring, 1.0);
end;

procedure TSetsDatabase.InitCatalogs;
var
  i: integer;
  s: TStringList;
  s1, s2, s3, s4, s5, stmp: string;
  fname: string;
  scatalog: string;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  {$IFNDEF CRAWLER}
  yyyy: integer;
  {$ENDIF}
  progressstring: string;
begin
  fname := basedefault + 'db\db_catalogs.txt';
  if not fexists(fname) then
    Exit;

  progressstring := 'Loading Catalogs...';
  if Assigned(progressfunc) then
    progressfunc(progressstring, 0);

  s := TStringList.Create;
  try
    S_LoadFromFile(s, fname);
    if s.Count > 1 then
    begin
      stmp := s.Strings[0];
      if Trim(stmp) = 'Category,Number,Name,Year,Weight' then
      begin
        for i := 1 to s.Count - 1 do
        begin
          if i mod 50 = 0 then
            if Assigned(progressfunc) then
              progressfunc(progressstring, i / s.Count);
          stmp := Trim(s.Strings[i]);
          splitstring(stmp, s1, s2, s3, s4, s5, ',');
          scatalog := fixpartname(s2);
          pi := PieceInfo(scatalog);
          if pi <> nil then
          begin
            pi.weight := atof(s5, 0.0);
            pci := PieceColorInfo(scatalog, CATALOGCOLORINDEX);
            if pci = nil then
              pci := TPieceColorInfo.Create(scatalog, CATALOGCOLORINDEX);
            pci.pieceinfo := pi;
            pci.parttype := TYPE_CATALOG;
            {$IFNDEF CRAWLER}
            yyyy := atoi(s4);
            pci.year := yyyy;
            {$ENDIF}

            if fColors[CATALOGCOLORINDEX].knownpieces = nil then
              fColors[CATALOGCOLORINDEX].knownpieces := THashStringList.Create;
            fColors[CATALOGCOLORINDEX].knownpieces.AddObject(scatalog, pci);
          end;
        end;
      end;
    end;
  finally
    s.Free;
  end;
  if Assigned(progressfunc) then
    progressfunc(progressstring, 1.0);
end;

procedure TSetsDatabase.MarkInventoriedPart(const pcs: string);
begin
  MarkInventoriedPart(PieceInfo(pcs), pcs);
  MarkInventoriedPart(PieceInfo(GetBLNetPieceName(pcs)), pcs);
  MarkInventoriedPart(PieceInfo(BricklinkPart(pcs)), pcs);
  MarkInventoriedPart(PieceInfo(RebrickablePart(pcs)), pcs);
end;

procedure TSetsDatabase.MarkInventoriedPart(const pi: TPieceInfo; const pcs: string);
begin
  if (pi <> nil) and (pi <> fstubpieceinfo) then
  begin
    pi.inventoryfound := True;
    pi.inventoryname := pcs;
  end;
end;

procedure TSetsDatabase.MarkUnInventoriedPart(const pcs: string);
begin
  MarkUnInventoriedPart(PieceInfo(pcs), pcs);
  MarkUnInventoriedPart(PieceInfo(GetBLNetPieceName(pcs)), pcs);
  MarkUnInventoriedPart(PieceInfo(BricklinkPart(pcs)), pcs);
  MarkUnInventoriedPart(PieceInfo(RebrickablePart(pcs)), pcs);
end;

procedure TSetsDatabase.MarkUnInventoriedPart(const pi: TPieceInfo; const pcs: string);
begin
  if (pi <> nil) and (pi <> fstubpieceinfo) then
  begin
    pi.inventoryfound := False;
    pi.inventoryname := '';
  end;
end;

procedure TSetsDatabase.InitPartReferences;
var
  i: integer;
  pregressstring: string;
begin
  pregressstring := 'Loading Parts Inventories...';
  if Assigned(progressfunc) then
    progressfunc(pregressstring, 0.0);
  for i := 0 to fpartsinventories.Count - 1 do
  begin
    inc(fpartsinventoriesvalidcount);
    if Assigned(progressfunc) then
      if i mod 50 = 0 then
        progressfunc(pregressstring, i / fpartsinventories.Count);
    MarkInventoriedPart(fpartsinventories.Strings[i]);
  end;
  if Assigned(progressfunc) then
    progressfunc(pregressstring, 1.0);
end;

procedure TSetsDatabase.InitPiecesInventories;
var
  stmp: TStringList;
  fname: string;
  i: integer;
begin
  fpartsinventories := TStringList.Create;
  fname := basedefault + 'db\db_pieces_inventories.txt';
  if fexists(fname) then
  begin
    stmp := TStringList.Create;
    try
      S_LoadFromFile(stmp, fname);
      for i := 0 to stmp.Count - 1 do
        fpartsinventories.Add(fixpartname(stmp.Strings[i]));
    finally
      stmp.Free;
    end;
  end;
  fpartsinventories.Sorted := True;
end;

procedure TSetsDatabase.InitSetReferences;
var
  i: integer;
  binset: PBinarySetRecord;
  numrecs: integer;
//  fs: TFileStream;
  pci: TPieceColorInfo;
  spiece, scolor, snum, scost, sset: string;
  s: TStringList;
  idx, j: integer;
  cc, num: integer;
  pregressstring: string;
begin
  pregressstring := 'Loading sets...';
  if Assigned(progressfunc) then
    progressfunc(pregressstring, 0.0);
  s := TStringList.Create;
  for i := 0 to fsets.Count - 1 do
  begin
    if i mod 500 = 0 then
      if Assigned(progressfunc) then
        progressfunc(pregressstring, i / fsets.Count);

    sset := fsets.strings[i];
    if fallsets.IndexOf(sset) < 0 then
      if fallsetswithoutextra.IndexOf(sset) < 0 then
      begin
        binset := fbinarysets.GetSet(sset);
        if binset <> nil then
        begin
          numrecs := binset.numitems;
          for j := 0 to numrecs - 1 do
          begin
            spiece := binset.data[j].piece;
            cc := binset.data[j].color;
            num := binset.data[j].num;

            if spiece <> '' then
            begin
              if (cc >= -1) and (cc <= MAXINFOCOLOR) then
              begin
                if fcolors[cc].knownpieces = nil then
                begin
                  fcolors[cc].knownpieces := THashStringList.Create;
                  idx := -1;
                end
                else
                  idx := fcolors[cc].knownpieces.Indexof(spiece);
                if idx < 0 then
                begin
                  pci := TPieceColorInfo.Create(spiece, cc);
                  fcolors[cc].knownpieces.AddObject(spiece, pci);
                end
                else
                  pci := fcolors[cc].knownpieces.Objects[idx] as TPieceColorInfo;
                AddSetPiece(sset, spiece, '1', cc, num, pci);
                pci.AddSetReference(sset, num);
              end;
            end;
          end;
        end
        else if fexists(basedefault + 'db\sets\' + sset + '.txt') then
        begin
         // numrecs := 0;
          S_LoadFromFile(s, basedefault + 'db\sets\' + sset + '.txt');
          for j := 1 to s.Count - 1 do
          begin
            splitstring(s.strings[j], spiece, scolor, snum, scost, ',');

            if Pos('BL ', spiece) = 1 then
              spiece := RebrickablePart(Copy(spiece, 4, Length(spiece) - 3))
            else
              spiece := RebrickablePart(spiece);

            if spiece <> '' then
            begin
              if Pos('BL', scolor) = 1 then
              begin
                scolor := Copy(scolor, 3, Length(scolor) - 2);
                cc := BrickLinkColorToSystemColor(StrToIntDef(scolor, 0));
              end
              else if Pos('RB', scolor) = 1 then
              begin
                scolor := Copy(scolor, 3, Length(scolor) - 2);
                cc := RebrickableColorToSystemColor(StrToIntDef(scolor, 0));
              end
              else
                cc := atoi(scolor);

            {  binset[numrecs].piece := spiece;
              binset[numrecs].color := cc;
              binset[numrecs].num := num;
              binset[numrecs].cost := atof(scost);
              inc(numrecs);       }
              num := atoi(snum);

              if (cc >= -1) and (cc <= MAXINFOCOLOR) then
              begin
                if fcolors[cc].knownpieces = nil then
                begin
                  fcolors[cc].knownpieces := THashStringList.Create;
                  idx := -1;
                end
                else
                  idx := fcolors[cc].knownpieces.Indexof(spiece);
                if idx < 0 then
                begin
                  pci := TPieceColorInfo.Create(spiece, cc);
                  fcolors[cc].knownpieces.AddObject(spiece, pci);
                end
                else
                  pci := fcolors[cc].knownpieces.Objects[idx] as TPieceColorInfo;
                AddSetPiece(sset, spiece, '1', cc, num, pci);
                pci.AddSetReference(sset, num);
              end;
            end;

           { fs := TFileStream.Create(basedefault + 'db\sets\' + sset + '.dat', fmCreate or fmShareExclusive);
            fs.Write(binset, numrecs * SizeOf(TBinarySetItem));
            fs.Free;  }
          end;
          fbinarysets.UpdateSetFromText(sset, s);

        end;
      end;
  end;
  s.Free;
  if Assigned(progressfunc) then
    progressfunc(pregressstring, 1.0);
end;

procedure TSetsDatabase.InitPieces;
var
  s: TStringList;
  sBooks: TStringList;
  sKP: TStringList;
  sCatalogs: TStringList;
  sp: TPieceInfo;
  {$IFNDEF CRAWLER}
  sp2: TPieceInfo;
  {$ENDIF}
  stmp: string;
  stmp2: string;
  i, p: integer;
  s1, s2, s3, s4: string;
  sextra: TStringList;
  needsave: boolean;
  fn: string;
{  spart: string;
  idx: integer;}
begin
  fpieces := TStringList.Create;
  fpieceshash := THashTable.Create;

  s := TStringList.Create;
  S_LoadFromFile(s, basedefault + 'db\db_pieces.txt');
  fn := basedefault + 'db\db_pieces.extra.txt';
  if fexists(fn) then
  begin
    sextra := TStringList.Create;
    try
      S_LoadFromFile(sextra, fn);
      RemoveBlancLines(sextra);
      sextra.Sort;
      needsave := False;
      for i := sextra.Count - 1 downto 1 do
      begin
        if sextra.Strings[i] = sextra.Strings[i - 1] then
        begin
          needsave := True;
          sextra.Delete(i);
        end
        else if Trim(sextra.Strings[i]) = '' then
        begin
          needsave := True;
          sextra.Delete(i);
        end
        else if Pos(',', Trim(sextra.Strings[i])) = 1 then
        begin
          needsave := True;
          sextra.Delete(i);
        end;
      end;
      if needsave then
      begin
        S_BackupFile(fn);
        S_SaveToFile(sextra, fn);
      end;
      s.AddStrings(sextra);
    finally
      sextra.Free;
    end;
  end;

  fn := basedefault + 'db\db_books.txt';
  if fexists(fn) then
  begin
    sBooks := TStringList.Create;
    try
      S_LoadFromFile(sBooks, fn);
      if sBooks.Count > 0 then
      begin
        stmp := sBooks.Strings[0];
        if Trim(stmp) = 'Number,Name,Year' then
        begin
          for i := 1 to sBooks.Count - 1 do
          begin
            splitstring(sBooks.Strings[i], s1, s2, s3, ',');
            s1 := Trim(s1);
            s.Add(s1 + ',' + Trim(s2));
          end;
        end;
      end;
    finally
      sBooks.Free;
    end;
  end;

  fn := basedefault + 'db\db_catalogs.txt';
  if fexists(fn) then
  begin
    sCatalogs := TStringList.Create;
    try
      S_LoadFromFile(sCatalogs, fn);
      if sCatalogs.Count > 0 then
      begin
        stmp := sCatalogs.Strings[0];
        if Trim(stmp) = 'Category,Number,Name,Year,Weight' then
        begin
          for i := 1 to sCatalogs.Count - 1 do
          begin
            splitstring(sCatalogs.Strings[i], s1, s2, s3, s4, ',');
            s2 := Trim(s2);
            s.Add(s2 + ',' + Trim(s3));
          end;
        end;
      end;
    finally
      sCatalogs.Free;
    end;
  end;

  fn := basedefault + 'db\db_knownpieces.txt';
  if fexists(fn) then
  begin
    sKP := TStringList.Create;
    try
      S_LoadFromFile(sKP, fn);
      if sKP.Count > 0 then
      begin
        stmp := sKP.Strings[0];
        if Trim(stmp) = 'Part,Color,Desc' then
        begin
          for i := 1 to sKP.Count - 1 do
          begin
            splitstring(sKP.Strings[i], s1, s2, s3, ',');
            s1 := Trim(s1);
            s.Add(s1 + ',' + Trim(s3));
          end;
        end;
      end;
    finally
      sKP.Free;
    end;
  end;

  stmp := s.Text;
  SetLength(stmp2, Length(stmp));
  for i := 1 to Length(stmp) do
    if stmp[i] = '"' then
      stmp2[i] := ' '
    else
      stmp2[i] := stmp[i];

  s.Text := stmp2;
  if s.Count > 0 then
    if Trim(s.Strings[0]) = 'piece_id,descr' then
    begin
      for i := 1 to s.Count - 1 do
      begin
        stmp := s.Strings[i];
        p := Pos(',', stmp);
        if p > 0 then
        begin
          sp := TPieceInfo.Create;
          sp.name := fixpartname(Trim(Copy(stmp, 1, p - 1)));
          sp.lname := LowerCase(sp.name);
          {$IFNDEF CRAWLER}
          sp.desc := fixdescname(Trim(Copy(stmp, p + 1, Length(stmp) - p)));
          {$ENDIF}
          fpieces.AddObject(sp.name, sp);
        end;
      end;
    end;

  InitPiecesAlias;
  InitNewNames;
  // Remove duplicates
{  for i := 0 to fpieces.Count - 1 do
  begin
    spart := LowerCase(fixpartname(fpieces.Strings[i]));
    idx := fpiecesaliasBL.IndexOfUCS(spart);
    if idx >= 0 then
    begin
      spart := (fpiecesaliasBL.Objects[idx] as TString).text;
      (fpieces.Objects[i] as TPieceInfo).name := spart;
      (fpieces.Objects[i] as TPieceInfo).lname := LowerCase(spart);
      spart := LowerCase(fixpartname(spart));
    end;
    fpieces.Strings[i] := spart;
  end;     }

  fpieces.Sort;
  {$IFNDEF CRAWLER}
  for i := fpieces.Count - 1 downto 1 do
    if fpieces.Strings[i] = fpieces.Strings[i - 1] then
    begin
      sp2 := fpieces.Objects[i - 1] as TPieceInfo;
      if Pos(sp2.name, sp2.desc) = 1 then
      begin
        fpieces.Objects[i - 1].Free;
        fpieces.Delete(i - 1);
      end
      else
      begin
        fpieces.Objects[i].Free;
        fpieces.Delete(i);
      end;
    end;
  {$ELSE}
  for i := fpieces.Count - 1 downto 1 do
    if fpieces.Strings[i] = fpieces.Strings[i - 1] then
    begin
      fpieces.Objects[i].Free;
      fpieces.Delete(i);
    end;
  {$ENDIF}
  for i := 0 to fpieces.Count - 1 do
  begin
    sp := (fpieces.Objects[i] as TPieceInfo);
    s1 := sp.name;
    fpieces.Strings[i] := s1;
  end;

  fpieces.Sorted := True;
  fpieceshash.AssignStringList(fpieces);
  s.Free;

  InitCrawlerLinks;
end;

function TSetsDatabase.PieceAlias(const pcs: string): string;
var
  i, idx: integer;
  alias: TStringList;
  stest: string;
begin
  Result := pcs;
  if fpieces.IndexOf(Result) >= 0 then
    Exit;

  alias := SearchGlobalPieceAlias(pcs);
  if alias = nil then
    Exit;

  for i := 0 to alias.Count - 1 do
  begin
    stest := alias.Strings[i];
    idx := fpieces.IndexOf(stest);
    if idx >= 0 then
    begin
      Result := stest;
      Exit;
    end;
  end;
end;

function TSetsDatabase.SearchGlobalPieceAlias(const pcs: string): TStringList;
var
  idx: integer;
begin
  idx := fpiecesalias.IndexOf(lowercase(pcs));
  if idx < 0 then
    Result := nil
  else
    Result := fpiecesalias.Objects[idx] as TStringList;
end;

procedure TSetsDatabase.InitNewNames;
var
  fnname: string;
  stmp: TStringList;
  i: integer;
  ts: TString;
  s1, s2: string;
begin
  fpiecenewnames := TStringList.Create;
  stmp := TStringList.Create;
  try
    fnname := basedefault + 'db\db_newnames.txt';
    if fexists(fnname) then
      S_LoadFromFile(stmp, fnname)
    else
      stmp.Add('part,newname');

    if stmp.Strings[0] = 'part,newname' then
    begin
      for i := 1 to stmp.Count - 1 do
      begin
        splitstring(stmp.Strings[i], s1, s2, ',');
        s1 := UpperCase(s1);
        ts := TString.Create;
        ts.text := s2;
        fpiecenewnames.AddObject(s1, ts);
      end;
    end;
    fpiecenewnames.Sorted := True;
  finally
    stmp.Free;
  end;
end;

var
  lastgnpnidx: integer = -1;

function TSetsDatabase.GetBLNetPieceName(const pcs: string): string;
begin
  Result := GetNewPieceName(pcs);
  if UpperCase(pcs) = UpperCase(Result) then
    Result := BrickLinkPart(pcs);
end;

function TSetsDatabase.GetNewPieceName(const pcs: string): string;
var
  check: string;
  idx: integer;
begin
  check := UpperCase(pcs);
  if lastgnpnidx >= 0 then
    if lastgnpnidx < fpiecenewnames.Count then
      if fpiecenewnames.Strings[lastgnpnidx] = check then
      begin
        Result := (fpiecenewnames.Objects[lastgnpnidx] as TString).Text;
        Exit;
      end;

  idx := fpiecenewnames.IndexOf(check);
  if idx >= 0 then
  begin
    Result := (fpiecenewnames.Objects[idx] as TString).Text;
    lastgnpnidx := idx;
    Exit;
  end;

  check := UpperCase(BrickLinkPart(pcs));
  idx := fpiecenewnames.IndexOf(check);
  if idx >= 0 then
  begin
    Result := (fpiecenewnames.Objects[idx] as TString).Text;
    lastgnpnidx := idx;
    Exit;
  end;

  check := UpperCase(RebrickablePart(pcs));
  idx := fpiecenewnames.IndexOf(check);
  if idx >= 0 then
  begin
    Result := (fpiecenewnames.Objects[idx] as TString).Text;
    lastgnpnidx := idx;
    Exit;
  end;

  Result := pcs;
  lastgnpnidx := -1;
end;

procedure TSetsDatabase.SetNewPieceName(const pcs: string; const newname: string);
var
  check: string;
  idx: integer;
  ts: TString;
begin
  check := UpperCase(pcs);
  idx := fpiecenewnames.IndexOf(check);
  if idx >= 0 then
  begin
    if (newname = '') or (newname = pcs) then
    begin
      fpiecenewnames.Objects[idx].Free;
      fpiecenewnames.Delete(idx);
    end
    else
      (fpiecenewnames.Objects[idx] as TString).Text := newname;
    SaveNewNames;
    Exit;
  end;

  check := UpperCase(BrickLinkPart(pcs));
  idx := fpiecenewnames.IndexOf(check);
  if idx >= 0 then
  begin
    if (newname = '') or (newname = pcs) then
    begin
      fpiecenewnames.Objects[idx].Free;
      fpiecenewnames.Delete(idx);
    end
    else
      (fpiecenewnames.Objects[idx] as TString).Text := newname;
    SaveNewNames;
    Exit;
  end;

  check := UpperCase(RebrickablePart(pcs));
  idx := fpiecenewnames.IndexOf(check);
  if idx >= 0 then
  begin
    if (newname = '') or (newname = pcs) then
    begin
      fpiecenewnames.Objects[idx].Free;
      fpiecenewnames.Delete(idx);
    end
    else
      (fpiecenewnames.Objects[idx] as TString).Text := newname;
    SaveNewNames;
    Exit;
  end;

  if (newname = '') or (newname = pcs) then
    Exit;

  ts := TString.Create;
  ts.Text := newname;
  fpiecenewnames.AddObject(UpperCase(pcs), ts);
  SaveNewNames;
end;

procedure TSetsDatabase.UnSetNewPieceName(const pcs: string; const newname: string);
var
  check: string;
  idx: integer;
  ts: TString;
begin
  if (newname = '') or (newname = pcs) then
    Exit;

  check := UpperCase(pcs);
  idx := fpiecenewnames.IndexOf(check);
  if idx < 0 then
  begin
    check := UpperCase(RebrickablePart(pcs));
    idx := fpiecenewnames.IndexOf(check);
    if idx < 0 then
      Exit;
  end;

  ts := fpiecenewnames.Objects[idx] as TString;
  if ts.text <> newname then
    Exit;

  ts.Free;
  fpiecenewnames.Delete(idx);
  SaveNewNames;
end;

procedure TSetsDatabase.SaveNewNames;
var
  fnname: string;
  stmp: TStringList;
  i: integer;
begin
  stmp := TStringList.Create;
  try
    stmp.Add('part,newname');
    for i := 0 to fpiecenewnames.Count - 1 do
      stmp.Add(fpiecenewnames.Strings[i] + ',' + (fpiecenewnames.Objects[i] as TString).text);
    fnname := basedefault + 'db\db_newnames.txt';
    S_BackupFile(fnname);
    S_SaveToFile(stmp, fnname);
  finally
    stmp.Free;
  end;
end;

procedure TSetsDatabase.InitPiecesAlias;
var
  s: TStringList;
  i, p: integer;
  stmp: string;
  s1, s2: string;
  ss: TString;
  ok: boolean;

  procedure AddGlobalAlias(const x, y: string);
  var
    SL: TStringList;
    idx: integer;
    idx2: integer;
    x2: string;
  begin
    x2 := LowerCase(x);
    idx := fpiecesalias.IndexOf(x2);
    if idx < 0 then
    begin
      idx := fpiecesalias.Add(x2);
      SL := TStringList.Create;
      SL.Add(x);
      fpiecesalias.Objects[idx] := SL;
    end
    else
      SL := fpiecesalias.Objects[idx] as TStringList;
    idx2 := SL.IndexOf(y);
    if idx2 < 0 then
      SL.Add(y);
  end;

begin
  fpiecesalias := THashStringList.Create;
  fpiecesaliasBL := THashStringList.Create;
  fpiecesaliasRB := THashStringList.Create;
  s := TStringList.Create;

  for i := 1 to 10 do
  begin
    ok := True;
    try
      S_LoadFromFile(s, basedefault + 'db\db_pieces_alias.txt');
    except
      ok := False;
      Sleep(150);
    end;
    if ok then
      break;
  end;

  if s.Count > 0 then
    if Trim(s.Strings[0]) = 'bricklink,rebricable' then
    begin
      for i := s.Count - 1 downto 1 do
      begin
        stmp := s.Strings[i];
        p := Pos(',', stmp);
        if p > 0 then
        begin
          s1 := fixpartname(Trim(Copy(stmp, p + 1, Length(stmp) - p)));
          s2 := fixpartname(Trim(Copy(stmp, 1, p - 1)));
          // jval Maybe remove the following if
          if (fpiecesaliasBL.IndexOf(s1) < 0) and (fpiecesaliasRB.IndexOf(s2) < 0) then
          begin
            ss := TString.Create;
            ss.Text := s2;
            fpiecesaliasBL.AddObject(s1, ss);

            ss := TString.Create;
            ss.Text := s1;
            fpiecesaliasRB.AddObject(s2, ss);
          end;
          AddGlobalAlias(s1, s2);
          AddGlobalAlias(s2, s1);
        end;
      end;
    end;

{  fpiecesaliasBL.Sorted := True;
  fpiecesaliasRB.Sorted := True;}
  s.Free;
end;

procedure TSetsDatabase.InitCrawlerLinks;
var
  s: TStringList;
  i, idx: integer;
  stmp: string;
  s1, s2, s3: string;
  ss: TString;
begin
  fCrawlerLinks := TStringList.Create;
  s := TStringList.Create;
  S_LoadFromFile(s, basedefault + 'db\db_crawlerlinks.txt');

  if s.Count > 0 then
    if Trim(s.Strings[0]) = 'part,color,bllink' then
    begin
      for i := s.Count - 1 downto 1 do
      begin
        stmp := Trim(s.Strings[i]);
        splitstring(stmp, s1, s2, s3, ',');
        if fCrawlerLinks.IndexOf(s1 + ',' + s2) < 0 then
        begin
          ss := TString.Create;
          idx := fCrawlerLinks.AddObject(s1 + ',' + s2, ss);
          (fCrawlerLinks.Objects[idx] as TString).Text := s3;
        end;
      end;
    end;

  fCrawlerLinks.Sorted := True;
  s.Free;
end;


procedure TSetsDatabase.AddPieceAlias(const bl, rb: string);
var
  s: TStringList;
  ch: string;
  idx: integer;
  i: integer;
  ok: boolean;
begin
  if strtrim(bl) <> '' then
    AddPieceAlias('', rb);

  s := TStringList.Create;

  for i := 1 to 10 do
  begin
    ok := True;
    try
      S_LoadFromFile(s, basedefault + 'db\db_pieces_alias.txt');
    except
      ok := False;
      Sleep(150);
    end;
    if ok then
      break;
  end;
  // Rebrickable part = SPACES, delete reference
  if strtrim(bl) = '' then
  begin
    ch := fixpartname(strtrim(BricklinkPart(rb))) + ',' + fixpartname(strtrim(rb));
    idx := s.IndexOf(ch);
    if idx > 0 then
    begin
      s.delete(idx);
      FreeHashList(fpiecesalias);
      FreeHashList(fpiecesaliasBL);
      FreeHashList(fpiecesaliasRB);
      S_BackupFile(basedefault + 'db\db_pieces_alias.txt');
      S_SaveToFile(s, basedefault + 'db\db_pieces_alias.txt');
      InitPiecesAlias;
    end;
  end
  else
  begin
    if fixpartname(strtrim(strupper(bl))) <> fixpartname(strtrim(strupper(rb))) then
    begin
      ch := fixpartname(strtrim(bl)) + ',' + fixpartname(strtrim(rb));
      if s.IndexOf(ch) < 0 then
      begin
        FreeHashList(fpiecesalias);
        FreeHashList(fpiecesaliasBL);
        FreeHashList(fpiecesaliasRB);
        s.Add(ch);
        S_BackupFile(basedefault + 'db\db_pieces_alias.txt');
        S_SaveToFile(s, basedefault + 'db\db_pieces_alias.txt');
        InitPiecesAlias;
      end;
    end;
  end;
  s.Free;
end;

procedure TSetsDatabase.AddCrawlerLink(const part: string; const color: integer; const link: string);
var
  ch: string;
  idx: integer;
  s: TStringList;
  i: integer;
begin
  FreeList(fCrawlerLinks);
  InitCrawlerLinks;
  // link = SPACES, delete reference
  if strtrim(link) = '' then
  begin
    ch := strtrim(part) + ',' + itoa(color);
    idx := fCrawlerLinks.IndexOf(ch);
    if idx > 0 then
    begin
      fCrawlerLinks.Objects[idx].Free;
      fCrawlerLinks.delete(idx);
    end
    else
      Exit;
  end
  else
  begin
    ch := strtrim(part) + ',' + itoa(color);
    idx := fCrawlerLinks.IndexOf(ch);
    if idx < 0 then
      idx := fCrawlerLinks.AddObject(ch, TString.Create);
    (fCrawlerLinks.Objects[idx] as TString).Text := link;
  end;
  s := TStringList.Create;
  s.Add('part,color,bllink');
  for i := 0 to fCrawlerLinks.Count - 1 do
    s.Add(fCrawlerLinks.Strings[i] + ',' + (fCrawlerLinks.Objects[i] as TString).Text);
  S_BackupFile(basedefault + 'db\db_crawlerlinks.txt');
  S_SaveToFile(s, basedefault + 'db\db_crawlerlinks.txt');
  s.Free;
end;

function TSetsDatabase.CrawlerLink(const part: string; const color: integer): string;
var
  ch: string;
  idx: integer;
begin
  ch := strtrim(part) + ',' + itoa(color);
  idx := fCrawlerLinks.IndexOf(ch);
  if idx >= 0 then
    Result := strtrim((fCrawlerLinks.Objects[idx] as TString).Text)
  else
    Result := '';
end;

function TSetsDatabase.BrickLinkPart(const s: string): string;
var
  idx: integer;
  spiece: string;
begin
  spiece := Trim(s);
  if fpiecesaliasBL = nil then
  begin
    Result := spiece;
    Exit;
  end;
  idx := IndexOfString(fpieceshash, spiece);
  if idx < 0 then
  begin
    Result := spiece;
    Exit;
  end;
  idx := fpiecesaliasBL.IndexOf(fixpartname(spiece));
  if idx > -1 then
    Result := fixpartname((fpiecesaliasBL.Objects[idx] as TString).Text)
  else
    Result := fixpartname(spiece);
end;

function TSetsDatabase.BrickLinkColorToSystemColor(const c: integer): integer;
begin
  if c >=0 then
    if c <= MAXBRICKLINKCOLOR then
    begin
      Result := fbricklinkcolortosystemcolor[c];
      Exit;
    end;
  if (c = INSTRUCTIONCOLORINDEX) or (c = BOXCOLORINDEX) or (c = CATALOGCOLORINDEX) then
  begin
    Result := c;
    Exit;
  end;
  Result := 0;
end;

function TSetsDatabase.RebrickableColorToSystemColor(const c: integer): integer;
begin
  if c >=0 then
    if c <= MAXREBRICKABLECOLOR then
    begin
      Result := frebrickablecolortosystemcolor[c];
      Exit;
    end;
  if (c = INSTRUCTIONCOLORINDEX) or (c = BOXCOLORINDEX) or (c = CATALOGCOLORINDEX) then
  begin
    Result := c;
    Exit;
  end;
  Result := 0;
end;

function TSetsDatabase.RebrickablePart(const s: string): string;
var
  idx: integer;
  spiece: string;
begin
  spiece := Trim(s);
  if fpiecesaliasRB = nil then
  begin
    Result := spiece;
    Exit;
  end;
  idx := fpiecesaliasRB.IndexOf(fixpartname(spiece));
  if idx > -1 then
    Result := fixpartname((fpiecesaliasRB.Objects[idx] as TString).Text)
  else
    Result := fixpartname(spiece);
  idx := IndexOfString(fpieceshash, Result);
  if idx < 0 then
  begin
    Result := spiece;
    Exit;
  end;
end;

{$IFNDEF CRAWLER}
function TSetsDatabase.SetDesc(const s: string): string;
var
  idx: integer;
begin
  idx := fsetshash.GetPos(s);
  if idx = -1 then
  begin
    Result := '';
    Exit;
  end;
  if fsets.Strings[idx] = s then
    Result := RemoveSpecialTagsFromString((fsets.Objects[idx] as TSetExtraInfo).Text)
  else
  begin
    idx := fsets.IndexOf(s);
    if idx >= 0 then
      Result := RemoveSpecialTagsFromString((fsets.Objects[idx] as TSetExtraInfo).Text)
    else
      Result := '';
  end;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.SetYear(const s: string): integer;
var
  idx: integer;
begin
  idx := fsetshash.GetPos(s);
  if idx = -1 then
  begin
    Result := 0;
    Exit;
  end;
  if fsets.Strings[idx] = s then
    Result := (fsets.Objects[idx] as TSetExtraInfo).year
  else
  begin
    idx := fsets.IndexOf(s);
    if idx >= 0 then
      Result := (fsets.Objects[idx] as TSetExtraInfo).year
    else
      Result := 0;
  end;
end;
{$ENDIF}

function TSetsDatabase.IsMoc(const s: string): boolean;
var
  idx: integer;
begin
  idx := fsetshash.GetPos(s);
  if idx = -1 then
  begin
    Result := False;
    Exit;
  end;
  if fsets.Strings[idx] = s then
    Result := (fsets.Objects[idx] as TSetExtraInfo).moc
  else
  begin
    idx := fsets.IndexOf(s);
    if idx >= 0 then
      Result := (fsets.Objects[idx] as TSetExtraInfo).moc
    else
      Result := False;
  end;
end;

{$IFNDEF CRAWLER}
function TSetsDatabase.SetListAtYear(const y: integer): TStringList;
var
  i: integer;
begin
  Result := TStringList.Create;
  for i := 0 to AllSets.Count - 1 do
    if not IsMoc(AllSets.Strings[i]) then
      if SetYear(AllSets.Strings[i]) = y then
        Result.Add(AllSets.Strings[i]);
end;
{$ENDIF}

function TSetsDatabase.SetListWithInvLotsBetween(const a, b: integer): TStringList;
var
  i: integer;
  inv: TBrickInventory;
  a1, b1: integer;
  n: integer;
begin
  if a <= b then
  begin
    a1 := a;
    b1 := b;
  end
  else
  begin
    a1 := b;
    b1 := a;
  end;
  Result := TStringList.Create;
  for i := 0 to AllSets.Count - 1 do
  begin
    inv := GetSetInventory(AllSets.Strings[i]);
    if inv <> nil then
    begin
      n := inv.numlooseparts;
      if (n >= a1) and (n <= b1) then
        Result.Add(AllSets.Strings[i]);
    end;
  end;
end;

function TSetsDatabase.SetListWithInvPartsBetween(const a, b: integer): TStringList;
var
  i: integer;
  inv: TBrickInventory;
  a1, b1: integer;
  n: integer;
begin
  if a <= b then
  begin
    a1 := a;
    b1 := b;
  end
  else
  begin
    a1 := b;
    b1 := a;
  end;
  Result := TStringList.Create;
  for i := 0 to AllSets.Count - 1 do
  begin
    inv := GetSetInventory(AllSets.Strings[i]);
    if inv <> nil then
    begin
      n := inv.totallooseparts;
      if (n >= a1) and (n <= b1) then
        Result.Add(AllSets.Strings[i]);
    end;
  end;
end;

function TSetsDatabase.PieceListForSets(const slist: TStringList): TStringList;
var
  i: integer;
  tmpinv: TBrickInventory;
begin
  Result := TStringList.Create;
  tmpinv := TBrickInventory.Create;
  for i := 0 to slist.Count - 1 do
    tmpinv.MergeWith(GetSetInventory(slist.Strings[i]));
  tmpinv.Reorganize;
  for i := 0 to tmpinv.numlooseparts - 1 do
    Result.Add(tmpinv.looseparts[i].part + ',' + itoa(tmpinv.looseparts[i].color));
  tmpinv.Free;
end;

{$IFNDEF CRAWLER}
function TSetsDatabase.PieceListForYear(const y: integer): TStringList;
var
  l: TStringList;
begin
  l := SetListAtYear(y);
  Result := PieceListForSets(l);
  l.Free;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.PieceDesc(const s: string): string;
var
  idx: integer;
begin
  idx := fpieceshash.GetPos(s);
  if idx = -1 then
  begin
    idx := fpieceshash.GetPos(BrickLinkPart(s));
    if idx = -1 then
    begin
      idx := fpieceshash.GetPos(RebrickablePart(s));
      if idx = -1 then
      begin
        Result := '';
        Exit;
      end;
    end;
  end;
  Result := RemoveSpecialTagsFromString((fpieces.Objects[idx] as TPieceInfo).desc);
end;

function TSetsDatabase.PieceDesc(const pi: TPieceInfo): string;
begin
  if pi = nil then
  begin
    Result := '';
    Exit;
  end;

  Result := RemoveSpecialTagsFromString(pi.desc);
end;

{$ENDIF}

destructor TSetsDatabase.Destroy;
var
  i: integer;
begin
  FlashPartTypes;
  fPartTypeList.Free;

  fcurrencies.Free;
  fcurrencyconvert.Free;

  {$IFNDEF CRAWLER}
  SaveStorage;
  {$ENDIF}

  for i := -1 to MAXINFOCOLOR do
    FreeHashList(fcolors[i].knownpieces);

  for i := 0 to MAXCATEGORIES - 1 do
    fcategories[i].knownpieces.Free;

  FreeList(fpiecenewnames);
  FreeHashList(fpiecesalias);
  FreeHashList(fpiecesaliasBL);
  FreeHashList(fpiecesaliasRB);
  FreeList(fCrawlerLinks);
  FreeHashList(fallsets);
  FreeHashList(fallsetswithoutextra);
  FreeHashList(fallbooks);
  FreeList(fpieces);
  FreeList(fpiececodes);
  {$IFNDEF CRAWLER}
  FreeList(fbasemolds);
  {$ENDIF}
  fpieceshash.Free;
  fsetshash.Free;
  fcolorpieces.Free;
  try
    S_BackupFile(basedefault + 'cache\' + fcrawlerfilename);
    S_SaveToFile(fcrawlerpriority, basedefault + 'cache\' + fcrawlerfilename);
  except
    I_Warning('fcrawlerpriority.SaveToFile(): Can not save tmp file'#13#10);
  end;
  {$IFNDEF CRAWLER}
  FreeList(fstorage);
  {$ENDIF}
  fcrawlerpriority.Free;
  fstubpieceinfo.Free;
  FreeList(fsets);
  fCacheDB.Free;
  fbinarysets.Free;
  fbinaryparts.Free;
  fpartsinventories.Free;
  ffixedblcolors.Free;
  fCrawlerCache.Free;
  {$IFNDEF CRAWLER}
  fStorageBinsCache.Free;
  {$ENDIF}
  fcrawlerhistory.Free;
  {$IFNDEF CRAWLER}
  S_ShutDownFileSystem;
  {$ENDIF}
  inherited;
end;

procedure TSetsDatabase.SaveCrawlerData;
begin
  if fcrawlerpriority <> nil then
  begin
    try
      S_BackupFile(basedefault + 'cache\' + fcrawlerfilename);
      S_SaveToFile(fcrawlerpriority, basedefault + 'cache\' + fcrawlerfilename);
    except
      I_Warning('fcrawlerpriority.SaveToFile(): Can not save tmp file'#13#10);
    end;
  end;
end;

{$IFNDEF CRAWLER}
procedure TSetsDatabase.SaveStorage;
begin
  S_BackupFile(basedefault + 'db\db_storage.txt');
  S_SaveToFile(fstorage, basedefault + 'db\db_storage.txt');
end;

procedure TSetsDatabase.LoadStorage;
var
  i: integer;
  s1: TStringList;
  s2: TStringList;
  pt, cls: string;
  cl: integer;
  pci: TPieceColorInfo;
  x1, x2: string;
  pregressstring: string;
  fn: string;
begin
  fstorage.Clear;
  fstorage.Add('Part,Color,Storage');
  fn := basedefault + 'db\db_storage.txt';
  if not fexists(fn) then
    Exit;
  s1 := TStringList.Create;
  S_LoadFromFile(s1, fn);
  if s1.Count <= 1 then
  begin
    s1.Free;
    Exit;
  end;
  if s1.Strings[0] <> 'Part,Color,Storage' then
  begin
    s1.Free;
    Exit;
  end;
  pregressstring := 'Loading Storage...';
  if Assigned(progressfunc) then
    progressfunc(pregressstring, 0.0);
  for i := 1 to s1.Count - 1 do
  begin
    splitstring(s1.strings[i], x1, x2, ',');
    x1 := RebrickablePart(x1);
    s1.Strings[i] := x1 + ',' + x2;
    s2 := string2stringlist(s1.Strings[i], ',');
    if s2.Count > 2 then
    begin
      pt := s2.Strings[0];
      cls := s2.Strings[1];
      cl := atoi(cls);
      pci := PieceColorInfo(pt, cl);

      if pci = nil then
      begin
        pci := TPieceColorInfo.Create(pt, cl);
        if fcolors[cl].knownpieces = nil then
          fcolors[cl].knownpieces := THashStringList.Create;
        fcolors[cl].knownpieces.AddObject(pt, pci);
        pci.Load;
      end;

      if pci <> nil then
      begin
        fstorage.Add(s1.Strings[i]);
        s2.Delete(0);
        s2.Delete(0);
        pci.Storage.Text := s2.Text;
      end;
    end;
    s2.Free;
    if Assigned(progressfunc) then
      if i mod 10 = 0 then
        progressfunc(pregressstring, i / s1.Count);
  end;
  if Assigned(progressfunc) then
    progressfunc(pregressstring, 1.0);
  s1.Free;
end;

function TSetsDatabase.CheckStorageReport: TStringList;
var
  i: integer;
  s, s2: TStringList;
  spart, scolor, sstorage: string;
  spartcolor: string;
  idx: integer;
  fn: string;
begin
  Result := TStringList.Create;
  fn := basedefault + 'db\db_storage.txt';
  if not fexists(fn) then
    Exit;

  s := TStringList.Create;
  S_LoadFromFile(s, fn);
  if s.Count <= 1 then
  begin
    s.Free;
    Exit;
  end;
  if s.Strings[0] <> 'Part,Color,Storage' then
  begin
    s.Free;
    Exit;
  end;

  for i := 1 to s.Count - 1 do
  begin
    splitstring(s.strings[i], spart, scolor, sstorage, ',');
    spart := Trim(spart);
    if spart <> '' then
    begin
      spart := RebrickablePart(spart);
      spartcolor := spart + ',' + scolor;
      idx := Result.IndexOf(spartcolor);
      if idx < 0 then
        idx := Result.AddObject(spartcolor, TStringList.Create);
      s2 := Result.Objects[idx] as TStringList;
      s2.Add(sstorage);
    end;
  end;

  s.Free;
end;

function TSetsDatabase.StorageBins: TStringlist;
var
  i, j: integer;
  s1, s2: TStringList;
  s_Storage, s_Num, s_Remarks, stmp: string;
  fn: string;
begin
  Result := TStringList.Create;
  fn := basedefault + 'db\db_storage.txt';
  if not fexists(fn) then
    Exit;
  s1 := TStringList.Create;
  S_LoadFromFile(s1, fn);
  if s1.Count <= 1 then
  begin
    s1.Free;
    Exit;
  end;
  if s1.Strings[0] <> 'Part,Color,Storage' then
    Exit;
  for i := 1 to s1.Count - 1 do
  begin
    s2 := string2stringlist(s1.Strings[i], ',');
    for j := 2 to s2.Count - 1 do
    begin
      stmp := s2.Strings[j];
      splitstring(stmp, s_Storage, s_Num, s_Remarks, ':');
      if Trim(s_Storage) <> '' then
        if Result.IndexOf(s_Storage) < 0 then
          Result.Add(s_Storage);
    end;
    s2.Free;
  end;
  s1.Free;
  Result.Sort;
end;

function TSetsDatabase.StorageBinsForMold(const mld: string): TStringlist;
var
  i, j: integer;
  s1, s2: TStringList;
  s_Storage, s_Num, s_Remarks, stmp: string;
  ch: char;
  check: string;
  fn: string;
begin
  Result := TStringList.Create;
  fn := basedefault + 'db\db_storage.txt';
  if not fexists(fn) then
    Exit;
  s1 := TStringList.Create;
  S_LoadFromFile(s1, fn);
  if s1.Count <= 1 then
  begin
    s1.Free;
    Exit;
  end;
  if s1.Strings[0] <> 'Part,Color,Storage' then
    Exit;
  check := Trim(mld);
  if check = '' then
    Exit;
  ch := check[1];
  for i := 1 to s1.Count - 1 do
  begin
    if s1.Strings[i] <> '' then
      if s1.Strings[i][1] = ch then
      begin
        s2 := string2stringlist(s1.Strings[i], ',');
        if s2.Count > 0 then
          if s2.Strings[0] = mld then
            for j := 2 to s2.Count - 1 do
            begin
              stmp := s2.Strings[j];
              splitstring(stmp, s_Storage, s_Num, s_Remarks, ':');
              if Result.IndexOf(s_Storage) < 0 then
                Result.Add(s_Storage);
            end;
        s2.Free;
      end;
  end;
  s1.Free;
  Result.Sort;
end;

function TSetsDatabase.StorageBinsForPart(const pcs: string; const color: integer): TStringlist;
var
  i, j: integer;
  s1, s2: TStringList;
  s_Storage, s_Num, s_Remarks, stmp: string;
  ch: char;
  check: string;
  scolor: string;
  fn: string;
begin
  Result := TStringList.Create;
  fn := basedefault + 'db\db_storage.txt';
  if not fexists(fn) then
    Exit;
  s1 := TStringList.Create;
  S_LoadFromFile(s1, fn);
  if s1.Count <= 1 then
  begin
    s1.Free;
    Exit;
  end;
  if s1.Strings[0] <> 'Part,Color,Storage' then
    Exit;
  check := Trim(pcs);
  if check = '' then
    Exit;
  ch := check[1];
  scolor := itoa(color);
  for i := 1 to s1.Count - 1 do
  begin
    if s1.Strings[i] <> '' then
      if s1.Strings[i][1] = ch then
      begin
        s2 := string2stringlist(s1.Strings[i], ',');
        if s2.Count > 1 then
          if s2.Strings[0] = pcs then
            if  s2.Strings[1] = scolor then
            for j := 2 to s2.Count - 1 do
            begin
              stmp := s2.Strings[j];
              splitstring(stmp, s_Storage, s_Num, s_Remarks, ':');
              if Result.IndexOf(s_Storage) < 0 then
                Result.Add(s_Storage);
            end;
        s2.Free;
      end;
  end;
  s1.Free;
  Result.Sort;
end;

function TSetsDatabase.InventoryForStorageBin(const st: string): TBrickInventory;
var
  i, j: integer;
  s1, s2: TStringList;
  s_Part, s_Color, s_Storage, s_Num, s_Remarks, stmp: string;
  ss: string;
  fn: string;
begin
  Result := TBrickInventory.Create;
  fn := basedefault + 'db\db_storage.txt';
  if not fexists(fn) then
    Exit;
  s1 := TStringList.Create;
  S_LoadFromFile(s1, fn);
  if s1.Count <= 1 then
  begin
    s1.Free;
    Exit;
  end;
  if s1.Strings[0] <> 'Part,Color,Storage' then
    Exit;
  for i := 1 to s1.Count - 1 do
  begin
    splitstring(s1.Strings[i], s_Part, s_Color, ss, ',');
    s2 := string2stringlist(ss, ',');
    for j := 0 to s2.Count - 1 do
    begin
      stmp := s2.Strings[j];
      splitstring(stmp, s_Storage, s_Num, s_Remarks, ':');
      if s_Storage = st then
        if s_Num <> '' then
          Result.AddLoosePart(s_Part, atoi(s_Color), atoi(s_Num));
    end;
    s2.Free;
  end;
  s1.Free;
end;

function TSetsDatabase.InventoryForStorageBinCache(const st: string): TBrickInventory;
var
  i, j: integer;
  s2: TStringList;
  stmp, s_Part, s_Color, s_Remarks, s_Storage, s_Num, ss: string;
begin
  Result := TBrickInventory.Create;
  if fStorageBinsCache.Count <= 0 then
    Exit;
  if fStorageBinsCache.Strings[0] <> 'Part,Color,Storage' then
    Exit;
  for i := 1 to fStorageBinsCache.Count - 1 do
  begin
    splitstring(fStorageBinsCache.Strings[i], s_Part, s_Color, ss, ',');
    s2 := string2stringlist(ss, ',');
    for j := 0 to s2.Count - 1 do
    begin
      stmp := s2.Strings[j];
      splitstring(stmp, s_Storage, s_Num, s_Remarks, ':');
      if s_Storage = st then
        if s_Num <> '' then
          Result.AddLoosePart(s_Part, atoi(s_Color), atoi(s_Num));
    end;
    s2.Free;
  end;
end;

procedure TSetsDatabase.FetchStorageBinsCache;
var
  fn: string;
begin
  fStorageBinsCache.Clear;
  fn := basedefault + 'db\db_storage.txt';
  if not fexists(fn) then
    Exit;
  S_LoadFromFile(fStorageBinsCache, fn);
end;

function TSetsDatabase.InventoryForAllStorageBins: TBrickInventory;
var
  i, j: integer;
  s1, s2: TStringList;
  s_Part, s_Color, s_Storage, s_Num, s_Remarks, stmp: string;
  ss: string;
  fn: string;
begin
  Result := TBrickInventory.Create;
  fn := basedefault + 'db\db_storage.txt';
  if not fexists(fn) then
    Exit;
  s1 := TStringList.Create;
  S_LoadFromFile(s1, fn);
  if s1.Count <= 1 then
  begin
    s1.Free;
    Exit;
  end;
  if s1.Strings[0] <> 'Part,Color,Storage' then
    Exit;
  for i := 1 to s1.Count - 1 do
  begin
    splitstring(s1.Strings[i], s_Part, s_Color, ss, ',');
    s2 := string2stringlist(ss, ',');
    for j := 0 to s2.Count - 1 do
    begin
      stmp := s2.Strings[j];
      splitstring(stmp, s_Storage, s_Num, s_Remarks, ':');
      if s_Num <> '' then
        Result.AddLoosePart(s_Part, atoi(s_Color), atoi(s_Num));
    end;
    s2.Free;
  end;
  s1.Free;
end;

procedure TSetsDatabase.SetPieceStorage(const piece: string; const color: integer; const st: TStringList);
var
  pci: TPieceColorInfo;
  s: string;
  len: integer;
  i, j: integer;
  idx: integer;
begin
  pci := PieceColorInfo(piece, color);
  if pci <> nil then
  begin
    for i := 0 to st.Count - 1 do
    begin
      s := st.Strings[i];
      for j := 1 to length(s) do
        if s[j] = ',' then
          s[j] := ';';
      st.Strings[i] := s;
    end;
    pci.storage.Text := st.Text;
    s := Format('%s,%d,', [piece, color]);
    len := Length(s);
    idx := -1;
    for i := 1 to fstorage.Count - 1 do
      if LeftStr(fstorage.Strings[i], len) = s then
      begin
        idx := i;
        break;
      end;
    if idx = -1 then
      fstorage.Add(s + stringlist2string(st, ','))
    else
      fstorage.Strings[idx] := s + stringlist2string(st, ',');
  end;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
procedure TSetsDatabase.RefreshAllSetsYears;
var
  i: integer;
begin
  for i := 0 to fallsets.Count - 1 do
    RefreshSetYears(fallsets.Strings[i]);
end;
{$ENDIF}

procedure TSetsDatabase.RefreshAllSetsAssets;
var
  i: integer;
  setname: string;
begin
  for i := 0 to fsets.Count - 1 do
  begin
    setname := fsets.Strings[i];
    if not (fsets.Objects[i] as TSetExtraInfo).hasinstructions then
      if fcolors[INSTRUCTIONCOLORINDEX].knownpieces.IndexOf(setname) >= 0 then
        (fsets.Objects[i] as TSetExtraInfo).hasinstructions := True;
    if not (fsets.Objects[i] as TSetExtraInfo).hasoriginalbox then
      if fcolors[BOXCOLORINDEX].knownpieces.IndexOf(setname) >= 0 then
        (fsets.Objects[i] as TSetExtraInfo).hasoriginalbox := True;
  end;
end;

{$IFNDEF CRAWLER}
function TSetsDatabase.GetItemWeight(const pcs: string; const color: integer; pi: TPieceInfo = nil): double;
var
  idx: integer;
  ss: TSetExtraInfo;
begin
  if color <> INSTRUCTIONCOLORINDEX  then
    if color <> BOXCOLORINDEX then
    begin
      if pi = nil then
        pi := PieceInfo(pcs);
      Result := pi.weight;
      Exit;
    end;

  idx := fsets.IndexOf(Trim(pcs));
  if idx < 0 then
  begin
    Result := 0.0;
    Exit;
  end;

  ss := fsets.Objects[idx] as TSetExtraInfo;
  if color = INSTRUCTIONCOLORINDEX then
    Result := ss.instructionsweight
  else if color = BOXCOLORINDEX then
    Result := ss.originalboxweight
  else
    Result := 0.0;
end;

function TSetsDatabase.GetAssetWeight(const aset: string; const aasset: char): double;
begin
  if aasset = 'I' then
    Result := GetAssetWeight(aset, INSTRUCTIONCOLORINDEX)
  else if aasset = 'B' then
    Result := GetAssetWeight(aset, BOXCOLORINDEX)
  else
  begin
    Result := 0.0;
    I_Error('TSetsDatabase.GetAssetWeight(): Called with invalid argument "' + aasset + '"');
  end;
end;

function TSetsDatabase.GetAssetWeight(const aset: string; const aasset: integer): double;
var
  idx: integer;
  ss: TSetExtraInfo;
begin
  idx := fsets.IndexOf(Trim(aset));
  if idx < 0 then
  begin
    Result := 0.0;
    Exit;
  end;

  ss := fsets.Objects[idx] as TSetExtraInfo;
  if aasset = INSTRUCTIONCOLORINDEX then
    Result := ss.instructionsweight
  else if aasset = BOXCOLORINDEX then
    Result := ss.originalboxweight
  else
    Result := 0.0;
end;

procedure TSetsDatabase.SetAssetWeight(const aset: string; const aasset: char; const ww: double);
begin
  if aasset = 'I' then
    SetAssetWeight(aset, INSTRUCTIONCOLORINDEX, ww)
  else if aasset = 'B' then
    SetAssetWeight(aset, BOXCOLORINDEX, ww)
  else
    I_Error('TSetsDatabase.SetAssetWeight(): Called with invalid argument "' + aasset + '"');
end;

procedure TSetsDatabase.SetAssetWeight(const aset: string; const aasset: integer; const ww: double);
var
  sl: TStringList;
  idx: integer;
  casset: char;
  sset: string;
  ss: TsetExtraInfo;
  fn: string;
begin
  if aasset = INSTRUCTIONCOLORINDEX then
    casset := 'I'
  else if aasset = BOXCOLORINDEX then
    casset := 'B'
  else
  begin
    I_Error('TSetsDatabase.SetAssetWeight(): Called with invalid argument "' + itoa(aasset) + '"');
    Exit;
  end;

  sset := Trim(aset);
  idx := fsets.IndexOf(sset);
  if idx < 0 then
    Exit;
  ss := fsets.Objects[idx] as TsetExtraInfo;
  if casset = 'I' then
    ss.instructionsweight := ww
  else
    ss.originalboxweight := ww;

  sl := TStringList.Create;
  try
    fn := basedefault + 'db\db_set_assets.txt';
    if fexists(fn) then
      S_LoadFromFile(sl, fn);
    if sl.Count = 0 then
      sl.Add('Set,Asset,Weight')
    else if sl.Strings[0] = 'Set,Asset' then
      sl.Strings[0] := 'Set,Asset,Weight';
    if sl.Strings[0] = 'Set,Asset,Weight' then
    begin
      idx := IndexOfStart(sl, sset + ',' + casset);
      if idx < 0 then
        sl.Add(sset + ',' + casset + ',' + Format('%2.4f', [ww]))
      else
        sl.Strings[idx] := sset + ',' + casset + ',' + Format('%2.4f', [ww]);

      S_BackupFile(fn);
      S_SaveToFile(sl, fn);
    end;
  finally
    sl.Free;
  end;
end;

function TSetsDatabase.UpdateAssetWeightFromNET(const aset: string; const casset: char): boolean;
var
  ww: double;
begin
  Result := False;
  if casset in  ['I', 'B'] then
  begin
    ww := GetAssetWeightFromNET(aset, casset);
    if ww > 0.0 then
    begin
      SetAssetWeight(aset, casset, ww);
      Result := True;
    end;
  end;
end;

function TSetsDatabase.UpdateAssetWeightFromNET(const aset: string; const aasset: integer): boolean;
var
  casset: char;
begin
  Result := False;
  if aasset = INSTRUCTIONCOLORINDEX then
    casset := 'I'
  else if aasset = BOXCOLORINDEX then
    casset := 'B'
  else
    Exit;

  Result := UpdateAssetWeightFromNET(aset, casset);
end;

function TSetsDatabase.GetAssetWeightFromNET(const aset: string; const casset: char): double;
var
  alink: string;
  adata: string;
  afname: string;
  check: string;
  adir: string;
begin
  Result := 0.0;
  if not (casset in ['I', 'B']) then
    Exit;

  if casset = 'I' then
    check := '(Instructions Entry)'
  else
    check := '(Original Box Entry)';
  alink := sbricklink + 'v2/catalog/catalogitem.page?' + decide(casset = 'B', 'O', casset) + '=' + aset;
  adata := NET_GetURLString(alink);
  if Pos(UpperCase(check), UpperCase(adata)) > 0 then
  begin
    if casset = 'I' then
      adir := basedefault + 'db\instructions'
    else
      adir := basedefault + 'db\boxes';
    if not DirectoryExists(adir) then
      MkDir(adir);
    afname := adir + '\' + aset + '.htm';
    SaveStringToFile(afname, adata);
    NET_GetItemWeightFromText(adata, Result)
  end;
end;

function TSetsDatabase.GetAssetWeightFromNET(const aset: string; const aasset: integer): double;
var
  casset: char;
begin
  Result := 0.0;
  if aasset = INSTRUCTIONCOLORINDEX then
    casset := 'I'
  else if aasset = BOXCOLORINDEX then
    casset := 'B'
  else
    Exit;

  Result := GetAssetWeightFromNET(aset, casset);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
procedure TSetsDatabase.RefreshSetYears(const setid: string);
var
  inv: TBrickInventory;
  pci: TPieceColorInfo;
  i: integer;
  y: integer;
begin
  inv := GetSetInventory(setid);
  if inv = nil then
    Exit;
  y := db.SetYear(setid);
  if y > 0 then
  begin
    pci := PieceColorInfo(setid, -1);
    if pci <> nil then
      pci.year := y;
    pci := PieceColorInfo(setid, INSTRUCTIONCOLORINDEX);
    if pci <> nil then
      pci.year := y;
    pci := PieceColorInfo(setid, BOXCOLORINDEX);
    if pci <> nil then
      pci.year := y;
    for i := 0 to inv.numlooseparts - 1 do
    begin
      pci := PieceColorInfo(@inv.looseparts[i]);
      if pci <> nil then
        pci.UpdateSetYears(setid, y);
    end;
  end;
end;
{$ENDIF}

function TSetsDatabase.RefreshInv(const inv: TBrickInventory): boolean;
var
  slist: TStringList;
  i, j: integer;
  bl, blnew: string;
  cnt: integer;
  pci: TPieceColorInfo;
  pregressstring: string;
begin
  Result := False;
  cnt := 0;
  slist := inv.GetMoldList;
  pregressstring := 'Refreshing...';
  if assigned(progressfunc) then
    progressfunc(pregressstring, 0);
  for i := 0 to slist.Count - 1 do
  begin
    bl := BrickLinkPart(slist.Strings[i]);
    if strupper(bl) = strupper(slist.Strings[i]) then
    begin
      blNew := NET_GetBricklinkAlias(slist.Strings[i]);
      if (Pos('3068bpb', blnew) = 1) and (length(blnew) = 10) then
        blnew := '3068bpb0' + blnew[8] + blnew[9] + blnew[10];
      if (blnew <> '') and (strupper(bl) <> strupper(blnew)) then
      begin
        SetNewPieceName(slist.Strings[i], blnew);
//        AddPieceAlias(blnew, slist.Strings[i]);
        for j := 0 to inv.numlooseparts - 1 do
          if strupper(inv.looseparts[j].part) = strupper(slist.Strings[i]) then
          begin
            pci := PieceColorInfo(slist.Strings[i], inv.looseparts[j].color);
            if pci <> nil then
              pci.InternetUpdate;
          end;
        inc(cnt);
      end;
    end;
    if assigned(progressfunc) then
      progressfunc(pregressstring, ((1 + i) / slist.count) * (2 / 3));
  end;

  for i := 0 to inv.numlooseparts - 1 do
  begin
    pci := PieceColorInfo(inv.looseparts[i].part, inv.looseparts[i].color);
    if pci <> nil then
      if pci.invalid then
      begin
        pci.InternetUpdate;
        if not pci.invalid then
          inc(cnt);
      end;
    if assigned(progressfunc) then
      progressfunc(pregressstring, ((1 + i) / inv.numlooseparts) / 3 + 2 / 3);
  end;

  if cnt > 0 then
    Result := True;

  slist.Free;
  if assigned(progressfunc) then
    progressfunc(pregressstring, 1.0);
end;

function TSetsDatabase.RefreshSet(const s: string; const lite: boolean = False): boolean;
var
  inv: TBrickInventory;
  pci: TPieceColorInfo;
  idx: integer;
  fn: string;
begin
  Result := False;

  inv := GetSetInventory(s);

  if inv = nil then
    Exit;

  fn := basedefault + 'db\sets\' + s + '.txt';
  if fexists(fn) then
  begin
    inv.Clear;
    inv.LoadLooseParts(fn);
    idx := fallsetswithoutextra.IndexOf(s);
    if idx > -1 then
    begin
      (fallsetswithoutextra.Objects[idx] as TBrickInventory).Clear;
      (fallsetswithoutextra.Objects[idx] as TBrickInventory).MergeWith(inv);
    end;
    Result := True;
    if not lite then
      RefreshInv(inv);
    fbinarysets.UpdateSetFromTextFile(s, fn)
  end
  else
    if not lite then
      Result := RefreshInv(inv);

  if Result then
    if not lite then
    begin
      pci := PieceColorInfo(s, -1);
      if pci <> nil then
        pci.InternetUpdate;

      inv.DoUpdateCostValues;
    end;

end;

{$IFNDEF CRAWLER}
function TSetsDatabase.RefreshPart(const s: string): boolean;
var
  inv: TBrickInventory;
  i: integer;
  ret1, ret2, ret3: boolean;
begin
  inv := TBrickInventory.Create;
  for i := -1 to MAXINFOCOLOR do
    if Colors(i).knownpieces <> nil then
      if Colors(i).knownpieces.IndexOf(s) > - 1 then
        inv.AddLoosePartFast(s, i, 1);

  ret1 := RefreshInv(inv);
  inv.Free;

  ret2 := RefreshPartCategory(s);

  ret3 := UpdatePartInventory(s, False);

  Result := ret1 or ret2 or ret3;
end;
{$ENDIF}

function TSetsDatabase.SetPartCategory(const s: string; const newcat: integer): boolean;
var
  oldcat, tmpcat: integer;
  idx: integer;
  pinf: TPieceInfo;
  spiece: string;
  sl: TStringList;
  fname: string;
begin
  Result := False;

  spiece := fixpartname(RebrickablePart(s));
  idx := IndexOfString(fpieceshash, spiece);
  if idx < 0 then
    Exit;

  pinf := fpieces.Objects[idx] as TPieceInfo;
  if pinf = nil then
    Exit;
  oldcat := pinf.category;
  if oldcat = newcat then
    Exit;

  tmpcat := newcat;
  if (tmpcat <= 0) or (tmpcat >= MAXCATEGORIES) then
    Exit;

  if fcategories[tmpcat].knownpieces = nil then
    fcategories[tmpcat].knownpieces := THashStringList.Create;

  idx := fcategories[tmpcat].knownpieces.IndexOf(pinf.name);
  if idx >= 0 then
    Exit;

  pinf.category := tmpcat;

  fcategories[tmpcat].knownpieces.AddObject(pinf.name, pinf);
  idx := fcategories[oldcat].knownpieces.IndexOf(pinf.name);
  if idx >= 0 then
    fcategories[oldcat].knownpieces.Delete(idx);

  sl := TStringList.Create;
  try
    fname := basedefault + 'db\db_pieces_categories.txt';
    if fexists(fname) then
      S_LoadFromFile(sl, fname)
    else
      sl.Add('Category,Part');

    idx := sl.IndexOf(itoa(oldcat) + ',' + pinf.name);
    if idx < 0 then
      idx := sl.IndexOf(itoa(oldcat) + ',BL ' + BricklinkPart(pinf.name));
    if idx >= 0 then
      sl.Delete(idx);
    sl.Add(itoa(tmpcat) + ',' + pinf.name);
    S_BackupFile(fname);
    S_SaveToFile(sl, fname);
  finally
    sl.Free;
  end;

  Result := True;
end;

procedure TSetsDatabase.RefreshPartsCategory(const L: TStringList);
var
  tmpcat: integer;
  idx: integer;
  pinf: TPieceInfo;
  spiece: string;
  sl: TStringList;
  fname: string;
  tmpweight: double;
  s: string;
  i: integer;
  oldcategory: integer;
  oldweight: double;
  parttyp: char;
begin
  if L = nil then
    Exit;

  fname := basedefault + 'db\db_pieces_categories.txt';
  sl := TStringList.Create;
  try
    if fexists(fname) then
      S_LoadFromFile(sl, fname)
    else
      sl.Add('Category,Part');

    for i := 0 to L.Count - 1 do
    begin
      s := fixpartname(L.Strings[i]);
      spiece := fixpartname(RebrickablePart(s));
      idx := IndexOfString(fpieceshash, spiece);
      if idx < 0 then
        continue;

      pinf := fpieces.Objects[idx] as TPieceInfo;
      if pinf = nil then
        continue;
      oldcategory := pinf.category;
      oldweight := pinf.weight;

      tmpweight := -1.0;
      tmpcat := -2;
      NET_GetBricklinkCategory(BrickLinkPart(s), tmpcat, tmpweight, parttyp);
      if tmpweight > 0 then
        if tmpweight <> oldweight then
          UpdatePartWeight(s, tmpweight);

      if (tmpcat <= 0) or (tmpcat >= MAXCATEGORIES) or (tmpcat = oldcategory) then
        continue;

      if fcategories[tmpcat].knownpieces = nil then
        fcategories[tmpcat].knownpieces := THashStringList.Create;

      idx := fcategories[tmpcat].knownpieces.IndexOf(pinf.name);
      if idx >= 0 then
        continue;

      pinf.category := tmpcat;

      fcategories[tmpcat].knownpieces.AddObject(pinf.name, pinf);
      idx := fcategories[oldcategory].knownpieces.IndexOf(pinf.name);
      if idx >= 0 then
        fcategories[oldcategory].knownpieces.Delete(idx);

      sl.Add(itoa(tmpcat) + ',' + pinf.name);
    end;

    S_BackupFile(fname);
    S_SaveToFile(sl, fname);
  finally
    sl.Free;
  end;
end;

function TSetsDatabase.RefreshPartCategory(const s: string): boolean;
var
  tmpcat: integer;
  idx: integer;
  pinf: TPieceInfo;
  spiece: string;
  sl: TStringList;
  fname: string;
  tmpweight: double;
  parttyp: char;
  oldcat: integer;
begin
  Result := False;

  spiece := RebrickablePart(s);
  idx := IndexOfString(fpieceshash, spiece);
  if idx < 0 then
    Exit;

  pinf := fpieces.Objects[idx] as TPieceInfo;
  if pinf = nil then
    Exit;
  if pinf.category > 0 then
    Exit;

  tmpweight := -1.0;
  tmpcat := -2;
  NET_GetBricklinkCategory(BrickLinkPart(s), tmpcat, tmpweight, parttyp);
  if tmpweight > 0 then
    UpdatePartWeight(s, tmpweight);

  if (tmpcat <= 0) or (tmpcat >= MAXCATEGORIES) then
    Exit;

  if fcategories[tmpcat].knownpieces = nil then
    fcategories[tmpcat].knownpieces := THashStringList.Create;

  idx := fcategories[tmpcat].knownpieces.IndexOf(pinf.name);
  if idx >= 0 then
    Exit;

  oldcat := pinf.category;
  if oldcat <> tmpcat then
  begin
    pinf.category := tmpcat;

    fcategories[tmpcat].knownpieces.AddObject(pinf.name, pinf);
    idx := fcategories[oldcat].knownpieces.IndexOf(pinf.name);
    if idx >= 0 then
      fcategories[oldcat].knownpieces.Delete(idx);

    sl := TStringList.Create;
    try
      fname := basedefault + 'db\db_pieces_categories.txt';
      if fexists(fname) then
        S_LoadFromFile(sl, fname)
      else
        sl.Add('Category,Part');

      idx := sl.IndexOf(itoa(oldcat) + ',' + pinf.name);
      if idx > 0 then
        sl.Delete(idx);
      sl.Add(itoa(tmpcat) + ',' + pinf.name);
      S_BackupFile(fname);
      S_SaveToFile(sl, fname);
    finally
      sl.Free;
    end;
  end;

  Result := True;
end;

function TSetsDatabase.RefreshMinifigCategory(const s: string): boolean;
var
  tmpcat: integer;
  idx: integer;
  pinf: TPieceInfo;
  spiece: string;
  sl: TStringList;
  fname: string;
  tmpweight: double;
begin
  Result := False;

  spiece := RebrickablePart(s);
  idx := IndexOfString(fpieceshash, spiece);
  if idx < 0 then
    Exit;

  pinf := fpieces.Objects[idx] as TPieceInfo;
  if pinf = nil then
    Exit;

  if pinf.category > 0 then
    Exit;

  tmpweight := -1.0;
  tmpcat := -2;
  NET_GetBricklinkMinifigCategory(BrickLinkPart(s), tmpcat, tmpweight);
  if tmpweight > 0 then
    UpdatePartWeight(s, tmpweight);

  if (tmpcat <= 0) or (tmpcat >= MAXCATEGORIES) then
    Exit;

  if fcategories[tmpcat].knownpieces = nil then
    fcategories[tmpcat].knownpieces := THashStringList.Create;

  idx := fcategories[tmpcat].knownpieces.IndexOf(pinf.name);
  if idx >= 0 then
    Exit;

  pinf.category := tmpcat;

  fcategories[tmpcat].knownpieces.AddObject(pinf.name, pinf);
  idx := fcategories[0].knownpieces.IndexOf(pinf.name);
  if idx >= 0 then
    fcategories[0].knownpieces.Delete(idx);

  sl := TStringList.Create;
  try
    fname := basedefault + 'db\db_pieces_categories.txt';
    if fexists(fname) then
      S_LoadFromFile(sl, fname)
    else
      sl.Add('Category,Part');

    idx := sl.IndexOf('0,' + pinf.name);
    if idx > 0 then
      sl.Delete(idx);
    sl.Add(itoa(tmpcat) + ',' + pinf.name);
    S_BackupFile(fname);
    S_SaveToFile(sl, fname);
  finally
    sl.Free;
  end;

  Result := True;
end;

function TSetsDatabase.RefreshPartWeight(const s: string): boolean;
var
  tmpcat: integer;
  idx: integer;
  pinf: TPieceInfo;
  spiece: string;
  tmpweight: double;
  parttyp: char;
begin
  Result := False;

  spiece := RebrickablePart(s);
  idx := IndexOfString(fpieceshash, spiece);
  if idx < 0 then
    Exit;

  pinf := fpieces.Objects[idx] as TPieceInfo;
  if pinf = nil then
    Exit;
  if pinf.Weight > 0 then
    Exit;

  tmpweight := -1.0;
  tmpcat := -2;
  NET_GetBricklinkCategory(BrickLinkPart(s), tmpcat, tmpweight, parttyp);
  if tmpweight > 0 then
    UpdatePartWeight(s, tmpweight);
end;

function TSetsDatabase.RefreshMinifigWeight(const s: string): boolean;
var
  idx: integer;
  pinf: TPieceInfo;
  spiece: string;
  tmpweight: double;
begin
  Result := False;

  spiece := RebrickablePart(s);
  idx := IndexOfString(fpieceshash, spiece);
  if idx < 0 then
    Exit;

  pinf := fpieces.Objects[idx] as TPieceInfo;
  if pinf = nil then
    Exit;
  if pinf.Weight > 0 then
    Exit;

  tmpweight := -1.0;
  NET_GetBricklinkMinifigWeight(BrickLinkPart(s), tmpweight);
  if tmpweight > 0 then
    UpdatePartWeight(s, tmpweight);
end;

function TSetsDatabase.smallparsepartname(const p: string; const data: string): string;
var
  i: integer;
  b1: integer;
  pname: string;
begin
  Result := stringreplace(p, ',', '', [rfReplaceAll, rfIgnoreCase]);
  b1 := Pos('Name:', data);
  if b1 < 1 then
    Exit;
  pname := '';
  for i := b1 + 5 to Length(data) do
  begin
    if data[i] = '"' then
      Break
    else
      pname := pname + data[i];
  end;
  pname := Trim(pname);
  Result := stringreplace(pname, ',', '', [rfReplaceAll, rfIgnoreCase])
end;

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdateSetAssetsFromBricklink(const s: string): boolean;
var
  desc: string;
  asset: setasset_t;
  y1: integer;
  i1, o1: boolean;
  pi: TPieceInfo;
begin
  Result := False;

  y1 := SetYear(s);
  asset := DownloadSetAssetsFromBricklink(s, 'S', y1);
  if asset.year = y1 then
    asset := DownloadSetAssetsFromBricklink(s, 'M', y1);
  if asset.year = y1 then
    asset := DownloadSetAssetsFromBricklink(s, 'G', y1);
  if asset.year = y1 then
    asset := DownloadSetAssetsFromBricklink(s, 'B', y1);
  if (asset.year = y1) and not asset.hasinstructions and not asset.hasoriginalbox then
    Exit;

  i1 := PieceColorInfo(s, INSTRUCTIONCOLORINDEX) <> nil;
  o1 := PieceColorInfo(s, BOXCOLORINDEX) <> nil;
  if i1 then
    if GetAssetWeight(s, INSTRUCTIONCOLORINDEX) = 0.0 then
      UpdateAssetWeightFromNET(s, INSTRUCTIONCOLORINDEX);
  if o1 then
    if GetAssetWeight(s, BOXCOLORINDEX) = 0.0 then
      UpdateAssetWeightFromNET(s, BOXCOLORINDEX);
  if (asset.year = y1) and (asset.hasinstructions = i1) and (asset.hasoriginalbox = o1) then
    Exit;

  desc := SetDesc(s);

  if desc = '' then
  begin
    pi := PieceInfo(s);
    if IsValidPieceInfo(pi) then
      desc := pi.desc;
  end;
  if desc = '' then
    desc := s;
  Result := UpdateSetInfoEx(s, desc, asset.year, False, asset.hasinstructions, asset.hasoriginalbox);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.DownloadSetAssetsFromBricklink(const s: string; const typ: string; const def: integer): setasset_t;
var
  tmpname: string;
  urlstr: string;
  slist: TStringList;
  stmp: string;
  ret: boolean;
  y, p, i, l: integer;
  tmp1, tmp2: string;
  savename: string;
begin
  Result.year := def;
  Result.hasinstructions := False;
  Result.hasoriginalbox := False;
  tmpname := I_NewTempFile('tmpsetyear_' + Trim(s));
  try
    if typ = 'S' then
    begin
      urlstr := sbricklink + 'v2/catalog/catalogitem.page?S=' + GetBLNetPieceName(Trim(s)) + '#T=S&O={}';
      savename := basedefault + 'db\setmolds\' + GetBLNetPieceName(Trim(s)) + '.htm';
    end
    else if typ = 'M' then
    begin
      urlstr := sbricklink + 'v2/catalog/catalogitem.page?M=' + GetBLNetPieceName(Trim(s)) + '#T=S&O={}';
      savename := basedefault + 'db\minifigs\' + GetBLNetPieceName(Trim(s)) + '.htm';
    end
    else if typ = 'G' then
    begin
      urlstr := sbricklink + 'v2/catalog/catalogitem.page?G=' + GetBLNetPieceName(Trim(s)) + '#T=S&O={}';
      savename := basedefault + 'db\gears\' + GetBLNetPieceName(Trim(s)) + '.htm';
    end
    else if typ = 'B' then
    begin
      urlstr := sbricklink + 'v2/catalog/catalogitem.page?B=' + GetBLNetPieceName(Trim(s)) + '#T=S&O={}';
      savename := basedefault + 'db\books\' + GetBLNetPieceName(Trim(s)) + '.htm';
    end
    else
    begin
      urlstr := sbricklink + 'v2/catalog/catalogitem.page?P=' + GetBLNetPieceName(Trim(s)) + '#T=S&O={}';
      savename := basedefault + 'db\molds\' + GetBLNetPieceName(Trim(s)) + '.htm';
    end;
    ret := UrlDownloadToFile(nil, PChar(urlstr), PChar(tmpname), 0, nil) = 0;
    if not ret then
    begin
      Exit;
    end;
    CopyFile(tmpname, savename);
  except
    Exit;
  end;

  slist := TStringList.Create;
  try
    S_LoadFromFile(slist, tmpname);
    tmp2 := slist.Text;
    tmp1 := StringReplace(tmp2, '�', '"', [rfReplaceAll, rfIgnoreCase]);
    tmp2 := StringReplace(tmp1, '&nbsp;', ' ', [rfReplaceAll, rfIgnoreCase]);
    tmp1 := StringReplace(tmp2, '&#40;', '(', [rfReplaceAll, rfIgnoreCase]);
    tmp2 := StringReplace(tmp1, '&#41;', ')', [rfReplaceAll, rfIgnoreCase]);
    tmp1 := StringReplace(tmp2, '&#39;', '''', [rfReplaceAll, rfIgnoreCase]);
    tmp2 := StringReplace(tmp1, '&#38;', '&', [rfReplaceAll, rfIgnoreCase]);
    tmp1 := StringReplace(tmp2, '&amp;', '&', [rfReplaceAll, rfIgnoreCase]);

    stmp := UpperCase(tmp1);
    Result.hasinstructions := Pos(UpperCase('catalogitem.page?I=' + GetBLNetPieceName(Trim(s))), stmp) > 0;
    Result.hasoriginalbox := Pos(UpperCase('catalogitem.page?O=' + GetBLNetPieceName(Trim(s))), stmp) > 0;

    if (typ = 'S') or (typ = 'G') or (typ = 'B') then
    begin
      p := Pos('itemYear', tmp1);
      if p <= 0 then
        p := Pos('itemyear', tmp1);
      if p > 0 then
      begin
        l := 0;
        tmp2 := '';
        for i := p to Length(tmp1) do
        begin
          if l = 4 then
            break;
          if tmp1[i] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] then
          begin
            tmp2 := tmp2 + tmp1[i];
            inc(l);
          end;
        end;
        y := atoi(tmp2, -1);
        if y > 1931 then
          if y < 2050 then
            Result.year := y;
      end;
    end
    else
    begin
      p := Pos('id="yearReleasedSec">', tmp1);
      if p <= 0 then
        p := Pos('yearReleasedSec', tmp1);
      if p > 0 then
      begin
        l := 0;
        tmp2 := '';
        for i := p to Length(tmp1) do
        begin
          if l = 4 then
            break;
          if tmp1[i] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] then
          begin
            tmp2 := tmp2 + tmp1[i];
            inc(l);
          end;
        end;
        y := atoi(tmp2, -1);
        if y >= 1932 then
          if y <= 2050 then
            Result.year := y;
      end;
    end;
  finally
    slist.free;
  end;

  DeleteFile(tmpname);

  RefreshSetYears(s);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.QryNewInventoriesFromBricklink(const path: string; const check: string): TStringList;
var
  fname: string;
  stmp: string;
  p, i: integer;
  htm, htm1: string;
  sl: TStringList;
  idx: integer;
begin
  Result := TStringList.Create;
  fname := I_NewTempFile(itoa(random(1000)) + '.htm');
  if UrlDownloadToFile(nil, PChar(path), PChar(fname), 0, nil) <> 0 then
    Exit;

  sl := TStringList.Create;
  S_LoadFromFile(sl, fname);
  htm := sl.Text;
  htm1 := UpperCase(htm);
  sl.Free;
  while True do
  begin
    p := Pos(UpperCase(check), htm1);
    if p <= 1 then
      break;
    stmp := '';
    idx := p + length(check);
    for i := p + length(check) to length(htm) do
    begin
      if htm[i] in ['"', '''', '>', '<'] then
        break
      else if htm[i] <> ' ' then
        stmp := stmp + htm[i];
      idx := i;
    end;
    if Result.IndexOf(stmp) < 0 then
      if GetSetInventory(stmp) = nil then
        Result.Add(stmp);
    delete(htm, 1, idx);
    htm1 := UpperCase(htm);
  end;
  deletefile(fname);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.QryNewSetAsPartFromBricklink(const path: string; const check: string): TStringList;
var
  fname: string;
  stmp: string;
  p, i: integer;
  htm, htm1: string;
  sl: TStringList;
  idx: integer;
begin
  Result := TStringList.Create;
  fname := I_NewTempFile('qnsap' + itoa(random(1000)) + '.htm');
  if UrlDownloadToFile(nil, PChar(path), PChar(fname), 0, nil) <> 0 then
    Exit;

  sl := TStringList.Create;
  S_LoadFromFile(sl, fname);
  htm := sl.Text;
  htm1 := UpperCase(htm);
  sl.Free;
  if fColors[-1].knownpieces = nil then
    fColors[-1].knownpieces := THashStringList.Create;
  while True do
  begin
    p := Pos(UpperCase(check), htm1);
    if p <= 1 then
      break;
    stmp := '';
    idx := p + length(check);
    for i := p + length(check) to length(htm) do
    begin
      if htm[i] in ['"', '''', '>', '<'] then
        break
      else if htm[i] <> ' ' then
        stmp := stmp + htm[i];
      idx := i;
    end;
    if fColors[-1].knownpieces.IndexOfUCS(stmp) < 0 then
      if Result.IndexOf(stmp) < 0 then
        Result.Add(stmp);
    delete(htm, 1, idx);
    htm1 := UpperCase(htm);
  end;
  deletefile(fname);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.QryNewCatalogsFromBricklink(const path: string; const check: string): TStringList;
var
  fname: string;
  stmp: string;
  p, i: integer;
  htm, htm1: string;
  sl: TStringList;
  idx: integer;
begin
  Result := TStringList.Create;
  fname := I_NewTempFile('qncfb' + itoa(random(1000)) + '.htm');
  if UrlDownloadToFile(nil, PChar(path), PChar(fname), 0, nil) <> 0 then
    Exit;

  sl := TStringList.Create;
  S_LoadFromFile(sl, fname);
  htm := sl.Text;
  htm1 := UpperCase(htm);
  sl.Free;
  if fColors[CATALOGCOLORINDEX].knownpieces = nil then
    fColors[CATALOGCOLORINDEX].knownpieces := THashStringList.Create;
  while True do
  begin
    p := Pos(UpperCase(check), htm1);
    if p <= 1 then
      break;
    stmp := '';
    idx := p + length(check);
    for i := p + length(check) to length(htm) do
    begin
      if htm[i] in ['"', '''', '>', '<'] then
        break
      else if htm[i] <> ' ' then
        stmp := stmp + htm[i];
      idx := i;
    end;
    if fColors[CATALOGCOLORINDEX].knownpieces.IndexOfUCS(stmp) < 0 then
      if Result.IndexOf(stmp) < 0 then
        Result.Add(stmp);
    delete(htm, 1, idx);
    htm1 := UpperCase(htm);
  end;
  deletefile(fname);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.QryPartsFromBricklink(const path: string; const check: string): TStringList;
var
  fname: string;
  stmp: string;
  p, i: integer;
  htm, htm1: string;
  sl: TStringList;
  idx: integer;
begin
  Result := TStringList.Create;
  fname := I_NewTempFile('qnpfb' + itoa(random(1000)) + '.htm');
  if UrlDownloadToFile(nil, PChar(path), PChar(fname), 0, nil) <> 0 then
    Exit;

  sl := TStringList.Create;
  S_LoadFromFile(sl, fname);
  stmp := StringReplace(sl.Text, '&nbsp;', ' ', [rfReplaceAll, rfIgnoreCase]);
  htm := StringReplace(stmp, '&amp;', '&', [rfReplaceAll, rfIgnoreCase]);
  htm1 := UpperCase(htm);
  sl.Free;
  while True do
  begin
    p := Pos(UpperCase(check), htm1);
    if p <= 1 then
      break;
    stmp := '';
    idx := p + length(check);
    for i := p + length(check) to length(htm) do
    begin
      if htm[i] in ['"', '''', '>', '<', '&'] then
        break
      else if htm[i] <> ' ' then
        stmp := stmp + htm[i];
      idx := i;
    end;
    if Result.IndexOf(stmp) < 0 then
      Result.Add(stmp);
    delete(htm, 1, idx);
    htm1 := UpperCase(htm);
  end;
  deletefile(fname);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.QryNewPartsFromFile(const fname: string; const check: string): TStringList;
var
  stmp: string;
  p, i: integer;
  htm, htm1: string;
  sl: TStringList;
  idx: integer;
begin
  Result := TStringList.Create;
  sl := TStringList.Create;
  S_LoadFromFile(sl, fname);
  stmp := StringReplace(sl.Text, '&nbsp;', ' ', [rfReplaceAll, rfIgnoreCase]);
  htm := StringReplace(stmp, '&amp;', '&', [rfReplaceAll, rfIgnoreCase]);
  htm1 := UpperCase(htm);
  sl.Free;
  while True do
  begin
    p := Pos(UpperCase(check), htm1);
    if p <= 1 then
      break;
    stmp := '';
    idx := p + length(check);
    for i := p + length(check) to length(htm) do
    begin
      if htm[i] in ['"', '''', '>', '<', '&'] then
        break
      else if htm[i] <> ' ' then
        stmp := stmp + htm[i];
      idx := i;
    end;
    if IndexOfString(fpieceshash, stmp) < 0 then
      if IndexOfString(fpieceshash, RebrickablePart(stmp)) < 0 then
        if Result.IndexOf(stmp) < 0 then
          Result.Add(stmp);
    delete(htm, 1, idx);
    htm1 := UpperCase(htm);
  end;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.QryNewPartsFromBricklink(const path: string; const check: string; const savelink: string = ''): TStringList;
var
  fname: string;
begin
  if savelink = '' then
  begin
    fname := I_NewTempFile('qnpfb' + itoa(random(1000)) + '.htm');
    if UrlDownloadToFile(nil, PChar(path), PChar(fname), 0, nil) <> 0 then
    begin
      Result := TStringList.Create;
      Exit;
    end;
  end
  else
  begin
    fname := savelink;
    if UrlDownloadToFile(nil, PChar(path), PChar(fname), 0, nil) <> 0 then
    begin
      Result := TStringList.Create;
      Exit;
    end;
  end;

  Result := QryNewPartsFromFile(fname, check);

  if savelink = '' then
    deletefile(fname);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdateSetAsPartFromBricklink(const pid: string; const donet: boolean = True): boolean;
var
  fname, fname2: string;
  SL: TStringList;
  urlstr, urlstr2: string;
  spart: string;
  tmp1: string;
  j, p1: integer;
  newsetname: string;
  ret: integer;
  doupdateyear: boolean;
begin
  Result := False;
  spart := Trim(pid);
  if spart = '' then
    Exit;

  fname := basedefault + 'db\molds\' + spart + '.htm';

  if donet then
  begin
    urlstr := sbricklink + 'catalogItemInv.asp?S=' + GetBLNetPieceName(Trim(spart)) + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
    if UrlDownloadToFile(nil, PChar(urlstr), PChar(fname), 0, nil) <> 0 then
      Exit;
    if not fexists(fname) then
      Exit;
  end;

  if not fexists(fname) then
    if not NET_ExistsCache(spart, 'SMGB', fname) then
      Exit;

  SL := TStringList.Create;
  S_LoadFromFile(SL, fname);
  tmp1 := SL.Text;
  p1 := Pos('SIZE="+0"><B>', tmp1);
  if p1 > 0 then
  begin
    newsetname := '';
    for j := p1 + 13 to Length(tmp1) do
    begin
      if tmp1[j] = '<' then
        Break
      else
        newsetname := newsetname + tmp1[j];
    end;
    newsetname := RemoveSpecialTagsFromString(newsetname);
    SetMoldName(spart, newsetname);
    AddMoldColor(spart, -1);

    if donet then
    begin
      fname2 := basedefault + 'db\setmolds\' + Trim(spart) + '.htm';
      urlstr2 := sbricklink + 'v2/catalog/catalogitem.page?S=' + Trim(spart);
      ret := UrlDownloadToFile(nil, PChar(urlstr2), PChar(fname2), 0, nil);

      doupdateyear := True;
      if ret <> 0 then
        if not fexists(fname) then
          doupdateyear := False;

      if ret = 0 then
        SetPartType(spart, -1, 'S');

      if doupdateyear then
        UpdateSetYearFromDiskCache(spart, fname2);
    end
    else
      UpdateSetYearFromDiskCache(spart, fname);

    RefreshPartCategory(spart);

    Result := True;
  end;
  SL.Free;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdateMinifigAsPartFromBricklink(const pid: string; const donet: boolean = True): boolean;
var
  fname, fname2: string;
  SL: TStringList;
  urlstr, urlstr2: string;
  spart: string;
  tmp1: string;
  j, p1: integer;
  newsetname: string;
  ret: integer;
  doupdateyear: boolean;
begin
  Result := False;
  spart := Trim(pid);
  if spart = '' then
    Exit;

  fname := basedefault + 'db\molds\' + spart + '.htm';

  if donet then
  begin
    urlstr := sbricklink + 'catalogItemInv.asp?M=' + GetBLNetPieceName(Trim(spart)) + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
    if UrlDownloadToFile(nil, PChar(urlstr), PChar(fname), 0, nil) <> 0 then
      Exit;
  end;

  if not fexists(fname) then
    Exit;

  SL := TStringList.Create;
  S_LoadFromFile(SL, fname);
  tmp1 := SL.Text;
  p1 := Pos('SIZE="+0"><B>', tmp1);
  if p1 > 0 then
  begin
    newsetname := '';
    for j := p1 + 13 to Length(tmp1) do
    begin
      if tmp1[j] = '<' then
        Break
      else
        newsetname := newsetname + tmp1[j];
    end;
    newsetname := RemoveSpecialTagsFromString(newsetname);
    SetMoldName(spart, newsetname);
    AddMoldColor(spart, -1);

    if donet then
    begin
      fname2 := basedefault + 'db\minifigs\' + Trim(spart) + '.htm';
      urlstr2 := sbricklink + 'v2/catalog/catalogitem.page?M=' + Trim(spart);
      ret := UrlDownloadToFile(nil, PChar(urlstr2), PChar(fname2), 0, nil);

      doupdateyear := True;
      if ret <> 0 then
        if not fexists(fname) then
          doupdateyear := False;

      if ret = 0 then
        SetPartType(spart, -1, 'M');

      if doupdateyear then
        UpdateMinifigYearFromDiskCache(spart, fname2);
    end
    else
      UpdateMinifigYearFromDiskCache(spart, fname);

    RefreshPartCategory(spart);

    Result := True;
  end;
  SL.Free;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdatePartKnownColorsFromBricklink(const pid: string; const donet: boolean = True): boolean;
var
  fname: string;
  SL: TStringList;
  desc: string;
  i, j, k: integer;
  s, check: string;
  urlstr, spart: string;
  sCheck: TStringList;
  snum: string;
  N: TDNumberList;
  p, num: integer;
  w: double;
  pi: TPieceInfo;
  ret1, ret2: boolean;
begin
  Result := False;
  spart := Trim(pid);
  if spart = '' then
    Exit;
  fname := basedefault + 'db\molds\' + spart + '.htm';

  if donet then
  begin
    urlstr := sbricklink + 'v2/catalog/catalogitem.page?P=' + GetBLNetPieceName(spart);
    if UrlDownloadToFile(nil, PChar(urlstr), PChar(fname), 0, nil) <> 0 then
      if not fexists(fname) then
        Exit;
  end;

  if not fexists(fname) then
    Exit;

  SL := TStringList.Create;
  sCheck := TStringList.Create;
  N := TDNumberList.Create;
  try
    sCheck.Add('http://' + BL_NET + '/catalogPG.asp?P=' + GetBLNetPieceName(spart) + '&colorID=');
    sCheck.Add('http://' + BL_NET + '/catalogItemIn.asp?P=' + GetBLNetPieceName(spart) + '&colorID=');
    sCheck.Add('//img.' + s_bricklink + '.com/ItemImage/PN/');
    sCheck.Add('onclick="showInventoryWithColor( ');
    S_LoadFromFile(SL, fname);
    SL.Text := RemoveSpecialTagsFromString(SL.Text);
    for i := 0 to SL.Count - 1 do
    begin
      s := Trim(SL.Strings[i]);
      for j := 0 to sCheck.Count - 1 do
      begin
        check := sCheck.Strings[j];
        p := Pos(check, s);
        if p > 0 then
        begin
          snum := '';
          for k := p + length(check) to length(s) do
          begin
            if s[k] in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] then
              snum := snum + s[k]
            else
              break;
          end;
          if snum <> '' then
          begin
            num := atoi(snum);
            if num >= 0 then
              if N.IndexOf(num) < 0 then
                N.Add(num);
          end;
        end;
      end;
    end;
    if N.Count = 0 then
      if Pos(UpperCase('catalogList.asp?catType=P&catString=1060"'), UpperCase(SL.Text)) > 0 then
        N.Add(0);
    if N.Count > 0 then
    begin
      desc := PieceDesc(spart);
      if (desc = '') or (desc = spart) or (desc = GetBLNetPieceName(spart)) then
      begin
        check := 'class=pciItemNameLink><b>[%strColorString%]';
        s := SL.Text;
        p := Pos(check, s);
        desc := '';
        for i := p + length(check) to length(s) do
        begin
          if s[i] = '<' then
            break
          else
          begin
            if s[i] = ',' then
              s[i] := ' ';
            desc := desc + s[i];
          end;
        end;
        if desc <> '' then
          SetMoldName(spart, desc);
      end;
      for i := 0 to N.Count - 1 do
        AddKnownPiece(spart, BrickLinkColorToSystemColor(N.Numbers[i]), desc);
      UpdateItemYearFromDiskCache(spart);
      pi := PieceInfo(spart);
      if pi <> nil then
        if pi <> fstubpieceinfo then
        begin
          w := -1.0;
          if NET_GetItemWeightFromDisk(spart, w) then
            if w > 0 then
              if pi.weight <> w then
                UpdatePartWeight(pi, w);
        end;
      Result := True;
    end;
  finally
    SL.Free;
    sCheck.Free;
    N.Free;
  end;
  if Result then
    UpdateMoldYearsFromNet(pid);
  if not Result then
    if donet then
    begin
      ret1 := UpdateSetAsPartFromBricklink(pid);
      ret2 := UpdateSetAssetsFromBricklink(pid);
      Result := ret1 or ret2;
      if not Result then
        Result := UpdateMinifigAsPartFromBricklink(pid);
    end;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdateGearKnownColorsFromBricklink(const pid: string; const donet: boolean = True): boolean;
var
  fname: string;
  SL: TStringList;
  desc: string;
  i, j, k: integer;
  s, check: string;
  urlstr, spart: string;
  sCheck: TStringList;
  snum: string;
  N: TDNumberList;
  p, num: integer;
  checkyear: string;
  yearnum: integer;
  pci: TPieceColorInfo;
  cl: integer;
  hasinstructions: boolean;
begin
  Result := False;
  spart := Trim(pid);
  if spart = '' then
    Exit;
  fname := basedefault + 'db\molds\' + spart + '.htm';

  if donet then
  begin
    urlstr := sbricklink + 'v2/catalog/catalogitem.page?G=' + GetBLNetPieceName(spart);
    if UrlDownloadToFile(nil, PChar(urlstr), PChar(fname), 0, nil) <> 0 then
      if not fexists(fname) then
        Exit;
  end;

  if not fexists(fname) then
    Exit;

  yearnum := 0;
  SL := TStringList.Create;
  sCheck := TStringList.Create;
  N := TDNumberList.Create;
  try
    sCheck.Add('http://' + BL_NET + '/catalogPG.asp?G=' + GetBLNetPieceName(spart) + '&colorID=');
    sCheck.Add('http://' + BL_NET + '/catalogItemIn.asp?G=' + GetBLNetPieceName(spart) + '&colorID=');
    sCheck.Add('//img.' + s_bricklink + '.com/ItemImage/GN/');
    sCheck.Add('//img.' + s_bricklink + '.com/ItemImage/GL/');
    checkyear := 'catType=G&itemYear=';
    S_LoadFromFile(SL, fname);
    SL.Text := RemoveSpecialTagsFromString(SL.Text);
    hasinstructions := False;
    for i := 0 to SL.Count - 1 do
    begin
      s := Trim(SL.Strings[i]);

      hasinstructions := hasinstructions or (Pos('catalogitem.page?I=' + GetBLNetPieceName(spart), s) > 0);

      if yearnum = 0 then
      begin
        p := Pos(checkyear, s);
        if p > 0 then
        begin
          snum := '';
          for k := p + length(checkyear) to length(s) do
          begin
            if s[k] in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] then
              snum := snum + s[k]
            else
              break;
          end;
          yearnum := atoi(snum);
        end;
      end;

      for j := 0 to sCheck.Count - 1 do
      begin
        check := sCheck.Strings[j];
        p := Pos(check, s);
        if p > 0 then
        begin
          snum := '';
          for k := p + length(check) to length(s) do
          begin
            if s[k] in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] then
              snum := snum + s[k]
            else
              break;
          end;
          num := atoi(snum);
          if num >= 0 then
            if N.IndexOf(num) < 0 then
              N.Add(num);
        end;
      end;
    end;
    if N.Count > 0 then
    begin
      desc := PieceDesc(spart);
      if (desc = '') or (desc = spart) or (desc = GetBLNetPieceName(spart)) then
      begin
        check := 'class=pciItemNameLink><b>[%strColorString%]';
        s := SL.Text;
        p := Pos(check, s);
        desc := '';
        for i := p + length(check) to length(s) do
        begin
          if s[i] = '<' then
            break
          else
          begin
            if s[i] = ',' then
              s[i] := ' ';
            desc := desc + s[i];
          end;
        end;
        if desc <> '' then
          SetMoldName(spart, desc);
      end;
      if N.Count > 0 then
      begin
        for i := 0 to N.Count - 1 do
        begin
          pci := nil;
          cl := BrickLinkColorToSystemColor(N.Numbers[i]);
          AddKnownPiece(spart, cl, desc, pci);
          if pci <> nil then
            if (yearnum >= 1932) and (yearnum <= 2050) then
              SetItemYear(pci, yearnum);
          SetPartType(spart, BrickLinkColorToSystemColor(N.Numbers[i]), 'G');
        end;
        if hasinstructions then
        begin
          AddKnownPiece(spart, INSTRUCTIONCOLORINDEX, desc, pci);
          if pci <> nil then
          begin
            if (yearnum >= 1932) and (yearnum <= 2050) then
              SetItemYear(pci, yearnum);
            SetPartType(pci, 'I');
          end;
        end;
        Result := True;
      end;
    end;
  finally
    SL.Free;
    sCheck.Free;
    N.Free;
  end;

//  if not Result then
//    Result := UpdateSetAsPartFromBricklink(pid);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdateBookKnownColorsFromBricklink(const pid: string; const donet: boolean = True): boolean;
var
  fname: string;
  SL: TStringList;
  desc: string;
  i, j, k: integer;
  s, check: string;
  urlstr, spart: string;
  sCheck: TStringList;
  snum: string;
  N: TDNumberList;
  p, num: integer;
  checkyear: string;
  yearnum: integer;
  pci: TPieceColorInfo;
  cl: integer;
  tmpweight, oldweight: double;
begin
  Result := False;
  spart := Trim(pid);
  if spart = '' then
    Exit;
  fname := basedefault + 'db\books\' + spart + '.htm';

  if donet then
  begin
    urlstr := sbricklink + 'v2/catalog/catalogitem.page?B=' + GetBLNetPieceName(spart);
    if UrlDownloadToFile(nil, PChar(urlstr), PChar(fname), 0, nil) <> 0 then
      if not fexists(fname) then
        Exit;
  end;

  if not fexists(fname) then
    if not NET_ExistsCache(spart, 'PBG', fname) then
      Exit;

  yearnum := 0;
  SL := TStringList.Create;
  sCheck := TStringList.Create;
  N := TDNumberList.Create;
  try
    sCheck.Add('http://' + BL_NET + '/catalogPG.asp?B=' + GetBLNetPieceName(spart) + '&colorID=');
    sCheck.Add('http://' + BL_NET + '/catalogItemIn.asp?B=' + GetBLNetPieceName(spart) + '&colorID=');
    sCheck.Add('//img.' + s_bricklink + '.com/ItemImage/BN/');
    checkyear := 'catType=B&itemYear=';
    S_LoadFromFile(SL, fname);
    SL.Text := RemoveSpecialTagsFromString(SL.Text);
    for i := 0 to SL.Count - 1 do
    begin
      s := Trim(SL.Strings[i]);

      if yearnum = 0 then
      begin
        p := Pos(checkyear, s);
        if p > 0 then
        begin
          snum := '';
          for k := p + length(checkyear) to length(s) do
          begin
            if s[k] in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] then
              snum := snum + s[k]
            else
              break;
          end;
          yearnum := atoi(snum);
        end;
      end;

      for j := 0 to sCheck.Count - 1 do
      begin
        check := sCheck.Strings[j];
        p := Pos(check, s);
        if p > 0 then
        begin
          snum := '';
          for k := p + length(check) to length(s) do
          begin
            if s[k] in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] then
              snum := snum + s[k]
            else
              break;
          end;
          num := atoi(snum);
          if num >= 0 then
            if N.IndexOf(num) < 0 then
              N.Add(num);
        end;
      end;
    end;
    if N.Count > 0 then
    begin
      desc := PieceDesc(spart);
      if (desc = '') or (desc = spart) or (desc = GetBLNetPieceName(spart)) then
      begin
        check := 'class=pciItemNameLink><b>[%strColorString%]';
        s := SL.Text;
        p := Pos(check, s);
        desc := '';
        for i := p + length(check) to length(s) do
        begin
          if s[i] = '<' then
            break
          else
          begin
            if s[i] = ',' then
              s[i] := ' ';
            desc := desc + s[i];
          end;
        end;
        if desc <> '' then
          SetMoldName(spart, desc);
      end;
      if N.Count > 0 then
      begin
        for i := 0 to N.Count - 1 do
        begin
          pci := nil;
          cl := BrickLinkColorToSystemColor(N.Numbers[i]);
          AddKnownPiece(spart, cl, desc, pci);
          if pci <> nil then
            if (yearnum >= 1932) and (yearnum <= 2050) then
              SetItemYear(pci, yearnum);
          SetPartType(spart, BrickLinkColorToSystemColor(N.Numbers[i]), 'B');
        end;

        tmpweight := -1.0;
        oldweight := -2.0;
        if pci.pieceinfo <> nil then
          oldweight := (pci.pieceinfo as TPieceInfo).weight;

        if NET_GetItemWeightFromText(SL.Text, tmpweight) then
          if tmpweight > 0.0 then
            if tmpweight <> oldweight then
              UpdatePartWeight(spart, tmpweight);

        Result := True;
      end;
    end;
  finally
    SL.Free;
    sCheck.Free;
    N.Free;
  end;

//  if not Result then
//    Result := UpdateSetAsPartFromBricklink(pid);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdateItemYearFromDiskCache(const pid: string): boolean;
var
  fname: string;
  SL: TStringList;
  i, j, k: integer;
  s: string;
  spart: string;
  snum: string;
  p: integer;
  checkyear: string;
  checkyear1: string;
  checkyear2: string;
  yearnum: integer;
  pci: TPieceColorInfo;
  N: TDNumberList;
  minyear: integer;
begin
  Result := False;
  spart := Trim(pid);
  if spart = '' then
    Exit;
  if not NET_ExistsCache(spart, 'PBCGM', fname) then
  begin
    if HasSetColorsOnly(spart) then
      Result := UpdateSetYearFromDiskCache(spart, basedefault + 'db\setmolds\' + spart + '.htm');
    Exit;
  end;

  yearnum := 0;
  SL := TStringList.Create;
  checkyear := '';
  try
    checkyear1 := 'catType=G&itemYear=';
    checkyear2 := '<span id="yearReleasedSec">';
    S_LoadFromFile(SL, fname);
    for i := 0 to SL.Count - 1 do
    begin
      s := Trim(SL.Strings[i]);

      p := Pos(checkyear1, s);
      if p > 0 then
        checkyear := checkyear1
      else
      begin
        p := Pos(checkyear2, s);
        if p > 0 then
          checkyear := checkyear2;
      end;

      if p > 0 then
      begin
        snum := '';
        for k := p + length(checkyear) to length(s) do
        begin
          if s[k] in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] then
            snum := snum + s[k]
          else
            break;
        end;
        yearnum := atoi(snum);
        if (yearnum >= 1932) and (yearnum <= 2050) then
          break;
      end;
    end;

    if (yearnum >= 1932) and (yearnum <= 2050) then
    begin
      N := GetMoldKnownColors(spart);
      for i := 0 to N.Count - 1 do
      begin
        pci := PieceColorInfo(spart, N.Numbers[i]);
        if pci <> nil then
          if pci.year = 0 then
          begin
            if (pci.sparttype = 'G') or (N.Count = 1) then
              SetItemYear(pci, yearnum)
            else if (pci.sparttype = 'P') and (N.Count = 2) then
            begin
              minyear := MAXINFOCOLOR + 1;
              for j := 0 to N.Count - 1 do
                if N.Numbers[j] < minyear then
                  minyear := N.Numbers[j];
              if minyear = -1 then
                if N.Numbers[i] > -1 then
                  SetItemYear(pci, yearnum)
            end;
          end;
      end;
      N.Free;
      Result := True;
    end;
  finally
    SL.Free;
  end;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.GetPartYearFromNet(const pid: string; const cl: integer; var yyyy: integer): boolean;
var
  pci: TPieceColorInfo;
begin
  pci := PieceColorInfo(pid, cl);
  Result := GetPartYearFromNet(pci, yyyy);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdatePartYearFromNet(const pid: string; const cl: integer): boolean;
var
  pci: TPieceColorInfo;
begin
  pci := PieceColorInfo(pid, cl);
  Result := UpdatePartYearFromNet(pci);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdatePartYearFromNet(const pci: TPieceColorInfo): boolean;
var
  yyyy: integer;
begin
  yyyy := 0;
  if GetPartYearFromNet(pci, yyyy) then
    if yyyy >= 1932 then
      if yyyy <= 2050 then
      begin
        SetItemYear(pci, yyyy);
        Result := True;
        Exit;
      end;
  Result := False;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.GetPartYearFromNet(const pci: TPieceColorInfo; var yyyy: integer): boolean;
var
  urlstr: string;
  stmpfn: string;
  sl: TStringList;
  i, j, p: integer;
  htm: string;
  nyear: integer;
  check: string;
  syear: string;
  bcolor: integer;
  utmp: string;
begin
  Result := False;

  if pci = nil then
    Exit;

  if (pci.year <> 0) and not pci.canedityear then
    Exit;

  bcolor := colors(pci.color).BrickLingColor;
  if bcolor <= 0 then
    Exit;

  stmpfn := I_NewTempFile(Trim(pci.piece) + '_' + itoa(bcolor));
  urlstr := sbricklink + 'catalogItemIn.asp?P=' + GetBLNetPieceName(Trim(pci.piece)) + '&v=3&in=A&colorID=' + itoa(bcolor);
  if UrlDownloadToFile(nil, PChar(urlstr), PChar(stmpfn), 0, nil) <> 0 then
//    if not fexists(stmpfn) then
      Exit;

  sl := TStringList.Create;
  try
    S_LoadFromFile(sl, stmpfn);
    htm := sl.Text;
    p := Pos('By Year:', htm);
    if p > 0 then
    begin
      htm := Copy(htm, p, Length(htm) - p);
      htm := StringReplace(htm, '</td>', #13#10, [rfReplaceAll, rfIgnoreCase]);
      sl.Text := htm;
      yyyy := 10000000;
      check := 'ITEMYEAR=';
      for i := 0 to sl.Count - 1 do
      begin
        utmp := UpperCase(sl.Strings[i]);
        p := Pos(check, utmp);
        if p > 0 then
        begin
          syear := '';
          for j := p + Length(check) to Length(utmp) do
          begin
            if utmp[j] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] then
              syear := syear + utmp[j]
            else
              Break;
          end;
          nyear := atoi(syear);
          if nyear < yyyy then
            if nyear >= 1932 then
              if nyear <= 2050 then
                yyyy := nyear;
        end;
      end;
      if yyyy >= 1932 then
        if yyyy <= 2050 then
          Result := True;
    end;
  finally
    sl.Free;
  end;
end;

function TSetsDatabase.UpdateMoldYearsFromNet(const pid: string): boolean;
var
  N: TDNumberList;
  i: integer;
begin
  Result := False;
  N := GetMoldKnownColors(pid);
  for i := 0 to N.Count - 1 do
    if N.Numbers[i] >= 0 then
      if N.Numbers[i] <= LASTNORMALCOLORINDEX then
        if UpdatePartYearFromNet(pid, N.Numbers[i]) then
          Result := True;
  N.Free;
end;

{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdateSetYearFromNet(const pid: string): boolean;
  function _do_update_year(const typ: char): boolean;
  var
    fname: string;
    urlstr: string;
    stmpfn: string;
  begin
    Result := False;
    stmpfn := I_NewTempFile(Trim(pid));

    urlstr := sbricklink + 'v2/catalog/catalogitem.page?' + typ + '=' + GetBLNetPieceName(Trim(pid));
    if UrlDownloadToFile(nil, PChar(urlstr), PChar(stmpfn), 0, nil) <> 0 then
      if not fexists(stmpfn) then
        Exit;

    Result := UpdateSetYearFromDiskCache(Trim(pid), stmpfn);

    if Result then
    begin
      fname := basedefault + 'db\setmolds\' + Trim(pid) + '.htm';
      CopyFile(stmpfn, fname);
    end;
    DeleteFile(stmpfn);
  end;
var
  i: integer;
  SS: string;
begin
  Result := False;

  if not HasSetColorsOnly(pid) then
    Exit;

  SS := 'SGB';
  for i := 1 to Length(SS) do
  begin
    Result := _do_update_year(SS[i]);
    if Result then
      Exit;
  end;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdateYearForAllColors(const pid: string; const yearnum: integer): boolean;
var
  N: TDNumberList;
  i: integer;
  pci: TPieceColorInfo;
  spart: string;
begin
  Result := False;
  spart := trim(pid);
  if spart = '' then
    Exit;

  if (yearnum < 1932) or (yearnum > 2050) then
    Exit;

  N := GetMoldKnownColors(spart);
  for i := 0 to N.Count - 1 do
  begin
    pci := PieceColorInfo(spart, N.Numbers[i]);
    if pci <> nil then
      if (pci.year = 0) or (pci.canedityear) then
      begin
        SetItemYear(pci, yearnum);
        Result := True;
      end;
  end;
  N.Free;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.GetSetYearFromDiskCache(const fname: string): integer;
var
  s, snum: string;
  yearnum: integer;
  SL: TStringList;
  i, p: integer;
  checkyear: string;
begin
  Result := 0;

  if not fexists(fname) then
    Exit;

  SL := TStringList.Create;
  S_LoadFromFile(SL, fname);
  s := Trim(SL.Text);
  SL.Free;

  checkyear := 'itemYear=';
  p := Pos(checkyear, s);
  if p > 0 then
  begin
    snum := '';
    for i := p + length(checkyear) to Length(s) do
    begin
      if s[i] in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] then
        snum := snum + s[i]
      else
        break;
    end;
    yearnum := atoi(snum);
    if (yearnum >= 1932) and (yearnum <= 2050) then
      Result := yearnum;
    Exit;
  end;

  // Minifigure ?
  checkyear := '<span id="yearReleasedSec">';
  p := Pos(checkyear, s);
  if p > 0 then
  begin
    snum := '';
    for i := p + length(checkyear) to Length(s) do
    begin
      if s[i] in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] then
        snum := snum + s[i]
      else
        break;
    end;
    yearnum := atoi(snum);
    if (yearnum >= 1932) and (yearnum <= 2050) then
      Result := yearnum;
    Exit;
  end;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdateSetYearFromDiskCache(const pid: string; const fname: string): boolean;
var
  yearnum: integer;
  spart: string;
begin
  Result := False;

  spart := Trim(pid);
  if spart = '' then
    Exit;

  if not HasSetColorsOnly(spart) then
    Exit;

  yearnum := GetSetYearFromDiskCache(fname);
  if (yearnum >= 1932) and (yearnum <= 2050) then
    Result := UpdateYearForAllColors(spart, yearnum);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdateMinifigYearFromDiskCache(const pid: string; const fname: string): boolean;
var
  yearnum: integer;
  spart: string;
begin
  Result := False;

  spart := Trim(pid);
  if spart = '' then
    Exit;

  if not HasMinifigColorsOnly(spart) then
    Exit;

  yearnum := GetSetYearFromDiskCache(fname);
  if (yearnum >= 1932) and (yearnum <= 2050) then
    Result := UpdateYearForAllColors(spart, yearnum);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
procedure TSetsDatabase.SetItemYear(const pci: TPieceColorInfo; const yyyy: integer);
begin
  if pci = nil then
    Exit;
  if (yyyy < 1932) or (yyyy > 2050) then
    Exit;
  if pci.year <> yyyy then
  begin
    pci.canedityear := True;
    SaveItemYearInfo(pci.piece, pci.color, yyyy);
    pci.year := yyyy;
    pci.UpdatePartYears;
  end;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
procedure TSetsDatabase.SaveItemYearInfo(const pid: string; const cl: integer; const yyyy: integer);
var
  gy: TStringList;
  fname: string;
  newrec: string;
  i: integer;
  check, checkU: string;
begin
  if (yyyy < 1932) or (yyyy > 2050) then
    Exit;

  gy := TStringList.Create;
  fname := basedefault + 'db\db_pieces_years.txt';
  if fexists(fname) then
    S_LoadFromFile(gy, fname);
  if gy.Count = 0 then
    gy.Add('Part,Color,Year');
  if gy.Strings[0] <> 'Part,Color,Year' then
    gy.Insert(0, 'Part,Color,Year');
  check := Trim(pid) + ',' + itoa(cl) + ',';
  newrec := check + itoa(yyyy);
  checkU := UpperCase(check);
  for i := 1 to gy.Count - 1 do
    if Pos(checkU, UpperCase(gy.Strings[i])) = 1 then
    begin
      gy.Strings[i] := newrec;
      S_BackupFile(fname);
      S_SaveToFile(gy, fname);
      gy.Free;
      Exit;
    end;

  if gy.IndexOf(newrec) < 0 then
  begin
    gy.Add(newrec);
    S_BackupFile(fname);
    S_SaveToFile(gy, fname);
  end;
  gy.Free;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdateCatalogFromBricklink(const pid: string): boolean;
var
  fname: string;
  SL: TStringList;
  desc: string;
  i: integer;
  s, check: string;
  urlstr, spart: string;
  p: integer;
  stmp: string;
  yyyy: integer;
  wwww: double;
  ct: integer;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  fn: string;
begin
  Result := False;
  spart := Trim(pid);
  if spart = '' then
    Exit;
  if not DirectoryExists(basedefault + 'db\catalogs\') then
    MkDir(basedefault + 'db\catalogs\');
  fname := basedefault + 'db\catalogs\' + spart + '.htm';
  urlstr := sbricklink + 'v2/catalog/catalogitem.page?C=' + GetBLNetPieceName(spart);
  if UrlDownloadToFile(nil, PChar(urlstr), PChar(fname), 0, nil) <> 0 then
    Exit;

  SL := TStringList.Create;
  try
    S_LoadFromFile(SL, fname);

    desc := PieceDesc(spart);
    if (desc = '') or (desc = spart) or (desc = GetBLNetPieceName(spart)) then
    begin
      s := SL.Text;
      check := '<span id="item-name-title">';
      p := Pos(check, s);
      desc := '';
      for i := p + length(check) to length(s) do
      begin
        if s[i] = '<' then
          break
        else
        begin
          if s[i] = ',' then
            s[i] := ' ';
          desc := desc + s[i];
        end;
      end;
      if desc <> '' then
        SetMoldName(spart, desc);
    end;
    AddKnownPiece(spart, CATALOGCOLORINDEX, desc);
    pci := PieceColorInfo(spart, CATALOGCOLORINDEX);

    if pci <> nil then
    begin
      pi := PieceInfo(pci);
      if IsValidPieceInfo(pi) then
      begin
        // year
        check := 'itemYear=';
        p := Pos(check, s);
        if p <= 0 then
        begin
          check := 'itemyear=';
          p := Pos(check, s);
        end;
        stmp := '';
        for i := p + length(check) to length(s) do
        begin
          if s[i] in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] then
            stmp := stmp + s[i]
          else
            Break;
        end;
        yyyy := atoi(stmp);
        if (yyyy <= 1931) or (yyyy >= 2050) then
          yyyy := 0;
        pci.year := yyyy;

        // weight
        check := '<span id="item-weight-info">';
        p := Pos(check, s);
        stmp := '';
        for i := p + length(check) to length(s) do
        begin
          if s[i] in ['.', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] then
            stmp := stmp + s[i]
          else
            Break;
        end;
        wwww := atof(stmp, 0.0);
        pi.weight := wwww;

        // category
        check := 'catString=';
        p := Pos(check, s);
        if p <= 0 then
        begin
          check := 'catstring=';
          p := Pos(check, s);
        end;
        stmp := '';
        for i := p + length(check) to length(s) do
        begin
          if s[i] in ['.', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] then
            stmp := stmp + s[i]
          else
            Break;
        end;
        ct := atoi(stmp);
        if (ct < 0) or (ct >= MAXCATEGORIES) then
          ct := 0;
        pi.category := ct;
        if fcategories[ct].knownpieces = nil then
          fcategories[ct].knownpieces := THashStringList.Create;
        fcategories[ct].knownpieces.AddObject(spart, pi);

        stmp := itoa(ct) + ',' + spart + ',' + desc + ',' + itoa(yyyy) + ',' + Format('%2.4f', [wwww]);
        fn := basedefault + 'db\db_catalogs.txt';
        if not fexists(fn) then
          S_AppendLineToFile(fn, 'Category,Number,Name,Year,Weight');
        S_BackupFile(fn);
        S_AppendLineToFile(fn, stmp);

        SetPartType(pci, 'C');

        Result := True;
      end;
    end;

  finally
    SL.Free;
  end;
end;
{$ENDIF}

function TSetsDatabase.ParseKnownPiecesFromHTML(const shtml: string): TStringList;
var
  p0a, p0b, p1, p2: integer;
  tmp1, tmp2: string;
  slist, stmp1: TStringList;
  i, j: integer;
  bl_part, bl_scolor, desc: string;
  bl_color: integer;
begin
  Result := TStringList.Create;
  p0a := Pos('Counterparts:', shtml);
  p0b := Pos('Alternate Items:', shtml);
  if p0a = 0 then
    p1 := p0b
  else if p0b = 0 then
    p1 := p0a
  else if p0a > p0b then
    p1 := p0b
  else
    p1 := p0a;
  p2 := Pos('Summary:', shtml);
  if p2 = 0 then
    p2 := Length(shtml);
  if (p1 > 0) then
    if (p2 > p1) then
    begin
      tmp2 := Copy(shtml, p1, p2 - p1);
      tmp1 := StringReplace(tmp2, '�', '"', [rfReplaceAll, rfIgnoreCase]);
      tmp2 := StringReplace(tmp1, '&nbsp;', ' ', [rfReplaceAll, rfIgnoreCase]);
      tmp1 := StringReplace(tmp2, '&#40;', '(', [rfReplaceAll, rfIgnoreCase]);
      tmp2 := StringReplace(tmp1, '&#41;', ')', [rfReplaceAll, rfIgnoreCase]);
      tmp1 := StringReplace(tmp2, '&#39;', '''', [rfReplaceAll, rfIgnoreCase]);
      tmp2 := StringReplace(tmp1, '&#38;', '&', [rfReplaceAll, rfIgnoreCase]);
      tmp1 := StringReplace(tmp2, '</TR>', '</TR>'#13#10, [rfReplaceAll, rfIgnoreCase]);
      tmp2 := StringReplace(tmp1, '&amp;', '&', [rfReplaceAll, rfIgnoreCase]);
      slist := TStringList.Create;
      slist.Text := tmp2;
      stmp1 := TStringList.Create;
      for i := 0 to slist.Count - 1 do
      begin
        tmp1 := slist.Strings[i];
        if Pos('<A HREF="/v2/catalog/catalogitem.page?', tmp1) > 0 then
        begin
          tmp2 := StringReplace(tmp1, '</TD>', '</TD>'#13#10, [rfReplaceAll, rfIgnoreCase]);
          stmp1.Text := tmp2;
          if stmp1.Count = 6 then
          begin
            bl_part := '';
            bl_scolor := '';
            tmp1 := stmp1.Strings[2]; // color & part
            p1 := Pos('idColor=', tmp1);
            if p1 > 0 then
            begin
              for j := p1 to Length(tmp1) do
              begin
                if tmp1[j] = '>' then
                  Break;
                if tmp1[j] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] then
                  bl_scolor := bl_scolor + tmp1[j]
                else if bl_scolor <> '' then
                  Break;
              end;

              p1 := Pos('/v2/catalog/catalogitem.page?P=', tmp1);
              if p1 <= 0 then
                p1 := Pos('/v2/catalog/catalogitem.page?G=', tmp1);
              if p1 > 0 then
                for j := p1 + 31 to Length(tmp1) do
                begin
                  if tmp1[j] = '&' then
                    Break
                  else
                    bl_part := bl_part + tmp1[j];
                end;
            end
            else
            begin
              p1 := Pos('/v2/catalog/catalogitem.page?M=', tmp1);
              if p1 > 0 then
              begin
                bl_scolor := '0';
                for j := p1 + 31 to Length(tmp1) do
                begin
                  if tmp1[j] = '"' then
                    Break
                  else
                    bl_part := bl_part + tmp1[j];
                end;
              end;
            end;
            bl_scolor := Trim(bl_scolor);
            bl_color := atoi(bl_scolor, -2);
            bl_part := Trim(bl_part);

            if (bl_color > -2) and (bl_part <> '') then
            begin
              desc := smallparsepartname(bl_part, stmp1.Strings[0]);
              Result.Add('BL ' + bl_part + ',' + 'BL ' + bl_scolor + ',' + desc);
            end;
          end;
        end;
      end;
      slist.Free;
      stmp1.Free;
    end;
end;

{$IFNDEF CRAWLER}
procedure TSetsDatabase.AddKnownPieces(const sl: TStringList);
var
  i: integer;
  stmp, spart, scolor, sdesc: string;
  ncolor: integer;
begin
  for i := 0 to sl.Count - 1 do
  begin
    stmp := sl.Strings[i];
    if i = 0 then
      if UpperCase(stmp) = UpperCase('Part,Color,Desc') then
        Continue;
    splitstring(stmp, spart, scolor, sdesc, ',');
    if Pos('BL ', spart) = 1 then
      spart := RebrickablePart(Trim(Copy(spart, 4, Length(spart) - 3)))
    else
      spart := RebrickablePart(Trim(spart));
    if Pos('BL', scolor) = 1 then
    begin
      scolor := Trim(Copy(scolor, 3, Length(scolor) - 2));
      ncolor := BrickLinkColorToSystemColor(atoi(scolor))
    end
    else if Pos('RB', scolor) = 1 then
    begin
      scolor := Trim(Copy(scolor, 3, Length(scolor) - 2));
      ncolor := RebrickableColorToSystemColor(atoi(scolor))
    end
    else
      ncolor := atoi(scolor);
    AddKnownPiece(spart, ncolor, Trim(sdesc));
  end;
end;
{$ENDIF}

procedure TSetsDatabase.RemoveDoublesFromList1(const fname1: string);
var
  lst: TStringList;
begin
  if not fexists(fname1) then
    Exit;

  lst := TStringList.Create;
  try
    S_LoadFromFile(lst, fname1);
    RemoveDoublesFromList1(lst);
    S_BackupFile(fname1);
    S_SaveToFile(lst, fname1);
  finally
    lst.Free;
  end;
end;

procedure TSetsDatabase.RemoveDoublesFromList1(const lst: TStringList);
var
  i, j: integer;
  s1, s2: string;
  s4, s5: string;
  N: TDNumberList;
begin
  N := TDNumberList.Create;
  try
    for i := 0 to lst.Count - 2 do
      if N.IndexOf(i) < 0 then
      begin
        splitstring(lst.Strings[i], s1, s2, ',');
        for j := i + 1 to lst.Count - 1 do
          if N.IndexOf(j) < 0 then
          begin
            splitstring(lst.Strings[j], s4, s5, ',');
            if s4 = s1 then
              N.Add(j);
          end;
      end;
    for i := lst.Count - 1 downto 0 do
      if N.IndexOf(i) > 0 then
        lst.Delete(i);
  finally
    N.Free;
  end;
end;

function TSetsDatabase.RemoveDoublesFromList2(const fname1: string): boolean;
var
  lst: TStringList;
begin
  Result := False;

  if not fexists(fname1) then
    Exit;

  lst := TStringList.Create;
  try
    S_LoadFromFile(lst, fname1);
    Result := RemoveDoublesFromList2(lst);
    S_BackupFile(fname1);
    S_SaveToFile(lst, fname1);
  finally
    lst.Free;
  end;
end;

function TSetsDatabase.RemoveDoublesFromList2(const lst: TStringList): boolean;
var
  i, j: integer;
  s1, s2, s3: string;
  s4, s5, s6: string;
  N: TDNumberList;
  len: integer;
begin
  len := lst.Count;
  N := TDNumberList.Create;
  try
    for i := 0 to lst.Count - 2 do
      if N.IndexOf(i) < 0 then
      begin
        splitstring(lst.Strings[i], s1, s2, s3, ',');
        for j := i + 1 to lst.Count - 1 do
          if N.IndexOf(j) < 0 then
          begin
            splitstring(lst.Strings[j], s4, s5, s6, ',');
            if s4 = s1 then
              if s5 = s2 then
                N.Add(j);
          end;
      end;
    for i := lst.Count - 1 downto 0 do
      if N.IndexOf(i) > 0 then
        lst.Delete(i);
  finally
    N.Free;
  end;
  Result := len <> lst.Count;
end;

{$IFNDEF CRAWLER}
function TSetsDatabase.AddKnownPiece(const spart: string; const color: integer; const desc: string): boolean;
var
  pci: TPieceColorInfo;
begin
  pci := nil;
  Result := AddKnownPiece(spart, color, desc, pci);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.AddKnownPiece(const spart: string; const color: integer; const desc: string; var pci: TPieceColorInfo): boolean;
var
  newname: string;
  pi: TPieceInfo;
  pcolor: colorinfo_p;
  tmppartname: string;
  checkname: string;
  idx2: integer;
  stmp1, stmp2: TStringList;
  kpfile: string;
begin
  Result := False;

  newname := fixpartname(spart);
  if color <>  -2 then
  begin
    pci := PieceColorInfo(newname, color);
    if pci = nil then
    begin
      pci := TPieceColorInfo.Create(newname, color);
      pcolor := Colors(color);
      if pcolor.knownpieces = nil then
        pcolor.knownpieces := THashStringList.Create;
      pcolor.knownpieces.AddObject(newname, pci);
      stmp1 := TStringList.Create;
      kpfile := basedefault + 'db\db_knownpieces.txt';
      if fexists(kpfile) then
        S_LoadFromFile(stmp1, kpfile)
      else
        stmp1.Add('Part,Color,Desc');
      stmp1.Add(newname + ',' + itoa(color) + ',' + desc);
      S_BackupFile(kpfile);
      RemoveBlancLines(stmp1);
      S_SaveToFile(stmp1, kpfile);
      stmp1.Free;
      Result := True;
    end;
  end;

  tmppartname := Trim(stringreplace(newname, ',', '', [rfReplaceAll, rfIgnoreCase]));
  if (PieceDesc(spart) = '') or
     (UpperCase(PieceDesc(newname)) = UpperCase(newname)) or
     (UpperCase(PieceDesc(newname)) = UpperCase(tmppartname)) then
  begin
    pi := PieceInfo(newname);
    if pi = nil then
    begin
      pi := TPieceInfo.Create;
      if desc = '' then
        pi.desc := spart
      else
        pi.desc := desc;
      pi.name := newname;
      pi.lname := LowerCase(newname);
      fpieces.AddObject(newname, pi);
      checkname := '';
    end
    else if pi = fstubpieceinfo then
    begin
      pi := TPieceInfo.Create;
      if desc = '' then
        pi.desc := spart
      else
        pi.desc := desc;
      pi.name := newname;
      pi.lname := LowerCase(newname);
      fpieces.AddObject(newname, pi);
      checkname := '';
    end
    else
    begin
      checkname := spart + ',' + tmppartname;
      pi.desc := desc;
    end;
    stmp2 := TStringList.Create;
    if fexists(basedefault + 'db\db_pieces.extra.txt') then
    begin
      S_LoadFromFile(stmp2, basedefault + 'db\db_pieces.extra.txt');
      RemoveBlancLines(stmp2);
    end;
    if checkname <> '' then
    begin
      idx2 := stmp2.IndexOf(checkname);
      if idx2 < 0 then
      begin
        checkname := spart + ',' + itoa(color);
        idx2 := stmp2.IndexOf(checkname);
        if idx2 < 0 then
        begin
          checkname := spart + ',' + '(Unknown)';
          idx2 := stmp2.IndexOf(checkname);
        end;
      end;
      if idx2 >= 0 then
        stmp2.Delete(idx2);
    end;
    checkname := spart + ',' + pi.desc;
    if stmp2.IndexOf(checkname) < 0 then
    begin
      stmp2.Add(spart + ',' + pi.desc);
      S_BackupFile(basedefault + 'db\db_pieces.extra.txt');
      RemoveBlancLines(stmp2);
      S_SaveToFile(stmp2, basedefault + 'db\db_pieces.extra.txt');
    end;
    stmp2.Free;
  end;
  RefreshPartCategory(spart);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.AddKnownPiecePINV(const spart: string; const color: integer; const desc: string): boolean;
var
  newname: string;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  pcolor: colorinfo_p;
  tmppartname: string;
  checkname: string;
  idx2: integer;
  stmp1, stmp2: TStringList;
  kpfile: string;
begin
  if color < -1 then
  begin
    Result := False;
    Exit;
  end;

  newname := fixpartname(spart);
  Result := AddKnownPiece(newname, color, desc);
  if Result then
    Exit;

  if fcolorpieces.IndexOf(itoa(color) + ',' + newname) < 0 then
  begin
    pci := PieceColorInfo(newname, color);
    if pci = nil then
    begin
      pci := TPieceColorInfo.Create(newname, color);
      pcolor := Colors(color);
      if pcolor.knownpieces = nil then
        pcolor.knownpieces := THashStringList.Create;
      pcolor.knownpieces.AddObject(newname, pci);
    end;
    stmp1 := TStringList.Create;
    kpfile := basedefault + 'db\db_knownpieces.txt';
    if fexists(kpfile) then
      S_LoadFromFile(stmp1, kpfile)
    else
      stmp1.Add('Part,Color,Desc');
    stmp1.Add(newname + ',' + itoa(color) + ',' + desc);
    S_BackupFile(kpfile);
    RemoveBlancLines(stmp1);
    S_SaveToFile(stmp1, kpfile);
    stmp1.Free;
    Result := True;
  end;

  tmppartname := Trim(stringreplace(newname, ',', '', [rfReplaceAll, rfIgnoreCase]));
  if (PieceDesc(spart) = '') or
     (UpperCase(PieceDesc(newname)) = UpperCase(newname)) or
     (UpperCase(PieceDesc(newname)) = UpperCase(tmppartname)) then
  begin
    pi := PieceInfo(newname);
    if pi = nil then
    begin
      pi := TPieceInfo.Create;
      if desc = '' then
        pi.desc := spart
      else
        pi.desc := desc;
      pi.name := newname;
      pi.lname := LowerCase(newname);
      fpieces.AddObject(newname, pi);
      checkname := '';
    end
    else if pi = fstubpieceinfo then
    begin
      pi := TPieceInfo.Create;
      if desc = '' then
        pi.desc := spart
      else
        pi.desc := desc;
      pi.name := newname;
      pi.lname := LowerCase(newname);
      fpieces.AddObject(newname, pi);
      checkname := '';
    end
    else
    begin
      checkname := spart + ',' + tmppartname;
      pi.desc := desc;
    end;
    stmp2 := TStringList.Create;
    if fexists(basedefault + 'db\db_pieces.extra.txt') then
    begin
      S_LoadFromFile(stmp2, basedefault + 'db\db_pieces.extra.txt');
      RemoveBlancLines(stmp2);
    end;
    if checkname <> '' then
    begin
      idx2 := stmp2.IndexOf(checkname);
      if idx2 < 0 then
      begin
        checkname := spart + ',' + itoa(color);
        idx2 := stmp2.IndexOf(checkname);
        if idx2 < 0 then
        begin
          checkname := spart + ',' + '(Unknown)';
          idx2 := stmp2.IndexOf(checkname);
        end;
      end;
      if idx2 >= 0 then
        stmp2.Delete(idx2);
    end;
    checkname := spart + ',' + pi.desc;
    if stmp2.IndexOf(checkname) < 0 then
    begin
      stmp2.Add(spart + ',' + pi.desc);
      S_BackupFile(basedefault + 'db\db_pieces.extra.txt');
      RemoveBlancLines(stmp2);
      S_SaveToFile(stmp2, basedefault + 'db\db_pieces.extra.txt');
    end;
    stmp2.Free;
  end;
  RefreshPartCategory(spart);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdateNameFromRebrickable(const pid: string): boolean;
begin
  if Pos('-', pid) > 0 then
  begin
    Result := UpdateSetNameFromRebrickable(pid);
    if not Result then
      Result := UpdatePartNameFromRebrickable(pid);
  end
  else
  begin
    Result := UpdatePartNameFromRebrickable(pid);
    if not Result then
      Result := UpdateSetNameFromRebrickable(pid);
  end;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdatePartNameFromRebrickable(const pid: string): boolean;
var
  fname: string;
  SL: TStringList;
  urlstr: string;
  j, p1: integer;
  newpartname: string;
  spart: string;
  htm, htm1: string;
  check: string;
begin
  Result := False;
  spart := Trim(pid);
  if spart = '' then
    Exit;
  fname := basedefault + 'db\rmolds\' + spart + '.htm';
  urlstr := 'http://mail.rebrickable.com/parts/' + Trim(spart) + '/';
  if UrlDownloadToFile(nil, PChar(urlstr), PChar(fname), 0, nil) <> 0 then
    Exit;

  SL := TStringList.Create;
  S_LoadFromFile(SL, fname);
  htm := SL.Text;
  htm1 := UpperCase(htm);
  SL.Free;
  check := '<H1>LEGO PART ' + UpperCase(spart) + ' ';
  p1 := Pos(check, htm1);
  if p1 > 0 then
  begin
    newpartname := '';
    for j := p1 + Length(check) to Length(htm) do
    begin
      if htm[j] = '<' then
        Break
      else
      begin
        if htm[j] = ',' then
          newpartname := newpartname + ' '
        else
          newpartname := newpartname + htm[j];
      end;
    end;
    newpartname := Trim(RemoveSpecialTagsFromString(newpartname));
    if newpartname <> '' then
    begin
      SetMoldName(spart, newpartname);
      Result := True;
    end;
  end;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdateSetNameFromRebrickable(const pid: string): boolean;
var
  fname: string;
  SL: TStringList;
  urlstr: string;
  j, p1: integer;
  newpartname: string;
  spart: string;
  htm, htm1: string;
  check: string;
begin
  Result := False;
  spart := Trim(pid);
  if spart = '' then
    Exit;
  fname := basedefault + 'db\rmolds\' + spart + '.htm';
  urlstr := 'http://mail.rebrickable.com/sets/' + Trim(spart) + '/';
  if UrlDownloadToFile(nil, PChar(urlstr), PChar(fname), 0, nil) <> 0 then
    Exit;

  SL := TStringList.Create;
  S_LoadFromFile(SL, fname);
  htm := SL.Text;
  htm1 := UpperCase(htm);
  SL.Free;
  check := '<H1>LEGO SET ' + UpperCase(spart) + ' - ';
  p1 := Pos(check, htm1);
  if p1 > 0 then
  begin
    newpartname := '';
    for j := p1 + Length(check) to Length(htm) do
    begin
      if htm[j] = '<' then
        Break
      else
      begin
        if htm[j] = ',' then
          newpartname := newpartname + ' '
        else
          newpartname := newpartname + htm[j];
      end;
    end;
    newpartname := Trim(RemoveSpecialTagsFromString(newpartname));
    if newpartname <> '' then
    begin
      SetMoldName(spart, newpartname);
      Result := True;
    end;
  end;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.SetMoldName(const spart: string; const adesc: string): boolean;
var
  pi: TPieceInfo;
  doadd, dosave: boolean;
  stmp2: TStringList;
  i: integer;
  s_part, s_desc, s_color, s_year: string;
  newname: string;
  desc: string;
  ss: TSetExtraInfo;
  idx: integer;

  procedure _update_set_desc(const fname: string);
  var
    sl: TStringList;
    dosave: boolean;
    ii: integer;
  begin
    if not fexists(fname) then
      Exit;
    dosave := false;
    sl := TStringList.Create;
    if S_LoadFromFile(sl, fname) then
      if sl.Count > 0 then
        if sl.Strings[0] = 'set_id,descr,year' then
        begin
          for ii := 1 to sl.Count - 1 do
          begin
            splitstring(sl.Strings[ii], s_part, s_desc, s_year, ',');
            if s_part = newname then
              if s_desc <> desc then
              begin
                sl.Strings[ii] := spart + ',' + desc + ',' + s_year;
                dosave := true;
                break;
              end;
          end;
        end;
    if dosave then
    begin
      S_BackupFile(fname);
      S_SaveToFile(sl, fname);
    end;
    sl.Free;
  end;

begin
  Result := False;
  desc := '';
  for i := 1 to Length(adesc) do
  begin
    if adesc[i] <> ',' then
      desc := desc + adesc[i]
    else
    begin
      if i < Length(adesc) then
        if adesc[i + 1] = ' ' then
          desc := desc + ' -';
    end;
  end;

  newname := fixpartname(spart);
  pi := PieceInfo(newname);
  if pi = nil then
  begin
    pi := TPieceInfo.Create;
    if desc = '' then
      pi.desc := spart
    else
      pi.desc := desc;
    pi.name := newname;
    pi.lname := LowerCase(newname);
    fpieces.AddObject(newname, pi);
    doadd := True;
  end
  else if pi = fstubpieceinfo then
  begin
    pi := TPieceInfo.Create;
    if desc = '' then
      pi.desc := newname
    else
      pi.desc := desc;
    pi.name := newname;
    pi.lname := LowerCase(newname);
    fpieces.AddObject(newname, pi);
    doadd := True;
  end
  else
  begin
    if pi.desc = desc then
      if pi.desc <> '' then
        Exit;

    if desc = '' then
      pi.desc := newname
    else
      pi.desc := desc;
    doadd := False;
  end;
  Result := True;
  if doadd then
  begin
    stmp2 := TStringList.Create;
    if fexists(basedefault + 'db\db_pieces.extra.txt') then
    begin
      S_LoadFromFile(stmp2, basedefault + 'db\db_pieces.extra.txt');
      RemoveBlancLines(stmp2);
    end;
    stmp2.Add(newname + ',' + pi.desc);
    S_BackupFile(basedefault + 'db\db_pieces.extra.txt');
    RemoveBlancLines(stmp2);
    S_SaveToFile(stmp2, basedefault + 'db\db_pieces.extra.txt');
    stmp2.Free;
  end
  else
  begin
    stmp2 := TStringList.Create;
    if fexists(basedefault + 'db\db_pieces.txt') then
    begin
      dosave := False;
      S_LoadFromFile(stmp2, basedefault + 'db\db_pieces.txt');
      RemoveBlancLines(stmp2);
      if stmp2.Count > 1 then
        if stmp2.Strings[0] = 'piece_id,descr' then
        begin
          for i := 1 to stmp2.Count - 1 do
          begin
            splitstring(stmp2.Strings[i], s_part, s_desc, ',');
            if fixpartname(s_part) = newname then
            begin
              stmp2.Strings[i] := newname + ',' + pi.desc;
              dosave := True;
            end;
          end;
          if dosave then
          begin
            S_BackupFile(basedefault + 'db\db_pieces.txt');
            RemoveBlancLines(stmp2);
            S_SaveToFile(stmp2, basedefault + 'db\db_pieces.txt');
          end;
        end;
    end;
    stmp2.Clear;
    if fexists(basedefault + 'db\db_pieces.extra.txt') then
    begin
      dosave := False;
      S_LoadFromFile(stmp2, basedefault + 'db\db_pieces.extra.txt');
      RemoveBlancLines(stmp2);
      for i := 0 to stmp2.Count - 1 do
      begin
        splitstring(stmp2.Strings[i], s_part, s_desc, ',');
        if fixpartname(s_part) = newname then
        begin
          stmp2.Strings[i] := newname + ',' + pi.desc;
          dosave := True;
        end;
      end;
      if dosave then
      begin
        S_BackupFile(basedefault + 'db\db_pieces.extra.txt');
        RemoveBlancLines(stmp2);
        S_SaveToFile(stmp2, basedefault + 'db\db_pieces.extra.txt');
      end;
    end;
    stmp2.Clear;
    if fexists(basedefault + 'db\db_knownpieces.txt') then
    begin
      dosave := False;
      S_LoadFromFile(stmp2, basedefault + 'db\db_knownpieces.txt');
      RemoveBlancLines(stmp2);
      if stmp2.Count > 1 then
        if stmp2.Strings[0] = 'Part,Color,Desc' then
        begin
          for i := 1 to stmp2.Count - 1 do
          begin
            splitstring(stmp2.Strings[i], s_part, s_color, s_desc, ',');
            s_part := fixpartname(s_part);
            if s_part = newname then
            begin
              stmp2.Strings[i] := newname + ',' + s_color + ',' + pi.desc;
              dosave := True;
            end;
          end;
          if dosave then
          begin
            S_BackupFile(basedefault + 'db\db_knownpieces.txt');
            RemoveBlancLines(stmp2);
            S_SaveToFile(stmp2, basedefault + 'db\db_knownpieces.txt');
          end;
        end;
    end;
    stmp2.Free;
  end;
  idx := fsets.IndexOf(newname);
  if idx >= 0 then
  begin
    ss := fsets.Objects[idx] as TSetExtraInfo;
    ss.Text := desc;
    _update_set_desc(basedefault + 'db\db_sets.txt');
    _update_set_desc(basedefault + 'db\db_mocs.txt');
  end;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.AddMoldSet(const spart: string): boolean;
var
  i: integer;
  kpfile: string;
  desc: string;
  stmp1: TStringList;
  s1, s2, s3: string;
begin
  Result := True;
  desc := SetDesc(spart);
  stmp1 := TStringList.Create;
  kpfile := basedefault + 'db\db_knownpieces.txt';
  if fexists(kpfile) then
    S_LoadFromFile(stmp1, kpfile)
  else
    stmp1.Add('Part,Color,Desc');
  for i := 1 to stmp1.Count - 1 do
  begin
    splitstring(stmp1.Strings[i], s1, s2, s3, ',');
    s1 := fixpartname(s1);
    if s2 = '-1' then
      if s1 = spart then
      begin
        stmp1.Strings[i] := s1 + ',-1,' + desc;
        S_BackupFile(kpfile);
        RemoveBlancLines(stmp1);
        S_SaveToFile(stmp1, kpfile);
        stmp1.Free;
        Exit;
      end;
  end;
  stmp1.Add(spart + ',-1,' + desc);
  S_BackupFile(kpfile);
  S_SaveToFile(stmp1, kpfile);
  stmp1.Free;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.AddMoldColor(const spart: string; const color: integer): boolean;
var
  newname: string;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  pcolor: colorinfo_p;
  kpfile: string;
  desc: string;
  stmp1: TStringList;
  lineadd: string;
  ss: TSetExtraInfo;
  idx: integer;
begin
  Result := False;

  newname := fixpartname(spart);
  pi := PieceInfo(newname);
  if pi = nil then
    Exit;

  desc := pi.desc;

  pci := PieceColorInfo(newname, color);
  if pci = nil then
  begin
    pci := TPieceColorInfo.Create(newname, color);
    pcolor := Colors(color);
    if pcolor.knownpieces = nil then
      pcolor.knownpieces := THashStringList.Create;
    pcolor.knownpieces.AddObject(newname, pci);
    stmp1 := TStringList.Create;
    kpfile := basedefault + 'db\db_knownpieces.txt';
    if fexists(kpfile) then
      S_LoadFromFile(stmp1, kpfile)
    else
      stmp1.Add('Part,Color,Desc');

    lineadd := newname + ',' + itoa(color) + ',' + desc;
    if stmp1.IndexOf(lineadd) < 0 then
    begin
      stmp1.Add(lineadd);
      S_BackupFile(kpfile);
      RemoveBlancLines(stmp1);
      S_SaveToFile(stmp1, kpfile);
    end;

    stmp1.Free;
    CrawlerPriorityPart(newname, color);
    Result := True;

    if color = INSTRUCTIONCOLORINDEX then
    begin
      idx := fsets.IndexOf(newname);
      if idx >= 0 then
      begin
        ss := fsets.Objects[idx] as TSetExtraInfo;
        ss.hasinstructions := True;
      end;
    end
    else if color = BOXCOLORINDEX then
    begin
      idx := fsets.IndexOf(newname);
      if idx >= 0 then
      begin
        ss := fsets.Objects[idx] as TSetExtraInfo;
        ss.hasoriginalbox := True;
      end;
    end;
  end;
  pci.pieceinfo := pi;
end;
{$ENDIF}

function TSetsDatabase.GetMoldKnownColors(const spart: string): TDNumberList;
var
  i: integer;
  stmp: string;
begin
  Result := TDNumberList.Create;
  if Pos('BL ', spart) = 1 then
    stmp := fixpartname(RebrickablePart(Copy(spart, 4, Length(spart) - 3)))
  else
    stmp := fixpartname(RebrickablePart(spart));
  for i := -1 to MAXINFOCOLOR do
    if (fcolors[i].id = i) or (i = -1) then
      if fcolors[i].knownpieces <> nil then
        if fcolors[i].knownpieces.IndexOfUCS(stmp) >= 0 then
          Result.Add(i);
end;

function TSetsDatabase.GetMoldNumColors(const spart: string): integer;
var
  i: integer;
  stmp: string;
begin
  Result := 0;
  if Pos('BL ', spart) = 1 then
    stmp := fixpartname(RebrickablePart(Copy(spart, 4, Length(spart) - 3)))
  else
    stmp := fixpartname(RebrickablePart(spart));
  for i := -1 to MAXINFOCOLOR do
    if (fcolors[i].id = i) or (i = -1) then
      if fcolors[i].knownpieces <> nil then
        if fcolors[i].knownpieces.IndexOfUCS(stmp) >= 0 then
          inc(Result);
end;

function TSetsDatabase.GetMoldNumNormalPartColors(const spart: string): integer;
var
  i: integer;
  stmp: string;
begin
  Result := 0;
  if Pos('BL ', spart) = 1 then
    stmp := fixpartname(RebrickablePart(Copy(spart, 4, Length(spart) - 3)))
  else
    stmp := fixpartname(RebrickablePart(spart));
  for i := 0 to LASTNORMALCOLORINDEX do
    if fcolors[i].id = i then
      if fcolors[i].knownpieces <> nil then
        if fcolors[i].knownpieces.IndexOfUCS(stmp) >= 0 then
          inc(Result);
end;

function TSetsDatabase.HasSetColorsOnly(const spart: string): boolean;
var
  i, idx: integer;
  stmp: string;
  pci: TPieceColorInfo;
begin
  Result := False;
  if Pos('BL ', spart) = 1 then
    stmp := fixpartname(RebrickablePart(Copy(spart, 4, Length(spart) - 3)))
  else
    stmp := fixpartname(RebrickablePart(spart));
  for i := 0 to LASTNORMALCOLORINDEX do
    if fcolors[i].id = i then
      if fcolors[i].knownpieces <> nil then
        if fcolors[i].knownpieces.IndexOfUCS(stmp) >= 0 then
          Exit;

  idx := -1;
  if fcolors[-1].knownpieces <> nil then
  begin
    idx := fcolors[-1].knownpieces.IndexOfUCS(stmp);
    if idx < 0 then
      Exit;
  end;

  if fcolors[INSTRUCTIONCOLORINDEX].knownpieces <> nil then
    if fcolors[INSTRUCTIONCOLORINDEX].knownpieces.IndexOfUCS(stmp) >= 0 then
    begin
      Result := True;
      Exit;
    end;

  if fcolors[BOXCOLORINDEX].knownpieces <> nil then
    if fcolors[BOXCOLORINDEX].knownpieces.IndexOfUCS(stmp) >= 0 then
    begin
      Result := True;
      Exit;
    end;

  if idx >= 0 then
  begin
    pci := fcolors[-1].knownpieces.Objects[idx] as TPieceColorInfo;
    Result := (pci.sparttype = 'S') or (pci.parttype = TYPE_SET);
  end;

end;

function TSetsDatabase.HasMinifigColorsOnly(const spart: string): boolean;
var
  i, idx: integer;
  stmp: string;
  pci: TPieceColorInfo;
begin
  Result := False;
  if Pos('BL ', spart) = 1 then
    stmp := fixpartname(RebrickablePart(Copy(spart, 4, Length(spart) - 3)))
  else
    stmp := fixpartname(RebrickablePart(spart));
  for i := 0 to MAXINFOCOLOR do
    if fcolors[i].id = i then
      if fcolors[i].knownpieces <> nil then
        if fcolors[i].knownpieces.IndexOfUCS(stmp) >= 0 then
          Exit;

  if fcolors[-1].knownpieces <> nil then
  begin
    idx := fcolors[-1].knownpieces.IndexOfUCS(stmp);
    if idx < 0 then
      Exit;
    pci := fcolors[-1].knownpieces.Objects[idx] as TPieceColorInfo;
    Result := (pci.sparttype = 'M') or (pci.parttype = TYPE_MINIFIGURE);
  end;
end;

function TSetsDatabase.GetMoldColorsFlags(const spart: string): integer;
var
  i: integer;
  stmp: string;
begin
  if Pos('BL ', spart) = 1 then
    stmp := fixpartname(RebrickablePart(Copy(spart, 4, Length(spart) - 3)))
  else
    stmp := fixpartname(RebrickablePart(spart));

  Result := 0;

  for i := 0 to LASTNORMALCOLORINDEX do
    if (fcolors[i].id = i) or (i = -1) then
      if fcolors[i].knownpieces <> nil then
        if fcolors[i].knownpieces.IndexOfUCS(stmp) >= 0 then
        begin
          Result := Result or COLORFLAG_PART;
          break;
        end;

  if GetSetInventory(stmp) <> nil then
    Result := Result or COLORFLAG_SET;
end;

function TSetsDatabase.MoldHasNoColors(const spart: string): boolean;
var
  i: integer;
  stmp: string;
begin
  if Pos('BL ', spart) = 1 then
    stmp := fixpartname(RebrickablePart(Copy(spart, 4, Length(spart) - 3)))
  else
    stmp := fixpartname(RebrickablePart(spart));
  for i := -1 to MAXINFOCOLOR do
    if (fcolors[i].id = i) or (i = -1) then
      if fcolors[i].knownpieces <> nil then
        if fcolors[i].knownpieces.IndexOfUCS(stmp) >= 0 then
        begin
          Result := False;
          Exit;
        end;
  Result := True;
end;

procedure TSetsDatabase.LoadKnownPieces;
var
  i: integer;
  stmp, spart, scolor, sdesc: string;
  ncolor: integer;
  sl: TStringList;
  kpfile: string;
begin
  kpfile := basedefault + 'db\db_knownpieces.txt';
  if not fexists(kpfile) then
    Exit;

  sl := TStringList.Create;
  S_LoadFromFile(sl, kpfile);
  RemoveBlancLines(sl);
  for i := 1 to sl.Count - 1 do
  begin
    stmp := sl.Strings[i];
    splitstring(stmp, spart, scolor, sdesc, ',');
    if Pos('BL ', spart) = 1 then
      spart := RebrickablePart(fixpartname(Trim(Copy(spart, 4, Length(spart) - 3))))
    else
      spart := RebrickablePart(fixpartname(Trim(spart)));
    if Pos('BL', scolor) = 1 then
    begin
      scolor := Trim(Copy(scolor, 3, Length(scolor) - 2));
      ncolor := BrickLinkColorToSystemColor(atoi(scolor))
    end
    else if Pos('RB', scolor) = 1 then
    begin
      scolor := Trim(Copy(scolor, 3, Length(scolor) - 2));
      ncolor := RebrickableColorToSystemColor(atoi(scolor))
    end
    else
      ncolor := atoi(scolor);
    LoadKnownPiece(spart, ncolor, Trim(sdesc));
  end;
  sl.Free;
end;

procedure TSetsDatabase.LoadKnownPiece(const spart: string; const color: integer; const desc: string);
var
  newname: string;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  pcolor: colorinfo_p;
  {$IFNDEF CRAWLER}
  tmppartname: string;
  checkname: string;
  idx2: integer;
  stmp2: TStringList;
  needssave: boolean;
  {$ENDIF}
begin
  newname := fixpartname(spart);
  pci := PieceColorInfo(newname, color);
  if pci = nil then
  begin
    pci := TPieceColorInfo.Create(newname, color);
    pcolor := Colors(color);
    if pcolor.knownpieces = nil then
      pcolor.knownpieces := THashStringList.Create;
    pcolor.knownpieces.AddObject(newname, pci);
    CrawlerPriorityPart(newname, color);
  end;

  {$IFDEF CRAWLER}
  pi := PieceInfo(newname);
  if (pi = nil) or (pi = fstubpieceinfo) then
  begin
    pi := TPieceInfo.Create;
    pi.name := newname;
    pi.lname := LowerCase(newname);
    fpieces.AddObject(newname, pi);
  end;
  {$ELSE}
  tmppartname := Trim(stringreplace(newname, ',', '', [rfReplaceAll, rfIgnoreCase]));
  if (PieceDesc(newname) = '') or
     (UpperCase(PieceDesc(newname)) = UpperCase(newname)) or
     (UpperCase(PieceDesc(newname)) = UpperCase(tmppartname)) then
  begin
    pi := PieceInfo(newname);
    needssave := false;
    if pi = nil then
    begin
      needssave := True;
      pi := TPieceInfo.Create;
      if desc = '' then
        pi.desc := spart
      else
        pi.desc := desc;
      pi.name := newname;
      pi.lname := LowerCase(newname);
      fpieces.AddObject(newname, pi);
      checkname := '';
    end
    else if pi = fstubpieceinfo then
    begin
      pi := TPieceInfo.Create;
      if desc = '' then
        pi.desc := newname
      else
        pi.desc := desc;
      pi.name := newname;
      pi.lname := LowerCase(newname);
      fpieces.AddObject(newname, pi);
      checkname := '';
    end
    else if pi.desc <> desc then
    begin
      checkname := newname + ',' + tmppartname;
      pi.desc := desc;
    end
    else
    begin
      needssave := False;
      checkname := '';
    end;
    pci.pieceinfo := pi;
    if needssave then
    begin
      stmp2 := TStringList.Create;
      if fexists(basedefault + 'db\db_pieces.extra.txt') then
      begin
        S_LoadFromFile(stmp2, basedefault + 'db\db_pieces.extra.txt');
        RemoveBlancLines(stmp2);
      end;
      if checkname <> '' then
      begin
        idx2 := stmp2.IndexOf(checkname);
        if idx2 < 0 then
        begin
          checkname := newname + ',' + itoa(color);
          idx2 := stmp2.IndexOf(checkname);
          if idx2 < 0 then
          begin
            checkname := newname + ',' + '(Unknown)';
            idx2 := stmp2.IndexOf(checkname);
          end;
        end;
        if idx2 >= 0 then
          stmp2.Delete(idx2);
      end;
      stmp2.Add(newname + ',' + pi.desc);
      S_BackupFile(basedefault + 'db\db_pieces.extra.txt');
      RemoveBlancLines(stmp2);
      S_SaveToFile(stmp2, basedefault + 'db\db_pieces.extra.txt');
      stmp2.Free;
    end;
  end;
  {$ENDIF}
end;

procedure TSetsDatabase.SetSetIsGear(const sid: string; const value: boolean);
var
  s: TStringList;
  fname: string;
  idx: integer;
  pci: TPieceColorInfo;
begin
  if IsGear(sid) = value then
    Exit;
  s := TStringList.Create;
  fname := basedefault + 'db\db_gears.txt';
  if fexists(fname) then
    S_LoadFromFile(s, fname);
  if value then
    s.Add(sid)
  else
  begin
    idx := s.IndexOf(sid);
    if idx >= 0 then
      s.Delete(idx);
  end;
  S_SaveToFile(s, fname);
  s.Free;

  pci := PieceColorInfo(sid, -1);
  if pci <> nil then
  begin
    if value then
      pci.parttype := TYPE_GEAR
    else if IsBook(sid) then
      pci.parttype := TYPE_BOOK
    else
      pci.parttype := TYPE_SET;
  end;
end;

function TSetsDatabase.IsGear(const sid: string): boolean;
var
  s: THashStringList;
  fname: string;
begin
  s := THashStringList.Create;
  fname := basedefault + 'db\db_gears.txt';
  if fexists(fname) then
    SH_LoadFromFile(s, fname);
  Result := s.IndexOfUCS(sid) >= 0;
  s.Free;
end;

function TSetsDatabase.IsMinifigure(const mid: string): boolean;
var
  pci: TPieceColorInfo;
begin
  Result := False;
  pci := PieceColorInfo(mid, -1);
  if pci <> nil then
    if pci.sparttype = 'M' then
      Result := True;
end;

{$IFNDEF CRAWLER}
function TSetsDatabase.AutoFixSticker(const stk: string): boolean;
var
  pci1, pci2: TPieceColorInfo;
begin
  Result := False;

  if Pos('stk', stk) <= 0 then
    Exit;

  pci1 := PieceColorInfo(stk, -1);
  if pci1 = nil then
    Exit;

  pci2 := PieceColorInfo(stk, 9999);
  if pci2 = nil then
    Exit;

  if pci2.year <= 0 then
    if pci1.year > 1931 then
      if pci1.year <= 2050 then
      begin
        SetItemYear(pci2, pci1.year);
        Result := True;
      end;

  if pci2.sparttype = ' ' then
    if pci1.sparttype = 'P' then
    begin
      SetPartType(pci2.piece, pci2.color, 'P');
      Result := True;
    end;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.DownloadPartInventory(const s: string): boolean;
begin
  Result := UpdatePartInventory(s, True);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdatePartInventory(const s: string; const forcedownload: boolean): boolean;
var
  fname: string;
  blname: string;
  urlstr: string;
  htm: string;
  slist: TStringList;
  stmp1: TStringList;
  stmp2: TStringList;
  tmp1, tmp2: string;
  extra1: TStringList;
  extra2: TStringList;
  extra3: TStringList;
  p1, p2: integer;
  i, j: integer;
  bl_snum: string;
  bl_num: integer;
  bl_part: string;
  bl_scolor: string;
  bl_color: integer;
  inbr: boolean;
  p_is_B: boolean;
  p_is_G: boolean;
  p_is_P: boolean;
  p_is_S: boolean;
  p_is_M: boolean;
  sout: TStringList;
  pi: TPieceInfo;
  pci: TPieceColorInfo;
  pcolor: colorinfo_p;
  tmppartname: string;
  newpartname: string;
  checkname: string;
  idx, idx2: integer;
  kc: TDNumberList;
  s_part, s_desc: string;
begin
  blname := GetBLNetPieceName(Trim(s));
  fname := basedefault + 'db\parts\' + blname + '.htm';
  if forcedownload then
  begin
    urlstr := sbricklink + 'catalogItemInv.asp?P=' + blname + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
    if UrlDownloadToFile(nil, PChar(urlstr), PChar(fname), 0, nil) <> 0 then
    begin
      Result := False;
      Exit;
    end;
  end
  else
  begin
    if not fexists(fname) then
    begin
      urlstr := sbricklink + 'catalogItemInv.asp?P=' + blname + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
      if UrlDownloadToFile(nil, PChar(urlstr), PChar(fname), 0, nil) <> 0 then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;

  sout := TStringList.Create;
  sout.Add('Part,Color,Num');

  slist := TStringList.Create;
  stmp1 := TStringList.Create;
  try
    S_LoadFromFile(slist, fname);
    tmp1 := slist.Text;

    extra1 := ParseKnownPiecesFromHTML(tmp1);
    extra2 := TStringList.Create;
    extra3 := TStringList.Create;
    p1 := Pos('Regular Items:', tmp1);
    p2 := Pos('Extra Items:', tmp1);
    if p2 < 1 then
      p2 := Pos('Counterparts:', tmp1);
    if p2 < 1 then
      p2 := Pos('Alternate Items:', tmp1);
    if p2 < 1 then
      p2 := Pos('Summary:', tmp1);
    if (p1 > 0) then
      if (p2 > p1) then
      begin
        tmp2 := Copy(tmp1, p1, p2 - p1);
        tmp1 := StringReplace(tmp2, '�', '"', [rfReplaceAll, rfIgnoreCase]);
        tmp2 := StringReplace(tmp1, '&nbsp;', ' ', [rfReplaceAll, rfIgnoreCase]);
        tmp1 := StringReplace(tmp2, '&#40;', '(', [rfReplaceAll, rfIgnoreCase]);
        tmp2 := StringReplace(tmp1, '&#41;', ')', [rfReplaceAll, rfIgnoreCase]);
        tmp1 := StringReplace(tmp2, '&#39;', '''', [rfReplaceAll, rfIgnoreCase]);
        tmp2 := StringReplace(tmp1, '&#38;', '&', [rfReplaceAll, rfIgnoreCase]);
        tmp1 := StringReplace(tmp2, '</TR>', '</TR>'#13#10, [rfReplaceAll, rfIgnoreCase]);
        tmp2 := StringReplace(tmp1, '&amp;', '&', [rfReplaceAll, rfIgnoreCase]);
        slist.Text := tmp2;
        for i := 0 to slist.Count - 1 do
        begin
          tmp1 := slist.Strings[i];
          if Pos('<A HREF="/v2/catalog/catalogitem.page?', tmp1) > 0 then
          begin
            tmp2 := StringReplace(tmp1, '</TD>', '</TD>'#13#10, [rfReplaceAll, rfIgnoreCase]);
            stmp1.Text := tmp2;
            if stmp1.Count = 6 then
            begin
              tmp1 := Trim(stmp1.Strings[1]); // qty
              bl_snum := '';
              inbr := False;
              for j := 1 to Length(tmp1) do
              begin
                if tmp1[j] = '<' then
                  inbr := True
                else if tmp1[j] = '>' then
                  inbr := False
                else if not inbr and (tmp1[j] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) then
                  bl_snum := bl_snum + tmp1[j];
              end;
              bl_num := atoi(bl_snum, -1);

              bl_part := '';
              bl_scolor := '';
              tmp1 := stmp1.Strings[2]; // color & part
              p1 := Pos('idColor=', tmp1);
              p_is_B := False;
              p_is_G := False;
              p_is_P := False;
              p_is_S := False;
              p_is_M := False;
              if p1 > 0 then
              begin
                p_is_P := True;
                for j := p1 to Length(tmp1) do
                begin
                  if tmp1[j] = '>' then
                    Break;
                  if tmp1[j] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] then
                    bl_scolor := bl_scolor + tmp1[j]
                  else if bl_scolor <> '' then
                    Break;
                end;

                p1 := Pos('/v2/catalog/catalogitem.page?P=', tmp1);
                if p1 <= 0 then
                begin
                  p1 := Pos('/v2/catalog/catalogitem.page?G=', tmp1);
                  if p1 > 0 then
                  begin
                    p_is_P := False;
                    p_is_G := True;
                  end;
                end;
                if p1 > 0 then
                  for j := p1 + 31 to Length(tmp1) do
                  begin
                    if tmp1[j] = '&' then
                      Break
                    else
                      bl_part := bl_part + tmp1[j];
                  end;
              end
              else
              begin
                p1 := Pos('/v2/catalog/catalogitem.page?M=', tmp1);
                if p1 > 0 then
                begin
                  p_is_M := True;
                  bl_scolor := '0';
                  for j := p1 + 31 to Length(tmp1) do
                  begin
                    if tmp1[j] = '"' then
                      Break
                    else
                      bl_part := bl_part + tmp1[j];
                  end;
                end;

                if p1 <= 0 then
                begin
                  p1 := Pos('/v2/catalog/catalogitem.page?G=', tmp1);
                  if p1 > 0 then
                  begin
                    p_is_G := True;
                    bl_scolor := '0';
                    for j := p1 + 31 to Length(tmp1) do
                    begin
                      if tmp1[j] = '"' then
                        Break
                      else
                        bl_part := bl_part + tmp1[j];
                    end;
                  end;
                end;

                if p1 <= 0 then
                begin
                  p1 := Pos('/v2/catalog/catalogitem.page?B=', tmp1);
                  if p1 > 0 then
                  begin
                    p_is_B := True;
                    bl_scolor := '0';
                    for j := p1 + 31 to Length(tmp1) do
                    begin
                      if tmp1[j] = '"' then
                        Break
                      else
                        bl_part := bl_part + tmp1[j];
                    end;
                  end;
                end;

                if p1 <= 0 then
                begin
                  p1 := Pos('/v2/catalog/catalogitem.page?S=', tmp1);
                  if p1 > 0 then
                  begin
                    p_is_S := True;
                    bl_scolor := '0';
                    for j := p1 + 31 to Length(tmp1) do
                    begin
                      if tmp1[j] = '"' then
                        Break
                      else
                        bl_part := bl_part + tmp1[j];
                    end;
                  end;
                end;

              end;
              bl_scolor := Trim(bl_scolor);
              if bl_scolor = '' then
                bl_color := 0
              else
                bl_color := atoi(bl_scolor, -2);

              bl_part := fixpartname(Trim(bl_part));

              if (bl_color > -2) and (bl_part <> '') and (bl_num > 0) then
              begin
                if p_is_S or p_is_B then
                begin
                  sout.Add('BL ' + bl_part + ',-1,' + bl_snum);
                  pci := PieceColorInfo(RebrickablePart(bl_part), -1);
                end
                else
                begin
                  if bl_color = 0 then
                  begin
                    sout.Add('BL ' + bl_part + ',-2,' + bl_snum);
                    pci := nil;
                  end
                  else
                  begin
                    sout.Add('BL ' + bl_part + ',' + 'BL ' + bl_scolor + ',' + bl_snum);
                    pci := PieceColorInfo(RebrickablePart(bl_part), BrickLinkColorToSystemColor(bl_color));
                  end;
                end;
                if pci = nil then
                begin
                  if bl_color <> 0 then
                  begin
                    pci := TPieceColorInfo.Create(RebrickablePart(bl_part), BrickLinkColorToSystemColor(bl_color));
                    pcolor := Colors(BrickLinkColorToSystemColor(bl_color));
                    if pcolor.knownpieces = nil then
                      pcolor.knownpieces := THashStringList.Create;
                    pcolor.knownpieces.AddObject(RebrickablePart(bl_part), pci);
                  end;
                end;
                if pci <> nil then
                  pci.UpdatePartReference(s, bl_num);

                tmppartname := Trim(stringreplace(bl_part, ',', '', [rfReplaceAll, rfIgnoreCase]));
                if (PieceDesc(RebrickablePart(bl_part)) = '') or
                   (UpperCase(PieceDesc(RebrickablePart(bl_part))) = UpperCase(bl_part)) or
                   (UpperCase(PieceDesc(RebrickablePart(bl_part))) = UpperCase(tmppartname)) then
                begin
                  pi := PieceInfo(bl_part);
                  newpartname := Trim(smallparsepartname(bl_part, stmp1.Strings[0]));
                  if pi = nil then
                  begin
                    pi := TPieceInfo.Create;
                    pi.desc := newpartname;
                    pi.name := bl_part;
                    pi.lname := LowerCase(bl_part);
                    idx := fpieces.AddObject(bl_part, pi);
                    checkname := '';
                  end
                  else if pi = fstubpieceinfo then
                  begin
                    pi := TPieceInfo.Create;
                    pi.desc := newpartname;
                    pi.name := bl_part;
                    pi.lname := LowerCase(bl_part);
                    idx := fpieces.AddObject(bl_part, pi);
                    checkname := bl_part + ',' + tmppartname;
                  end
                  else
                  begin
                    checkname := bl_part + ',' + tmppartname;
                    pi.desc := newpartname;
                  end;
                  stmp2 := TStringList.Create;
                  if fexists(basedefault + 'db\db_pieces.extra.txt') then
                  begin
                    S_LoadFromFile(stmp2, basedefault + 'db\db_pieces.extra.txt');
                    RemoveBlancLines(stmp2);
                  end;
                  if checkname <> '' then
                  begin
                    idx2 := stmp2.IndexOf(checkname);
                    if idx2 < 0 then
                    begin
                      checkname := bl_part + ',' + bl_part;
                      idx2 := stmp2.IndexOf(checkname);
                      if idx2 < 0 then
                      begin
                        checkname := bl_part + ',' + '(Unknown)';
                        idx2 := stmp2.IndexOf(checkname);
                      end;
                    end;
                    if idx2 >= 0 then
                      stmp2.Delete(idx2);
                  end;
                  stmp2.Add(bl_part + ',' + pi.desc);
                  if bl_color <> 0 then
                    extra2.Add('BL ' + bl_part + ',' + 'BL ' + bl_scolor + ',' + pi.desc)
                  else
                    extra3.Add(bl_part + ',' + pi.desc);
                  S_BackupFile(basedefault + 'db\db_pieces.extra.txt');
                  RemoveBlancLines(stmp2);
                  S_SaveToFile(stmp2, basedefault + 'db\db_pieces.extra.txt');
                  stmp2.Free;
                  RefreshPartCategory(bl_part);
                end;
              end;
            end;
          end;
        end;
      end;

    if extra1.Count > 0 then
    begin
      AddKnownPieces(extra1);
      if extra1.Strings[0] <> 'Part,Color,Desc' then
        extra1.Insert(0, 'Part,Color,Desc');
      S_SaveToFile(extra1, basedefault + 'db\parts\' + blname + '.alternatives.txt');
    end;
    extra1.Free;
    AddKnownPieces(extra2);
    extra2.Free;
    if extra3.Count > 0 then
    begin
      kc := GetMoldKnownColors(Trim(s));
      if kc <> nil then
      begin
        for i := 0 to extra3.Count - 1 do
        begin
          splitstring(extra3.Strings[i], s_part, s_desc, ',');
          s_part := RebrickablePart(s_part);
          for j := 0 to kc.Count - 1 do
            AddKnownPiece(s_part, kc.Numbers[j], s_desc);
        end;
        kc.Free;
      end;
    end;
    extra3.Free;
  finally
    slist.Free;
    stmp1.Free;
  end;

  if sout.Count > 1 then
  begin
    S_SaveToFile(sout, basedefault + 'db\parts\' + blname + '.txt');
    fbinaryparts.UpdatePartFromText(blname, sout);
    if fpartsinventories.IndexOf(blname) < 0 then
    begin
      fpartsinventories.Add(blname);
      fname := basedefault + 'db\db_pieces_inventories.txt';
      S_BackupFile(fname);
      S_AppendLineToFile(fname, blname);
//      S_SaveToFile(fpartsinventories, fname);
    end;
    MarkInventoriedPart(s);
    inc(fpartsinventoriesvalidcount);
  end;

  sout.Free;

  Result := True;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.SetPartInventory(const s: string; const stext: string): boolean;
var
  sout: TStringList;
begin
  sout := TStringList.Create;
  try
    sout.Text := stext;
    Result := SetPartInventory(s, sout);
  finally
    sout.Free;
  end;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.SetPartInventory(const s: string; const sout: TStringList): boolean;
var
  blname: string;
  idx: integer;
  fname: string;
begin
  blname := GetBLNetPieceName(Trim(s));
  if sout.Count > 1 then
  begin
    S_SaveToFile(sout, basedefault + 'db\parts\' + blname + '.txt');
    fbinaryparts.UpdatePartFromText(blname, sout);
    if fpartsinventories.IndexOf(blname) < 0 then
    begin
      fpartsinventories.Add(blname);
      fname := basedefault + 'db\db_pieces_inventories.txt';
      S_BackupFile(fname);
      S_AppendLineToFile(fname, blname);
//      S_SaveToFile(fpartsinventories, fname);
    end;
    MarkInventoriedPart(s);
    inc(fpartsinventoriesvalidcount);
    Result := True;
  end
  else
  begin
    idx := fpartsinventories.IndexOf(blname);
    if idx >= 0 then
    begin
      fbinaryparts.DeletePart(blname);
      fpartsinventories.Delete(idx);
      S_BackupFile(fname);
      S_SaveToFile(fpartsinventories, fname);
    end;
    MarkUnInventoriedPart(s);
    inc(fpartsinventoriesvalidcount);
    Result := False;
  end;
end;
{$ENDIF}


{$IFNDEF CRAWLER}
function TSetsDatabase.DownloadSetFromBricklinkNew(const s: string; const typ1: char = ' '): boolean;
var
  tmpname: string;
  urlstr: string;
  slist: TStringList;
  tmp1: string;
  tmp2: string;
  p1: Integer;
  p2: Integer;
  i, j: integer;
  stmp1: TStringList;
  stmp2: TStringList;
  sout: TStringList;
  bl_part, bl_scolor, bl_snum: string;
  bl_num, bl_color: integer;
  inbr: boolean;
  newsetname: string;
  pci: TPieceColorInfo;
  idx: integer;
  pi: TPieceInfo;
  pcolor: colorinfo_p;
  yyy: Integer;
  tmppartname: string;
  newpartname: string;
  checkname: string;
  idx2: integer;
  typ: string;
  extral: TStringList;
  didP: boolean;
  didS: boolean;
  didM: boolean;
  didB: boolean;
  didG: boolean;
  p_is_B: boolean;
  p_is_G: boolean;
  p_is_P: boolean;
  p_is_S: boolean;
  p_is_M: boolean;
  sdesc: string;
  assets: setasset_t;
  ptype: char;
  yearfname, yearurl, trysequence: string;
begin
  tmpname := I_NewTempFile('tmpset_' + Trim(s));
  didB := False;
  didP := False;
  didS := False;
  didM := False;
  didG := False;
  Result := False;
  typ := '';
  try
    pi := PieceInfo(s);
    if pi <> nil then
      if pi.category = 961 then
      begin
        urlstr := sbricklink + 'catalogItemInv.asp?P=' + GetBLNetPieceName(Trim(s)) + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
        typ := 'P';
        Result := UrlDownloadToFile(nil, PChar(urlstr), PChar(tmpname), 0, nil) = 0;
        didP := True;
      end;

    if not Result then
    begin
      if typ1 = 'G' then
      begin
        urlstr := sbricklink + 'catalogItemInv.asp?G=' + GetBLNetPieceName(Trim(s)) + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
        typ := 'G';
        Result := UrlDownloadToFile(nil, PChar(urlstr), PChar(tmpname), 0, nil) = 0;
        didG := True;
      end;
    end;

    if not Result then
    begin
      if typ1 in ['M', 'B'] then
      begin
        if typ1 = 'M' then
        begin
          urlstr := sbricklink + 'catalogItemInv.asp?M=' + GetBLNetPieceName(Trim(s)) + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
          typ := 'M';
          Result := UrlDownloadToFile(nil, PChar(urlstr), PChar(tmpname), 0, nil) = 0;
          didM := True;
        end;
        if typ1 = 'B' then
        begin
          urlstr := sbricklink + 'catalogItemInv.asp?B=' + GetBLNetPieceName(Trim(s)) + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
          typ := 'B';
          Result := UrlDownloadToFile(nil, PChar(urlstr), PChar(tmpname), 0, nil) = 0;
          didB := True;
        end;
      end;
    end;

    if not Result then
    begin
      urlstr := sbricklink + 'catalogItemInv.asp?S=' + GetBLNetPieceName(Trim(s)) + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
      typ := 'S';
      Result := UrlDownloadToFile(nil, PChar(urlstr), PChar(tmpname), 0, nil) = 0;
      didS := True;
    end;
    if not Result then
      if not didB then
      begin
        urlstr := sbricklink + 'catalogItemInv.asp?B=' + GetBLNetPieceName(Trim(s)) + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
        typ := 'B';
        Result := UrlDownloadToFile(nil, PChar(urlstr), PChar(tmpname), 0, nil) = 0;
        didB := True;
      end;
    if not Result then
      if not didM then
      begin
        urlstr := sbricklink + 'catalogItemInv.asp?M=' + GetBLNetPieceName(Trim(s)) + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
        typ := 'M';
        Result := UrlDownloadToFile(nil, PChar(urlstr), PChar(tmpname), 0, nil) = 0;
        didM := True;
      end;
    if not Result then
      if not didP then
      begin
        urlstr := sbricklink + 'catalogItemInv.asp?P=' + GetBLNetPieceName(Trim(s)) + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
        typ := 'P';
        Result := UrlDownloadToFile(nil, PChar(urlstr), PChar(tmpname), 0, nil) = 0;
        didP := True;
      end;
  except
    Result := False;
    Exit;
  end;
  if not Result then
  begin
    Exit;
  end;

  sout := TStringList.Create;
  sout.Add('Part,Color,Num');

  newsetname := '';

  slist := TStringList.Create;
  stmp1 := TStringList.Create;
  try
    S_LoadFromFile(slist, tmpname);
    tmp1 := slist.Text;
    p1 := Pos('SIZE="+0"><B>', tmp1);
    if p1 = 0 then
    begin
      if not didM then
      begin
        urlstr := sbricklink + 'catalogItemInv.asp?M=' + GetBLNetPieceName(Trim(s)) + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
        typ := 'M';
        Result := UrlDownloadToFile(nil, PChar(urlstr), PChar(tmpname), 0, nil) = 0;
        if Result then
        begin
          S_LoadFromFile(slist, tmpname);
          tmp1 := slist.Text;
          p1 := Pos('SIZE="+0"><B>', tmp1);
        end;
      end;
      if p1 = 0 then
      begin
        if not didP then
        begin
          urlstr := sbricklink + 'catalogItemInv.asp?P=' + GetBLNetPieceName(Trim(s)) + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
          typ := 'P';
          Result := UrlDownloadToFile(nil, PChar(urlstr), PChar(tmpname), 0, nil) = 0;
          if Result then
          begin
            S_LoadFromFile(slist, tmpname);
            tmp1 := slist.Text;
            p1 := Pos('SIZE="+0"><B>', tmp1);
          end;
        end;
      end;
      if p1 = 0 then
      begin
        if not didS then
        begin
          urlstr := sbricklink + 'catalogItemInv.asp?S=' + GetBLNetPieceName(Trim(s)) + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
          typ := 'S';
          Result := UrlDownloadToFile(nil, PChar(urlstr), PChar(tmpname), 0, nil) = 0;
          if Result then
          begin
            S_LoadFromFile(slist, tmpname);
            tmp1 := slist.Text;
            p1 := Pos('SIZE="+0"><B>', tmp1);
          end;
        end;
      end;
      if p1 = 0 then
      begin
        urlstr := sbricklink + 'catalogItemInv.asp?G=' + GetBLNetPieceName(Trim(s)) + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
        typ := 'G';
        Result := UrlDownloadToFile(nil, PChar(urlstr), PChar(tmpname), 0, nil) = 0;
        if Result then
        begin
          S_LoadFromFile(slist, tmpname);
          tmp1 := slist.Text;
          p1 := Pos('SIZE="+0"><B>', tmp1);
        end;
        didG := True;
      end;
      if p1 = 0 then
      begin
        urlstr := sbricklink + 'catalogItemInv.asp?B=' + GetBLNetPieceName(Trim(s)) + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
        typ := 'B';
        Result := UrlDownloadToFile(nil, PChar(urlstr), PChar(tmpname), 0, nil) = 0;
        if Result then
        begin
          S_LoadFromFile(slist, tmpname);
          tmp1 := slist.Text;
          p1 := Pos('SIZE="+0"><B>', tmp1);
        end;
        didG := True;
      end;
    end;
    if p1 > 0 then
    begin
      for j := p1 + 13 to Length(tmp1) do
      begin
        if tmp1[j] = '<' then
          Break
        else
          newsetname := newsetname + tmp1[j];
      end;
    end;
    extral := ParseKnownPiecesFromHTML(tmp1);
    p1 := Pos('Regular Items:', tmp1);
    p2 := Pos('Extra Items:', tmp1);
    if p2 < 1 then
      p2 := Pos('Counterparts:', tmp1);
    if p2 < 1 then
      p2 := Pos('Alternate Items:', tmp1);
    if p2 < 1 then
      p2 := Pos('Summary:', tmp1);
    if (p1 > 0) then
      if (p2 > p1) then
      begin
        tmp2 := Copy(tmp1, p1, p2 - p1);
        tmp1 := StringReplace(tmp2, '�', '"', [rfReplaceAll, rfIgnoreCase]);
        tmp2 := StringReplace(tmp1, '&nbsp;', ' ', [rfReplaceAll, rfIgnoreCase]);
        tmp1 := StringReplace(tmp2, '&#40;', '(', [rfReplaceAll, rfIgnoreCase]);
        tmp2 := StringReplace(tmp1, '&#41;', ')', [rfReplaceAll, rfIgnoreCase]);
        tmp1 := StringReplace(tmp2, '&#39;', '''', [rfReplaceAll, rfIgnoreCase]);
        tmp2 := StringReplace(tmp1, '&#38;', '&', [rfReplaceAll, rfIgnoreCase]);
        tmp1 := StringReplace(tmp2, '</TR>', '</TR>'#13#10, [rfReplaceAll, rfIgnoreCase]);
        tmp2 := StringReplace(tmp1, '&amp;', '&', [rfReplaceAll, rfIgnoreCase]);
        slist.Text := tmp2;
        for i := 0 to slist.Count - 1 do
        begin
          tmp1 := slist.Strings[i];
          if Pos('<A HREF="/v2/catalog/catalogitem.page?', tmp1) > 0 then
          begin
            tmp2 := StringReplace(tmp1, '</TD>', '</TD>'#13#10, [rfReplaceAll, rfIgnoreCase]);
            stmp1.Text := tmp2;
            if stmp1.Count = 6 then
            begin
              tmp1 := Trim(stmp1.Strings[1]); // qty
              bl_snum := '';
              inbr := False;
              for j := 1 to Length(tmp1) do
              begin
                if tmp1[j] = '<' then
                  inbr := True
                else if tmp1[j] = '>' then
                  inbr := False
                else if not inbr and (tmp1[j] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) then
                  bl_snum := bl_snum + tmp1[j];
              end;
              bl_num := atoi(bl_snum, -1);

              bl_part := '';
              bl_scolor := '';
              tmp1 := stmp1.Strings[2]; // color & part
              p1 := Pos('idColor=', tmp1);
              p_is_B := False;
              p_is_G := False;
              p_is_P := False;
              p_is_S := False;
              p_is_M := False;
              ptype := ' ';
              if p1 > 0 then
              begin
                p_is_P := True;
                for j := p1 to Length(tmp1) do
                begin
                  if tmp1[j] = '>' then
                    Break;
                  if tmp1[j] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] then
                    bl_scolor := bl_scolor + tmp1[j]
                  else if bl_scolor <> '' then
                    Break;
                end;

                p1 := Pos('/v2/catalog/catalogitem.page?P=', tmp1);
                if p1 <= 0 then
                begin
                  p1 := Pos('/v2/catalog/catalogitem.page?G=', tmp1);
                  if p1 > 0 then
                  begin
                    p_is_P := False;
                    p_is_G := True;
                    ptype := 'G';
                  end
                end
                else
                  ptype := 'P';
                if p1 > 0 then
                  for j := p1 + 31 to Length(tmp1) do
                  begin
                    if tmp1[j] = '&' then
                      Break
                    else
                      bl_part := bl_part + tmp1[j];
                  end;
              end
              else
              begin
                p1 := Pos('/v2/catalog/catalogitem.page?M=', tmp1);
                if p1 > 0 then
                begin
                  p_is_M := True;
                  bl_scolor := '0';
                  for j := p1 + 31 to Length(tmp1) do
                  begin
                    if tmp1[j] = '"' then
                      Break
                    else
                      bl_part := bl_part + tmp1[j];
                  end;
                  ptype := 'M';
                end;

                if p1 <= 0 then
                begin
                  p1 := Pos('/v2/catalog/catalogitem.page?G=', tmp1);
                  if p1 > 0 then
                  begin
                    p_is_G := True;
                    bl_scolor := '0';
                    for j := p1 + 31 to Length(tmp1) do
                    begin
                      if tmp1[j] = '"' then
                        Break
                      else
                        bl_part := bl_part + tmp1[j];
                    end;
                    ptype := 'G';
                  end;
                end;

                if p1 <= 0 then
                begin
                  p1 := Pos('/v2/catalog/catalogitem.page?B=', tmp1);
                  if p1 > 0 then
                  begin
                    p_is_B := True;
                    bl_scolor := '0';
                    for j := p1 + 31 to Length(tmp1) do
                    begin
                      if tmp1[j] = '"' then
                        Break
                      else
                        bl_part := bl_part + tmp1[j];
                    end;
                    ptype := 'B';
                  end;
                end;

                if p1 <= 0 then
                begin
                  p1 := Pos('/v2/catalog/catalogitem.page?S=', tmp1);
                  if p1 > 0 then
                  begin
                    p_is_S := True;
                    bl_scolor := '0';
                    for j := p1 + 31 to Length(tmp1) do
                    begin
                      if tmp1[j] = '"' then
                        Break
                      else
                        bl_part := bl_part + tmp1[j];
                    end;
                    ptype := 'S';
                  end;
                end;

              end;
              bl_scolor := Trim(bl_scolor);
              bl_color := atoi(bl_scolor, -2);
              bl_part := fixpartname(Trim(bl_part));

              if (bl_color > -2) and (bl_part <> '') and (bl_num > 0) then
              begin
                if p_is_S or p_is_B then
                begin
                  sout.Add('BL ' + bl_part + ',-1,' + bl_snum);
                  pci := PieceColorInfo(RebrickablePart(bl_part), -1);
                end
                else
                begin
                  sout.Add('BL ' + bl_part + ',' + 'BL ' + bl_scolor + ',' + bl_snum);
                  pci := PieceColorInfo(RebrickablePart(bl_part), BrickLinkColorToSystemColor(bl_color));
                end;
                if pci = nil then
                begin
                  pci := TPieceColorInfo.Create(RebrickablePart(bl_part), BrickLinkColorToSystemColor(bl_color));
                  pcolor := Colors(BrickLinkColorToSystemColor(bl_color));
                  if pcolor.knownpieces = nil then
                    pcolor.knownpieces := THashStringList.Create;
                  pcolor.knownpieces.AddObject(RebrickablePart(bl_part), pci);
                end;
                pci.UpdateSetReference(s, bl_num);
                if ptype <> #0 then
                  db.SetPartType(pci, ptype);

                tmppartname := Trim(stringreplace(bl_part, ',', '', [rfReplaceAll, rfIgnoreCase]));
                if (PieceDesc(RebrickablePart(bl_part)) = '') or
                   (UpperCase(PieceDesc(RebrickablePart(bl_part))) = UpperCase(bl_part)) or
                   (UpperCase(PieceDesc(RebrickablePart(bl_part))) = UpperCase(tmppartname)) then
                begin
                  pi := PieceInfo(bl_part);
                  newpartname := Trim(smallparsepartname(bl_part, stmp1.Strings[0]));
                  if pi = nil then
                  begin
                    pi := TPieceInfo.Create;
                    pi.desc := newpartname;
                    pi.name := bl_part;
                    pi.lname := LowerCase(bl_part);
                    idx := fpieces.AddObject(bl_part, pi);
                    checkname := '';
                  end
                  else if pi = fstubpieceinfo then
                  begin
                    pi := TPieceInfo.Create;
                    pi.desc := newpartname;
                    pi.name := bl_part;
                    pi.lname := LowerCase(bl_part);
                    idx := fpieces.AddObject(bl_part, pi);
                    checkname := bl_part + ',' + tmppartname;
                  end
                  else
                  begin
                    checkname := bl_part + ',' + tmppartname;
                    pi.desc := newpartname;
                  end;
                  stmp2 := TStringList.Create;
                  if fexists(basedefault + 'db\db_pieces.extra.txt') then
                  begin
                    S_LoadFromFile(stmp2, basedefault + 'db\db_pieces.extra.txt');
                    RemoveBlancLines(stmp2);
                  end;
                  if checkname <> '' then
                  begin
                    idx2 := stmp2.IndexOf(checkname);
                    if idx2 < 0 then
                    begin
                      checkname := bl_part + ',' + bl_part;
                      idx2 := stmp2.IndexOf(checkname);
                      if idx2 < 0 then
                      begin
                        checkname := bl_part + ',' + '(Unknown)';
                        idx2 := stmp2.IndexOf(checkname);
                      end;
                    end;
                    if idx2 >= 0 then
                      stmp2.Delete(idx2);
                  end;
                  stmp2.Add(bl_part + ',' + pi.desc);
                  S_BackupFile(basedefault + 'db\db_pieces.extra.txt');
                  RemoveBlancLines(stmp2);
                  S_SaveToFile(stmp2, basedefault + 'db\db_pieces.extra.txt');
                  stmp2.Free;
                  RefreshPartCategory(bl_part);
                end;
              end;
            end;
          end;
        end;
      end;

    if extral.Count > 0 then
    begin
      AddKnownPieces(extral);
      if extral.Strings[0] <> 'Part,Color,Desc' then
        extral.Insert(0, 'Part,Color,Desc');
      S_SaveToFile(extral, basedefault + 'db\sets\' + s + '.alternatives.txt');
    end;
    extral.Free;
  finally
    slist.Free;
    stmp1.Free;
  end;
  DeleteFile(tmpname);

  newsetname := RemoveSpecialTagsFromString(newsetname);
  if sout.count > 1 then
  begin
    if newsetname = '' then
      newsetname := s;
    newsetname := stringreplace(newsetname, ',', '', [rfReplaceAll, rfIgnoreCase]);
    S_SaveToFile(sout, basedefault + 'db\sets\' + s + '.txt');
    fbinarysets.UpdateSetFromText(s, sout);

    sout.Free;
    Result := True;
    assets := DownloadSetAssetsFromBricklink(s, typ, SetYear(s));
    yyy := assets.year;
    sdesc := SetDesc(s);
    if (sdesc = '') or (sdesc = s) then
    begin
      UpdateSetInfoEx(s, newsetname, yyy, False, assets.hasinstructions, assets.hasoriginalbox);
{      stmp2 := TStringList.Create;
      if fexists(basedefault + 'db\db_sets.txt') then
      begin
        S_BackupFile(basedefault + 'db\db_sets.txt');
        stmp2.LoadFromFile(basedefault + 'db\db_sets.txt');
      end
      else
        stmp2.Add('set_id,descr,year');
      stmp2.Add(s + ',' + newsetname + ',0');
      stmp2.SaveToFile(basedefault + 'db\db_sets.txt');
      stmp2.Free;    }
    end
    else
      UpdateSetInfoEx(s, sdesc, yyy, False, assets.hasinstructions, assets.hasoriginalbox);
    if assets.hasinstructions then
      UpdateAssetWeightFromNET(s, 'I');
    if assets.hasoriginalbox then
      UpdateAssetWeightFromNET(s, 'B');
    if fcolors[-1].knownpieces.IndexOf(s) = -1 then
    begin
      pci := TPieceColorInfo.Create(s, -1);
      fcolors[-1].knownpieces.AddObject(s, pci);
    end;
   end
  else
  begin
    sout.Free;
    Result := False;   ////

    if Length(typ) = 1 then
    begin
      if typ[1] = 'P' then
        trysequence := 'P'
      else
        trysequence := typ[1] + 'P';
      if not NET_ExistsCache(s, 'PBCGM', yearfname) then
      begin
        yearurl := sbricklink + 'v2/catalog/catalogitem.page?' + typ + '=' + GetBLNetPieceName(Trim(s));
        case typ[1] of
          'S': yearfname := basedefault + 'db\setmolds\' + s + '.htm';
          'G': yearfname := basedefault + 'db\gears\' + s + '.htm';
          'M': yearfname := basedefault + 'db\minifigs\' + s + '.htm';
          'B': yearfname := basedefault + 'db\books\' + s + '.htm';
        else
          yearfname := basedefault + 'db\molds\' + s + '.htm';
        end;
        UrlDownloadToFile(nil, PChar(yearurl), PChar(yearfname), 0, nil);
      end;
      if fexists(yearfname) then
      begin
        yyy := GetSetYearFromDiskCache(yearfname);
        if (yyy >= 1932) and (yyy <= 2050) then
        begin
          pci := PieceColorInfo(s, -1);
          pci.canedityear := True;
          SaveItemYearInfo(pci.piece, pci.color, yyy);
          pci.year := yyy;
        end;
      end;
    end;
  end;
  RefreshSetYears(s);
  if Result then
  begin
    SetMoldName(s, SetDesc(s));
    AddMoldSet(s);
    RefreshPartCategory(s);

    SetSetIsGear(s, didG and (typ = 'G'));
    if typ = 'B' then
      SetSetIsBook(s);
    if Length(typ) = 1 then
      db.SetPartTypeIndirect(s, -1, typ[1]);
  end;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.DownloadSetAlternatesFromBricklinkNew(const s: string): boolean;
var
  tmpname: string;
  urlstr: string;
  slist: TStringList;
  tmp1: string;
  p1: Integer;
  typ: string;
  extral: TStringList;
begin
  tmpname := I_NewTempFile('tmpsetalt_' + Trim(s));
  try
    urlstr := sbricklink + 'catalogItemInv.asp?S=' + GetBLNetPieceName(Trim(s)) + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
    typ := 'S';
    Result := UrlDownloadToFile(nil, PChar(urlstr), PChar(tmpname), 0, nil) = 0;
    if not Result then
    begin
      urlstr := sbricklink + 'catalogItemInv.asp?M=' + GetBLNetPieceName(Trim(s)) + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
      typ := 'M';
      Result := UrlDownloadToFile(nil, PChar(urlstr), PChar(tmpname), 0, nil) = 0;
    end;
  except
    Result := False;
    Exit;
  end;
  if not Result then
  begin
    Exit;
  end;

  Result := False;

  slist := TStringList.Create;
  try
    S_LoadFromFile(slist, tmpname);
    tmp1 := slist.Text;
    p1 := Pos('SIZE="+0"><B>', tmp1);
    if p1 = 0 then
    begin
      urlstr := sbricklink + 'catalogItemInv.asp?M=' + GetBLNetPieceName(Trim(s)) + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
      typ := 'M';
      Result := UrlDownloadToFile(nil, PChar(urlstr), PChar(tmpname), 0, nil) = 0;
      if Result then
      begin
        S_LoadFromFile(slist, tmpname);
        tmp1 := slist.Text;
      end;
    end;
    p1 := Pos('SIZE="+0"><B>', tmp1);
    if p1 = 0 then
    begin
      urlstr := sbricklink + 'catalogItemInv.asp?G=' + GetBLNetPieceName(Trim(s)) + '&viewType=P&bt=0&sortBy=0&sortAsc=A';
      typ := 'G';
      Result := UrlDownloadToFile(nil, PChar(urlstr), PChar(tmpname), 0, nil) = 0;
      if Result then
      begin
        S_LoadFromFile(slist, tmpname);
        tmp1 := slist.Text;
      end;
    end;
    extral := ParseKnownPiecesFromHTML(tmp1);
    if extral.Count > 0 then
    begin
      AddKnownPieces(extral);
      if extral.Strings[0] <> 'Part,Color,Desc' then
        extral.Insert(0, 'Part,Color,Desc');
      S_SaveToFile(extral, basedefault + 'db\sets\' + s + '.alternatives.txt');
      Result := True;
    end;
    extral.Free;
  finally
    slist.Free;
  end;
  DeleteFile(tmpname);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.DownloadSetFromBricklink(const s: string; const typ: string = ''): boolean;
var
  i: integer;
  slist: TStringList;
  fname: string;
  sout: TStringList;
  stmp: TStringList;
  stmp2: TStringList;
  pci: TPieceColorInfo;
  idx: integer;
  pi: TPieceInfo;
  pcolor: colorinfo_p;
begin
  Result := False;
  fname := I_NewTempFile(s);
  if typ = '' then
  begin
    if not DownloadFile('http://' + BL_NET + '/catalogDownload.asp?itemType=S&viewType=4&itemTypeInv=S&itemNo=' + GetBLNetPieceName(s) + '&downloadType=T', fname) then
      Exit;
  end
  else
  begin
    if not DownloadFile('http://' + BL_NET + '/catalogDownload.asp?itemType=M&viewType=4&itemTypeInv=' + typ + '&itemNo=' + GetBLNetPieceName(s) + '&downloadType=T', fname) then
      Exit;
  end;
  if not fexists(fname) then
    Exit;
  slist := TStringList.Create;
  S_LoadFromFile(slist, fname);
  if slist.Count < 2 then
  begin
    slist.Free;
    if typ = '' then
      Result := DownloadSetFromBricklink(s, 'M');
    Exit;
  end;
  if Pos('Type', slist.Strings[0]) <> 1 then
  begin
    slist.Free;
    if typ = '' then
      Result := DownloadSetFromBricklink(s, 'M');
    Exit;
  end;
  sout := TStringList.Create;
  sout.Add('Part,Color,Num');

  for i := 1 to slist.Count - 1 do
  begin
    stmp := string2stringlist(slist.Strings[i], Chr(9));
    if stmp.Count > 8 then
      if stmp.Strings[5] = 'N' then
        if stmp.Strings[6] = 'N' then
          if stmp.Strings[8] = 'N' then
          begin
            sout.Add('BL ' + stmp.Strings[1] + ',' + 'BL ' + stmp.Strings[4] + ',' + stmp.Strings[3]);
            pci := PieceColorInfo(RebrickablePart(stmp.Strings[1]), BrickLinkColorToSystemColor(atoi(stmp.Strings[4])));
            if pci = nil then
            begin
              pci := TPieceColorInfo.Create(RebrickablePart(stmp.Strings[1]), BrickLinkColorToSystemColor(atoi(stmp.Strings[4])));
              pcolor := Colors(BrickLinkColorToSystemColor(atoi(stmp.Strings[4])));
              if pcolor.knownpieces = nil then
                pcolor.knownpieces := THashStringList.Create;
              pcolor.knownpieces.AddObject(RebrickablePart(stmp.Strings[1]), pci);
            end;
            stmp.Strings[1] := fixpartname(stmp.Strings[1]);
            if PieceDesc(RebrickablePart(stmp.Strings[1])) = '' then
            begin
              pi := TPieceInfo.Create;
              pi.desc := stringreplace(Trim(stmp.Strings[2]), ',', '', [rfReplaceAll, rfIgnoreCase]);
              pi.name := Trim(stmp.Strings[1]);
              pi.lname := LowerCase(pi.name);
              idx := fpieces.AddObject(pi.name, pi);
              stmp2 := TStringList.Create;
              if fexists(basedefault + 'db\db_pieces.extra.txt') then
              begin
                S_LoadFromFile(stmp2, basedefault + 'db\db_pieces.extra.txt');
                RemoveBlancLines(stmp2);
              end;
              stmp2.Add(fpieces.Strings[idx] + ',' + (fpieces.Objects[idx] as TPieceInfo).desc);
              S_BackupFile(basedefault + 'db\db_pieces.extra.txt');
              RemoveBlancLines(stmp2);
              S_SaveToFile(stmp2, basedefault + 'db\db_pieces.extra.txt');
              stmp2.Free;
            end;
          end;
    stmp.Free;
  end;
  S_SaveToFile(sout, basedefault + 'db\sets\' + s + '.txt');
  fbinarysets.UpdateSetFromText(s, sout);

  sout.Free;
  slist.Free;
  Result := True;
  if SetDesc(s) = '' then
  begin
    stmp2 := TStringList.Create;
    if fexists(basedefault + 'db\db_sets.txt') then
    begin
      S_BackupFile(basedefault + 'db\db_sets.txt');
      S_LoadFromFile(stmp2, basedefault + 'db\db_sets.txt');
    end
    else
      stmp2.Add('set_id,descr,year');
    stmp2.Add(s + ',' + s + ',0');
    S_SaveToFile(stmp2, basedefault + 'db\db_sets.txt');
    stmp2.Free;
  end;
  if fcolors[-1].knownpieces.IndexOf(s) = -1 then
    fcolors[-1].knownpieces.AddObject(s, TPieceColorInfo.Create(s, -1));

  RefreshSetYears(s);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdateSet(const s: string; const data: string = ''): boolean;
var
  i: integer;
  slist: TStringList;
  sout: TStringList;
  spiece, scolor, snum, scode, scost: string;
  color: integer;
  stmp2: TStringList;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  pcolor: colorinfo_p;
  docodes: boolean;
begin
  slist := TStringList.Create;
  slist.Text := data;
  if slist.Count < 2 then
  begin
    slist.Free;
    Result := False;
    Exit;
  end;

  sout := TStringList.Create;
  sout.Add('Part,Color,Num');

  docodes := Pos('Code,Num', slist.Strings[0]) = 1;

  for i := 1 to slist.Count - 1 do
  begin
    if docodes then
    begin
      splitstring(slist.Strings[i], scode, snum, scost, ',');
      if GetPieceColorFromCode(scode, spiece, color) then
      begin
        if spiece = '' then
          Continue;
        if spiece = '6141' then
          spiece := '4073'
        else if Pos('Mx', spiece) = 1 then
          spiece[1] := 'm';
        spiece := fixpartname(spiece);

        if Pos('BL ', spiece) = 1 then
          spiece := RebrickablePart(Copy(spiece, 4, Length(spiece) - 3))
        else
          spiece := RebrickablePart(spiece);

        scolor := itoa(color);
        sout.Add(spiece + ',' + scolor + ',' + snum);

      end
      else
        I_Warning('TSetsDatabase.UpdateSet(): Unknown part code %s'#13#10, [scode]);
    end
    else
    begin
      splitstring(slist.Strings[i], spiece, scolor, snum, scost, ',');
      if spiece = '' then
        Continue;
      if spiece = '6141' then
        spiece := '4073'
      else if Pos('Mx', spiece) = 1 then
        spiece[1] := 'm';
      spiece := fixpartname(spiece);

      sout.Add(spiece + ',' + scolor + ',' + snum);

      if Pos('BL ', spiece) = 1 then
        spiece := RebrickablePart(Copy(spiece, 4, Length(spiece) - 3))
      else
        spiece := RebrickablePart(spiece);

      if Pos('BL', scolor) = 1 then
      begin
        scolor := Trim(Copy(scolor, 3, Length(scolor) - 2));

        color := BrickLinkColorToSystemColor(StrToIntDef(scolor, 0))
      end
      else if Pos('RB', scolor) = 1 then
      begin
        scolor := Trim(Copy(scolor, 3, Length(scolor) - 2));

        color := RebrickableColorToSystemColor(StrToIntDef(scolor, 0))
      end
      else
        color := StrToIntDef(scolor, 0);
    end;

    pci := PieceColorInfo(spiece, color);
    if pci = nil then
    begin
      pci := TPieceColorInfo.Create(spiece, color);
      pcolor := Colors(color);
      if pcolor.knownpieces = nil then
        pcolor.knownpieces := THashStringList.Create;
      pcolor.knownpieces.AddObject(spiece, pci);
    end;
    if PieceDesc(spiece) = '' then
    begin
      pi := TPieceInfo.Create;
      pi.desc := spiece;
      pi.name := spiece;
      pi.lname := LowerCase(spiece);
      fpieces.AddObject(spiece, pi);
      pci.pieceinfo := pi;
      stmp2 := TStringList.Create;
      if fexists(basedefault + 'db\db_pieces.extra.txt') then
      begin
        S_LoadFromFile(stmp2, basedefault + 'db\db_pieces.extra.txt');
        RemoveBlancLines(stmp2);
      end;
      stmp2.Add(spiece + ',' + spiece);
      S_BackupFile(basedefault + 'db\db_pieces.extra.txt');
      RemoveBlancLines(stmp2);
      S_SaveToFile(stmp2, basedefault + 'db\db_pieces.extra.txt');
      stmp2.Free;
    end;
  end;
  S_SaveToFile(sout, basedefault + 'db\sets\' + s + '.txt');
  fbinarysets.UpdateSetFromText(s, sout);

  sout.Free;
  slist.Free;
  Result := True;
  if SetDesc(s) = '' then
  begin
    stmp2 := TStringList.Create;
    if fexists(basedefault + 'db\db_sets.txt') then
    begin
      S_BackupFile(basedefault + 'db\db_sets.txt');
      S_LoadFromFile(stmp2, basedefault + 'db\db_sets.txt');
    end
    else
      stmp2.Add('set_id,descr,year');
    stmp2.Add(s + ',' + s + ',0');
    S_SaveToFile(stmp2, basedefault + 'db\db_sets.txt');
    stmp2.Free;
  end;
  if fcolors[-1].knownpieces.IndexOf(s) = -1 then
    fcolors[-1].knownpieces.AddObject(s, TPieceColorInfo.Create(s, -1));
  RefreshSet(s, True);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdateSetInfo(const s: string; const desc: string; const year: integer; const ismoc: boolean): boolean;
var
  ss: TSetExtraInfo;
  i, idx: integer;
  t1, t2: string;
  s1, s2: TStringList;
  txt: string;
begin
  idx := fsets.IndexOf(s);
  if idx < 0 then
    idx := fsets.AddObject(s, TSetExtraInfo.Create);
  if idx >= 0 then
  begin
    ss := fsets.Objects[idx] as TSetExtraInfo;
    ss.Moc := ismoc;
    if year < 0 then
      ss.year := SetYear(s)
    else
      ss.year := year;
    txt := desc;
    for i := 1 to length(txt) do
      if txt[i] = ',' then
        txt[i] := ' ';
    ss.text := txt;
  end;

  t1 := basedefault + 'db\db_sets.txt';
  S_BackupFile(t1);
  s1 := TStringList.Create;
  s1. Add('set_id,descr,year');

  t2 := basedefault + 'db\db_mocs.txt';
  S_BackupFile(t2);
  s2 := TStringList.Create;
  s2.Add('set_id,descr,year');

  for i := 0 to fsets.Count - 1 do
  begin
    ss := fsets.Objects[i] as TSetExtraInfo;
    txt := fsets.Strings[i] + ',' + ss.text + ',' + itoa(ss.year);
    if ss.moc then
      s2.Add(txt)
    else
      s1.Add(txt);
  end;

  S_SaveToFile(s1, t1);
  S_SaveToFile(s2, t2);

  s1.Free;
  s2.Free;

  Result := True;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.UpdateSetInfoEx(const s: string; const desc: string; const year: integer; const ismoc: boolean; const hasI, hasB: boolean): boolean;
var
  ss: TSetExtraInfo;
  i, idx: integer;
  t1, t2: string;
  s1, s2: TStringList;
  txt: string;
  updateI, updateB: boolean;
  SA: TStringList;
  pci: TPieceColorInfo;
  k: integer;
  dosave: boolean;
begin
  idx := fsets.IndexOf(s);
  if idx < 0 then
    idx := fsets.AddObject(s, TSetExtraInfo.Create);
  if idx >= 0 then
  begin
    updateI := False;
    updateB := False;
    ss := fsets.Objects[idx] as TSetExtraInfo;
    ss.Moc := ismoc;
    if year < 0 then
      ss.year := SetYear(s)
    else
      ss.year := year;
    txt := desc;
    for i := 1 to length(txt) do
      if txt = ',' then
        txt := ' ';
    ss.text := txt;
    if hasI then
      if not ss.hasinstructions then
      begin
        ss.hasinstructions := True;
        updateI := True;
        pci := TPieceColorInfo.Create(s, INSTRUCTIONCOLORINDEX);
        pci.parttype := TYPE_INSTRUCTIONS;
        fcolors[INSTRUCTIONCOLORINDEX].knownpieces.AddObject(s, pci);
        SetItemYear(pci, ss.year);
        SetPartType(pci, 'I');
      end;

    if hasB then
      if not ss.hasoriginalbox then
      begin
        ss.hasoriginalbox := True;
        updateB := True;
        pci := TPieceColorInfo.Create(s, BOXCOLORINDEX);
        pci.parttype := TYPE_BOX;
        fcolors[BOXCOLORINDEX].knownpieces.AddObject(s, pci);
        SetItemYear(pci, ss.year);
        SetPartType(pci, 'O');
      end;

    if updateI or updateB then
    begin
      SA := TStringList.Create;
      try
        dosave := False;
        if fexists(basedefault + 'db\db_set_assets.txt') then
          S_LoadFromFile(SA, basedefault + 'db\db_set_assets.txt');
        if SA.Count = 0 then
        begin
          SA.Add('Set,Asset,Weight');
          dosave := True;
        end;
        if SA.Strings[0] = 'Set,Asset' then
        begin
          if updateI then
          begin
            k := SA.IndexOf(s + ',I');
            if k < 0 then
            begin
              SA.Add(s + ',I');
              dosave := True;
            end;
          end;
          if updateB then
          begin
            k := SA.IndexOf(s + ',B');
            if k < 0 then
            begin
              SA.Add(s + ',B');
              dosave := True;
            end;
          end;
        end
        else if SA.Strings[0] = 'Set,Asset,Weight' then
        begin
          if updateI then
          begin
            k := IndexOfStart(SA, s + ',I');
            if k < 0 then
            begin
              SA.Add(s + ',I,' + Format('%2.4f', [GetAssetWeight(s, 'I')]));
              dosave := True;
            end;
          end;
          if updateB then
          begin
            k := IndexOfStart(SA, s + ',B');
            if k < 0 then
            begin
              SA.Add(s + ',B,' + Format('%2.4f', [GetAssetWeight(s, 'B')]));
              dosave := True;
            end;
          end;
        end;
        if dosave then
        begin
          S_BackupFile(basedefault + 'db\db_set_assets.txt');
          S_SaveToFile(SA, basedefault + 'db\db_set_assets.txt');
        end;
      finally
        SA.Free;
      end;
    end;
  end;
  t1 := basedefault + 'db\db_sets.txt';
  S_BackupFile(t1);
  s1 := TStringList.Create;
  s1.Add('set_id,descr,year');

  t2 := basedefault + 'db\db_mocs.txt';
  S_BackupFile(t2);
  s2 := TStringList.Create;
  s2.Add('set_id,descr,year');

  for i := 0 to fsets.Count - 1 do
  begin
    ss := fsets.Objects[i] as TSetExtraInfo;
    txt := fsets.Strings[i] + ',' + ss.text + ',' + itoa(ss.year);
    if ss.moc then
      s2. Add(txt)
    else
      s1.Add(txt);
  end;

  S_SaveToFile(s1, t1);
  S_SaveToFile(s2, t2);

  s1.Free;
  s2.Free;

  Result := True;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
procedure TSetsDatabase.SetSetIsBook(const sid: string);
var
  lst: TStringList;
  fname: string;
  desc: string;
  i: integer;
begin
  if IsBook(sid) then
    Exit;
  fallbooks.Add(sid);

  lst := TStringList.Create;
  fname := basedefault + 'db\db_books.txt';
  if fexists(fname) then
    S_LoadFromFile(lst, fname);
  if lst.Count = 0 then
    lst.Add('Number,Name,Year');
  if lst.Strings[0] = 'Number,Name,Year' then
  begin
    desc := SetDesc(sid);
    for i := 1 to length(desc) do
      if desc[i] =',' then
        desc[i] := ' ';
    lst.Add(sid + ',' + desc + ',' + itoa(SetYear(sid)));
    S_BackupFile(fname);
    S_SaveToFile(lst, fname);
  end;
  lst.Free;
end;
{$ENDIF}

function TSetsDatabase.IsBook(const s: string): boolean;
begin
  Result := fallbooks.IndexOfUCS(s) >= 0;
end;

function TSetsDatabase.GetPieceColorFromCode(const code: string; var spiece: string; var scolor: string): boolean;
var
  icolor: integer;
begin
  result := GetPieceColorFromCode(code, spiece, icolor);
  if result then
    scolor := itoa(icolor)
  else
    scolor := '';
end;

function TSetsDatabase.GetPieceColorFromCode(const code: string; var spiece: string; var scolor: integer): boolean;
var
  idx: integer;
  cpi: TCodePieceInfo;
begin
  idx := fpiececodes.IndexOf(Trim(code));
  if idx < 0 then
  begin
    Result := False;
    Exit;
  end;
  cpi := fpiececodes.Objects[idx] as TCodePieceInfo;
  spiece := cpi.piece;
  scolor := cpi.color;
  Result := True;
end;

function TSetsDatabase.GetCodeFromPieceColor(const spiece: string; const scolor: integer): string;
var
  pci: TPieceColorInfo;
begin
  pci := PieceColorInfo(spiece, scolor);
  if pci <> nil then
    Result := pci.code
  else
    Result := '';
end;

{$IFNDEF CRAWLER}
function TSetsDatabase.GetColorIdFromName(const cs: string; var cc: integer): boolean;
var
  check, foo: string;
  i: integer;
begin
  check := UpperCase(Trim(cs));
  if check = '(NOT APPLICABLE)' then
  begin
    Result := True;
    cc := -1;
    Exit;
  end;
  for i := -1 to MAXINFOCOLOR do
    if fcolors[i].id = i then
      if UpperCase(Trim(fcolors[i].name)) = check then
      begin
        Result := True;
        cc := i;
        Exit;
      end;

  splitstring(cs, check, foo, '(');
  check := UpperCase(Trim(check));
  for i := -1 to MAXINFOCOLOR do
    if fcolors[i].id = i then
      if UpperCase(Trim(fcolors[i].name)) = check then
      begin
        Result := True;
        cc := i;
        Exit;
      end;

  Result := False;
end;
{$ENDIF}

function TSetsDatabase.UpdatePartWeight(const pi: TPieceInfo; const w: double): boolean;
var
  sl: TStringList;
  i: integer;
  check, sweight: string;
begin
  Result := False;

  if pi = nil then
    Exit;

  if pi = fstubpieceinfo then
    Exit;

  if pi.weight = w then
    Exit;

  Result := True;
  pi.weight := w;

  sl := TStringList.Create;
  if fexists(basedefault + 'db\db_pieces_weight.txt') then
    S_LoadFromFile(sl, basedefault + 'db\db_pieces_weight.txt')
  else
    sl.Add('Part,Weight');
  if sl.Count < 1 then
  begin
    sl.Free;
    Exit;
  end;
  if sl.Strings[0] <> 'Part,Weight' then
  begin
    sl.Free;
    Exit;
  end;

  for i := 1 to sl.Count - 1 do
  begin
    splitstring(sl.Strings[i], check, sweight, ',');
    if check = pi.name then
    begin
      sl.Strings[i] := pi.name + ',' + Format('%2.4f', [w]);
      S_BackupFile(basedefault + 'db\db_pieces_weight.txt');
      S_SaveToFile(sl, basedefault + 'db\db_pieces_weight.txt');
      sl.Free;
      Exit;
    end;
  end;

  sl.Add(pi.name + ',' + Format('%2.4f', [w]));
  S_BackupFile(basedefault + 'db\db_pieces_weight.txt');
  S_SaveToFile(sl, basedefault + 'db\db_pieces_weight.txt');
  sl.Free;
end;

function TSetsDatabase.UpdatePartWeight(const pcs: string; const w: double): boolean;
var
  sl: TStringList;
  idx, i: integer;
  check, spart, sweight: string;
begin
  Result := False;
  if w <= 0 then
    Exit;

  check := Trim(pcs);
  spart := check;
  idx := fpieceshash.GetPos(spart);
  if idx < 0 then
  begin
    spart := RebrickablePart(check);
    idx := fpieceshash.GetPos(spart);
    if idx < 0 then
    begin
      spart := BricklinkPart(check);
      idx := fpieceshash.GetPos(spart);
      if idx < 0 then
      begin
        if Pos('BL ', check) = 1 then
        begin
          spart := RebrickablePart(Copy(check, 4, Length(check) - 3));
          idx := fpieceshash.GetPos(spart);
        end;
      end;
    end;
  end;
  if idx < 0 then
    Exit;

  if (fpieces.Objects[idx] as TPieceInfo).weight = w then
    Exit;

  Result := True;
  (fpieces.Objects[idx] as TPieceInfo).weight := w;

  sl := TStringList.Create;
  if fexists(basedefault + 'db\db_pieces_weight.txt') then
    S_LoadFromFile(sl, basedefault + 'db\db_pieces_weight.txt')
  else
    sl.Add('Part,Weight');
  if sl.Count < 1 then
  begin
    sl.Free;
    Exit;
  end;
  if sl.Strings[0] <> 'Part,Weight' then
  begin
    sl.Free;
    Exit;
  end;

  for i := 1 to sl.Count - 1 do
  begin
    splitstring(sl.Strings[i], check, sweight, ',');
    if check = spart then
    begin
      sl.Strings[i] := spart + ',' + Format('%2.4f', [w]);
      S_BackupFile(basedefault + 'db\db_pieces_weight.txt');
      S_SaveToFile(sl, basedefault + 'db\db_pieces_weight.txt');
      sl.Free;
      Exit;
    end;
  end;

  sl.Add(spart + ',' + Format('%2.4f', [w]));
  S_BackupFile(basedefault + 'db\db_pieces_weight.txt');
  S_SaveToFile(sl, basedefault + 'db\db_pieces_weight.txt');
  sl.Free;
end;

function TSetsDatabase.GetPartWeight(const pcs: string): double;
var
  pi: TPieceInfo;
begin
  Result := 0.0;
  pi := PieceInfo(pcs);
  if pi <> nil then
    if pi <> fstubpieceinfo then
      Result := pi.weight;
end;

function TSetsDatabase.PieceInfo(const piece: string): TPieceInfo;
var
  idx: integer;
  check: string;
  i: integer;
  alias: TStringList;
begin
  check := fixpartname(piece);
  idx := fpieceshash.GetPos(check);
  if idx = -1 then
  begin
    idx := fpieceshash.GetPos(RebrickablePart(piece));
    if idx = -1 then
    begin
      idx := fpieceshash.GetPos(BricklinkPart(piece));
      if idx = -1 then
      begin
        alias := SearchGlobalPieceAlias(piece);
        if alias <> nil then
        begin
          for i := 0 to alias.Count - 1 do
          begin
            idx := fpieces.IndexOf(alias.Strings[i]);
            if idx > 0 then
              break;
          end;
        end;
      end;
{      if idx = -1 then
      begin
        check := LowerCase(check);
        for i := 0 to fpieces.Count - 1 do
        begin
          pi := fpieces.Objects[i] as TPieceInfo;
          if check = pi.lname then
          begin
            idx := i;
            break;
          end;
        end;
      end;}
      if idx = -1 then
      begin
        Result := fstubpieceinfo;
        Exit;
      end;
    end;
  end;
  Result := (fpieces.Objects[idx] as TPieceInfo);
end;

function TSetsDatabase.PieceInfo(const pci: TPieceColorInfo): TPieceInfo;
begin
  if pci = nil then
  begin
    Result := fstubpieceinfo;
    Exit;
  end;

  if pci.pieceinfo <> nil then
    if pci.pieceinfo is TPieceInfo then
    begin
      Result := pci.pieceinfo as TPieceInfo;
      if Result <> fstubpieceinfo then
        Exit;
    end;

  Result := PieceInfo(pci.piece);
  pci.pieceinfo := Result;
end;

function TSetsDatabase.PieceInfo(const brick: brickpool_p): TPieceInfo;
begin
  Result := PieceInfo(PieceColorInfo(brick));
end;

function TSetsDatabase.IsValidPieceInfo(const pi: TPieceInfo): boolean;
begin
  if (pi = nil) or (pi = fstubpieceinfo) then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

function TSetsDatabase.PieceColorInfo(const brick: brickpool_p): TPieceColorInfo;
var
  check1, check2: string;
begin
  if brick.pci <> nil then
    if brick.pci is TPieceColorInfo then
    begin
      Result := brick.pci as TPieceColorInfo;
      if Result.color = brick.color then
      begin
        if Result.piece = brick.part then
          Exit;
        if Result.piece = RebrickablePart(brick.part) then
          Exit;
        check1 := UpperCase(Result.piece);
        check2 := UpperCase(brick.part);
        if check1 = check2 then
          Exit;
      end;
    end;

  Result := PieceColorInfo(brick.part, brick.color);
  brick.pci := Result;
end;

function TSetsDatabase.PieceColorInfo(const piece: string; const color: integer;
  const suspect: TObject = nil): TPieceColorInfo;
var
  idx: integer;
  check1, check2: string;

  function _checkalias: TPieceColorInfo;
  var
    alias: TStringList;
    i: integer;
  begin
    alias := SearchGlobalPieceAlias(piece);
    if alias <> nil then
    begin
      for i := 0 to alias.Count - 1 do
      begin
        idx := fcolors[color].knownpieces.IndexOf(piece);
        if idx >= 0 then
        begin
          Result := fcolors[color].knownpieces.Objects[idx] as TPieceColorInfo;
          Exit;
        end;
      end;
    end;
    Result := nil;
  end;

  function _checkset: TPieceColorInfo;
  begin
    if Pos('-', piece) > 1 then
      if color <> -1 then
        if color <> INSTRUCTIONCOLORINDEX then
          if color <> BOXCOLORINDEX then
          begin
            Result := PieceColorInfo(piece, -1);
            Exit;
          end;
    Result := nil;
  end;

begin
  if suspect <> nil then
    if suspect is TPieceColorInfo then
    begin
      Result := suspect as TPieceColorInfo;
      if Result.color = color then
      begin
        if Result.piece = piece then
          Exit;
        if Result.piece = RebrickablePart(piece) then
          Exit;
        check1 := UpperCase(Result.piece);
        check2 := UpperCase(piece);
        if check1 = check2 then
          Exit;
      end;
    end;

  if (color < -1) or (color > MAXINFOCOLOR) then
  begin
    Result := _checkset;
    Exit;
  end;

  if fcolors[color].knownpieces = nil then
  begin
    Result := _checkset;
    Exit;
  end;

  idx := fcolors[color].knownpieces.IndexOf(piece);
  if idx < 0 then
  begin
    idx := fcolors[color].knownpieces.IndexOf(RebrickablePart(piece));
    if idx < 0 then
    begin
      idx := fcolors[color].knownpieces.IndexOfUCS(piece);
      if idx < 0 then
        idx := fcolors[color].knownpieces.IndexOfUCS(RebrickablePart(piece));
    end;

    if idx >= 0 then
      Result := fcolors[color].knownpieces.Objects[idx] as TPieceColorInfo
    else
      Result := _checkalias;
    if Result = nil then
      Result := _checkset;
    Exit;
  end;
  Result := fcolors[color].knownpieces.Objects[idx] as TPieceColorInfo;
end;

function TSetsDatabase.Priceguide(const brick: brickpool_p): priceguide_t;
var
  pci: TPieceColorInfo;
begin
  pci := PieceColorInfo(brick);
  if pci = nil then
  begin
    FillChar(Result, SizeOf(Result), 0);
    Exit;
  end;

  if not pci.hasloaded then
    pci.Load;

  Result := pci.priceguide;
end;

function TSetsDatabase.Priceguide(const piece: string; const color: integer = -1): priceguide_t;
var
  pci: TPieceColorInfo;
begin
  pci := PieceColorInfo(piece, color);
  if pci = nil then
  begin
    FillChar(Result, SizeOf(Result), 0);
    Exit;
  end;

  if not pci.hasloaded then
    pci.Load;

  Result := pci.priceguide;
end;

function TSetsDatabase.Availability(const brick: brickpool_p): availability_t;
var
  pci: TPieceColorInfo;
begin
  pci := PieceColorInfo(brick);
  if pci = nil then
  begin
    FillChar(Result, SizeOf(Result), 0);
    Exit;
  end;

  if not pci.hasloaded then
    pci.Load;

  Result := pci.availability;
end;

function TSetsDatabase.Availability(const piece: string; const color: integer = -1): availability_t;
var
  pci: TPieceColorInfo;
begin
  pci := PieceColorInfo(piece, color);
  if pci = nil then
  begin
    FillChar(Result, SizeOf(Result), 0);
    Exit;
  end;

  if not pci.hasloaded then
    pci.Load;

  Result := pci.availability;
end;

{$IFNDEF CRAWLER}
function TSetsDatabase.PArecAt(const piece: string; const color: integer; const at: TDateTime): parecdate_t;
var
  A: parecarray_p;
  Q: qtydatearray_p;
  P: pricedatearray_p;
  Asize: integer;
  x: integer;

  procedure _fillQ(const offs: integer);
  var
    i: integer;
    pi: PInteger;
  begin
    for i := 0 to Asize - 1 do
    begin
      pi := PInteger(integer(@A[i]) + offs);
      Q[i].qty := pi^;
    end;
  end;

  procedure _fillP(const offs: integer);
  var
    i: integer;
    pi: PDouble;
  begin
    for i := 0 to Asize - 1 do
    begin
      pi := PDouble(integer(@A[i]) + offs);
      P[i].price := pi^;
    end;
  end;

begin
  Result.date := at;

  PG_MakeHistoryParecArray(piece, color, A, Asize);
  Q := malloc(Asize * SizeOf(qtydate_t));
  P := malloc(Asize * SizeOf(pricedate_t));

  for x := 0 to Asize - 1 do
  begin
    Q[x].date := A[x].date;
    P[x].date := A[x].date;
  end;

  _fillQ(integer(@A[0].priceguide.nTimesSold) - integer(@A[0]));
  Result.priceguide.nTimesSold := PG_QtyAt(Q, Asize, at);
  _fillQ(integer(@A[0].priceguide.nTotalQty) - integer(@A[0]));
  Result.priceguide.nTotalQty := PG_QtyAt(Q, Asize, at);
  _fillP(integer(@A[0].priceguide.nMinPrice) - integer(@A[0]));
  Result.priceguide.nMinPrice := PG_PriceAt(P, Asize, at);
  _fillP(integer(@A[0].priceguide.nAvgPrice) - integer(@A[0]));
  Result.priceguide.nAvgPrice := PG_PriceAt(P, Asize, at);
  _fillP(integer(@A[0].priceguide.nQtyAvgPrice) - integer(@A[0]));
  Result.priceguide.nQtyAvgPrice := PG_PriceAt(P, Asize, at);
  _fillP(integer(@A[0].priceguide.nMaxPrice) - integer(@A[0]));
  Result.priceguide.nMaxPrice := PG_PriceAt(P, Asize, at);

  _fillQ(integer(@A[0].priceguide.uTimesSold) - integer(@A[0]));
  Result.priceguide.uTimesSold := PG_QtyAt(Q, Asize, at);
  _fillQ(integer(@A[0].priceguide.uTotalQty) - integer(@A[0]));
  Result.priceguide.uTotalQty := PG_QtyAt(Q, Asize, at);
  _fillP(integer(@A[0].priceguide.uMinPrice) - integer(@A[0]));
  Result.priceguide.uMinPrice := PG_PriceAt(P, Asize, at);
  _fillP(integer(@A[0].priceguide.uAvgPrice) - integer(@A[0]));
  Result.priceguide.uAvgPrice := PG_PriceAt(P, Asize, at);
  _fillP(integer(@A[0].priceguide.uQtyAvgPrice) - integer(@A[0]));
  Result.priceguide.uQtyAvgPrice := PG_PriceAt(P, Asize, at);
  _fillP(integer(@A[0].priceguide.uMaxPrice) - integer(@A[0]));
  Result.priceguide.uMaxPrice := PG_PriceAt(P, Asize, at);

  _fillQ(integer(@A[0].availability.nTotalLots) - integer(@A[0]));
  Result.availability.nTotalLots := PG_QtyAt(Q, Asize, at);
  _fillQ(integer(@A[0].availability.nTotalQty) - integer(@A[0]));
  Result.availability.nTotalQty := PG_QtyAt(Q, Asize, at);
  _fillP(integer(@A[0].availability.nMinPrice) - integer(@A[0]));
  Result.availability.nMinPrice := PG_PriceAt(P, Asize, at);
  _fillP(integer(@A[0].availability.nAvgPrice) - integer(@A[0]));
  Result.availability.nAvgPrice := PG_PriceAt(P, Asize, at);
  _fillP(integer(@A[0].availability.nQtyAvgPrice) - integer(@A[0]));
  Result.availability.nQtyAvgPrice := PG_PriceAt(P, Asize, at);
  _fillP(integer(@A[0].availability.nMaxPrice) - integer(@A[0]));
  Result.availability.nMaxPrice := PG_PriceAt(P, Asize, at);

  _fillQ(integer(@A[0].availability.uTotalLots) - integer(@A[0]));
  Result.availability.uTotalLots := PG_QtyAt(Q, Asize, at);
  _fillQ(integer(@A[0].availability.uTotalQty) - integer(@A[0]));
  Result.availability.uTotalQty := PG_QtyAt(Q, Asize, at);
  _fillP(integer(@A[0].availability.uMinPrice) - integer(@A[0]));
  Result.availability.uMinPrice := PG_PriceAt(P, Asize, at);
  _fillP(integer(@A[0].availability.uAvgPrice) - integer(@A[0]));
  Result.availability.uAvgPrice := PG_PriceAt(P, Asize, at);
  _fillP(integer(@A[0].availability.uQtyAvgPrice) - integer(@A[0]));
  Result.availability.uQtyAvgPrice := PG_PriceAt(P, Asize, at);
  _fillP(integer(@A[0].availability.uMaxPrice) - integer(@A[0]));
  Result.availability.uMaxPrice := PG_PriceAt(P, Asize, at);

  memfree(pointer(A), Asize * SizeOf(parecdate_t));
  memfree(pointer(Q), Asize * SizeOf(qtydate_t));
  memfree(pointer(P), Asize * SizeOf(pricedate_t));
end;

function TSetsDatabase.GetPartLastUpdateDate(const piece: string; const color: integer): TDateTime;
var
  A: parecarray_p;
  Asize: integer;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Asize);
  if Asize > 0 then
    Result := A[Asize - 1].date
  else if not TryEncodeDateTime(2014, 1, 1, 0, 0, 0, 0, Result) then
    Result := Now;
end;
{$ENDIF}

function TSetsDatabase.ConvertCurrency(const cur: string): double;
begin
  Result := fcurrencies.Convert(cur);
end;

function TSetsDatabase.ConvertCurrencyAt(const cur: string; const dd: TDateTime): double;
begin
  Result := fcurrencyconvert.ConvertAt(cur, dd);
end;

function TSetsDatabase.RecentCrawlerPart(const cpiece: string): boolean;
var
  idx: integer;
begin
  Result := True;
  idx := fCrawlerCache.IndexOf(cpiece);
  if idx < 0 then
  begin
    fCrawlerCache.Add(cpiece);
    Result := False;
  end
  else if fCrawlerCache.Count > 500 then
    fCrawlerCache.Delete(0);
end;

procedure TSetsDatabase.CrawlerPriorityPart(const piece: string; const color: integer = -1);
var
  cnt, i: integer;
  s: string;
  sl: TStringList;
  dn: integer;
  idx: integer;
begin
  if not floaded then
    Exit;
  if not AllowInternetAccess then
    Exit;
  cnt := fcrawlerpriority.Count;
  if cnt > 250000 then
  begin
    sl := TStringList.Create;
    for i := 1000 to cnt - 1 do
      sl.Add(fcrawlerpriority.Strings[i]);
    fcrawlerpriority.Clear;
    fcrawlerpriority.AddStrings(sl);
    sl.Free;
  end;
  s := IntToStr(color) + ',' + piece;
  if not RecentCrawlerPart(s) then
  begin
    idx := fcrawlerpriority.IndexOf(s);
    if idx >= 0 then
    begin
      fcrawlerpriority.Delete(idx);
      idx := fcrawlerpriority.IndexOf(s);
      if idx >= 0 then
      begin
        fcrawlerpriority.Delete(idx);
        idx := fcrawlerpriority.IndexOf(s);
        if idx >= 0 then
          fcrawlerpriority.Delete(idx);
      end;
    end;
  end;
  cnt := fcrawlerpriority.Count;
  dn := cnt - 1000;
  if dn < 0 then
    dn := 0;
  for i := cnt - 1 downto dn do
    if fcrawlerpriority.Strings[i] = s then
      Exit;
  fcrawlerpriority.Add(s);
  TmpSaveCrawler;
end;

procedure TSetsDatabase.CrawlerPriorityPartColor(const cpiece: string);
var
  cnt, i: integer;
  s: string;
  sl: TStringList;
  dn: integer;
  idx: integer;
begin
  if not floaded then
    Exit;
  if not AllowInternetAccess then
    Exit;
  cnt := fcrawlerpriority.Count;
  if cnt > 250000 then
  begin
    sl := TStringList.Create;
    for i := 1000 to cnt - 1 do
      sl.Add(fcrawlerpriority.Strings[i]);
    fcrawlerpriority.Clear;
    fcrawlerpriority.AddStrings(sl);
    sl.Free;
  end;
  if not RecentCrawlerPart(s) then
  begin
    idx := fcrawlerpriority.IndexOf(cpiece);
    if idx >= 0 then
    begin
      fcrawlerpriority.Delete(idx);
      idx := fcrawlerpriority.IndexOf(cpiece);
      if idx >= 0 then
      begin
        fcrawlerpriority.Delete(idx);
        idx := fcrawlerpriority.IndexOf(cpiece);
        if idx >= 0 then
          fcrawlerpriority.Delete(idx);
      end;
    end;
  end;
  cnt := fcrawlerpriority.Count;
  dn := cnt - 1000;
  s := cpiece;
  if dn < 0 then
    dn := 0;
  for i := cnt - 1 downto dn do
    if fcrawlerpriority.Strings[i] = s then
      Exit;
  fcrawlerpriority.Add(s);
  TmpSaveCrawler;
end;

procedure TSetsDatabase.TmpSaveCrawler;
begin
  if fcrawlerpriority.Count mod 10 = 9 then
  try
    S_BackupFile(basedefault + 'cache\' + fcrawlerfilename);
    S_SaveToFile(fcrawlerpriority, basedefault + 'cache\' + fcrawlerfilename);
  except
    I_Warning('fcrawlerpriority.SaveToFile(): Can not save tmp file'#13#10);
  end;
end;

procedure TSetsDatabase.Crawler(const rlevel: integer = 0);
var
  spart, scolor: string;
  idx: integer;
  pci: TPieceColorInfo;
  pi: TPieceInfo;
  s: string;
  inv: TBrickInventory;
  ltmp, lsets1, lsets2, lparts1, lparts2: TStringList;
  oldname, newname: string;
  i: integer;
  stmp: string;
  codeok: boolean;
begin
  if not floaded then
    Exit;

  if rlevel > 100 then
    Exit;

//  fpciloader.CheckJobDone;

  if fcrawlerpriority.Count = 0 then
  begin
    ltmp := TStringList.Create;
    lsets1 := TStringList.Create;
    lsets2 := TStringList.Create;
    lparts1 := TStringList.Create;
    lparts2 := TStringList.Create;
    try
      ltmp.AddStrings(fcolorpieces);
      for i := 0 to ltmp.Count - 1 do
        if Pos('-1,', ltmp.Strings[i]) <> 1 then
        begin
          oldname := secondword(ltmp.strings[i], ',');
          newname := GetNewPieceName(oldname);
          if (newname = oldname) or (newname = '') then
            lparts1.Add(ltmp.Strings[i])
          else
            lparts2.Add(ltmp.Strings[i]);
        end;
      fcrawlerpriority.AddStrings(lparts1);
      fcrawlerpriority.AddStrings(lparts2);
      for i := 0 to ltmp.Count - 1 do
      begin
        stmp := ltmp.Strings[i];
        if Pos('-1,', stmp) = 1 then
        begin
          if (Pos('-1,sw', stmp) = 1) and (CountNumbers(stmp) = 4) then
            lsets1.Add(stmp)
          else if (Pos('-1,cty', stmp) = 1) and (CountNumbers(stmp) = 4) then
            lsets1.Add(stmp)
          else
            lsets2.Add(stmp);
        end;
      end;
      fcrawlerpriority.AddStrings(lsets1);
      fcrawlerpriority.AddStrings(lsets2);
    finally
      ltmp.Free;
      lsets1.Free;
      lsets2.Free;
      lparts1.Free;
      lparts2.Free;
    end;
    fcrawlerhistory.Clear;
{    for i := 0 to fallbooks.Count - 1 do
      fcrawlerpriority.Add('-1,' + fallbooks.Strings[i]);}
    if fcrawlerpriority.Count = 0 then
      Exit;
  end;

  TmpSaveCrawler;

  if fcrawlerrandom = 0 then
  begin
    idx := fcrawlerpriority.Count;
    if idx = 0 then
      Exit;

    Dec(idx);
  end
  else
  begin
    if fcrawlerrandom > 100 then
      fcrawlerrandom := 100;
//    idx := fcrawlerpriority.Count - round((fcrawlerpriority.Count * Random(fcrawlerrandom)) / 100);
    idx := fcrawlerpriority.Count - Random(round(fcrawlerpriority.Count / 100 * fcrawlerrandom));
    if idx >= fcrawlerpriority.Count then
      idx := fcrawlerpriority.Count - 1
    else if idx < 0 then
      idx := 0;
  end;
  
  s := fcrawlerpriority.Strings[idx];
  flastcrawlpiece := s;
  codeok := true;
  if Pos(',', s) < 1 then
    codeok := GetPieceColorFromCode(s, spart, scolor)
  else
    splitstring(s, scolor, spart, ',');
  fcrawlerpriority.Delete(idx);

  scolor := Trim(scolor);
  spart := Trim(spart);

  idx := fcrawlerpriority.IndexOf(s);
  if idx > -1 then
  begin
    fcrawlerpriority.Delete(idx);
    idx := fcrawlerpriority.IndexOf(s);
    if idx > -1 then
    begin
      fcrawlerpriority.Delete(idx);
      idx := fcrawlerpriority.IndexOf(s);
      if idx > -1 then
        fcrawlerpriority.Delete(idx);
    end;
  end;

  if fcrawlerhistory.IndexOf(s) > 0 then
  begin
    Crawler(rlevel + 1);
    Exit;
  end;

  fcrawlerhistory.Add(s);
  if fcrawlerhistory.Count > 20000 then
    fcrawlerhistory.Delete(0);

  if not codeok then
  begin
    Crawler(rlevel + 1);
    Exit;
  end;

  pci := PieceColorInfo(spart, StrToIntDef(scolor, -1));
  if pci = nil then
    Exit;

  pci.InternetUpdate;

  {$IFNDEF CRAWLER}
  if scolor = '-1' then
    if inventory <> nil then
      inventory.StorePieceInventoryStatsRec(basedefault + 'out\' + spart + '\' + spart + '.history', Trim(spart), -1);
  {$ENDIF}

  if (scolor = '89') or (scolor = '') or ((scolor = '-1') and (Pos('-', spart) > 0)) then // set
  begin
    inv := GetSetInventory(spart);
    if inv <> nil then
    begin
      if not DirectoryExists(basedefault + 'out\') then
        ForceDirectories(basedefault + 'out\');
      if not DirectoryExists(basedefault + 'out\' + spart + '\') then
        ForceDirectories(basedefault + 'out\' + spart + '\');
      inv.StoreHistoryStatsRec(basedefault + 'out\' + spart + '\' + spart + '.stats');
      inv.StoreHistoryEvalRec(basedefault + 'out\' + spart + '\' + spart + '.ieval');
    end;
  end
  else if scolor = '-1' then
  begin
    inv := GetSetInventory(spart);
    if inv <> nil then
    begin
      if not DirectoryExists(basedefault + 'out\') then
        ForceDirectories(basedefault + 'out\');
      if not DirectoryExists(basedefault + 'out\' + spart + '\') then
        ForceDirectories(basedefault + 'out\' + spart + '\');
      inv.StoreHistoryStatsRec(basedefault + 'out\' + spart + '\' + spart + '.stats');
      inv.StoreHistoryEvalRec(basedefault + 'out\' + spart + '\' + spart + '.ieval');
    end
    else
    begin
      inv := PieceInfo(spart).Inventory(-1);
      if inv <> nil then
      begin
        if inv.numlooseparts > 0 then
        begin
          if not DirectoryExists(basedefault + 'cache') then
            MkDir(basedefault + 'cache\');
          if not DirectoryExists(basedefault + 'cache\' + scolor) then
            MkDir(basedefault + 'cache\' + scolor);
          inv.StoreHistoryStatsRec(basedefault + 'cache\' + scolor + '\' + spart + '.stats');
          inv.StoreHistoryEvalRec(basedefault + 'cache\' + scolor + '\' + spart + '.ieval');
        end;
        inv.Free;
      end;
    end;
  end
  else if (scolor <> '9996') and (scolor <> '9997') and (scolor <> '9998') and (scolor <> '9999') then
  begin
    pi := PieceInfo(spart);
    inv := pi.Inventory(atoi(scolor));
    if inv <> nil then
    begin
      if inv.numlooseparts > 0 then
      begin
        if not DirectoryExists(basedefault + 'cache') then
          MkDir(basedefault + 'cache\');
        if not DirectoryExists(basedefault + 'cache\' + scolor) then
          MkDir(basedefault + 'cache\' + scolor);
        inv.StoreHistoryStatsRec(basedefault + 'cache\' + scolor + '\' + spart + '.stats');
        inv.StoreHistoryEvalRec(basedefault + 'cache\' + scolor + '\' + spart + '.ieval');
      end;
      inv.Free;
    end;
  end;
end;

procedure TSetsDatabase.ExportPriceGuide(const fname: string);
var
  s: TStringList;
  i, j: integer;
  k, tot: integer;
  pregressstring: string;
  kp: THashStringList;
begin
  AllowInternetAccess := False;

  pregressstring := 'Export price guide...';
  if Assigned(progressfunc) then
    progressfunc(pregressstring, 0.0);

  printf('TSetsDatabase.ExportPriceGuide(' + fname + ')'#13#10);

  s := TStringList.Create;
  s.Add('Part;Color;BLPart;BLColor;pg_nTimesSold;pg_nTotalQty;pg_nMinPrice;pg_nAvgPrice;' +
        'pg_nQtyAvgPrice;pg_nMaxPrice;pg_uTimesSold;pg_uTotalQty;pg_uMinPrice;' +
        'pg_uAvgPrice;pg_uQtyAvgPrice;pg_uMaxPrice;av_nTotalLots;av_nTotalQty;' +
        'av_nMinPrice;av_nAvgPrice;av_nQtyAvgPrice;av_nMaxPrice;av_uTotalLots;' +
        'av_uTotalQty;av_uMinPrice;av_uAvgPrice;av_uQtyAvgPrice;av_uMaxPrice;' +
        'EvaluatePriceNew;EvaluatePriceUsed');

  tot := 0;
  for i := -1 to MAXINFOCOLOR do
    if (fcolors[i].id = i) or (i = -1) then
      if fcolors[i].knownpieces <> nil then
        Inc(tot, fcolors[i].knownpieces.Count);

  k := 0;
  for i := -1 to MAXINFOCOLOR do
    if (fcolors[i].id = i) or (i = -1) then
    begin
      kp := fcolors[i].knownpieces;
      if kp <> nil then
      begin
        for j := 0 to kp.Count - 1 do
        begin
          if j mod 500 = 0 then
            if Assigned(progressfunc) then
              progressfunc(pregressstring, (k + j) / tot);
          s.Add((kp.Objects[j] as TPieceColorInfo).dbExportStringPG);
        end;
        inc(k, kp.Count);
        if Assigned(progressfunc) then
          progressfunc(pregressstring, k / tot);
      end;
    end;

  S_SaveToFile(s, fname);
  s.Free;

  if Assigned(progressfunc) then
    progressfunc(pregressstring, 1.0);

  AllowInternetAccess := True;
end;

procedure TSetsDatabase.ExportPartOutGuide(const fname: string);
var
  i: integer;
  inv: TBrickInventory;
  pg: priceguide_t;
  av: availability_t;
  sold_nAvg: partout_t;
  sold_nQtyAvg: partout_t;
  sold_uAvg: partout_t;
  sold_uQtyAvg: partout_t;
  avail_nAvg: partout_t;
  avail_nQtyAvg: partout_t;
  avail_uAvg: partout_t;
  avail_uQtyAvg: partout_t;
  s: TStringList;
  sset: string;
  pregressstring: string;
begin
  AllowInternetAccess := False;

  pregressstring := 'Export partout guide...';
  if Assigned(progressfunc) then
    progressfunc(pregressstring, 0.0);

  printf('TSetsDatabase.ExportPartOutGuide(' + fname + ')'#13#10);

  s := TStringList.Create;

  s.Add('Set,pg_nTimesSold,pg_nTotalQty,pg_nMinPrice,pg_nAvgPrice,' +
        'pg_nQtyAvgPrice,pg_nMaxPrice,pg_uTimesSold,pg_uTotalQty,pg_uMinPrice,' +
        'pg_uAvgPrice,pg_uQtyAvgPrice,pg_uMaxPrice,av_nTotalLots,av_nTotalQty,' +
        'av_nMinPrice,av_nAvgPrice,av_nQtyAvgPrice,av_nMaxPrice,av_uTotalLots,' +
        'av_uTotalQty,av_uMinPrice,av_uAvgPrice,av_uQtyAvgPrice,av_uMaxPrice,' +
        'sold_nPct,sold_nAvg,sold_nQtyAvg,sold_uPct,sold_uAvg,sold_uQtyAvg,' +
        'avail_nPct,avail_nAvg,avail_nQtyAvg,avail_uPct,avail_uAvg,avail_uQtyAvg');

  for i := 0 to fallsets.Count - 1 do
  begin
    if i mod 100 = 0 then
      if Assigned(progressfunc) then
        progressfunc(pregressstring, i / fallsets.Count);

    sset := fallsets.Strings[i];
    pg := Priceguide(sset);
    av := Availability(sset);
    inv := fallsets.Objects[i] as TBrickInventory;
    inv.UpdateCostValues;
    sold_nAvg := inv.SoldPartOutValue_nAvg;
    sold_nQtyAvg := inv.SoldPartOutValue_nQtyAvg;
    sold_uAvg := inv.SoldPartOutValue_uAvg;
    sold_uQtyAvg := inv.SoldPartOutValue_uQtyAvg;
    avail_nAvg := inv.AvailablePartOutValue_nAvg;
    avail_nQtyAvg := inv.AvailablePartOutValue_nQtyAvg;
    avail_uAvg := inv.AvailablePartOutValue_uAvg;
    avail_uQtyAvg := inv.AvailablePartOutValue_uQtyAvg;

    s.Add(
      Format(
        '%s;%d;%d;%2.2f;%2.2f;%2.2f;%2.2f;%d;%d;%2.2f;%2.2f;%2.2f;%2.2f;' +
        '%d;%d;%2.2f;%2.2f;%2.2f;%2.2f;%d;%d;%2.2f;%2.2f;%2.2f;%2.2f;' +
        '%2.2f;%2.2f;%2.2f;%2.2f;%2.2f;%2.2f;%2.2f;%2.2f;%2.2f;%2.2f;%2.2f;%2.2f',
       [sset,
        pg.nTimesSold,
        pg.nTotalQty,
        pg.nMinPrice,
        pg.nAvgPrice,
        pg.nQtyAvgPrice,
        pg.nMaxPrice,
        pg.uTimesSold,
        pg.uTotalQty,
        pg.uMinPrice,
        pg.uAvgPrice,
        pg.uQtyAvgPrice,
        pg.uMaxPrice,
        av.nTotalLots,
        av.nTotalQty,
        av.nMinPrice,
        av.nAvgPrice,
        av.nQtyAvgPrice,
        av.nMaxPrice,
        av.uTotalLots,
        av.uTotalQty,
        av.uMinPrice,
        av.uAvgPrice,
        av.uQtyAvgPrice,
        av.uMaxPrice,
        sold_nAvg.percentage,
        sold_nAvg.value,
        sold_nQtyAvg.value,
        sold_uAvg.percentage,
        sold_uAvg.value,
        sold_uQtyAvg.value,
        avail_nAvg.percentage,
        avail_nAvg.value,
        avail_nQtyAvg.value,
        avail_uAvg.percentage,
        avail_uAvg.value,
        avail_uQtyAvg.value]));
  end;

  S_BackupFile(fname);
  S_SaveToFile(s, fname);
  s.Free;

  if Assigned(progressfunc) then
    progressfunc(pregressstring, 1.0);

  AllowInternetAccess := True;
end;

{$IFNDEF CRAWLER}
procedure TSetsDatabase.ExportDatabase(const fname: string);
var
  s: TStringList;
  i, j: integer;
  k, tot: integer;
  progressstring: string;
begin
  progressstring := 'Export database...';
  if Assigned(progressfunc) then
    progressfunc(progressstring, 0.0);

  printf('TSetsDatabase.ExportDatabase(' + fname + ')'#13#10);

  s := TStringList.Create;
  s.Add('Type;CatId;Custom;Name;BLAlias;BLName;Color;BLColor;Code;Inventory;Year;Weight;Desc');

  tot := 0;
  for i := -1 to MAXINFOCOLOR do
    if (fcolors[i].id = i) or (i = -1) then
      if fcolors[i].knownpieces <> nil then
        Inc(tot, fcolors[i].knownpieces.Count);

  k := 0;
  for i := -1 to MAXINFOCOLOR do
    if (fcolors[i].id = i) or (i = -1) then
    begin
      if fcolors[i].knownpieces <> nil then
      begin
        for j := 0 to fcolors[i].knownpieces.Count - 1 do
        begin
          if j mod 500 = 0 then
            if Assigned(progressfunc) then
              progressfunc(progressstring, (k + j) / tot);
          s.Add((fcolors[i].knownpieces.Objects[j] as TPieceColorInfo).dbExportStringDB);
        end;
        inc(k, fcolors[i].knownpieces.Count);
        if Assigned(progressfunc) then
          progressfunc(progressstring, k / tot);
      end;
    end;

  S_BackupFile(fname);
  S_SaveToFile(s, fname);
  s.Free;

  if Assigned(progressfunc) then
    progressfunc(progressstring, 1.0);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
procedure TSetsDatabase.ExportPartColor(const fname: string);
var
  s: TStringList;
  i, j: integer;
  k, tot: integer;
  progressstring: string;
  pci: TPieceColorInfo;
begin
  progressstring := 'Export piece/color data...';
  if Assigned(progressfunc) then
    progressfunc(progressstring, 0.0);

  printf('TSetsDatabase.ExportPartColor(' + fname + ')'#13#10);

  s := TStringList.Create;
  s.Add('Name;Color');

  tot := 0;
  for i := -1 to MAXINFOCOLOR do
    if (fcolors[i].id = i) or (i = -1) then
      if fcolors[i].knownpieces <> nil then
        Inc(tot, fcolors[i].knownpieces.Count);

  k := 0;
  for i := -1 to MAXINFOCOLOR do
    if (fcolors[i].id = i) or (i = -1) then
    begin
      if fcolors[i].knownpieces <> nil then
      begin
        for j := 0 to fcolors[i].knownpieces.Count - 1 do
        begin
          if j mod 500 = 0 then
            if Assigned(progressfunc) then
              progressfunc(progressstring, (k + j) / tot);
          pci := fcolors[i].knownpieces.Objects[j] as TPieceColorInfo;
          s.Add(Format('%s;%d', [pci.piece, pci.color]));
        end;
        inc(k, fcolors[i].knownpieces.Count);
        if Assigned(progressfunc) then
          progressfunc(progressstring, k / tot);
      end;
    end;

  S_BackupFile(fname);
  S_SaveToFile(s, fname);
  s.Free;

  if Assigned(progressfunc) then
    progressfunc(progressstring, 1.0);
end;
{$ENDIF}

constructor TPieceInfo.Create;
begin
  fname := '';
  {$IFNDEF CRAWLER}
  fdesc := '';
  {$ENDIF}
  fcategory := 0;
  fweight := 0;
  {$IFNDEF CRAWLER}
  fmoldvariations := TStringList.Create;
  falternates := TStringList.Create;
  fpatternof := '';
  fprintof := '';
  fpatterns := TStringList.Create;
  fprints := TStringList.Create;
  {$ENDIF}
  {$IFNDEF CRAWLER}
  fdimentionx := 0;
  fdimentiony := 0;
  fdimentionz := 0;
  {$ENDIF}
  finventoryfound := False;
  fpartsinventoriesvalidcount := 0;
  finventoryname := '';
  inherited;
end;

destructor TPieceInfo.Destroy;
begin
  {$IFNDEF CRAWLER}
  FreeList(fmoldvariations);
  FreeList(falternates);
  FreeList(fpatterns);
  FreeList(fprints);
  {$ENDIF}
  Inherited Destroy;
end;

function TPieceInfo.Inventory(const color: integer): TBrickInventory;
var
  stmp: string;
  slist, slist2: TStringList;
  fnametmp: string;
  s_part, s_color, s_defcolor, s_num: string;
  i: integer;
begin
  if not hasinventory then
  begin
    Result := nil;
    Exit;
  end;

  if finventoryname = '' then
  begin
    Result := nil;
    Exit;
  end;

  slist := TStringList.Create;
  stmp := db.binaryparts.GetPartAsText(finventoryname, color);
  slist.Text := stmp;
  if slist.Count < 2 then
  begin
    fnametmp := basedefault + 'db\parts\' + finventoryname + '.txt';
    if fexists(fnametmp) then
    begin
      slist2 := TStringList.Create;
      try
        S_LoadFromFile(slist2, fnametmp);
        s_defcolor := itoa(color);
        slist.Clear;
        for i := 0 to slist2.Count - 1 do
        begin
          splitstring(slist2.Strings[i], s_part, s_color, s_num, ',');
          s_part := Trim(s_part);
          s_color := Trim(s_color);
          s_num := Trim(s_num);
          if (s_color = '') or (s_color = '-2') or (s_color = 'BL 0') or (s_color = 'BL0') then
            slist.Add(s_part + ',' + s_defcolor + ',' + s_num)
          else
            slist.Add(s_part + ',' + s_color + ',' + s_num)
        end;
      finally
        slist2.Free;
      end;
    end;
  end;
  if slist.Count < 2 then
  begin
    slist.Free;
    Result := nil;
    Exit;
  end;

  Result := TBrickInventory.Create;
  Result.LoadLooseParts(slist);
  {$IFNDEF CRAWLER}
  for i := 0 to Result.numlooseparts - 1 do
    db.AddKnownPiecePINV(Result.looseparts[i].part, Result.looseparts[i].color, db.PieceDesc(Result.looseparts[i].part));
  {$ENDIF}

  slist.Free;
end;

function TPieceInfo.InventoryAsText(const color: integer): string;
var
  stmp: string;
  slist, slist2: TStringList;
  fnametmp: string;
  s_part, s_color, s_defcolor, s_num: string;
  i: integer;
begin
  if not hasinventory then
  begin
    Result := '';
    Exit;
  end;

  if finventoryname = '' then
  begin
    Result := '';
    Exit;
  end;

  slist := TStringList.Create;
  stmp := db.binaryparts.GetPartAsText(finventoryname, color);
  slist.Text := stmp;
  if slist.Count < 2 then
  begin
    fnametmp := basedefault + 'db\parts\' + finventoryname + '.txt';
    if fexists(fnametmp) then
    begin
      slist2 := TStringList.Create;
      try
        S_LoadFromFile(slist2, fnametmp);
        if color = -2 then
        begin
          slist.Text := slist2.Text;
        end
        else
        begin
          slist.Clear;
          s_defcolor := itoa(color);
          for i := 0 to slist2.Count - 1 do
          begin
            splitstring(slist2.Strings[i], s_part, s_color, s_num, ',');
            s_part := Trim(s_part);
            s_color := Trim(s_color);
            s_num := Trim(s_num);
            if (s_color = '') or (s_color = '-2') or (s_color = 'BL 0') or (s_color = 'BL0') then
              slist.Add(s_part + ',' + s_defcolor + ',' + s_num)
            else
              slist.Add(s_part + ',' + s_color + ',' + s_num)
          end;
        end;
      finally
        slist2.Free;
      end;
    end;
  end;
  if slist.Count < 2 then
  begin
    slist.Free;
    Result := '';
    Exit;
  end;

  Result := slist.Text;
  slist.Free;
end;

function TPieceInfo.hasinventory: boolean;
var
  dbvalid: integer;
  invname: string;
begin
  if finventoryfound then
  begin
    Result := True;
    Exit;
  end;

  dbvalid := db.partsinventoriesvalidcount;
  if dbvalid = fpartsinventoriesvalidcount then
  begin
    Result := False;
    Exit;
  end;

  invname := fname;
  Result := db.partsinventories.IndexOf(invname) > 0;
  if not Result then
  begin
    invname := db.GetBLNetPieceName(fname);
    Result := db.partsinventories.IndexOf(invname) > 0;
    if not Result then
    begin
      invname := db.BrickLinkPart(fname);
      Result := db.partsinventories.IndexOf(invname) > 0;
      if not Result then
      begin
        invname := db.RebrickablePart(fname);
        Result := db.partsinventories.IndexOf(invname) > 0;
      end;
    end;
  end;

  if Result then
  begin
    finventoryfound := True;
    finventoryname := invname;
  end
  else
    finventoryname := '';

  fpartsinventoriesvalidcount := dbvalid;
end;

function PartTypeToPartTypeName(const pt: char): string;
begin
  case pt of
    'P': Result := SPT_PART;
    'S': Result := SPT_SET;
    'M': Result := SPT_MINIFIG;
    'C': Result := SPT_CATALOG;
    'B': Result := SPT_BOOK;
    'I': Result := SPT_INSTRUCTIONS;
    'O': Result := SPT_BOX;
    'G': Result := SPT_GEAR;
  else
    Result := '';
  end;
end;

function PartTypeNameToPartType(const pn: string): char;
var
  check: string;
begin
  check := UpperCase(pn);
  if check = UpperCase(SPT_PART) then
    Result := 'P'
  else if check = UpperCase(SPT_SET) then
    Result := 'S'
  else if check = UpperCase(SPT_MINIFIG) then
    Result := 'M'
  else if check = UpperCase(SPT_CATALOG) then
    Result := 'C'
  else if check = UpperCase(SPT_BOOK) then
    Result := 'B'
  else if check = UpperCase(SPT_INSTRUCTIONS) then
    Result := 'I'
  else if check = UpperCase(SPT_BOX) then
    Result := 'O'
  else if check = UpperCase(SPT_GEAR) then
    Result := 'G'
  else
    Result := ' ';
end;

constructor TPieceColorInfo.Create(const apiece: string; const acolor: integer);
begin
  flastinternetupdate := Now - 1.0;
  fsparttype := ' ';
  {$IFNDEF CRAWLER}
  fsetmost := '';
  fsetmostnum := 0;
  fpartmost := '';
  fpartmostnum := 0;
  {$ENDIF}
  fcode := '';
  fcrawlerlink := '';
  fpieceinfo := nil;
  if acolor = CATALOGCOLORINDEX then
    fparttype := TYPE_CATALOG
  else if acolor = INSTRUCTIONCOLORINDEX then
    fparttype := TYPE_INSTRUCTIONS
  else if acolor = BOXCOLORINDEX then
    fparttype := TYPE_BOX
  else
    fparttype := TYPE_PART;
  fcacheread := False;
  fsets := TStringList.Create;
  fsets.Sorted := True;
  fparts := TStringList.Create;
  fparts.Sorted := True;
  {$IFNDEF CRAWLER}
  fstorage := TStringList.Create;
  {$ENDIF}
  FillChar(fpriceguide, SizeOf(priceguide_t), 0);
  FillChar(favailability, SizeOf(availability_t), 0);
  {$IFNDEF CRAWLER}
  fappearsinsets := 0;
  fappearsinsetstotal := 0;
  fappearsinparts := 0;
  fappearsinpartstotal := 0;
  {$ENDIF}
  fpiece := apiece;
  fcolor := acolor;
  fhash := MkPCIHash(apiece, acolor);
  fcolorstr := IntToStr(fcolor);
  fneedssave := False;
  fhasloaded := False;
  fdate := Now;
  {$IFNDEF CRAWLER}
  fcanedityear := False;
  ffirstsetyear := 9999;
  flastsetyear := 0;
  fyear := 0;
  {$ENDIF}
  inherited Create;
end;

destructor TPieceColorInfo.Destroy;
begin
  FreeList(fsets);
  FreeList(fparts);
  {$IFNDEF CRAWLER}
  FreeList(fstorage);
  {$ENDIF}
  inherited;
end;

procedure TPieceColorInfo.SetSPartType(const t: char);
begin
  if not (t in ['P', 'S', 'M', 'C', 'B', 'I', 'O', 'G', ' ']) then
    Exit;
  fsparttype := t;
end;

function TPieceColorInfo.LoadFromDisk: boolean;
var
  fname: string;
  f: TFileStream;
  pa: parec_t;
  pad: parecdate_t;
  sz: integer;
begin
  fcacheread := False;
  inc(db.st_pciloads);
  if db.CacheDB.LoadPCI(self) then
  begin
    inc(db.st_pciloadscache);
    fneedssave := False;
    fcacheread := True;
    Result := True;
    Exit;
  end;
  Result := False;
  fname := PieceColorCacheFName(fpiece, fcolorstr);
  if fexists(fname) then
  begin
    try
      f := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
    except
      Sleep(150);
      try
        f := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
      except
        Exit;
      end;
    end;
    sz := f.Size;
    if sz = SizeOf(priceguide_t) + SizeOf(availability_t) then
    begin
      f.Read(pa, SizeOf(parec_t));
      f.Free;
      fdate := GetFileCreationTime(fname);
    end
    else if sz = SizeOf(priceguide_t) then
    begin
      f.Read(pa.priceguide, SizeOf(priceguide_t));
      ZeroMemory(@pa.availability, SizeOf(availability_t));
      f.Free;
      fdate := GetFileCreationTime(fname);
    end
    else if (sz mod SizeOf(parecdate_t) = 0) and (sz >= SizeOf(parecdate_t)) then
    begin
      f.Position := sz - SizeOf(parecdate_t);
      f.Read(pad, SizeOf(parecdate_t));
      pa.priceguide := pad.priceguide;
      pa.availability := pad.availability;
      fdate := pad.date;
      f.Free;
    end
    else
    begin
      ZeroMemory(@pa.priceguide, SizeOf(priceguide_t));
      ZeroMemory(@pa.availability, SizeOf(availability_t));
      f.Free;
      fdate := GetFileCreationTime(fname);
    end;
    PRICEADJUST(fpiece, fcolor, pa.priceguide, pa.availability, fdate);
    Assign(pa.priceguide);
    Assign(pa.availability);
    fneedssave := False;
    Result := Check;
  end;
end;

procedure TPieceColorInfo.Load;
begin
  if not LoadFromDisk then
  begin
    printf('Can not find cache info for part %d,%s'#13#10, [fcolor, fpiece]);
    InternetUpdate;
  end;
end;

procedure TPieceColorInfo.SaveToDisk;
begin
  if not fneedssave then
    Exit;

  DoSaveToDisk;
end;

procedure TPieceColorInfo.DoSaveToDisk;
var
  fname: string;
  fdir: string;
  f: TFileStream;
  pa: parec_t;
  pad: parecdate_t;
  d: TDateTime;
  sz: Int64;
begin
  if not fhasloaded then
    Exit;

  fdir := PieceColorCacheDir(fpiece, fcolorstr);
  if not DirectoryExists(fdir) then
    ForceDirectories(fdir);
  fname := PieceColorCacheFName(fpiece, fcolorstr);

  if not fexists(fname) then
  begin
    try
      f := TFileStream.Create(fname, fmCreate or fmShareDenyWrite);
    except
      Sleep(150);
      try
        f := TFileStream.Create(fname, fmCreate or fmShareDenyWrite);
      except
        Exit;
      end;
    end;
  end
  else
  begin
    d := GetFileCreationTime(fname);
    f := TFileStream.Create(fname, fmOpenReadWrite or fmShareDenyWrite);
    sz := f.size;
    if sz = SizeOf(parec_t) then
    begin
      f.Read(pa, SizeOf(parec_t));
      pad.priceguide := pa.priceguide;
      pad.availability := pa.availability;
      pad.date := d - 1.0;
      f.Position := 0;
      f.Write(pad, SizeOf(pad))
    end
    else if sz = SizeOf(priceguide_t) then
    begin
      f.Read(pa.priceguide, SizeOf(priceguide_t));
      ZeroMemory(@pa.availability, SizeOf(availability_t));
      pad.priceguide := pa.priceguide;
      pad.availability := pa.availability;
      pad.date := d - 1.0;
      f.Position := 0;
      f.Write(pad, SizeOf(pad));
    end;
  end;

  f.Position := f.Size;
  Check;
  f.Write(fpriceguide, SizeOf(priceguide_t));
  f.Write(favailability, SizeOf(availability_t));
  f.Write(fdate, SizeOf(TDateTime));

  f.Free;

  db.CacheDB.SavePCI(self);
  fneedssave := False;
end;

procedure TPieceColorInfo.InternetUpdate;
var
  pg: priceguide_t;
  av: availability_t;
  fdir, fname: string;
  tmpdate: double;
begin
  tmpdate := Now;
  if tmpdate - flastinternetupdate < 1 / (24 * 60 * 12) then // 5 seconds
    Exit;
  flastinternetupdate := tmpdate;
  fdir := PieceColorCacheDir(fpiece, fcolorstr);
  if not DirectoryExists(fdir) then
    ForceDirectories(fdir);
  fname := PieceColorCacheFName(fpiece, fcolorstr) + '.htm';
  if NET_GetPriceGuideForElement(self, fpiece, fcolorstr, pg, av, fname, fcrawlerlink) then
  begin
    fdate := Now;
    PRICEADJUST(fpiece, fcolor, pg, av, fdate);
    Assign(pg);
    Assign(av);
    SaveToDisk;
  end
  else if not fexists(PieceColorCacheFName(fpiece, fcolorstr)) then
  begin
    FillChar(pg, SizeOf(priceguide_t), 0);
    FillChar(av, SizeOf(availability_t), 0);
    fdate := Now;
    PRICEADJUST(fpiece, fcolor, pg, av, fdate);
    Assign(pg);
    Assign(av);
    SaveToDisk;
  end;
end;

function TPieceColorInfo.invalid: boolean;
begin
  Result := fpriceguide.nTimesSold + fpriceguide.nTotalQty + fpriceguide.uTimesSold + fpriceguide.uTotalQty +
            favailability.nTotalLots + favailability.nTotalQty + favailability.uTotalLots + favailability.uTotalQty = 0;
end;

procedure TPieceColorInfo.AddPartReference(const apart: string; const numpieces: integer);
begin
  if numpieces > 0 then
  begin
    {$IFNDEF CRAWLER}
    Inc(fappearsinparts);
    Inc(fappearsinpartstotal, numpieces);
    {$ENDIF}
    if fparts.IndexOf(apart) < 0 then
      fparts.Add(apart);
    {$IFNDEF CRAWLER}
    if numpieces > fpartmostnum then
    begin
      fpartmost := apart;
      fpartmostnum := numpieces;
    end;
    {$ENDIF}
  end;
end;

procedure TPieceColorInfo.AddSetReference(const aset: string; const numpieces: integer);
begin
  if numpieces > 0 then
  begin
    {$IFNDEF CRAWLER}
    Inc(fappearsinsets);
    Inc(fappearsinsetstotal, numpieces);
    {$ENDIF}
    if fsets.IndexOf(aset) < 0 then
      fsets.Add(aset);
    {$IFNDEF CRAWLER}
    if numpieces > fsetmostnum then
    begin
      fsetmost := aset;
      fsetmostnum := numpieces;
    end;
    {$ENDIF}
  end;
end;

{$IFNDEF CRAWLER}
procedure TPieceColorInfo.UpdatePartYears(const y: integer = -1);
var
  yyyy: integer;
begin
{  if db.IsMoc(setid) then
    Exit;}
  if y = -1 then
    yyyy := fyear
  else
    yyyy := y;
  if (yyyy < 1932) or (yyyy > 2050) then
    Exit;

  if yyyy < ffirstsetyear then
  begin
    ffirstsetyear := yyyy;
    if (ffirstsetyear < fyear) or (fyear = 0) then
      fyear := yyyy;
  end;
  if yyyy > flastsetyear then
  begin
    flastsetyear := yyyy;
    if fyear = 0 then
      fyear := flastsetyear;
  end;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
procedure TPieceColorInfo.UpdateSetYears(const setid: string; const y: integer = -1);
var
  yyyy: integer;
begin
{  if db.IsMoc(setid) then
    Exit;}
  if y = -1 then
    yyyy := db.SetYear(setid)
  else
    yyyy := y;
  if (yyyy < 1932) or (yyyy > 2050) then
    Exit;
  if yyyy < ffirstsetyear then
  begin
    ffirstsetyear := yyyy;
    if (ffirstsetyear < fyear) or (fyear = 0) then
      fyear := yyyy;
  end;
  if yyyy > flastsetyear then
  begin
    flastsetyear := yyyy;
    if fyear = 0 then
      fyear := flastsetyear;
  end;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TPieceColorInfo.PG_nTimesSoldAt(const at: TDateTime): integer;
begin
  Result := bi_pghistory.PG_nTimesSoldAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.PG_nTotalQtyAt(const at: TDateTime): integer;
begin
  Result := bi_pghistory.PG_nTotalQtyAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.PG_nMinPriceAt(const at: TDateTime): double;
begin
  Result := bi_pghistory.PG_nMinPriceAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.PG_nAvgPriceAt(const at: TDateTime): double;
begin
  Result := bi_pghistory.PG_nAvgPriceAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.PG_nQtyAvgPriceAt(const at: TDateTime): double;
begin
  Result := bi_pghistory.PG_nQtyAvgPriceAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.PG_nMaxPriceAt(const at: TDateTime): double;
begin
  Result := bi_pghistory.PG_nMaxPriceAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.PG_uTimesSoldAt(const at: TDateTime): integer;
begin
  Result := bi_pghistory.PG_uTimesSoldAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.PG_uTotalQtyAt(const at: TDateTime): integer;
begin
  Result := bi_pghistory.PG_uTotalQtyAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.PG_uMinPriceAt(const at: TDateTime): double;
begin
  Result := bi_pghistory.PG_uMinPriceAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.PG_uAvgPriceAt(const at: TDateTime): double;
begin
  Result := bi_pghistory.PG_uAvgPriceAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.PG_uQtyAvgPriceAt(const at: TDateTime): double;
begin
  Result := bi_pghistory.PG_uQtyAvgPriceAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.PG_uMaxPriceAt(const at: TDateTime): double;
begin
  Result := bi_pghistory.PG_uMaxPriceAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.AV_nTotalLotsAt(const at: TDateTime): integer;
begin
  Result := bi_pghistory.AV_nTotalLotsAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.AV_nTotalQtyAt(const at: TDateTime): integer;
begin
  Result := bi_pghistory.AV_nTotalQtyAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.AV_nMinPriceAt(const at: TDateTime): double;
begin
  Result := bi_pghistory.AV_nMinPriceAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.AV_nAvgPriceAt(const at: TDateTime): double;
begin
  Result := bi_pghistory.AV_nAvgPriceAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.AV_nQtyAvgPriceAt(const at: TDateTime): double;
begin
  Result := bi_pghistory.AV_nQtyAvgPriceAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.AV_nMaxPriceAt(const at: TDateTime): double;
begin
  Result := bi_pghistory.AV_nMaxPriceAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.AV_uTotalLotsAt(const at: TDateTime): integer;
begin
  Result := bi_pghistory.AV_uTotalLotsAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.AV_uTotalQtyAt(const at: TDateTime): integer;
begin
  Result := bi_pghistory.AV_uTotalQtyAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.AV_uMinPriceAt(const at: TDateTime): double;
begin
  Result := bi_pghistory.AV_uMinPriceAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.AV_uAvgPriceAt(const at: TDateTime): double;
begin
  Result := bi_pghistory.AV_uAvgPriceAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.AV_uQtyAvgPriceAt(const at: TDateTime): double;
begin
  Result := bi_pghistory.AV_uQtyAvgPriceAt(fpiece, fcolor, at);
end;

function TPieceColorInfo.AV_uMaxPriceAt(const at: TDateTime): double;
begin
  Result := bi_pghistory.AV_uMaxPriceAt(fpiece, fcolor, at);
end;
{$ENDIF}

{$IFNDEF CRAWLER}
procedure TPieceColorInfo.SetYear(const y: integer);
begin
  if (y < 1932) or (y > 2050) then
    Exit;
  if (y > ffirstsetyear) and (ffirstsetyear >= 1932) and (ffirstsetyear <= 2050) then
    fyear := ffirstsetyear
  else
    fyear := y;
end;
{$ENDIF}

procedure TPieceColorInfo.UpdateSetReference(const aset: string; const numpieces: integer);
begin
  if numpieces > 0 then
  begin
    if fsets.IndexOf(aset) < 0 then
    begin
      fsets.Add(aset);
      {$IFNDEF CRAWLER}
      Inc(fappearsinsets);
      Inc(fappearsinsetstotal, numpieces);
      {$ENDIF}
      {$IFNDEF CRAWLER}
      if numpieces > fsetmostnum then
      begin
        fsetmost := aset;
        fsetmostnum := numpieces;
      end;
      {$ENDIF}
    end;
  end;
end;

procedure TPieceColorInfo.UpdatePartReference(const apart: string; const numpieces: integer);
begin
  if numpieces > 0 then
  begin
    if fparts.IndexOf(apart) < 0 then
    begin
      fparts.Add(apart);
      {$IFNDEF CRAWLER}
      Inc(fappearsinparts);
      Inc(fappearsinpartstotal, numpieces);
      {$ENDIF}
      {$IFNDEF CRAWLER}
      if numpieces > fpartmostnum then
      begin
        fpartmost := apart;
        fpartmostnum := numpieces;
      end;
      {$ENDIF}
    end;
  end;
end;

function TPieceColorInfo.Check: boolean;
const
  MAXPRICE = 250000.00;
begin
  Result := True;

  if between(fpriceguide.nTimesSold, 0, MaxInt) and
     between(fpriceguide.nTotalQty, 0, MaxInt) and
     (fpriceguide.nTimesSold <= fpriceguide.nTotalQty) and
     between(fpriceguide.nMinPrice, 0, MAXPRICE) and
     between(fpriceguide.nAvgPrice, 0, MAXPRICE) and
     between(fpriceguide.nQtyAvgPrice, 0, MAXPRICE) and
     between(fpriceguide.nMaxPrice, 0, MAXPRICE) and
     (fpriceguide.nMaxPrice >= fpriceguide.nQtyAvgPrice) and
     (fpriceguide.nMaxPrice >= fpriceguide.nAvgPrice) and
     (fpriceguide.nMaxPrice >= fpriceguide.nMinPrice) and
     (fpriceguide.nMinPrice <= fpriceguide.nQtyAvgPrice) and
     (fpriceguide.nMinPrice <= fpriceguide.nAvgPrice) then
  else
  begin
    fpriceguide.nTimesSold := 0;
    fpriceguide.nTotalQty := 0;
    fpriceguide.nMinPrice := 0;
    fpriceguide.nAvgPrice := 0;
    fpriceguide.nQtyAvgPrice := 0;
    fpriceguide.nMaxPrice := 0;
    fneedssave := True;
    Result := False;
  end;

  if between(fpriceguide.uTimesSold, 0, MaxInt) and
     between(fpriceguide.uTotalQty, 0, MaxInt) and
     (fpriceguide.uTimesSold <= fpriceguide.uTotalQty) and
     between(fpriceguide.uMinPrice, 0, MAXPRICE) and
     between(fpriceguide.uAvgPrice, 0, MAXPRICE) and
     between(fpriceguide.uQtyAvgPrice, 0, MAXPRICE) and
     between(fpriceguide.uMaxPrice, 0, MAXPRICE) and
     (fpriceguide.uMaxPrice >= fpriceguide.uQtyAvgPrice) and
     (fpriceguide.uMaxPrice >= fpriceguide.uAvgPrice) and
     (fpriceguide.uMaxPrice >= fpriceguide.uMinPrice) and
     (fpriceguide.uMinPrice <= fpriceguide.uQtyAvgPrice) and
     (fpriceguide.uMinPrice <= fpriceguide.uAvgPrice) then
  else
  begin
    fpriceguide.uTimesSold := 0;
    fpriceguide.uTotalQty := 0;
    fpriceguide.uMinPrice := 0;
    fpriceguide.uAvgPrice := 0;
    fpriceguide.uQtyAvgPrice := 0;
    fpriceguide.uMaxPrice := 0;
    fneedssave := True;
    Result := False;
  end;

  if between(favailability.nTotalLots, 0, MaxInt) and
     between(favailability.nTotalQty, 0, MaxInt) and
     (favailability.nTotalLots <= favailability.nTotalQty) and
     between(favailability.nMinPrice, 0, MAXPRICE) and
     between(favailability.nAvgPrice, 0, MAXPRICE) and
     between(favailability.nQtyAvgPrice, 0, MAXPRICE) and
     between(favailability.nMaxPrice, 0, MAXPRICE) and
     (favailability.nMaxPrice >= favailability.nQtyAvgPrice) and
     (favailability.nMaxPrice >= favailability.nAvgPrice) and
     (favailability.nMaxPrice >= favailability.nMinPrice) and
     (favailability.nMinPrice <= favailability.nQtyAvgPrice) and
     (favailability.nMinPrice <= favailability.nAvgPrice) then
  else
  begin
    favailability.nTotalLots := 0;
    favailability.nTotalQty := 0;
    favailability.nMinPrice := 0;
    favailability.nAvgPrice := 0;
    favailability.nQtyAvgPrice := 0;
    favailability.nMaxPrice := 0;
    fneedssave := True;
    Result := False;
  end;

  if between(favailability.uTotalLots, 0, MaxInt) and
     between(favailability.uTotalQty, 0, MaxInt) and
     (favailability.uTotalLots <= favailability.uTotalQty) and
     between(favailability.uMinPrice, 0, MAXPRICE) and
     between(favailability.uAvgPrice, 0, MAXPRICE) and
     between(favailability.uQtyAvgPrice, 0, MAXPRICE) and
     between(favailability.uMaxPrice, 0, MAXPRICE) and
     (favailability.uMaxPrice >= favailability.uQtyAvgPrice) and
     (favailability.uMaxPrice >= favailability.uAvgPrice) and
     (favailability.uMaxPrice >= favailability.uMinPrice) and
     (favailability.uMinPrice <= favailability.uQtyAvgPrice) and
     (favailability.uMinPrice <= favailability.uAvgPrice) then
  else
  begin
    favailability.uTotalLots := 0;
    favailability.uTotalQty := 0;
    favailability.uMinPrice := 0;
    favailability.uAvgPrice := 0;
    favailability.uQtyAvgPrice := 0;
    favailability.uMaxPrice := 0;
    fneedssave := True;
    Result := False;
  end;

end;

procedure TPieceColorInfo.Assign(const pg: priceguide_t);
begin
  fpriceguide.nTimesSold := pg.nTimesSold;
  fpriceguide.nTotalQty := pg.nTotalQty;
  if pg.nMinPrice > 0 then
    fpriceguide.nMinPrice := pg.nMinPrice;
  if pg.nAvgPrice > 0 then
    fpriceguide.nAvgPrice := pg.nAvgPrice;
  if pg.nQtyAvgPrice > 0 then
    fpriceguide.nQtyAvgPrice := pg.nQtyAvgPrice;
  if pg.nMaxPrice > 0 then
    fpriceguide.nMaxPrice := pg.nMaxPrice;
  fpriceguide.uTimesSold := pg.uTimesSold;
  fpriceguide.uTotalQty := pg.uTotalQty;
  if pg.uMinPrice > 0 then
    fpriceguide.uMinPrice := pg.uMinPrice;
  if pg.uAvgPrice > 0 then
    fpriceguide.uAvgPrice := pg.uAvgPrice;
  if pg.uQtyAvgPrice > 0 then
    fpriceguide.uQtyAvgPrice := pg.uQtyAvgPrice;
  if pg.uMaxPrice > 0 then
    fpriceguide.uMaxPrice := pg.uMaxPrice;
  fneedssave := True;
  fhasloaded := True;
end;

procedure TPieceColorInfo.Assign(const av: availability_t);
begin
  favailability.nTotalLots := av.nTotalLots;
  favailability.nTotalQty := av.nTotalQty;
  if av.nMinPrice > 0 then
    favailability.nMinPrice := av.nMinPrice;
  if av.nAvgPrice > 0 then
    favailability.nAvgPrice := av.nAvgPrice;
  if av.nQtyAvgPrice > 0 then
    favailability.nQtyAvgPrice := av.nQtyAvgPrice;
  if av.nMaxPrice > 0 then
    favailability.nMaxPrice := av.nMaxPrice;
  favailability.uTotalLots := av.uTotalLots;
  favailability.uTotalQty := av.uTotalQty;
  if av.uMinPrice > 0 then
    favailability.uMinPrice := av.uMinPrice;
  if av.uAvgPrice > 0 then
    favailability.uAvgPrice := av.uAvgPrice;
  if av.uQtyAvgPrice > 0 then
    favailability.uQtyAvgPrice := av.uQtyAvgPrice;
  if av.uMaxPrice > 0 then
    favailability.uMaxPrice := av.uMaxPrice;
  fneedssave := True;
  fhasloaded := True;
end;

function TPieceColorInfo.EvaluatePriceNew: double;
begin
  if not fhasloaded then
    Load;
  if fpriceguide.nTimesSold > 0 then
    Result := fpriceguide.nQtyAvgPrice
  else if fpriceguide.uTimesSold > 0 then
    Result := fpriceguide.uQtyAvgPrice * 1.20
  else if favailability.nTotalLots > 0 then
    Result := favailability.nQtyAvgPrice
  else if favailability.uTotalLots > 0 then
    Result := favailability.uQtyAvgPrice * 1.20
  else
    Result := 0;
end;

function TPieceColorInfo.EvaluatePriceNewAvg: double;
begin
  if not fhasloaded then
    Load;
  if fpriceguide.nTimesSold > 0 then
    Result := fpriceguide.nAvgPrice
  else if fpriceguide.uTimesSold > 0 then
    Result := fpriceguide.uAvgPrice * 1.20
  else if favailability.nTotalLots > 0 then
    Result := favailability.nAvgPrice
  else if favailability.uTotalLots > 0 then
    Result := favailability.uAvgPrice * 1.20
  else
    Result := 0;
end;

function TPieceColorInfo.EvaluatePriceUsed: double;
begin
  if not fhasloaded then
    Load;
  if fpriceguide.uTimesSold > 0 then
    Result := fpriceguide.uQtyAvgPrice
  else if favailability.uTotalLots > 0 then
    Result := favailability.uQtyAvgPrice
  else if fpriceguide.nTimesSold > 0 then
    Result := fpriceguide.nQtyAvgPrice / 1.20
  else if favailability.nTotalLots > 0 then
    Result := favailability.nQtyAvgPrice / 1.20
  else
    Result := 0;
end;

function TPieceColorInfo.EvaluatePriceUsedAvg: double;
begin
  if not fhasloaded then
    Load;
  if fpriceguide.uTimesSold > 0 then
    Result := fpriceguide.uAvgPrice
  else if favailability.uTotalLots > 0 then
    Result := favailability.uAvgPrice
  else if fpriceguide.nTimesSold > 0 then
    Result := fpriceguide.nAvgPrice / 1.20
  else if favailability.nTotalLots > 0 then
    Result := favailability.nAvgPrice / 1.20
  else
    Result := 0;
end;

function TPieceColorInfo.dbExportStringPG: string;
begin
  if not fhasloaded then
    Load;

  Result :=
    Format('%s;%d;%s;%d;%d;%d;%2.5f;%2.5f;%2.5f;%2.5f;%d;%d;%2.5f;%2.5f;%2.5f;%2.5f'+
           ';%d;%d;%2.5f;%2.5f;%2.5f;%2.5f;%d;%d;%2.5f;%2.5f;%2.5f;%2.5f;%2.5f;%2.5f', [
        fpiece,
        fcolor,
        db.BrickLinkPart(fpiece),
        db.Colors(fcolor).BrickLingColor,
        fpriceguide.nTimesSold,
        fpriceguide.nTotalQty,
        fpriceguide.nMinPrice,
        fpriceguide.nAvgPrice,
        fpriceguide.nQtyAvgPrice,
        fpriceguide.nMaxPrice,
        fpriceguide.uTimesSold,
        fpriceguide.uTotalQty,
        fpriceguide.uMinPrice,
        fpriceguide.uAvgPrice,
        fpriceguide.uQtyAvgPrice,
        fpriceguide.uMaxPrice,
        favailability.nTotalLots,
        favailability.nTotalQty,
        favailability.nMinPrice,
        favailability.nAvgPrice,
        favailability.nQtyAvgPrice,
        favailability.nMaxPrice,
        favailability.uTotalLots,
        favailability.uTotalQty,
        favailability.uMinPrice,
        favailability.uAvgPrice,
        favailability.uQtyAvgPrice,
        favailability.uMaxPrice,
        EvaluatePriceNew,
        EvaluatePriceUsed
      ]);
end;

{$IFNDEF CRAWLER}
function TPieceColorInfo.dbExportStringDB: string;
var
  pi: TPieceInfo;
  si: TSetExtraInfo;
  weight: double;
  idx: integer;
  desc: string;
  customstr: string;
  invstr: string;
  ayear: integer;
begin
  if not fhasloaded then
    Load;

  pi := db.PieceInfo(self);
  weight := 0;
  customstr := '0';
  invstr := '0';
  ayear := 0;
  if fcolor = INSTRUCTIONCOLORINDEX then
  begin
    idx := db.Sets.IndexOf(fpiece);
    if idx >= 0 then
    begin
      si := db.Sets.Objects[idx] as TSetExtraInfo;
      weight := si.instructionsweight;
      ayear := si.year;
    end;
    desc := 'Instructions for ' + db.SetDesc(fpiece);
  end
  else if fcolor = BOXCOLORINDEX then
  begin
    idx := db.AllSets.IndexOf(fpiece);
    if idx >= 0 then
    begin
      si := db.Sets.Objects[idx] as TSetExtraInfo;
      weight := si.originalboxweight;
      ayear := si.year;
    end;
    desc := 'Original box for ' + db.SetDesc(fpiece);
  end
  else
  begin
    weight := pi.weight;
    if fcolor = -1 then
    begin
      if db.GetSetInventory(fpiece) <> nil then
        invstr := '1';
    end
    else if pi.hasinventory then
      invstr := '1';
    if (fcolor = -1) and (db.AllSets.IndexOf(fpiece) >= 0) then
    begin
      desc := db.SetDesc(fpiece);
      ayear := db.SetYear(fpiece);
      if db.IsMoc(fpiece) then
        customstr := '1';
    end
    else
    begin
      desc := db.PieceDesc(fpiece);
      ayear := fyear;
      if pi.category = CATEGORYCUSTOMMINIFIGS then
        customstr := '1';
    end;
  end;
  desc := Trim(StringReplace(desc, '"', ' ', [rfReplaceAll]));

  Result :=
    Format('%s;%d;%s;"%s";"%s";"%s";%d;%d;"%s";%s;%d;%2.5f;"%s"', [
        ItemType,
        pi.category,
        customstr,
        fpiece,
        db.RebrickablePart(fpiece),
        db.GetBLNetPieceName(fpiece),
        fcolor,
        db.Colors(fcolor).BrickLingColor,
        fcode,
        invstr,
        ayear,
        weight,
        desc
      ]);
end;
{$ENDIF}

function F_nDemand(const favailability: availability_t; const fpriceguide: priceguide_t): double;
begin
  if favailability.nTotalQty = 0 then
  begin
    Result := fpriceguide.nTotalQty + 1.0;
    if Result > 10.0 then
      Result := 10.0;
    Exit;
  end;

  Result := fpriceguide.nTotalQty / favailability.nTotalQty;
  if Result > 10.0 then
    Result := 10.0;
end;
                                     
function TPieceColorInfo.nDemand: double;
begin
  if not fhasloaded then
    Load;

  Result := F_nDemand(favailability, fpriceguide);
end;

function F_uDemand(const favailability: availability_t; const fpriceguide: priceguide_t): double;
begin
  if favailability.uTotalQty = 0 then
  begin
    Result := fpriceguide.uTotalQty + 1.0;
    if Result > 10.0 then
      Result := 10.0;
    Exit;
  end;

  Result := fpriceguide.uTotalQty / favailability.uTotalQty;
  if Result > 10.0 then
    Result := 10.0;
end;

function TPieceColorInfo.uDemand: double;
begin
  if not fhasloaded then
    Load;

  Result := F_uDemand(favailability, fpriceguide);
end;

function iscardboard(const pci: TPieceColorInfo): Boolean;
var
  pi: TPieceInfo;
begin
  if pci <> nil then
  begin
    pi := TPieceInfo(pci.pieceinfo);
    if pi <> nil then
      if pi.category = 1060 then
      begin
        Result := True;
        Exit;
      end;
  end;

  Result := False;
end;


function TPieceColorInfo.ItemType: string;
begin
  if fsparttype in ['P', 'S', 'M', 'C', 'B', 'I', 'O', 'G'] then
    Result := fsparttype
  else if (fparttype = TYPE_CATALOG) or (fcolor = CATALOGCOLORINDEX) then
    Result := 'C'
  else if fparttype = TYPE_SET then
    Result := 'S'
  else if fparttype = TYPE_MINIFIGURE then
    Result := 'M'
  else if fparttype = TYPE_BOOK then
    Result := 'B'
  else if fparttype = TYPE_GEAR then
    Result := 'G'
  else if (fparttype = TYPE_INSTRUCTIONS) or (fcolor = INSTRUCTIONCOLORINDEX) then
    Result := 'I'
  else if (fparttype = TYPE_BOX) or (fcolor = BOXCOLORINDEX) then
    Result := 'O'
  else if Pos('-', fpiece) > 0 then
    Result := 'S'
  else if issticker(fpiece) or iscardboard(self) then
    Result := 'P'
  else if fcolor = -1 then
    Result := 'M'
  else
    Result := 'P';
end;

function TSetsDatabase.LoadFromDisk(const fname: string): boolean;
var
  s: TStringList;
  kp: THashStringList;
  i, j, tot: integer;
  sset, snum, spiece, scolor, stype: string[255];
  sitemcheck: string[255];
  sc, sp: string;
  npieces: integer;
  cc: integer;
  pci: TPieceColorInfo;
  idx: integer;
  k, len: integer;
  ss: string;
  prosets: TStringList;
  lastignoredset: string;
  sout: TStringList;
  lsets1, lsets2: TStringList;
  lparts1, lparts2: TStringList;
  stmp: string;
  oldname, newname: string;
begin
  AllowInternetAccess := False;
  fcurrencies := TCurrency.Create(basedefault + 'db\db_currency.txt');
  fcurrencyconvert := TCurrencyConvert.Create('eur', basedefault + 'db\db_currencyconvert.txt');

{  fallsets.Sorted := True;
  fallsetswithoutextra.Sorted := True;}

  InitSetReferences;
  LoadKnownPieces;

  s := TStringList.Create;
  S_LoadFromFile(s, fname);
  if s.Count = 0 then
  begin
    s.Free;
    Result := False;
    AllowInternetAccess := True;
    Exit;
  end;

  if s.Strings[0] <> 'set_id,piece_id,num,color,type' then
    if s.Strings[0] <> 'set_num,part_num,quantity,color_id,type' then
    begin
      s.Free;
      Result := False;
      AllowInternetAccess := True;
      Exit;
    end;

  sout := TStringList.Create;
  sout.Add('set_id,piece_id,num,color,type');

  if Assigned(progressfunc) then
    progressfunc('Loading database...', 0.0);

  prosets := TStringList.Create;
  prosets.AddStrings(fallsets);
  prosets.Sorted := True;
  lastignoredset := '';

  s.Delete(0);
  s.Sort;
  for i := 0 to s.Count - 1 do
  begin
    if i mod 500 = 0 then
      if Assigned(progressfunc) then
        progressfunc('Loading database...', i / s.Count);
      ss := s.strings[i];
      len := Length(ss);
      sset := '';

      k := 0;

      while k < len do
      begin
        Inc(k);
        if ss[k] <> ',' then
          sset := sset + ss[k]
        else
          Break;
      end;

      if sset = lastignoredset then
        Continue;

      if prosets.IndexOf(sset) >= 0 then
      begin
        lastignoredset := sset;
        Continue;
      end;

      sout.Add(ss);

      spiece := '';
      snum := '';
      scolor := '';
      stype := '';

      while k < len do
      begin
        Inc(k);
        if ss[k] <> ',' then
          spiece := spiece + ss[k]
        else
          Break;
      end;
      spiece := RebrickablePart(spiece);
      if spiece = '6141' then
        spiece := '4073'
      else if Pos('Mx', spiece) = 1 then
        spiece[1] := 'm'
      else
        spiece := fixpartname(spiece);

      while k < len do
      begin
        Inc(k);
        if ss[k] <> ',' then
          snum := snum + ss[k]
        else
          Break;
      end;

      while k < len do
      begin
        Inc(k);
        if ss[k] <> ',' then
          scolor := scolor + ss[k]
        else
          Break;
      end;

      while k < len do
      begin
        Inc(k);
        if ss[k] <> ',' then
          stype := stype + ss[k]
        else
          Break;
      end;


    cc := StrToIntDef(scolor, 0);
    npieces := StrToIntDef(snum, 0);
    if (cc >= -1) and (cc <= MAXINFOCOLOR) then
    begin
      if fcolors[cc].knownpieces = nil then
      begin
        fcolors[cc].knownpieces := ThashStringList.Create;
        idx := -1;
      end
      else
        idx := fcolors[cc].knownpieces.Indexof(spiece);
      if idx < 0 then
      begin
        pci := TPieceColorInfo.Create(spiece, cc);
        fcolors[cc].knownpieces.AddObject(spiece, pci);
      end
      else
        pci := fcolors[cc].knownpieces.Objects[idx] as TPieceColorInfo;
      AddSetPiece(sset, spiece, stype, cc, npieces, pci);
      pci.AddSetReference(sset, npieces);
    end
    else
      AddSetPiece(sset, spiece, stype, cc, npieces); // ? remove ?
  end;

  if Assigned(progressfunc) then
    progressfunc('Loading database...', 1.0);

  if sout.Count <> s.Count + 1 then
  begin
    S_BackupFile(fname);
    S_SaveToFile(sout, fname);
  end;
  sout.Free;
  s.Free;

  for i := -1 to MAXINFOCOLOR - 1 do
  begin
    kp := fcolors[i].knownpieces;
    if kp <> nil then
    begin
      if i = -1 then
      begin
        for j := 0 to kp.Count - 1 do
//          if not IsMoc(kp.Strings[j]) then
            fcolorpieces.AddObject('-1,' + kp.Strings[j], kp.Objects[j]);
      end
      else
      begin
        scolor := IntToStr(i) + ',';
        for j := 0 to kp.Count - 1 do
          fcolorpieces.AddObject(scolor + kp.Strings[j], kp.Objects[j]);
      end;
    end;
  end;

  prosets.Free;

  fcolorpieces.Sorted := True;

  kp := fcolors[MAXINFOCOLOR].knownpieces;
  if kp <> nil then
  begin
    for j := 0 to kp.Count - 1 do
    begin
      sitemcheck := kp.Strings[j];
      if fcolorpieces.IndexOf('-1,' + sitemcheck) < 0 then
        fcolorpieces.AddObject('9999,' + sitemcheck, kp.Objects[j]);
    end;
  end;

  if fexists(basedefault + 'cache\' + fcrawlerfilename) then
  begin
    S_LoadFromFile(fcrawlerpriority, basedefault + 'cache\' + fcrawlerfilename);

    for i := 0 to fcrawlerpriority.Count - 1 do
    begin
      splitstring(fcrawlerpriority.Strings[i], sc, sp, ',');
      spiece := RebrickablePart(sp);
      if spiece = '6141' then
        spiece := '4073'
      else if Pos('Mx', spiece) = 1 then
        spiece[1] := 'm'
      else
        spiece := fixpartname(spiece);

      cc := StrToIntDef(sc, 0);
      if (cc >= -1) and (cc <= MAXINFOCOLOR) then
      begin
        if fcolors[cc].knownpieces = nil then
          fcolors[cc].knownpieces := THashStringList.Create;
        idx := fcolors[cc].knownpieces.Indexof(spiece);
        if idx < 0 then
        begin
          pci := TPieceColorInfo.Create(spiece, cc);
          fcolors[cc].knownpieces.AddObject(spiece, pci);
        end;
      end;
    end;
  end
  else
  begin
    lparts1 := TStringList.Create;
    lparts2 := TStringList.Create;
    try
      for i := 0 to fcolorpieces.Count - 1 do
        if Pos('-1,', fcolorpieces.Strings[i]) <> 1 then
        begin
          oldname := secondword(fcolorpieces.strings[i], ',');
          newname := GetNewPieceName(oldname);
          if (newname = oldname) or (newname = '') then
            lparts1.Add(fcolorpieces.Strings[i])
          else
            lparts2.Add(fcolorpieces.Strings[i]);
        end;
      fcrawlerpriority.AddStrings(lparts1);
      fcrawlerpriority.AddStrings(lparts2);
    finally
      lparts1.Free;
      lparts2.Free;
    end;
    lsets1 := TStringList.Create;
    lsets2 := TStringList.Create;
    try
      for i := 0 to fcolorpieces.Count - 1 do
      begin
        stmp := fcolorpieces.Strings[i];
        if Pos('-1,', stmp) = 1 then
        begin
          if (Pos('-1,sw', stmp) = 1) and (CountNumbers(stmp) = 4) then
            lsets1.Add(stmp)
          else if (Pos('-1,cty', stmp) = 1) and (CountNumbers(stmp) = 4) then
            lsets1.Add(stmp)
          else
            lsets2.Add(stmp);
        end;
      end;
      fcrawlerpriority.AddStrings(lsets1);
      fcrawlerpriority.AddStrings(lsets2);
    finally
      lsets1.Free;
      lsets2.Free;
    end;
  end;

//  fpciloader.Activate(@fpciloaderparams);

  if Assigned(progressfunc) then
    progressfunc('Loading cache...', 0.0);
  tot := 0;
  for i := -1 to MAXINFOCOLOR do
    if (fcolors[i].id = i) or (i = -1) then
      if fcolors[i].knownpieces <> nil then
        tot := tot + fcolors[i].knownpieces.Count;

  k := 0;
  for i := -1 to MAXINFOCOLOR do
    if (fcolors[i].id = i) or (i = -1) then
    begin
      kp := fcolors[i].knownpieces;
      if kp <> nil then
      begin
        for j := 0 to kp.Count - 1 do
        begin
          if Assigned(progressfunc) then
            if j mod 1000 = 999 then
              progressfunc('Loading cache...', (k + j) / tot);
          (kp.Objects[j] as TPieceColorInfo).Load;
{          if not (kp.Objects[j] as TPieceColorInfo).cacheread then
            (kp.Objects[j] as TPieceColorInfo).DoSaveToDisk;}
        end;
        inc(k, kp.Count);
        if Assigned(progressfunc) then
          progressfunc('Loading cache...', k / tot);
      end;
    end;
  if Assigned(progressfunc) then
    progressfunc('Loading cache...', 1.0);

  LoadPieceCodes;

  {$IFNDEF CRAWLER}
  LoadStorage;
  {$ENDIF}

  {$IFNDEF CRAWLER}
  RefreshAllSetsYears;
  {$ENDIF}

  RefreshAllSetsAssets;
  InitPartTypes;

  {$IFNDEF CRAWLER}
  InitYearTable;
  InitBaseMolds;
  InitRelationShips;
  {$ENDIF}

  Result := True;
  AllowInternetAccess := True;
  floaded := True;
end;

var
  cacheidx: Integer = -1;
  cacheidx2: Integer = -1;

procedure TSetsDatabase.AddSetPiece(const setid: string; const part: string;
  const typ: string; const color: integer; const num: integer; const pci: TObject = nil);
var
  idx: integer;
  inv: TBrickInventory;
begin
  if (cacheidx >= 0) and (cacheidx < fallsets.Count) and (fallsets.Strings[cacheidx] = setid) then
    inv := fallsets.Objects[cacheidx] as TBrickInventory
  else
  begin
    idx := fallsets.IndexOf(setid);
    if idx < 0 then
    begin
      inv := TBrickInventory.Create;
      idx := fallsets.AddObject(setid, inv);
    end
    else
      inv := fallsets.Objects[idx] as TBrickInventory;
    cacheidx := idx;
  end;
  if fcolors[color].knownpieces = nil then
    fcolors[color].knownpieces := THashStringList.Create;
  inv.AddLoosePartFast(part, color, num, pci);

  if typ <> '1' then
    Exit;

  if (cacheidx2 >= 0) and (cacheidx2 < fallsetswithoutextra.Count) and (fallsetswithoutextra.Strings[cacheidx2] = setid) then
    inv := fallsetswithoutextra.Objects[cacheidx2] as TBrickInventory
  else
  begin
    idx := fallsetswithoutextra.IndexOf(setid);
    if idx < 0 then
    begin
      inv := TBrickInventory.Create;
      idx := fallsetswithoutextra.AddObject(setid, inv);
    end
    else
      inv := fallsetswithoutextra.Objects[idx] as TBrickInventory;
    cacheidx2 := idx;
  end;
  inv.AddLoosePartFast(part, color, num, pci);
end;

function TSetsDatabase.GetSetInventory(const setid: string): TBrickInventory;
var
  idx: integer;
begin
  idx := fallsets.IndexOf(setid);
  if idx < 0 then
  begin
    if fexists(basedefault + 'db\sets\' + setid + '.txt') then
    begin
      idx := fallsets.Add(setid);
      fallsets.Objects[idx] := TBrickInventory.Create;
      (fallsets.Objects[idx] as TBrickInventory).LoadLooseParts(basedefault + 'db\sets\' + setid + '.txt');
      Result := (fallsets.Objects[idx] as TBrickInventory);
    end
    else if fexists(basedefault + 'mosaic\' + setid + '.txt') then
    begin
      idx := fallsets.Add(setid);
      fallsets.Objects[idx] := TBrickInventory.Create;
      (fallsets.Objects[idx] as TBrickInventory).LoadLooseParts(basedefault + 'mosaic\' + setid + '.txt');
      Result := (fallsets.Objects[idx] as TBrickInventory);
    end
    else if fexists(basedefault + 'out\' + setid + '\' + setid + '.txt') then
    begin
      idx := fallsets.Add(setid);
      fallsets.Objects[idx] := TBrickInventory.Create;
      (fallsets.Objects[idx] as TBrickInventory).LoadLooseParts(basedefault + 'out\' + setid + '\' + setid + '.txt');
      Result := (fallsets.Objects[idx] as TBrickInventory);
    end
    else
      Result := nil;
    Exit;
  end
  else if Pos('mosaic_', setid) = 1 then
  begin
    if fexists(basedefault + 'mosaic\' + setid + '.txt') then
    begin
      (fallsets.Objects[idx] as TBrickInventory).Clear;
      (fallsets.Objects[idx] as TBrickInventory).LoadLooseParts(basedefault + 'mosaic\' + setid + '.txt');
      Result := (fallsets.Objects[idx] as TBrickInventory);
      Exit;
    end;
  end;

  if idx >= 0 then
    Result := fallsets.Objects[idx] as TBrickInventory
  else
    Result := nil;
end;

function TSetsDatabase.GetSetInventoryWithOutExtra(const setid: string): TBrickInventory;
var
  idx: integer;
begin
  idx := fallsetswithoutextra.IndexOf(setid);
  if idx < 0 then
  begin
    if fexists(basedefault + 'db\sets\' + setid + '.txt') then
    begin
      idx := fallsetswithoutextra.Add(setid);
      fallsetswithoutextra.Objects[idx] := TBrickInventory.Create;
      (fallsetswithoutextra.Objects[idx] as TBrickInventory).LoadLooseParts(basedefault + 'db\sets\' + setid + '.txt');
      Result := (fallsetswithoutextra.Objects[idx] as TBrickInventory);
    end
    else
      Result := nil;
    Exit;
  end;
  Result := fallsetswithoutextra.Objects[idx] as TBrickInventory;
end;

procedure TSetsDatabase.ReloadCache;
var
  i, j: integer;
  k, mx: integer;
  kp: THashStringList;
  progressstring: string;
begin
  progressstring := 'Reloading cache...';
  if assigned(progressfunc) then
    progressfunc(progressstring, 0.0);

  mx := 0;
  for i := -1 to MAXINFOCOLOR do
    if (fcolors[i].id = i) or (i = -1) then
      if fcolors[i].knownpieces <> nil then
        inc(mx, fcolors[i].knownpieces.Count);

  st_pciloads := 0;
  st_pciloadscache := 0;
  fCacheDB.Free;
  fCacheDB := TCacheDB.Create(basedefault + 'cache\cache.db');
  k := 0;
  for i := -1 to MAXINFOCOLOR do
  begin
    if (fcolors[i].id = i) or (i = -1) then
      if fcolors[i].knownpieces <> nil then
      begin
        kp := fcolors[i].knownpieces;
        for j := 0 to kp.Count - 1 do
        begin
          if j mod 500 = 0 then
            if assigned(progressfunc) then
              progressfunc(progressstring, (k + j) / mx);
          (kp.Objects[j] as TPieceColorInfo).Load;
        end;
        inc(k, kp.Count);
        if assigned(progressfunc) then
          progressfunc(progressstring, k / mx);
      end;
  end;

 if assigned(progressfunc) then
   progressfunc(progressstring, 1.0);
end;

{$IFNDEF CRAWLER}
function TSetsDatabase.GetUnknownPiecesFromCache: TStringList;
var
  i: integer;
  pitem: cachedbitem_p;
  spart: string;
  sdesc: string;
  pi: TPieceInfo;
  pci: TPieceColorInfo;
begin
  Result := TStringList.Create;
  Result.Add('Part,Color,Desc');
  ReloadCache;
  for i := 0 to CACHEDBHASHSIZE - 1 do
  begin
    pitem := @cachedb.parecs[i];
    spart := Trim(cachedb.apart(pitem));
    if spart <> '' then
    begin
      pi := PieceInfo(spart);
      if (pi = nil) or (pi = fstubpieceinfo) then
      begin
        sdesc := SetDesc(spart);
        if sdesc = '' then
          sdesc := spart;
        Result.Add(spart + ',' + itoa(pitem.color) + ',' + sdesc)
      end
      else
      begin
        pci := PieceColorInfo(spart, pitem.color);
        if pci = nil then
          Result.Add(spart + ',' + itoa(pitem.color) + ',' + pi.desc)
        else
          pci.pieceinfo := pi;
      end;
    end;
  end;
end;
{$ENDIF}

{$IFNDEF CRAWLER}
function TSetsDatabase.RemoveUnknownPiecesFromCache: integer;
var
  i: integer;
  pitem: cachedbitem_p;
  spart: string;
  pi: TPieceInfo;
  pci: TPieceColorInfo;
begin
  Result := 0;
  ReloadCache;
  for i := 0 to CACHEDBHASHSIZE - 1 do
  begin
    pitem := @cachedb.parecs[i];
    spart := Trim(cachedb.apart(pitem));
    if spart <> '' then
    begin
      pi := PieceInfo(spart);
      if (pi = nil) or (pi = fstubpieceinfo) then
      begin
        ZeroMemory(@cachedb.parecs[i], SizeOf(cachedbitem_t));
        inc(Result);
      end
      else
      begin
        pci := PieceColorInfo(spart, pitem.color);
        if pci = nil then
        begin
          ZeroMemory(@cachedb.parecs[i], SizeOf(cachedbitem_t));
          inc(Result);
        end
        else
          pci.pieceinfo := pi;
      end;
    end;
  end;
  cachedb.FlashAll;
end;
{$ENDIF}

procedure TSetsDatabase.GetCacheHashEfficiency(var hits, total: integer);
var
  i, j: integer;
  k, mx: integer;
  pci: TPieceColorInfo;
  A: PIntegerArray;
  kp: THashStringList;
begin
  GetMem(A, CACHEDBHASHSIZE * SizeOf(integer));

  if assigned(progressfunc) then
    progressfunc('Calculating...', 0.0);

  MT_ZeroMemory(A, CACHEDBHASHSIZE * SizeOf(integer));
  hits := 0;
  total := 0;

  mx := 0;
  for i := -1 to MAXINFOCOLOR do
    if (fcolors[i].id = i) or (i = -1) then
      inc(mx);

  if mx > 0 then
  begin
    k := 0;
    for i := -1 to MAXINFOCOLOR do
    begin
      if (fcolors[i].id = i) or (i = -1) then
      begin
        inc(k);
        if assigned(progressfunc) then
          progressfunc('Calculating...', k / mx);
        if fcolors[i].knownpieces <> nil then
        begin
          kp := fcolors[i].knownpieces;
          for j := 0 to kp.Count - 1 do
          begin
            pci := kp.Objects[j] as TPieceColorInfo;
            inc(A[MkPCIHash(pci.piece, pci.color)]);
            inc(total);
          end;
        end;
      end;
    end;
  end;

  for i := 0 to CACHEDBHASHSIZE - 1 do
    if A[i] > 0 then
      inc(hits);

 if assigned(progressfunc) then
   progressfunc('Calculating...', 1.0);

  FreeMem(A, CACHEDBHASHSIZE * SizeOf(integer));
end;

procedure TSetsDatabase.InitPartTypes;
var
  sl: TStringList;
  fname: string;
  s1, s2, s3: string;
  spiece: string;
  scolor: string;
  ncolor: integer;
  i: integer;
  pci: TPieceColorInfo;
  progressstring: string;
{$IFNDEF CRAWLER}
  spci: TStringList;
  needsave: boolean;
{$ENDIF}
begin
  fname := basedefault + 'db\db_parttypes.txt';
  if not fexists(fname) then
    Exit;

  progressstring := 'Initializing part types...';

  sl := TStringList.Create;
{$IFNDEF CRAWLER}
  spci := TStringList.Create;
{$ENDIF}  
  try
    S_LoadFromFile(sl, fname);
    if sl.Count > 0 then
      if Trim(sl.Strings[0]) = 'Part,Color,Type' then
      begin
        if Assigned(progressfunc) then
          progressfunc(progressstring, 0.0);
        for i := 1 to sl.Count - 1 do
        begin
          if i mod 500 = 0 then
            if Assigned(progressfunc) then
              progressfunc(progressstring, i / sl.Count);

          splitstring(sl.Strings[i], s1, s2, s3, ',');

          if (Length(s3) = 1) and (s3[1] in ['P', 'S', 'M', 'C', 'B', 'I', 'O', 'G']) then
          begin
            if Pos('BL ', s1) = 1 then
              spiece := RebrickablePart(Trim(Copy(s1, 4, Length(s1) - 3)))
            else
              spiece := RebrickablePart(Trim(s1));

            if Pos('BL', s2) = 1 then
            begin
              scolor := Trim(Copy(s2, 3, Length(s2) - 2));
              ncolor := BrickLinkColorToSystemColor(StrToIntDef(scolor, 0))
            end
            else if Pos('RB', s2) = 1 then
            begin
              scolor := Trim(Copy(s2, 3, Length(s2) - 2));
              ncolor := RebrickableColorToSystemColor(StrToIntDef(scolor, 0))
            end
            else
            begin
              scolor := Trim(s2);
              ncolor := StrToIntDef(scolor, 0);
            end;

            pci := PieceColorInfo(spiece, ncolor);
            if pci <> nil then
            begin
              pci.sparttype := s3[1];
              {$IFNDEF CRAWLER}
              spci.Add(itoa(integer(pci)));
              {$ENDIF}
            end;
          end;
        end;
        if Assigned(progressfunc) then
          progressfunc(progressstring, 1.0);
      end;
  finally
    sl.Free;
  end;
{$IFNDEF CRAWLER}
  spci.Sort;
  needsave := False;
  for i := spci.Count - 1 downto 1 do
    if spci.Strings[i] = spci.Strings[i - 1] then
    begin
      spci.Delete(i);
      needsave := True;
    end;
  if needsave then
  begin
    sl := TStringList.Create;
    try
      sl.Add('Part,Color,Type');
      for i := 0 to spci.Count - 1 do
      begin
        pci := TPieceColorInfo(TObject(atoi(spci.Strings[i])));
        if pci.sparttype in ['P', 'S', 'M', 'C', 'B', 'I', 'O', 'G'] then
          sl.Add(pci.piece + ',' + pci.fcolorstr + ',' + pci.sparttype);
      end;
      S_BackupFile(fname);
      S_SaveToFile(sl, fname);
    finally
      sl.Free;
    end;
  end;
  spci.Free;
{$ENDIF}
end;

procedure TSetsDatabase.FlashPartTypes;
var
  sL: TStringList;
  fname: string;
  fexisted: boolean;
begin
  if fPartTypeList.Count = 0 then
    Exit;

  sl := TStringList.Create;
  try
    fname := basedefault + 'db\db_parttypes.txt';
    fexisted := fexists(fname);
    if fexisted and (fPartTypeList.Count = 1) then
    begin
      S_BackupFile(fname);
      S_AppendLineToFile(fname, fPartTypeList.Strings[0]);
    end
    else
    begin
      if not fexisted then
        sl.Add('Part,Color,Type')
      else
      begin
        S_BackupFile(fname);
        S_LoadFromFile(sl, fname);
        S_FirstLine(sl, 'Part,Color,Type');
      end;
      sl.AddStrings(fPartTypeList);
      S_SaveToFile(sl, fname);
    end;
    fPartTypeList.Clear;
  finally
    sl.Free;
  end;
end;

function TSetsDatabase.SetPartType(const pci: TPieceColorInfo; const pt: char = ' '): boolean;
var
  fname: string;
  ss: string;
begin
  Result := False;

  if not (pt in ['P', 'S', 'M', 'C', 'B', 'I', 'O', 'G', ' ']) then
    Exit;

  if pci <> nil then
  begin
    if pci.sparttype <> pt then
    begin
      pci.sparttype := pt;
      fname := basedefault + 'db\db_parttypes.txt';
      if not fexists(fname) then
        S_AppendLineToFile(fname, 'Part,Color,Type');
      ss := pci.piece + ',' + itoa(pci.color) + ',' + pt;
      fPartTypeList.Add(ss);
      if fPartTypeList.Count >= 100 then
        FlashPartTypes;
      Result := True;
    end;
  end;
end;

function TSetsDatabase.SetPartType(const pcs: string; const cl: integer; const pt: char = ' '): boolean;
var
  pci: TPieceColorInfo;
  fname: string;
  ss: string;
begin
  Result := False;

  if not (pt in ['P', 'S', 'M', 'C', 'B', 'I', 'O', 'G', ' ']) then
    Exit;

  pci := PieceColorInfo(pcs, cl);
  if pci <> nil then
  begin
    if pci.sparttype <> pt then
    begin
      pci.sparttype := pt;
      fname := basedefault + 'db\db_parttypes.txt';
      if not fexists(fname) then
        S_AppendLineToFile(fname, 'Part,Color,Type');
      ss := pcs + ',' + itoa(cl) + ',' + pt;
      fPartTypeList.Add(ss);
      if fPartTypeList.Count >= 100 then
        FlashPartTypes;
      Result := True;
    end;
  end;
end;

function TSetsDatabase.SetPartTypeIndirect(const pcs: string; const cl: integer; const pt: char): boolean;
var
  pci: TPieceColorInfo;
  fname: string;
  ss: string;
  check: string;
  idx: integer;
  sl: TStringList;
begin
  Result := False;

  if not (pt in ['P', 'S', 'M', 'C', 'B', 'I', 'O', 'G', ' ']) then
    Exit;

  pci := PieceColorInfo(pcs, cl);
  if pci <> nil then
  begin
    fname := basedefault + 'db\db_parttypes.txt';
    if not fexists(fname) then
      S_AppendLineToFile(fname, 'Part,Color,Type');
    sl := TStringList.Create;
    S_LoadFromFile(sl, fname);
    check := pcs + ',' + itoa(cl) + ',' + pci.sparttype;
    idx := sl.IndexOf(check);
    if idx > 0 then
      sl.Delete(idx);

    pci.sparttype := pt;
    ss := pcs + ',' + itoa(cl) + ',' + pt;
    sl.Add(ss);
    S_FirstLine(sl, 'Part,Color,Type');
    S_SaveToFile(sl, fname);
    sl.Free;

    Result := True;
  end;
end;

function PieceColorCacheDir(const piece, color: string): string;
begin
  if color = '-1' then
    Result := basedefault + 'cache\9999\'
  else
    Result := basedefault + 'cache\' + color + '\' ;
end;

function PieceColorCacheFName(const piece, color: string): string;
begin
  Result := PieceColorCacheDir(piece, color) +  piece + '.cache';
end;

function PieceColorCacheFName2(const piece, color: string): string;
begin
  Result := PieceColorCacheDir(piece, color) +  piece + '.cache2';
end;

function PieceColorCacheFName3(const piece, color: string): string;
begin
  Result := PieceColorCacheDir(piece, color) +  piece + '.cache3';
end;

initialization
  basedefault := ParamStr(0);
  if Pos('\', basedefault) > 0 then
  begin
    while basedefault[Length(basedefault)] <> '\' do
      SetLength(basedefault, Length(basedefault) - 1);
  end
  else
    basedefault := '';

  ChDir(basedefault);

end.

