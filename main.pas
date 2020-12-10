unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, OleCtrls, SHDocVw, Readhtml, FramView, Htmlview, ExtCtrls,
  StdCtrls, Buttons, bi_docwriter, bi_db, bi_hash, bi_orders, Menus, jpeg,
  pngimage;

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
    N3: TMenuItem;
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
    N7: TMenuItem;
    Missingfordismandaledsets1: TMenuItem;
    Usedsetstobuiltfromscratch1: TMenuItem;
    N8: TMenuItem;
    Weightqueries1: TMenuItem;
    Batchlink1: TMenuItem;
    S1: TMenuItem;
    Reloadcache1: TMenuItem;
    CheckCacheHashEfficiency1: TMenuItem;
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
  private
    { Private declarations }
    streams: TStringList;
    initialized: boolean;
    document: TDocument;
    entries: TStringList;
    entriesHash: THashTable;
    goback, gofwd: TStringList;
    progress_string: string;
    Missingformultiplesetslist: TStringList;
    Missingfordismandaledsetslist: TStringList;
    orders: TOrders;
    dodraworderinfo: boolean;
    dismantaledsetsinv: TBrickInventory;
    procedure dbloadprogress(const s: string; d : Double);
    procedure ShowLooseParts(inv: TBrickInventory; colormask: Integer = -1; partmask: string = ''; cat: Integer = -1);
    procedure ShowSetInventory(const setid: string);
    procedure ShowColors;
    procedure ShowCategoryColors(const cat: integer);
    procedure ShowCategories;
    procedure ShowPiece(pcs: string);
    function GetAPieceColor(pcs: string): integer;
    procedure DrawPieceList(const tit: string; const lst: TStringList);
    procedure DrawPieceListLugbulk(const tit: string; const lst: TStringList);
    procedure UsedPiecesbeloweuroKgr(const x: integer);
    procedure ShowColorPiece(const pcs: string; const color: integer);
    procedure ShowSetsICanBuild(const pct: double);
    procedure ShowSetsAtYear(const year: integer);
    procedure ShowSetsForPartOutNew;
    procedure ShowSetsForPartOutUsed;
    procedure ShowSetsForPartInUsed(const posost: integer);
    procedure ShowMissingFromStorageBins;
    procedure ShowMissingToBuildSetInventory(const setid: string; const numsets: integer);
    procedure ShowMissingToBuilMultipledSets(const setids: TStringList);
    procedure ShowLugbulkSuggestions(const years: string; const demand, sold: integer; const price: double);
    procedure ShowCompare2Sets(const set1, set2: string);
    procedure ShowOrders(const seller: string = '');
    procedure ShowOrder(const orderid: string);
    procedure DrawOrderInf(const orderid: string);
    procedure ShowStorageBins;
    procedure ShowStorageInventory(const st: string);
    procedure ShowHomePage;
    procedure ShowInventorySets(const inv: TBrickInventory; const header_flash: boolean);
    procedure ShowMySets;
    procedure ShowMySetsPieces;
    procedure DrawNavigateBar;
    procedure DrawHeadLine(const s: string);
    procedure DrawPartOutValue(inv: TBrickInventory; const setid: string = '');
    procedure DrawInventoryTable(inv: TBrickInventory; const lite: Boolean = false; const setid: string = '');
    procedure DrawBrickOrderInfo(const brick: brickpool_p);
    procedure UpdateDismantaledsetsinv;
    procedure DrawPriceguide(const part: string; const color: integer = -1);
    procedure HTMLClick(const SRC: String; var Handled: Boolean);
    procedure StoreInventoryStatsRec(const piece: string; const color: string = '');
  public
    { Public declarations }
    activebits: integer;
  end;

var
  MainForm: TMainForm;

implementation

uses
  bi_delphi, bi_pak, bi_io, bi_system, bi_tmp, slpash, bi_utils, timing,
  searchset, searchpart, frm_multiplesets, PreviewForm, bl_orderxml,
  compare2sets, mosaicfrm, editpiecefrm, strutils, frm_diagrams,
  frm_lugbulksuggest, frm_selectsets, bi_script, frm_batch;

{$R *.dfm}

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
begin
{  I_InitializeIO;
  I_InitTempFiles;}
  I_Init;
  SplashForm := TSplashForm.Create(nil);
  outproc := outprocmemo;
  dodraworderinfo := true;
  printf('Starting BrickInventory...'#13#10);
  activebits := 0;

  dismantaledsetsinv := nil;

  streams := TStringList.Create;
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
end;

procedure TMainForm.DrawNavigateBar;
begin
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2><tr>');
  document.write('<td width=16%><a href="back">Back</a></td>');
  document.write('<td width=16%><a href="fwd">Forward</a></td>');
  document.write('<td width=16%><a href="home">Home</a></td>');
  document.write('<td width=16%><a href="inv/0/C/-1">My loose parts</a></td>');
  document.write('<td width=16%><a href="mysets">My sets</a></td>');
  document.write('<td width=16%><a href="colors">Colors</a></td>');
  document.write('<td width=16%><a href="categories">Categories</a></td>');
  document.write('<td width=16%><a href="orders">Orders</a></td>');
  document.write('<td width=16%><a href="ShowStorageBins">Storage Bins</a></td>');

  document.write('</tr></table></p></div><br><br>');

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
    exit;

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
      exit;
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

  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  DrawHeadLine('Orders');

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

  orderslist := TStringList.Create;
  for i := 0 to orders.numorders - 1 do
    for j := 0 to orders.orders[i].Count - 1 do
      if (seller = '') or (orders.orders[i].ORDER[j].SELLER = seller) then
        orderslist.Add(itoa(orders.orders[i].ORDER[j].ORDERID));

  orderslist.Sort;

  for i := 0 to orderslist.Count - 1 do
  begin
      inc(aa);
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

  orderslist.Free;
{  for i := 0 to orders.numorders - 1 do
    for j := 0 to orders.orders[i].Count - 1 do
    begin
      inc(aa);
      oo := orders.orders[i].ORDER[j];
      document.write('<tr bgcolor=' + TBGCOLOR + '>');
      document.write('<td width=5% align=right>' + IntToStr(aa) + '.</td>');
      document.write('<td width=10%><a href=order/' + itoa(oo.ORDERID) + '>' + itoa(oo.ORDERID) + '</a></td>');
      document.write('<td width=10% align=right>' + oo.ORDERDATE + '</td>');
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
    end;}

  document.write('<tr bgcolor=' + TBGCOLOR + '>');
  document.write('<td></td>');
  document.write('<td colspan="4">Total</td>');

  document.write('<td width=8% align=right>' + itoa(numlots) + '</td>');
  document.write('<td width=8% align=right>' + itoa(numitems) + '</td>');
  document.write('<td width=15% align=right>' + Format('€ %2.3f', [sum]) + '</td>');
  document.write('<td width=15% align=right>' + Format('€ %2.3f', [gransum]) + '</td>');
  document.write('<td width=15% align=right>' + Format('€ %2.3f', [evalsum]) + '</td>');
  document.write('</tr></table>');

  document.Flash;

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
begin
  Screen.Cursor := crHourglass;

  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  DrawHeadLine('<a href=storage/>Storage Bins</a> <a href="diagramstorage/Storage Bins"><img src="images\diagram.png">');

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

  aa := 0;
  evalsum := 0.0;
  numitems := 0;
  numlots := 0;
  tw := 0.0;

  for i := 0 to storages.Count - 1 do
  begin
    inc(aa);
    st := storages.Strings[i];
    inv := db.InventoryForStorageBin(st);
    inv.StoreHistoryStatsRec(basedefault + 'storage\storage_' + filenamestring(st) + '.stats');

    document.write('<tr bgcolor=' + TBGCOLOR + '>');
    document.write('<td width=5% align=right>' + IntToStr(aa) + '.</td>');
    document.write('<td width=20%><a href=storage/' + st + '>' + st + '</a></td>');
    numlots := numlots + inv.numlooseparts;
    numitems := numitems + inv.totallooseparts;
    eval := inv.SoldPartOutValue_uQtyAvg.value;
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

  document.write('<tr bgcolor=' + TBGCOLOR + '>');
  document.write('<td>*</td>');
  document.write('<td>Total</td>');

  document.write('<td width=20% align=right>' + itoa(numlots) + '</td>');
  document.write('<td width=20% align=right>' + itoa(numitems) + '</td>');
  document.write('<td width=20% align=right>' + Format('€ %2.3f', [evalsum]) + '</td>');
  document.write('<td width=20% align=right>' + Format('%2.3f Kgr', [tw]) + '</td>');
  document.write('<td width=20% align=right>' + Format('€ %2.3f / Kgr', [dbl_safe_div(evalsum, tw)]) + '</td>');
  document.write('</tr></table>');

  document.Flash;

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

  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  if st = '' then
  begin
    inv := db.InventoryForAllStorageBins;
    inv.StoreHistoryStatsRec(basedefault + 'storage\storagebins.stats');
    snext := '';
    sprev := '';
    html_next := '';
    html_prev := '';
  end
  else
  begin
    inv := db.InventoryForStorageBin(st);
    inv.StoreHistoryStatsRec(basedefault + 'storage\storage_' + filenamestring(st) + '.stats');

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
    document.Flash;
    Screen.Cursor := crDefault;
    exit;
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
  ShowInventorySets(inv, false);
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  inv.Free;
  document.Flash;
  Screen.Cursor := crDefault;
end;



procedure TMainForm.ShowOrder(const orderid: string);
var
  inv: TBrickInventory;
begin

  Screen.Cursor := crHourGlass;

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
    document.Flash;
    Screen.Cursor := crDefault;
    exit;
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
  ShowInventorySets(inv, false);
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.Flash;
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
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="colors">Colors</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="categories">Categories</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="orders">Orders</a></td></tr>');
  document.write('<tr bgcolor=' + TBGCOLOR + '><td><a href="ShowStorageBins">Storage Bins</a></td></tr>');

  document.write('</table></p></div></body>');

  document.Flash;
end;

procedure TMainForm.dbloadprogress(const s: string; d : Double);
begin
  progress_string := s;
  SplashProgress(progress_string, d);
end;

procedure TMainForm.HTMLImageRequest(Sender: TObject; const SRC: String;
  var Stream: TMemoryStream);
var
  idx: integer;
  m: TMemoryStream;
  ps: TPakStream;
  stmp: string;

  function BLCOLOR1: string;
  var
    p: integer;
    n: string;
  begin
    result := '';
    p := Pos('\', SRC);
    if p < 1 then
      exit;
    n := Copy(SRC, 1, p - 1);
    result := itoa(db.colors(atoi(n)).BrickLingColor);
  end;

  function RBCOLOR1: string;
  var
    p: integer;
  begin
    result := '';
    p := Pos('\', SRC);
    if p < 1 then
      exit;
    result := Copy(SRC, 1, p - 1);
  end;


begin
  if fexists(basedefault + SRC) then
    exit;
  idx := streams.IndexOf(strupper(SRC));
  if idx = -1 then
  begin
    ps := TPakStream.Create(SRC, pm_full);
    if ps.IOResult <> 0 then
    begin
      Screen.Cursor := crHourglass;
      ps.Free;
      if RightStr(SRC, 4) = '.jpg' then // set
      begin
        ForceDirectories(basedefault + 's\');
        if not DownloadFile('http://www.bricklink.com/SL/' + ExtractFileName(SRC), basedefault + 's\' + ExtractFileName(SRC)) then
          if not DownloadFile('http://www.bricklink.com/ML/' + ExtractFileName(SRC), basedefault + 's\' + ExtractFileName(SRC)) then
            if not DownloadFile('http://www.1000steine.com/brickset/images/' + ExtractFileName(SRC), basedefault + 's\' + ExtractFileName(SRC)) then
              DownloadFile('http://img.rebrickable.com/img/sets-b/' + ExtractFileName(SRC), basedefault + 's\' + ExtractFileName(SRC))
      end
      else if RightStr(SRC, 4) = '.png' then
      begin
        ForceDirectories(basedefault + RBCOLOR1 + '\');
        if RBCOLOR1 = '89' then
        begin
          if not DownloadJpgFileToPNG('http://www.bricklink.com/SL/' + ChangeFileExt(ExtractFileName(SRC), '.jpg'), basedefault + '89\' + ExtractFileName(SRC)) then
            DownloadJpgFileToPNG('http://www.1000steine.com/brickset/images/' + ChangeFileExt(ExtractFileName(SRC), '.jpg'), basedefault + '89\' + ExtractFileName(SRC));
        end
        else
        begin
          if not DownloadJpgFileToPNG('http://www.bricklink.com/P/' + BLCOLOR1 + '/' + db.BrickLinkPart(firstword(ExtractFileName(SRC), '.')) + '.jpg', basedefault + RBCOLOR1 + '\' + ExtractFileName(SRC)) then
            if not DownloadGIFFileToPNG('http://www.bricklink.com/P/' + BLCOLOR1 + '/' + db.BrickLinkPart(firstword(ExtractFileName(SRC), '.')) + '.gif', basedefault + RBCOLOR1 + '\' + ExtractFileName(SRC)) then
              if not DownloadGIFFileToPNG('http://www.bricklink.com/M/' + db.BrickLinkPart(firstword(ExtractFileName(SRC), '.')) + '.gif', basedefault + RBCOLOR1 + '\' + ExtractFileName(SRC)) then
                if not DownloadJpgFileToPNG('http://www.bricklink.com/M/' + db.BrickLinkPart(firstword(ExtractFileName(SRC), '.')) + '.jpg', basedefault + RBCOLOR1 + '\' + ExtractFileName(SRC)) then ;
        end
      end;
      ps := TPakStream.Create(SRC, pm_full);
      Screen.Cursor := crDefault; 
    end;

    if ps.IOResult <> 0 then
    begin
      ps.Free;

      sTmp := findsimilarimagestring(entriesHash, SRC);
      printf('Image %s not found, retrying ... [%s]'#13#10,[SRC, sTmp]);
      if sTmp = SRC then
      begin
        Stream := nil;
        exit;
      end;
      if sTmp = '' then
      begin
        Stream := nil;
        exit;
      end;
      if sTmp[Length(sTmp)] = '\' then
      begin
        Stream := nil;
        exit;
      end;

      ps := TPakStream.Create(sTmp, pm_full);
      if ps.IOResult <> 0 then
      begin
        ps.Free;
        Stream := nil;
        exit;
      end;
    end;
    m := TMemoryStream.Create;
    m.LoadFromStream(ps);
    ps.Free;
    idx := streams.AddObject(strupper(SRC), m);
  end;
  Stream := streams.Objects[idx] as TMemoryStream;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  TimingForm.CrawlerTimer.Enabled := false;
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
  I_Quit;

  document.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  i: integer;
  parts: TDStringList;
begin
  if initialized then
    exit;

  ShowSplash;
  printf('PAK_InitFileSystem(): Initializing pak files.'#13#10);
  PAK_InitFileSystem;
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
  PAK_GetEntries(entries);
  entries.Sort;
  entriesHash.AssignStringList(entries);
  db := TSetsDatabase.Create;
  progress_string := 'Loading database...';
  db.progressfunc := dbloadprogress;
  db.LoadFromDisk(basedefault + 'db\db_set_pieces.txt');
  inventory := TBrickInventory.Create;
  if FileExists(basedefault + 'myparts.txt') then
    inventory.LoadLooseParts(basedefault + 'myparts.txt');
  if FileExists(basedefault + 'mysets.txt') then
    inventory.LoadSets(basedefault + 'mysets.txt');

  initialized := true;
  HideSplash;

  orders.LoadFilesDirectory(basedefault + 'orders');
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
    Result := '<td width=25%><p align="center"><b>' + cont + '</b><br></p>';
    Result := result + '<table width=99% bgcolor=' + TBGCOLOR + ' border=2><tbody><tr align="RIGHT">';
    result := result + '<td width="50%">Percent:</td><td width="50%"><b>' + sp;
    result := result + '</b></td></tr><tr align="RIGHT"><td>Avg Price:</td><td><b>€ ' + sy1;
    result := result + '</b></td></tr><tr align="RIGHT"><td>Qty Avg Price:</td><td><b>€ ' + sy2;
    result := Result + '</b></td></tr></tbody></table></td>';
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
  document.write('<td width=50% align="center"><b>Sold</b></td>');
  document.write('<td width=50% align="center"><b>Available</b></td>');
  document.write('</tr>');
  document.write('</table>');
  document.write('<table  width=99% bgcolor=' + THBGCOLOR + ' border=2>');
  document.write('<tr>');
  inv.UpdateCostValues;
  document.write(_poutcol('New', inv.SoldPartOutValue_nAvg.percentage, inv.SoldPartOutValue_nAvg.value, inv.SoldPartOutValue_nQtyAvg.value));
  document.write(_poutcol('Used', inv.SoldPartOutValue_uAvg.percentage, inv.SoldPartOutValue_uAvg.value, inv.SoldPartOutValue_uQtyAvg.value));
  document.write(_poutcol('New', inv.AvailablePartOutValue_nAvg.percentage, inv.AvailablePartOutValue_nAvg.value, inv.AvailablePartOutValue_nQtyAvg.value));
  document.write(_poutcol('Used', inv.AvailablePartOutValue_uAvg.percentage, inv.AvailablePartOutValue_uAvg.value, inv.AvailablePartOutValue_uQtyAvg.value));
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

procedure TMainForm.DrawBrickOrderInfo(const brick: brickpool_p);
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
begin
  if not dodraworderinfo then
    exit;

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
              document.write('<td colspan="4"><b><a href="sinv/%s">%d x %s</a></b> - %s <img width=48px src=s\%s.jpg></td>',
                [sid, inventory.sets[i].dismantaled,
                 sid, db.SetDesc(sid), sid]);
              document.write('<td align="right">%d</td>', [num]);
              document.write('<td colspan="3"><br></td></tr>');
            end;
            inv2.Free;
          end;
        end;

  pci := db.PieceColorInfo(brick.part, brick.color);
  if pci <> nil then
  begin
    for i := 0 to pci.storage.Count - 1 do
    begin
      document.write('<tr bgcolor=#EEEEEE>');
      splitstring(pci.storage.Strings[i], stor, stmp, ':');
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
                ['<b><a href=storage/' + stor + '>' + stor + '</a></b>:' + stmp3]);
    end;
  end;

  document.write('<tr bgcolor=' + TFGCOLOR + '><td colspan="8"></td></tr>');
end;

procedure TMainForm.DrawInventoryTable(inv: TBrickInventory; const lite: Boolean = false; const setid: string = '');
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

  if inv = nil then
    inv := inventory;

  Screen.Cursor := crHourGlass;

  inv.SortPieces;
  if not lite then
    DrawPartOutValue(inv, setid);

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

  ShowSplash;
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
    scolor := itoa(brick.Color);
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=35%><img src=' + scolor + '\' + brick.part + '.png><br><b>');
    document.write('<a href=spiece/' + brick.part + '>' + brick.part + '</a></b>');
    document.write(' - ' + db.PieceDesc(brick.part) + '</td><td width=20%>');
    document.BlancColorCell(db.colors(brick.color).RGB, 25);
    document.write('<a href=spiecec/' + brick.part + '/' + IntToStr(brick.color) + '>' +  db.colors(brick.color).name + ' (' + scolor + ') (BL=' + IntToStr(db.colors(brick.color).BrickLingColor) + ')<img src="images\details.png"></a></td>');
    pci := db.PieceColorInfo(brick.part, brick.color);
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
    pi := db.PieceInfo(brick.part);
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

    DrawBrickOrderInfo(brick);
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

  document.write('</tr>');

  cl.Free;
  pl.Free;

  HideSplash;

  document.write('</tr></table>');

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
    Result := result + '<table width=99% bgcolor=' + TBGCOLOR + ' border=2><tbody><tr align="RIGHT">';
    result := result + '<td width="50%">' + til + '</td><td width="50%"><b>' + sx1;
    result := result + '</b></td></tr><tr align="RIGHT"><td>Total Qty:</td><td><b>' + sx2;
    result := result + '</b></td></tr><tr align="RIGHT"><td>Min Price:</td><td><b>€ ' + sy1;
    result := result + '</b></td></tr><tr align="RIGHT"><td>Avg Price:</td><td><b>€ ' + sy2;
    result := result + '</b></td></tr><tr align="RIGHT"><td>Qty Avg Price:</td><td><b>€ ' + sy3;
    result := Result + '</b></td></tr><tr align="RIGHT"><td>Max Price:</td><td><b>€ ' + sy4;
    result := Result + '</b></td></tr></tbody></table></td>';
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

procedure TMainForm.ShowInventorySets(const inv: TBrickInventory; const header_flash: boolean);
var
  aa, i: integer;
  set1: set_p;
  link: string;
  sinv: TBrickInventory;
  tot: integer;
begin
  if inv.numsets = 0 then
    if not header_flash then
      Exit;

  Screen.Cursor := crHourGlass;

  if header_flash then
  begin
    document.write('<body background="splash.jpg">');
    DrawNavigateBar;
    document.write('<div style="color:' + DFGCOLOR + '">');
    document.write('<p align=center>');
    link := '<a href=dismantleallsets>Dismantle All Sets</a> - <a href=buildallsets>Build All Sets</a>';

    tot := 0;
    for i := 0 to inv.numsets - 1 do
      if inv.sets[i].num > 0 then
      begin
        sinv := db.GetSetInventory(inv.sets[i].setid);
        if sinv <> nil then
          tot := tot + sinv.totallooseparts * inv.sets[i].num;
      end;

    DrawHeadLine('My Sets (' + IntToStr(inv.numsets) + ' lots)<br>(' + IntToStr(inv.totalsetsbuilted) + ' builted - ' + IntToStr(inv.totalsetsdismantaled) + ' dismantaled)<br><br>' +
      'My builded sets have <a href="ShowMySetsPieces">' + IntToStr(tot) + ' parts</a><br><br>' +
      link);

  end;

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Set</b></th>');
  if header_flash then
  begin
    document.write('<th>Num Builded</th>');
    document.write('<th>Num Dismandaled</th>');
  end
  else
    document.write('<th>Num</th>');
  document.write('</tr>');

  ShowSplash;
  set1 := @inv.sets[0];
  aa := 0;
  for i := 0 to inv.numsets - 1 do
  begin
    inc(aa);

    if inv = inventory then
      inventory.StorePieceInventoryStatsRec(basedefault + 'out\' + set1.setid + '\' + set1.setid + '.history', set1.setid, -1);

    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%><img width=64px src=s\' + set1.setid + '.jpg><br>');
    document.write('<a href="sinv/' + set1.setid + '">');
    document.write('<b>' + set1.setid + '</b> - ' + db.SetDesc(set1.setid));
    document.write('</td><td width=15% align=right>');
    if header_flash then
    begin
      document.write(IntToStr(set1.num) + '</td>');
      document.write('<td width=15% align=right>' + IntToStr(set1.dismantaled) + '</td></tr>');
    end
    else
      document.write(IntToStr(set1.num + set1.dismantaled) + '</td></tr>');
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
    document.Flash;
  end;

  Screen.Cursor := crDefault;

end;

procedure TMainForm.ShowMySets;
begin
  ShowInventorySets(inventory, true);
end;

procedure TMainForm.ShowSetInventory(const setid: string);
var
  inv: TBrickInventory;
  missing: integer;
  st: set_t;
  i, j: integer;
  desc: string;
  tmpsets: TStringList;
  s1: string;
  sx, ss1, ss2, ss3: string;
  sf1, sf2, sf3, sf4: string;
  numbricks: integer;
  numplates: integer;
  numtiles: integer;
  numslopes: integer;
  idx: integer;
  lnk: string;
begin

  Screen.Cursor := crHourGlass;

  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  inv := db.GetSetInventory(setid);
  if inv = nil then
  begin
    lnk := '<a href=downloadset/' + setid + '>Try download set inventory from bricklink.com</a>';
    DrawHeadLine('Can not find inventory for ' + setid + '<br><br>' + lnk);
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
    document.Flash;
    Screen.Cursor := crDefault;
    exit;
  end;

  inv.DoUpdateCostValues;
  inv.SortPieces;

  s1 := basedefault + 'out\' + setid + '\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
  s1 := s1 + setid + '.txt';
  inv.SaveLooseParts(s1);

  inv.StoreHistoryStatsRec(basedefault + 'out\' + setid + '\' + setid + '.stats');
  inventory.StorePieceInventoryStatsRec(basedefault + 'out\' + setid + '\' + setid + '.history', setid, -1);

  for j := 0 to inv.numlooseparts - 1 do
    inventory.StorePieceInventoryStatsRec(
      basedefault + 'cache\' + itoa(decide(inv.looseparts[j].color = -1, 9999, inv.looseparts[j].color)) + '\' + inv.looseparts[j].part + '.history',
      inv.looseparts[j].part,
      inv.looseparts[j].color
    );
  for j := 0 to inv.numsets - 1 do
    inventory.StorePieceInventoryStatsRec(
      basedefault + 'out\' + inv.sets[j].setid + '\' + inv.sets[j].setid + '.history',
      inv.sets[j].setid, -1
    );

  inventory.GetSetInfo(setid, @st);
  sf1 := '<a href=addset/' + setid + '>+</a>';
  if st.num > 0 then
    sf2 := '<a href=removeset/' + setid + '>-</a>'
  else
    sf2 := '';
  sf3 := '<a href=addsetdismantaled/' + setid + '>+</a>';
  if st.dismantaled > 0 then
    sf4 := '<a href=removesetdismantaled/' + setid + '>-</a>'
  else
    sf4 := '';
  ss1 := Format('Inventory for %s - %s <br>(%d lots, %d parts, %d sets)<br>You have %s%d%s builted and %s%d%s dismantaled<br><img width=360px src=s\' + setid + '.jpg>' +
      ' <a href=diagrampiece/' + setid + '/-1><img src="images\diagram.png"></a>' + '<br>[Year: <a href=ShowSetsAtYear/%d>%d</a>]<br>',
    [setid, db.SetDesc(setid), inv.numlooseparts, inv.totallooseparts, inv.totalsetsbuilted + inv.totalsetsdismantaled,
     sf2, st.num, sf1, sf4, st.dismantaled, sf3, db.SetYear(setid), db.SetYear(setid)]);
  idx := db.allsets.IndexOf(setid);
  if idx > -1 then
  begin
    if idx > 0 then
      ss1 := ss1 + '<a href=sinv/' + db.allsets.Strings[idx - 1] + '>Prev(' + db.allsets.Strings[idx - 1] + ' - ' + db.SetDesc(db.allsets.Strings[idx - 1]) + ')</a>  ';
    if idx < db.allsets.Count - 1 then
      ss1 := ss1 + '<a href=sinv/' + db.allsets.Strings[idx + 1] + '>Next(' + db.allsets.Strings[idx + 1] + ' - ' + db.SetDesc(db.allsets.Strings[idx + 1]) + ')</a>  ';
  end
  else
  begin
    splitstring(setid, ss2, ss3, '-');
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
    DrawHeadLine('You can dismantle this set to your loose parts! <a href=dismantleset/' + setid + '>(dismantle it!)</a>');
  if missing = 0 then
    DrawHeadLine('You can built this set! <a href=buildset/' + setid + '>(Built it!)</a>')
  else
    DrawHeadLine(Format('%d part' + decide(missing = 1, ' is', 's are') + ' missing to build a copy of this set (%2.2f%s) (<a href="missingtobuildset/%s">show</a>)', [missing, missing/inv.totallooseparts*100, '%', setid]));
  DrawHeadLine(Format('<a href="missingtobuildset/%s/2">Check to build 2 sets</a>', [setid]));

  numbricks := inv.totalloosepartsbycategory(5);
  numplates := inv.totalloosepartsbycategory(26);
  numtiles := inv.totalloosepartsbycategory(37) + inv.totalloosepartsbycategory(39);
  numslopes := inv.totalloosepartsbycategory(31) + inv.totalloosepartsbycategory(32) + inv.totalloosepartsbycategory(33);

  DrawHeadLine(Format('Bricks: %d<br>Slopes: %d<br>Plates: %d<br>Tiles: %d<br>Other %d', [numbricks, numslopes, numplates, numtiles, inv.totallooseparts - numbricks - numplates - numslopes - numtiles]));

  DrawPriceguide(setid);
  DrawInventoryTable(inv, false, setid);
  document.write('<br>');
  document.write('<br>');
  ShowInventorySets(inv, false);
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
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
    document.Flash;
    Screen.Cursor := crDefault;
    exit;
  end;

  DrawHeadLine('Colors');

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Color</b></th>');
  document.write('<th>Lots</th>');
  document.write('<th>Num pieces</th>');
  document.write('<th>Weight (Kg)</th>');
  document.write('</tr>');

  ShowSplash;
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
    document.BlancColorCell(cp.RGB, 25);
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

  HideSplash;

  document.write('</tr></table>');
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.Flash;
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
    document.Flash;
    Screen.Cursor := crDefault;
    exit;
  end;

  DrawHeadLine('<a href="inv/' + IntToStr(Integer(inv)) +'/CAT/' + IntToStr(cat) + '"><b>' + db.categories[cat].name + '</b> - Colors');

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Color</b></th>');
  document.write('<th>Lots</th>');
  document.write('<th>Num pieces</th>');
  document.write('<th>Weight (Kg)</th>');
  document.write('</tr>');

  ShowSplash;
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
      document.BlancColorCell(cp.RGB, 25);
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

  HideSplash;

  document.write('</tr></table>');
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.Flash;
  Screen.Cursor := crDefault;
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
    document.Flash;
    Screen.Cursor := crDefault;
    exit;
  end;

  DrawHeadLine('Categories');

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Category</b></th>');
  document.write('<th>Lots</th>');
  document.write('<th>Num pieces</th>');
  document.write('<th>Weight (Kg)</th>');
  document.write('</tr>');

  ShowSplash;
  tlots := 0;
  tparts := 0;
  tweight := 0.0;
  aa := 0;
  for i := 0 to MAXCATEGORIES - 1 do
  begin
    cp := @db.categories[i];
    if (cp.knownpieces = nil) or (cp.knownpieces.Count = 0) then
      Continue;
    nparts := inv.totalloosepartsbycategory(i);
    if nparts > 0 then
    begin
      inc(aa);
      document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');
      document.write('<td width=50%>');
      document.write('<a href="inv/' + IntToStr(Integer(inv)) +'/CAT/' + IntToStr(i) + '">');
      document.write('<b>' + cp.name + '</b></a> ');
      document.write('<a href="catcolors/' + IntToStr(i) + '"><img src="images\colors.png"></a>');
//      document.write('<b>...</b></a>');

      document.write('</td>');
      nlots := inv.numlotsbycategory(i);
      document.write('<td width=25% align=right>' + IntToStr(nlots) + '</td>');
      document.write('<td width=25% align=right>' + IntToStr(nparts) + '</td>');
      nweight := inv.weightbycategory(i) / 1000;
      document.write('<td width=25% align=right>' + Format('%2.3f', [nweight]) + '</td>');

      tlots := tlots + nlots;
      tparts := tparts + nparts;
      tweight := tweight + nweight;

      document.write('</tr>');
    end;
    SplashProgress('Working...', i / MAXCATEGORIES);
  end;

  document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>*</td>');
  document.write('<td width=50%><b>Total</b></td>');      
  document.write('<td width=25% align=right>' + IntToStr(tlots) + '</td>');
  document.write('<td width=25% align=right>' + IntToStr(tparts) + '</td>');
  document.write('<td width=25% align=right>' + Format('%2.3f', [tweight]) + '</td>');
  document.write('</tr>');

  HideSplash;


  document.write('</tr></table>');
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.Flash;
  Screen.Cursor := crDefault;
end;

function TMainForm.GetAPieceColor(pcs: string): integer;
var
  i: integer;
begin
  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
      if db.Colors(i).knownpieces <> nil then
      begin
        result := db.Colors(i).knownpieces.IndexOf(pcs);
        if result < 0 then
          result := db.Colors(i).knownpieces.IndexOf(strupper(pcs));
        if result >= 0 then
          exit;
      end;
  result := 0;
end;

procedure TMainForm.ShowPiece(pcs: string);
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
  nextP, prevP: string;
begin
  UpdateDismantaledsetsinv;

  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  idx := db.AllPieces.IndexOf(db.RebricablePart(pcs));

  if idx >= 0 then
    pcs := db.RebricablePart(pcs);

  if idx < 0 then
  begin
    idx := db.AllPieces.IndexOf(db.BrickLinkPart(pcs));
    if idx >= 0 then
      pcs := db.BrickLinkPart(pcs);
  end;

  if idx < 0 then
    for i := 0 to db.AllPieces.Count - 1 do
      if strupper(pcs) = strupper(db.AllPieces.Strings[i]) then
      begin
        idx := i;
        break;
      end;

  if idx < 0 then
  begin
    DrawHeadLine('Can not find piece ' + pcs);
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
    document.Flash;
    Screen.Cursor := crDefault;
    exit;
  end;

  if idx > 0 then
    prevP := db.AllPieces.Strings[idx - 1]
  else
    prevP := '';

  if idx < db.AllPieces.Count - 1 then
    nextP := db.AllPieces.Strings[idx + 1]
  else
    nextP := '';

  pi := db.PieceInfo(pcs);

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

  cathtml := '<a href="catcolors/' + IntToStr(pi.category) + '">' + '<b>' + db.categories[pi.category].name + '<img src="images\colors.png"></b></a>';
  if pi.weight > 0.0 then
    DrawHeadLine(pcs + ' - ' + db.PieceDesc(pcs) + ' (' + Format('%2.2f gr', [pi.weight]) + ')' + '<a href=editpiece/' + pcs + '/' + itoa(GetAPieceColor(pcs)) + '><img src="images\edit.png"></a>' + refrhtml + '<br>' + cathtml)
  else
    DrawHeadLine(pcs + ' - ' + db.PieceDesc(pcs) + '<a href=editpiece/' + pcs + '/' + itoa(GetAPieceColor(pcs)) + '><img src="images\edit.png"></a>' + refrhtml + '<br>' + cathtml);

  storages := db.StorageBinsForMold(pcs);
  if storages.Count > 0 then
  begin
    stortxt := '<b>Storage Bins:</b><br>';
    for i := 0 to storages.Count - 1 do
      stortxt := stortxt + '<a href=storage/' + storages.Strings[i] + '>' + storages.Strings[i] + '</a><br>';
    DrawHeadLine(stortxt);
  end;
  storages.Free;

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

  aa := 0;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
      if db.Colors(i).knownpieces <> nil then
      begin
        idx := db.Colors(i).knownpieces.IndexOf(pcs);
        if idx < 0 then
          idx := db.Colors(i).knownpieces.IndexOf(strupper(pcs));
        if idx >= 0 then
        begin

          inventory.StorePieceInventoryStatsRec(basedefault + 'cache\' + IntToStr(decide(i = -1, 9999, i)) + '\' + pcs + '.history', pcs, i);

          numpieces := inventory.LoosePartCount(pcs, i);
          if numpieces > 0 then
          begin
            inc(aa);
            document.write('<td width=10%><img src=' + IntToStr(i) + '\' + pcs + '.png>');
            document.BlancColorCell(db.colors(i).RGB, 25);
            document.write('<a href=spiecec/' + pcs + '/' + IntToStr(i) + '>' + db.colors(i).name + '</a> (' + IntToStr(i) + ') (BL=' + IntToStr(db.colors(i).BrickLingColor) + ')<img src="images\details.png"></td>');
            document.write('<td width=10% align=right>' + Format('%d', [numpieces]) +
              '<br><a href=editpiece/' + pcs + '/' + itoa(i) + '><img src="images\edit.png"></a>' +
              '<br><a href=diagrampiece/' + pcs + '/' + itoa(i) + '><img src="images\diagram.png"></a>' +
              '</td>');
            if aa mod 5 = 0 then
              document.write('</tr>');
          end;
        end;
      end;

  document.write('</table><br><br>');

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

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
      if db.Colors(i).knownpieces <> nil then
      begin
        idx := db.Colors(i).knownpieces.IndexOf(pcs);
        if idx < 0 then
          idx := db.Colors(i).knownpieces.IndexOf(strupper(pcs));
        if idx >= 0 then
        begin

          inc(aa);
          numpieces := inventory.LoosePartCount(pcs, i);
          totpieces := totpieces + numpieces;
          document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');
          document.write('<td width=35%><img src=' + IntToStr(i) + '\' + pcs + '.png></td>');
          document.write('<td width=20%>');
          document.BlancColorCell(db.colors(i).RGB, 25);
          document.write('<a href=spiecec/' + pcs + '/' + IntToStr(i) + '>' + db.colors(i).name + '</a> (' + IntToStr(i) + ') (BL=' + IntToStr(db.colors(i).BrickLingColor) + ')<img src="images\details.png"></td>');
          pci := db.PieceColorInfo(pcs, i);
          if pci <> nil then
          begin
            document.write('<td width=10% align=right>');
            document.write('N=%2.3f<br>U=%2.3f</td>', [pci.nDemand, pci.uDemand]);
          end
          else
            document.write('<td width=10% align=right></td>');
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
  document.Flash;

end;

procedure TMainForm.DrawPieceList(const tit: string; const lst: TStringList);
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

  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine(tit);

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

  for i := 0 to lst.Count - 1 do
  begin
    inc(aa);
    splitstring(lst.Strings[i], pcs, col, ',');
    cl := atoi(col);
    numpieces := inventory.LoosePartCount(pcs, cl);
    totpieces := totpieces + numpieces;
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');

    document.write('<td width=35%><img src=' + col + '\' + pcs + '.png>');
    document.write('<a href=spiece/' + pcs + '>' + pcs + '</a></b>');
    document.write(' - ' + db.PieceDesc(pcs) + '</td>');
    document.write('<td width=20%>');
    document.BlancColorCell(db.colors(cl).RGB, 25);
    document.write('<a href=spiecec/' + pcs + '/' + col + '>' + db.colors(cl).name + '</a> (' + col + ') (BL=' + IntToStr(db.colors(cl).BrickLingColor) + ')<img src="images\details.png"></td>');
    document.write('<td width=15% align=right>' + Format('%d', [numpieces]) +
            '<br><a href=editpiece/' + pcs + '/' + itoa(cl) + '><img src="images\edit.png"></a>' +
            '<br><a href=diagrampiece/' + pcs + '/' + itoa(cl) + '><img src="images\diagram.png"></a>' +
            '</td>');

    pi := db.PieceInfo(pcs);
    pci := db.PieceColorInfo(pcs, cl);
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
    SplashProgress('Working', i / lst.Count);
  end;

  document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right><b>Total</b></td>');
  document.write('<td width=35%><b> </b></td>');
  document.write('<td width=20%><b> </b></td>');
  document.write('<td width=10% align=right><b>' + IntToStr(totpieces) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [prnt]) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [prut]) + '</b></td>');
  document.write('<td width=10% align=right><b> </b></td>');

  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.Flash;

  HideSplash;

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

  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine(tit);

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
    splitstring(lst.Strings[i], pcs, col, ',');
    cl := atoi(col);
    numpieces := inventory.LoosePartCount(pcs, cl);
    totpieces := totpieces + numpieces;
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');

    pi := db.PieceInfo(pcs);
    pci := db.PieceColorInfo(pcs, cl);

    document.write('<td width=35%><img src=' + col + '\' + pcs + '.png>');
    document.write('<a href=spiece/' + pcs + '>' + pcs + '</a></b>');
    document.write(' - ' + db.PieceDesc(pcs) + '</td>');
    document.write('<td width=20%>');
    document.BlancColorCell(db.colors(cl).RGB, 25);
    document.write('<a href=spiecec/' + pcs + '/' + col + '>' + db.colors(cl).name + '</a> (' + col + ') (BL=' + IntToStr(db.colors(cl).BrickLingColor) + ')<img src="images\details.png"></td>');
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
    SplashProgress('Working', i / lst.Count);
  end;

  document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right><b>Total</b></td>');
  document.write('<td width=35%><b> </b></td>');
  document.write('<td width=20%><b> </b></td>');
  document.write('<td width=10% align=right><b>' + IntToStr(totpieces) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [prnt]) + '</b></td>');
  document.write('<td width=10% align=right><b>' + Format('€ %2.2f', [prut]) + '</b></td>');
  document.write('<td width=10% align=right><b> </b></td>');

  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.Flash;

  HideSplash;

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
  p: integer;
  fname: string;
begin
  result := '';
  fname := PieceColorCacheFName(pcs, itoa(color)) + '.htm';
  if not fexists(fname) then
    exit;
  s := TStringList.Create;
  s.LoadFromFile(fname);
  stmp := Utf8ToAnsi(s.Text);
  s.Free;

  p := Pos('Group by Currency', stmp);
  if p <= 0 then
    exit;
  stmp2 := Copy(stmp, p, Length(stmp) - p - 1);

  p := Pos('<TABLE ', stmp2);
  if p <= 0 then
    exit;
  stmp3 := Copy(stmp2, p, Length(stmp2) - p - 1);

  p := Pos('Indicates whether', stmp3);
  if p <= 0 then
    p := Pos('Select Language', stmp3);
  if p <= 0 then
  begin
    result := StringReplace(stmp3, '/images/box16', 'images\box16', [rfReplaceAll, rfIgnoreCase]);
  end
  else
  begin
    stmp := Copy(stmp3, 1, p - Length(CHECK2) + 15);
    result := StringReplace(stmp, '/images/box16', 'images\box16', [rfReplaceAll, rfIgnoreCase]);
  end;
end;

procedure TMainForm.ShowColorPiece(const pcs: string; const color: integer);
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
begin
  UpdateDismantaledsetsinv;
  
  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  pi := db.PieceInfo(pcs);
  pci := db.PieceColorInfo(pcs, color);
  if pci <> nil then
    stmp := '<br>Appears in ' + IntToStr(pci.sets.Count) + ' sets'
  else
    stmp := '';

  DrawHeadLine('<a href=spiece/' + pcs + '>' + pcs + '</a> - ' + db.Colors(color).name + ' ' + db.PieceDesc(pcs) +
    ' <a href=editpiece/' + pcs + '/' + itoa(color) + '><img src="images\edit.png"></a>' +
    '<br><a href=diagrampiece/' + pcs + '/' + itoa(color) + '><img src="images\diagram.png"></a>' +

    '<br><br><img src=' + IntToStr(color) + '\' + pcs + '.png>' + stmp);

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

  if (color >= 0) and (color <= MAXINFOCOLOR) then
    if db.Colors(color).id = color then
      if db.Colors(color).knownpieces <> nil then
      begin
        idx := db.Colors(color).knownpieces.IndexOf(pcs);
        if idx < 0 then
          idx := db.Colors(color).knownpieces.IndexOf(strupper(pcs));
        if idx >= 0 then
        begin
          i := color;
          inc(aa);

          inventory.StorePieceInventoryStatsRec(basedefault + 'cache\' + IntToStr(decide(i = -1, 9999, i)) + '\' + pcs + '.history', pcs, i);
          numpieces := inventory.LoosePartCount(pcs, i);
          document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td>');
          document.write('<td width=35%><a href=spiece/' + pcs + '><img src=' + IntToStr(i) + '\' + pcs + '.png></a></td>');
          document.write('<td width=20%>');
          document.BlancColorCell(db.colors(i).RGB, 25);
          document.write(db.colors(i).name + ' (' + IntToStr(i) + ') (BL=' + IntToStr(db.colors(i).BrickLingColor) + ')</td>');
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
            begin
              inv := db.GetSetInventory(pci.sets.Strings[j]);
              if db.SetYear(pci.sets.Strings[j]) > 1950 then
                y := Format('[Year: <a href=ShowSetsAtYear/%d>%d</a>]', [db.SetYear(pci.sets.Strings[j]), db.SetYear(pci.sets.Strings[j])])
              else
                y := '';
              DrawHeadLine(Format('<a href="sinv/%s">%s - %s</a> %s<br>(appears %d times)<br><img width=240px src=s\' + pci.sets.Strings[j] + '.jpg>',
                [pci.sets.Strings[j], pci.sets.Strings[j], db.SetDesc(pci.sets.Strings[j]), y, inv.LoosePartCount(pcs, color)]));
            end;

        end;
      end;


  document.write('</table>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write(BLColorPieceInfo(pcs, color));
  document.write('</body>');
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
  aa := 0;
  for i := 0 to numsets - 1 do
  begin
    inc(aa);
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%><img width=64px src=s\' + A[i].setid + '.jpg><br>');
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
  end;
  HideSplash;

  document.write('</tr></table>');

  document.write('</p>');
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.Flash;

  Screen.Cursor := crDefault;


  dispose(A);
end;

procedure TMainForm.ShowSetsAtYear(const year: integer);
var
  lsets: TStringList;
  i: integer;
  aa: integer;
begin
  Screen.Cursor := crHourGlass;

  lsets := db.SetListAtYear(year);

  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine('<a href=ShowSetsAtYear/' + itoa(year - 1) + '>' + itoa(year - 1) + '</a> - ' +
               '<b>' + itoa(year) + '</b>' +
               ' - <a href=ShowSetsAtYear/' + itoa(year + 1) + '>' + itoa(year + 1) + '</a> ');

  DrawHeadLine('Sets');

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
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%><img width=64px src=s\' + lsets.Strings[i] + '.jpg><br>');
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
  end;
  HideSplash;

  document.write('</tr></table>');

  document.write('</p>');
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.Flash;

  lsets.Free;
  Screen.Cursor := crDefault;

end;


type
  _costclass = class(TObject)
    setcost: double;
    invcost: double;
    numparts: integer;
    demand: double;
  end;

procedure TMainForm.ShowSetsForPartOutNew;
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
  demand: double;
begin
  Screen.Cursor := crHourGlass;

  list := TStringList.Create;

  ShowSplash;
  for i := 0 to db.AllSets.Count - 1 do
  begin
    if i mod 100 = 0 then
      SplashProgress('Working...', i / db.AllSets.Count);

    if db.SetYear(db.AllSets.Strings[i]) < 1990 then
      continue;

    av := db.Availability(db.AllSets.Strings[i]);
    if av.nTotalLots < 5 then
      continue;
    setcost := av.nQtyAvgPrice;
    invcost := 0.0;

    inv := db.GetSetInventory(db.AllSets.Strings[i]);
    numparts := 0;
    if inv.numsets > 0 then
    begin
      inv2 := inv.Clone;
      inv2.DismandalAllSets;

      demand := inv2.nDemand.value;
      if demand < 0.75 then
      begin
        inv2.Free;
        continue;
      end;
      invcost := invcost + inv2.SoldPartOutValue_nQtyAvg.value;
      numparts := numparts + inv2.totallooseparts;
      inv2.Free;
    end
    else
    begin
      demand := inv.nDemand.value;
      if demand < 0.75 then
        continue;
      invcost := invcost + inv.SoldPartOutValue_nQtyAvg.value;
      numparts := numparts + inv.totallooseparts;
    end;

    if invcost > setcost * 1.25 then
    begin
      cls := _costclass.Create;
      cls.invcost := invcost;
      cls.setcost := setcost;
      cls.numparts := numparts;
      cls.demand := demand;
      list.AddObject(db.AllSets.Strings[i], cls);
    end;
  end;

  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine('New sets to buy for partout');

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Set</b></th>');
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
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%><img width=64px src=s\' + list.Strings[i] + '.jpg><br>');
    document.write('<a href="sinv/' + list.Strings[i] + '">');
    document.write('<b>' + list.Strings[i] + '</b> - ' + db.SetDesc(list.Strings[i]));
    cls := list.Objects[i] as _costclass;
    document.write('</td><td width=15% align=right>');
    document.write(IntToStr(cls.numparts) + '</td>');
    document.write('</td><td width=15% align=right>');
    document.write('€ %2.2f</td>', [cls.setcost]);
    document.write('</td><td width=15% align=right>');
    document.write('€ %2.2f</td>', [cls.invcost]);
    document.write('</td><td width=15% align=right>');
    document.write('%2.2f</td>', [cls.demand]);
    document.write('</td><td width=15% align=right>');
    document.write(Format('%2.3f%s', [cls.invcost / cls.setcost * 100, '%']) + '</td>');
    document.write('</td><td width=15% align=right>');
    document.write(Format('%2.3f', [cls.invcost / cls.setcost * cls.demand]) + '</td></tr>');

    if list.Count > 1 then
    begin
      if list.Count < 20 then
        SplashProgress('Working...', i / (list.Count - 1))
      else
        if (i mod 5) = 0 then
          SplashProgress('Working...', i / (list.Count - 1));
    end;
  end;
  HideSplash;

  document.write('</table>');

  document.write('</p>');
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.Flash;

  Screen.Cursor := crDefault;


  FreeList(list);
end;

procedure TMainForm.ShowSetsForPartOutUsed;
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
  demand: double;
  sset: string;
begin
  Screen.Cursor := crHourGlass;

  list := TStringList.Create;

  ShowSplash;
  for i := 0 to db.AllSets.Count - 1 do
  begin
    if i mod 100 = 0 then
      SplashProgress('Working...', i / db.AllSets.Count);

    if db.SetYear(db.AllSets.Strings[i]) < 1990 then
      continue;

    av := db.Availability(db.AllSets.Strings[i]);
    if av.uTotalLots < 5 then
      continue;
    setcost := av.uQtyAvgPrice;

    numparts := 0;
    invcost := 0.0;
    inv := db.GetSetInventory(db.AllSets.Strings[i]);
    if inv.numsets > 0 then
    begin
      inv2 := inv.Clone;
      inv2.DismandalAllSets;

      demand := inv2.uDemand.value;
      if demand < 0.75 then
      begin
        inv2.Free;
        continue;
      end;

      invcost := inv2.SoldPartOutValue_uQtyAvg.value;
      numparts := numparts + inv2.totallooseparts;
      inv2.Free;
    end
    else
    begin
      demand := inv.uDemand.value;
      if demand < 0.75 then
        continue;

      invcost := inv.SoldPartOutValue_uQtyAvg.value;
      numparts := numparts + inv.totallooseparts;
    end;

    if invcost > setcost * 1.25 then
    begin
      cls := _costclass.Create;
      cls.invcost := invcost;
      cls.setcost := setcost;
      cls.numparts := numparts;
      cls.demand := demand;
      list.AddObject(db.AllSets.Strings[i], cls);
    end;
  end;

  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine('Used sets to buy for partout');

  document.write('<table width=99% bgcolor=' + TBGCOLOR + ' border=2>');
  document.write('<tr bgcolor=' + THBGCOLOR + '>');
  document.write('<th><b>#</b></th>');
  document.write('<th><b>Set</b></th>');
  document.write('<th>Num Pieces</th>');
  document.write('<th>Set Cost</th>');
  document.write('<th>Part Out value</th>');
  document.write('<th>Demand</th>');
  document.write('<th>GAIN</th>');
  document.write('</tr>');
  HideSplash;

  ShowSplash;
  aa := 0;
  for i := 0 to list.Count - 1 do
  begin
    inc(aa);
    sset := list.Strings[i];
    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%><img width=64px src=s\' + sset + '.jpg><br>');
    document.write('<a href="sinv/' + sset + '">');
    document.write('<b>' + sset + '</b> - ' + db.SetDesc(sset));
    cls := list.Objects[i] as _costclass;
    document.write('</td><td width=15% align=right>');
    document.write(IntToStr(cls.numparts) + '</td>');
    document.write('</td><td width=15% align=right>');
    document.write('€ %2.2f</td>', [cls.setcost]);
    document.write('</td><td width=15% align=right>');
    document.write('€ %2.2f</td>', [cls.invcost]);
    document.write('</td><td width=15% align=right>');
    document.write('%2.2f</td>', [cls.demand]);
    document.write('</td><td width=15% align=right>');
    document.write(Format('%2.3f%s', [cls.invcost / cls.setcost * 100, '%']) + '</td></tr>');

    if list.Count > 1 then
    begin
      if list.Count < 20 then
        SplashProgress('Working...', i / (list.Count - 1))
      else
        if (i mod 5) = 0 then
          SplashProgress('Working...', i / (list.Count - 1));
    end;
  end;
  HideSplash;

  document.write('</table>');

  document.write('</p>');
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.Flash;

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

      if inv2.SoldPartOutValue_uQtyAvg.percentage < 0.999 then
      begin
        inv2.Free;
        continue;
      end;

      invcost := invcost + inv2.SoldPartOutValue_uQtyAvg.value;
      numparts := numparts + inv2.totallooseparts;
      inv2.Free;
    end
    else
    begin
      if inv.SoldPartOutValue_uQtyAvg.percentage < 0.999 then
        continue;

      invcost := invcost + inv.SoldPartOutValue_uQtyAvg.value;
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

  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  DrawHeadLine('Used sets to built from scratch');

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
  aa := 0;
  for i := 0 to list.Count - 1 do
  begin
    inc(aa);
    setid := list.Strings[i];
    minv := inventory.InventoryForMissingToBuildSet(setid, 1);
    mvalue := minv.SoldPartOutValue_uQtyAvg.value;
    minv.Free;
    missing := inventory.MissingToBuildSet(setid);

    document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=65%><img width=64px src=s\' + setid + '.jpg><br>');
    document.write('<a href="sinv/' + setid + '">');
    document.write('<b>' + setid + '</b> - ' + db.SetDesc(setid));
    cls := list.Objects[i] as _costclass;
    document.write('</td><td width=15% align=right>');
    document.write(IntToStr(cls.numparts) + '</td>');
    document.write('</td><td width=15% align=right>');
    document.write(Format('%d<br>%2.2f%s', [missing, missing / cls.numparts * 100, '%']) + '</td>');
    document.write('</td><td width=15% align=right>');
    document.write('€ %2.2f</td>', [cls.setcost]);
    document.write('</td><td width=15% align=right>');
    document.write('€ %2.2f</td>', [cls.invcost]);
    document.write('</td><td width=15% align=right>');
    document.write('€ %2.2f<br>%2.2f%s</td>', [mvalue, mvalue / cls.invcost * 100, '%']);
    document.write('</td><td width=15% align=right>');
    document.write(Format('%2.3f%s', [cls.setcost / cls.invcost * 100, '%']) + '</td></tr>');

    if list.Count > 1 then
    begin
      if list.Count < 20 then
        SplashProgress('Working...', i / (list.Count - 1))
      else
        if (i mod 5) = 0 then
          SplashProgress('Working...', i / (list.Count - 1));
    end;
  end;
  HideSplash;

  document.write('</table>');

  document.write('</p>');
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.Flash;

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
    document.Flash;
    Screen.Cursor := crDefault;
    exit;
  end;
  missing := inv.totallooseparts;

  DrawHeadLine(Format('Missing items from storage bins (%d parts)', [missing]));

  DrawInventoryTable(inv);
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.Flash;
  inv.Free;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowMissingToBuildSetInventory(const setid: string; const numsets: integer);
var
  inv: TBrickInventory;
  missing: integer;
  s1: string;
  nparts: integer;
begin
  Screen.Cursor := crHourGlass;

  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');
  inv := inventory.InventoryForMissingToBuildSet(setid, numsets);
  if inv = nil then
  begin
    DrawHeadLine('Can not find missing inventory for ' + setid);
    document.write('<br>');
    document.write('</p>');
    document.write('</div>');
    document.write('</body>');
    document.Flash;
    Screen.Cursor := crDefault;
    exit;
  end;
  missing := inv.totallooseparts;
  s1 := basedefault + 'out\' + setid + '\';
  if not DirectoryExists(s1) then
    ForceDirectories(s1);
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
      decide(missing = 1, 'is', 'are') + ' missing to build a copy of this set %s (%2.2f%s)', [missing, inv.numlooseparts, setid, missing/db.GetSetInventory(setid).totallooseparts*100, '%']))
  else
  begin
    nparts := db.GetSetInventory(setid).totallooseparts;
    if nparts > 0 then
      DrawHeadLine(Format('%d part' + decide(missing = 1, '', 's') + ' in %d lots ' +
        decide(missing = 1, 'is', 'are') + ' missing to build %d copies of this set %s (%2.2f%s)', [missing, inv.numlooseparts, numsets, setid, missing / nparts / numsets * 100, '%']));
  end;

  DrawHeadLine(Format('<a href="missingtobuildset/%s/%d">Check to build %d sets</a>', [setid, numsets + 1, numsets + 1]));

  DrawInventoryTable(inv);
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
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
    document.Flash;
    Screen.Cursor := crDefault;
    exit;
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
  DrawHeadLine(Format('%d parts in %d lots are missing to build the following sets (%2.2f%s), total parts = %d,%s', [missing, inv.numlooseparts, missing/sinv.totallooseparts*100, '%', sinv.totallooseparts, s1]));

  DrawInventoryTable(inv);
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.Flash;
  inv.Free;
  sinv.Free;
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
    document.Flash;
    Screen.Cursor := crDefault;
    exit;
  end;

  miss1 := inv1.MissingToBuildInventory(inv2);
  miss2 := inv2.MissingToBuildInventory(inv1);
  missinv1 := inv1.InventoryForMissingToBuildInventory(inv2);
  missinv2 := inv2.InventoryForMissingToBuildInventory(inv1);

  b := dodraworderinfo;
  dodraworderinfo := false;

  document.write('<table width=100%><tr valign=top>');

  document.write('<td width=50%>');
  DrawHeadLine(Format('<a href="sinv/%s">%s - %s</a><br><br><img width=240px src=s\' + set1 + '.jpg>', [set1, set1, db.SetDesc(set1)]));
  DrawHeadLine(Format('%d parts in %d lots are missing from this set to build set %s (%2.2f%s)',
//    [miss1, missinv1.numlooseparts, set2, miss1/db.GetSetInventoryWithOutExtra(set2).totallooseparts*100, '%']));
    [miss1, missinv1.numlooseparts, set2, miss1 / inv2.totallooseparts * 100, '%']));
  DrawInventoryTable(missinv1, true);
  document.write('</td>');

  document.write('<td width=50%>');
  DrawHeadLine(Format('<a href="sinv/%s">%s - %s</a><br><br><img width=240px src=s\' + set2 + '.jpg>', [set2, set2, db.SetDesc(set2)]));
  DrawHeadLine(Format('%d parts in %d lots are missing from this set to build set %s (%2.2f%s)',
//    [miss2, missinv2.numlooseparts, set1, miss2/db.GetSetInventoryWithOutExtra(set1).totallooseparts*100, '%']));
    [miss2, missinv2.numlooseparts, set1, miss2 / inv1.totallooseparts * 100, '%']));
  DrawInventoryTable(missinv2, true);
  document.write('</td></tr></table>');

  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.Flash;
  missinv1.Free;
  missinv2.Free;
  dodraworderinfo := b;
  Screen.Cursor := crDefault;

end;

procedure TMainForm.ShowLooseParts(inv: TBrickInventory;
  colormask: Integer = -1; partmask: string = ''; cat: Integer = -1);
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
begin
  UpdateDismantaledsetsinv;

  if inv = nil then
  begin
    inv := inventory;
    if not DirectoryExists(basedefault + 'out\looseparts\') then
      ForceDirectories(basedefault + 'out\looseparts\');
    inv.StoreHistoryStatsRec(basedefault + 'out\looseparts\looseparts.stats');
  end;

  Screen.Cursor := crHourGlass;

  inv.SortPieces;
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
    DrawPartOutValue(inv);
  end;

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
      scolor := itoa(brick.color);
      document.write('<tr bgcolor=' + TBGCOLOR + '><td width=5% align=right>' + IntToStr(aa) + '.</td><td width=35%><img src=' + scolor + '\' + brick.part + '.png><br>');
      document.write('<b><a href=spiece/' + brick.part + '>' + brick.part + '</a></b>');
      document.write('<a href="inv/' + invs +'/P/' + decide(partmask='',brick.part,'') + '">' + ' - ' );
      document.write(db.PieceDesc(brick.part) + '</a> <a href=spiece/' + brick.part + '>...</a></td><td width=25%>');
      document.BlancColorCell(db.colors(brick.color).RGB, 20);
      document.write('<a href="inv/' + invs +'/C/' + IntToStr(decide(colormask=-1, brick.color, -1)) + '">');
      document.write(db.colors(brick.color).name + ' (' + scolor + ') (BL=' +
                     IntToStr(db.colors(brick.color).BrickLingColor) +  ')</a> <a href=spiecec/' +
                     brick.part + '/' + scolor + '><img src="images\details.png"></a></td>');

      pci := db.PieceColorInfo(brick.part, brick.color);
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
      pi := db.PieceInfo(brick.part);
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

  HideSplash;

  document.write('</tr></table></p>');
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  document.Flash;

  Screen.Cursor := crDefault;
end;

procedure TMainForm.HTMLHotSpotClick(Sender: TObject; const SRC: String;
  var Handled: Boolean);
begin
  HTMLClick(SRC, Handled);
end;

procedure TMainForm.HTMLClick(const SRC: String; var Handled: Boolean);
var
  s1, s2, s3, s4, s5: string;
  slink: string;
  scrollx, scrolly: integer;
  i: integer;
  idx: integer;
  inv: TBrickInventory;
begin
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
      Handled := true;
      exit;
    end;

    if Pos('StoreInventoryStatsRec/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, s3, '/');
      StoreInventoryStatsRec(s2, s3);
      Handled := true;
      exit;
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
      Handled := true;
      exit;
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
      Handled := true;
      exit;
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
      Handled := true;
      exit;
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
      Handled := true;
      exit;
    end;

    if SRC = 'refreshpieceall' then
    begin
      Screen.Cursor := crHourglass;
      ShowSplash;
      for i := 0 to db.AllPieces.Count - 1 do
        db.RefreshPart(db.AllPieces.Strings[i]);
      HideSplash;
      Screen.Cursor := crDefault;
      Handled := true;
      exit;
    end;

    if Pos('addset/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      inventory.StorePieceInventoryStatsRec(basedefault + 'out\' + s2 + '\' + s2 + '.history', s2, -1);
      inventory.AddSet(s2, false);
      inventory.StorePieceInventoryStatsRec(basedefault + 'out\' + s2 + '\' + s2 + '.history', s2, -1);
      btn_SaveClick(nil);
      HTMLClick('refresh', Handled);
      Handled := true;
      exit;
    end;

    if Pos('downloadset/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      if db.DownloadSetFromBricklink(s2) then
        HTMLClick('refresh', Handled)
      else
        MessageBeep(MB_ICONERROR);
      Handled := true;
      exit;
    end;

    if Pos('addsetdismantaled/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      inventory.StorePieceInventoryStatsRec(basedefault + 'out\' + s2 + '\' + s2 + '.history', s2, -1);
      inventory.AddSet(s2, true);
      inventory.StorePieceInventoryStatsRec(basedefault + 'out\' + s2 + '\' + s2 + '.history', s2, -1);
      btn_SaveClick(nil);
      HTMLClick('refresh', Handled);
      Handled := true;
      exit;
    end;

    if Pos('removeset/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      inventory.RemoveSet(s2, false);
      btn_SaveClick(nil);
      HTMLClick('refresh', Handled);
      Handled := true;
      exit;
    end;

    if Pos('removesetdismantaled/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      inventory.RemoveSet(s2, true);
      btn_SaveClick(nil);
      HTMLClick('refresh', Handled);
      Handled := true;
      exit;
    end;

    if Pos('diagrampiece/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, s3, '/');
      DiagramPiece(s2, atoi(s3));
      Handled := true;
      exit;
    end;

    if Pos('diagramstorage/', SRC) = 1 then
    begin
      if SRC = 'diagramstorage/Storage Bins' then
      begin
        Screen.Cursor := crHourglass;
        inv := db.InventoryForAllStorageBins;
        inv.StoreHistoryStatsRec(basedefault + 'storage\storagebins.stats');
        inv.Free;
        Screen.Cursor := crDefault;
      end;
      splitstring(SRC, s1, s2, '/');
      DiagramStorage(s2);
      Handled := true;
      exit;
    end;

    if Pos('diagramorder/', SRC) = 1 then
    begin
      splitstring(SRC, s1, s2, '/');
      DiagramOrder(s2);
      Handled := true;
      exit;
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
        Handled := true;
        exit;
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
        Handled := true;
        exit;
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

  Handled := true;
  AddressEdit.Text := slink;
  if Pos('inv/', slink) = 1 then
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
  else if Pos('ShowSetsAtYear/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    ShowSetsAtYear(atoi(s2));
  end
  else if Pos('multymissing/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    ShowMissingToBuilMultipledSets(TStringList(StrToInt(s2)));
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
  else if Pos('spiece/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    ShowPiece(s2);
  end
  else if Pos('spiecec/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, s3, '/');
    ShowColorPiece(s2, StrToIntDef(s3, 0));
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
    ShowMissingToBuildSetInventory(s2, StrToIntDef(s3, 1));
  end
  else if Pos('UsedPiecesbeloweuroKgr/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    UsedPiecesbeloweuroKgr(atoi(s2));
  end
  else if slink = 'ShowMySetsPieces' then
  begin
    ShowMySetsPieces;
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
    ShowSetsForPartOutNew;
  end
  else if Pos('ShowSetsForPartOutUsed/', slink) = 1 then
  begin
    ShowSetsForPartOutUsed;
  end
  else if Pos('ShowSetsForPartInUsed/', slink) = 1 then
  begin
    splitstring(slink, s1, s2, '/');
    ShowSetsForPartInUsed(atoi(s2));
  end
  else if slink = 'ShowMissingFromStorageBins' then
  begin
   ShowMissingFromStorageBins;
  end
  else
    Handled := false;

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
      frm.mosaic.inventory.SaveLooseParts(basedefault + 'mosaic\' + fname + '.txt');
      frm.mosaic.Bitmap.SaveToFile(basedefault + 'mosaic\' + fname + '.bmp');
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
  HTMLClick('ShowSetsForPartOutNew/', foo);
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
  HTMLClick('ShowSetsForPartOutUsed/', foo);
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
begin
  lst := TStringList.Create;

  inv := TBrickInventory.Create;

  for i := 0 to MAXINFOCOLOR do
    if db.Colors(i).id = i then
      if db.Colors(i).knownpieces <> nil then
        for j := 0 to db.Colors(i).knownpieces.Count - 1 do
        begin
          pcs := db.Colors(i).knownpieces.Strings[j];
          pi := db.PieceInfo(pcs);
          weight := pi.Weight;
          if weight > 0 then
          begin
            pci := db.PieceColorInfo(pcs, i);
            if pci <> nil then
            begin
              cost := pci.EvaluatePriceUsed;
              if cost > 0.0 then
              begin
                perkgr := dbl_safe_div(cost, weight) * 1000;
                if perkgr <= x then
                begin
                  lst.Add(pcs + ',' + itoa(i));
                  inv.AddLoosePart(pcs, i, 1);
                end;
              end;
            end;
          end;
        end;

  lst.Sort;
  DrawPieceList('Used pieces with price below ' + itoa(x) + ' euro/Kgr', lst);
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

  document.write('<body background="splash.jpg">');
  DrawNavigateBar;
  document.write('<div style="color:' + DFGCOLOR + '">');
  document.write('<p align=center>');

  inv := TBrickInventory.Create;
  for i := 0 to inventory.numsets - 1 do
    for j := 1 to inventory.sets[i].num do
      inv.AddSet(inventory.sets[i].setid, false);
  inv.DismandalAllSets;
  inv.SortPieces;

  DrawHeadLine('My builded sets Inventory');

  DrawInventoryTable(inv, false);
  document.write('<br>');
  document.write('<br>');
  document.write('</p>');
  document.write('</div>');
  document.write('</body>');
  inv.Free;
  document.Flash;
  Screen.Cursor := crDefault;

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
      CopyFile(fname, fname2);
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
      pci := db.PieceColorInfo(pcs, color);
      if pci <> nil then
      begin
        cost := pci.EvaluatePriceNew;
        if cost > 0.0 then
        begin
          perkgr := dbl_safe_div(cost, weight) * 1000;
          if perkgr >= price then
            if pci.nDemand >= demand / 100 then
              if pci.priceguide.nTotalQty > sold then
              begin
                lst2.Add(pcs + ',' + itoa(color));
                inv.AddLoosePart(pcs, color, 1);
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
  ShowMessage(Format('Maximum:'#13#10'%d hits / %d total'#13#10'Efficiency = %2.2f%s'#13#10#13#10'Current:'#13#10'%d hits / %d total'#13#10'Efficiency = %2.2f%s',
    [hits, total, dbl_safe_div(hits * 100, total), '%', db.pciloadscache, db.pciloads, dbl_safe_div(db.pciloadscache * 100, db.pciloads), '%']));
end;

end.


