unit frm_diagrams;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, TeeProcs, TeEngine, Chart, StdCtrls, Series;

type
  TDiagramForm = class(TForm)
    Chart1: TChart;
    Label1: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Image1: TImage;
    Label2: TLabel;
    Edit1: TEdit;
    Series1: TLineSeries;
    Notebook1: TNotebook;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Button19: TButton;
    Button20: TButton;
    Button21: TButton;
    Button22: TButton;
    Button23: TButton;
    Button24: TButton;
    Button25: TButton;
    Button26: TButton;
    Button27: TButton;
    Button28: TButton;
    Button29: TButton;
    Button30: TButton;
    Button31: TButton;
    Button32: TButton;
    Button33: TButton;
    Button34: TButton;
    Button35: TButton;
    Button36: TButton;
    Button37: TButton;
    Button38: TButton;
    Button39: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
    procedure Button23Click(Sender: TObject);
    procedure Button24Click(Sender: TObject);
    procedure Button25Click(Sender: TObject);
    procedure Button26Click(Sender: TObject);
    procedure Button27Click(Sender: TObject);
    procedure Button28Click(Sender: TObject);
    procedure Button29Click(Sender: TObject);
    procedure Button30Click(Sender: TObject);
    procedure Button31Click(Sender: TObject);
    procedure Button32Click(Sender: TObject);
    procedure Button33Click(Sender: TObject);
    procedure Button38Click(Sender: TObject);
    procedure Button39Click(Sender: TObject);
    procedure Button34Click(Sender: TObject);
    procedure Button35Click(Sender: TObject);
    procedure Button36Click(Sender: TObject);
    procedure Button37Click(Sender: TObject);
  private
    { Private declarations }
  protected
    piece: string;
    color: integer;
  public
    { Public declarations }
  end;

procedure DiagramPiece(const part: string; const color: integer);

procedure DiagramStorage(const storage: string);

procedure DiagramOrder(const order: string);

implementation

{$R *.dfm}

uses
  bi_db, bi_delphi, bi_utils, DateUtils, StrUtils, bi_priceadjust, bi_orders;

type
  parecarray_t = array[0..$1fff] of parecdate_t;
  parecarray_p = ^parecarray_t;

  extracacheinfo_t = record
    dir: string[128];
    date: TDateTime;
  end;

function s2date1(const s: string): TDateTime;
var
  ayear, amonth, aday: word;
begin
  if length(s) <> 8 then
  begin
    result := 0.0;
    exit;
  end;
  ayear := atoi(leftstr(s, 4));
  amonth := atoi(rightstr(leftstr(s, 6), 2));
  aday := atoi(rightstr(s, 2));
  if not TryEncodeDateTime(ayear, amonth, aday, 0, 0, 0, 0, result) then
    result := 0.0;
end;


procedure SortParecArray(const A: parecarray_p; const num: integer);

  procedure QuickSort(iLo, iHi: Integer);
  var
     Lo, Hi: integer;
     Pivot: TDateTime;
     T: parecdate_t;
  begin
    Lo := iLo;
    Hi := iHi;
    Pivot := A[(Lo + Hi) div 2].date;
    repeat
      while A[Lo].date < Pivot do Inc(Lo);
      while A[Hi].date > Pivot do Dec(Hi);
      if Lo <= Hi then
      begin
        T := A[Lo];
        A[Lo] := A[Hi];
        A[Hi] := T;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then QuickSort(iLo, Hi);
    if Lo < iHi then QuickSort(Lo, iHi);
  end;

begin
  if num > 0 then
    QuickSort(0, num - 1);
end;

const
  PG_nTimesSold = 1;
  PG_nTotalQty = 2;
  PG_nMinPrice = 3;
  PG_nAvgPrice = 4;
  PG_nQtyAvgPrice = 5;
  PG_nMaxPrice = 6;
  PG_uTimesSold = 7;
  PG_uTotalQty = 8;
  PG_uMinPrice = 9;
  PG_uAvgPrice = 10;
  PG_uQtyAvgPrice = 11;
  PG_uMaxPrice = 12;
  AV_nTotalLots = 13;
  AV_nTotalQty = 14;
  AV_nMinPrice = 15;
  AV_nAvgPrice = 16;
  AV_nQtyAvgPrice = 17;
  AV_nMaxPrice = 18;
  AV_uTotalLots = 19;
  AV_uTotalQty = 20;
  AV_uMinPrice = 21;
  AV_uAvgPrice = 22;
  AV_uQtyAvgPrice = 23;
  AV_uMaxPrice = 24;
  N_demand = 25;
  U_demand = 26;
  D_numpieces = 27;
  N_partout = 28;
  U_partout = 29;

const
  C_TITLES: array[1..40] of string = (
    'PG_nTimesSold',
    'PG_nTotalQty',
    'PG_nMinPrice',
    'PG_nAvgPrice',
    'PG_nQtyAvgPrice',
    'PG_nMaxPrice',
    'PG_uTimesSold',
    'PG_uTotalQty',
    'PG_uMinPrice',
    'PG_uAvgPrice',
    'PG_uQtyAvgPrice',
    'PG_uMaxPrice',
    'AV_nTotalLots',
    'AV_nTotalQty',
    'AV_nMinPrice',
    'AV_nAvgPrice',
    'AV_nQtyAvgPrice',
    'AV_nMaxPrice',
    'AV_uTotalLots',
    'AV_uTotalQty',
    'AV_uMinPrice',
    'AV_uAvgPrice',
    'AV_uQtyAvgPrice',
    'AV_uMaxPrice',
    'N_demand',
    'U_demand',
    'Inventory history',
    'N_partoutvalue',
    'U_partoutvalue',
    'PG_nAvgPrice',
    'PG_nQtyAvgPrice',
    'PG_uAvgPrice',
    'PG_uQtyAvgPrice',
    'AV_nAvgPrice',
    'AV_nQtyAvgPrice',
    'AV_uAvgPrice',
    'AV_uQtyAvgPrice',
    'N_demand',
    'U_demand',
    'E_costeval'
  );

var
  NNN: TDateTime;

procedure MakeChart(const c: TChart; const piece: string; const color: integer; const idx: integer);
var
  A: parecarray_p;
  num: integer;
  sname: string;
  s: TStringList;
  fname: string;
  EXTRA: array[0..127] of extracacheinfo_t;
  numextra: integer;
  i, j, k: integer;
  s1, s2: string;
  f: TFileStream;
  x, y: double;
  mx: double;
  ddxxx: double;
  dd20141216, dd20141220: double;
  h: pieceinventoryhistory_t;
  h2: brickstatshistory_t;
  st: set_t;
  e: orderevalhistory_t;
  list40: TStringList;
  o40, d40, e40: string;
  zz: boolean;
  list27a, list27b: TStringList;
  check27: string;
  len27a: integer;
  len27b: integer;
  string27: string;
  date27: double;
  num27: integer;
  snum27: string;
  date20150403: integer;
begin
  if idx = 40 then
  begin
    fname := basedefault + 'orders\' + piece + '.eval';
    if fexists(fname) then
    begin
      list40 := TStringList.Create;
      if fexists(basedefault + 'orders\eval_history.txt') then
      begin
        list40.LoadFromFile(basedefault + 'orders\eval_history.txt');
        if list40.Count > 0 then
        begin
          if list40.Strings[0] = 'Order,Date,Eval' then
            list40.Delete(0)
          else
            list40.Clear;
        end;
      end;
      c.Series[0].Clear;
      c.Series[0].XValues.DateTime := True;
      c.BottomAxis.DateTimeFormat := 'dd/mm/yyyy';

      c.Title.Text.Text := C_TITLES[idx];

      zz := false;
      for i := 0 to list40.Count - 1 do
      begin
        splitstring(list40.strings[i], o40, d40, e40, ',');
        if o40 = piece then
        begin
          x := s2date1(d40);
          y := atof(e40);
          if not zz then
          begin
            c.Series[0].AddXY(x, 0.0);
            zz := true;
          end;
          c.Series[0].AddXY(x, y);
        end;
      end;

      list40.Free;

      f := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
      for i := 0 to (f.Size div SizeOf(orderevalhistory_t)) - 1 do
      begin
        f.Read(e, SizeOf(orderevalhistory_t));
        x := e.time;
        y := e.eval;
        if not zz then
          if i = 0 then
          begin
            c.Series[0].AddXY(x, 0.0);
            zz := true;
          end;
        c.Series[0].AddXY(x, y);
      end;

      f.Free;
    end;

    exit;
  end;

  if (idx >= 30) and (idx <=39) then
  begin
    date20150403 := round(s2date1('20150403'));

    if piece = 'Storage Bins' then
      fname := basedefault + 'storage\storagebins.stats'
    else
      fname := basedefault + 'storage\storage_' + filenamestring(piece) + '.stats';
    if fexists(fname) then
    begin
      c.Series[0].Clear;
      c.Series[0].XValues.DateTime := True;
      c.BottomAxis.DateTimeFormat := 'dd/mm/yyyy';

      c.Title.Text.Text := C_TITLES[idx];

      f := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
      for i := 0 to (f.Size div SizeOf(brickstatshistory_t)) - 1 do
      begin
        f.Read(h2, SizeOf(brickstatshistory_t));
        x := h2.time;
        if idx = 30 then
          y := h2.Sold_nAvg.value
        else if idx = 31 then
          y := h2.Sold_nQtyAvg.value
        else if idx = 32 then
          y := h2.Sold_uAvg.value
        else if idx = 33 then
          y := h2.Sold_uQtyAvg.value
        else if idx = 34 then
          y := h2.Avail_nAvg.value
        else if idx = 35 then
          y := h2.Avail_nQtyAvg.value
        else if idx = 36 then
          y := h2.Avail_uAvg.value
        else if idx = 37 then
          y := h2.Avail_uQtyAvg.value
        else if idx = 38 then
          y := h2.nDemand.value
        else
          y := h2.uDemand.value;
        if i = 0 then
          c.Series[0].AddXY(x, 0.0);
        if trunc(x) <> date20150403 then
          c.Series[0].AddXY(x, y);
      end;

      f.Free;
    end;
    exit;
  end;

  if (idx = 28) or (idx = 29) then
  begin
    fname := basedefault + 'out\' + piece + '\' + piece + '.stats';
    if fexists(fname) then
    begin
      c.Series[0].Clear;
      c.Series[0].XValues.DateTime := True;
      c.BottomAxis.DateTimeFormat := 'dd/mm/yyyy';

      c.Title.Text.Text := C_TITLES[idx];

      f := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
      for i := 0 to (f.Size div SizeOf(brickstatshistory_t)) - 1 do
      begin
        f.Read(h2, SizeOf(brickstatshistory_t));
        x := h2.time;
        if idx = 28 then
          y := h2.Sold_nQtyAvg.value
        else
          y := h2.Sold_uQtyAvg.value;
        if i = 0 then
          c.Series[0].AddXY(x, 0.0);
        c.Series[0].AddXY(x, y);
      end;

      f.Free;
    end;
    exit;
  end;

  if idx = 27 then
  begin
    if color = -1 then
      fname := basedefault + 'out\' + piece + '\' + piece + '.history'
    else
      fname := basedefault + 'cache\' + IntToStr(color) + '\' + piece + '.history';
    if fexists(fname) then
    begin
      c.Series[0].Clear;
      c.Series[0].XValues.DateTime := True;
      c.BottomAxis.DateTimeFormat := 'dd/mm/yyyy';

      c.Title.Text.Text := C_TITLES[idx];
      c.Series[0].AddXY(s2date1('20140401'), 0.0);

      if color > -1 then
        if fexists(basedefault + 'history\parts.txt') then
        begin
          list27a := TStringList.Create;
          list27b := TStringList.Create;
          list27a.LoadFromFile(basedefault + 'history\parts.txt');
          list27a.Sort;
          check27 := piece + ',' + itoa(color) + ',';
          len27a := length(check27);
          for i := 0 to list27a.Count - 1 do
            if fexists(basedefault + 'history\' + list27a.Strings[i]) then
              if length(list27a.Strings[i]) = 20 then
              begin
                list27b.LoadFromFile(basedefault + 'history\' + list27a.Strings[i]);
                num27 := 0;
                for j := 1 to list27b.Count - 1 do
                begin
                  string27 := list27b.Strings[j];
                  if Pos(check27, string27) = 1 then
                  begin
                    snum27 := '';
                    for k := len27a + 1 to length(string27) do
                      snum27 := snum27 + string27[k];
                    num27 := num27 + atoi(snum27);
                  end;
                end;
                date27 := s2date1(
                  list27a.Strings[i][13] + list27a.Strings[i][14] + list27a.Strings[i][15] + list27a.Strings[i][16] +
                  list27a.Strings[i][17] + list27a.Strings[i][18] + list27a.Strings[i][19] + list27a.Strings[i][20]
                );
                c.Series[0].AddXY(date27, num27);

              end;
          list27a.Free;
          list27b.Free;
        end;

      f := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
      for i := 0 to (f.Size div SizeOf(pieceinventoryhistory_t)) - 1 do
      begin
        f.Read(h, SizeOf(pieceinventoryhistory_t));
        x := h.time;
        if color = -1 then
          y := h.nbuilded
        else
          y := h.nnew + h.nused;
        c.Series[0].AddXY(x, y);
      end;
      if color = -1 then
      begin
        inventory.GetSetInfo(piece, @st);
        c.Series[0].AddXY(Now, st.num);
      end
      else
      begin
        c.Series[0].AddXY(Now, inventory.LoosePartCount(piece, color));
      end;

      f.Free;
    end;
    exit;
  end;
  
  NNN := Now;
  numextra := 1;
  EXTRA[0].dir := 'cache';
  EXTRA[0].date := s2date1('20141101');
  sname := basedefault + 'cache\additional.txt';
  if fexists(sname) then
  begin
    s := TStringList.Create;
    try
      s.LoadFromFile(sname);
      for i := 0 to s.Count - 1 do
      begin
        splitstring(s.Strings[i], s1, s2, ',');
        EXTRA[numextra].dir := s1;
        EXTRA[numextra].date := s2date1(s2);
        inc(numextra);
      end;
    finally
      s.Free;
    end;
  end;

  num := 0;
  new(A);
  for i := 0 to numextra - 1 do
  begin
    fname := basedefault + EXTRA[i].dir + '\' + itoa(decide(color = -1, 9999, color)) + '\' + piece + '.cache';
    if not fexists(fname) then
      continue;
    try
      f := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
    except
      sleep(50);
      f := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
    end;
    if f.Size = 160 then
    begin
      f.Read(A[num].priceguide, SizeOf(priceguide_t));
      f.Read(A[num].availability, SizeOf(availability_t));
      A[num].date := EXTRA[i].date;
      inc(num);
    end
    else if f.Size mod SizeOf(parecdate_t) = 0 then
    begin
      for j := 1 to (f.Size div SizeOf(parecdate_t)) do
      begin
        f.Read(A[num], SizeOf(parecdate_t));
        if j >= 1 then
        begin
          if A[num].date < s2date1('20141101') then
          begin
            A[num].date := s2date1('20141101') + j - 2;
            if A[num].date > s2date1('20141121') then
              A[num].date := s2date1('20141121');
          end;
        end
        else if j = 1 then
        begin
          if A[num].date < EXTRA[i].date then
            A[num].date := EXTRA[i].date
        end;
        inc(num);
      end;
    end;
    f.free;
  end;

  for i := 0 to num - 1 do
  begin
    PRICEADJUST(piece, color, @A[i]);
    if A[i].date < s2date1('20140401') then
      A[i].date := s2date1('20140401');
  end;

  SortParecArray(A, num);

  c.Series[0].Clear;
  c.Series[0].XValues.DateTime := True;
  c.BottomAxis.DateTimeFormat := 'dd/mm/yyyy';

  c.Title.Text.Text := C_TITLES[idx];

//  c.LeftAxis.StartPosition := 0.0;
  mx := 0.0;

  if num > 0 then
    c.Series[0].AddXY(A[0].date, 0.0);

  ddxxx := s2date1('20141101');
  dd20141216 := s2date1('20141216');
  dd20141220 := s2date1('20141220');
  for i := 0 to num - 1 do
  begin
    x := A[i].date;
    if x = ddxxx then
      continue;
    if (x >= dd20141216) and (x <= dd20141220) then
      continue;
    case idx of
      PG_nTimesSold: y := A[i].priceguide.nTimesSold;
      PG_nTotalQty: y := A[i].priceguide.nTotalQty;
      PG_nMinPrice: y := A[i].priceguide.nMinPrice;
      PG_nAvgPrice: y := A[i].priceguide.nAvgPrice;
      PG_nQtyAvgPrice: y := A[i].priceguide.nQtyAvgPrice;
      PG_nMaxPrice: y := A[i].priceguide.nMaxPrice;
      PG_uTimesSold: y := A[i].priceguide.uTimesSold;
      PG_uTotalQty: y := A[i].priceguide.uTotalQty;
      PG_uMinPrice: y := A[i].priceguide.uMinPrice;
      PG_uAvgPrice: y := A[i].priceguide.uAvgPrice;
      PG_uQtyAvgPrice: y := A[i].priceguide.uQtyAvgPrice;
      PG_uMaxPrice: y := A[i].priceguide.uMaxPrice;
      AV_nTotalLots: y := A[i].availability.nTotalLots;
      AV_nTotalQty: y := A[i].availability.nTotalQty;
      AV_nMinPrice: y := A[i].availability.nMinPrice;
      AV_nAvgPrice: y := A[i].availability.nAvgPrice;
      AV_nQtyAvgPrice: y := A[i].availability.nQtyAvgPrice;
      AV_nMaxPrice: y := A[i].availability.nMaxPrice;
      AV_uTotalLots: y := A[i].availability.uTotalLots;
      AV_uTotalQty: y := A[i].availability.uTotalQty;
      AV_uMinPrice: y := A[i].availability.uMinPrice;
      AV_uAvgPrice: y := A[i].availability.uAvgPrice;
      AV_uQtyAvgPrice: y := A[i].availability.uQtyAvgPrice;
      AV_uMaxPrice: y := A[i].availability.uMaxPrice;
      N_demand: y := F_nDemand(A[i].availability, A[i].priceguide);
      U_demand: y := F_uDemand(A[i].availability, A[i].priceguide);
    else
      y := 0.0;
    end;
    c.Series[0].AddXY(x, y);
    if y > mx then
      mx := y;
  end;
//  c.LeftAxis.EndPosition := mx * 1.05;
  dispose(A);
end;

procedure DiagramPiece(const part: string; const color: integer);
var
  f: TDiagramForm;
  pci: TPieceColorInfo;
  st: set_t;
begin
  f := TDiagramForm.Create(nil);
  try
    f.Notebook1.ActivePage := f.Notebook1.Pages[1];
    f.Label1.Caption := db.PieceDesc(part);
    if color = -1 then
    begin
      f.Label4.Visible := false;
      f.Button28.Visible := true;
      f.Button29.Visible := true;
    end
    else
    begin
      f.Label4.Caption := db.colors(color).name;
      f.Button28.Visible := false;
      f.Button29.Visible := false;
    end;
    pci := db.PieceColorInfo(part, color);
    PieceToImage(f.Image1, part, color);
    if pci <> nil then
    begin
      f.Panel1.Color := RGBInvert(db.colors(color).RGB);
      if color = -1 then
      begin
        inventory.GetSetInfo(part, @st);
        f.Edit1.Text := itoa(st.num) + ' builded - ' + itoa(st.dismantaled) + ' dismantaled';
        f.Label2.Caption := 'Num sets:';
        f.Panel1.Visible := false;
      end
      else
      begin
        f.Edit1.Text := itoa(inventory.LoosePartCount(part, color));
        f.Label2.Caption := 'Num pieces:';
        f.Panel1.Visible := true;
      end;
      f.piece := part;
      f.color := color;
      Screen.Cursor := crHourglass;
      MakeChart(f.Chart1, part, color, 5);
      Screen.Cursor := crDefault;
      f.ShowModal;
    end;
  finally
    f.Free;
  end;
end;

procedure DiagramStorage(const storage: string);
var
  f: TDiagramForm;
begin
  f := TDiagramForm.Create(nil);
  try
    f.Notebook1.ActivePage := f.Notebook1.Pages[0];
    f.Label1.Caption := storage;
    f.Label4.Visible := false;
    f.Edit1.Visible := false;
    f.Label2.Visible := false;
    f.Panel1.Visible := false;
    f.piece := storage;
    f.color := -1;
    Screen.Cursor := crHourglass;
    MakeChart(f.Chart1, storage, -1, 33);
    Screen.Cursor := crDefault;
    f.ShowModal;
  finally
    f.Free;
  end;
end;

procedure DiagramOrder(const order: string);
var
  f: TDiagramForm;
begin
  f := TDiagramForm.Create(nil);
  try
    f.Notebook1.ActivePage := f.Notebook1.Pages[2];
    f.Label1.Caption := 'Order #' + order;
    f.Label4.Visible := false;
    f.Edit1.Visible := false;
    f.Label2.Visible := false;
    f.Panel1.Visible := false;
    f.Width := f.Width - f.Notebook1.Width;
    f.Notebook1.Width := 0;
    f.piece := order;
    f.color := -1;
    Screen.Cursor := crHourglass;
    MakeChart(f.Chart1, order, -1, 40);
    Screen.Cursor := crDefault;
    f.ShowModal;
  finally
    f.Free;
  end;
end;

procedure TDiagramForm.Button1Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 1);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button2Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 2);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button3Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 3);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button4Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 4);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button5Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 5);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button6Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 6);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button7Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 7);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button8Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 8);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button9Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 9);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button10Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 10);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button11Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 11);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button12Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 12);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button13Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 13);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button14Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 14);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button15Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 15);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button16Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 16);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button17Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 17);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button18Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 18);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button19Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 19);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button20Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 20);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button21Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 21);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button22Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 22);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button23Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 23);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button24Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 24);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button25Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 25);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button26Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 26);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button27Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 27);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button28Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 28);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button29Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 29);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button30Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 30);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button31Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 31);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button32Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 32);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button33Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 33);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button38Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 38);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button39Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 39);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button34Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 34);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button35Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 35);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button36Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 36);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button37Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  MakeChart(Chart1, piece, color, 37);
  Screen.Cursor := crDefault;
end;

end.
