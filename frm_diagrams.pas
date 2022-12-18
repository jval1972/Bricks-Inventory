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
//    Chart Diagrams Form
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit frm_diagrams;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, TeeProcs, TeEngine, Chart, StdCtrls, Series, Menus,
  Buttons, Grids, ValEdit, ComCtrls, clipbrd;

type
  TDiagramForm = class(TForm)
    DescLabel: TLabel;
    ColorLabel: TLabel;
    ColorPanel: TPanel;
    Label2: TLabel;
    Edit1: TEdit;
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
    ScrollBox1: TScrollBox;
    Image1: TImage;
    ExcludeDaysButton1: TButton;
    Button41: TButton;
    Button42: TButton;
    Button43: TButton;
    Button44: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Button45: TButton;
    Button46: TButton;
    Button47: TButton;
    Button48: TButton;
    PopupMenu1: TPopupMenu;
    CopyChartToClipboard1: TMenuItem;
    NumSamplesLabel: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Chart1: TChart;
    Series1: TLineSeries;
    ValueListEditor1: TValueListEditor;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    Panel3: TPanel;
    CopySpeedButton1: TSpeedButton;
    UpdateSpeedButton1: TSpeedButton;
    CurrencyButton1: TSpeedButton;
    CurrencyButton2: TSpeedButton;
    CurrencyButton3: TSpeedButton;
    CurrencyButton4: TSpeedButton;
    CurrencyButton5: TSpeedButton;
    CurrencyButton6: TSpeedButton;
    CurrencyButton7: TSpeedButton;
    CurrencyButton8: TSpeedButton;
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
    procedure Image1DblClick(Sender: TObject);
    procedure ExcludeDaysButton1Click(Sender: TObject);
    procedure Button41Click(Sender: TObject);
    procedure Button42Click(Sender: TObject);
    procedure Button43Click(Sender: TObject);
    procedure Button44Click(Sender: TObject);
    procedure Button45Click(Sender: TObject);
    procedure Button46Click(Sender: TObject);
    procedure Button47Click(Sender: TObject);
    procedure Button48Click(Sender: TObject);
    procedure CopyChartToClipboard1Click(Sender: TObject);
    procedure CopySpeedButton1Click(Sender: TObject);
    procedure UpdateSpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure CurrencyButtonClick(Sender: TObject);
  private
    { Private declarations }
    fcurrency: string;
  protected
    piece: string;
    color: integer;
    procedure DoMakeChart(const idx: integer);
  public
    { Public declarations }
  end;

procedure DiagramPiece(const part: string; const color: integer; const chid: integer = 5);

procedure DiagramStorage(const storage: string);

procedure DiagramOrder(const order: string);

implementation

{$R *.dfm}

uses
  bi_db, bi_delphi, bi_utils, bi_orders, bi_pghistory, bi_globals;

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
  PG_nAverageLOT = 41;
  PG_uAverageLOT = 42;
  AV_nAverageLOT = 43;
  AV_uAverageLOT = 44;
  PG_nuPriceRatio = 45;
  AV_nuPriceRatio = 46;
  PG_nuQtyRatio = 47;
  AV_nuQtyRatio = 48;

const
  C_TITLES: array[1..48] of string = (
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
    'E_costeval',
    'PG_nAverageLOT',
    'PG_uAverageLOT',
    'AV_nAverageLOT',
    'AV_uAverageLOT',
    'PG_nuPriceRatio',
    'AV_nuPriceRatio',
    'PG_nuQtyRatio',
    'AV_nuQtyRatio'
  );

procedure MakeChart(const c: TChart; const piece: string; const color: integer; const idx: integer);
var
  A: parecarray_p;
  num: integer;
  fname: string;
  numextra: integer;
  i, j, k: integer;
  f: TFileStream;
  x, y: double;
  mx: double;
//  ddxxx: double;
//  dd20141216, dd20141220: double;
  dd20151218: double;
  dd20160119: double;
  dd20160123: double;
  dd20160604: double;
  dd20170412: double;
  dd20170426: double;
  dd20170430: double;
  dd20170510: double;
  dd20170614: double;
  dd20171220: double;
  dd20170418: double;
  dd20180207: double;
  dd20160218: double;
  dd20160309: double;
  dd20180601: double;
  dd20180904: double;
  truncx: integer;
  dd20170102: double;
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
  date20190402: integer;
  date20190405: integer;
  ii: integer;
  num_actual: integer;
begin
  c.Tag := idx;

  // Order
  if idx = 40 then
  begin
    date20190402 := round(s2date1('20190402'));
    date20190405 := round(s2date1('20190405'));
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
          truncx := Trunc(x);
          if truncx >= date20190402 then
            if truncx <= date20190405 then
              Continue;
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
      j := 0;
      for i := 0 to (f.Size div SizeOf(orderevalhistory_t)) - 1 do
      begin
        f.Read(e, SizeOf(orderevalhistory_t));
        x := e.time;
        truncx := Trunc(x);
        if truncx >= date20190402 then
          if truncx <= date20190405 then
            Continue;
        y := e.eval;
        if not zz then
          if j = 0 then
          begin
            c.Series[0].AddXY(x, 0.0);
            zz := true;
          end;
        c.Series[0].AddXY(x, y);
        inc(j);
      end;

      f.Free;
    end;

    Exit;
  end;

  // Storage Bins
  if (idx >= 30) and (idx <=39) then
  begin
    date20150403 := round(s2date1('20150403'));
    date20190402 := round(s2date1('20190402'));
    date20190405 := round(s2date1('20190405'));

    if piece = 'Storage Bins' then
      fname := basedefault + 'storage\storagebins.stats'
    else if piece = 'Loose Parts' then
      fname := basedefault + 'out\looseparts\looseparts.stats'
    else
      fname := basedefault + 'storage\storage_' + filenamestring(piece) + '.stats';
    if fexists(fname) then
    begin
      c.Series[0].Clear;
      c.Series[0].XValues.DateTime := True;
      c.BottomAxis.DateTimeFormat := 'dd/mm/yyyy';

      c.Title.Text.Text := C_TITLES[idx];

      f := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
      j := 0;
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
        truncx := Trunc(x);
        if truncx <> date20150403 then
          if (truncx < date20190402) or (truncx > date20190405) then
          begin
            if j = 0 then
              c.Series[0].AddXY(x, 0.0);
            c.Series[0].AddXY(x, y);
            inc(j);
          end;
      end;

      f.Free;
    end;
    Exit;
  end;

  // N_PartOut, U_PartOut
  if (idx = 28) or (idx = 29) then
  begin
    if (color = -1) or (color = 9999) then
    begin
      fname := basedefault + 'out\' + piece + '\' + piece + '.stats';
      if color = -1 then
        if not fexists(fname) then
          fname := basedefault + 'cache\' + itoa(color) + '\' + piece + '.stats';
    end
    else
      fname := basedefault + 'cache\' + itoa(color) + '\' + piece + '.stats';
    if fexists(fname) then
    begin
      dd20160218 := s2date1('20160218');
      dd20160309 := s2date1('20160309');
      date20190402 := round(s2date1('20190402'));
      date20190405 := round(s2date1('20190405'));

      c.Series[0].Clear;
      c.Series[0].XValues.DateTime := True;
      c.BottomAxis.DateTimeFormat := 'dd/mm/yyyy';

      c.Title.Text.Text := C_TITLES[idx];

      f := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
      ii := 0;
      for i := 0 to (f.Size div SizeOf(brickstatshistory_t)) - 1 do
      begin
        f.Read(h2, SizeOf(brickstatshistory_t));

        x := h2.time;
        if mustexclude(piece, -1, x) then
          Continue;
        if x >= dd20160218 then
          if x <= dd20160309 then
            Continue;
        truncx := Trunc(x);
        if truncx >= date20190402 then
          if truncx <= date20190405 then
            Continue;

        if idx = 28 then
          y := h2.Sold_nQtyAvg.value
        else
          y := h2.Sold_uQtyAvg.value;
        if ii = 0 then
          c.Series[0].AddXY(x, 0.0);
        if y > 0.0001 then
          c.Series[0].AddXY(x, y);
        inc(ii);
      end;

      f.Free;
    end;
    Exit;
  end;

  dd20170614 := s2date1('20170614');
  dd20171220 := s2date1('20171220');
  dd20170418 := s2date1('20170418');
  dd20180207 := s2date1('20180207');

  // My inventory
  if idx = 27 then
  begin
    dd20151218 := s2date1('20151218');
    dd20160119 := s2date1('20160119');
    dd20160123 := s2date1('20160123');
    dd20160604 := s2date1('20160604');
    dd20170102 := s2date1('20170102');
    dd20170412 := s2date1('20170412');
    dd20170426 := s2date1('20170426');
    dd20170430 := s2date1('20170430');
    dd20170510 := s2date1('20170510');
    dd20180601 := s2date1('20180601');
    dd20180904 := s2date1('20180904');

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
                  if Pos1(check27, string27) then
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
                if Trunc(date27) <> Trunc(dd20151218) then
                  if Trunc(date27) <> Trunc(dd20160119) then
                  begin
                    if (piece = '2431') and (color = 71) and (Trunc(date27) = Trunc(dd20160123)) then
                    else if (piece = '44302') and (date27 >= dd20170412) and (date27 <= dd20170426) then
                    else if (piece = '3069b') and (date27 >= dd20170430) and (date27 <= dd20170510) then
                    else if (piece = '3069b') and (date27 >= dd20170614) and (date27 <= dd20171220) then
                    else if (piece = '2412b') and (date27 >= dd20170418) and (date27 <= dd20180207) then
                    else if mustexclude(piece, color, date27) then
                    else
                      c.Series[0].AddXY(date27, num27);
                  end;
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
        truncx := Trunc(x);
        if truncx <> Trunc(dd20151218) then
          if truncx <> Trunc(dd20160119) then
          begin
            if (piece = '2431') and (color = 71) and (truncx = Trunc(dd20160123)) then
            else if (piece = '44302') and (x >= dd20170412) and (x <= dd20170426) then
            else if (piece = '3069b') and (x >= dd20170430) and (x <= dd20170510) then
            else if (piece = '3069b') and (x >= dd20170614) and (x <= dd20171220) then
            else if (piece = '3024') and (color = 320) and (truncx = Trunc(dd20160604)) then
            else if (piece = '3023') and (color = 72) and (truncx = Trunc(dd20160604)) then
            else if (piece = '2412b') and (truncx >= dd20170418) and (truncx <= dd20180207) then
            else if (piece = '4073') and (truncx = Trunc(dd20170102)) then
            else if mustexclude(piece, color, x) then
            else if (color = -1) and (truncx >= dd20180601) and (truncx <= dd20180904) and (y = 0) then
            else
              c.Series[0].AddXY(x, y);
          end;
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
    Exit;
  end;

  // Price Guide 
  PG_MakeHistoryParecArray(piece, color, A, num);

  c.Series[0].Clear;
  c.Series[0].XValues.DateTime := True;
  c.BottomAxis.DateTimeFormat := 'dd/mm/yyyy';

  c.Title.Text.Text := C_TITLES[idx];

//  c.LeftAxis.StartPosition := 0.0;
  mx := 0.0;

//  ddxxx := s2date1('20141101');
//  dd20141216 := s2date1('20141216');
//  dd20141220 := s2date1('20141220');
//  dd20170430 := s2date1('20170430');
//  dd20170510 := s2date1('20170510');
  ii := 0;
  num_actual := 0;
  for i := 0 to num - 1 do
  begin
    x := A[i].date;
{    if x = ddxxx then
      continue;
    if (x >= dd20141216) and (x <= dd20141220) then
      continue;
    if (piece = '3069b') and (x >= dd20170430) and (x <= dd20170510) then
      continue;
    if (piece = '3069b') and (x >= dd20170614) and (x <= dd20171220) then
      continue;
    if (piece = '2412b') and (x >= dd20170418) and (x <= dd20180207) then
      continue;}
    if mustexclude(piece, color, x) then
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
      PG_nAverageLOT: y := dbl_safe_div(A[i].priceguide.nTotalQty, A[i].priceguide.nTimesSold);
      PG_uAverageLOT: y := dbl_safe_div(A[i].priceguide.uTotalQty, A[i].priceguide.uTimesSold);
      AV_nAverageLOT: y := dbl_safe_div(A[i].availability.nTotalQty, A[i].availability.nTotalLots);
      AV_uAverageLOT: y := dbl_safe_div(A[i].availability.uTotalQty, A[i].availability.uTotalLots);
      PG_nuPriceRatio: y := dbl_safe_div(A[i].priceguide.nQtyAvgPrice, A[i].priceguide.uQtyAvgPrice);
      AV_nuPriceRatio: y := dbl_safe_div(A[i].availability.nQtyAvgPrice, A[i].availability.uQtyAvgPrice);
      PG_nuQtyRatio: y := dbl_safe_div(A[i].priceguide.nTotalQty , A[i].priceguide.uTotalQty);
      AV_nuQtyRatio: y := dbl_safe_div(A[i].availability.nTotalQty , A[i].availability.uTotalQty);
    else
      y := 0.0;
    end;
    if ii = 0 then
    begin
      c.Series[0].AddXY(x - 0.00001, 0.0);
      inc(num_actual);
    end;
    inc(ii);
    c.Series[0].AddXY(x, y);
    inc(num_actual);
    if y > mx then
      mx := y;
  end;
  if num_actual = 0 then
    c.Series[0].AddXY(Now(), 0.0);
  memfree(pointer(A), num * SizeOf(parecdate_t));
end;

procedure DiagramPiece(const part: string; const color: integer; const chid: integer = 5);
var
  f: TDiagramForm;
  pci: TPieceColorInfo;
  st: set_t;
  hasinv: boolean;
  chid1: integer;
begin
  f := TDiagramForm.Create(nil);
  try
    f.UpdateSpeedButton1.Visible := True;
    f.Notebook1.ActivePage := f.Notebook1.Pages[1];
    f.DescLabel.Caption := db.PieceDesc(part);
    hasinv := (color = -1) and (db.GetSetInventory(part) <> nil);
    if hasinv then
    begin
      if f.DescLabel.Caption = '' then
        f.DescLabel.Caption := db.SetDesc(part);
      f.ColorLabel.Visible := false;
      f.Button28.Visible := true;
      f.Button29.Visible := true;
    end
    else
    begin
      if color = -1 then
        f.ColorLabel.Caption := '(Not Applicable)'
      else
        f.ColorLabel.Caption := db.colors(color).name;

      if db.PieceInfo(part).hasinventory and (color <> 9996) and (color <> 9997) and (color <> 9998) then
      begin
        f.Button28.Visible := true;
        f.Button29.Visible := true;
      end
      else
      begin
        f.Button28.Visible := false;
        f.Button29.Visible := false;
      end;
    end;
    if f.DescLabel.Caption <> '' then
      f.DescLabel.Caption := part + ' - ' + f.DescLabel.Caption
    else
      f.DescLabel.Caption := part;
    pci := db.PieceColorInfo(part, color);
    PieceToImage(f.Image1, part, color);
    if pci <> nil then
    begin
      f.ColorPanel.Caption := '';
      f.ColorPanel.Color := RGBInvert(db.colors(color).RGB);
      if hasinv then
      begin
        inventory.GetSetInfo(part, @st);
        f.Edit1.Text := itoa(st.num) + ' builded - ' + itoa(st.dismantaled) + ' dismantaled';
        f.Label2.Caption := 'Num sets:';
        f.ColorPanel.Visible := false;
      end
      else
      begin
        f.Edit1.Text := itoa(inventory.LoosePartCount(part, color));
        f.Label2.Caption := 'Num pieces:';
        if color = BOXCOLORINDEX then
          f.ColorPanel.Caption := 'B'
        else if color = INSTRUCTIONCOLORINDEX then
          f.ColorPanel.Caption := 'I'
        else if color = CATALOGCOLORINDEX then
          f.ColorPanel.Caption := 'C'
        else if color = -1 then
          f.ColorPanel.Caption := '-';
        f.ColorPanel.Visible := true;
      end;
      f.piece := part;
      f.color := color;
      Screen.Cursor := crHourglass;
      chid1 := chid;
      if (chid1 <= 0) or (chid1 > 28) then
        chid1 := 5;
      f.DoMakeChart(chid1);
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
    f.UpdateSpeedButton1.Visible := False;
    f.Notebook1.ActivePage := f.Notebook1.Pages[0];
    f.DescLabel.Caption := storage;
    f.ColorLabel.Visible := false;
    f.Edit1.Visible := false;
    f.Label2.Visible := false;
    f.ColorPanel.Visible := false;
    f.piece := storage;
    f.color := -1;
    Screen.Cursor := crHourglass;
    f.DoMakeChart(33);
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
    f.UpdateSpeedButton1.Visible := False;
    f.Notebook1.ActivePage := f.Notebook1.Pages[2];
    f.DescLabel.Caption := 'Order #' + order;
    f.ColorLabel.Visible := false;
    f.Edit1.Visible := false;
    f.Label2.Visible := false;
    f.ColorPanel.Visible := false;
    f.Width := f.Width - f.Notebook1.Width;
    f.Notebook1.Width := 0;
    f.piece := order;
    f.color := -1;
    Screen.Cursor := crHourglass;
    f.DoMakeChart(40);
    Screen.Cursor := crDefault;
    f.ShowModal;
  finally
    f.Free;
  end;
end;

procedure TDiagramForm.Button1Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(1);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button2Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(2);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button3Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(3);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button4Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(4);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button5Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(5);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button6Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(6);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button7Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(7);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button8Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(8);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button9Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(9);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button10Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(10);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button11Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(11);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button12Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(12);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button13Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(13);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button14Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(14);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button15Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(15);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button16Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(16);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button17Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(17);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button18Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(18);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button19Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(19);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button20Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(20);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button21Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(21);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button22Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(22);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button23Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(23);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button24Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(24);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button25Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(25);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button26Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(26);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button27Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(27);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button28Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(28);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button29Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(29);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button30Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(30);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button31Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(31);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button32Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(32);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button33Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(33);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button38Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(38);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button39Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(39);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button34Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(34);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button35Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(35);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button36Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(36);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button37Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(37);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Image1DblClick(Sender: TObject);
begin
  try
    if Image1.Align = alClient then
    begin
      Image1.Align := alNone;
      Image1.AutoSize := True;
      Image1.Stretch := False;
    end
    else
    begin
      Image1.Align := alClient;
      Image1.AutoSize := False;
      Image1.Stretch := True;
    end;
  except
  end;
end;

procedure TDiagramForm.ExcludeDaysButton1Click(Sender: TObject);
var
  em: TExcludeManager;
  Form: TForm;
  Prompt: TLabel;
  Memo: TMemo;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
  refreshid: integer;
begin
  em := TExcludeManager.Create(piece, color);
  refreshid := -1;

  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := 'Exclude days';
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Caption := 'Edit dates to exclude:';
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
        WordWrap := True;
      end;
      Memo := TMemo.Create(Form);
      with Memo do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := Prompt.Top + Prompt.Height + 5;
        Width := MulDiv(164, DialogUnits.X, 4);
        Height := MulDiv(82, DialogUnits.Y, 4);
        ScrollBars := ssBoth;
        WordWrap := False;
        Text := em.GetText;
      end;
      ButtonTop := Memo.Top + Memo.Height + 15;
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := 'OK';
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := 'Cancel';
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), Memo.Top + Memo.Height + 15,
          ButtonWidth, ButtonHeight);
        Form.ClientHeight := Top + Height + 13;
      end;
      if ShowModal = mrOk then
      begin
        em.SetText(Memo.Lines.Text);
        refreshid := Chart1.Tag;
      end;
    finally
      Form.Free;
    end;

  em.Free;

  if refreshid >= 0 then
  begin
    Screen.Cursor := crHourglass;
    DoMakeChart(refreshid);
    Screen.Cursor := crDefault;
  end;

end;

procedure TDiagramForm.Button41Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(41);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button42Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(42);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button43Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(43);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button44Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(44);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button45Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(45);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button46Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(46);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button47Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(47);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.Button48Click(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  DoMakeChart(48);
  Screen.Cursor := crDefault;
end;

procedure TDiagramForm.CopyChartToClipboard1Click(Sender: TObject);
begin
  Chart1.CopyToClipboardBitmap;
end;

procedure TDiagramForm.CopySpeedButton1Click(Sender: TObject);
begin
  Chart1.CopyToClipboardBitmap;
end;

procedure TDiagramForm.UpdateSpeedButton1Click(Sender: TObject);
var
  pci: TPieceColorInfo;
begin
  pci := db.PieceColorInfo(piece, color);
  if pci <> nil then
  begin
    screen.Cursor := crHourglass;
    try
      pci.InternetUpdate;
      if Chart1.Tag <> 0 then
        DoMakeChart(Chart1.Tag);
    finally
      screen.Cursor := crDefault;
    end;
  end;
end;

procedure TDiagramForm.DoMakeChart(const idx: integer);
var
  i: integer;
begin
  MakeChart(Chart1, piece, color, idx);
  if UpperCase(fcurrency) <> 'EUR' then
    if idx in [3..6, 9..12, 15..18, 21..24, 25, 26, 28, 29, 30..40, 41..48] then
    begin
      for i := 0 to Chart1.Series[0].Count - 1 do
        Chart1.Series[0].YValue[i] := Chart1.Series[0].YValue[i] / db.ConvertCurrencyAt(fcurrency, TDateTime(Chart1.Series[0].XValue[i])); 
    end;

  NumSamplesLabel.Caption := 'Num samples: ' + itoa(Chart1.Series[0].Count) +
    ' (' +
    FormatDateTime('dd/mm/yy', TDateTime(Chart1.Series[0].XValue[0])) +
    ' - ' +
    FormatDateTime('dd/mm/yy', TDateTime(Chart1.Series[0].XValue[Chart1.Series[0].Count - 1])) +
    ')';
  ValueListEditor1.Strings.Clear;
  ValueListEditor1.TitleCaptions.Text := 'Date'#13#10 + Chart1.Title.Text.Text;
  if idx in [3..6, 9..12, 15..18, 21..24, 25, 26, 28, 29, 30..40, 41..48] then
  begin
    for i := 0 to Chart1.Series[0].Count - 1 do
      ValueListEditor1.Strings.Add(FormatDateTime('c', TDateTime(Chart1.Series[0].XValue[i])) + '=' + Format('%2.6f', [Chart1.Series[0].YValue[i]]));
  end
  else
  begin
    for i := 0 to Chart1.Series[0].Count - 1 do
      ValueListEditor1.Strings.Add(FormatDateTime('c', TDateTime(Chart1.Series[0].XValue[i])) + '=' + Format('%d', [round(Chart1.Series[0].YValue[i])]));
  end;
end;

procedure TDiagramForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
  fcurrency := 'EUR';
end;

procedure TDiagramForm.SpeedButton1Click(Sender: TObject);
begin
  try
    clipboard.AsText := ValueListEditor1.Strings.Text;
  except
  end;
end;

procedure TDiagramForm.CurrencyButtonClick(Sender: TObject);
begin
  if Sender is TSpeedButton then
  begin
    fcurrency := (Sender as TSpeedButton).Caption;
    if Chart1.Tag <> 0 then
      DoMakeChart(Chart1.Tag);
  end;
end;

end.
