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
//    Price quide history
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_pghistory;

interface

uses
  bi_db;

type
  exclude_t = record
    mindate: string[8];
    maxdate: string[8];
    part: string[64];
    color: integer;
    mindatedbl: double;
    maxdatedbl: double;
  end;
  exclude_p = ^exclude_t;
  exclude_a = array[0..$FFF] of exclude_t;
  exclude_pa = ^exclude_a;

type
  TExcludeManager = class(TObject)
  private
    fname: string;
    fdir: string;
    fpart: string;
    fcolor: integer;
    fexcludes: exclude_pa;
    fnumexcludes: integer;
  protected
    procedure Clear;
    procedure AddExclude(const d1, d2: string);
  public
    constructor Create(const part: string; const color: integer); virtual;
    destructor Destroy; override;
    procedure LoadFromFile;
    procedure SaveToFile;
    function GetText: string;
    procedure SetText(const atext: string);
    function Exclude(const date: double): boolean;
  end;

function mustexclude(const part: string; const color: integer; const date1: double): boolean;

function s2date1(const s: string): TDateTime;

type
  parecarray_t = array[0..$1fff] of parecdate_t;
  parecarray_p = ^parecarray_t;

  extracacheinfo_t = record
    dir: string[128];
    date: TDateTime;
  end;

// Retrieve all Pricequide & History from crawler data
procedure PG_MakeHistoryParecArray(const piece: string; const color: integer; var A: parecarray_p; var Asize: integer);

type
  qtydate_t = record
    qty: integer;
    date: TDateTime;
  end;
  qtydate_p = ^qtydate_t;
  qtydatearray_t = array[0..$1fff] of qtydate_t;
  qtydatearray_p = ^qtydatearray_t;

  pricedate_t = record
    price: double;
    date: TDateTime;
  end;
  pricedate_p = ^pricedate_t;
  pricedatearray_t = array[0..$1fff] of pricedate_t;
  pricedatearray_p = ^pricedatearray_t;

// Retrieve Pricequide & Availability history arrays
procedure PG_History_nTimesSold(const piece: string; const color: integer; var Q: qtydatearray_p; var Qsize: integer);
procedure PG_History_nTotalQty(const piece: string; const color: integer; var Q: qtydatearray_p; var Qsize: integer);
procedure PG_History_nMinPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
procedure PG_History_nAvgPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
procedure PG_History_nQtyAvgPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
procedure PG_History_nMaxPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);

procedure PG_History_uTimesSold(const piece: string; const color: integer; var Q: qtydatearray_p; var Qsize: integer);
procedure PG_History_uTotalQty(const piece: string; const color: integer; var Q: qtydatearray_p; var Qsize: integer);
procedure PG_History_uMinPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
procedure PG_History_uAvgPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
procedure PG_History_uQtyAvgPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
procedure PG_History_uMaxPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);

procedure AV_History_nTotalLots(const piece: string; const color: integer; var Q: qtydatearray_p; var Qsize: integer);
procedure AV_History_nTotalQty(const piece: string; const color: integer; var Q: qtydatearray_p; var Qsize: integer);
procedure AV_History_nMinPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
procedure AV_History_nAvgPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
procedure AV_History_nQtyAvgPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
procedure AV_History_nMaxPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);

procedure AV_History_uTotalLots(const piece: string; const color: integer; var Q: qtydatearray_p; var Qsize: integer);
procedure AV_History_uTotalQty(const piece: string; const color: integer; var Q: qtydatearray_p; var Qsize: integer);
procedure AV_History_uMinPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
procedure AV_History_uAvgPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
procedure AV_History_uQtyAvgPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
procedure AV_History_uMaxPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);

// Retrieve Qty & Price from history arrays
function PG_QtyAt(const Q: qtydatearray_p; const Qsize: integer; const at: TDateTime): integer;
function PG_PriceAt(const P: pricedatearray_p; const Psize: integer; const at: TDateTime): double;

// Retrieve Pricequide & Availability at a given date
function PG_nTimesSoldAt(const piece: string; const color: integer; const at: TDateTime): integer;
function PG_nTotalQtyAt(const piece: string; const color: integer; const at: TDateTime): integer;
function PG_nMinPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
function PG_nAvgPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
function PG_nQtyAvgPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
function PG_nMaxPriceAt(const piece: string; const color: integer; const at: TDateTime): double;

function PG_uTimesSoldAt(const piece: string; const color: integer; const at: TDateTime): integer;
function PG_uTotalQtyAt(const piece: string; const color: integer; const at: TDateTime): integer;
function PG_uMinPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
function PG_uAvgPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
function PG_uQtyAvgPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
function PG_uMaxPriceAt(const piece: string; const color: integer; const at: TDateTime): double;

function AV_nTotalLotsAt(const piece: string; const color: integer; const at: TDateTime): integer;
function AV_nTotalQtyAt(const piece: string; const color: integer; const at: TDateTime): integer;
function AV_nMinPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
function AV_nAvgPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
function AV_nQtyAvgPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
function AV_nMaxPriceAt(const piece: string; const color: integer; const at: TDateTime): double;

function AV_uTotalLotsAt(const piece: string; const color: integer; const at: TDateTime): integer;
function AV_uTotalQtyAt(const piece: string; const color: integer; const at: TDateTime): integer;
function AV_uMinPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
function AV_uAvgPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
function AV_uQtyAvgPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
function AV_uMaxPriceAt(const piece: string; const color: integer; const at: TDateTime): double;

implementation

uses
  bi_delphi, bi_globals, bi_priceadjust, DateUtils, StrUtils, SysUtils, Classes;

const
  NUMEXCLUDES = 49;

var
  excludes: array[0..NUMEXCLUDES - 1] of exclude_t = (
    (
      mindate: '20141101';
      maxdate: '20141101';
      part:    '';
      color:   -2;
    ),
    (
      mindate: '20141216';
      maxdate: '20141220';
      part:    '';
      color:   -2;
    ),
    (
      mindate: '20170430';
      maxdate: '20170510';
      part:    '3069b';
      color:   -2;
    ),
    (
      mindate: '20170614';
      maxdate: '20171220';
      part:    '3069b';
      color:   -2;
    ),
    (
      mindate: '20170418';
      maxdate: '20180207';
      part:    '2412b';
      color:   -2;
    ),
    (
      mindate: '20190401';
      maxdate: '20190401';
      part:    '';
      color:   -2;
    ),
    (
      mindate: '20190402';
      maxdate: '20190402';
      part:    '';
      color:   -2;
    ),
    (
      mindate: '20190403';
      maxdate: '20190403';
      part:    '';
      color:   -2;
    ),
    (
      mindate: '20171126';
      maxdate: '20171126';
      part:    '';
      color:   -2;
    ),
    (
      mindate: '20180409';
      maxdate: '20180409';
      part:    '';
      color:   -2;
    ),
    (
      mindate: '20151009';
      maxdate: '20151012';
      part:    '';
      color:   -2;
    ),
    (
      mindate: '20141215';
      maxdate: '20141215';
      part:    '';
      color:   -2;
    ),
    (
      mindate: '20150227';
      maxdate: '20150307';
      part:    '';
      color:   -2;
    ),
    (
      mindate: '20150313';
      maxdate: '20150316';
      part:    '';
      color:   -2;
    ),
    (
      mindate: '20150323';
      maxdate: '20150326';
      part:    '';
      color:   -2;
    ),
    (
      mindate: '20150409';
      maxdate: '20150411';
      part:    '';
      color:   -2;
    ),
    (
      mindate: '20150424';
      maxdate: '20150426';
      part:    '';
      color:   -2;
    ),
    (
      mindate: '20150430';
      maxdate: '20150501';
      part:    '';
      color:   -2;
    ),
    (
      mindate: '20150523';
      maxdate: '20150525';
      part:    '';
      color:   -2;
    ),
    (
      mindate: '20150625';
      maxdate: '20150628';
      part:    '';
      color:   -2;
    ),
    (
      mindate: '20150731';
      maxdate: '20150807';
      part:    '';
      color:   -2;
    ),
    (
      mindate: '20180105';
      maxdate: '20180105';
      part:    '';
      color:   2;
    ),
    (
      mindate: '20180407';
      maxdate: '20180407';
      part:    '';
      color:   2;
    ),
    (
      mindate: '20180424';
      maxdate: '20180424';
      part:    '';
      color:   -2;
    ),
    (
      mindate: '20171126';
      maxdate: '20171127';
      part:    '6014b';
      color:   4;
    ),
    (
      mindate: '20171219';
      maxdate: '20171221';
      part:    '3069b';
      color:   82;
    ),
    (
      mindate: '20180328';
      maxdate: '20180329';
      part:    '4858';
      color:   0;
    ),
    (
      mindate: '20180105';
      maxdate: '20180105';
      part:    '3068b';
      color:   2;
    ),
    (
      mindate: '20180330';
      maxdate: '20180331';
      part:    '3010';
      color:   15;
    ),
    (
      mindate: '20180216';
      maxdate: '20180216';
      part:    '3023';
      color:   4;
    ),
    (
      mindate: '20180105';
      maxdate: '20180106';
      part:    '4070';
      color:   2;
    ),
    (
      mindate: '20171126';
      maxdate: '20171127';
      part:    '4070';
      color:   4;
    ),
    (
      mindate: '20180105';
      maxdate: '20180106';
      part:    '3666';
      color:   2;
    ),
    (
      mindate: '20171126';
      maxdate: '20171127';
      part:    '3666';
      color:   4;
    ),
    (
      mindate: '20180120';
      maxdate: '20180121';
      part:    '';
      color:   -2;
    ),
    (
      mindate: '20150902';
      maxdate: '20150902';
      part:    '10211-1';
      color:   -1;
    ),
    (
      mindate: '20170311';
      maxdate: '20170311';
      part:    '10211-1';
      color:   -1;
    ),
    (
      mindate: '20140610';
      maxdate: '20150222';
      part:    '2780';
      color:   0;
    ),
    (
      mindate: '20180120';
      maxdate: '20180121';
      part:    '3004';
      color:   3;
    ),
    (
      mindate: '20180120';
      maxdate: '20180121';
      part:    '3005';
      color:   1;
    ),
    (
      mindate: '20180120';
      maxdate: '20180121';
      part:    '3005';
      color:   2;
    ),
    (
      mindate: '20180120';
      maxdate: '20180121';
      part:    '3006';
      color:   2;
    ),
    (
      mindate: '20171126';
      maxdate: '20171127';
      part:    '3622';
      color:   4;
    ),
    (
      mindate: '20171126';
      maxdate: '20171127';
      part:    '6111';
      color:   4;
    ),
    (
      mindate: '20180120';
      maxdate: '20180121';
      part:    '3010';
      color:   7;
    ),
    (
      mindate: '20180120';
      maxdate: '20180121';
      part:    '3010';
      color:   8;
    ),
    (
      mindate: '20180120';
      maxdate: '20180121';
      part:    '3010';
      color:   15;
    ),
    (
      mindate: '20180120';
      maxdate: '20180121';
      part:    '3010';
      color:   19;
    ),
    (
      mindate: '20180120';
      maxdate: '20180121';
      part:    '3010';
      color:   28;
    )
  );

function s2date1(const s: string): TDateTime;
var
  ayear, amonth, aday: word;
begin
  if length(s) <> 8 then
  begin
    result := 0.0;
    Exit;
  end;
  ayear := atoi(leftstr(s, 4));
  amonth := atoi(rightstr(leftstr(s, 6), 2));
  aday := atoi(rightstr(s, 2));
  if not TryEncodeDateTime(ayear, amonth, aday, 0, 0, 0, 0, result) then
    result := 0.0;
end;

////////////////////////////////////////////////////////////////////////////////

constructor TExcludeManager.Create(const part: string; const color: integer);
begin
  fnumexcludes := 0;
  fexcludes := nil;
  fpart := part;
  fcolor := color;
  fname := basedefault + 'cache\' + IntToStr(color) + '\' + part + '.excludes';
  fdir := basedefault + 'cache\' + IntToStr(color);
  if not DirectoryExists(fdir) then
    ForceDirectories(fdir);
  Inherited Create;
  LoadFromFile;
end;

destructor TExcludeManager.Destroy;
begin
  Clear;
  Inherited;
end;

procedure TExcludeManager.Clear;
begin
  if fexcludes <> nil then
  begin
    memfree(pointer(fexcludes), fnumexcludes * SizeOf(exclude_t));
    fnumexcludes := 0;
  end;
end;

procedure TExcludeManager.AddExclude(const d1, d2: string);
begin
  if Length(d1) <> 8 then
    Exit;
  if Length(d2) <> 8 then
    Exit;
  realloc(pointer(fexcludes), fnumexcludes * SizeOf(exclude_t), (fnumexcludes + 1) * SizeOf(exclude_t));
  fexcludes[fnumexcludes].mindate := d1;
  fexcludes[fnumexcludes].maxdate := d2;
  fexcludes[fnumexcludes].part := fpart;
  fexcludes[fnumexcludes].color := fcolor;
  fexcludes[fnumexcludes].mindatedbl := s2date1(fexcludes[fnumexcludes].mindate);
  fexcludes[fnumexcludes].maxdatedbl := s2date1(fexcludes[fnumexcludes].maxdate);
  inc(fnumexcludes);
end;

procedure TExcludeManager.LoadFromFile;
var
  s: TStringList;
  i: integer;
  s1, s2: string;
begin
  Clear;
  if fexists(fname) then
  begin
    s := TStringList.Create;
    try
      s.LoadFromFile(fname);
      if s.Count > 1 then
      begin
        for i := 0 to s.Count - 1 do
        begin
          splitstring(s.Strings[i], s1, s2, ',');
          s1 := Trim(s1);
          s2 := Trim(s2);
          if s2 = '' then
          begin
            if Length(s1) = 8 then
              s2 := s1
            else if IsIntegerInRange(atoi(s1), 2014, 2050) then
            begin
              s2 := s1 + '1231';
              s1 := s1 + '0101';
            end;
          end;
          AddExclude(s1, s2);
        end;
      end;
    finally
      s.Free;
    end;
  end;
end;

function TExcludeManager.GetText: string;
var
  s: TStringList;
  i: integer;
begin
  s := TStringList.Create;
  s.Add('date1,date2');
  for i := 0 to fnumexcludes - 1 do
    s.Add(Format('%s,%s', [fexcludes[i].mindate, fexcludes[i].maxdate]));
  result := s.Text;
  s.Free;
end;

procedure TExcludeManager.SetText(const atext: string);
var
  s: TStringList;
begin
  s := TStringList.Create;
  try
    s.Text := atext;
    s.SaveToFile(fname);
  finally
    s.Free;
  end;
  LoadFromFile;
end;

procedure TExcludeManager.SaveToFile;
var
  s: TStringList;
begin
  s := TStringList.Create;
  try
    s.Text := GetText;
    s.SaveToFile(fname);
  finally
    s.Free;
  end;
end;

function TExcludeManager.Exclude(const date: double): boolean;
var
  i: integer;
begin
  for i := 0 to fnumexcludes - 1 do
    if date >= fexcludes[i].mindatedbl then
      if date < fexcludes[i].maxdatedbl + 1 then
      begin
        Result := True;
        Exit;
      end;

  Result := False;
end;

var
  dd20131231a: double;
  dd20180904a: double;
  dd20180907a: double;

function mustexclude(const part: string; const color: integer; const date1: double): boolean;
var
  i: integer;
  ep: exclude_p;
  em: TExcludeManager;
begin
  Result := False;

  if date1 < dd20131231a then
  begin
    Result := True;
    Exit;
  end;

  if date1 > dd20180904a then
    if date1 < dd20180907a then
    begin
      Result := True;
      Exit;
    end;

  for i := 0 to NUMEXCLUDES - 1 do
  begin
    ep := @excludes[i];
    if (color = ep.color) or (ep.color = -2) then
      if (date1 >= ep.mindatedbl) and (date1 <= ep.maxdatedbl + 1) then
        if (part = ep.part) or (ep.part = '') then
        begin
          Result := True;
          Exit;
        end;
  end;

  em := TExcludeManager.Create(part, color);
  if em.Exclude(date1) then
    Result := True;
  em.Free;
end;

function iszeroparec(const p: parecdate_t): boolean;
begin
  result := false;

  if p.priceguide.nTimesSold <> 0 then
    Exit;
  if p.priceguide.nTotalQty <> 0 then
    Exit;
  if p.priceguide.nMinPrice <> 0 then
    Exit;
  if p.priceguide.nAvgPrice <> 0 then
    Exit;
  if p.priceguide.nQtyAvgPrice <> 0 then
    Exit;
  if p.priceguide.nMaxPrice <> 0 then
    Exit;
  if p.priceguide.uTimesSold <> 0 then
    Exit;
  if p.priceguide.uTotalQty <> 0 then
    Exit;
  if p.priceguide.uMinPrice <> 0 then
    Exit;
  if p.priceguide.uAvgPrice <> 0 then
    Exit;
  if p.priceguide.uQtyAvgPrice <> 0 then
    Exit;
  if p.priceguide.uMaxPrice <> 0 then
    Exit;

  if p.availability.nTotalLots <> 0 then
    Exit;
  if p.availability.nTotalQty <> 0 then
    Exit;
  if p.availability.nMinPrice <> 0 then
    Exit;
  if p.availability.nAvgPrice <> 0 then
    Exit;
  if p.availability.nQtyAvgPrice <> 0 then
    Exit;
  if p.availability.nMaxPrice <> 0 then
    Exit;
  if p.availability.uTotalLots <> 0 then
    Exit;
  if p.availability.uTotalQty <> 0 then
    Exit;
  if p.availability.uMinPrice <> 0 then
    Exit;
  if p.availability.uAvgPrice <> 0 then
    Exit;
  if p.availability.uQtyAvgPrice <> 0 then
    Exit;
  if p.availability.uMaxPrice <> 0 then
    Exit;

  result := true;
end;

procedure SortParecArray(const A: parecarray_p; const num: integer);
var
  T: parecdate_t;

  procedure QuickSort(iLo, iHi: Integer);
  var
     Lo, Hi: integer;
     Pivot: TDateTime;
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
  begin
    QuickSort(0, num - 1);
    if num > 1 then
      if abs(A[1].date - A[0].date) < 0.001 then
        if iszeroparec(A[1]) then
          if not iszeroparec(A[0]) then
          begin
            T := A[0];
            A[0] := A[1];
            A[1] := T;
          end;
  end;
end;

procedure FixParecArray(const A: parecarray_p; const num: integer);
  procedure fix_value(const v1, v2: PDouble);
  begin
    if v1^ = 0.0 then
      v1^ := v2^
    else
      v2^ := v1^;
  end;
var
  p_nMinPrice: double;
  p_nAvgPrice: double;
  p_nQtyAvgPrice: double;
  p_nMaxPrice: double;
  p_uMinPrice: double;
  p_uAvgPrice: double;
  p_uQtyAvgPrice: double;
  p_uMaxPrice: double;
  a_nMinPrice: double;
  a_nAvgPrice: double;
  a_nQtyAvgPrice: double;
  a_nMaxPrice: double;
  a_uMinPrice: double;
  a_uAvgPrice: double;
  a_uQtyAvgPrice: double;
  a_uMaxPrice: double;
  i: integer;
begin
  p_nMinPrice := 0.0;
  p_nAvgPrice := 0.0;
  p_nQtyAvgPrice := 0.0;
  p_nMaxPrice := 0.0;
  p_uMinPrice := 0.0;
  p_uAvgPrice := 0.0;
  p_uQtyAvgPrice := 0.0;
  p_uMaxPrice := 0.0;
  a_nMinPrice := 0.0;
  a_nAvgPrice := 0.0;
  a_nQtyAvgPrice := 0.0;
  a_nMaxPrice := 0.0;
  a_uMinPrice := 0.0;
  a_uAvgPrice := 0.0;
  a_uQtyAvgPrice := 0.0;
  a_uMaxPrice := 0.0;
  for i := 0 to num - 1 do
  begin
    fix_value(@A[i].priceguide.nMinPrice, @p_nMinPrice);
    fix_value(@A[i].priceguide.nAvgPrice, @p_nAvgPrice);
    fix_value(@A[i].priceguide.nQtyAvgPrice, @p_nQtyAvgPrice);
    fix_value(@A[i].priceguide.nMaxPrice, @p_nMaxPrice);
    fix_value(@A[i].priceguide.uMinPrice, @p_uMinPrice);
    fix_value(@A[i].priceguide.uAvgPrice, @p_uAvgPrice);
    fix_value(@A[i].priceguide.uQtyAvgPrice, @p_uQtyAvgPrice);
    fix_value(@A[i].priceguide.uMaxPrice, @p_uMaxPrice);
    fix_value(@A[i].availability.nMinPrice, @a_nMinPrice);
    fix_value(@A[i].availability.nAvgPrice, @a_nAvgPrice);
    fix_value(@A[i].availability.nQtyAvgPrice, @a_nQtyAvgPrice);
    fix_value(@A[i].availability.nMaxPrice, @a_nMaxPrice);
    fix_value(@A[i].availability.uMinPrice, @a_uMinPrice);
    fix_value(@A[i].availability.uAvgPrice, @a_uAvgPrice);
    fix_value(@A[i].availability.uQtyAvgPrice, @a_uQtyAvgPrice);
    fix_value(@A[i].availability.uMaxPrice, @a_uMaxPrice);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

const
  MAXEXTRACACKHEDIRS = 128;

procedure PG_MakeHistoryParecArray(const piece: string; const color: integer; var A: parecarray_p; var Asize: integer);
var
  i, j, l: integer;
  EXTRA: array[0..MAXEXTRACACKHEDIRS - 1] of extracacheinfo_t;
  numextra: integer;
  sname: string;
  fname: string;
  s, aliasSL: TStringList;
  s1, s2: string;
  aliaspiece: string;
  hasalias: boolean;
  f: TFileStream;
  num: integer;
begin
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
        if numextra = MAXEXTRACACKHEDIRS then
          break;
      end;
    finally
      s.Free;
    end;
  end;

  aliasSL := TStringList.Create;
  aliasSL.Add(piece);
  aliaspiece := db.GetBLNetPieceName(piece);
  hasalias := (UpperCase(aliaspiece) <> UpperCase(piece)) and (Trim(aliaspiece) <> '');
  if hasalias then
    aliasSL.Add(aliaspiece);

  Asize := 0;

  for l := 0 to aliasSL.Count - 1 do
  begin
    for i := 0 to numextra - 1 do
    begin
      fname := basedefault + EXTRA[i].dir + '\' + itoa(decide(color = -1, 9999, color)) + '\' + aliasSL.Strings[l] + '.cache';
      if not fexists(fname) then
        continue;
      try
        f := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
      except
        sleep(100);
        f := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
      end;
      if f.Size = 160 then
      begin
        inc(Asize);
      end
      else if f.Size mod SizeOf(parecdate_t) = 0 then
      begin
        inc(Asize, f.Size div SizeOf(parecdate_t));
      end;
      f.free;
    end;
  end;

  Asize := Asize + 8192 * 4;

  num := 0;
  A := mallocz(Asize * SizeOf(parecdate_t));
  for l := 0 to aliasSL.Count - 1 do
  begin
    for i := 0 to numextra - 1 do
    begin
      fname := basedefault + EXTRA[i].dir + '\' + itoa(decide(color = -1, 9999, color)) + '\' + aliasSL.Strings[l] + '.cache';
      if not fexists(fname) then
        continue;
      try
        f := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
      except
        sleep(100);
        f := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
      end;
      if f.Size = 160 then
      begin
        f.Read(A[num].priceguide, SizeOf(priceguide_t));
        f.Read(A[num].availability, SizeOf(availability_t));
        A[num].date := EXTRA[i].date;
        if num < Asize - 1 then
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
          if num < Asize - 1 then
            inc(num)
          else
          begin
            realloc(pointer(A), Asize * SizeOf(parecdate_t), (1024 + Asize) * SizeOf(parecdate_t));
            Asize := Asize + 1024;
            inc(num);
          end;
        end;
      end;
      f.free;
    end;
  end;

  aliasSL.Free;

  for i := 0 to num - 1 do
  begin
    PRICEADJUST(piece, color, @A[i]);
    if A[i].date < s2date1('20140401') then
      A[i].date := s2date1('20140401');
  end;

  j := 0;
  for i := 0 to num - 1 do
  begin
    if mustexclude(piece, color, A[i].date) then
      continue;
    if i > j then
      A[j] := A[i];
    inc(j);
  end;

  SortParecArray(A, j);
  FixParecArray(A, j);

  if j <> Asize then
  begin
    if j = 0 then
    begin
      j := 1;
      FillChar(A[0], SizeOf(parecdate_t), Chr(0));
      A[0].date := Now;
    end;
    realloc(pointer(A), Asize * SizeOf(parecdate_t), j * SizeOf(parecdate_t));
    Asize := j;
  end;
end;

procedure PG_History_nTimesSold(const piece: string; const color: integer; var Q: qtydatearray_p; var Qsize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Qsize);

  Q := malloc(Qsize * SizeOf(qtydate_t));
  for i := 0 to Qsize - 1 do
  begin
    Q[i].qty := A[i].priceguide.nTimesSold;
    Q[i].date := A[i].date;
  end;

  memfree(pointer(A), Qsize * SizeOf(parecdate_t));
end;

procedure PG_History_nTotalQty(const piece: string; const color: integer; var Q: qtydatearray_p; var Qsize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Qsize);

  Q := malloc(Qsize * SizeOf(qtydate_t));
  for i := 0 to Qsize - 1 do
  begin
    Q[i].qty := A[i].priceguide.nTotalQty;
    Q[i].date := A[i].date;
  end;

  memfree(pointer(A), Qsize * SizeOf(parecdate_t));
end;

procedure PG_History_nMinPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Psize);

  P := malloc(Psize * SizeOf(pricedate_t));
  for i := 0 to Psize - 1 do
  begin
    P[i].price := A[i].priceguide.nMinPrice;
    P[i].date := A[i].date;
  end;

  memfree(pointer(A), Psize * SizeOf(parecdate_t));
end;

procedure PG_History_nAvgPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Psize);

  P := malloc(Psize * SizeOf(pricedate_t));
  for i := 0 to Psize - 1 do
  begin
    P[i].price := A[i].priceguide.nAvgPrice;
    P[i].date := A[i].date;
  end;

  memfree(pointer(A), Psize * SizeOf(parecdate_t));
end;

procedure PG_History_nQtyAvgPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Psize);

  P := malloc(Psize * SizeOf(pricedate_t));
  for i := 0 to Psize - 1 do
  begin
    P[i].price := A[i].priceguide.nQtyAvgPrice;
    P[i].date := A[i].date;
  end;

  memfree(pointer(A), Psize * SizeOf(parecdate_t));
end;

procedure PG_History_nMaxPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Psize);

  P := malloc(Psize * SizeOf(pricedate_t));
  for i := 0 to Psize - 1 do
  begin
    P[i].price := A[i].priceguide.nMaxPrice;
    P[i].date := A[i].date;
  end;

  memfree(pointer(A), Psize * SizeOf(parecdate_t));
end;

procedure PG_History_uTimesSold(const piece: string; const color: integer; var Q: qtydatearray_p; var Qsize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Qsize);

  Q := malloc(Qsize * SizeOf(qtydate_t));
  for i := 0 to Qsize - 1 do
  begin
    Q[i].qty := A[i].priceguide.uTimesSold;
    Q[i].date := A[i].date;
  end;

  memfree(pointer(A), Qsize * SizeOf(parecdate_t));
end;

procedure PG_History_uTotalQty(const piece: string; const color: integer; var Q: qtydatearray_p; var Qsize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Qsize);

  Q := malloc(Qsize * SizeOf(qtydate_t));
  for i := 0 to Qsize - 1 do
  begin
    Q[i].qty := A[i].priceguide.uTotalQty;
    Q[i].date := A[i].date;
  end;

  memfree(pointer(A), Qsize * SizeOf(parecdate_t));
end;

procedure PG_History_uMinPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Psize);

  P := malloc(Psize * SizeOf(pricedate_t));
  for i := 0 to Psize - 1 do
  begin
    P[i].price := A[i].priceguide.uMinPrice;
    P[i].date := A[i].date;
  end;

  memfree(pointer(A), Psize * SizeOf(parecdate_t));
end;

procedure PG_History_uAvgPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Psize);

  P := malloc(Psize * SizeOf(pricedate_t));
  for i := 0 to Psize - 1 do
  begin
    P[i].price := A[i].priceguide.uAvgPrice;
    P[i].date := A[i].date;
  end;

  memfree(pointer(A), Psize * SizeOf(parecdate_t));
end;

procedure PG_History_uQtyAvgPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Psize);

  P := malloc(Psize * SizeOf(pricedate_t));
  for i := 0 to Psize - 1 do
  begin
    P[i].price := A[i].priceguide.uQtyAvgPrice;
    P[i].date := A[i].date;
  end;

  memfree(pointer(A), Psize * SizeOf(parecdate_t));
end;

procedure PG_History_uMaxPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Psize);

  P := malloc(Psize * SizeOf(pricedate_t));
  for i := 0 to Psize - 1 do
  begin
    P[i].price := A[i].priceguide.uMaxPrice;
    P[i].date := A[i].date;
  end;

  memfree(pointer(A), Psize * SizeOf(parecdate_t));
end;

procedure AV_History_nTotalLots(const piece: string; const color: integer; var Q: qtydatearray_p; var Qsize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Qsize);

  Q := malloc(Qsize * SizeOf(qtydate_t));
  for i := 0 to Qsize - 1 do
  begin
    Q[i].qty := A[i].availability.nTotalLots;
    Q[i].date := A[i].date;
  end;

  memfree(pointer(A), Qsize * SizeOf(parecdate_t));
end;

procedure AV_History_nTotalQty(const piece: string; const color: integer; var Q: qtydatearray_p; var Qsize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Qsize);

  Q := malloc(Qsize * SizeOf(qtydate_t));
  for i := 0 to Qsize - 1 do
  begin
    Q[i].qty := A[i].availability.nTotalQty;
    Q[i].date := A[i].date;
  end;

  memfree(pointer(A), Qsize * SizeOf(parecdate_t));
end;

procedure AV_History_nMinPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Psize);

  P := malloc(Psize * SizeOf(pricedate_t));
  for i := 0 to Psize - 1 do
  begin
    P[i].price := A[i].availability.nMinPrice;
    P[i].date := A[i].date;
  end;

  memfree(pointer(A), Psize * SizeOf(parecdate_t));
end;

procedure AV_History_nAvgPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Psize);

  P := malloc(Psize * SizeOf(pricedate_t));
  for i := 0 to Psize - 1 do
  begin
    P[i].price := A[i].availability.nAvgPrice;
    P[i].date := A[i].date;
  end;

  memfree(pointer(A), Psize * SizeOf(parecdate_t));
end;

procedure AV_History_nQtyAvgPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Psize);

  P := malloc(Psize * SizeOf(pricedate_t));
  for i := 0 to Psize - 1 do
  begin
    P[i].price := A[i].availability.nQtyAvgPrice;
    P[i].date := A[i].date;
  end;

  memfree(pointer(A), Psize * SizeOf(parecdate_t));
end;

procedure AV_History_nMaxPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Psize);

  P := malloc(Psize * SizeOf(pricedate_t));
  for i := 0 to Psize - 1 do
  begin
    P[i].price := A[i].availability.nMaxPrice;
    P[i].date := A[i].date;
  end;

  memfree(pointer(A), Psize * SizeOf(parecdate_t));
end;

procedure AV_History_uTotalLots(const piece: string; const color: integer; var Q: qtydatearray_p; var Qsize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Qsize);

  Q := malloc(Qsize * SizeOf(qtydate_t));
  for i := 0 to Qsize - 1 do
  begin
    Q[i].qty := A[i].availability.uTotalLots;
    Q[i].date := A[i].date;
  end;

  memfree(pointer(A), Qsize * SizeOf(parecdate_t));
end;

procedure AV_History_uTotalQty(const piece: string; const color: integer; var Q: qtydatearray_p; var Qsize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Qsize);

  Q := malloc(Qsize * SizeOf(qtydate_t));
  for i := 0 to Qsize - 1 do
  begin
    Q[i].qty := A[i].availability.uTotalQty;
    Q[i].date := A[i].date;
  end;

  memfree(pointer(A), Qsize * SizeOf(parecdate_t));
end;

procedure AV_History_uMinPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Psize);

  P := malloc(Psize * SizeOf(pricedate_t));
  for i := 0 to Psize - 1 do
  begin
    P[i].price := A[i].availability.uMinPrice;
    P[i].date := A[i].date;
  end;

  memfree(pointer(A), Psize * SizeOf(parecdate_t));
end;

procedure AV_History_uAvgPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Psize);

  P := malloc(Psize * SizeOf(pricedate_t));
  for i := 0 to Psize - 1 do
  begin
    P[i].price := A[i].availability.uAvgPrice;
    P[i].date := A[i].date;
  end;

  memfree(pointer(A), Psize * SizeOf(parecdate_t));
end;

procedure AV_History_uQtyAvgPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Psize);

  P := malloc(Psize * SizeOf(pricedate_t));
  for i := 0 to Psize - 1 do
  begin
    P[i].price := A[i].availability.uQtyAvgPrice;
    P[i].date := A[i].date;
  end;

  memfree(pointer(A), Psize * SizeOf(parecdate_t));
end;

procedure AV_History_uMaxPrice(const piece: string; const color: integer; var P: pricedatearray_p; var Psize: integer);
var
  A: parecarray_p;
  i: integer;
begin
  PG_MakeHistoryParecArray(piece, color, A, Psize);

  P := malloc(Psize * SizeOf(pricedate_t));
  for i := 0 to Psize - 1 do
  begin
    P[i].price := A[i].availability.uMaxPrice;
    P[i].date := A[i].date;
  end;

  memfree(pointer(A), Psize * SizeOf(parecdate_t));
end;

////////////////////////////////////////////////////////////////////////////////

function PG_QtyAt(const Q: qtydatearray_p; const Qsize: integer; const at: TDateTime): integer;
var
  idx1, idx2, idx: integer;
  i: integer;
  qty1, qty2: integer;
  span1, span2: double;
begin
  if Qsize = 0 then
  begin
    Result := 0;
    Exit;
  end;

  if Qsize = 1 then
  begin
    Result := Q[0].qty;
    Exit;
  end;

  idx1 := 0;
  for i := 0 to Qsize - 1 do
  begin
    if Q[i].qty = 0 then
      inc(idx1)
    else
      break;
  end;

  idx2 := Qsize - 1;
  for i := Qsize - 1 downto 0 do
  begin
    if Q[i].qty = 0 then
      dec(idx2)
    else
      break;
  end;

  if idx2 < idx1 then
  begin
    Result := 0;
    Exit;
  end;

  if idx1 = idx2 then
  begin
    Result := Q[idx1].qty;
    Exit;
  end;

  if at <= Q[idx1].date then
  begin
    Result := Q[idx1].qty;
    Exit;
  end;

  if at >= Q[idx2].date then
  begin
    Result := Q[idx2].qty;
    Exit;
  end;

  idx := idx1 + 1;
  for i := idx1 + 1 to idx2 do
  begin
    if Q[i].date >= at then
    begin
      idx := i;
      Break;
    end;
  end;

  qty1 := Q[idx - 1].qty;
  qty2 := Q[idx].qty;

  span1 := DaySpan(at, Q[idx - 1].date);
  span2 := DaySpan(Q[idx].date, at);

  if span1 < 0.01 then
  begin
    Result := qty1;
    Exit;
  end;

  if span2 < 0.01 then
  begin
    Result := qty2;
    Exit;
  end;

  Result := qty1 + Round(((qty2 - qty1) / (span1 + span2)) * span1);
end;

function PG_PriceAt(const P: pricedatearray_p; const Psize: integer; const at: TDateTime): double;
var
  idx1, idx2, idx: integer;
  i: integer;
  price1, price2: double;
  span1, span2: double;
begin
  if Psize = 0 then
  begin
    Result := 0.0;
    Exit;
  end;

  if Psize = 1 then
  begin
    Result := P[0].price;
    Exit;
  end;

  idx1 := 0;
  for i := 0 to Psize - 1 do
  begin
    if P[i].price = 0.0 then
      inc(idx1)
    else
      break;
  end;

  idx2 := Psize - 1;
  for i := Psize - 1 downto 0 do
  begin
    if P[i].price = 0 then
      dec(idx2)
    else
      break;
  end;

  if idx2 < idx1 then
  begin
    Result := 0;
    Exit;
  end;

  if idx1 = idx2 then
  begin
    Result := P[idx1].price;
    Exit;
  end;

  if at <= P[idx1].date then
  begin
    Result := P[idx1].price;
    Exit;
  end;

  if at >= P[idx2].date then
  begin
    Result := P[idx2].price;
    Exit;
  end;

  idx := idx1 + 1;
  for i := idx1 + 1 to idx2 do
  begin
    if P[i].date >= at then
    begin
      idx := i;
      Break;
    end;
  end;

  price1 := P[idx - 1].price;
  price2 := P[idx].price;

  span1 := DaySpan(at, P[idx - 1].date);
  span2 := DaySpan(P[idx].date, at);

  if span1 < 0.01 then
  begin
    Result := price1;
    Exit;
  end;

  if span2 < 0.01 then
  begin
    Result := price2;
    Exit;
  end;

  Result := price1 + ((price2 - price1) / (span1 + span2)) * span1;
end;

////////////////////////////////////////////////////////////////////////////////

function PG_nTimesSoldAt(const piece: string; const color: integer; const at: TDateTime): integer;
var
  Q: qtydatearray_p;
  Qsize: integer;
begin
  PG_History_nTimesSold(piece, color, Q, Qsize);
  Result := PG_QtyAt(Q, Qsize, at);
  memfree(pointer(Q), Qsize * SizeOf(qtydate_t));
end;

function PG_nTotalQtyAt(const piece: string; const color: integer; const at: TDateTime): integer;
var
  Q: qtydatearray_p;
  Qsize: integer;
begin
  PG_History_nTotalQty(piece, color, Q, Qsize);
  Result := PG_QtyAt(Q, Qsize, at);
  memfree(pointer(Q), Qsize * SizeOf(qtydate_t));
end;

function PG_nMinPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
var
  P: pricedatearray_p;
  Psize: integer;
begin
  PG_History_nMinPrice(piece, color, P, Psize);
  Result := PG_PriceAt(P, Psize, at);
  memfree(pointer(P), Psize * SizeOf(pricedate_t));
end;

function PG_nAvgPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
var
  P: pricedatearray_p;
  Psize: integer;
begin
  PG_History_nAvgPrice(piece, color, P, Psize);
  Result := PG_PriceAt(P, Psize, at);
  memfree(pointer(P), Psize * SizeOf(pricedate_t));
end;

function PG_nQtyAvgPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
var
  P: pricedatearray_p;
  Psize: integer;
begin
  PG_History_nQtyAvgPrice(piece, color, P, Psize);
  Result := PG_PriceAt(P, Psize, at);
  memfree(pointer(P), Psize * SizeOf(pricedate_t));
end;

function PG_nMaxPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
var
  P: pricedatearray_p;
  Psize: integer;
begin
  PG_History_nMaxPrice(piece, color, P, Psize);
  Result := PG_PriceAt(P, Psize, at);
  memfree(pointer(P), Psize * SizeOf(pricedate_t));
end;

function PG_uTimesSoldAt(const piece: string; const color: integer; const at: TDateTime): integer;
var
  Q: qtydatearray_p;
  Qsize: integer;
begin
  PG_History_uTimesSold(piece, color, Q, Qsize);
  Result := PG_QtyAt(Q, Qsize, at);
  memfree(pointer(Q), Qsize * SizeOf(qtydate_t));
end;

function PG_uTotalQtyAt(const piece: string; const color: integer; const at: TDateTime): integer;
var
  Q: qtydatearray_p;
  Qsize: integer;
begin
  PG_History_uTotalQty(piece, color, Q, Qsize);
  Result := PG_QtyAt(Q, Qsize, at);
  memfree(pointer(Q), Qsize * SizeOf(qtydate_t));
end;

function PG_uMinPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
var
  P: pricedatearray_p;
  Psize: integer;
begin
  PG_History_uMinPrice(piece, color, P, Psize);
  Result := PG_PriceAt(P, Psize, at);
  memfree(pointer(P), Psize * SizeOf(pricedate_t));
end;

function PG_uAvgPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
var
  P: pricedatearray_p;
  Psize: integer;
begin
  PG_History_uAvgPrice(piece, color, P, Psize);
  Result := PG_PriceAt(P, Psize, at);
  memfree(pointer(P), Psize * SizeOf(pricedate_t));
end;

function PG_uQtyAvgPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
var
  P: pricedatearray_p;
  Psize: integer;
begin
  PG_History_uQtyAvgPrice(piece, color, P, Psize);
  Result := PG_PriceAt(P, Psize, at);
  memfree(pointer(P), Psize * SizeOf(pricedate_t));
end;

function PG_uMaxPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
var
  P: pricedatearray_p;
  Psize: integer;
begin
  PG_History_uMaxPrice(piece, color, P, Psize);
  Result := PG_PriceAt(P, Psize, at);
  memfree(pointer(P), Psize * SizeOf(pricedate_t));
end;

function AV_nTotalLotsAt(const piece: string; const color: integer; const at: TDateTime): integer;
var
  Q: qtydatearray_p;
  Qsize: integer;
begin
  AV_History_nTotalLots(piece, color, Q, Qsize);
  Result := PG_QtyAt(Q, Qsize, at);
  memfree(pointer(Q), Qsize * SizeOf(qtydate_t));
end;

function AV_nTotalQtyAt(const piece: string; const color: integer; const at: TDateTime): integer;
var
  Q: qtydatearray_p;
  Qsize: integer;
begin
  AV_History_nTotalQty(piece, color, Q, Qsize);
  Result := PG_QtyAt(Q, Qsize, at);
  memfree(pointer(Q), Qsize * SizeOf(qtydate_t));
end;

function AV_nMinPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
var
  P: pricedatearray_p;
  Psize: integer;
begin
  AV_History_nMinPrice(piece, color, P, Psize);
  Result := PG_PriceAt(P, Psize, at);
  memfree(pointer(P), Psize * SizeOf(pricedate_t));
end;

function AV_nAvgPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
var
  P: pricedatearray_p;
  Psize: integer;
begin
  AV_History_nAvgPrice(piece, color, P, Psize);
  Result := PG_PriceAt(P, Psize, at);
  memfree(pointer(P), Psize * SizeOf(pricedate_t));
end;

function AV_nQtyAvgPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
var
  P: pricedatearray_p;
  Psize: integer;
begin
  AV_History_nQtyAvgPrice(piece, color, P, Psize);
  Result := PG_PriceAt(P, Psize, at);
  memfree(pointer(P), Psize * SizeOf(pricedate_t));
end;

function AV_nMaxPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
var
  P: pricedatearray_p;
  Psize: integer;
begin
  AV_History_nMaxPrice(piece, color, P, Psize);
  Result := PG_PriceAt(P, Psize, at);
  memfree(pointer(P), Psize * SizeOf(pricedate_t));
end;

function AV_uTotalLotsAt(const piece: string; const color: integer; const at: TDateTime): integer;
var
  Q: qtydatearray_p;
  Qsize: integer;
begin
  AV_History_uTotalLots(piece, color, Q, Qsize);
  Result := PG_QtyAt(Q, Qsize, at);
  memfree(pointer(Q), Qsize * SizeOf(qtydate_t));
end;

function AV_uTotalQtyAt(const piece: string; const color: integer; const at: TDateTime): integer;
var
  Q: qtydatearray_p;
  Qsize: integer;
begin
  AV_History_uTotalQty(piece, color, Q, Qsize);
  Result := PG_QtyAt(Q, Qsize, at);
  memfree(pointer(Q), Qsize * SizeOf(qtydate_t));
end;

function AV_uMinPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
var
  P: pricedatearray_p;
  Psize: integer;
begin
  AV_History_uMinPrice(piece, color, P, Psize);
  Result := PG_PriceAt(P, Psize, at);
  memfree(pointer(P), Psize * SizeOf(pricedate_t));
end;

function AV_uAvgPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
var
  P: pricedatearray_p;
  Psize: integer;
begin
  AV_History_uAvgPrice(piece, color, P, Psize);
  Result := PG_PriceAt(P, Psize, at);
  memfree(pointer(P), Psize * SizeOf(pricedate_t));
end;

function AV_uQtyAvgPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
var
  P: pricedatearray_p;
  Psize: integer;
begin
  AV_History_uQtyAvgPrice(piece, color, P, Psize);
  Result := PG_PriceAt(P, Psize, at);
  memfree(pointer(P), Psize * SizeOf(pricedate_t));
end;

function AV_uMaxPriceAt(const piece: string; const color: integer; const at: TDateTime): double;
var
  P: pricedatearray_p;
  Psize: integer;
begin
  AV_History_uMaxPrice(piece, color, P, Psize);
  Result := PG_PriceAt(P, Psize, at);
  memfree(pointer(P), Psize * SizeOf(pricedate_t));
end;

////////////////////////////////////////////////////////////////////////////////

var
  ixxx: integer;

initialization

  for ixxx := 0 to NUMEXCLUDES - 1 do
  begin
    excludes[ixxx].mindatedbl := s2date1(excludes[ixxx].mindate);
    excludes[ixxx].maxdatedbl := s2date1(excludes[ixxx].maxdate);
  end;

  dd20131231a := s2date1('20131231') + 0.5;
  dd20180904a := s2date1('20180904') + 0.5;
  dd20180907a := s2date1('20180907') + 0.5;

end.
