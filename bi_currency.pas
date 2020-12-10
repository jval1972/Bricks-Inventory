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
//    Currency convertion
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_currency;

interface

uses
  SysUtils, Classes;

type
  TCurrency = class(TObject)
  private
    currencies: TStringList;
  public
    constructor Create(const afile: string); virtual;
    destructor Destroy; override;
    function Convert(const cur: string): double;
  end;

type
  currencyconvert_t = record
    currency: string[3];
    sdate: string[8];
    date: double;
    value: double;
  end;
  currencyconvert_p = ^currencyconvert_t;
  currencyconvert_a = array[0..$FFFF] of currencyconvert_t;
  currencyconvert_pa = ^currencyconvert_a;

  TCurrencyConvert = class(TObject)
  private
    fbasecurrencyname: string;
    ffilename: string;
    A: currencyconvert_pa;
    fnumitems: integer;
    frealnumitems: integer;
  protected
    procedure Grow;
    procedure LoadFromFile;
    procedure SaveToFile;
    procedure Clear;
    procedure Add(const scurrency: string; const sdate: string; const value: double); overload;
    procedure Add(const scurrency: string; const ddate: TDateTime; const value: double); overload;
    function IndexOf(const scurrency: string; const sdate: string): integer; overload;
    function IndexOf(const scurrency: string; const ddate: TDateTime): integer; overload;
  public
    constructor Create(const abasecurrencyname: string; const afilename: string); virtual;
    destructor Destroy; override;
    procedure InternetUpdate(const acurrency: string);
    function ConvertAt(const acurrency: string; const dd: TDateTime): double;
    property basecurrencyname: string read fbasecurrencyname write fbasecurrencyname;
    property filename: string read ffilename write ffilename;
  end;

implementation

uses
  bi_delphi, bi_utils, DateUtils, StrUtils, UrlMon, bi_tmp;

constructor TCurrency.Create(const afile: string);
var
  s: TStringList;
  i: integer;
  s1, s2: string;
begin
  currencies := TStringList.Create;
  if fexists(afile) then
  begin
    s := TStringList.Create;
    try
      S_LoadFromFile(s, afile);
      if s.Count > 0 then
        if s.Strings[0] = 'currency,euro' then
          for i := 1 to s.Count - 1 do
          begin
            splitstring(s.Strings[i], s1, s2, ',');
            currencies.AddObject(s1, TDouble.Create(atof(s2, 1.0)));
          end;
    finally
      s.Free;
    end;
  end;
  currencies.Sorted := True;
  Inherited Create;
end;

destructor TCurrency.Destroy;
begin
  FreeList(currencies);
  Inherited;
end;

function TCurrency.Convert(const cur: string): double;
var
  idx: integer;
begin
  idx := currencies.IndexOf(cur);
  if idx < 0 then
    Result := 1.0
  else
    Result := (currencies.Objects[idx] as TDouble).value;
end;

////////////////////////////////////////////////////////////////////////////////
function stringtodate(const s: string): TDateTime;
var
  ayear, amonth, aday: word;
begin
  if Length(s) <> 8 then
  begin
    Result := 0.0;
    Exit;
  end;
  ayear := atoi(leftstr(s, 4));
  amonth := atoi(rightstr(leftstr(s, 6), 2));
  aday := atoi(rightstr(s, 2));
  if not TryEncodeDateTime(ayear, amonth, aday, 0, 0, 0, 0, result) then
    result := 0.0;
end;

function datetostring(const dd: TDateTime): string;
begin
  Result := FormatDateTime('yyyymmdd', dd);
end;

constructor TCurrencyConvert.Create(const abasecurrencyname: string; const afilename: string);
begin
  ffilename := afilename;
  fbasecurrencyname := UpperCase(abasecurrencyname);
  A := nil;
  fnumitems := 0;
  frealnumitems := 0;
  Inherited Create;
  LoadFromFile;
  if IndexOf('USD', Now) < 0 then
    InternetUpdate('USD');
  if IndexOf('GBP', Now) < 0 then
    InternetUpdate('GBP');
  if IndexOf('CZK', Now) < 0 then
    InternetUpdate('CZK');
  if IndexOf('PLN', Now) < 0 then
    InternetUpdate('PLN');
  if IndexOf('CAD', Now) < 0 then
    InternetUpdate('CAD');
  if IndexOf('AUD', Now) < 0 then
    InternetUpdate('AUD');
  if IndexOf('HUF', Now) < 0 then
    InternetUpdate('HUF');
end;

destructor TCurrencyConvert.Destroy;
begin
//  SaveToFile;
  Clear;
  Inherited;
end;

procedure TCurrencyConvert.Clear;
begin
  ReallocMem(A, 0);
  fnumitems := 0;
  frealnumitems := 0;
end;

procedure TCurrencyConvert.InternetUpdate(const acurrency: string);
var
  urlstr: string;
  ret: boolean;
  tmpname: string;
  htm: string;
  p: integer;
  i: integer;
  svalue: string;
  check: string;
begin
  urlstr := 'https://www.google.com/search?q=+' + LowerCase(acurrency) + '+to+' + LowerCase(fbasecurrencyname);
  tmpname := I_NewTempFile('currency_' + acurrency + '_' + fbasecurrencyname + '_' + itoa(random(1000)) + '.html');
  ret := UrlDownloadToFile(nil, PChar(urlstr), PChar(tmpname), 0, nil) = 0;
  if not ret then
    Exit;

  htm := LoadStringFromFile(tmpname);
  check := '<div class="dDoNo vk_bk">';
  p := Pos(check, htm);
  if p < 1 then
  begin // give a second chance
    p := Pos(UpperCase(check), UpperCase(htm));
    if p < 1 then
      Exit;
  end;
  svalue := '';
  for i := p + length(check) - 1 to length(htm) do
  begin
    if htm[i] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', ','] then
      svalue := svalue + htm[i]
    else if htm[i] in ['<', '/'] then
      break;
  end;
  Add(acurrency, Now, atof(svalue));
  SaveToFile;
end;

function TCurrencyConvert.ConvertAt(const acurrency: string; const dd: TDateTime): double;
var
  index1, index2: integer;
  date1, date2: double;
  diff: double;
  min1, min2: double;
  value1, value2: double;
  check: string;
  i: integer;
begin
  check := UpperCase(acurrency);
  if check = UpperCase(fbasecurrencyname) then
  begin
    Result := 1.0;
    Exit;
  end;

  date1 := stringtodate(datetostring(dd));
  date2 := stringtodate(datetostring(dd + 1));
  index1 := -1;
  index2 := -1;
  min1 := 9999999999.0;
  min2 := 9999999999.0;

  for i := 0 to fnumitems - 1 do
    if check = A[i].currency then
    begin
      if date1 >= A[i].date then
      begin
        diff := date1 - A[i].date;
        if diff < min1 then
        begin
          min1 := diff;
          index1 := i;
        end;
      end;
      if date2 <= A[i].date then
      begin
        diff := A[i].date - date2;
        if diff < min2 then
        begin
          min2 := diff;
          index2 := i;
        end;
      end;
    end;

  if (index1 = -1) and (index2 > -1) then
  begin
    Result := A[index2].value;
    Exit;
  end;

  if (index1 > -1) and (index2 = -1) then
  begin
    Result := A[index1].value;
    Exit;
  end;

  if (index1 > -1) and (index2 > -1) then
  begin
    date1 := A[index1].date;
    date2 := A[index2].date;
    if date1 = date2 then
    begin
      Result := A[index1].value;
      Exit;
    end;

    value1 := A[index1].value;
    value2 := A[index2].value;
    if value1 = value2 then
    begin
      Result := value1;
      Exit;
    end;

    Result := value2 - ((value2 - value1) * (date2 - dd)) / (date2 - date1);
    Exit;
  end;

  Result := 1.0;
end;

procedure TCurrencyConvert.Grow;
var
  delta: integer;
begin
  if fnumitems >= frealnumitems then
  begin
    if frealnumitems < 16 then
      delta := 4
    else if frealnumitems < 64 then
      delta := 8
    else if frealnumitems < 256 then
      delta := 16
    else if frealnumitems < 1024 then
      delta := 64
    else
      delta := 256;
    frealnumitems := frealnumitems + delta;
    ReallocMem(A, frealnumitems * SizeOf(currencyconvert_t));
  end;
end;

procedure TCurrencyConvert.Add(const scurrency: string; const sdate: string; const value: double);
var
  pc: currencyconvert_p;
begin
  Grow;
  pc := @A[fnumitems];
  pc.currency := UpperCase(scurrency);
  pc.sdate := sdate;
  pc.date := stringtodate(sdate);
  pc.value := value;
  inc(fnumitems);
end;

procedure TCurrencyConvert.Add(const scurrency: string; const ddate: TDateTime; const value: double);
var
  pc: currencyconvert_p;
begin
  Grow;
  pc := @A[fnumitems];
  pc.currency := UpperCase(scurrency);
  pc.sdate := datetostring(ddate);
  pc.date := stringtodate(pc.sdate);
  pc.value := value;
  inc(fnumitems);
end;

procedure TCurrencyConvert.LoadFromFile;
var
  s: TStringList;
  i: integer;
  scurrency, sdate, svalue: string;
begin
  Clear;
  if fexists(ffilename) then
  begin
    s := TStringList.Create;
    try
      S_LoadFromFile(s, ffilename);
      if s.Count > 0 then
      begin
        if s.Strings[0] = 'currency,date,value' then
          s.Delete(0);
        for i := 0 to s.Count - 1 do
        begin
          splitstring(s.Strings[i], scurrency, sdate, svalue, ',');
          Add(scurrency, sdate, atof(svalue));
        end;
      end;
    finally
      s.Free;
    end;
  end;
end;

procedure TCurrencyConvert.SaveToFile;
var
  s: TStringList;
  i: integer;
begin
  s := TStringList.Create;
  try
    s.Add('currency,date,value');
    for i := 0 to fnumitems - 1 do
      s.Add(A[i].currency + ',' + A[i].sdate + ',' + Format('%1.15f', [A[i].value]));
    backupfile(ffilename);
    S_SaveToFile(s, ffilename);
  finally
    s.Free;
  end;
end;

function TCurrencyConvert.IndexOf(const scurrency: string; const sdate: string): integer;
var
  i: integer;
  check: string;
begin
  if Length(sdate) <> 8 then
  begin
    Result := -1;
    Exit;
  end;

  check := UpperCase(scurrency);
  for i := 0 to fnumitems - 1 do
    if check = A[i].currency then
      if sdate = A[i].sdate then
      begin
        Result := i;
        Exit;
      end;

  Result := -1;
end;

function TCurrencyConvert.IndexOf(const scurrency: string; const ddate: TDateTime): integer;
begin
  Result := IndexOf(scurrency, datetostring(ddate));
end;

end.
