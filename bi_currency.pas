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

var
  currency_names: TStringList;

type
  TCurrency = class(TObject)
  private
    ffilename: string;
    currencies: TStringList;
    procedure DoSave;
  public
    constructor Create(const afile: string); virtual;
    destructor Destroy; override;
    function Convert(const cur: string): double;
    procedure Update(const cur: string; const value: double);
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
    allowgooglesearch: boolean;
  protected
    procedure Grow;
    procedure LoadFromFile;
    procedure SaveToFile;
    procedure Clear;
    procedure Add(const scurrency: string; const sdate: string; const value: double); overload;
    procedure Add(const scurrency: string; const ddate: TDateTime; const value: double); overload;
    function IndexOf(const scurrency: string; const sdate: string): integer; overload;
    function IndexOf(const scurrency: string; const ddate: TDateTime): integer; overload;
    procedure InternetUpdateMD(const acurrency: string; const ddate: TDateTime);
  public
    constructor Create(const abasecurrencyname: string; const afilename: string); virtual;
    destructor Destroy; override;
    procedure InternetUpdate(const acurrency: string; const ddate: TDateTime);
    procedure UpdateAllCurrenciesAt(const ddate: TDateTime);
    function ConvertAt(const acurrency: string; const dd: TDateTime): double;
    property basecurrencyname: string read fbasecurrencyname write fbasecurrencyname;
    property filename: string read ffilename write ffilename;
  end;

function stringtodate(const s: string): TDateTime;

function datetostring(const dd: TDateTime): string;

implementation

uses
  bi_delphi, bi_db, bi_utils, DateUtils, StrUtils, UrlMon, bi_tmp, bi_cachefile;

const
  C_CURRENCY_NAMES =
    'EUR=Euro'#13#10+
    'USD=US Dollar'#13#10+
    'RUB=Russian Ruble'#13#10+
    'RON=Romanian Leu'#13#10+
    'UAH=Ukraine Hryvnia'#13#10+
    'AED=U.A.E. Dirham'#13#10+
    'ALL=Albanian lek'#13#10+
    'AMD=Armenian Dram'#13#10+
    'AUD=Australian Dollar'#13#10+
    'AZN=Azerbaijanian Manat'#13#10+
    'BGN=Bulgarian Lev'#13#10+
    'BYN=Belarussian Ruble'#13#10+
    'CAD=Canadian Dollar'#13#10+
    'CHF=Swiss Franc'#13#10+
    'CNY=Chinese yuan renminbi'#13#10+
    'CZK=Czech Koruna'#13#10+
    'DKK=Danish Krone'#13#10+
    'GBP=Pound Sterling'#13#10+
    'GEL=Georgian Lar'#13#10+
    'HKD=Hong Kong dollar'#13#10+
    'HRK=Croatian Kuna'#13#10+
    'HUF=Hungarian Forint'#13#10+
    'ILS=Shekel Israelit'#13#10+
    'INR=Indian rupee'#13#10+
    'ISK=Iceland Krona'#13#10+
    'JPY=Japanese Yen'#13#10+
    'KGS=Kyrgyzstan Som'#13#10+
    'KRW=South Korean won'#13#10+
    'KWD=Kuwaiti Dinar'#13#10+
    'KZT=Kazakhstan Tenge'#13#10+
    'MKD=Macedonian denar'#13#10+
    'MYR=Malaysian Ringgit'#13#10+
    'NOK=Norwegian Krone'#13#10+
    'NZD=New Zealand Dollar'#13#10+
    'PLN=Polish Zloty'#13#10+
    'RSD=Serbian Dinar'#13#10+
    'SEK=Swedish Krona'#13#10+
    'TJS=Tajikistan Somoni'#13#10+
    'TMT=Turkmenistan Manat'#13#10+
    'TRY=Turkish Lira'#13#10+
    'UZS=Uzbekistan Sum'#13#10+
    'XDR=Special Drawing Rights'#13#10;

constructor TCurrency.Create(const afile: string);
var
  s: TStringList;
  i: integer;
  s1, s2: string;
begin
  currencies := TStringList.Create;
  ffilename := afile;
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

procedure TCurrency.Update(const cur: string; const value: double);
var
  idx: integer;
begin
  idx := currencies.IndexOf(cur);
  if idx < 0 then
    currencies.AddObject(cur, TDouble.Create(value))
  else if (currencies.Objects[idx] as TDouble).value <> value then
    (currencies.Objects[idx] as TDouble).value := value
  else
    Exit;

  DoSave;
end;

procedure TCurrency.DoSave;
var
  s: TStringList;
  i: integer;
begin
  s := TStringList.Create;
  try
    s.Add('currency,euro');
    for i := 0 to currencies.Count - 1 do
      s.Add(currencies.Strings[i] + ',' + Format('%1.15f', [(currencies.Objects[i] as TDouble).value]));
    backupfile(ffilename);
    S_SaveToFile(s, ffilename);
  finally
    s.Free;
  end;
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
  allowgooglesearch := false;
  fbasecurrencyname := UpperCase(abasecurrencyname);
  A := nil;
  fnumitems := 0;
  frealnumitems := 0;
  Inherited Create;
  LoadFromFile;
  UpdateAllCurrenciesAt(Now);
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

procedure TCurrencyConvert.UpdateAllCurrenciesAt(const ddate: TDateTime);
var
  i: integer;
  cname: string;
begin
  if ddate <> 0.0 then
    for i := 0 to currency_names.Count - 1 do
    begin
      cname := strupper(currency_names.Names[i]);
      if cname <> fbasecurrencyname then
        if IndexOf(cname, ddate) < 0 then
          InternetUpdate(cname, ddate);
    end;
end;

procedure TCurrencyConvert.InternetUpdate(const acurrency: string; const ddate: TDateTime);
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
  if allowgooglesearch and (datetostring(ddate) = datetostring(Now)) then
  begin
    urlstr := 'https://www.google.com/search?q=+' + LowerCase(acurrency) + '+to+' + LowerCase(fbasecurrencyname);
    tmpname := I_NewTempFile('currency_' + acurrency + '_' + fbasecurrencyname + '_' + itoa(random(1000)) + '.html');
    ret := UrlDownloadToFile(nil, PChar(urlstr), PChar(tmpname), 0, nil) = 0;
  end
  else
    ret := False;
  if not ret then
  begin
    InternetUpdateMD(acurrency, ddate);
    Exit;
  end;

  htm := LoadStringFromFile(tmpname);
  check := '<span class="DFlfde SwHCTb" data-precision="2" data-value=';
  p := Pos(check, htm);
  if p < 1 then
  begin
    check := '<div class="dDoNo vk_bk">';
    p := Pos(check, htm);
    if p < 1 then
    begin // give a second chance
      p := Pos(UpperCase(check), UpperCase(htm));
      if p < 1 then
      begin
        InternetUpdateMD(acurrency, Now);
        Exit;
      end;
    end;
  end;
  svalue := '';
  for i := p + length(check) - 1 to length(htm) do
  begin
    if htm[i] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', ','] then
      svalue := svalue + htm[i]
    else if htm[i] in ['<', '/', '>'] then
      break;
  end;
  Add(acurrency, Now, atof(svalue));
  SaveToFile;
end;

procedure TCurrencyConvert.InternetUpdateMD(const acurrency: string; const ddate: TDateTime);
var
  spath: string;
  sdate: string;
  urlstr: string;
  ret: boolean;
  sl: TStringList;
  i: integer;
  idx: integer;
  eur_value: Double;
  eur_nominal: integer;
  cur_value: Double;
  cur_nominal: integer;
  s: string;
begin
  if not DirectoryExists(basedefault + 'db\') then
    ForceDirectories(basedefault + 'db\');
  if not DirectoryExists(basedefault + 'db\currency\') then
    ForceDirectories(basedefault + 'db\currency\');

  sdate := FormatDateTime('dd.mm.yyyy', ddate);
  spath := basedefault + 'db\currency\' + sdate + '.xml';

  if not fexists(spath) then
  begin
    urlstr := 'https://www.bnm.md/en/official_exchange_rates?get_xml=1&date=' + sdate;
    ret := UrlDownloadToFile(nil, PChar(urlstr), PChar(spath), 0, nil) = 0;
    if not ret then
      Exit;
  end;

  if not fexists(spath) then
    Exit;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(spath);
    for i := 0 to sl.Count - 1 do
      sl.Strings[i] := Trim(sl.Strings[i]);

    eur_value := -1.0;
    eur_nominal := -1;

    idx := sl.IndexOf('<CharCode>' + fbasecurrencyname + '</CharCode>');
    if idx >= 0 then
    begin
      for i := idx + 1 to idx + 7 do
      begin
        if i = sl.Count then
          Break;
        s := sl.Strings[i];
        if eur_value < 0 then
          if Pos1('<Value>', s) then
          begin
            s := StringReplace(s, '<Value>', '', [rfReplaceAll, rfIgnoreCase]);
            s := StringReplace(s, '</Value>', '', [rfReplaceAll, rfIgnoreCase]);
            trimproc(s);
            eur_value := atof(s);
          end;
        if eur_nominal < 0 then
          if Pos1('<Nominal>', s) then
          begin
            s := StringReplace(s, '<Nominal>', '', [rfReplaceAll, rfIgnoreCase]);
            s := StringReplace(s, '</Nominal>', '', [rfReplaceAll, rfIgnoreCase]);
            trimproc(s);
            eur_nominal := atoi(s);
          end;
      end;
    end;

    cur_value := -1.0;
    cur_nominal := -1;

    if (eur_value > 0) and (eur_nominal > 0) then
    begin
      idx := sl.IndexOf('<CharCode>' + UpperCase(acurrency)+ '</CharCode>');
      if idx >= 0 then
      begin
        for i := idx + 1 to idx + 7 do
        begin
          if i = sl.Count then
            Break;
          s := sl.Strings[i];
          if cur_value < 0 then
            if Pos1('<Value>', s) then
            begin
              s := StringReplace(s, '<Value>', '', [rfReplaceAll, rfIgnoreCase]);
              s := StringReplace(s, '</Value>', '', [rfReplaceAll, rfIgnoreCase]);
              trimproc(s);
              cur_value := atof(s);
            end;
          if cur_nominal < 0 then
            if Pos1('<Nominal>', s) then
            begin
              s := StringReplace(s, '<Nominal>', '', [rfReplaceAll, rfIgnoreCase]);
              s := StringReplace(s, '</Nominal>', '', [rfReplaceAll, rfIgnoreCase]);
              trimproc(s);
              cur_nominal := atoi(s);
            end;
        end;
      end;
    end;

    if (cur_value > 0) and (cur_nominal > 0) and (eur_value > 0) and (eur_nominal > 0) then
    begin
      Add(acurrency, ddate, (cur_value / cur_nominal) / (eur_value / eur_nominal));
      SaveToFile;
    end;

  finally
    sl.Free;
  end;
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
  if value <= 0.0000000001 then
    Exit;

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

initialization
  currency_names := TStringList.Create;
  currency_names.Text := C_CURRENCY_NAMES;

finalization
  currency_names.Free;

end.
