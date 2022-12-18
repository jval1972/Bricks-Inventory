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
//    Internet Crawler
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_crawler;

interface

uses
  WinInet, SysUtils, Classes, bi_db;

var
  type1_pg_hits: integer = 0;
  type2_pg_hits: integer = 0;
  last_pg_hit: integer = 0;
  allowtype1_hits: boolean = False;

var
  useUSDvalues: boolean = False;
  useVATValues: boolean = True;

function NET_GetPriceGuideForElement(const pci: TPieceColorInfo; id: string; const color: string;
  var ret1: priceguide_t; var ret2: availability_t; const cachefile: string;
  var clink: string): boolean;

function NET_GetBricklinkAlias(const id: string; const typ: char = 'P'): string;

function NET_GetBricklinkCategory(const id: string; var fcat: integer; var fweight: double; var parttype: char): boolean;

function NET_GetItemWeightFromDisk(const id: string; var fweight: double): boolean;

function NET_GetItemWeightFromText(const txt: string; var fweight: double): boolean;

function NET_GetBricklinkMinifigCategory(const id: string; var fcat: integer; var fweight: double): boolean;

function NET_GetBricklinkMinifigWeight(const id: string; var fweight: double): boolean;

function NET_GetURLHeadByte(const URL: string; XXX: integer = 0): byte;

function NET_GetURLString(const URL: string): string;

function NET_DownloadFile(const URL: string; const fname: string): boolean;

var
  AllowInternetAccess: Boolean = True;

function issticker(const part: string; const pci: TPieceColorInfo = nil): Boolean;

{$IFDEF CRAWLER}
var
  USDwarning: Boolean = False;
  FAILwarning: Boolean = False;
{$ENDIF}

function NET_ExistsCache(const id: string; const hits: string; out fname: string): boolean;

implementation

uses
  Windows, bi_delphi, bi_script, bi_utils, bi_system, bi_globals, bi_cachefile,
  strutils;

var
  hSession: HInternet;

procedure OpenInetConnection;
begin
  hSession := InternetOpen(
                 'Microsoft Internet Explorer',  // agent. (can be "Microsoft Internet Explorer")
                 INTERNET_OPEN_TYPE_PRECONFIG,   // access
                 nil,                            // proxy server
                 nil,                            // defauts
                 0);                             // synchronous
end;

procedure CloseInetConnection;
begin
  InternetCloseHandle(hSession); // close connection to internet
end;

const
  BUFFERSIZE = $4000;
  MAXRETRY = 3;
  DELAYR = 10;

function NET_GetURLHeadByte(const URL: string; XXX: integer = 0): byte;
var
  hURL: HInternet;
  Buffer: Byte;
  BufferLen: Cardinal;
begin
  if XXX > MAXRETRY then
  begin
    Result := 0;
    Exit;
  end;

  if XXX = MAXRETRY then
  begin
    CloseInetConnection;
    OpenInetConnection;
  end;

  hURL := InternetOpenURL(
                 hSession,          // Handle to current session
                 PChar(URL),        // URL to read
                 nil,               // HTTP headers to send to server
                 0,                 // Header length
                 0, 0);             // flags   (might want to add some like INTERNET_FLAG_RELOAD with forces a reload from server and not from cache)

  if hURL = nil then
  begin
    inc(XXX);
    sleep(DELAYR);
    Result := NET_GetURLHeadByte(URL, XXX);
    Exit;
  end;

  Result := 0;

  InternetReadFile(
        hURL,                  // File URL
        @Buffer,               // Buffer that receives data
        SizeOf(Buffer),        // bytes to read
        BufferLen);            // bytes read
  if BufferLen > 0 then
    Result := Buffer;
end;

function GetURLString(const URL: string; XXX: integer = 0): string;
var
  hURL: HInternet;
  Buffer: array[1..BUFFERSIZE] of Char;
  BufferLen: Cardinal;
  i: integer;
  len: integer;
begin
  if not AllowInternetAccess then
  begin
    Result := '';
    Exit;
  end;

  if XXX > MAXRETRY then
  begin
    Result := '';
    Exit;
  end;

  if XXX = MAXRETRY then
  begin
    CloseInetConnection;
    OpenInetConnection;
  end;

  hURL := InternetOpenURL(
                 hSession,          // Handle to current session
                 PChar(URL),        // URL to read
                 nil,               // HTTP headers to send to server
                 0,                 // Header length
                 0, 0);             // flags   (might want to add some like INTERNET_FLAG_RELOAD with forces a reload from server and not from cache)

  if hURL = nil then
  begin
    inc(XXX);
    sleep(DELAYR);
    Result := GetURLString(URL, XXX);
    Exit;
  end;

  Result := '';

  repeat
    InternetReadFile(
        hURL,                  // File URL
        @Buffer,               // Buffer that receives data
        SizeOf(Buffer),        // bytes to read
        BufferLen);            // bytes read
    if BufferLen > 0 then
    begin
      len := Length(Result);
      SetLength(Result, Length(Result) + BufferLen);
      for i := 1 to BufferLen do
        Result[len + i] := Buffer[i];
    end;
  until BufferLen = 0;
end;

function NET_GetURLString(const URL: string): string;
var
  hURL: HInternet;
  Buffer: array[1..BUFFERSIZE] of Char;
  BufferLen: Cardinal;
  i: integer;
  len: integer;
begin
  hURL := InternetOpenURL(
                 hSession,          // Handle to current session
                 PChar(URL),        // URL to read
                 nil,               // HTTP headers to send to server
                 0,                 // Header length
                 0, 0);             // flags   (might want to add some like INTERNET_FLAG_RELOAD with forces a reload from server and not from cache)

  if hURL = nil then
  begin
    Result := '';
    Exit;
  end;

  Result := '';

  repeat
    InternetReadFile(
        hURL,                  // File URL
        @Buffer,               // Buffer that receives data
        SizeOf(Buffer),        // bytes to read
        BufferLen);            // bytes read
    if BufferLen > 0 then
    begin
      len := Length(Result);
      SetLength(Result, Length(Result) + BufferLen);
      for i := 1 to BufferLen do
        Result[len + i] := Buffer[i];
    end;
  until BufferLen = 0;
end;

function NET_DownloadFile(const URL: string; const fname: string): boolean;
var
  hURL: HInternet;
  Buffer: array[1..BUFFERSIZE] of Char;
  BufferLen: Cardinal;
  f: TFileStream;
  path: string;
begin
  hURL := InternetOpenURL(
                 hSession,          // Handle to current session
                 PChar(URL),        // URL to read
                 nil,               // HTTP headers to send to server
                 0,                 // Header length
                 0, 0);             // flags   (might want to add some like INTERNET_FLAG_RELOAD with forces a reload from server and not from cache)

  if hURL = nil then
  begin
    Result := False;
    Exit;
  end;

  path := ExtractFilePath(fname);
  if not DirectoryExists(path) then
    ForceDirectories(path);
    
  f := TFileStream.Create(fname, fmCreate);

  repeat
    InternetReadFile(
        hURL,                  // File URL
        @Buffer,               // Buffer that receives data
        SizeOf(Buffer),        // bytes to read
        BufferLen);            // bytes read
    if BufferLen > 0 then
      f.Write(Buffer, BufferLen);
  until BufferLen = 0;

  Result := f.Size > 0;
  f.Free;
end;

function issticker(const part: string; const pci: TPieceColorInfo = nil): Boolean;
var
  p: integer;
  pi: TPieceInfo;
begin
  p := Pos('stk', part);
  if (p > 1) and (p + 3 <= Length(part)) then
    if IsNumericC(part[p - 1]) then
      if IsNumericC(part[p + 3]) then
      begin
        Result := True;
        Exit;
      end;

  if pci <> nil then
  begin
    pi := TPieceInfo(pci.pieceinfo);
    if pi <> nil then
      if pi.category = 160 then // Sticker
      begin
        Result := True;
        Exit;
      end;
  end;

  Result := False;
end;

function NET_MakePriceGuideLink2(const typ: string; const itemid: string;
  const blcolor: integer; const dec: integer = 6): string;
var
  cI: char;
  cV: char;
begin
  if useUSDvalues and not useVATValues then
  begin
    Result := 'https://' + BL_NET + '/priceguide.asp?' +
                'a=' + typ +
                '&itemid=' + itemid +
                '&colorID=' + itoa(blcolor) +
                '&viewdec=' + itoa(dec);
    if (typ = 's') or (typ = 'S') then
      Result := Result + '&viewMy=&viewExclude=Y';
    Exit;
  end;
  if useUSDValues then
    cI := '1'
  else
    cI := '2';
  if useVATValues then
    cV := 'y'
  else
    cV := 'n';

  Result := 'http://' + BL_NET + '/priceGuideSummary.asp?' +
              'vcID=' + cI +
              '&vatInc=' + cV +
              '&a=' + typ +
              '&itemID=' + itemid +
              '&colorID=' + itoa(blcolor) +
              '&viewdec=' + itoa(dec);
  if (typ = 's') or (typ = 'S') then
    Result := Result + '&viewMy=&viewExclude=Y';
end;

function SaveParec2(const id: string; const color: string;
  const ret1: priceguide_t; const ret2: availability_t; const currency: string;
  const link: string): boolean;
var
  parec2: parecdate2_t;
  directory: string;
  filename2: string;
  fs: TFileStream;
  i: integer;
  ok: boolean;
begin
  Result := False;
  directory := PieceColorCacheDir(id, color);
  if not DirectoryExists(directory) then
    ForceDirectories(directory);
  filename2 := PieceColorCacheFName2(id, color);

  ZeroMemory(@parec2, SizeOf(parecdate2_t));
  parec2.priceguide := ret1;
  parec2.availability := ret2;
  parec2.date := Now;
  parec2.currency := currency;
  parec2.url := link;

  ok := True;
  for i := 1 to 10 do
  begin
    try
      if fexists(filename2) then
      begin
        fs := TFileStream.Create(filename2, fmOpenReadWrite);
        fs.Position := fs.Size;
      end
      else
        fs := TFileStream.Create(filename2, fmCreate);
      try
        fs.Write(parec2, SizeOf(parecdate2_t));
      finally
        fs.Free;
      end;
    except
      ok := False;
      Sleep(100);
    end;
    if ok then
      Break;
  end;
end;

function SaveParec3(const id: string; const color: string;
  const ret1: priceguide_t; const ret2: availability_t; const currency: string;
  const link: string; const vat: boolean): boolean;
var
  parec3: parecdate3_t;
  directory: string;
  filename3: string;
  fs: TFileStream;
  i: integer;
  ok: boolean;
  link1, link2: string;
begin
  Result := False;
  directory := PieceColorCacheDir(id, color);
  if not DirectoryExists(directory) then
    ForceDirectories(directory);
  filename3 := PieceColorCacheFName3(id, color);

  ZeroMemory(@parec3, SizeOf(parecdate3_t));
  parec3.priceguide := ret1;
  parec3.availability := ret2;
  parec3.date := Now;
  parec3.currency := currency;
  if Length(Trim(link)) > 127 then
  begin
    splitstring(Trim(link), link1, link2, ':');
    if Pos1('//', link2) then
    begin
      link2[1] := ' ';
      link2[2] := ' ';
      link2 := Trim(link2);
      if Length(link2) > 127 then
      begin
        if Pos1('www.', link2) then
        begin
          link2[1] := ' ';
          link2[2] := ' ';
          link2[3] := ' ';
          link2[4] := ' ';
          link2 := Trim(link2);
          if Length(link2) > 127 then
            parec3.url := RightStr(link2, 127)
          else
            parec3.url := link2;
        end
        else
          parec3.url := RightStr(link2, 127)
      end
      else
        parec3.url := link2;
    end
    else
      parec3.url := RightStr(Trim(link), 127)
  end
  else
    parec3.url := Trim(link);
  parec3.vat := vat;

  ok := True;
  for i := 1 to 10 do
  begin
    try
      if fexists(filename3) then
      begin
        fs := TFileStream.Create(filename3, fmOpenReadWrite);
        fs.Position := fs.Size;
      end
      else
        fs := TFileStream.Create(filename3, fmCreate);
      try
        fs.Write(parec3, SizeOf(parecdate3_t));
      finally
        fs.Free;
      end;
    except
      ok := False;
      Sleep(100);
    end;
    if ok then
      Break;
  end;
end;

function HTMLLinkParseParam(const htms: string; const QRY: string; const sdefault: string = ''): string;
var
  i: integer;
  parmlist: TStringList;
  sss1, sss2: string;
  check: string;
begin
  Result := sdefault;
  splitstring(htms, sss1, sss2, '?');
  parmlist := string2stringlist(sss2, '&');
  check := UpperCase(QRY);
  for i := 0 to parmlist.Count - 1 do
  begin
    splitstring(parmlist.Strings[i], sss1, sss2, '=');
    if UpperCase(sss1) = check then
    begin
      Result := sss2;
      Break;
    end;
  end;
  parmlist.Free;
end;

function NET_GetPriceGuideForElement2(const pci: TPieceColorInfo; const id: string;
  const color: string; var ret1: priceguide_t; var ret2: availability_t;
  const cachefile: string; var clink: string; const forcelink: string = ''): boolean;
var
  link: string;
  htm: string;
  typ: string;
  itemid: string;
  blcolor: integer;
  cc: integer;
  result_t: array[1..24] of Double;
  i: integer;
  SL: TStringList;
  sLine: TStringList;
  line: string;
  group: integer;
  idx: integer;
  USDval: double;
  skeep: string;
  cstr: string;
  savetyp: boolean;

  function atofdollar(ss: string): double;
  var
    p1, p2: integer;
    tmp: string;
    ii: integer;
  begin
    ss := Trim(ss);
    if ss = '' then
    begin
      Result := 0.0;
      Exit;
    end;
    if ss[1] <> '$' then
    begin
      Result := 0.0;
      Exit;
    end;
    ss[1] := ' ';
    ss := Trim(ss);
    if ss = '' then
    begin
      Result := 0.0;
      Exit;
    end;

    p1 := CharPos(',', ss);
    p2 := CharPos('.', ss);

    if (p1 > 0) and (p2 > 0) then
    begin
      if p2 < p1 then
        if DecimalSeparator <> '.' then
        begin
          ss[p1] :='.';
          ss[p2] :=',';
        end;
      tmp := '';
      for ii := 1 to Length(ss) do
        if ss[ii] <> ',' then
          tmp := tmp + ss[ii];
      Result := atof(tmp);
      Exit;
    end;

    if (p1 > 0) and (p2 = 0) then
    begin
      if ss[p1] <> DecimalSeparator then
        ss[p1] := DecimalSeparator;
    end
    else if (p1 = 0) and (p2 > 0) then
    begin
      if ss[p2] <> DecimalSeparator then
        ss[p2] := DecimalSeparator;
    end;
    Result := atof(ss);
  end;

  function atofeuro(ss: string): double;
  var
    p1, p2: integer;
    tmp: string;
    ii: integer;
  begin
    ss := Trim(ss);
    if ss = '' then
    begin
      Result := 0.0;
      Exit;
    end;
    if not Pos1('EUR ', ss) then
    begin
      Result := 0.0;
      Exit;
    end;
    ss[1] := ' ';
    ss[2] := ' ';
    ss[3] := ' ';
    ss := Trim(ss);
    if ss = '' then
    begin
      Result := 0.0;
      Exit;
    end;

    p1 := CharPos(',', ss);
    p2 := CharPos('.', ss);

    if (p1 > 0) and (p2 > 0) then
    begin
      if p2 < p1 then
        if DecimalSeparator <> '.' then
        begin
          ss[p1] :='.';
          ss[p2] :=',';
        end;
      tmp := '';
      for ii := 1 to Length(ss) do
        if ss[ii] <> ',' then
          tmp := tmp + ss[ii];
      Result := atof(tmp);
      Exit;
    end;

    if (p1 > 0) and (p2 = 0) then
    begin
      if ss[p1] <> DecimalSeparator then
        ss[p1] := DecimalSeparator;
    end
    else if (p1 = 0) and (p2 > 0) then
    begin
      if ss[p2] <> DecimalSeparator then
        ss[p2] := DecimalSeparator;
    end;
    Result := atof(ss);
  end;

begin
  Result := False;
  itemid := db.GetBLNetPieceName(id);
  typ := pci.ItemType;
  savetyp := typ <> pci.sparttype;
  if (Length(typ) = 1) and (typ[1] in ['P', 'S', 'M', 'C', 'B', 'I', 'O', 'G']) then
  begin
    cc := StrToIntDef(color, -2);
    if (cc < -1) or (cc > LASTNORMALCOLORINDEX) then
      blcolor := 0
    else
      blcolor := db.colors(cc).BrickLingColor;
  end
  else
  begin
    if (color = '89') or (color = '') or ((color = '-1') and (Pos('-', id) > 0))  then // set
    begin
      typ := 'S';
      blcolor := 0;
    end
    else if color = '-1' then
    begin
      if issticker(id, pci) then // sticker
      begin
        typ := 'P';
        blcolor := 0;
      end
      else if db.isbook(id) then  // book
      begin
        typ := 'B';
        blcolor := 0;
      end
      else if db.IsGear(id) then
      begin
        typ := 'G';
        blcolor := 0
      end
      else if db.GetMoldNumNormalPartColors(id) > 0 then
      begin
        typ := 'P';
        blcolor := 0
      end
      else // minifigure
      begin
        typ := 'M';
        blcolor := 0;
      end;
    end
    else if color = '9996' then // Catalog
    begin
      typ := 'C';
      blcolor := 0;
    end
    else if color = '9997' then // Instructions
    begin
      typ := 'I';
      blcolor := 0;
    end
    else if color = '9998' then // Original Box
    begin
      typ := 'O';
      blcolor := 0;
    end
    else  // Part
    begin
      typ := 'P';
      cc := StrToIntDef(color, -2);
      if (cc < -1) or (cc > LASTNORMALCOLORINDEX) then
        blcolor := 0
      else
        blcolor := db.colors(cc).BrickLingColor;
    end;
  end;

  if forcelink <> '' then
  begin
    link := forcelink;
    savetyp := False;
  end
  else
    link := NET_MakePriceGuideLink2(typ, itemid, blcolor);
  htm := GetURLString(link);
  if Pos('This item has not been sold or listed for sale yet', htm) > 0 then
    Exit;
 // SaveStringToFile('e:\test1.htm', htm);
  if Pos('(No Data)', htm) > 0 then
    Exit;

  htm := StringReplace(htm, '&nbsp;', ' ', [rfReplaceAll, rfIgnoreCase]);
  htm := StringReplace(htm, #10, '', [rfReplaceAll, rfIgnoreCase]);
  htm := StringReplace(htm, #13, '', [rfReplaceAll, rfIgnoreCase]);
  htm := StringReplace(htm, '<tr>', #13#10'<tr>', [rfReplaceAll, rfIgnoreCase]);
  htm := StringReplace(htm, '<TR ', #13#10'<TR ', [rfReplaceAll, rfIgnoreCase]);

  skeep := htm;

  FillChar(result_t, SizeOf(result_t), 0);

  if useUSDvalues then
  begin
    USDval := db.ConvertCurrencyAt('USD', Now);
    cstr := '$'
  end
  else
  begin
    USDval := 1.0;
    cstr := 'EUR'
  end;

  group := 0;
  SL := TStringList.Create;
  sLine := TStringList.Create;
  try
    SL.Text := htm;
    for i := 0 to SL.Count - 1 do
    begin
      if Pos(UpperCase('Past 6 Months Sales'), UpperCase(SL.Strings[i])) > 0 then
        group := 1
      else if Pos(UpperCase('Current Items for Sale'), UpperCase(SL.Strings[i])) > 0 then
        group := 2
      else if group > 0 then
      begin
        line := StringReplace(SL.Strings[i], '</td>', #13#10, [rfReplaceAll, rfIgnoreCase]);
        sLine.Text := line;
        if sLine.Count = 8 then
        begin
          sLine.Strings[0] := htmlstripstring(sLine.Strings[0]);
          sLine.Strings[1] := Trim(htmlstripstring(sLine.Strings[1]));
          sLine.Strings[2] := Trim(htmlstripstring(sLine.Strings[2]));
          sLine.Strings[3] := Trim(htmlstripstring(sLine.Strings[3]));
          sLine.Strings[4] := Trim(htmlstripstring(sLine.Strings[4]));
          sLine.Strings[5] := Trim(htmlstripstring(sLine.Strings[5]));
          sLine.Strings[6] := Trim(htmlstripstring(sLine.Strings[6]));
          if (Pos(cstr, sLine.Strings[3]) = 1) and (Pos(cstr, sLine.Strings[4]) = 1) and
             (Pos(cstr, sLine.Strings[5]) = 1) and (Pos(cstr, sLine.Strings[6]) = 1) then
          begin
            if Pos('NEW', UpperCase(sLine.Strings[0])) > 0 then
            begin
              if group = 1 then
                idx := 1
              else
                idx := 13;
            end
            else if Pos('USED', UpperCase(sLine.Strings[0])) > 0 then
            begin
              if group = 1 then
                idx := 7
              else
                idx := 19;
            end
            else
            begin
              idx := 0;
              Continue;
            end;
            result_t[idx] := atoi(sLine.Strings[1]);
            result_t[idx + 1] := atoi(sLine.Strings[2]);
            if useUSDvalues then
            begin
              result_t[idx + 2] := atofdollar(sLine.Strings[3]);
              result_t[idx + 3] := atofdollar(sLine.Strings[4]);
              result_t[idx + 4] := atofdollar(sLine.Strings[5]);
              result_t[idx + 5] := atofdollar(sLine.Strings[6]);
            end
            else
            begin
              result_t[idx + 2] := atofeuro(sLine.Strings[3]);
              result_t[idx + 3] := atofeuro(sLine.Strings[4]);
              result_t[idx + 4] := atofeuro(sLine.Strings[5]);
              result_t[idx + 5] := atofeuro(sLine.Strings[6]);
            end;
            Result := Result or
              ((result_t[idx] <> 0) and (result_t[idx + 1] <> 0) and
               (result_t[idx + 2] <> 0.0) and (result_t[idx + 3] <> 0.0) and
               (result_t[idx + 4] <> 0.0) and (result_t[idx + 5] <> 0.0));
          end;
        end;
      end;
    end;
  finally
    SL.Free;
    sLine.Free;
  end;

  if Result then
  begin
    ret1.nTimesSold := Round(result_t[1]);
    ret1.nTotalQty := Round(result_t[2]);
    ret1.nMinPrice := result_t[3];
    ret1.nAvgPrice := result_t[4];
    ret1.nQtyAvgPrice := result_t[5];
    ret1.nMaxPrice := result_t[6];
    ret1.uTimesSold := Round(result_t[7]);
    ret1.uTotalQty := Round(result_t[8]);
    ret1.uMinPrice := result_t[9];
    ret1.uAvgPrice := result_t[10];
    ret1.uQtyAvgPrice := result_t[11];
    ret1.uMaxPrice := result_t[12];

    ret2.nTotalLots := Round(result_t[12 + 1]);
    ret2.nTotalQty := Round(result_t[12 + 2]);
    ret2.nMinPrice := result_t[12 + 3];
    ret2.nAvgPrice := result_t[12 + 4];
    ret2.nQtyAvgPrice := result_t[12 + 5];
    ret2.nMaxPrice := result_t[12 + 6];
    ret2.uTotalLots := Round(result_t[12 + 7]);
    ret2.uTotalQty := Round(result_t[12 + 8]);
    ret2.uMinPrice := result_t[12 + 9];
    ret2.uAvgPrice := result_t[12 + 10];
    ret2.uQtyAvgPrice := result_t[12 + 11];
    ret2.uMaxPrice := result_t[12 + 12];

    if useUSDvalues then
    begin
      SaveParec3(id, color, ret1, ret2, 'USD', link, useVATValues);

      ret1.nMinPrice := result_t[3] * USDVal;
      ret1.nAvgPrice := result_t[4] * USDVal;
      ret1.nQtyAvgPrice := result_t[5] * USDVal;
      ret1.nMaxPrice := result_t[6] * USDVal;
      ret1.uMinPrice := result_t[9] * USDVal;
      ret1.uAvgPrice := result_t[10] * USDVal;
      ret1.uQtyAvgPrice := result_t[11] * USDVal;
      ret1.uMaxPrice := result_t[12] * USDVal;

      ret2.nMinPrice := result_t[12 + 3] * USDVal;
      ret2.nAvgPrice := result_t[12 + 4] * USDVal;
      ret2.nQtyAvgPrice := result_t[12 + 5] * USDVal;
      ret2.nMaxPrice := result_t[12 + 6] * USDVal;
      ret2.uMinPrice := result_t[12 + 9] * USDVal;
      ret2.uAvgPrice := result_t[12 + 10] * USDVal;
      ret2.uQtyAvgPrice := result_t[12 + 11] * USDVal;
      ret2.uMaxPrice := result_t[12 + 12] * USDVal;
    end
    else
      SaveParec3(id, color, ret1, ret2, 'EUR', link, useVATValues);

    clink := link;
    SL := TStringList.Create;
    try
      SL.Text := skeep;
      S_SaveToFile(SL, cachefile + 'l');
    finally
      SL.Free;
    end;

    if savetyp then
      if Length(typ) = 1 then
        db.SetPartType(pci, typ[1]);
  end;
end;

var
  do_not_treat_as_gear: TStringList;

function NET_GetPriceGuideForElement(const pci: TPieceColorInfo; id: string; const color: string;
  var ret1: priceguide_t; var ret2: availability_t; const cachefile: string;
  var clink: string): boolean;
var
  link: string;
  cc: integer;
  htm, s1: string;
  i, p: integer;
  lvl: Integer;
  instr: boolean;
  strsign: char;
  c: char;
  sc: TScriptEngine;
  result_t: array[1..24] of Double;
  token: string;
  idx: integer;
  skeep: string;
  loadedfromcache: boolean;
  isdollar: Boolean;
  USDval: Double;
  didbook: boolean;
  didgear: boolean;
  savelink: boolean;
  fixlink: boolean;
  slist: TStringList;
  doabort: boolean;
  tryparttype: char;
  forcelink: string;
  sRETRY: string;
  typ: string;
begin
  Result := False;
  cc := -1000;
  DecimalSeparator := '.';
  if color = '-1' then
  begin
    if db.IsMoc(id) then
    begin
      {$IFDEF CRAWLER}
      FAILwarning := True;
      {$ENDIF}
      Exit;
    end;
  end;
  if db.pieceinfo(pci).category = CATEGORYCUSTOMMINIFIGS then
  begin
    {$IFDEF CRAWLER}
    FAILwarning := True;
    {$ENDIF}
    Exit;
  end;

  tryparttype := ' ';
  didbook := False;
  didgear := False;
  isdollar := False;
  savelink := False;
{$IFDEF CRAWLER}
  USDwarning := False;
  FAILwarning := False;
{$ENDIF}
  FillChar(ret1, SizeOf(priceguide_t), 0);
  FillChar(ret2, SizeOf(availability_t), 0);
  link := db.CrawlerLink(id, atoi(color));
  if (link = '') or (Pos('catalogPriceGuide.asp', link) > 0) then
  begin
    if NET_GetPriceGuideForElement2(pci, id, color, ret1, ret2, cachefile, clink) then
    begin
      Result := True;
      inc(type2_pg_hits);
      last_pg_hit := 2;
      if Pos('catalogPriceGuide.asp', link) > 0 then
        db.AddCrawlerLink(id, atoi(color), '');
      Exit;
    end;
    if (color = '9996') or (color = '9997') or (color = '9998') then
    begin
    {$IFDEF CRAWLER}
      FAILwarning := True;
    {$ENDIF}
      Exit;
    end;
  end
  else if Pos(UpperCase(BL_NET) + '/PRICEGUIDE', UpperCase(link)) > 0 then
  begin
    forcelink := link;
    if NET_GetPriceGuideForElement2(pci, id, color, ret1, ret2, cachefile, clink, forcelink) then
    begin
      Result := True;
      inc(type2_pg_hits);
      last_pg_hit := 2;
      Exit;
    end;
    if (color = '9996') or (color = '9997') or (color = '9998') then
    begin
    {$IFDEF CRAWLER}
      FAILwarning := True;
    {$ENDIF}
      Exit;
    end;
  end
  else if Pos(UpperCase(BL_NET) + '/' + 'CATALOGPG.ASP' + '?G=', UpperCase(link)) > 0 then
  begin
    forcelink := NET_MakePriceGuideLink2(
                  'G',
                  HTMLLinkParseParam(link, 'G', id),
                  atoi(HTMLLinkParseParam(link, 'ColorID', '0')),
                  atoi(HTMLLinkParseParam(link, 'prDec', '4'))
                );
    if NET_GetPriceGuideForElement2(pci, id, color, ret1, ret2, cachefile, clink, forcelink) then
    begin
      Result := True;
      inc(type2_pg_hits);
      last_pg_hit := 2;
      db.AddCrawlerLink(id, atoi(color), forcelink);
      if pci.sparttype <> 'G' then
        db.SetPartType(pci, 'G');
      Exit;
    end;
    if (color = '9996') or (color = '9997') or (color = '9998') then
    begin
    {$IFDEF CRAWLER}
      FAILwarning := True;
    {$ENDIF}
      Exit;
    end;
  end
  else if Pos(UpperCase(BL_NET) + '/' + 'CATALOGPG.ASP' + '?S=', UpperCase(link)) > 0 then
  begin
    forcelink := NET_MakePriceGuideLink2(
                  'S',
                  HTMLLinkParseParam(link, 'S', id),
                  atoi(HTMLLinkParseParam(link, 'ColorID', '0')),
                  atoi(HTMLLinkParseParam(link, 'prDec', '4'))
                );
    if NET_GetPriceGuideForElement2(pci, id, color, ret1, ret2, cachefile, clink, forcelink) then
    begin
      Result := True;
      inc(type2_pg_hits);
      last_pg_hit := 2;
      db.AddCrawlerLink(id, atoi(color), forcelink);
      if pci.sparttype <> 'S' then
        db.SetPartType(pci, 'S');
      Exit;
    end;
    if (color = '9996') or (color = '9997') or (color = '9998') then
    begin
    {$IFDEF CRAWLER}
      FAILwarning := True;
    {$ENDIF}
      Exit;
    end;
  end
  else if Pos(UpperCase(BL_NET) + '/' + 'CATALOGPG.ASP' + '?I=', UpperCase(link)) > 0 then
  begin
    forcelink := NET_MakePriceGuideLink2(
                  'I',
                  HTMLLinkParseParam(link, 'I', id),
                  atoi(HTMLLinkParseParam(link, 'ColorID', '0')),
                  atoi(HTMLLinkParseParam(link, 'prDec', '4'))
                );
    if NET_GetPriceGuideForElement2(pci, id, color, ret1, ret2, cachefile, clink, forcelink) then
    begin
      Result := True;
      inc(type2_pg_hits);
      last_pg_hit := 2;
      db.AddCrawlerLink(id, atoi(color), forcelink);
      if pci.sparttype <> 'I' then
        db.SetPartType(pci, 'I');
      Exit;
    end;
    if (color = '9996') or (color = '9997') or (color = '9998') then
    begin
    {$IFDEF CRAWLER}
      FAILwarning := True;
    {$ENDIF}
      Exit;
    end;
  end
  else if Pos(UpperCase(BL_NET) + '/' + 'CATALOGPG.ASP' + '?P=', UpperCase(link)) > 0 then
  begin
    forcelink := NET_MakePriceGuideLink2(
                  'P',
                  HTMLLinkParseParam(link, 'P', id),
                  atoi(HTMLLinkParseParam(link, 'ColorID', '0')),
                  atoi(HTMLLinkParseParam(link, 'prDec', '4'))
                );
    if NET_GetPriceGuideForElement2(pci, id, color, ret1, ret2, cachefile, clink, forcelink) then
    begin
      Result := True;
      inc(type2_pg_hits);
      last_pg_hit := 2;
      db.AddCrawlerLink(id, atoi(color), forcelink);
      if pci.sparttype <> 'P' then
        db.SetPartType(pci, 'P');
      Exit;
    end;
    if (color = '9996') or (color = '9997') or (color = '9998') then
    begin
    {$IFDEF CRAWLER}
      FAILwarning := True;
    {$ENDIF}
      Exit;
    end;
  end
  else if Pos(UpperCase(BL_NET) + '/' + 'CATALOGPG.ASP' + '?C=', UpperCase(link)) > 0 then
  begin
    forcelink := NET_MakePriceGuideLink2(
                  'C',
                  HTMLLinkParseParam(link, 'C', id),
                  atoi(HTMLLinkParseParam(link, 'ColorID', '0')),
                  atoi(HTMLLinkParseParam(link, 'prDec', '4'))
                );
    if NET_GetPriceGuideForElement2(pci, id, color, ret1, ret2, cachefile, clink, forcelink) then
    begin
      Result := True;
      inc(type2_pg_hits);
      last_pg_hit := 2;
      db.AddCrawlerLink(id, atoi(color), forcelink);
      if pci.sparttype <> 'C' then
        db.SetPartType(pci, 'C');
      Exit;
    end;
    if (color = '9996') or (color = '9997') or (color = '9998') then
    begin
    {$IFDEF CRAWLER}
      FAILwarning := True;
    {$ENDIF}
      Exit;
    end;
  end
  else if Pos(UpperCase(BL_NET) + '/' + 'CATALOGPG.ASP' + '?B=', UpperCase(link)) > 0 then
  begin
    forcelink := NET_MakePriceGuideLink2(
                  'B',
                  HTMLLinkParseParam(link, 'B', id),
                  atoi(HTMLLinkParseParam(link, 'ColorID', '0')),
                  atoi(HTMLLinkParseParam(link, 'prDec', '4'))
                );
    if NET_GetPriceGuideForElement2(pci, id, color, ret1, ret2, cachefile, clink, forcelink) then
    begin
      Result := True;
      inc(type2_pg_hits);
      last_pg_hit := 2;
      db.AddCrawlerLink(id, atoi(color), forcelink);
      if pci.sparttype <> 'B' then
        db.SetPartType(pci, 'B');
      Exit;
    end;
    if (color = '9996') or (color = '9997') or (color = '9998') then
    begin
    {$IFDEF CRAWLER}
      FAILwarning := True;
    {$ENDIF}
      Exit;
    end;
  end
  else if Pos(UpperCase(BL_NET) + '/' + 'CATALOGPG.ASP' + '?O=', UpperCase(link)) > 0 then
  begin
    forcelink := NET_MakePriceGuideLink2(
                  'O',
                  HTMLLinkParseParam(link, 'O', id),
                  atoi(HTMLLinkParseParam(link, 'ColorID', '0')),
                  atoi(HTMLLinkParseParam(link, 'prDec', '4'))
                );
    if NET_GetPriceGuideForElement2(pci, id, color, ret1, ret2, cachefile, clink, forcelink) then
    begin
      Result := True;
      inc(type2_pg_hits);
      last_pg_hit := 2;
      db.AddCrawlerLink(id, atoi(color), forcelink);
      if pci.sparttype <> 'O' then
        db.SetPartType(pci, 'O');
      Exit;
    end;
    if (color = '9996') or (color = '9997') or (color = '9998') then
    begin
    {$IFDEF CRAWLER}
      FAILwarning := True;
    {$ENDIF}
      Exit;
    end;
  end
  else if Pos(UpperCase(BL_NET) + '/' + 'CATALOGPG.ASP' + '?M=', UpperCase(link)) > 0 then
  begin
    forcelink := NET_MakePriceGuideLink2(
                  'M',
                  HTMLLinkParseParam(link, 'M', id),
                  atoi(HTMLLinkParseParam(link, 'ColorID', '0')),
                  atoi(HTMLLinkParseParam(link, 'prDec', '4'))
                );
    if NET_GetPriceGuideForElement2(pci, id, color, ret1, ret2, cachefile, clink, forcelink) then
    begin
      Result := True;
      inc(type2_pg_hits);
      last_pg_hit := 2;
      db.AddCrawlerLink(id, atoi(color), forcelink);
      if pci.sparttype <> 'M' then
        db.SetPartType(pci, 'M');
      Exit;
    end;
    if (color = '9996') or (color = '9997') or (color = '9998') then
    begin
    {$IFDEF CRAWLER}
      FAILwarning := True;
    {$ENDIF}
      Exit;
    end;
  end;

  if not allowtype1_hits then
  begin
    typ := pci.ItemType;
    sRETRY := '';
    if (typ = 'P') and (color = '9999') then
      sRETRY := 'MGBS'
    else if (typ = 'M') and ((color = '-1') or (color = '9999')) then
      sRETRY := 'GBSP'
    else if (typ = 'S') and ((color = '-1') or (color = '9999')) then
      sRETRY := 'GBMP'
    else if (typ = 'G') and ((color = '-1') or (color = '9999')) then
      sRETRY := 'BSMP'
    else if (typ = 'B') and ((color = '-1') or (color = '9999')) then
      sRETRY := 'SGMP';
    for i := 1 to Length(sRETRY) do
    begin
      forcelink := NET_MakePriceGuideLink2(sRETRY[i], db.GetBLNetPieceName(id), 0, 6);
      if NET_GetPriceGuideForElement2(pci, id, color, ret1, ret2, cachefile, clink, forcelink) then
      begin
        Result := True;
        inc(type2_pg_hits);
        last_pg_hit := 2;
        db.AddCrawlerLink(id, atoi(color), forcelink);
        db.SetPartType(pci, sRETRY[i]);
        Exit;
      end;
    end;

    // Retry part as gear
    if (typ = 'P') and (color <> '-1') and (color <> '9999') and (pci.sparttype <> 'P') then
    begin
      if do_not_treat_as_gear.IndexOf(pci.piece) < 0 then
      begin
        cc := StrToIntDef(color, -2);
        if (cc > -1) and (cc <= LASTNORMALCOLORINDEX) then
        begin
          forcelink := NET_MakePriceGuideLink2('G', db.GetBLNetPieceName(id), db.colors(cc).BrickLingColor, 6);
          if NET_GetPriceGuideForElement2(pci, id, color, ret1, ret2, cachefile, clink, forcelink) then
          begin
            Result := True;
            inc(type2_pg_hits);
            last_pg_hit := 2;
            db.AddCrawlerLink(id, atoi(color), forcelink);
            db.SetPartType(pci, 'G');
            Exit;
          end;
        end;
      end;
    end;

  {$IFDEF CRAWLER}
    FAILwarning := True;
  {$ENDIF}
    Exit;
  end;

  if link <> '' then
  begin
    if Pos('asp?G=', link) > 0 then
      tryparttype := 'G'
    else if Pos('asp?P=', link) > 0 then
      tryparttype := 'P'
    else if Pos('asp?S=', link) > 0 then
      tryparttype := 'S'
    else if Pos('asp?M=', link) > 0 then
      tryparttype := 'M'
    else if Pos('asp?O=', link) > 0 then
      tryparttype := 'O'
    else if Pos('asp?I=', link) > 0 then
      tryparttype := 'I'
    else if Pos('asp?C=', link) > 0 then
      tryparttype := 'C'
    else if Pos('asp?B=', link) > 0 then
      tryparttype := 'B';
  end;

  inc(type1_pg_hits);
  last_pg_hit := 1;

  if Pos('catalogPriceGuide.asp', link) > 0 then
  begin
    link := '';
    fixlink := True;
  end
  else
    fixlink := False;
  if link <> '' then
    if Pos('&PRDEC=', UpperCase(link)) <= 0 then
    begin
      link := link + '&prdec=4';
      fixlink := True;
    end;
  if link = '' then
  begin
    id := db.BrickLinkPart(id);
    if (color = '89') or (color = '') or ((color = '-1') and (Pos('-', id) > 0))  then // set
    begin
      link := 'http://' + BL_NET + '/' + 'catalogPG.asp' + '?S=' + db.GetBLNetPieceName(id) + '--&colorID=0&v=D&viewExclude=Y&cID=Y&prDec=4';
      tryparttype := 'S';
    end
    else if color = '-1' then // minifigure - sticker
    begin
      if issticker(id, pci) then // sticker
      begin
        link := 'https://' + BL_NET + '/' + 'catalogPG.asp' + '?P=' + db.GetBLNetPieceName(id) + '&colorID=0&prDec=4';
        tryparttype := 'P';
      end
      else if db.isbook(id) then  // book
      begin
        link := 'https://' + BL_NET + '/' + 'catalogPG.asp' + '?B=' + db.GetBLNetPieceName(id) + '&prdec=4';
        didbook := True;
        tryparttype := 'B';
      end
      else if db.IsGear(id) then
      begin
        link := 'https://' + BL_NET + '/' + 'catalogPG.asp' + '?G=' + db.GetBLNetPieceName(id) + '&prDec=4';
        didgear := True;
        tryparttype := 'G';
      end
      else  // minifigure
      begin
        link := 'https://' + BL_NET + '/' + 'catalogPG.asp' + '?M=' + db.GetBLNetPieceName(id) + '&prDec=4';
        tryparttype := 'M';
      end
    end
    else if atoi(color) = CATALOGCOLORINDEX then // Catalogs
    begin
      link := 'https://' + BL_NET + '/' + 'catalogPG.asp' + '?C=' + db.GetBLNetPieceName(id) + '&ColorID=0&prDec=4'
    end
    else if atoi(color) = INSTRUCTIONCOLORINDEX then // INstructions
    begin
      link := 'https://' + BL_NET + '/' + 'catalogPG.asp' + '?I=' + db.GetBLNetPieceName(id) + '&ColorID=0&prDec=4'
    end
    else if atoi(color) = BOXCOLORINDEX then // Original box
    begin
      link := 'https://' + BL_NET + '/' + 'catalogPG.asp' + '?O=' + db.GetBLNetPieceName(id) + '&ColorID=0&prDec=4'
    end
    else // part
    begin
      cc := StrToIntDef(color, -2);
      if (cc < -1) or (cc > LASTNORMALCOLORINDEX) then
        link := 'https://' + BL_NET + '/' + 'catalogPG.asp' + '?P=' + db.GetBLNetPieceName(id) + '&colorID=0&prDec=4'
      else
        link := 'https://' + BL_NET + '/' + 'catalogPG.asp' + '?P=' + db.GetBLNetPieceName(id) + '&colorID=' + IntToStr(db.colors(cc).BrickLingColor) + '&prDec=4';
      tryparttype := 'P';
    end;
  end;
  loadedfromcache := False;
  htm := GetURLString(link);
  if htm <> '' then
    skeep := htm
  else
  begin
    skeep := '';
    if FileExists(cachefile) then
    begin
      with TStringList.Create do
      try
        LoadFromFile(cachefile);
        loadedfromcache := True;
        skeep := Text;
        htm := skeep;
      finally
        Free;
      end;
    end;
  end;
  if htm = '' then
    Exit;

  p := Pos('Last 6 Months Sales', htm);
  if p <= 0 then
  begin
    if (Pos('3068bpb', id) = 1) and (length(id) = 10) then
    begin
      id := '3068bpb0' + id[8] + id[9] + id[10];
      cc := StrToIntDef(color, -2);
      if (cc < -1) or (cc > LASTNORMALCOLORINDEX) then
        link := 'https://' + BL_NET + '/' + 'catalogPG.asp' + '?P=' + db.GetBLNetPieceName(id) + '&colorID=0&prDec=4'
      else
        link := 'https://' + BL_NET + '/' + 'catalogPG.asp' + '?P=' + db.GetBLNetPieceName(id) + '&colorID=' + IntToStr(db.colors(cc).BrickLingColor) + '&prDec=4';
      tryparttype := 'P';
      savelink := True;
      loadedfromcache := False;
      htm := GetURLString(link);
      if htm <> '' then
        skeep := htm
    end
    else if color = '9999' then
    begin
      link := 'https://' + BL_NET + '/' + 'catalogPG.asp' + '?M=' + db.GetBLNetPieceName(id) + '&prDec=4';
      tryparttype := 'M';
      savelink := True;
      loadedfromcache := False;
      htm := GetURLString(link);
      if htm <> '' then
        skeep := htm
    end
    else
    begin
      cc := StrToIntDef(color, -2);
      if (cc < -1) or (cc > LASTNORMALCOLORINDEX) then
        link := 'https://' + BL_NET + '/' + 'catalogPG.asp' + '?G=' + db.GetBLNetPieceName(id) + '&colorID=0&prDec=4'
      else
        link := 'https://' + BL_NET + '/' + 'catalogPG.asp' + '?G=' + db.GetBLNetPieceName(id) + '&colorID=' + IntToStr(db.colors(cc).BrickLingColor) + '&prDec=4';
      tryparttype := 'G';
      savelink := True;
      loadedfromcache := False;
      htm := GetURLString(link);
      if htm <> '' then
        skeep := htm
    end;
  end;

  p := Pos('Last 6 Months Sales', htm);
  if p <= 0 then
  begin
    cc := StrToIntDef(color, -2);
    if (cc < -1) or (cc > MAXINFOCOLOR) then
    begin
      link := 'https://' + BL_NET + '/catalogPriceGuide.asp?S=' + db.GetBLNetPieceName(id) + '&prDec=4';
      tryparttype := 'S';
    end
    else
    begin
      if cc > LASTNORMALCOLORINDEX then
        link := 'https://' + BL_NET + '/catalogPriceGuide.asp?P=' + db.GetBLNetPieceName(id) + '&colorid=0&prDec=4'
      else
        link := 'https://' + BL_NET + '/catalogPriceGuide.asp?P=' + db.GetBLNetPieceName(id) + '&colorid=' + IntToStr(db.colors(cc).BrickLingColor) + '&prDec=4';
      tryparttype := 'P';
    end;
    savelink := False;
    loadedfromcache := False;
    htm := GetURLString(link);
    if htm <> '' then
      skeep := htm;
    isdollar := True;
    p := Pos('Last 6 Months Sales', htm);
  end;

  if p <= 0 then
  begin
    if color = '9999' then
    begin
      isdollar := False;
      link := 'https://' + BL_NET + '/' + 'catalogPG.asp' + '?G=' + db.GetBLNetPieceName(id) + '&prDec=4';
      tryparttype := 'G';
      savelink := True;
      loadedfromcache := False;
      htm := GetURLString(link);
      if htm <> '' then
        skeep := htm;
      p := Pos('Last 6 Months Sales', htm);
    end;
  end;

  if p <= 0 then
  begin
    if (color = '-1') or (color = '9999') then
      if not didbook then
      begin
        isdollar := False;
        link := 'https://' + BL_NET + '/' + 'catalogPG.asp' + '?B=' + db.GetBLNetPieceName(id) + '&prDec=4';
        tryparttype := 'B';
        savelink := True;
        loadedfromcache := False;
        htm := GetURLString(link);
        if htm <> '' then
          skeep := htm;
        p := Pos('Last 6 Months Sales', htm);
      end;
  end;

  if p <= 0 then
  begin
  {$IFNDEF CRAWLER}
    I_Warning('NET_GetPriceGuideForElement(): Can not retrieve bricklink info for part = ' + id + ', color = ' + itoa(cc) + #13#10);
  {$ELSE}
    FAILwarning := True;
  {$ENDIF}
    Exit;
  end;

  if savelink or fixlink then
    db.AddCrawlerLink(id, atoi(color), link);

  htm := Copy(htm, p, Length(htm) - p + 1);

  lvl := 0;
  instr := False;
  s1 := '';
  strsign := ' ';
  for i := 1 to Length(htm) do
  begin
    c := htm[i];
    if c = '<' then
    begin
      if not instr then
        inc(lvl);
    end
    else if c = '>' then
    begin
      if not instr then
        Dec(lvl);
    end
    else if (c in ['''','"']) and (lvl = 0) then
    begin
      if instr then
      begin
        if strsign = c then
          instr := False
      end
      else
      begin
        strsign := c;
        instr := True;
      end;
    end;
    if not instr and (lvl = 0) then
      if not (c in ['>', '''', '"']) then
        s1 := s1 + c
      else
        s1 := s1 + ' ';
  end;

  s1 := StringReplace(s1, '&nbsp;', ' ', [rfReplaceAll, rfIgnoreCase]);
  s1 := StringReplace(s1, ':', ' ', [rfReplaceAll, rfIgnoreCase]);
  s1 := StringReplace(s1, '~EUR' , '',  [rfReplaceAll, rfIgnoreCase]);
  s1 := StringReplace(s1, 'EUR' , '',  [rfReplaceAll, rfIgnoreCase]);
  s1 := StringReplace(s1, 'Times Sold' , 'TimesSold',  [rfReplaceAll, rfIgnoreCase]);
  s1 := StringReplace(s1, 'Total Lots' , 'TotalLots',  [rfReplaceAll, rfIgnoreCase]);
  s1 := StringReplace(s1, 'Total Qty' , 'TotalQty',  [rfReplaceAll, rfIgnoreCase]);
  s1 := StringReplace(s1, 'Min Price' , 'MinPrice',  [rfReplaceAll, rfIgnoreCase]);
  s1 := StringReplace(s1, 'Qty Avg Price' , 'QtyAvgPrice',  [rfReplaceAll, rfIgnoreCase]);
  s1 := StringReplace(s1, 'Avg Price' , 'AvgPrice',  [rfReplaceAll, rfIgnoreCase]);
  s1 := StringReplace(s1, 'Max Price' , 'MaxPrice',  [rfReplaceAll, rfIgnoreCase]);

  if isdollar then
    s1 := StringReplace(s1, '$' , '',  [rfReplaceAll, rfIgnoreCase]);

  FillChar(result_t, SizeOf(result_t), 0);

  idx := 1;

  sc := TScriptEngine.Create(s1);
  while sc.GetString do
  begin
    token := UpperCase(sc._String);
    if token = 'UNAVAILABLE' then
      idx := idx + 6
    else if (token = 'TIMESSOLD') or (token = 'TOTALQTY') or (token = 'TOTALLOTS') then
    begin
      sc.MustGetInteger;
      result_t[idx] := sc._Integer;
      Inc(idx);
    end
    else if (token = 'MINPRICE') or (token = 'MAXPRICE') or (token = 'QTYAVGPRICE') or (token = 'AVGPRICE') then
    begin
      sc.MustGetFloat;
      result_t[idx] := sc._Float;
      Inc(idx);
    end;
    if idx > 24 then
      Break;
  end;
  sc.Free;

  if idx <= 24 then
    Exit;

  if tryparttype <> ' ' then
    db.SetPartType(id, atoi(color), tryparttype);

  if isdollar then
  begin
    ret1.nTimesSold := Round(result_t[1]);
    ret1.nTotalQty := Round(result_t[2]);
    ret1.nMinPrice := result_t[3];
    ret1.nAvgPrice := result_t[4];
    ret1.nQtyAvgPrice := result_t[5];
    ret1.nMaxPrice := result_t[6];
    ret1.uTimesSold := Round(result_t[7]);
    ret1.uTotalQty := Round(result_t[8]);
    ret1.uMinPrice := result_t[9];
    ret1.uAvgPrice := result_t[10];
    ret1.uQtyAvgPrice := result_t[11];
    ret1.uMaxPrice := result_t[12];

    ret2.nTotalLots := Round(result_t[12 + 1]);
    ret2.nTotalQty := Round(result_t[12 + 2]);
    ret2.nMinPrice := result_t[12 + 3];
    ret2.nAvgPrice := result_t[12 + 4];
    ret2.nQtyAvgPrice := result_t[12 + 5];
    ret2.nMaxPrice := result_t[12 + 6];
    ret2.uTotalLots := Round(result_t[12 + 7]);
    ret2.uTotalQty := Round(result_t[12 + 8]);
    ret2.uMinPrice := result_t[12 + 9];
    ret2.uAvgPrice := result_t[12 + 10];
    ret2.uQtyAvgPrice := result_t[12 + 11];
    ret2.uMaxPrice := result_t[12 + 12];

    SaveParec3(id, color, ret1, ret2, 'USD', link, False);

    USDval := db.ConvertCurrencyAt('USD', Now);
{$IFDEF CRAWLER}
    USDwarning := True;
{$ENDIF}
    result_t[3] := result_t[3] * USDval;
    result_t[4] := result_t[4] * USDval;
    result_t[5] := result_t[5] * USDval;
    result_t[6] := result_t[6] * USDval;
    result_t[9] := result_t[9] * USDval;
    result_t[10] := result_t[10] * USDval;
    result_t[11] := result_t[11] * USDval;
    result_t[12] := result_t[12] * USDval;

    result_t[12 + 3] := result_t[12 + 3] * USDval;
    result_t[12 + 4] := result_t[12 + 4] * USDval;
    result_t[12 + 5] := result_t[12 + 5] * USDval;
    result_t[12 + 6] := result_t[12 + 6] * USDval;
    result_t[12 + 9] := result_t[12 + 9] * USDval;
    result_t[12 + 10] := result_t[12 + 10] * USDval;
    result_t[12 + 11] := result_t[12 + 11] * USDval;
    result_t[12 + 12] := result_t[12 + 12] * USDval;
  end;

  ret1.nTimesSold := Round(result_t[1]);
  ret1.nTotalQty := Round(result_t[2]);
  ret1.nMinPrice := result_t[3];
  ret1.nAvgPrice := result_t[4];
  ret1.nQtyAvgPrice := result_t[5];
  ret1.nMaxPrice := result_t[6];
  ret1.uTimesSold := Round(result_t[7]);
  ret1.uTotalQty := Round(result_t[8]);
  ret1.uMinPrice := result_t[9];
  ret1.uAvgPrice := result_t[10];
  ret1.uQtyAvgPrice := result_t[11];
  ret1.uMaxPrice := result_t[12];

  ret2.nTotalLots := Round(result_t[12 + 1]);
  ret2.nTotalQty := Round(result_t[12 + 2]);
  ret2.nMinPrice := result_t[12 + 3];
  ret2.nAvgPrice := result_t[12 + 4];
  ret2.nQtyAvgPrice := result_t[12 + 5];
  ret2.nMaxPrice := result_t[12 + 6];
  ret2.uTotalLots := Round(result_t[12 + 7]);
  ret2.uTotalQty := Round(result_t[12 + 8]);
  ret2.uMinPrice := result_t[12 + 9];
  ret2.uAvgPrice := result_t[12 + 10];
  ret2.uQtyAvgPrice := result_t[12 + 11];
  ret2.uMaxPrice := result_t[12 + 12];

  if not isdollar then
    SaveParec3(id, color, ret1, ret2, 'EUR', link, True);

  if not loadedfromcache then
  begin
    slist := TStringList.Create;
    try
      slist.Text := skeep;
      S_SaveToFile(slist, cachefile);
    finally
      slist.Free;
    end;
  end;

  clink := link;

  Result := True;
end;

function NET_GetBricklinkAlias(const id: string; const typ: char = 'P'): string;
var
  s: string;
  s1: string;
  p: integer;
  i: integer;
  id1: string;
begin
  Result := '';

  id1 := strtrim(id);

  if Length(id1) = 12 then
    if Pos1('dupupn', id1) then
      if IsNumericC(id1[7]) and IsNumericC(id1[8]) and
         (id1[9] = 'p') and (id1[10] = 'r') and
         IsNumericC(id1[11]) and IsNumericC(id1[12]) then
          id1 := 'dupupn00' + id1[7] + id1[8] + 'pr00' + id1[11] + id1[12];

  if Length(id1) = 15 then
    if Pos1('dupupn', id1) then
      if IsNumericC(id1[7]) and IsNumericC(id1[8]) and
         (id1[9] = 'c') and IsNumericC(id1[10]) and IsNumericC(id1[11]) and
         (id1[12] = 'p') and (id1[13] = 'r') and
         IsNumericC(id1[14]) and IsNumericC(id1[15]) then
          id1 := 'dupupn00' + id1[7] + id1[8] + 'c' + id1[10] + id1[11] + 'pr00' + id1[14] + id1[15];

  if UpperCase(id1) = '44302A' then
    Exit;
  if UpperCase(id1) = '44302' then
    Exit;
  s := GetURLString('https://rebrickable.com/parts/' + id1);
  SaveStringToFile(basedefault + 'db\rmolds\rebrickable_' + id1 + '.htm', s);
  s1 := '' + BL_NET + '/v2/search.page?q=';
  p := Pos(s1, s);
  if p <= 0 then
  begin
    s1 := 'href="https://www.bricklink.com/v2/catalog/catalogitem.page?' + typ + '=';
    p := Pos(s1, s);
  end;
  if p <= 0 then
  begin
    s := GetURLString('https://rebrickable.com/sets/' + id1);
    s1 := '' + BL_NET + '/v2/search.page?q=';
    p := Pos(s1, s);
  end;
  if p > 0 then
  begin
    SaveStringToFile(basedefault + 'db\rmolds\' + id1 + '.htm', s);
    p := p + Length(s1);
    for i := p to length(s) do
    begin
      if (s[i] = '''') or (s[i] = '"') or (s[i] = '&') or (s[i] = '<') or (s[i] = '>') then
        break
      else
        Result := Result + s[i];
    end;
  end;
end;

function NET_GetItemWeightFromText(const txt: string; var fweight: double): boolean;
var
  s: string;
  s1: string;
  p: integer;
  i: integer;
  sweight: string;
  dotcnt, gcnt: integer;
  tmpweight: double;
begin
  Result := False;
  fweight := 0.0;

  s := txt;
  if s = '' then
    Exit;

  s1 := '<span id="item-weight-info">';
  p := Pos(s1, s);
  if p > 0 then
  begin
    Result := True;
    p := p + Length(s1);
    sweight := '';
    dotcnt := 0;
    gcnt := 0;
    for i := p to length(s) do
    begin
      if not (s[i] in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '.', 'g']) then
        break;
      if s[i] = 'g' then
        inc(gcnt);
      if gcnt > 1 then
        break;
      if s[i] = '.' then
        inc(dotcnt);
      if dotcnt > 1 then
        break;
      sweight := sweight + s[i];
    end;
    sweight := Trim(sweight);
    if sweight <> '' then
      if dotcnt <= 1 then
        if gcnt = 1 then
          if sweight[length(sweight)] = 'g' then
          begin
            SetLength(sweight, length(sweight) - 1);
            sweight := Trim(sweight);
            if sweight <> '' then
            begin
              if sweight[1] = '.' then
                sweight := '0' + sweight;
              if sweight[length(sweight)] = '.' then
                sweight := sweight + '0';
              tmpweight := atof(sweight, -1.0);
              if tmpweight > 0.0 then
                fweight := tmpweight;
            end;
          end;
  end;
end;

function NET_GetItemWeightFromDisk(const id: string; var fweight: double): boolean;
var
  s: string;
  s1: string;
  p: integer;
  i: integer;
  sweight: string;
  dotcnt, gcnt: integer;
  tmpweight: double;
  SL: TStringList;
  fname: string;
begin
  Result := False;
  fweight := 0.0;

  fname := basedefault + 'db\molds\' + id + '.htm';
  if not fexists(fname) then
  begin
    fname := basedefault + 'db\gears\' + id + '.htm';
    if not fexists(fname) then
    begin
      fname := basedefault + 'db\setmolds\' + id + '.htm';
      if not fexists(fname) then
      begin
        fname := basedefault + 'db\catalogs\' + id + '.htm';
        if not fexists(fname) then
        begin
          fname := basedefault + 'db\books\' + id + '.htm';
          if not fexists(fname) then
            Exit;
        end;
      end;
    end;
  end;

  SL := TStringList.Create;
  S_LoadFromFile(SL, fname);
  s := SL.Text;
  SL.Free;

  if s = '' then
    Exit;

  s1 := '<span id="item-weight-info">';
  p := Pos(s1, s);
  if p > 0 then
  begin
    Result := True;
    p := p + Length(s1);
    sweight := '';
    dotcnt := 0;
    gcnt := 0;
    for i := p to length(s) do
    begin
      if not (s[i] in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '.', 'g']) then
        break;
      if s[i] = 'g' then
        inc(gcnt);
      if gcnt > 1 then
        break;
      if s[i] = '.' then
        inc(dotcnt);
      if dotcnt > 1 then
        break;
      sweight := sweight + s[i];
    end;
    sweight := Trim(sweight);
    if sweight <> '' then
      if dotcnt <= 1 then
        if gcnt = 1 then
          if sweight[length(sweight)] = 'g' then
          begin
            SetLength(sweight, length(sweight) - 1);
            sweight := Trim(sweight);
            if sweight <> '' then
            begin
              if sweight[1] = '.' then
                sweight := '0' + sweight;
              if sweight[length(sweight)] = '.' then
                sweight := sweight + '0';
              tmpweight := atof(sweight, -1.0);
              if tmpweight > 0.0 then
                fweight := tmpweight;
            end;
          end;
  end;
end;

function NET_GetBricklinkCategory(const id: string; var fcat: integer;
  var fweight: double; var parttype: char): boolean;
var
  s: string;
  s1: string;
  p: integer;
  i: integer;
  scat: string;
  didset: boolean;
  tmpcat: integer;
  sweight: string;
  dotcnt, gcnt: integer;
  tmpweight: double;
  SL: TStringList;
  ptype: char;
begin
  Result := False;
  parttype := #0;
  if UpperCase(id) = '44302A' then
    Exit;
  if UpperCase(id) = '44302' then
    Exit;
  if db.IsBook(id) then
  begin
    s := GetURLString('https://' + BL_NET + '/v2/catalog/catalogitem.page?B=' + db.GetBLNetPieceName(strtrim(id)));
    ptype := 'B';
    didset := True;
  end
  else if CharPos('-', id) > 0 then
  begin
    s := GetURLString('https://' + BL_NET + '/v2/catalog/catalogitem.page?S=' + db.GetBLNetPieceName(strtrim(id)));
    ptype := 'S';
    didset := True;
  end
  else
  begin
    s := GetURLString('https://' + BL_NET + '/v2/catalog/catalogitem.page?P=' + db.GetBLNetPieceName(strtrim(id)));
    ptype := 'P';
    didset := False;
  end;
  s1 := 'catString=';
  p := Pos(s1, s);
  if p <= 0 then
  begin
    if not didset then
    begin
      s := GetURLString('https://' + BL_NET + '/v2/catalog/catalogitem.page?S=' + db.GetBLNetPieceName(strtrim(id)));
      ptype := 'S';
      p := Pos(s1, s);
    end;
    if p <= 0 then
    begin
      s := GetURLString('https://' + BL_NET + '/v2/catalog/catalogitem.page?M=' + db.GetBLNetPieceName(strtrim(id)));
      ptype := 'M';
      p := Pos(s1, s);
      if p <= 0 then
      begin
        s := GetURLString('https://' + BL_NET + '/v2/catalog/catalogitem.page?G=' + db.GetBLNetPieceName(strtrim(id)));
        ptype := 'G';
        p := Pos(s1, s);
        if p <= 0 then
        begin
          s := GetURLString('https://' + BL_NET + '/v2/catalog/catalogitem.page?B=' + db.GetBLNetPieceName(strtrim(id)));
          ptype := 'B';
          p := Pos(s1, s);
        end;
      end;
    end;
  end;
  SL := TStringList.Create;
  try
    if not DirectoryExists(basedefault + 'db\molds') then
      ForceDirectories(basedefault + 'db\molds');
    SL.Text := s;
    SL.SaveToFile(basedefault + 'db\molds\' + id + '.htm');
  finally
    SL.Free;
  end;
  if p > 0 then
  begin
    Result := True;
    scat := '';
    p := p + Length(s1);
    for i := p to length(s) do
    begin
      if not (s[i] in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']) then
        break
      else
        scat := scat + s[i];
    end;
    tmpcat := StrToIntDef(scat, -1);
    if (tmpcat < 0) or (tmpcat >= MAXCATEGORIES) then
      Result := False
    else
      fcat := tmpcat;
  end;
  s1 := '<span id="item-weight-info">';
  p := Pos(s1, s);
  if p > 0 then
  begin
    Result := True;
    parttype := ptype;
    p := p + Length(s1);
    sweight := '';
    dotcnt := 0;
    gcnt := 0;
    for i := p to length(s) do
    begin
      if not (s[i] in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '.', 'g']) then
        break;
      if s[i] = 'g' then
        inc(gcnt);
      if gcnt > 1 then
        break;
      if s[i] = '.' then
        inc(dotcnt);
      if dotcnt > 1 then
        break;
      sweight := sweight + s[i];
    end;
    sweight := Trim(sweight);
    if sweight <> '' then
      if dotcnt <= 1 then
        if gcnt = 1 then
          if sweight[length(sweight)] = 'g' then
          begin
            SetLength(sweight, length(sweight) - 1);
            sweight := Trim(sweight);
            if sweight <> '' then
            begin
              if sweight[1] = '.' then
                sweight := '0' + sweight;
              if sweight[length(sweight)] = '.' then
                sweight := sweight + '0';
              tmpweight := atof(sweight, -1.0);
              if tmpweight > 0.0 then
                fweight := tmpweight;
            end;
          end;
  end;
end;

function NET_GetBricklinkMinifigCategory(const id: string; var fcat: integer; var fweight: double): boolean;
var
  s: string;
  s1: string;
  p: integer;
  i: integer;
  scat: string;
  tmpcat: integer;
begin
  Result := False;
  s := GetURLString('https://' + BL_NET + '/v2/catalog/catalogitem.page?M=' + db.GetBLNetPieceName(strtrim(id)));
  s1 := 'catString=';
  p := Pos(s1, s);
  if p > 0 then
  begin
    Result := True;
    scat := '';
    p := p + Length(s1);
    for i := p to length(s) do
    begin
      if not (s[i] in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']) then
        break
      else
        scat := scat + s[i];
    end;
    tmpcat := StrToIntDef(scat, -1);
    if (tmpcat < 0) or (tmpcat >= MAXCATEGORIES) then
      Result := False
    else
      fcat := tmpcat;
  end;
end;

function NET_GetBricklinkMinifigWeight(const id: string; var fweight: double): boolean;
var
  s: string;
  s1: string;
  p: integer;
  i: integer;
  sweight: string;
  dotcnt, gcnt: integer;
  tmpweight: double;
begin
  Result := False;
  if UpperCase(id) = '44302A' then
    Exit;
  if UpperCase(id) = '44302' then
    Exit;
  s := GetURLString('https://' + BL_NET + '/v2/catalog/catalogitem.page?M=' + db.GetBLNetPieceName(strtrim(id)));
  s1 := '<span id="item-weight-info">';
  p := Pos(s1, s);
  if p > 0 then
  begin
    Result := True;
    p := p + Length(s1);
    sweight := '';
    dotcnt := 0;
    gcnt := 0;
    for i := p to length(s) do
    begin
      if not (s[i] in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '.', 'g']) then
        break;
      if s[i] = 'g' then
        inc(gcnt);
      if gcnt > 1 then
        break;
      if s[i] = '.' then
        inc(dotcnt);
      if dotcnt > 1 then
        break;
      sweight := sweight + s[i];
    end;
    sweight := Trim(sweight);
    if sweight <> '' then
      if dotcnt <= 1 then
        if gcnt = 1 then
          if sweight[length(sweight)] = 'g' then
          begin
            SetLength(sweight, length(sweight) - 1);
            sweight := Trim(sweight);
            if sweight <> '' then
            begin
              if sweight[1] = '.' then
                sweight := '0' + sweight;
              if sweight[length(sweight)] = '.' then
                sweight := sweight + '0';
              tmpweight := atof(sweight, -1.0);
              if tmpweight > 0.0 then
                fweight := tmpweight;
            end;
          end;
  end;
end;

function NET_ExistsCache(const id: string; const hits: string; out fname: string): boolean;
var
  i: integer;
  path1: string;
begin
  for i := 1 to Length(hits) do
  begin
    case toupper(hits[i]) of
      'P': path1 := 'molds';
      'G': path1 := 'gears';
      'S': path1 := 'setmolds';
      'B': path1 := 'books';
      'M': path1 := 'minifigs';
      'C': path1 := 'catalogs';
    else
      Continue;
    end;
    path1 := basedefault + 'db\' + path1 + '\' + id + '.htm';
    if fexists(path1) then
    begin
      Result := True;
      fname := path1;
      Exit;
    end;
  end;

  Result := False;
  fname := '';
end;

const
  S_DO_NOT_TREAT_AS_GEAR =
    '93096'#13#10 +
    '10050'#13#10 +
    '11098'#13#10 +
    '11126'#13#10 +
    '14682'#13#10 +
    '4006'#13#10 +
    '16598'#13#10 +
    '16599'#13#10 +
    '18018'#13#10 +
    '3865'#13#10 +
    '3808'#13#10 +
    '3811'#13#10 +
    '3813'#13#10 +
    '23983'#13#10 +
    '2745'#13#10 +
    '2921'#13#10 +
    '30170'#13#10 +
    '30407'#13#10 +
    '30807'#13#10 +
    '31007'#13#10 +
    '33322'#13#10 +
    '35494'#13#10 +
    '3835'#13#10 +
    '3836'#13#10 +
    '3837'#13#10 +
    '3838'#13#10 +
    '3839'#13#10 +
    '3840'#13#10 +
    '3841'#13#10 +
    '3844'#13#10 +
    '3846'#13#10 +
    '3847'#13#10 +
    '3848'#13#10 +
    '3849'#13#10 +
    '3852'#13#10 +
    '3853'#13#10 +
    '3854'#13#10 +
    '3855'#13#10 +
    '3856'#13#10 +
    '3857'#13#10 +
    '3861'#13#10 +
    '3901'#13#10 +
    '3959'#13#10 +
    '3960'#13#10 +
    '3961'#13#10 +
    '3962'#13#10 +
    '3965'#13#10 +
    '3966'#13#10 +
    '4023'#13#10 +
    '40389'#13#10 +
    '40507'#13#10 +
    '40607'#13#10 +
    '4066'#13#10 +
    '4080'#13#10 +
    '4509'#13#10 +
    '55306'#13#10 +
    '6912'#13#10 +
    '6936'#13#10 +
    '6943'#13#10 +
    '6992'#13#10 +
    '6993'#13#10 +
    '6994'#13#10 +
    '6995'#13#10 +
    '7930'#13#10 +
    '93557'#13#10 +
    '98279'#13#10 +
    'bb215'#13#10 +
    'bb237'#13#10 +
    'bb256'#13#10 +
    'bb768'#13#10 +
    'clikits098'#13#10 +
    'clikits099'#13#10 +
    'pillow01'#13#10 +
    'pri066'#13#10 +
    'pri068'#13#10 +
    'pri069'#13#10 +
    'tractor'#13#10 +
    '3861b'#13#10;


initialization
  OpenInetConnection;
  do_not_treat_as_gear := TStringList.Create;
  do_not_treat_as_gear.Text := S_DO_NOT_TREAT_AS_GEAR;
  do_not_treat_as_gear.Sorted := True;

finalization
  CloseInetConnection;
  do_not_treat_as_gear.Free;

end.
