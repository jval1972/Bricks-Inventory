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
//    Download Instructions Routines
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_instructions;

interface

uses
  SysUtils, Classes;

function InstructionDir(const aset: string): string;

function GetInstructionPage(const aset: string; const pg: integer; var outfile: string;
  const forcelink: boolean; const host: string): boolean;

function GetInstructionsFromNet(const aset: string; const outlist: TStringList): boolean;

function UpdateInstructionsFromNet(const aset: string; const forcelink: boolean;
  const prefhost: string = ''): boolean;

function GetInstructionsFromDisk(const aset: string; const outlist: TStringList): boolean;

function GetInstructions(const aset: string; const outlist: TStringList): boolean;

function InstructionsExist(const aset: string): boolean;

implementation

uses
  bi_delphi, bi_db, bi_utils, bi_cachefile, slpash;

function InstructionDir(const aset: string): string;
begin
  Result := '9997\' + strtrim(aset) + '\';
end;

var
  failedlinks: TStringList;

function GetInstructionPage(const aset: string; const pg: integer; var outfile: string;
  const forcelink: boolean; const host: string): boolean;
var
  sset, sname1, sname2, sname3, snum: string;
  i, p: integer;
  link, link2: string;
  sl: TStringList;
  fname: string;
begin
  snum := itoa(pg);
  sset := strtrim(aset);
  while Length(snum) < 3 do
    snum := '0' + snum;

  outfile := InstructionDir(sset) + snum + '.jpg';

  if fexists(basedefault + outfile) then
  begin
    if IsLikeJpeg(basedefault + outfile) then
    begin
      Result := True;
      Exit;
    end
    else
      fdelete(basedefault + outfile);
  end;

  sname1 := '';
  for i := 1 to Length(sset) do
    if IsNumericC(sset[i]) then
      sname1 := sname1 + sset[i]
    else
      Break;


  if sset <> sname1 + '-1' then
  begin
    Result := False;
    Exit;
  end;

  sname2 := sname1;
  if Length(sname1) = 5 then
    p := 3
  else
    p := 2;
  for i := Length(sname1) downto p do
    sname1[i] := '0';
  while Length(sname1) < 5 do
    sname1 := '0' + sname1;

  //  http://lego.brickinstructions.com/10000/10260/001.jpg
//  Result := NET_DownloadFileImg('https://letsbuilditagain.com/instructions/' + sname2 + '/' + snum + '.jpg', basedefault + outfile);
  if not DirectoryExists(basedefault + '9997') then
    MkDir(basedefault + '9997');
  if not DirectoryExists(basedefault + '9997\' + sset) then
    MkDir(basedefault + '9997\' + sset);

  link := '';
  link2 := '';
  if UpperCase(host) = 'LEGO' then
  begin
    link := 'http://media.brickinstructions.com/' + sname1 + '/' + sname2 + '/' + snum + '.jpg';
    link2 := 'http://lego.brickinstructions.com/' + sname1 + '/' + sname2 + '/' + snum + '.jpg';
  end
  else if UpperCase(host) = 'LETSBUILDITAGAIN' then
    link := 'https://letsbuilditagain.com/instructions/' + sname2 + '/' + snum + '.jpg'
  else if UpperCase(host) = 'BRICKFACTORY' then
  begin
    sname3 := sname1;
    while Length(sname3) < 7 do
      sname3 := '0' + sname3;
    link := 'http://www.brickfactory.info/scans/' + sname3 + '/' + sname2 + '/' + snum + '.jpg'
  end
  else if Trim(host) = '' then
  begin
    fname := basedefault + '9997\' + sset + '\' + sset + '-instructions.txt';
    if fexists(fname) then
    begin
      sl := TStringList.Create;
      try
        S_LoadFromFile(sl, fname);
        if sl.Count > 0 then
        begin
          if Pos('lego.brickinstructions.com', sl.Strings[sl.Count - 1]) > 0 then
            link := 'http://lego.brickinstructions.com/' + sname1 + '/' + sname2 + '/' + snum + '.jpg'
          else if Pos('letsbuilditagain.com', sl.Strings[sl.Count - 1]) > 0 then
             link := 'https://letsbuilditagain.com/instructions/' + sname2 + '/' + snum + '.jpg'
          else if Pos('brickfactory.info', sl.Strings[sl.Count - 1]) > 0 then
          begin
            sname3 := sname1;
            while Length(sname3) < 7 do
              sname3 := '0' + sname3;
            link := 'http://www.brickfactory.info/scans/' + sname3 + '/' + sname2 + '/' + snum + '.jpg';
          end;
        end;
      finally
        sl.Free;
      end;
    end;
  end;

  if link = '' then
    link := 'http://media.brickinstructions.com/' + sname1 + '/' + sname2 + '/' + snum + '.jpg';

  if not forcelink then
    if failedlinks.IndexOf(link) >= 0 then
    begin
      Result := False;
      Exit;
    end;

  SplashProgress('Donwloading page #' + itoa(pg), -1);

  Result := DownloadFileImg(link, basedefault + outfile);
  if not Result then
    if UpperCase(host) = 'LEGO' then
    begin
      if snum = '000' then
      begin
        link := 'http://media.brickinstructions.com/' + sname1 + '/' + sname2 + '/main.jpg';
        Result := DownloadFileImg(link, basedefault + outfile);
      end;
      if not Result then
        Result := DownloadFileImg(link2, basedefault + outfile);
    end;
  if Result then
    if not IsLikeJpeg(basedefault + outfile) then
    begin
      fdelete(basedefault + outfile);
      Result := False;
    end;
  if Result then
  begin
    fname := basedefault + '9997\' + sset + '\' + sset + '-instructions.txt';
    sl := TStringList.Create;
    try
      if fexists(fname) then
        S_LoadFromFile(sl, fname);
      sl.Add(snum + ',' + link);
      S_SaveToFile(sl, fname);
    finally
      sl.Free;
    end;
  end
  else
  begin
    failedlinks.Add(link);
    if failedlinks.Count > 10000 then
      failedlinks.Delete(0);
  end;
end;

function GetInstructionsFromNet(const aset: string; const outlist: TStringList): boolean;
var
  i: integer;
  outfile: string;
begin
  Result := False;

  for i := 1 to 999 do
  begin
    if i = 1 then
    begin
      if GetInstructionPage(aset, i, outfile, False, 'lego') then
      begin
        outlist.Add(outfile);
        Result := True;
      end
      else if GetInstructionPage(aset, i, outfile, False, 'letsbuilditagain') then
      begin
        outlist.Add(outfile);
        Result := True;
      end
      else if GetInstructionPage(aset, i, outfile, False, 'brickfactory') then
      begin
        outlist.Add(outfile);
        Result := True;
      end
      else
        Break;
    end
    else if GetInstructionPage(aset, i, outfile, False, '') then
    begin
      outlist.Add(outfile);
      Result := True;
    end
    else
      Break;
  end;
end;

function UpdateInstructionsFromNet(const aset: string; const forcelink: boolean;
  const prefhost: string = ''): boolean;
var
  i, p: integer;
  snum: string;
  outfile: string;
  sset: string;
  host: string;
begin
  Result := False;

  sset := strtrim(aset);
  p := 0;
  for i := 1 to 999 do
  begin
    snum := itoa(i);
    while Length(snum) < 3 do
      snum := '0' + snum;
    outfile := InstructionDir(sset) + snum + '.jpg';
    if fexists(basedefault + outfile) then
      if IsLikeJpeg(basedefault + outfile) then
        p := i;
    if p <> i then
      Break;
  end;

  for i := p + 1 to 999 do
  begin
    if prefhost = '' then
    begin
      if FileExists(basedefault + '9997\' + sset + '\' + sset + '-instructions.txt') then
        host := ''
      else
        host := 'lego';
    end
    else
      host := prefhost;
    if GetInstructionPage(aset, i, outfile, forcelink, host) then
      Result := True
    else if i = 1 then
    begin
      if GetInstructionPage(aset, i, outfile, forcelink, 'letsbuilditagain') then
        Result := True
      else if GetInstructionPage(aset, i, outfile, forcelink, 'brickfactory') then
        Result := True
      else
        Break;
    end
    else
      Break;
  end;

  HideSplash;
end;

function GetInstructionsFromDisk(const aset: string; const outlist: TStringList): boolean;
var
  i: integer;
  outfile: string;
  sset: string;
begin
  Result := False;

  sset := strtrim(aset);
  for i := 1 to 999 do
  begin
    outfile := InstructionDir(sset) + itoa(i) + '.jpg';
    if fexists(basedefault + outfile) then
    begin
      outlist.Add(outfile);
      Result := True;
    end
    else
      Break;
  end;
end;

function GetInstructions(const aset: string; const outlist: TStringList): boolean;
begin
  Result := GetInstructionsFromDisk(aset, outlist);
  if not Result then
    Result := GetInstructionsFromNet(aset, outlist);
  HideSplash;
end;

function InstructionsExist(const aset: string): boolean;
var
  sset: string;
begin
  sset := strtrim(aset);
  Result := fexists(InstructionDir(sset) + '001.jpg');
end;

initialization
  failedlinks := TStringList.Create;

finalization
  failedlinks.Free;

end.
