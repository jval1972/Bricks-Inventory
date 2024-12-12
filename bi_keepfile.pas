//------------------------------------------------------------------------------
//
//  BrickInventory: A tool for managing your brick collection
//  Copyright (C) 2014-2024 by Jim Valavanis
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
//    Keep file (for images)
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_keepfile;

interface

uses
  SysUtils, Classes;

procedure BI_KeepFileInit;

procedure BI_KeepFileDone;

function BI_KeepFileStream(const name: string): TStream;

function BI_KeepFileFlash(const id: integer = 0): boolean;

implementation

uses
  bi_delphi, bi_utils, bi_tmp;

var
  keepfile: TStringList = nil;

procedure BI_KeepFileInit;
begin
  keepfile := TStringList.Create;
end;

procedure BI_KeepFileDone;
var
  cnt: integer;
  mx: integer;
begin
  if keepfile = nil then
    exit;

  cnt := 0;
  mx := 0;
  while keepfile.Count > cnt do
  begin
    if not BI_KeepFileFlash(cnt) then
      inc(cnt);
    inc(mx);
    if mx > keepfile.Count then
      break;
  end;
  freelist(keepfile);
  keepfile := nil;
end;

function BI_KeepFileStream(const name: string): TStream;
var
  fs: TFileStream;
  tmppath: string;
  ts: TString;
begin
  if keepfile = nil then
    BI_KeepFileInit;

  tmppath := I_NewTempFile(name);
  fs := TFileStream.Create(tmppath, fmCreate);
  ts := TString.Create;
  ts.text := tmppath;
  keepfile.AddObject(name, ts);
  Result := fs;
end;

function BI_KeepFileFlash(const id: integer = 0): boolean;
var
  ts: TString;
begin
  Result := False;

  if keepfile = nil then
    Exit;

  if (id < 0) or (id >= keepfile.Count) then
    Exit;

  ts := keepfile.Objects[id] as TString;
  if CopyFile(ts.text, keepfile.Strings[id]) then
  begin
    ts.Free;
    keepfile.Delete(id);
    Result := True;
  end;
end;

end.
