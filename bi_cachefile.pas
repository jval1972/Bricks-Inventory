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
//    Cached File System
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_cachefile;

interface

uses
  Windows, SysUtils, Classes, bi_hash;

function S_SaveToFile(const s: TStringList; const fname: string; const maxretry: integer = 10; const msecs: integer = 100): boolean;

function S_SaveToFileUTF8(const s: TStringList; const fname: string; const maxretry: integer = 10; const msecs: integer = 100): boolean;

function S_LoadFromFile(const s: TStringList; const fname: string; const maxretry: integer = 10; const msecs: integer = 100): boolean;

function S_GetStringList(const fname: string; const fsorted: boolean; const maxretry: integer = 10; const msecs: integer = 100): TStringList;

function SH_SaveToFile(const s: THashStringList; const fname: string; const maxretry: integer = 10; const msecs: integer = 100): boolean;

function SH_LoadFromFile(const s: THashStringList; const fname: string; const maxretry: integer = 10; const msecs: integer = 100): boolean;

function S_AppendLineToFile(const fname: string; const line: string): boolean;

procedure S_BackupFile(const fn: string);

procedure S_SetCacheFile(const fn: string);

procedure S_InitFileSystem;

procedure S_FlashFileSystem;

procedure S_ShutDownFileSystem;

const
  MAXFSCACHEHITS = 10;

implementation

uses
  DateUtils, bi_delphi;

function S_SaveToFile_disk(const s: TStringList; const fname: string; const maxretry: integer = 10; const msecs: integer = 100): boolean;
var
  success: boolean;
  i: integer;

  procedure DoSave;
  begin
    success := True;
    try
      s.SaveToFile(fname);
    except
      success := False;
    end;
  end;

begin
  success := False;
  for i := 1 to maxretry do
  begin
    DoSave;
    if not success then
      Sleep(msecs)
    else
      break;
  end;
  Result := success;
end;

function S_LoadFromFile_disk(const s: TStringList; const fname: string; const maxretry: integer = 10; const msecs: integer = 500): boolean;
var
  success: boolean;
  i: integer;

  procedure DoLoad;
  begin
    success := True;
    try
      if fexists(fname) then
        s.LoadFromFile(fname);
    except
      success := False;
    end;
  end;

begin
  success := False;
  for i := 1 to maxretry do
  begin
    DoLoad;
    if not success then
      Sleep(msecs)
    else
      break;
  end;
  Result := success;
end;

function SH_SaveToFile_disk(const s: THashStringList; const fname: string; const maxretry: integer = 10; const msecs: integer = 500): boolean;
var
  success: boolean;
  i: integer;

  procedure DoSave;
  begin
    success := True;
    try
      s.SaveToFile(fname);
    except
      success := False;
    end;
  end;

begin
  success := False;
  for i := 1 to maxretry do
  begin
    DoSave;
    if not success then
      Sleep(msecs)
    else
      break;
  end;
  Result := success;
end;

function SH_LoadFromFile_disk(const s: THashStringList; const fname: string; const maxretry: integer = 10; const msecs: integer = 500): boolean;
var
  success: boolean;
  i: integer;

  procedure DoLoad;
  begin
    success := True;
    try
      if fexists(fname) then
        s.LoadFromFile(fname);
    except
      success := False;
    end;
  end;

begin
  success := False;
  for i := 1 to maxretry do
  begin
    DoLoad;
    if not success then
      Sleep(msecs)
    else
      break;
  end;
  Result := success;
end;

function DoAppendLineToFile(const fname: string; const line: string): boolean;
var
  f: file;
  i: integer;
  oldmode: byte;
  b: byte;
  BA: array[1..2] of byte;
  fsize: integer;
begin
  IOResult;
{$I-}
  oldmode := FileMode;
  if not fexists(fname) then
  begin
    assignfile(f, fname);
    FileMode := 2;
    rewrite(f, 1);
  end
  else
  begin
    assignfile(f, fname);
    FileMode := 2;
    reset(f, 1);
    fsize := FileSize(f);
    if fsize >= 1 then
    begin
      if fsize > 1 then
      begin
        seek(f, FileSize(f) - 2);
        BlockRead(f, BA, 2 * SizeOf(byte));
      end
      else
      begin
        BA[1] := 0;
        BA[2] := 0;
      end;
      if (BA[1] <> 13) or (BA[2] <> 10) then
      begin
        seek(f, fsize);
        BA[1] := 13;
        BA[2] := 10;
        BlockWrite(f, BA, 2 * SizeOf(byte));
      end;
    end;
  end;

  seek(f, FileSize(f));
  for i := 1 to length(line) do
  begin
    b := Ord(line[i]);
    BlockWrite(f, b, SizeOf(byte));
  end;
  BA[1] := 13;
  BA[2] := 10;
  BlockWrite(f, BA, 2 * SizeOf(byte));
  closeFile(f);

  FileMode := oldmode;
{$I+}
  Result := IOResult = 0;
end;

function S_AppendLineToFile_disk(const fname: string; const line: string): boolean;
var
  i: integer;
begin
  Result := True;
  for i := 1 to 10 do
  begin
    if DoAppendLineToFile(fname, line) then
      Exit;
    Sleep(50);
  end;
  for i := 1 to 10 do
  begin
    if DoAppendLineToFile(fname, line) then
      Exit;
    Sleep(200);
  end;
  for i := 1 to 10 do
  begin
    if DoAppendLineToFile(fname, line) then
      Exit;
    Sleep(1000);
  end;
  Result := False;
end;

procedure S_BackupFile_disk(const fn: string);
var
  fbck: string;
  fname: string;
begin
  fname := Trim(fn);

  if fname = '' then
    Exit;

  if not fexists(fname) then
    Exit;

  fbck := fname + '_' + FormatDateTime('yyyymmdd', Now);
  if fexists(fbck) then
    fbck := fbck + '_latest';
  CopyFile(fname, fbck);
end;

//------------------------------------------------------------------------------
type
  TFSList = class(TObject)
    list: TStringList;
    hits: integer;
    fname: string;
    needsbackup: boolean;
    constructor Create(const aname: string); virtual;
    procedure Flash;
    destructor Destroy; override;
  end;

constructor TFSList.Create(const aname: string);
begin
  Inherited Create;
  list := TStringList.Create;
  fname := aname;
  needsbackup := False;
  S_LoadFromFile_disk(list, fname);
  hits := 0;
end;

procedure TFSList.Flash;
begin
  if hits > 0 then
  begin
    if needsbackup then
    begin
      S_BackupFile_disk(fname);
      needsbackup := False;
    end;
    if S_SaveToFile_disk(list, fname) then
      hits := 0;
  end;
end;

destructor TFSList.Destroy;
begin
  if hits > 0 then
  begin
    if needsbackup then
      S_BackupFile_disk(fname);
    S_SaveToFile_disk(list, fname);
  end;
  list.Free;
  Inherited;
end;
//------------------------------------------------------------------------------

var
  FS: TStringList = nil;

procedure S_SetCacheFile(const fn: string);
var
  f: TFSList;
  idx: integer;
begin
  if FS = nil then
    Exit;

  idx := FS.IndexOf(fn);
  if idx < 0 then
  begin
    f := TFSList.Create(fn);
    FS.AddObject(fn, f);
  end;
end;

procedure S_InitFileSystem;
begin
  FS := TStringList.Create;
  FS.Sorted := True;
  FS.CaseSensitive := False;
end;

procedure S_ShutDownFileSystem;
var
  i: integer;
begin
  if FS = nil then
    Exit;

  for i := 0 to FS.Count - 1 do
    (FS.Objects[i] as TFSList).Free;
  FS.Free;
  FS := nil;
end;

procedure S_FlashFileSystem;
var
  i: integer;
begin
  if FS = nil then
    Exit;
  for i := 0 to FS.Count - 1 do
    (FS.Objects[i] as TFSList).Flash;
end;

function S_GetFSList(const fname: string): TFSList;
var
  idx: integer;
begin
  if FS = nil then
  begin
    Result := nil;
    Exit;
  end;

  idx := FS.IndexOf(fname);
  if idx >= 0 then
    Result := FS.Objects[idx] as TFSList
  else
    Result := nil;
end;

function S_SaveToFile(const s: TStringList; const fname: string; const maxretry: integer = 10; const msecs: integer = 100): boolean;
var
  f: TFSList;
begin
  f := S_GetFSList(fname);
  if f = nil then
  begin
    Result := S_SaveToFile_disk(s, fname, maxretry, msecs);
    Exit;
  end;
  if f.list <> s then
    f.list.Text := s.Text;
  inc(f.hits);
  if f.hits > MAXFSCACHEHITS then
    f.Flash;
  Result := True;
end;

function S_SaveToFileUTF8(const s: TStringList; const fname: string; const maxretry: integer = 10; const msecs: integer = 100): boolean;
var
  s2: TStringList;
begin
  s2 := TStringList.Create;
  s2.Text := AnsiToUtf8(s.Text);
  Result := S_SaveToFile(s2, fname, maxretry, msecs);
end;

function S_LoadFromFile(const s: TStringList; const fname: string; const maxretry: integer = 10; const msecs: integer = 100): boolean;
var
  f: TFSList;
begin
  f := S_GetFSList(fname);
  if f = nil then
  begin
    Result := S_LoadFromFile_disk(s, fname, maxretry, msecs);
    Exit;
  end;
  s.Text := f.list.Text;
  Result := True;
end;

function S_GetStringList(const fname: string; const fsorted: boolean; const maxretry: integer = 10; const msecs: integer = 100): TStringList;
var
  f: TFSList;
begin
  f := S_GetFSList(fname);
  if f = nil then
    Result := nil
  else
  begin
    Result := f.list;
    if fsorted then
      Result.Sorted := True;
  end;
end;

function SH_SaveToFile(const s: THashStringList; const fname: string; const maxretry: integer = 10; const msecs: integer = 100): boolean;
var
  f: TFSList;
begin
  f := S_GetFSList(fname);
  if f = nil then
  begin
    Result := SH_SaveToFile_disk(s, fname, maxretry, msecs);
    Exit;
  end;
  f.list.Text := s.Text;
  inc(f.hits);
  if f.hits > MAXFSCACHEHITS then
    f.Flash;
  Result := True;
end;

function SH_LoadFromFile(const s: THashStringList; const fname: string; const maxretry: integer = 10; const msecs: integer = 100): boolean;
var
  f: TFSList;
begin
  f := S_GetFSList(fname);
  if f = nil then
  begin
    Result := SH_LoadFromFile_disk(s, fname, maxretry, msecs);
    Exit;
  end;
  s.Text := f.list.Text;
  Result := True;
end;

function S_AppendLineToFile(const fname: string; const line: string): boolean;
var
  f: TFSList;
begin
  f := S_GetFSList(fname);
  if f = nil then
  begin
    Result := S_AppendLineToFile_disk(fname, line);
    Exit;
  end;
  f.list.Add(line);
  inc(f.hits);
  if f.hits > MAXFSCACHEHITS then
    f.Flash;
  Result := True;
end;

procedure S_BackupFile(const fn: string);
var
  f: TFSList;
begin
  f := S_GetFSList(fn);
  if f = nil then
  begin
    S_BackupFile_disk(fn);
    Exit;
  end;
  f.needsbackup := True;
end;

end.
