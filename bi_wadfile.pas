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
//    WAD File Container
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_wadfile;

interface

uses
  Classes, SysUtils;

type
  wadinfo_t = packed record
    // Should be "DWAD"
    identification: integer;
    numlumps: integer;
    infotableofs: integer;
  end;
  Pwadinfo_t = ^wadinfo_t;

  filelump_t = packed record
    filepos: integer;
    size: integer;
    name: string[55];
  end;
  Pfilelump_t = ^filelump_t;
  Tfilelump_tArray = packed array[0..$FFFF] of filelump_t;
  Pfilelump_tArray = ^Tfilelump_tArray;

const
  DWAD = integer(Ord('D') or
                (Ord('W') shl 8) or
                (Ord('A') shl 16) or
                (Ord('D') shl 24));

type
  TWadWriter = class(TObject)
  private
    lumps: TStringList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure AddData(const lumpname: string; const data: pointer; const size: integer);
    procedure AddFile(const lumpname: string; const datafname: string);
    procedure AddString(const lumpname: string; const data: string);
    procedure AddStringList(const lumpname: string; const lst: TStringList);
    procedure AddSeparator(const lumpname: string);
    procedure SaveToStream(const strm: TStream);
    procedure SaveToFile(const fname: string);
  end;

// -----------------------------------------------------------------------------

type
  TWadReader = class
  private
    h: wadinfo_t;
    la: Pfilelump_tArray;
    fs: TFileStream;
    ffilename: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure OpenWadFile(const aname: string);
    function ReadEntry(const id: integer; var buf: pointer; var bufsize: integer): boolean; overload;
    function ReadEntry(const aname: string; var buf: pointer; var bufsize: integer): boolean; overload;
    function SaveEntry(const id: integer; const fn: string): boolean; overload;
    function SaveEntry(const aname: string; const fn: string): boolean; overload;
    function EntryName(const id: integer): string;
    function EntryId(const aname: string): integer;
    function EntryInfo(const id: integer): Pfilelump_t; overload;
    function EntryInfo(const aname: string): Pfilelump_t; overload;
    function NumEntries: integer;
    function FileSize: integer;
    property FileName: string read ffilename;
  end;

implementation

uses
  bi_tmp, bi_delphi, bi_system, bi_binary;

constructor TWadWriter.Create;
begin
  lumps := TStringList.Create;
  Inherited;
end;

destructor TWadWriter.Destroy;
var
  i: integer;
begin
  for i := 0 to lumps.Count - 1 do
    if lumps.Objects[i] <> nil then
      lumps.Objects[i].Free;
  lumps.Free;
  Inherited;
end;

procedure TWadWriter.Clear;
var
  i: integer;
begin
  for i := 0 to lumps.Count - 1 do
    if lumps.Objects[i] <> nil then
      lumps.Objects[i].Free;
  lumps.Clear;
end;

procedure TWadWriter.AddData(const lumpname: string; const data: pointer; const size: integer);
var
  fs: TFileStream;
  datafname: string;
begin
  datafname := I_NewTempFile('wad');

  fs := TFileStream.Create(datafname, fmCreate);
  try
    fs.Write(data^, size);
  finally
    fs.Free;
  end;

  lumps.AddObject(lumpname, TStringInit.Create(datafname));
end;

procedure TWadWriter.AddFile(const lumpname: string; const datafname: string);
begin
  if FileExists(datafname) then
    lumps.AddObject(lumpname, TStringInit.Create(datafname));
end;

procedure TWadWriter.AddString(const lumpname: string; const data: string);
var
  m: TMemoryStream;
  datafname: string;
  i: integer;
begin
  datafname := I_NewTempFile('wad');

  m := TMemoryStream.Create;
  for i := 1 to Length(data) do
    m.Write(data[i], SizeOf(char));
  m.SaveToFile(datafname);
  m.Free;
  lumps.AddObject(lumpname, TStringInit.Create(datafname));
end;

procedure TWadWriter.AddStringList(const lumpname: string; const lst: TStringList);
var
  stmp: string;
begin
  stmp := lst.Text;
  AddString(lumpname, stmp);
end;

procedure TWadWriter.AddSeparator(const lumpname: string);
begin
  lumps.Add(lumpname);
end;

procedure TWadWriter.SaveToStream(const strm: TStream);
var
  h: wadinfo_t;
  fs: TFileStream;
  la: Pfilelump_tArray;
  i: integer;
  si: TStringInit;
  p, ssize: integer;
begin
  p := strm.Position;
  h.identification := DWAD;
  h.numlumps := lumps.Count;
  h.infotableofs := p + SizeOf(wadinfo_t);
  strm.Write(h, SizeOf(h));
  p := strm.Position;
  GetMem(la, lumps.Count * SizeOf(filelump_t));
  ZeroMemory(la, lumps.Count * SizeOf(filelump_t));
  strm.Write(la^, lumps.Count * SizeOf(filelump_t));

  for i := 0 to lumps.Count - 1 do
  begin
    la[i].filepos := strm.Position;
    si := lumps.Objects[i] as TStringInit;
    if si <> nil then
    begin
      fs := TFileStream.Create(si.text, fmOpenRead);
      la[i].size := fs.Size;
      strm.CopyFrom(fs, la[i].size);
      fs.Free;
    end
    else
      la[i].size := 0;
    la[i].name := lumps.Strings[i];
  end;
  ssize := strm.Position;
  strm.Position := p;
  strm.Write(la^, lumps.Count * SizeOf(filelump_t));
  FreeMem(la, lumps.Count * SizeOf(filelump_t));
  strm.Position := ssize;
end;

procedure TWadWriter.SaveToFile(const fname: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(fname, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

// -----------------------------------------------------------------------------

constructor TWadReader.Create;
begin
  h.identification := 0;
  h.numlumps := 0;
  h.infotableofs := 0;
  la := nil;
  fs := nil;
  ffilename := '';
  Inherited;
end;

destructor TWadReader.Destroy;
begin
  Clear;
  Inherited;
end;

procedure TWadReader.Clear;
begin
  if h.numlumps > 0 then
  begin
    MemFree(pointer(la), h.numlumps * SizeOf(filelump_t));
    h.identification := 0;
    h.numlumps := 0;
    h.infotableofs := 0;
    la := nil;
    ffilename := '';
  end
  else
  begin
    h.identification := 0;
    h.infotableofs := 0;
  end;
  if fs <> nil then
  begin
    fs.Free;
    fs := nil;
  end;
end;

procedure TWadReader.OpenWadFile(const aname: string);
begin
  if aname = '' then
    Exit;
  {$IFDEF DEBUG}
  print('Opening WAD file ' + aname + #13#10);
  {$ENDIF}
  Clear;
  if fexists(aname) then
  begin
    fs := TFileStream.Create(aname, fmOpenRead);

    fs.Read(h, SizeOf(wadinfo_t));
    if (h.numlumps > 0) and (h.infotableofs < fs.Size) and (h.identification = DWAD) then
    begin
      fs.Seek(h.infotableofs, sFromBeginning);
      la := malloc(h.numlumps * SizeOf(filelump_t));
      fs.Read(la^, h.numlumps * SizeOf(filelump_t));
      ffilename := aname;
    end
    else
      I_Warning('TWadReader.OpenWadFile(): Invalid WAD file ' + aname + #13#10);
  end
  else
    I_Warning('TWadReader.OpenWadFile(): Can not find WAD file ' + aname + #13#10);
end;

function TWadReader.ReadEntry(const id: integer; var buf: pointer; var bufsize: integer): boolean;
var
  b: TBinary;
  m: TMemoryStream;
begin
  if (fs <> nil) and (id >= 0) and (id < h.numlumps) then
  begin
    fs.Seek(la[id].filepos, sFromBeginning);

    b := TBinary.Create;
    b.LoadFromStream(fs, True);

    m := TMemoryStream.Create;
    b.SaveToStream(m, False);
    b.Free;

    bufsize := m.Size;
    buf := malloc(bufsize);
    memcpy(buf, m.Memory, bufsize);

    m.Free;

    Result := True;
  end
  else
    Result := False;
end;

function TWadReader.ReadEntry(const aname: string; var buf: pointer; var bufsize: integer): boolean;
var
  id: integer;
begin
  id := EntryId(aname);
  if id >= 0 then
    Result := ReadEntry(id, buf, bufsize)
  else
    Result := False;
end;

function TWadReader.SaveEntry(const id: integer; const fn: string): boolean;
var
  b: TBinary;
begin
  if (fs <> nil) and (id >= 0) and (id < h.numlumps) then
  begin
    fs.Seek(la[id].filepos, sFromBeginning);

    b := TBinary.Create;
    b.LoadFromStream(fs, True);

    b.SaveToFile(fn, False);
    b.Free;

    Result := True;
  end
  else
    Result := False;
end;

function TWadReader.SaveEntry(const aname: string; const fn: string): boolean;
var
  id: integer;
begin
  id := EntryId(aname);
  if id >= 0 then
    Result := SaveEntry(id, fn)
  else
    Result := false;
end;

function TWadReader.EntryName(const id: integer): string;
begin
  if (id >= 0) and (id < h.numlumps) then
    Result := la[id].name
  else
    Result := '';
end;

function TWadReader.EntryId(const aname: string): integer;
var
  i: integer;
  uname: string;
begin
  uname := strupper(aname);
  for i := h.numlumps - 1 downto 0 do
    if strupper(la[i].name) = uname then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

function TWadReader.EntryInfo(const id: integer): Pfilelump_t;
begin
  if (id >= 0) and (id < h.numlumps) then
    Result := @la[id]
  else
    Result := nil;
end;

function TWadReader.EntryInfo(const aname: string): Pfilelump_t;
begin
  result := EntryInfo(EntryId(aname));
end;

function TWadReader.NumEntries: integer;
begin
  Result := h.numlumps;
end;

function TWadReader.FileSize: integer;
begin
  if fs <> nil then
    Result := fs.Size
  else
    Result := 0;
end;

end.


