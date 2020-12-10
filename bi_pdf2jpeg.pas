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
//    Extract jpeg images from PDF
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_pdf2jpeg;

interface

uses
  SysUtils, Classes, jpeg;

type
  TPDF2Jpeg = class(TObject)
  private
    { Private declarations }
    Limages: TStringList;
    globalid: integer;
    procedure ClearImages;
  public
    { Public declarations }
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function OpenPDFFromFile(const fname: string): integer;
    function AppendPDFFromFile(const fname: string): integer;
    function OpenPDFFromData(const adata: string): integer;
    function AppendPDFFromData(const adata: string): integer;
    procedure SaveImagesToZip(const fname: string);
    procedure SaveImagesToDir(const apath: string);
    function Count: integer;
    function JPEG(const idx: integer): TJPEGImage;
  end;

implementation

uses
  bi_wzipfile;

constructor TPDF2Jpeg.Create;
begin
  Limages := TStringList.Create;
  globalid := 0;
  Inherited;
end;


destructor TPDF2Jpeg.Destroy;
begin
  ClearImages;
  Limages.Free;
  Inherited;
end;

procedure TPDF2Jpeg.ClearImages;
var
  i: integer;
begin
  for i := 0 to Limages.Count - 1 do
    if Limages.Objects[i] <> nil then
    begin
      Limages.Objects[i].Free;
      Limages.Objects[i] := nil;
    end;
  Limages.Clear;
end;

procedure TPDF2Jpeg.Clear;
begin
  ClearImages;
  globalid := 0;
end;

function TPDF2Jpeg.OpenPDFFromFile(const fname: string): integer;
begin
  ClearImages;
  Result := AppendPDFFromFile(fname);
end;

function TPDF2Jpeg.AppendPDFFromFile(const fname: string): integer;
var
  data: string;
  fs: TFileStream;
begin
  fs := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(data, fs.Size);
    fs.Read((@data[1])^, fs.Size);
    Result := AppendPDFFromData(data);
    SetLength(data, 0);
  finally
    fs.Free;
  end;
end;

function TPDF2Jpeg.OpenPDFFromData(const adata: string): integer;
begin
  ClearImages;
  Result := AppendPDFFromData(adata);
end;

function TPDF2Jpeg.AppendPDFFromData(const adata: string): integer;
var
  data: string;
  p: integer;
  chk1, chk2: string;
  jp: TJPEGImage;
  m: TMemoryStream;
  id: integer;
  imgname: string;
  okimage: boolean;

  function checkpos(const pp: integer; const len: integer): boolean;
  begin
    if pp <= 0 then
    begin
      Result := False;
      Exit;
    end;

    if pp = 1 then
    else if data[pp - 1] in [#10, #13] then
    else
    begin
      Result := False;
      Exit;
    end;

    if pp + len = Length(data) then
    else if data[pp + len] in [#10, #13] then
    else
    begin
      Result := False;
      Exit;
    end;

    Result := True;
  end;

begin
  Result := 0;
  data := adata;
  id := Limages.Count;
  chk1 := 'stream';
  chk2 := 'endstream';
  while True do
  begin
    p := Pos(chk1, data);
    if not checkpos(p, Length(chk1)) then
    begin
      if p = 0 then
        Break;
      Delete(data, 1, p + Length(chk1) - 1);
      if data = '' then
        Break
      else
        Continue;
    end;

    Delete(data, 1, p + Length(chk1) - 1);
    while data <> '' do
    begin
      if data[1] in [#10, #13] then
        Delete(data, 1, 1)
      else
        Break;
    end;

    p := Pos('JFIF', data);
    if p <> 7 then
      Continue;

    p := Pos(chk2, data);
    if p = 0 then
      Break;
{    if not checkpos(p, Length(chk2)) then
      Break;

    while Length(data) >= p do
    begin
      if data[p - 1] in [#10, #13] then
        Delete(data, p - 1, 1)
      else
        Break;
    end;}

    m := TMemoryStream.Create;
    m.Write((@data[1])^, p);
    m.Position := 0;

    okimage := True;
    jp := TJPEGImage.Create;
    try
      jp.LoadFromStream(m);
    except
      okimage := False;
    end;
    m.Free;

    if okimage then
    begin
      inc(id);
      imgname := IntToStr(id);
      while Length(imgname) < 3 do
        imgname := '0' + imgname;
      Limages.AddObject(imgname + '.jpg', jp);
      inc(Result);
    end
    else
      jp.Free;
  end;
end;

procedure TPDF2Jpeg.SaveImagesToZip(const fname: string);
var
  pk3: TWZipFile;
  m: TMemoryStream;
  i: integer;
begin
  pk3 := TWZipFile.Create;
  for i := 0 to Limages.Count - 1 do
  begin
    m := TMemoryStream.Create;
    (Limages.Objects[i] as TJPEGImage).SaveToStream(m);
    AddStreamToZip(pk3, Limages.Strings[i], m);
    m.Free;
  end;
  pk3.SaveToFile(fname);
  pk3.Free;
end;

procedure TPDF2Jpeg.SaveImagesToDir(const apath: string);
var
  i: integer;
  path: string;
begin
  path := Trim(apath);
  if path <> '' then
    if path[Length(path)] <> '\' then
      path := path + '\';
  if not DirectoryExists(path) then
    ForceDirectories(path);
  for i := 0 to Limages.Count - 1 do
    (Limages.Objects[i] as TJPEGImage).SaveToFile(path + Limages.Strings[i]);
end;

function TPDF2Jpeg.Count: integer;
begin
  Result := Limages.Count;
end;

function TPDF2Jpeg.JPEG(const idx: integer): TJPEGImage;
begin
  if (idx < 0) or (idx >= Limages.Count) then
  begin
    Result := nil;
    Exit;
  end;
  Result := Limages.Objects[idx] as TJPEGImage;
end;

end.
