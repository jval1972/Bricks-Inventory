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
//    Html writer class
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_docwriter;

interface

uses
  Classes, Htmlview;

type
  TDocument = class(TObject)
  private
    buffer: string;
    capacity: integer;
    size: integer;
    hview: THTMLViewer;
    multipage: boolean;
    key: string;
    navigatepos: integer;
    fsavepath: string;
    fnumids: integer;
    fdivisionscache: TStringList;
    fneedsidletime: boolean;
    currentpage: integer;
    _sectionstart: string;
    _sectionnavigate: string;
    _sectionbeforepages: string;
    function GenerateNavigation(const pg: integer): string;
  protected
    procedure SetSavePath(const value: string);
    procedure DoSaveString(const fname: string; const s: string);
    function DoLoadString(const fname: string): string;
  public
    constructor Create(const aview: THTMLViewer);
    destructor Destroy; override;
    procedure IdleEventHandler(Sender: TObject; var Done: Boolean);
    procedure write(const i: integer); overload;
    procedure write(const s: string); overload;
    procedure write(const Fmt: string; const Args: array of const); overload;
    procedure writecheck(const AA, fromAA, toAA: integer; const i: integer); overload;
    procedure writecheck(const AA, fromAA, toAA: integer; const s: string); overload;
    procedure writecheck(const AA, fromAA, toAA: integer; const Fmt: string; const Args: array of const); overload;
    procedure BlancColorCell(const RGB: LongWord; const width: integer);
    procedure SaveBufferToFile(const fname: string);
    procedure FlashMultiPageDocument(const akey: string; const pg: integer);
    procedure Flash;
    procedure NewMultiPageDocument(const key1, key2: string);
    procedure StartNavigateSection;
    procedure StartItemId(const id: integer);
    procedure EndNavigateSection;
    property savepath: string read fsavepath write SetSavePath;
    property needsidletime: boolean read fneedsidletime;
  end;

implementation

uses
  SysUtils, bi_delphi, bi_system, bi_tmp, bi_crc32, bi_defs, bi_utils;

constructor TDocument.Create(const aview: THTMLViewer);
begin
  inherited Create;
  hview := aview;
  buffer := '';
  capacity := 0;
  size := 0;
  multipage := False;
  navigatepos := 0;
  currentpage := 0;
  fnumids := 0;
  fsavepath := '';
  _sectionstart := '';
  _sectionnavigate := '';
  _sectionbeforepages := '';
  fneedsidletime := False;
  fdivisionscache := TStringList.Create;
  fdivisionscache.CaseSensitive := False;
end;

destructor TDocument.Destroy;
begin
  FreeList(fdivisionscache);
  Inherited;
end;

const
  MINDIVISIONCACHESIZE = 50;
  MAXADIVISIONCACHESIZE = 200;

procedure TDocument.DoSaveString(const fname: string; const s: string);
var
  ts: TString;
  idx: integer;
begin
  idx := fdivisionscache.IndexOf(fname);
  if idx >= 0 then
  begin
    ts := fdivisionscache.Objects[idx] as TString;
    ts.Text := s
  end
  else if fdivisionscache.Count >= MAXADIVISIONCACHESIZE then
  begin
    SaveStringToFile(fname, s);
    I_DeclareTempFile(fname);
  end
  else
  begin
    ts := TString.Create;
    ts.text := s;
    fdivisionscache.AddObject(fname, ts);
  end;
end;

function TDocument.DoLoadString(const fname: string): string;
var
  idx: integer;
begin
  idx := fdivisionscache.IndexOf(fname);
  if idx >= 0 then
    Result := (fdivisionscache.Objects[idx] as TString).text
  else
    Result := LoadStringFromFile(fname);
end;

procedure TDocument.IdleEventHandler(Sender: TObject; var Done: Boolean);
begin
  if fdivisionscache.Count <= MINDIVISIONCACHESIZE then
  begin
    Done := True;
    fneedsidletime := False;
    Exit;
  end;
  SaveStringToFile(fdivisionscache.Strings[0], (fdivisionscache.Objects[0] as TString).text);
  I_DeclareTempFile(fdivisionscache.Strings[0]);
  fdivisionscache.Objects[0].Free;
  fdivisionscache.Delete(0);
  Done := False;
  fneedsidletime := True;
end;

procedure TDocument.write(const i: integer);
begin
  write(IntToStr(i));
end;

procedure TDocument.write(const s: string);
var
  len: integer;
  growstep: integer;
begin
  len := Length(s);
  if capacity < len + size then
  begin
    if capacity > $200000 then
      growstep := $40000
    else if capacity > $100000 then
      growstep := $20000
    else if capacity > $40000 then
      growstep := $10000
    else if capacity > $20000 then
      growstep := 8192 * 4
    else
      growstep := 8192;
    capacity := (len + size + growstep) and not 1023;
    SetLength(buffer, capacity);
  end;
  memmove(@buffer[size + 1], @s[1], len);
  size := size + len;
end;

procedure TDocument.write(const Fmt: string; const Args: array of const);
var
  stmp: string;
begin
  FmtStr(stmp, Fmt, Args);
  write(stmp);
end;

procedure TDocument.writecheck(const AA, fromAA, toAA: integer; const i: integer);
begin
  if AA >= fromAA then
    if AA <= toAA then
      write(i);
end;

procedure TDocument.writecheck(const AA, fromAA, toAA: integer; const s: string);
begin
  if AA >= fromAA then
    if AA <= toAA then
      write(s);
end;

procedure TDocument.writecheck(const AA, fromAA, toAA: integer; const Fmt: string; const Args: array of const);
begin
  if AA >= fromAA then
    if AA <= toAA then
      write(Fmt, Args);
end;

procedure TDocument.BlancColorCell(const RGB: LongWord; const width: integer);
var
  hx: string;
  stmp: string;
begin
  hx := IntToHex(RGB, 6);
  stmp := '<table border=1 width=' + IntToStr(width) + ' bgcolor="#' + hx + '"><tr><td><br></td></tr></table>';
  write(stmp);
end;

procedure TDocument.SetSavePath(const value: string);
begin
  fsavepath := value;
  if fsavepath = '' then
    exit;
  if not (fsavepath[length(fsavepath)] in ['/', '\']) then
    fsavepath := fsavepath + '\';
end;

procedure TDocument.NewMultiPageDocument(const key1, key2: string);
begin
  key := IntToHex(CalcCRC32(key1), 8) + IntToHex(CalcCRC32(key2), 8);
  multipage := True;
  if not DirectoryExists(fsavepath + key) then
    MkDir(fsavepath + key);
end;

procedure TDocument.StartNavigateSection;
begin
  if not multipage then
    Exit;
  if navigatepos > 0 then
    I_Error('TDocument.StartNavigateSection(): Already called within the current document');
  navigatepos := size;
  SetLength(buffer, size);
  _sectionstart := buffer;
  DoSaveString(fsavepath + key + '\' + key + '_1_S.html', _sectionstart);
  buffer := '';
  size := 0;
  capacity := 0;
end;

procedure TDocument.StartItemId(const id: integer);
begin
  if not multipage then
    Exit;

  fnumids := id;
  
  if id = 1 then
  begin
    SetLength(buffer, size);
    _sectionbeforepages := buffer;
    DoSaveString(fsavepath + key + '\' + key + '_3_B.html', _sectionbeforepages);
    buffer := '';
    size := 0;
    capacity := 0;
  end
  else if id mod dpagesize = 1 then
  begin
    inc(currentpage);
    SetLength(buffer, size);
    DoSaveString(fsavepath + key + '\' + key + '_4_P' + IntToStrzFill(4, currentpage) + '.html', buffer);
    buffer := '';
    size := 0;
    capacity := 0;
  end;
end;

function TDocument.GenerateNavigation(const pg: integer): string;

  function GenerateNavigationLinks: string;
  var
    i: integer;
  begin
    Result := '';
    for i := 1 to currentpage do
    begin
      if i = pg then
        Result := Result + ' ' + itoa(i)
      else
        Result := Result + ' [<a href=navigate/' + key + '/' + itoa(i) + '>' + itoa(i) + '</a>]';
    end;
  end;

var
  prevstr, nextstr: string;
begin
  if currentpage <= 1 then
  begin
    Result := '';
    exit;
  end;
  
  if pg = 1 then
    prevstr := '[Previous]'
  else
    prevstr := '[<a href=navigate/' + key + '/' + itoa(pg - 1) + '>Previous</a>]';
  if pg = currentpage then
    nextstr := '[Next]'
  else
    nextstr := '[<a href=navigate/' + key + '/' + itoa(pg + 1) + '>Next</a>]';
  Result := '<table width=99% bgcolor=#FFFFFF border=2>' +
            '<tr bgcolor=#F7E967>' +
            '<td width 100%><p align=left><font color=#000080><b>' +
             prevstr + GenerateNavigationLinks + nextstr +
            '</b><br>' +
             Format('<b>%d</b> Items Found. Page <b>%d</b> of <b>%d</b> (Showing <b>%d</b> Items Per Page)',
              [fnumids, pg, currentpage, dpagesize]) +
            '</font></p></td>' +
            '</tr></table>';
end;

procedure TDocument.EndNavigateSection;
var
  i: integer;
begin
  if not multipage then
    Exit;

  inc(currentpage);
  SetLength(buffer, size);
  DoSaveString(fsavepath + key + '\' + key + '_4_P' + IntToStrzFill(4, currentpage) + '.html', buffer);
  buffer := '';
  size := 0;
  capacity := 0;

  _sectionnavigate := GenerateNavigation(1);
  DoSaveString(fsavepath + key + '\' + key + '_2_N' + IntToStrzFill(4, 1) + '.html', _sectionnavigate);
  for i := 2 to currentpage do
    DoSaveString(fsavepath + key + '\' + key + '_2_N' + IntToStrzFill(4, i) + '.html', GenerateNavigation(i));
end;

procedure TDocument.FlashMultiPageDocument(const akey: string; const pg: integer);
begin
  key := akey;
  hview.LoadHtmlFromString(
    DoLoadString(fsavepath + key + '\' + key + '_1_S.html') +
    DoLoadString(fsavepath + key + '\' + key + '_2_N' + IntToStrzFill(4, pg) + '.html') +
    DoLoadString(fsavepath + key + '\' + key + '_3_B.html') +
    DoLoadString(fsavepath + key + '\' + key + '_4_P' + IntToStrzFill(4, pg) + '.html') +
    DoLoadString(fsavepath + key + '\' + key + '_5_E' + '.html'));
  key := '';
  buffer := '';
  size := 0;
  capacity := 0;
  multipage := False;
  navigatepos := 0;
  currentpage := 0;
  fnumids := 0;
  _sectionstart := '';
  _sectionnavigate := '';
  _sectionbeforepages := '';
  fneedsidletime := True;
end;

procedure TDocument.Flash;
begin
  SetLength(buffer, size);
  capacity := size;
  if multipage then
  begin
    hview.LoadHtmlFromString(
      _sectionstart +
      _sectionnavigate +
      _sectionbeforepages +
      DoLoadString(fsavepath + key + '\' + key + '_4_P' + IntToStrzFill(4, 1) + '.html') +
      buffer);
    DoSaveString(fsavepath + key + '\' + key + '_5_E' + '.html', buffer);
  end
  else
    hview.LoadHtmlFromString(buffer);
  key := '';
  buffer := '';
  size := 0;
  capacity := 0;
  multipage := False;
  navigatepos := 0;
  currentpage := 0;
  fnumids := 0;
  _sectionstart := '';
  _sectionnavigate := '';
  _sectionbeforepages := '';
  fneedsidletime := True;
end;

procedure TDocument.SaveBufferToFile(const fname: string);
var
  t: TextFile;
  i: integer;
begin
  if fname = '' then
    exit;

  assignFile(t, fname);
  rewrite(t);

  if multipage then
  begin
    writeln(t, _sectionstart);
    writeln(t, _sectionbeforepages);
    for i := 1 to currentpage do
      writeln(t, DoLoadString(fsavepath + key + '\' + key + '_4_P' + IntToStrzFill(4, i) + '.html'));
  end;
  SetLength(buffer, size);
  capacity := size;
  writeln(t, buffer);

  CloseFile(t);
end;

end.

