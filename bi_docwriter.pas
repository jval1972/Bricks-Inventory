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
    fcapacity: integer;
    fsize: integer;
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
    _sectionafterpages: string;
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
    procedure title(const s: string); overload;
    procedure title(const Fmt: string; const Args: array of const); overload;
    procedure BlancColorCell(const RGB: LongWord; const width: integer);
    procedure SaveBufferToFile(const aname: string);
    procedure FlashMultiPageDocument(const akey: string; const pg: integer);
    procedure Flash;
    procedure NewMultiPageDocument(const key1, key2: string);
    procedure StartNavigateSection;
    procedure StartItemId(const id: integer);
    procedure EndNavigateSection;
    procedure MarkBottomNavigateSection;
    procedure PredictAdditionalSize(const sz: integer);
    property savepath: string read fsavepath write SetSavePath;
    property needsidletime: boolean read fneedsidletime;
    property size: integer read fsize;
  end;

implementation

uses
  SysUtils, bi_delphi, bi_system, bi_tmp, bi_crc32, bi_defs, bi_utils;

constructor TDocument.Create(const aview: THTMLViewer);
begin
  inherited Create;
  hview := aview;
  buffer := '';
  fcapacity := 0;
  fsize := 0;
  multipage := False;
  navigatepos := 0;
  currentpage := 0;
  fnumids := 0;
  fsavepath := '';
  _sectionstart := '';
  _sectionnavigate := '';
  _sectionbeforepages := '';
  _sectionafterpages := '';
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

procedure TDocument.PredictAdditionalSize(const sz: integer);
begin
  if fsize + sz <= fcapacity then
    Exit;

  fcapacity := (sz + fsize + 1024) and not 1023;
  SetLength(buffer, fcapacity);
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
  if fcapacity < len + fsize then
  begin
    if fcapacity > $200000 then
      growstep := $40000
    else if fcapacity > $100000 then
      growstep := $20000
    else if fcapacity > $40000 then
      growstep := $10000
    else if fcapacity > $20000 then
      growstep := 8192 * 4
    else
      growstep := 8192;
    fcapacity := (len + fsize + growstep) and not 1023;
    SetLength(buffer, fcapacity);
  end;
  memmove(@buffer[fsize + 1], @s[1], len);
  fsize := fsize + len;
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

procedure TDocument.title(const s: string);
begin
  write('<title>' + s + '</title>');
end;

procedure TDocument.title(const Fmt: string; const Args: array of const);
var
  stmp: string;
begin
  FmtStr(stmp, Fmt, Args);
  title(stmp);
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
  navigatepos := fsize;
  SetLength(buffer, fsize);
  _sectionstart := buffer;
  DoSaveString(fsavepath + key + '\' + key + '_1_S.html', _sectionstart);
  buffer := '';
  fsize := 0;
  fcapacity := 0;
end;

procedure TDocument.StartItemId(const id: integer);
begin
  if not multipage then
    Exit;

  fnumids := id;
  
  if id = 1 then
  begin
    SetLength(buffer, fsize);
    fcapacity := fsize;
    _sectionbeforepages := buffer;
    DoSaveString(fsavepath + key + '\' + key + '_3_B.html', _sectionbeforepages);
//    buffer := '';
    fsize := 0;
//    fcapacity := 0;
  end
  else if id mod dpagesize = 1 then
  begin
    inc(currentpage);
    SetLength(buffer, fsize);
    fcapacity := fsize;
    DoSaveString(fsavepath + key + '\' + key + '_4_P' + IntToStrzFill(4, currentpage) + '.html', buffer);
//    buffer := '';
    fsize := 0;
//    fcapacity := 0;
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
  SetLength(buffer, fsize);
  DoSaveString(fsavepath + key + '\' + key + '_4_P' + IntToStrzFill(4, currentpage) + '.html', buffer);
  buffer := '';
  fsize := 0;
  fcapacity := 0;

  _sectionnavigate := GenerateNavigation(1);
  DoSaveString(fsavepath + key + '\' + key + '_2_N' + IntToStrzFill(4, 1) + '.html', _sectionnavigate);
  for i := 2 to currentpage do
    DoSaveString(fsavepath + key + '\' + key + '_2_N' + IntToStrzFill(4, i) + '.html', GenerateNavigation(i));
end;

procedure TDocument.MarkBottomNavigateSection;
begin
  if not multipage then
    Exit;

  SetLength(buffer, fsize);
  _sectionafterpages := buffer;
  DoSaveString(fsavepath + key + '\' + key + '_5_A.html', buffer);
  buffer := '';
  fsize := 0;
  fcapacity := 0;
end;

procedure TDocument.FlashMultiPageDocument(const akey: string; const pg: integer);
var
  navstr: string;
begin
  key := akey;
  navstr := DoLoadString(fsavepath + key + '\' + key + '_2_N' + IntToStrzFill(4, pg) + '.html');
  hview.LoadHtmlFromString(
    DoLoadString(fsavepath + key + '\' + key + '_1_S.html') +
    navstr +
    DoLoadString(fsavepath + key + '\' + key + '_3_B.html') +
    DoLoadString(fsavepath + key + '\' + key + '_4_P' + IntToStrzFill(4, pg) + '.html') +
    DoLoadString(fsavepath + key + '\' + key + '_5_A' + '.html') +
    navstr +
    DoLoadString(fsavepath + key + '\' + key + '_6_E' + '.html')
  );
  key := '';
  buffer := '';
  fsize := 0;
  fcapacity := 0;
  multipage := False;
  navigatepos := 0;
  currentpage := 0;
  fnumids := 0;
  _sectionstart := '';
  _sectionnavigate := '';
  _sectionbeforepages := '';
  _sectionafterpages := '';
  fneedsidletime := True;
end;

procedure TDocument.Flash;
begin
  SetLength(buffer, fsize);
  fcapacity := fsize;
  if multipage then
  begin
    hview.LoadHtmlFromString(
      _sectionstart +
      _sectionnavigate +
      _sectionbeforepages +
      DoLoadString(fsavepath + key + '\' + key + '_4_P' + IntToStrzFill(4, 1) + '.html') +
      _sectionafterpages +
      _sectionnavigate +
      buffer
    );
    DoSaveString(fsavepath + key + '\' + key + '_6_E' + '.html', buffer);
  end
  else
    hview.LoadHtmlFromString(buffer);
  key := '';
  buffer := '';
  fsize := 0;
  fcapacity := 0;
  multipage := False;
  navigatepos := 0;
  currentpage := 0;
  fnumids := 0;
  _sectionstart := '';
  _sectionnavigate := '';
  _sectionbeforepages := '';
  _sectionafterpages := '';
  fneedsidletime := True;
end;

procedure TDocument.SaveBufferToFile(const aname: string);
var
  t: TextFile;
  i: integer;
  fname: string;
begin
  if aname = '' then
    exit;

  fname := '';
  for i := 1 to Length(aname) do
  begin
    if aname[i] = '?' then
      fname := fname + '_#QUEST#_'
    else if aname[i] = '*' then
      fname := fname + '_#STAR#_'
    else
      fname := fname + aname[i];
  end;

  assignFile(t, fname);
  rewrite(t);

  if multipage then
  begin
    writeln(t, _sectionstart);
    writeln(t, _sectionbeforepages);
    for i := 1 to currentpage do
      writeln(t, DoLoadString(fsavepath + key + '\' + key + '_4_P' + IntToStrzFill(4, i) + '.html'));
    writeln(t, _sectionafterpages);
  end;
  SetLength(buffer, fsize);
  fcapacity := fsize;
  writeln(t, buffer);

  CloseFile(t);
end;

end.

