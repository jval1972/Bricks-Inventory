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
//    Utility functions
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_utils;

interface

uses
  Windows, SysUtils, Classes, bi_hash, Graphics, ExtCtrls, Consts;

{$IFNDEF CRAWLER}
function findsimilarimagestring(const hash: THashTable; const s: string): string;
{$ENDIF}

function IndexOfString(const hash: THashTable; const s: string): integer;

function IndexOfStart(const lst: TStringList; const s: string): integer;

procedure RemoveDuplicates(const s: TStringList);

procedure FreeList(var s: TStringList);

procedure ClearList(const s: TStringList);

procedure SaveBmpToJpeg(const MyBitmap: TBitmap; const JPEGFName: string);

function RGBInvert(const t: TColor): TColor;

{$IFNDEF CRAWLER}
function PieceToImage(const img: TImage; const piece: string; color: integer = -1): boolean;
{$ENDIF}

function filenamestring(const s: string): string;

type
  TBStringList = class(TStringList)
  public
    procedure AppendFromFile(const fname: string);
  end;

function DownloadFile(SourceFile, DestFile: string): Boolean;

function DownloadFileImg(SourceFile, DestFile: string): Boolean;

function NET_DownloadFileImg(SourceFile, DestFile: string): Boolean;

function DownloadJpgFileToPNG(SourceFile, DestFile: string): Boolean;

function DownloadPngFileToJpg(SourceFile, DestFile: string): Boolean;

function IsLikeJpeg(const fname: string): boolean;

function DownloadGIFFileToPNG(SourceFile, DestFile: string): Boolean;

function JPG2PNG(const ajpg, apng: string): Boolean;

function PNG2JPG(const apng, ajpg: string): Boolean;

procedure SetClipboardTextWideString(const Text: WideString);

function GetFileCreationTime(sFileName: string): TDateTime;

function GetPrevAlphanumeric(const s: string): string;
function GetNextAlphanumeric(const s: string): string;

function InputQuery2(const ACaption, APrompt, APrompt2: string;
  var Value, Value2: string): Boolean;

function InputListQuery(const ACaption, APrompt: string; const AListItems: string;
  var Value: string): Boolean;

function InputInteger(const ACaption, APrompt: string;
  var Value: integer): Boolean;

function InputTwoIntegers(const ACaption, APrompt, APrompt2: string;
  var Value, Value2: integer): Boolean;

function CheckValidImageDonwload(const src: string): boolean;

{ Count the number of occurrences of a certain character in a String. }
function CountChars(const S: string; const C: char): integer;
function LeftPad(PadString: string; HowMany: integer; PadValue: string): string;
function RemoveChars(const str: string; numericOnly: boolean): string;
function ReplaceWhiteSpace(const s: string; const c: char; const strictMode: boolean): string;
function RemoveHTMLTags(S: string): string;
{ Return the position of the last occurence of a substring in String. }
function LastPos(SubStr, S: string): Integer;
{ Create URI from template replace text between two % }
function AbsoluteURI(const template, filename, color: string): string;
function GetHost(s: string): string;
{ Covert to file content to iner format }
function LDCadToCSV(const filename: string): string;
function LDrawToCSV(const filename: string): string;

function MinI(x, y: integer): integer;
function MaxI(x, y: integer): integer;

function GetAveCharSize(Canvas: TCanvas): TPoint;

function RemoveSpecialTagsFromString(const s: string): string;

function ResizeJpg2Png(const fin, fout: string; const dograyscale: boolean = True;
  const maxWidth: integer = 64; const maxHeight: integer = 64): boolean;

function ResizePng2Png(const fin, fout: string; const dograyscale: boolean = True;
  const maxWidth: integer = 64; const maxHeight: integer = 64): boolean;

procedure MakeGrayScaleBitmap(const ABitmap: TBitmap);

type
  TCounterObject = class(TObject)
  private
    fvalue: LongWord;
  public
    constructor Create(const avalue: LongWord = 0); virtual;
    procedure IncL;
    procedure DecL;
    property value: LongWord read fvalue write fvalue;
  end;

function htmlstripstring(const htm: string): string;

type
  TRecordInfo = class
  public
    position: Int64;
    constructor Create(const apos: Int64);
  end;

function IsValidDBMask(const msk: string): boolean;

procedure RemoveBlancLines(const lst: TStringList);

procedure backupfile(const fn: string);

function ftoadot(f: double): string;

procedure S_FirstLine(const sl: TStringList; const line: string);

function SortListAndFindFirstLeftMatch(const l: TStringList; const s: string): integer;

function GetMemoryUsed: LongWord;

procedure SetDoubleStringsToSpace(const lst: TStringList);

type
  TStringListContainer = class(TObject)
  private
    flist: TStringList;
    fflags: LongWord;
    procedure CreateList;
    function GetSorted: Boolean;
    procedure SetSorted(Value: Boolean);
    function GetCaseSensitive: Boolean;
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    function GetCount: integer;
    function Get(Index: Integer): string;
    procedure Put(Index: Integer; const S: string);
    function GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
    function GetTextStr: string;
    procedure SetTextStr(const Value: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const S: string): Integer;
    function AddObject(const S: string; AObject: TObject): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function IndexOf(const S: string): Integer;
    procedure Insert(Index: Integer; const S: string);
    procedure InsertObject(Index: Integer; const S: string;
      AObject: TObject);
    procedure Sort;
    procedure CustomSort(Compare: TStringListSortCompare);

    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    property Count: Integer read GetCount;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Strings[Index: Integer]: string read Get write Put; default;
    property Text: string read GetTextStr write SetTextStr;

    property Sorted: Boolean read GetSorted write SetSorted;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
  end;

procedure FreeListContainer(var s: TStringListContainer);

implementation

uses
  FastMM4,
  bi_delphi, bi_db, jpeg, bi_pak, bi_tmp, URLMon, Clipbrd, pngimage, GIFImage,
  bi_globals, bi_cachefile, bi_system, bi_multithread, Forms, StdCtrls, Controls,
  bi_crawler, StrUtils;

function IndexOfString(const hash: THashTable; const s: string): integer;
begin
  Result := hash.GetPos(s);
  if Result = -1 then
  begin
    Result := hash.List.IndexOf(s);
    Exit;
  end;
  if hash.List.Strings[Result] = s then
    Exit;
  Result := hash.List.IndexOf(s);
end;

function IndexOfStart(const lst: TStringList; const s: string): integer;
var
  i: integer;
begin
  Result := -1;
  if lst = nil then
    Exit;

  for i := 0 to lst.Count - 1 do
    if Pos1(s, lst.Strings[i]) then
    begin
      Result := i;
      Exit;
    end;
end;

function MaxI(x, y: integer): integer;
begin
  if x > y then
    Result := x
  else
    Result := y;
end;

function MinI(x, y: integer): integer;
begin
  if x < y then
    Result := x
  else
    Result := y;
end;

function CompareStringsInPercent(Str1, Str2: string): Byte;
type
  TLink = array[0..1] of Byte;
var
  tmpPattern: TLink;
  PatternA, PatternB: array of TLink;
  IndexA, IndexB, LengthStr: Integer;
begin
  Result := 100;
  // Building pattern tables
  LengthStr := MaxI(Length(Str1), Length(Str2));
  for IndexA := 1 to LengthStr do 
  begin
    if Length(Str1) >= IndexA then 
    begin
      SetLength(PatternA, (Length(PatternA) + 1));
      PatternA[Length(PatternA) - 1][0] := Byte(Str1[IndexA]);
      PatternA[Length(PatternA) - 1][1] := IndexA;
    end;
    if Length(Str2) >= IndexA then 
    begin
      SetLength(PatternB, (Length(PatternB) + 1));
      PatternB[Length(PatternB) - 1][0] := Byte(Str2[IndexA]);
      PatternB[Length(PatternB) - 1][1] := IndexA;
    end;
  end;
  // Quick Sort of pattern tables
  IndexA := 0;
  IndexB := 0;
  while ((IndexA < (Length(PatternA) - 1)) and (IndexB < (Length(PatternB) - 1))) do
  begin
    if Length(PatternA) > IndexA then 
    begin
      if PatternA[IndexA][0] < PatternA[IndexA + 1][0] then 
      begin
        tmpPattern[0]           := PatternA[IndexA][0];
        tmpPattern[1]           := PatternA[IndexA][1];
        PatternA[IndexA][0]     := PatternA[IndexA + 1][0];
        PatternA[IndexA][1]     := PatternA[IndexA + 1][1];
        PatternA[IndexA + 1][0] := tmpPattern[0];
        PatternA[IndexA + 1][1] := tmpPattern[1];
        if IndexA > 0 then Dec(IndexA);
      end
      else 
        Inc(IndexA);
    end;
    if Length(PatternB) > IndexB then
    begin
      if PatternB[IndexB][0] < PatternB[IndexB + 1][0] then 
      begin
        tmpPattern[0]           := PatternB[IndexB][0];
        tmpPattern[1]           := PatternB[IndexB][1];
        PatternB[IndexB][0]     := PatternB[IndexB + 1][0];
        PatternB[IndexB][1]     := PatternB[IndexB + 1][1];
        PatternB[IndexB + 1][0] := tmpPattern[0];
        PatternB[IndexB + 1][1] := tmpPattern[1];
        if IndexB > 0 then Dec(IndexB);
      end
      else 
        Inc(IndexB);
    end;
  end;
  // Calculating simularity percentage
  LengthStr := MinI(Length(PatternA), Length(PatternB));
  for IndexA := 0 to (LengthStr - 1) do 
  begin
    if PatternA[IndexA][0] = PatternB[IndexA][0] then 
    begin
      if MaxI(PatternA[IndexA][1], PatternB[IndexA][1]) - MinI(PatternA[IndexA][1],
        PatternB[IndexA][1]) > 0 then Dec(Result,
        ((100 div LengthStr) div (MaxI(PatternA[IndexA][1], PatternB[IndexA][1]) -
          MinI(PatternA[IndexA][1], PatternB[IndexA][1]))))
      else if Result < 100 then Inc(Result);
    end
    else 
      Dec(Result, (100 div LengthStr))
  end;
  SetLength(PatternA, 0);
  SetLength(PatternB, 0);
end;

{$IFNDEF CRAWLER}
function findsimilarimagestring(const hash: THashTable; const s: string): string;
var
  check: string;
  idx: integer;
  s1, s2, s4: string;
  nlist, lst: TStringList;
  i: integer;
  value: integer;
  v1: integer;
begin
  try
    idx := IndexOfString(hash, s);
    if idx > -1 then
    begin
      Result := s;
      Exit;
    end;

    splitstring(s, s1, s2, '\');
    s1 := UpperCase(s1);
    s2 := UpperCase(s2);
    if IsNumeric(s1) then
    begin
      s4 := IntToStr(db.Colors(StrToInt(s1)).alternateid);
      if s4 <> s1 then
      begin
        check := s4 + '\' + s2;
        idx := IndexOfString(hash, s);
        if idx > -1 then
        begin
          Result := check;
          Exit;
        end;
      end;
    end;

    s4 := '';
    for i := 1 to Length(s2) do
    begin
      if s2[i] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] then
        s4 := s4 + s2[i]
      else
        break;
    end;

    if s4 = '' then
    begin
      result := s;
      exit;
    end;

    check := s1 + '\' + s4;
    if check <> s then
    begin
      nlist := TStringList.Create;
      lst := hash.list;
      for i := 0 to lst.Count - 1 do
      begin
        if Pos1(check, lst.Strings[i]) then
          nlist.Add(lst.Strings[i]);
      end;

      value := -1;
      idx := -1;
      for i := 0 to nlist.Count - 1 do
      begin
        v1 := CompareStringsInPercent(s, nlist.Strings[i]);
        if v1 > value then
        begin
          value := v1;
          idx := i;
        end;
      end;
      if value > -1 then
      begin
        Result := nlist.Strings[idx];
        nlist.Free;
        Exit;
      end;
      nlist.Free;
    end;

  // not found!
    Result := s;
  except
    Result := s;
    Exit;
  end;
end;
{$ENDIF}

procedure RemoveDuplicates(const s: TStringList);
var
  buffer: TStringList;
  cnt: Integer;
begin
  s.Sort;
  buffer := TStringList.Create;
  try
    buffer.Sorted := True;
    buffer.Duplicates := dupIgnore;
    buffer.BeginUpdate;
    for cnt := 0 to s.Count - 1 do
      buffer.Add(s[cnt]);
    buffer.EndUpdate;
    s.Assign(buffer);
  finally
    FreeandNil(buffer);
  end;
end;

procedure FreeList(var s: TStringList);
var
  i: integer;
begin
  if s = nil then
    Exit;
  for i := 0 to s.Count - 1 do
    try
      if s.Objects[i] <> nil then
      begin
        s.Objects[i].Free;
        s.Objects[i] := nil;
      end;
    except
      s.Objects[i] := nil;
    end;
  FreeAndNil(s);
end;

procedure ClearList(const s: TStringList);
var
  i: integer;
begin
  if s = nil then
    Exit;
  for i := 0 to s.Count - 1 do
    try
      if s.Objects[i] <> nil then
      begin
        s.Objects[i].Free;
        s.Objects[i] := nil;
      end;
    except
      s.Objects[i] := nil;
    end;
  s.Clear;
end;

procedure SaveBmpToJpeg(const MyBitmap: TBitmap; const JPEGFName: string);
var
  MyJPEG: TJPEGImage;
begin
  MyJPEG := TJPEGImage.Create;
  try
    MyJPEG.Assign(MyBitmap);
    MyJPEG.SaveToFile(JPEGFName);
  finally
    MyJPEG.Free;
  end;
end;

function RGBInvert(const t: TColor): TColor;
var
  r, g, b: integer;
begin
  r := GetRValue(t);
  g := GetGValue(t);
  b := GetBValue(t);
  Result := RGB(b, g, r);
end;

//------------------------------------------------------------------------------
procedure TBStringList.AppendFromFile(const fname: string);
var
  s: TStringList;
begin
  s := TStringList.Create;
  try
    S_LoadFromFile(s, fname);
    AddStrings(s);
  finally
    s.Free;
  end;
end;

{$IFNDEF CRAWLER}
function PieceToImage(const img: TImage; const piece: string; color: integer = -1): boolean;
var
  ps: TPakStream;
  path: string;
  entriesHash: THashTable;
  entries: TStringList;
  sTmp: string;
  m: TMemoryStream;
  tmp: string;
  s1, s2: string;
  sticker: boolean;
begin
  Result := False;

  sticker := issticker(piece);

  if color = -1 then
    path := 's\'
  else
    path := itoa(color) + '\';
  path := path + piece;
  if color = -1 then
    path := path + '.jpg'
  else
    path := path + '.png';

  ps := TPakStream.Create(path, pm_full);
  if ps.IOResult <> 0 then
  begin
    ps.Free;
    if sticker then
    begin
      path := '-1\' + piece + '.png';
      ps := TPakStream.Create(path, pm_full);
      if ps.IOResult <> 0 then
      begin
        ps.Free;
        path := '9999\' + piece + '.png';
        ps := TPakStream.Create(path, pm_full);
        if ps.IOResult <> 0 then
        begin
          ps.Free;
          Exit;
        end;
      end;
    end
    else
    begin
      if color = -1 then
      begin
        path := '-1\' + piece + '.png';
        ps := TPakStream.Create(path, pm_full);
        if ps.IOResult <> 0 then
        begin
          ps.Free;
          path := '9999\' + piece + '.png';
          ps := TPakStream.Create(path, pm_full);
          if ps.IOResult <> 0 then
          begin
            ps.Free;
            Exit;
          end;
        end;
      end
      else
      begin
        entriesHash := THashTable.Create;
        entries := TStringList.Create;

        PAK_GetEntries(entries);
        entries.Sort;
        entriesHash.AssignStringList(entries);

        sTmp := findsimilarimagestring(entriesHash, path);

        entries.Free;
        entriesHash.Free;

        printf('Image %s not found, retrying ... [%s]'#13#10,[path, sTmp]);
        if sTmp = path then
          Exit;
        if sTmp = '' then
          Exit;
        if sTmp[Length(sTmp)] = '\' then
          Exit;
        ps := TPakStream.Create(sTmp, pm_full);
        if ps.IOResult <> 0 then
        begin
          ps.Free;
          Exit;
        end;
      end;
    end;
  end;

  m := TMemoryStream.Create;
  try
    m.LoadFromStream(ps);
    ps.Free;

    splitstring(path, s1, s2, '\');
    tmp := I_NewTempFile(s2);
    m.SaveToFile(tmp);
    try
      img.Picture.LoadFromFile(tmp);
      Result := True;
    except
      printf('Image %s is invalid '#13#10,[path]);
    end;
    DeleteFile(tmp);
  finally
    m.Free;
  end;
end;
{$ENDIF}

function filenamestring(const s: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to length(s) do
  begin
    if IsNumericC(s[i]) then
      Result := Result + s[i]
    else if Pos(toupper(s[i]), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ-') > 0 then
      Result := Result + s[i];
  end;
end;

function DownloadFile(SourceFile, DestFile: string): Boolean;
begin
  try
    Result := UrlDownloadToFile(nil, PChar(SourceFile), PChar(DestFile), 0, nil) = 0;
  except
    Result := False;
  end;
end;

function DownloadFileImg(SourceFile, DestFile: string): Boolean;
begin
  Result := DownloadFile(SourceFile, DestFile);
  if Result then
    Result := CheckValidImageDonwload(DestFile);
end;

function NET_DownloadFileImg(SourceFile, DestFile: string): Boolean;
begin
  Result := NET_DownloadFile(SourceFile, DestFile);
  if Result then
    Result := CheckValidImageDonwload(DestFile);
end;

procedure SetClipboardTextWideString(const Text: WideString);
var
  Count: Integer;
  Handle: HGLOBAL;
  Ptr: Pointer;
begin
  Count := (Length(Text) + 1) * SizeOf(WideChar);
  Handle := GlobalAlloc(GMEM_MOVEABLE, Count);
  try
    if Handle = 0 then RaiseLastOSError;
    Ptr := GlobalLock(Handle);
    if not Assigned(Ptr) then RaiseLastOSError;
    Move(PWideChar(Text)^, Ptr^, Count);
    GlobalUnlock(Handle);
    Clipboard.SetAsHandle(CF_UNICODETEXT, Handle);
  except
    GlobalFree(Handle);
    raise;
  end;
end;

function DownloadJpgFileToPNG(SourceFile, DestFile: string): Boolean;
var
  J: TJpegImage;
  B: TBitmap;
  P: TPNGObject;
begin
  Result := DownloadFileImg(SourceFile, 'tmp1.jpg');
  if not Result then
  begin
    if fexists('tmp1.jpg') then
      DeleteFile('tmp1.jpg');
    Exit;
  end;

  Result := True;
  J := TJpegImage.Create;
  B := TBitmap.Create;
  P := TPNGObject.Create;
  try
    J.LoadFromFile('tmp1.jpg');
    B.Assign(J);
    P.Assign(B);
    P.SaveToFile(DestFile);
  except
    Result := False;
  end;
  J.Free;
  B.Free;
  P.Free;

  if fexists('tmp1.jpg') then
    DeleteFile('tmp1.jpg');
end;

function JPG2PNG(const ajpg, apng: string): Boolean;
var
  J: TJpegImage;
  B: TBitmap;
  P: TPNGObject;
begin
  Result := False;
  if not fexists(ajpg) then
    Exit;

  J := TJpegImage.Create;
  B := TBitmap.Create;
  P := TPNGObject.Create;
  try
    J.LoadFromFile(ajpg);
    B.Assign(J);
    P.Assign(B);
    P.SaveToFile(apng);
    Result := True;
  except
    Result := False;
  end;
  J.Free;
  B.Free;
  P.Free;
end;

function PNG2JPG(const apng, ajpg: string): Boolean;
var
  J: TJpegImage;
  B: TBitmap;
  P: TPNGObject;
begin
  Result := False;
  if not fexists(apng) then
    Exit;

  J := TJpegImage.Create;
  B := TBitmap.Create;
  P := TPNGObject.Create;
  try
    P.LoadFromFile(apng);
    B.Assign(J);
    J.Assign(B);
    J.SaveToFile(ajpg);
    Result := True;
  except
    Result := False;
  end;
  J.Free;
  B.Free;
  P.Free;
end;

function DownloadPngFileToJpg(SourceFile, DestFile: string): Boolean;
var
  J: TJpegImage;
  B: TBitmap;
  P: TPNGObject;
begin
  Result := DownloadFileImg(SourceFile, 'tmp1.png');
  if not Result then
  begin
    if fexists('tmp1.png') then
      DeleteFile('tmp1.png');
    Exit;
  end;

  Result := True;
  J := TJpegImage.Create;
  B := TBitmap.Create;
  P := TPNGObject.Create;
  try
    P.LoadFromFile('tmp1.png');
    B.Assign(P);
    J.Assign(B);
    J.SaveToFile(DestFile);
  except
    Result := False;
  end;
  J.Free;
  B.Free;
  P.Free;

  if fexists('tmp1.png') then
    DeleteFile('tmp1.png');
end;

function IsLikeJpeg(const fname: string): boolean;
var
  f: TFileStream;
  b: byte;
begin
  f := TFileStream.Create(fname, fmOpenRead);

  f.Position := 6;
  f.Read(b, 1);
  if b <> 74 then
  begin
    Result := False;
    f.Free;
    Exit;
  end;

  f.Position := 7;
  f.Read(b, 1);
  if b <> 70 then
  begin
    Result := False;
    f.Free;
    Exit;
  end;

  f.Position := 7;
  f.Read(b, 1);
  if b <> 70 then
  begin
    Result := False;
    f.Free;
    Exit;
  end;

  f.Position := 8;
  f.Read(b, 1);
  if b <> 73 then
  begin
    Result := False;
    f.Free;
    Exit;
  end;

  f.Position := 9;
  f.Read(b, 1);
  if b <> 70 then
  begin
    Result := False;
    f.Free;
    Exit;
  end;

  Result := True;
  f.Free;

end;

function DownloadGIFFileToPNG(SourceFile, DestFile: string): Boolean;
var
  J: TGIFImage;
  B: TBitmap;
  P: TPNGObject;
begin
  Result := DownloadFileImg(SourceFile, 'tmp1.gif');
  if not Result then
  begin
    if fexists('tmp1.gif') then
      DeleteFile('tmp1.gif');
    Exit;
  end;

  if IsLikeJpeg('tmp1.gif') then
  begin
    Result := DownloadJpgFileToPNG(SourceFile, DestFile);
  end
  else
  begin
    Result := True;
    J := TGIFImage.Create;
    B := TBitmap.Create;
    P := TPNGObject.Create;
    try
      J.LoadFromFile('tmp1.gif');
      B.Assign(J);
      P.Assign(B);
      P.SaveToFile(DestFile);
    except
      Result := False;
    end;
    J.Free;
    B.Free;
    P.Free;
  end;

  if fexists('tmp1.gif') then
    DeleteFile('tmp1.gif');
end;

function GetFileCreationTime(sFileName: string): TDateTime;
var
  ffd: TWin32FindData;
  dft: DWORD;
  lft: TFileTime;
  h:   THandle;
begin
  //
  // get file information
  h := Windows.FindFirstFile(PChar(sFileName), ffd);
  if (INVALID_HANDLE_VALUE <> h) then
  begin
    //
    // we're looking for just one file,
    // so close our "find"
    Windows.FindClose(h);
    //
    // convert the FILETIME to
    // local FILETIME
    FileTimeToLocalFileTime(ffd.ftCreationTime, lft);
    //
    // convert FILETIME to
    // DOS time
    FileTimeToDosDateTime(lft, LongRec(dft).Hi, LongRec(dft).Lo);
    //
    // finally, convert DOS time to
    // TDateTime for use in Delphi's
    // native date/time functions
    Result := FileDateToDateTime(dft);
  end
  else
    Result := Now;
end;

function GetPrevAlphanumeric(const s: string): string;
var
  i, j, len: integer;
  c: char;
  n: integer;
  ret1a, ret1b, ret2: string;
begin
  Result := s;
  i := length(Result);
  len := i;
  while i > 0 do
  begin
    c := Result[i];
    if IsNumericC(c) then
    begin
      n := atoi(c);
      if n = 0 then
      begin
        ret1a := '';
        for j := 1 to i - 1 do
          ret1a := ret1a + Result[j];
        ret2 := '';
        for j := i + 1 to len do
          ret2 := ret2 + Result[j];
        ret1b := GetPrevAlphanumeric(ret1a);
        if ret1b <> ret1a then
        begin
          Result := ret1b + '9' + ret2;
          Exit;
        end
        else
          Exit;
      end
      else
      begin
        Result[i] := itoa(n - 1)[1];
        Exit;
      end;
    end;
    dec(i);
  end;
end;

function GetNextAlphanumeric(const s: string): string;
var
  i, j, len: integer;
  c: char;
  n: integer;
  ret1a, ret1b, ret2: string;
begin
  Result := s;
  i := length(Result);
  len := i;
  while i > 0 do
  begin
    c := Result[i];
    if IsNumericC(c) then
    begin
      n := atoi(c);
      if n = 9 then
      begin
        ret1a := '';
        for j := 1 to i - 1 do
          ret1a := ret1a + Result[j];
        ret2 := '';
        for j := i + 1 to len do
          ret2 := ret2 + Result[j];
        ret1b := GetNextAlphanumeric(ret1a);
        if ret1b <> ret1a then
        begin
          Result := ret1b + '0' + ret2;
          Exit;
        end
        else
          Exit;
      end
      else
      begin
        Result[i] := itoa(n + 1)[1];
        Exit;
      end;
    end;
    dec(i);
  end;
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

function InputQuery2(const ACaption, APrompt, APrompt2: string;
  var Value, Value2: string): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Prompt2: TLabel;
  Edit: TEdit;
  Edit2: TEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin         
  Result := False;
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      Position := poScreenCenter;

      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Caption := APrompt;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
        WordWrap := True;
      end;
      Edit := TEdit.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := Prompt.Top + Prompt.Height + 5;
        Width := MulDiv(164, DialogUnits.X, 4);
        MaxLength := 255;
        Text := Value;
        SelectAll;
      end;

      Prompt2 := TLabel.Create(Form);
      with Prompt2 do
      begin
        Parent := Form;
        Caption := APrompt2;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8) + Edit.Top + Edit.Height + 8;
        Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
        WordWrap := True;
      end;

      Edit2 := TEdit.Create(Form);
      with Edit2 do
      begin
        Parent := Form;
        Left := Prompt2.Left;
        Top := Prompt2.Top + Prompt2.Height + 5;
        Width := MulDiv(164, DialogUnits.X, 4);
        MaxLength := 255;
        Text := Value2;
        SelectAll;
      end;

      ButtonTop := Edit2.Top + Edit2.Height + 15;
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := 'OK';
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := 'Cancel';
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), Edit2.Top + Edit2.Height + 15,
          ButtonWidth, ButtonHeight);
        Form.ClientHeight := Top + Height + 13;
      end;
      if ShowModal = mrOk then
      begin
        Value := Edit.Text;
        Value2 := Edit2.Text;
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;

function InputListQuery(const ACaption, APrompt: string; const AListItems: string;
  var Value: string): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Combo: TComboBox;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Caption := APrompt;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
        WordWrap := True;
      end;
      Combo := TComboBox.Create(Form);
      with Combo do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := Prompt.Top + Prompt.Height + 5;
        Width := MulDiv(164, DialogUnits.X, 4);
        MaxLength := 255;
        Text := Value;
        Items.Text := AListItems;
        SelectAll;
      end;
      ButtonTop := Combo.Top + Combo.Height + 15;
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), Combo.Top + Combo.Height + 15,
          ButtonWidth, ButtonHeight);
        Form.ClientHeight := Top + Height + 13;
      end;
      if ShowModal = mrOk then
      begin
        Value := Combo.Text;
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;

function InputInteger(const ACaption, APrompt: string;
  var Value: integer): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      Position := poScreenCenter;

      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Caption := APrompt;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
        WordWrap := True;
      end;
      Edit := TEdit.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := Prompt.Top + Prompt.Height + 5;
        Width := MulDiv(164, DialogUnits.X, 4);
        MaxLength := 255;
        Text := itoa(Value);
        SelectAll;
      end;

      ButtonTop := Edit.Top + Edit.Height + 15;
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := 'OK';
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := 'Cancel';
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), Edit.Top + Edit.Height + 15,
          ButtonWidth, ButtonHeight);
        Form.ClientHeight := Top + Height + 13;
      end;
      if ShowModal = mrOk then
      begin
        Value := atoi(Edit.Text, -1);
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;


function InputTwoIntegers(const ACaption, APrompt, APrompt2: string;
  var Value, Value2: integer): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Prompt2: TLabel;
  Edit: TEdit;
  Edit2: TEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      Position := poScreenCenter;

      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Caption := APrompt;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
        WordWrap := True;
      end;
      Edit := TEdit.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := Prompt.Top + Prompt.Height + 5;
        Width := MulDiv(164, DialogUnits.X, 4);
        MaxLength := 255;
        Text := itoa(Value);
        SelectAll;
      end;

      Prompt2 := TLabel.Create(Form);
      with Prompt2 do
      begin
        Parent := Form;
        Caption := APrompt2;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8) + Edit.Top + Edit.Height + 8;
        Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
        WordWrap := True;
      end;

      Edit2 := TEdit.Create(Form);
      with Edit2 do
      begin
        Parent := Form;
        Left := Prompt2.Left;
        Top := Prompt2.Top + Prompt2.Height + 5;
        Width := MulDiv(164, DialogUnits.X, 4);
        MaxLength := 255;
        Text := itoa(Value2);
        SelectAll;
      end;

      ButtonTop := Edit2.Top + Edit2.Height + 15;
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := 'OK';
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := 'Cancel';
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), Edit2.Top + Edit2.Height + 15,
          ButtonWidth, ButtonHeight);
        Form.ClientHeight := Top + Height + 13;
      end;
      if ShowModal = mrOk then
      begin
        Value := atoi(Edit.Text, -1);
        Value2 := atoi(Edit2.Text, -1);
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;

function CheckValidImageDonwload(const src: string): boolean;
var
  f: TFileStream;
  b: Byte;
begin
  Result := False;
  if not fexists(src) then
    Exit;
  f := TFileStream.Create(src, fmOpenRead or fmShareDenyWrite);
  if f.Size = 0 then
  begin
    f.Free;
    fdelete(src);
    Exit;
  end;
  f.Read(b, 1);
  if b = Ord('<') then
  begin
    f.Free;
    fdelete(src);
    Exit;
  end;
  if b = 9 then
  begin
    f.Free;
    fdelete(src);
    Exit;
  end;
  if b = Ord(' ') then
  begin
    f.Free;
    fdelete(src);
    Exit;
  end;
  if b in [10, 13] then
  begin
    f.Free;
    fdelete(src);
    Exit;
  end;
  f.free;
  Result := True;
end;

function CountChars(const S: string; const C: char): integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    if S[i] = C then inc(Result);
end;

function RemoveHTMLTags(S: string): string;
var
  TagBegin, TagEnd, TagLength: integer;
begin
  TagBegin := CharPos( '<', S); // search position of first <

  while (TagBegin > 0) do
  begin  // while there is a < in S
    TagEnd := CharPos('>', S);              // find the matching >
    if TagEnd <= 0 then
      Break;
    TagLength := TagEnd - TagBegin + 1;
    Delete(S, TagBegin, TagLength);     // delete the tag
    TagBegin:= CharPos( '<', S);            // search for next <
  end;

  Result := S;                   // give the Result
end;

function LastPos(SubStr, S: string): Integer;
var
  Found, Len, Pos: integer;
begin
  Pos := Length(S);
  Len := Length(SubStr);
  Found := 0;
  while (Pos > 0) and (Found = 0) do
  begin
    if Copy(S, Pos, Len) = SubStr then
      Found := Pos;
    Dec(Pos);
  end;
  LastPos := Found;
end;

function LeftPad(PadString: string; HowMany: integer; PadValue: string): string;
var
   Counter : integer;
   x : integer;
   NewString : string;
begin
   Counter := HowMany - Length(PadString);
   for x := 1 to Counter do
   begin
      NewString := NewString + PadValue;
   end;
   Result := NewString + PadString;
end;

function ReplaceWhiteSpace(const s: string; const c: char; const strictMode: boolean): string;
var
  i: integer;
  InvalidChars : set of char;
begin
  Result := s;
  InvalidChars := [#3..#10,#13];
  if strictMode then
    InvalidChars := InvalidChars +[#32];
  for i := 1 to Length(Result) do
    if Result[i] in InvalidChars then
      Result[i] := c;
end;

function RemoveChars(const str: string; numericOnly: boolean): string;
const
  AlphaChars : set of char =
    ['a'..'z','A'..'Z'];
var
  i, Count: Integer;
  InvalidChars : set of char;
begin
  InvalidChars := eSpecialChars;
  if numericOnly then
    InvalidChars := InvalidChars + AlphaChars + [' '] - [DecimalSeparator];

  SetLength(Result, Length(str));
  Count := 0;
  for i := 1 to Length(str) do
    if not (str[i] in InvalidChars) then
    begin
      inc(Count);
      Result[Count] := str[i];
    end;
  SetLength(Result, Count);
end;

function AbsoluteURI(const template, filename, color: string): string;
var
  ident: string;
  pp: integer;
begin
  ident:= ExtractFileName(filename);
  pp := CharPos('.',ident);
  if pp > 0 then
    ident := Copy(ident, 0, pp - 1);

  Result := StringReplace(template, '%file%', filename, [rfReplaceAll]);
  Result := StringReplace(Result, '%color%', color, [rfReplaceAll]);
  Result := StringReplace(Result, '%id%', ident, [rfReplaceAll]);
end;

function GetHost(s: string): string;
var
  l, n: integer;
begin
  n := Pos('//', s);
  l := PosEx('/', s,n + 2);
  Result := Copy(s, 0, l);
end;

function LDCadToCSV(const filename: string): string;
var
  str: TStringList;
  i: integer;
  line, part, color, npart, unused: string;
begin
  str := TStringList.Create;
  Result := '';
  try
    str.LoadFromFile(filename);
    for i := 0 to str.Count - 1 do
    begin
      line := ReplaceWhiteSpace(Trim(str[i]), '_', True);
      if CharPos(':', line) <> 0 then
      begin
        splitstring(line, part, line, ':');
        splitstring(line, unused, color, npart, '_');
        color := Copy(color, CharPos('=', color) + 1, Length(color) - CharPos('=', color) - 1);
        npart := Copy(npart, CharPos('=', npart) + 1, Length(npart) - CharPos('=', npart) - 1);

        Result := Format('%s%s, %s, %s%s',[Result, part, color, npart, sLineBreak]);
      end;
    end;
  finally
    str.Free;
  end;
end;

function LDrawToCSV(const filename: string): string;
var
  str, ctx, inv: TStrings;
  color, part, data, extension: string;
  i, n, l: integer;
begin
  Result := '';
  str := TStringList.Create;  // single line in file
  ctx := TStringList.Create;  // content of file
  inv := TStringList.Create;  // inventory part,color:count

  if fexists(filename) then
  try
    ctx.LoadFromFile(filename);
    inv.NameValueSeparator := ',';
    for i := 0 to ctx.Count - 1 do
    begin
      ctx[i] := ReplaceWhiteSpace(ctx[i], #32, False);
      str.Clear;
      ExtractStrings([#32], [], PChar(ctx[i]), str);

      if str.Count > 2 then
      begin
        color := str[1];
        part := TrimRight(str[str.Count - 1]);
        extension := ExtractFileExt(part);
        if (str[0] = '1') and (extension = '.dat') then
        begin
          part := Copy(part, 1, Length(part) - Length(extension));
          data := part + inv.NameValueSeparator + color;
          n := inv.IndexOf(data);
          if n = -1 then
            inv.AddObject(data, TObject(1))
          else
          begin
            l := integer(inv.Objects[n]) + 1;
            inv.Objects[n] := TObject(l);
          end;
        end;
      end;
    end;
    for i := 0 to inv.Count - 1 do
      Result := Format('%s%s, %d%s', [Result ,inv.Strings[i] ,integer(inv.Objects[i]), sLineBreak]);
  finally
    FreeAndNil(ctx);
    FreeAndNil(str);
    FreeAndNil(inv);
  end;
end;

function RemoveSpecialTagsFromString(const s: string): string;
var
  tmp1, tmp2: string;
begin
  tmp2 := StringReplace(s, 'â€', '"', [rfReplaceAll, rfIgnoreCase]);
  tmp1 := StringReplace(tmp2, '&nbsp;', ' ', [rfReplaceAll, rfIgnoreCase]);
  tmp2 := StringReplace(tmp1, '&#40;', '(', [rfReplaceAll, rfIgnoreCase]);
  tmp1 := StringReplace(tmp2, '&#41;', ')', [rfReplaceAll, rfIgnoreCase]);
  tmp2 := StringReplace(tmp1, '&#39;', '''', [rfReplaceAll, rfIgnoreCase]);
  tmp1 := StringReplace(tmp2, Chr(160), '', [rfReplaceAll, rfIgnoreCase]);
  tmp2 := StringReplace(tmp1, '&amp;', '&', [rfReplaceAll, rfIgnoreCase]);
  Result := tmp2;
end;

function ResizeJpg2Png(const fin, fout: string; const dograyscale: boolean = True;
  const maxWidth: integer = 64; const maxHeight: integer = 64): boolean;
var
  J: TJpegImage;
  B: TBitmap;
  P: TPNGObject;
  thumbRect: TRect;
  adir: string;
  sinfile: string;
  ps: TPAKStream;
  loadok: boolean;
begin
  Result := False;

  sinfile := fin;
  if not fexists(sinfile) then
  begin
    ps := TPAKStream.Create(sinfile, pm_full);
    if ps.IOResult = 0 then
    begin
      sinfile := I_NewTempFile('ResizePng2Png.jpg');
      ps.CopyToFile(sinfile);
    end;
    ps.Free;
  end;

  if not fexists(sinfile) then
    Exit;

{  if not IsLikeJpeg(fin) then
    Exit;}

  J := TJpegImage.Create;
  B := TBitmap.Create;
  P := TPNGObject.Create;
  try
    loadok := True;
    try
      J.LoadFromFile(sinfile);
    except
      loadok := False;
    end;
    if loadok then
    begin
      if (J.Width > 0) and (J.Height > 0) then
      begin
        thumbRect.Left := 0;
        thumbRect.Top := 0;
        if J.Width > J.Height then
        begin
          thumbRect.Right := maxWidth;
          thumbRect.Bottom := (maxWidth * J.Height) div J.Width;
        end
        else
        begin
          thumbRect.Bottom := maxHeight;
          thumbRect.Right := (maxHeight * J.Width) div J.Height;
        end;
        B.Width := thumbRect.Right;
        B.Height := thumbRect.Bottom;
        B.PixelFormat := pf32Bit;
        B.Canvas.StretchDraw(thumbRect, J);
        if dograyscale then
          MakeGrayScaleBitmap(B);

        P.Assign(B);

        adir := Trim(ExtractFilePath(fout));
        if adir <> '' then
          if not DirectoryExists(adir) then
            ForceDirectories(adir);

        P.SaveToFile(fout);
        Result := fexists(fout);
      end;
    end;
  finally
    J.Free;
    B.Free;
    P.Free;
  end;
end;

function ResizePng2Png(const fin, fout: string; const dograyscale: boolean = True;
  const maxWidth: integer = 64; const maxHeight: integer = 64): boolean;
var
  J: TPNGObject;
  B: TBitmap;
  P: TPNGObject;
  thumbRect: TRect;
  adir: string;
  sinfile: string;
  ps: TPAKStream;
  loadok: boolean;
begin
  Result := False;

  sinfile := fin;
  if not fexists(sinfile) then
  begin
    ps := TPAKStream.Create(sinfile, pm_full);
    if ps.IOResult = 0 then
    begin
      sinfile := I_NewTempFile('ResizePng2Png.png');
      ps.CopyToFile(sinfile);
    end;
    ps.Free;
  end;

  if not fexists(sinfile) then
    Exit;

  J := TPNGObject.Create;
  B := TBitmap.Create;
  P := TPNGObject.Create;
  try
    loadok := True;
    try
      J.LoadFromFile(sinfile);
    except
      loadok := False;
    end;
    if loadok then
    begin
      if (J.Width > 0) and (J.Height > 0) then
      begin
        thumbRect.Left := 0;
        thumbRect.Top := 0;
        if J.Width > J.Height then
        begin
          thumbRect.Right := maxWidth;
          thumbRect.Bottom := (maxWidth * J.Height) div J.Width;
        end
        else
        begin
          thumbRect.Bottom := maxHeight;
          thumbRect.Right := (maxHeight * J.Width) div J.Height;
        end;
        B.Width := thumbRect.Right;
        B.Height := thumbRect.Bottom;
        B.PixelFormat := pf32Bit;
        B.Canvas.StretchDraw(thumbRect, J);
        if dograyscale then
          MakeGrayScaleBitmap(B);

        P.Assign(B);

        adir := Trim(ExtractFilePath(fout));
        if adir <> '' then
          if not DirectoryExists(adir) then
            ForceDirectories(adir);

        P.SaveToFile(fout);
        Result := fexists(fout);
      end;
    end;
  finally
    J.Free;
    B.Free;
    P.Free;
  end;
end;

procedure MakeGrayScaleBitmap(const ABitmap: TBitmap);
type
  PPixelRec = ^TPixelRec;
  TPixelRec = packed record
    B: Byte;
    G: Byte;
    R: Byte;
  end;
var
  X: Integer;
  Y: Integer;
  Gray: Byte;
  Pixel: PPixelRec;
begin
  ABitmap.PixelFormat := pf24bit;
  for Y := 0 to ABitmap.Height - 1 do
  begin
    Pixel := ABitmap.ScanLine[Y];
    for X := 0 to ABitmap.Width - 1 do
    begin
      Gray := Round((0.2989 * Pixel.R) + (0.587 * Pixel.G) + (0.1141 * Pixel.B));
      Pixel.R := Gray;
      Pixel.G := Gray;
      Pixel.B := Gray;
      Inc(Pixel);
    end;
  end;
  ABitmap.PixelFormat := pf32bit;
end;

constructor TCounterObject.Create(const avalue: LongWord = 0);
begin
  Inherited Create;
  fvalue := avalue;
end;

procedure TCounterObject.IncL;
begin
  inc(fvalue);
end;

procedure TCounterObject.DecL;
begin
  dec(fvalue);
end;

function htmlstripstring(const htm: string): string;
var
  i: integer;
  inbracket: boolean;
begin
  Result := '';
  inbracket := False;
  for i := 1 to Length(htm) do
  begin
    if htm[i] = '<' then
      inbracket := True
    else if htm[i] = '>' then
      inbracket := False
    else if not inbracket then
      Result := Result + htm[i];
  end;
end;

constructor TRecordInfo.Create(const apos: Int64);
begin
  position := apos;
end;

function IsValidDBMask(const msk: string): boolean;
var
  p: integer;
  cnt1, cnt2: integer;
begin
  p := CharPos('?', msk);
  if p <= 0 then
    p := CharPos('*', msk);
  if p <= 0 then
  begin
    Result := False;
    Exit;
  end;

  cnt1 := 0;
  cnt2 := 0;
  for p := 1 to Length(msk) do
  begin
    if msk[p] <> ' ' then
    begin
      if msk[p] in ['?' ,'*'] then
        inc(cnt1);
      inc(cnt2);
    end;
  end;

  Result := cnt2 > cnt1;
end;

procedure RemoveBlancLines(const lst: TStringList);
var
  i: integer;
begin
  if lst = nil then
    Exit;

  for i := lst.Count - 1 downto 0 do
    if Trim(lst.Strings[i]) = '' then
      lst.Delete(i);
end;

procedure backupfile(const fn: string);
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

function ftoadot(f: double): string;
var
  i: integer;
begin
  Result := FloatToStr(f);
  for i := 1 to Length(Result) do
    if Result[i] = ',' then
    begin
      Result[i] := '.';
      Exit;
    end;
end;

procedure S_FirstLine(const sl: TStringList; const line: string);
begin
  if sl.Count = 0 then
    sl.Add(line)
  else if sl.Strings[0] <> line then
    sl.Insert(0, line);
end;

function FindListLeft(const lst: TStringList; S: string; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
  test: string;
  len: integer;
begin
  Result := False;
  L := 0;
  len := Length(S);
  H := lst.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    test := LeftStr(lst.strings[i], len);
    C := AnsiCompareText(test, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if lst.Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function SortListAndFindFirstLeftMatch(const l: TStringList; const s: string): integer;
begin
  l.sorted := True;
  if not FindListLeft(l, s, result) then
    result := -1;
end;

function GetMemoryUsed: LongWord;
type
  PSmallBlockTypeState = ^TSmallBlockTypeState;
var
  st: TMemoryManagerState;
  sb: PSmallBlockTypeState;
  i: integer;
begin
  GetMemoryManagerState(st);
  result :=  st.TotalAllocatedMediumBlockSize
           + st.TotalAllocatedLargeBlockSize;
  for i := 0 to NumSmallBlockTypes - 1 do
  begin
    sb := @st.SmallBlockTypeStates[i];
    result := result + sb.UseableBlockSize * sb.AllocatedBlockCount;
  end;
end;

type
  SDSTS_data_t = record
    start, stop: integer;
    lst: TStringList;
  end;
  SDSTS_data_p = ^SDSTS_data_t;

function SetDoubleStringsToSpace_thr(p: pointer): integer; stdcall;
var
  parms: SDSTS_data_p;
  i: integer;
  lst: TStringList;
begin
  parms := SDSTS_data_p(p);
  lst := parms.lst;
  for i := parms.start downto parms.stop do
    if lst.Strings[i] = lst.Strings[i - 1] then
      lst.Strings[i] := '';
  Result := 1;
end;

procedure SetDoubleStringsToSpace(const lst: TStringList);
const
  INDEXES = '1234567890abcdefghijklmnopqrstuvwxyz';
var
  i, j, cnt: integer;
  parms: array[0..7] of SDSTS_data_t;
  lists: array[0..Length(INDEXES)] of TStringList;
  s: string;
begin
//  lst.Sort;
  cnt := lst.Count;
  if usemultithread and (cnt > 1024) then
  begin
    for i := 0 to Length(INDEXES) do
      lists[i] := TStringList.Create;

    for i := 0 to cnt - 1 do
    begin
      s := lst.Strings[i];
      if s <> '' then
      begin
        j := CharPos(tolower(s[1]), INDEXES);
        lists[j].Add(s);
      end;
    end;

    for i := 0 to Length(INDEXES) do
      lists[i].Sort;

    for j := 0 to Length(INDEXES) do
    begin
      cnt := lists[j].Count;
      if cnt > 128 then
      begin
        for i := 0 to 7 do
        begin
          parms[i].lst := lists[j];
          parms[i].start := cnt - 1 - i * (cnt div 8);
        end;
        for i := 0 to 6 do
          parms[i].stop := parms[i + 1].start;
        parms[7].stop := 1;
        MT_Execute(
          @SetDoubleStringsToSpace_thr, @parms[0],
          @SetDoubleStringsToSpace_thr, @parms[1],
          @SetDoubleStringsToSpace_thr, @parms[2],
          @SetDoubleStringsToSpace_thr, @parms[3],
          @SetDoubleStringsToSpace_thr, @parms[4],
          @SetDoubleStringsToSpace_thr, @parms[5],
          @SetDoubleStringsToSpace_thr, @parms[6],
          @SetDoubleStringsToSpace_thr, @parms[7]
        );
      end
      else
      begin
        parms[0].start := cnt - 1;
        parms[0].stop := 1;
        parms[0].lst := lists[j];
        SetDoubleStringsToSpace_thr(@parms[0]);
      end;
    end;
    lst.Clear;
    for i := 0 to Length(INDEXES) do
    begin
      lst.AddStrings(lists[i]);
      lists[i].Free;
    end;
  end
  else
  begin
    lst.Sort;
    parms[0].start := cnt - 1;
    parms[0].stop := 1;
    parms[0].lst := lst;
    SetDoubleStringsToSpace_thr(@parms[0]);
  end;
end;

const
  FLG_LST_SORTED = 1;
  FLG_LST_CASESENSITIVE = 2;

constructor TStringListContainer.Create;
begin
  Inherited Create;
  flist := nil;
  fflags := FLG_LST_CASESENSITIVE;
end;

destructor TStringListContainer.Destroy;
begin
  if flist <> nil then
  begin
    flist.Free;
    flist := nil;
  end;
  Inherited Destroy;
end;

procedure TStringListContainer.CreateList;
begin
  if flist <> nil then
    Exit;

  flist := TStringList.Create;
  flist.CaseSensitive := fflags and FLG_LST_CASESENSITIVE <> 0;
  flist.Sorted := fflags and FLG_LST_SORTED <> 0;
end;

function TStringListContainer.GetSorted: Boolean;
begin
  Result := fflags and FLG_LST_SORTED <> 0;
end;

procedure TStringListContainer.SetSorted(Value: Boolean);
begin
  if Value <> (fflags and FLG_LST_SORTED <> 0) then
  begin
    if Value then
      fflags := fflags or FLG_LST_SORTED
    else
      fflags := fflags and not FLG_LST_SORTED;
    if flist <> nil then
      flist.Sorted := Value;
  end;
end;

function TStringListContainer.GetCaseSensitive: Boolean;
begin
  Result := fflags and FLG_LST_CASESENSITIVE <> 0;
end;

procedure TStringListContainer.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> (fflags and FLG_LST_CASESENSITIVE <> 0) then
  begin
    if Value then
      fflags := fflags or FLG_LST_CASESENSITIVE
    else
      fflags := fflags and not FLG_LST_CASESENSITIVE;
    if flist <> nil then
      flist.CaseSensitive := Value;
  end;
end;


function TStringListContainer.GetCount: integer;
begin
  if flist = nil then
    Result := 0
  else
    Result := flist.Count;
end;

function TStringListContainer.Get(Index: Integer): string;
begin
  if flist = nil then
    Result := ''
  else
    Result := flist.Strings[Index];
end;

procedure TStringListContainer.Put(Index: Integer; const S: string);
begin
  CreateList;
  flist.Strings[Index] := S;
end;

function TStringListContainer.GetObject(Index: Integer): TObject;
begin
  if flist = nil then
    Result := nil
  else
    Result := flist.Objects[Index];
end;

procedure TStringListContainer.PutObject(Index: Integer; AObject: TObject);
begin
  CreateList;
  flist.Objects[Index] := AObject;
end;

function TStringListContainer.GetTextStr: string;
begin
  if flist = nil then
    Result := ''
  else
    Result := flist.Text;
end;

procedure TStringListContainer.SetTextStr(const Value: string);
begin
  if Value = '' then
  begin
    if flist <> nil then
    begin
      flist.Free;
      flist := nil;
    end;
    Exit;
  end;
  CreateList;
  flist.Text := Value;
end;

function TStringListContainer.Add(const S: string): Integer;
begin
  CreateList;
  Result := flist.Add(S);
end;

function TStringListContainer.AddObject(const S: string; AObject: TObject): Integer;
begin
  CreateList;
  Result := flist.AddObject(S, AObject);
end;

procedure TStringListContainer.Clear;
begin
  if flist <> nil then
  begin
    flist.Free;
    flist := nil;
  end;
  Exit;
end;

procedure TStringListContainer.Delete(Index: Integer);
begin
  if flist = nil then
    Exit;
  flist.Delete(Index);
  if flist.Count = 0 then
  begin
    flist.Free;
    flist := nil;
  end;
end;

procedure TStringListContainer.Exchange(Index1, Index2: Integer);
begin
  if flist <> nil then
    flist.Exchange(Index1, Index2);
end;

function TStringListContainer.IndexOf(const S: string): Integer;
begin
  if flist = nil then
    Result := -1
  else
    Result := flist.IndexOf(S);
end;

procedure TStringListContainer.Insert(Index: Integer; const S: string);
begin
  CreateList;
  flist.Insert(Index, S);
end;

procedure TStringListContainer.InsertObject(Index: Integer; const S: string;
  AObject: TObject);
begin
  CreateList;
  flist.InsertObject(Index, S, AObject);
end;

procedure TStringListContainer.Sort;
begin
  if flist = nil then
    Exit;
  flist.Sort;
end;

procedure TStringListContainer.CustomSort(Compare: TStringListSortCompare);
begin
  if flist = nil then
    Exit;
  flist.CustomSort(Compare);
end;

procedure TStringListContainer.LoadFromFile(const FileName: string);
begin
  CreateList;
  flist.LoadFromFile(FileName);
  if flist.Count = 0 then
  begin
    flist.Free;
    flist := nil;
  end;
end;

procedure TStringListContainer.LoadFromStream(Stream: TStream);
begin
  CreateList;
  flist.LoadFromStream(Stream);
  if flist.Count = 0 then
  begin
    flist.Free;
    flist := nil;
  end;
end;

procedure TStringListContainer.SaveToFile(const FileName: string);
begin
  CreateList;
  flist.SaveToFile(FileName);
  if flist.Count = 0 then
  begin
    flist.Free;
    flist := nil;
  end;
end;

procedure TStringListContainer.SaveToStream(Stream: TStream);
begin
  CreateList;
  flist.SaveToStream(Stream);
  if flist.Count = 0 then
  begin
    flist.Free;
    flist := nil;
  end;
end;

procedure FreeListContainer(var s: TStringListContainer);
var
  i: integer;
begin
  if s = nil then
    Exit;
  for i := 0 to s.Count - 1 do
    try
      if s.Objects[i] <> nil then
      begin
        s.Objects[i].Free;
        s.Objects[i] := nil;
      end;
    except
      s.Objects[i] := nil;
    end;
  FreeAndNil(s);
  s := nil;
end;

end.

