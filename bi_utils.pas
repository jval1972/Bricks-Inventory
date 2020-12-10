unit bi_utils;

interface

uses
  Windows, SysUtils, Classes, bi_hash, Graphics, ExtCtrls;

function findsimilarimagestring(const hash: THashTable; const s: string): string;

function IndexOfString(const hash: THashTable; const s: string): integer;

procedure RemoveDuplicates(const s: TStringList);

procedure FreeList(var s: TStringList);

procedure SaveBmpToJpeg(const MyBitmap : TBitmap; const JPEGFName: string);

function RGBInvert(const t: TColor): TColor;

procedure PieceToImage(const img: TImage; const piece: string; color: integer = -1);

function filenamestring(const s: string): string;

type
  TBStringList = class(TStringList)
  public
    procedure AppendFromFile(const fname: string);
  end;

procedure backupfile(const fname: string);

function DownloadFile(SourceFile, DestFile: string): Boolean;

function DownloadJpgFileToPNG(SourceFile, DestFile: string): Boolean;

function DownloadGIFFileToPNG(SourceFile, DestFile: string): Boolean;

procedure SetClipboardTextWideString(const Text: WideString);

function GetFileCreationTime(sFileName: string): TDateTime;

function GetPrevAlphanumeric(const s: string): string;
function GetNextAlphanumeric(const s: string): string;

implementation

uses
  bi_delphi, bi_db, jpeg, bi_pak, bi_tmp, URLMon, Clipbrd, pngimage, GIFImage;

function IndexOfString(const hash: THashTable; const s: string): integer;
begin
  result := hash.GetPos(s);
  if result = -1 then
  begin
    result := hash.List.IndexOf(s);
    exit;
  end;
  if hash.List.Strings[result] = s then
    exit;
  result := hash.List.IndexOf(s);
end;

function MaxI(x, y: integer): integer;
begin
  if x > y then
    result := x 
  else
    result := y;
end;

function MinI(x, y: integer): integer;
begin
  if x < y then
    result := x 
  else
    result := y;
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

function findsimilarimagestring(const hash: THashTable; const s: string): string;
var
  check: string;
  idx: integer;
  s1, s2, s4: string;
  nlist: TStringList;
  i: integer;
  value: integer;
  v1: integer;
begin
  idx := IndexOfString(hash, s);
  if idx > -1 then
  begin
    result := s;
    exit;
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
        result := check;
        exit;
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
  check := s1 + '\' + s4;
  if check <> s then
  begin
    nlist := TStringList.Create;
    for i := 0 to hash.list.Count - 1 do
      if Pos(check, hash.list.Strings[i]) = 1 then
        nlist.Add(hash.list.Strings[i]);

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
      result := nlist.Strings[idx];
      nlist.Free;
      exit;
    end;
    nlist.Free;
  end;

// not found!
  result := s;
end;

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
  for i := 0 to s.Count - 1 do
    s.Objects[i].Free;
  s.Free;
  s := nil;
end;

procedure SaveBmpToJpeg(const MyBitmap : TBitmap; const JPEGFName: string);
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
  result := RGB(b, g, r);
end;

//------------------------------------------------------------------------------
procedure TBStringList.AppendFromFile(const fname: string);
var
  s: TStringList;
begin
  s := TStringList.Create;
  try
    s.LoadFromFile(fname);
  finally
    self.AddStrings(s);
    s.Free;
  end;
end;

procedure PieceToImage(const img: TImage; const piece: string; color: integer = -1);
var
  ps: TPakStream;
  path: string;
  entriesHash: THashTable;
  entries: TStringList;
  sTmp: string;
  m: TMemoryStream;
  tmp: string;
  s1, s2: string;
begin
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
      exit;
    if sTmp = '' then
      exit;
    if sTmp[Length(sTmp)] = '\' then
      exit;
    ps := TPakStream.Create(sTmp, pm_full);
    if ps.IOResult <> 0 then
    begin
      ps.Free;
      exit;
    end;
  end;

  m := TMemoryStream.Create;
  m.LoadFromStream(ps);
  ps.Free;

  splitstring(path, s1, s2, '\');
  tmp := I_NewTempFile(s2);
  m.SaveToFile(tmp);
  m.Free;
  img.Picture.LoadFromFile(tmp);

end;

function filenamestring(const s: string): string;
var
  i: integer;
begin
  result := '';
  for i := 1 to length(s) do
  begin
    if IsNumericC(s[i]) then
      result := result + s[i]
    else if Pos(toupper(s[i]), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ-') > 0 then
      result := result + s[i];
  end;
end;

procedure backupfile(const fname: string);
var
  fbck: string;
begin
  if not fexists(fname) then
    exit;
  fbck := fname + '_' + FormatDateTime('yyyymmdd', Now);
  CopyFile(fname, fbck);
end;

function DownloadFile(SourceFile, DestFile: string): Boolean;
begin
  try
    Result := UrlDownloadToFile(nil, PChar(SourceFile), PChar(DestFile), 0, nil) = 0;
  except
    Result := False;
  end;
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
  result := DownloadFile(SourceFile, 'tmp1.jpg');
  if not result then
  begin
    if FileExists('tmp1.jpg') then
      DeleteFile('tmp1.jpg');
    exit;
  end;

  result := true;
  J := TJpegImage.Create;
  B := TBitmap.Create;
  P := TPNGObject.Create;
  try
    J.LoadFromFile('tmp1.jpg');
    B.Assign(J);
    P.Assign(B);
    P.SaveToFile(DestFile);
  except
    result := false;
  end;
  J.Free;
  B.Free;
  P.Free;

  if FileExists('tmp1.jpg') then
    DeleteFile('tmp1.jpg');
end;

function DownloadGIFFileToPNG(SourceFile, DestFile: string): Boolean;
var
  J: TGIFImage;
  B: TBitmap;
  P: TPNGObject;
begin
  result := DownloadFile(SourceFile, 'tmp1.gif');
  if not result then
  begin
    if FileExists('tmp1.gif') then
      DeleteFile('tmp1.gif');
    exit;
  end;

  result := true;
  J := TGIFImage.Create;
  B := TBitmap.Create;
  P := TPNGObject.Create;
  try
    J.LoadFromFile('tmp1.gif');
    B.Assign(J);
    P.Assign(B);
    P.SaveToFile(DestFile);
  except
    result := false;
  end;
  J.Free;
  B.Free;
  P.Free;

  if FileExists('tmp1.gif') then
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
  result := s;
  i := length(result);
  len := i;
  while i > 0 do
  begin
    c := result[i];
    if IsNumericC(c) then
    begin
      n := atoi(c);
      if n = 0 then
      begin
        ret1a := '';
        for j := 1 to i - 1 do
          ret1a := ret1a + result[j];
        ret2 := '';
        for j := i + 1 to len do
          ret2 := ret2 + result[j];
        ret1b := GetPrevAlphanumeric(ret1a);
        if ret1b <> ret1a then
        begin
          result := ret1b + '9' + ret2;
          exit;
        end
        else
          exit;
      end
      else
      begin
        result[i] := itoa(n - 1)[1];
        exit;
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
  result := s;
  i := length(result);
  len := i;
  while i > 0 do
  begin
    c := result[i];
    if IsNumericC(c) then
    begin
      n := atoi(c);
      if n = 9 then
      begin
        ret1a := '';
        for j := 1 to i - 1 do
          ret1a := ret1a + result[j];
        ret2 := '';
        for j := i + 1 to len do
          ret2 := ret2 + result[j];
        ret1b := GetNextAlphanumeric(ret1a);
        if ret1b <> ret1a then
        begin
          result := ret1b + '0' + ret2;
          exit;
        end
        else
          exit;
      end
      else
      begin
        result[i] := itoa(n + 1)[1];
        exit;
      end;
    end;
    dec(i);
  end;
end;


end.
