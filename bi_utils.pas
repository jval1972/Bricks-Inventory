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

function DownloadFileImg(SourceFile, DestFile: string): Boolean;

function DownloadJpgFileToPNG(SourceFile, DestFile: string): Boolean;

function DownloadPngFileToJpg(SourceFile, DestFile: string): Boolean;

function DownloadGIFFileToPNG(SourceFile, DestFile: string): Boolean;

procedure SetClipboardTextWideString(const Text: WideString);

function GetFileCreationTime(sFileName: string): TDateTime;

function GetPrevAlphanumeric(const s: string): string;
function GetNextAlphanumeric(const s: string): string;

function InputQuery2(const ACaption, APrompt, APrompt2: string;
  var Value, Value2: string): Boolean;

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

implementation

uses
  bi_delphi, bi_db, jpeg, bi_pak, bi_tmp, URLMon, Clipbrd, pngimage, GIFImage,
  bi_globals, Forms, StdCtrls, Controls, bi_crawler, StrUtils;

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
  sticker: boolean;
begin
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
          exit;
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
            exit;
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
    except
      printf('Image %s is invalid '#13#10,[path]);
    end;
  finally
    m.Free;
  end;
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
  if fexists(fbck) then
    fbck := fbck + '_latest';
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

function DownloadFileImg(SourceFile, DestFile: string): Boolean;
begin
  Result := DownloadFile(SourceFile, DestFile);
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
  result := DownloadFileImg(SourceFile, 'tmp1.jpg');
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

function DownloadPngFileToJpg(SourceFile, DestFile: string): Boolean;
var
  J: TJpegImage;
  B: TBitmap;
  P: TPNGObject;
begin
  result := DownloadFileImg(SourceFile, 'tmp1.png');
  if not result then
  begin
    if FileExists('tmp1.png') then
      DeleteFile('tmp1.png');
    exit;
  end;

  result := true;
  J := TJpegImage.Create;
  B := TBitmap.Create;
  P := TPNGObject.Create;
  try
    P.LoadFromFile('tmp1.png');
    B.Assign(P);
    J.Assign(B);
    J.SaveToFile(DestFile);
  except
    result := false;
  end;
  J.Free;
  B.Free;
  P.Free;

  if FileExists('tmp1.png') then
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
    result := false;
    f.Free;
    exit;
  end;

  f.Position := 7;
  f.Read(b, 1);
  if b <> 70 then
  begin
    result := false;
    f.Free;
    exit;
  end;

  f.Position := 7;
  f.Read(b, 1);
  if b <> 70 then
  begin
    result := false;
    f.Free;
    exit;
  end;

  f.Position := 8;
  f.Read(b, 1);
  if b <> 73 then
  begin
    result := false;
    f.Free;
    exit;
  end;

  f.Position := 9;
  f.Read(b, 1);
  if b <> 70 then
  begin
    result := false;
    f.Free;
    exit;
  end;

  result := true;
  f.Free;

end;

function DownloadGIFFileToPNG(SourceFile, DestFile: string): Boolean;
var
  J: TGIFImage;
  B: TBitmap;
  P: TPNGObject;
begin
  result := DownloadFileImg(SourceFile, 'tmp1.gif');
  if not result then
  begin
    if FileExists('tmp1.gif') then
      DeleteFile('tmp1.gif');
    exit;
  end;

  if IsLikeJpeg('tmp1.gif') then
  begin
    Result := DownloadJpgFileToPNG(SourceFile, DestFile);
  end
  else
  begin
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
  end;

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
  result := false;
  if not fexists(src) then
    exit;
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
  f.free;
  result := true;
end;

function CountChars(const S: string; const C: char): integer;
var
  i: Integer;
begin
  result := 0;
  for i := 1 to Length(S) do
    if S[i] = C then inc(result);
end;

function RemoveHTMLTags(S: string): string;
var
  TagBegin, TagEnd, TagLength: integer;
begin
  TagBegin := Pos( '<', S);      // search position of first < 

  while (TagBegin > 0) do begin  // while there is a < in S
    TagEnd := Pos('>', S);              // find the matching > 
    TagLength := TagEnd - TagBegin + 1;
    Delete(S, TagBegin, TagLength);     // delete the tag 
    TagBegin:= Pos( '<', S);            // search for next <
  end;

  Result := S;                   // give the result
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
begin
  ident:= ExtractFileName(filename);
  if Pos('.',ident) > 0 then
    ident := Copy(ident, 0, Pos('.', ident) - 1);

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
      line := ReplaceWhiteSpace(Trim(str[i]), '_', true);
      if Pos(':', line) <> 0 then
      begin
        splitstring(line, part, line, ':');
        splitstring(line, unused, color, npart, '_');
        color := Copy(color, Pos('=', color) + 1, Length(color) - Pos('=', color) - 1);
        npart := Copy(npart, Pos('=', npart) + 1, Length(npart) - Pos('=', npart) - 1);

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

  if FileExists(filename) then
  try
    ctx.LoadFromFile(filename);
    inv.NameValueSeparator := ',';
    for i := 0 to ctx.Count - 1 do
    begin
      ctx[i] := ReplaceWhiteSpace(ctx[i], #32, false);
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

end.
