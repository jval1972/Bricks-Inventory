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
//    Delphi library / low level routines
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_delphi;

interface

uses
  Windows,
  SysUtils, Classes;

type
  PPointer = ^Pointer;
  
  PString = ^string;

  PBoolean = ^Boolean;

  PInteger = ^Integer;

  PLongWord = ^LongWord;

  PShortInt = ^ShortInt;

  TWordArray = packed array[0..$FFFF] of word;
  PWordArray = ^TWordArray;

  TIntegerArray = packed array[0..$FFFF] of integer;
  PIntegerArray = ^TIntegerArray;

  TLongWordArray = packed array[0..$FFFF] of LongWord;
  PLongWordArray = ^TLongWordArray;

  TSmallintArray = packed array[0..$FFFF] of Smallint;
  PSmallintArray = ^TSmallintArray;

  TByteArray = packed array[0..$FFFF] of Byte;
  PByteArray = ^TByteArray;

  TCharArray = packed array[0..$FFFF] of Char;
  PCharArray = ^TCharArray;

  TBooleanArray = packed array[0..$FFFF] of boolean;
  PBooleanArray = ^TBooleanArray;

  PProcedure = procedure;
  PIntFunction = function: integer;

  TStringArray = array[0..$FFFF] of string;
  PStringArray = ^TStringArray;

  TPointerArray = packed array[0..$FFFF] of pointer;
  PPointerArray = ^TPointerArray;

  PSmallInt = ^SmallInt;
  TSmallIntPArray = packed array[0..$FFFF] of PSmallIntArray;
  PSmallIntPArray = ^TSmallIntPArray;

  PWord = ^Word;
  TWordPArray = packed array[0..$FFFF] of PWordArray;
  PWordPArray = ^TWordPArray;

  TLongWordPArray = packed array[0..$FFFF] of PLongWordArray;
  PLongWordPArray = ^TLongWordPArray;

  TIntegerPArray = packed array[0..$FFFF] of PIntegerArray;
  PIntegerPArray = ^TIntegerPArray;

  PByte = ^Byte;
  TBytePArray = packed array[0..$FFFF] of PByteArray;
  PBytePArray = ^TBytePArray;

  float = single;

  PDouble = ^Double;

type
  charset_t = set of char;

  twobytes = packed record
    byte1, byte2: byte;
  end;

type
  TOutProc = procedure (const s: string);

var
  outproc: TOutProc = nil;

procedure sprintf(var s: string; const Fmt: string; const Args: array of const);

procedure printf(const str: string); overload;

procedure printf(const Fmt: string; const Args: array of const); overload;

function itoa(i: integer): string;

function ftoa(f: double): string;

function atoi(const s: string; const default: integer = 0): integer;

function atoui(const s: string): longword; overload;

function atoui(const s: string; const default: longword): longword; overload;

function atof(const s: string): single; overload;

function atof(const s: string; const default: single): single; overload;

//
// Memory functions
//
function memmove(const destination, source: pointer; count: integer): pointer;

function memcpy(const dest0: pointer; const src0: pointer; count0: integer): pointer;

function memset(const dest0: pointer; const val: integer; const count0: integer): pointer;

function malloc(const size: integer): Pointer;

function mallocA(var Size: integer; const Align: integer; var original: pointer): pointer;

function mallocz(const size: integer): Pointer;

procedure realloc(var p: pointer; const oldsize, newsize: integer);

procedure memfree(var p: pointer; const size: integer);

var
  memoryusage: integer = 0;

function IntToStrZfill(const z: integer; const x: integer): string;

function intval(const b: boolean): integer;

function decide(const condition: boolean;
  const iftrue: integer; const iffalse: integer): integer; overload;

function decided(const condition: boolean;
  const iftrue: double; const iffalse: double): double;

function decide(const condition: boolean;
  const iftrue: boolean; const iffalse: boolean): boolean; overload;

function decide(const condition: boolean;
  const iftrue: string; const iffalse: string): string; overload;

function decide(const condition: boolean;
  const iftrue: pointer; const iffalse: pointer): pointer; overload;

function decide(const condition: integer;
  const iftrue: integer; const iffalse: integer): integer; overload;

function decide(const condition: integer;
  const iftrue: boolean; const iffalse: boolean): boolean; overload;

function decide(const condition: integer;
  const iftrue: string; const iffalse: string): string; overload;

function decide(const condition: integer;
  const iftrue: pointer; const iffalse: pointer): pointer; overload;

function decidef(const condition: boolean;
  const iftrue: single; const iffalse: single): single; 

function incp(var p: pointer; const size: integer = 1): pointer;

function pDiff(const p1, p2: pointer; const size: integer): integer;

function getenv(const env: string): string;

function fexists(const filename: string): boolean;

function fexpand(const filename: string): string;

procedure fdelete(const filename: string);

function fext(const filename: string): string;

function fname(const filename: string): string;

const
  fCreate = fmCreate or fmShareDenyWrite;
  fOpenReadOnly = fmOpenRead or fmShareDenyWrite;
  fOpenReadWrite = fmOpenReadWrite or fmShareDenyWrite;

  sFromBeginning = soFromBeginning;
  sFromCurrent = soFromCurrent;
  sFromEnd = soFromEnd;

  eSpecialChars = [',','.',PathDelim,'!','@','#','$','%','^','&','*','''','"',';','_','(',')',':','|','[',']'];

{type
  TStream = class
  protected
    FIOResult: integer;
  public
    OnBeginBusy: PProcedure;
    OnEndBusy: PProcedure;
    constructor Create;
    function Read(var Buffer; Count: Longint): Longint; virtual; abstract;
    function Write(const Buffer; Count: Longint): Longint; virtual; abstract;
    function Seek(Offset: Longint; Origin: Word): Longint; virtual; abstract;
    function Size: Longint; virtual; abstract;
    function Position: integer; virtual; abstract;
    function IOResult: integer;
  end;

  TMemoryStream = class(TStream)
  protected
    FSize: integer;
    FPosition: integer;
    FMemory: pointer;
    procedure Resize(newsize: integer);
  public
    OnBeginBusy: PProcedure;
    OnEndBusy: PProcedure;
    constructor Create;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Size: Longint; override;
    function Position: integer; override;
  end;}

{  TFile = class(TStream)
  private
    f: file;
  public
    constructor Create(const FileName: string; const mode: integer);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Size: Longint; override;
    function Position: integer; override;
  end;

  TCachedFile = class(TFile)
  private
    fBufSize: integer;
    fBuffer: pointer;
    fPosition: integer;
    fBufferStart: integer;
    fBufferEnd: integer;
    fSize: integer;
    fInitialized: boolean;
  protected
    procedure SetSize(NewSize: Longint); virtual;
    procedure ResetBuffer; virtual;
  public
    constructor Create(const FileName: string; mode: word; ABufSize: integer = $FFFF); virtual;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Position: integer; override;
  end;
 }

type
  TDNumberList = class(TObject)
  private
    fList: PIntegerArray;
    fNumItems: integer;
    fRealNumItems: integer;
  protected
    function Get(Index: Integer): integer; virtual;
    procedure Put(Index: Integer; const value: integer); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(const value: integer); overload; virtual;
    procedure AddRange(const value1, value2: integer); virtual;
    procedure Add(const nlist: TDNumberList); overload; virtual;
    function Delete(const Index: integer): boolean;
    function IndexOf(const value: integer): integer;
    procedure Clear;
    procedure Sort;
    property Count: integer read fNumItems;
    property Numbers[Index: Integer]: integer read Get write Put; default;
  end;

type
  TTextArray = array[0..$FFFF] of string[255];
  PTextArray = ^TTextArray;

type
  TDTextList = class
  private
    fList: PTextArray;
    fNumItems: integer;
  protected
    function Get(Index: Integer): string; virtual;
    procedure Put(Index: Integer; const value: string); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(const value: string); overload; virtual;
    procedure Add(const nlist: TDTextList); overload; virtual;
    function Delete(const Index: integer): boolean;
    function IndexOf(const value: string): integer;
    procedure Clear;
    property Count: integer read fNumItems;
    property Numbers[Index: Integer]: string read Get write Put; default;
  end;


const
  MaxListSize = MAXINT div 16;

type
{ TDStrings class }

  TDStrings = class
  private
    function GetCommaText: string;
    function GetName(Index: Integer): string;
    function GetValue(const Name: string): string;
    procedure SetCommaText(const Value: string);
    procedure SetValue(const Name, Value: string);
  protected
    function Get(Index: Integer): string; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: string; virtual;
    procedure Put(Index: Integer; const S: string); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: string); virtual;
    procedure SetByteStr(const A: PByteArray; const Size: integer); virtual;
  public
    function Add(const S: string): Integer; overload; virtual;
    function Add(const Fmt: string; const Args: array of const): Integer; overload; virtual;
    function AddObject(const S: string; AObject: TObject): Integer; virtual;
    procedure Append(const S: string);
    procedure AddStrings(Strings: TDStrings); virtual;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    function Equals(Strings: TDStrings): Boolean;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetText: PChar; virtual;
    function IndexOf(const S: string): Integer; virtual;
    function IndexOfName(const Name: string): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    procedure Insert(Index: Integer; const S: string); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: string;
      AObject: TObject);
    function  LoadFromFile(const FileName: string): boolean; virtual;
    function  LoadFromStream(const strm: TStream): boolean; virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    function SaveToFile(const FileName: string): boolean; virtual;
    procedure SetText(Text: PChar); virtual;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: string read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Names[Index: Integer]: string read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Values[const Name: string]: string read GetValue write SetValue;
    property Strings[Index: Integer]: string read Get write Put; default;
    property Text: string read GetTextStr write SetTextStr;
  end;

{ TDStringList class }

  TDStringList = class;

  PStringItem = ^TStringItem;
  TStringItem = record
    FString: string;
    FObject: TObject;
  end;

  PStringItemList = ^TStringItemList;
  TStringItemList = array[0..MaxListSize] of TStringItem;

  TDStringList = class(TDStrings)
  private
    FList: PStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure InsertItem(Index: Integer; const S: string);
  protected
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
  public
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

function findfile(const mask: string): string;

function findfiles(const mask: string): TDStringList;

procedure fprintf(var f: file; const str: string); overload;

procedure fprintf(var f: file; const Fmt: string; const Args: array of const); overload;

procedure fprintf(const f: TStream; const str: string); overload;

procedure fprintf(const f: TStream; const Fmt: string; const Args: array of const); overload;

function tan(const x: extended): extended;

function strupper(const S: string): string;

function strlower(const S: string): string;

function toupper(ch: Char): Char;

function tolower(ch: Char): Char;

function strremovespaces(const s: string): string;

function _SHL(const x: integer; const bits: integer): integer;

function _SHLW(const x: LongWord; const bits: LongWord): LongWord;

function _SHR(const x: integer; const bits: integer): integer;
function _SHR1(const x: integer): integer;
function _SHR2(const x: integer): integer;
function _SHR3(const x: integer): integer;
function _SHR4(const x: integer): integer;
function _SHR7(const x: integer): integer;
function _SHR8(const x: integer): integer;
function _SHR14(const x: integer): integer;

function _SHRW(const x: LongWord; const bits: LongWord): LongWord;

function StringVal(const Str: PChar): string;

procedure ZeroMemory(const dest0: pointer; const count0: integer);

function fopen(var f: file; const FileName: string; const mode: integer): boolean;

function fsize(const FileName: string): integer;

function fshortname(const FileName: string): string;

function strtrim(const S: string): string;

function capitalizedstring(const S: string; const splitter: char = ' '): string;

procedure splitstring(const inp: string; var out1, out2: string; const splitter: char); overload;

procedure splitstring2nd(const inp: string; var out1, out2: string; const splitter: char); overload;

procedure splitstring(const inp: string; var out1, out2, out3: string; const splitter: char); overload;

procedure splitstring(const inp: string; var out1, out2, out3, out4: string; const splitter: char); overload;

procedure splitstring(const inp: string; var out1, out2, out3, out4, out5: string; const splitter: char); overload;

procedure splitstring(const inp: string; var out1, out2, out3, out4, out5, out6: string; const splitter: char); overload;

procedure splitstring(const inp: string; var out1, out2, out3, out4, out5, out6, out7: string; const splitter: char); overload;

procedure splitstring(const inp: string; var out1, out2, out3, out4, out5, out6, out7, out8: string; const splitter: char); overload;

procedure splitstring(const inp: string; var out1, out2: string; const splitters: charset_t); overload;

procedure splitstring(const inp: string; var out1, out2, out3: string; const splitters: charset_t); overload;

function firstword(const inp: string; const splitter: char = ' '): string; overload;

function firstword(const inp: string; const splitters: charset_t): string; overload;

function secondword(const inp: string; const splitter: char = ' '): string; overload;

function secondword(const inp: string; const splitters: charset_t): string; overload;

function lastword(const inp: string; const splitter: char = ' '): string; overload;

function lastword(const inp: string; const splitters: charset_t): string; overload;

procedure FreeAndNil(var Obj);

function StrLCopy(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar; 

function fabs(const f: float): float;

procedure MakeDir(const dir: string);

function PascalText(src: PChar): string;

function CopyFile(const sname, dname: string): boolean;

function CacheReadFile(const fname: string): boolean;

function HexToInt(const s: string): integer;

function IsNumeric(const s: string): boolean;

function IsNumericC(const c: char): boolean;

function CountNumbers(const s: string): integer;

function between(const x: double; const x1, x2: double): Boolean;

var
  mmxMachine: byte = 0;
  AMD3DNowMachine: byte = 0;

type
  TDouble = class(TObject)
  private
    fvalue: double;
  public
    constructor Create(const avalue: double = 0.0); virtual;
    procedure Add(const avalue: double);
    property value: double read fvalue write fvalue;
  end;

  TCInteger = class(TObject)
  private
    fvalue: integer;
  public
    constructor Create(const avalue: integer = 0); virtual;
    procedure IncValue; virtual;
    procedure Add(const avalue: integer);
    property value: integer read fvalue write fvalue;
  end;

  TString = class(TObject)
  public
    text: string;
  end;

function string2stringlist(const s: string; const c: char): TStringList;
function stringlist2string(const s: TStringList; const c: char): string;

function dbl_equal(const dbl1, dbl2: double): boolean;

function dbl_safe_div(const a, b: double): double;

procedure QSortIntegers(const A: PIntegerArray; const Len: integer);

function SaveStringToFile(const fname: string; const s: string): boolean;

function LoadStringFromFile(const fname: string): string;

function SaveDataToFile(const fname: string; const fdata: pointer; const size: integer): boolean;

function IsIntegerInRange(const test, f1, f2: integer): boolean;

function btoi(const b: boolean): integer;

function itob(const i: integer): boolean;

function btoa(const b: boolean): string;

function atob(const s: string): boolean;

function RemoveSpaces(const s: string): string;

function CharPos(const ch: Char; const s: string): integer;

function Pos1(const subs, s: string): boolean;

function MakePathString(const str: string): string;

implementation

procedure sprintf(var s: string; const Fmt: string; const Args: array of const);
begin
  FmtStr(s, Fmt, Args);
end;

procedure printf(const str: string);
begin
  if Assigned(outproc) then
    outproc(str)
  else if IsConsole then
    write(str);
end;

procedure printf(const Fmt: string; const Args: array of const);
var
  s: string;
begin
  sprintf(s, Fmt, Args);
  printf(s);
end;

procedure fprintf(var f: file; const str: string);
begin
  BlockWrite(f, (@str[1])^, Length(str));
end;

procedure fprintf(var f: file; const Fmt: string; const Args: array of const);
var
  s: string;
begin
  sprintf(s, Fmt, Args);
  fprintf(f, s);
end;

procedure fprintf(const f: TStream; const str: string); overload;
begin
  f.Write((@str[1])^, Length(str));
end;

{procedure fprintf(const f: TFile; const str: string);
begin
  fprintf(f.f, str);
end;}

procedure fprintf(const f: TStream; const Fmt: string; const Args: array of const);
var
  s: string;
begin
  sprintf(s, Fmt, Args);
  fprintf(f, s);
end;

function itoa(i: integer): string;
begin
  sprintf(Result, '%d', [i]);
end;

function ftoa(f: double): string;
begin
  Result := FloatToStr(f);
end;

function atoi(const s: string; const default: integer = 0): integer;
var
  code: integer;
  ret2: integer;
  p: integer;
  s1: string;
begin
  val(s, Result, code);
  if code <> 0 then
  begin
    if Pos1('0x', s) then
      val('$' + Copy(s, 3, Length(s) - 2), ret2, code)
    else
      val('$' + s, ret2, code);
    if code = 0 then
      Result := ret2
    else
    begin
      if Length(s) > 3 then
        if s[Length(s) - 3] = ',' then
        begin
          s1 := '';
          for p := 1 to Length(s) do
            if s[p] <> ',' then
              s1 := s1 + s[p];
          Result := atoi(s1, default);
          Exit;
        end;
      Result := default;
    end;
  end;
end;

function atoui(const s: string): longword; overload;
var
  code: integer;
  ret2: longword;
begin
  val(s, Result, code);
  if code <> 0 then
  begin
    if Pos1('0x', s) then
      val('$' + Copy(s, 3, Length(s) - 2), ret2, code)
    else
      val('$' + s, ret2, code);
    if code = 0 then
      Result := ret2
    else
      Result := 0;
  end;
end;

function atoui(const s: string; const default: longword): longword; overload;
var
  code: integer;
  ret2: longword;
begin
  val(s, Result, code);
  if code <> 0 then
  begin
    if Pos1('0x', s) then
      val('$' + Copy(s, 3, Length(s) - 2), ret2, code)
    else
      val('$' + s, ret2, code);
    if code = 0 then
      Result := ret2
    else
      Result := default;
  end;
end;

function atof(const s: string): single;
begin
  Result := atof(s, 0.0);
end;

function atof(const s: string; const default: single): single;
var
  code: integer;
  i: integer;
  str: string;
begin
  val(s, Result, code);
  if code <> 0 then
  begin
    str := s;
    for i := 1 to Length(str) do
      if str[i] in ['.', ','] then
        str[i] := DecimalSeparator;
    val(str, Result, code);
    if code <> 0 then
      Result := default;
  end;
end;

procedure memcpy_MMX8(const dst: pointer; const src: pointer; const len: integer); assembler;
asm
  push esi
  push edi

  mov esi, src
  mov edi, dst
  mov ecx, len
  shr ecx, 3  // 8 bytes per iteration

@@loop1:
// Read in source data
  movq  mm1, [esi]
// Non-temporal stores
  movntq [edi], mm1

  add esi, 8
  add edi, 8
  dec ecx
  jnz @@loop1

  emms

  pop edi
  pop esi
end;

procedure memcpy_MMX64(const dst: pointer; const src: pointer; const len: integer); assembler;
asm
  push esi
  push edi

  mov esi, src
  mov edi, dst
  mov ecx, len
  shr ecx, 6    // 64 bytes per iteration

@@loop1:

// Read in source data
  movq mm1, [esi]
  movq mm2, [esi + 8]
  movq mm3, [esi + 16]
  movq mm4, [esi + 24]
  movq mm5, [esi + 32]
  movq mm6, [esi + 40]
  movq mm7, [esi + 48]
  movq mm0, [esi + 56]

// Non-temporal stores
  movntq [edi], mm1
  movntq [edi + 8], mm2
  movntq [edi + 16], mm3
  movntq [edi + 24], mm4
  movntq [edi + 32], mm5
  movntq [edi + 40], mm6
  movntq [edi + 48], mm7
  movntq [edi + 56], mm0

  add esi, 64
  add edi, 64
  dec ecx
  jnz @@loop1

  emms

  pop edi
  pop esi
end;

procedure memcpy_3DNow64(const dst: pointer; const src: pointer; const len: integer); assembler;
asm
  push esi
  push edi

  mov esi, src
  mov edi, dst
  mov ecx, len
  shr ecx, 6    // 64 bytes per iteration

@@loop1:
// Prefetch next loop, non-temporal
  prefetch [esi + 64]
  prefetch [esi + 96]

// Read in source data
  movq mm1, [esi]
  movq mm2, [esi + 8]
  movq mm3, [esi + 16]
  movq mm4, [esi + 24]
  movq mm5, [esi + 32]
  movq mm6, [esi + 40]
  movq mm7, [esi + 48]
  movq mm0, [esi + 56]

// Non-temporal stores
  movntq [edi], mm1
  movntq [edi + 8], mm2
  movntq [edi + 16], mm3
  movntq [edi + 24], mm4
  movntq [edi + 32], mm5
  movntq [edi + 40], mm6
  movntq [edi + 48], mm7
  movntq [edi + 56], mm0

  add esi, 64
  add edi, 64
  dec ecx
  jnz @@loop1

  emms

  pop edi
  pop esi
end;

function memmove(const destination, source: pointer; count: integer): pointer;
begin
  Move(source^, destination^, count);
  Result := destination;
end;

function memcpy(const dest0: pointer; const src0: pointer; count0: integer): pointer;
var
  dest: PByte;
  src: PByte;
  count: integer;
begin
  if mmxMachine = 0 then
  begin
    Move(src0^, dest0^, count0);
    Result := dest0;
    Exit;
  end;

{  if abs(integer(dest0) - integer(src0)) < 8 then
  begin
    printf('memcpy(): FUCK!!');
    Exit;
  end;}

  // if copying more than 16 bytes and we can copy 8 byte aligned
  if (count0 > 16) and (((integer(dest0) xor integer(src0)) and 7) = 0) then
  begin
    dest := PByte(dest0);
    src := PByte(src0);

    // copy up to the first 8 byte aligned boundary
    count := integer(dest) and 7;
    Move(src^, dest^, count);
    inc(dest, count);
    inc(src, count);
    count := count0 - count;

   // if there are blocks of 64 bytes
    if count and (not 63) <> 0 then
    begin
      if AMD3DNowMachine <> 0 then
        memcpy_3DNow64(dest, src, count and (not 63))
      else
        memcpy_MMX64(dest, src, count and (not 63));
      inc(src, count and (not 63));
      inc(dest, count and (not 63));
      count := count and 63;
    end;

    // if there are blocks of 8 bytes
    if count and (not 7) <> 0 then
    begin
      memcpy_MMX8(dest, src, count);
      inc(src, count and (not 7));
      inc(dest, count and (not 7));
      count := count and 7;
    end;

    // copy any remaining bytes
    Move(src^, dest^, count);
  end
  else
  begin
    // use the regular one if we cannot copy 8 byte aligned
    Move(src0^, dest0^, count0);
  end;
  Result := dest0;
end;

type
  union_8b = record
    case integer of
      1: (bytes: array[0..7] of byte);
      2: (words: array[0..3] of word);
      3: (dwords: array[0..1] of LongWord);
  end;
  
function memset(const dest0: pointer; const val: integer; const count0: integer): pointer;
var
  data: union_8b;
  pdat: pointer;
  dest: PByte;
  count: integer;
begin
  if mmxMachine = 0 then
  begin
    FillChar(dest0^, count0, val);
    Result := dest0;
    Exit;
  end;

  dest := PByte(dest0);
  count := count0;

  while (count > 0) and (integer(dest) and 7 <> 0) do
  begin
    dest^ := val;
    inc(dest);
    dec(count);
  end;

  if count = 0 then
  begin
    Result := dest0;
    Exit;
  end;

  data.bytes[0] := val;
  data.bytes[1] := val;
  data.words[1] := data.words[0];
  data.dwords[1] := data.dwords[0];
  pdat := @data;

  if count >= 64 then
  begin
    asm
      push esi
      push edi

      mov edi, dest
      mov esi, pdat

      mov ecx, count
      // 64 bytes per iteration
      shr ecx, 6
      // Read in source data
      movq mm1, [esi]
      movq mm2, mm1
      movq mm3, mm1
      movq mm4, mm1
      movq mm5, mm1
      movq mm6, mm1
      movq mm7, mm1
      movq mm0, mm1
@@loop1:
      movntq [edi], mm1
      movntq [edi + 8], mm2
      movntq [edi + 16], mm3
      movntq [edi + 24], mm4
      movntq [edi + 32], mm5
      movntq [edi + 40], mm6
      movntq [edi + 48], mm7
      movntq [edi + 56], mm0

      add edi, 64
      dec ecx
      jnz @@loop1

      pop edi
      pop esi
    end;

    inc(dest, count and (not 63));
    count := count and 63;
  end;

  if count >= 8 then
  begin
    asm
      push esi
      push edi

      mov edi, dest
      mov esi, pdat

      mov ecx, count
      // 8 bytes per iteration
      shr ecx, 3
      // Read in source data
      movq mm1, [esi]
@@loop2:
      movntq  [edi], mm1

      add edi, 8
      dec ecx
      jnz @@loop2

      pop edi
      pop esi
    end;
    inc(dest, count and (not 7));
    count := count and 7;
  end;

  while count > 0 do
  begin
    dest^ := val;
    inc(dest);
    dec(count);
  end;

  asm
    emms
  end;

  Result := dest0;
end;

function malloc(const size: integer): Pointer;
begin
  if size = 0 then
    Result := nil
  else
  begin
    GetMem(Result, size);
    memoryusage := memoryusage + size;
  end;
end;

function mallocA(var Size: integer; const Align: integer; var original: pointer): pointer;
begin
  Size := Size + Align;
  Result := malloc(Size);
  original := Result; 
  if Result <> nil then
    Result := pointer(integer(Result) and (1 - Align) + Align);
end;

function mallocz(const size: integer): Pointer;
begin
  Result := malloc(size);
  if Result <> nil then
    ZeroMemory(Result, size);
end;

procedure realloc(var p: pointer; const oldsize, newsize: integer);
begin
  if newsize = 0 then
    memfree(p, oldsize)
  else if newsize <> oldsize then
  begin
    reallocmem(p, newsize);
    memoryusage := memoryusage - oldsize + newsize;
  end;
end;

procedure memfree(var p: pointer; const size: integer);
begin
  if p <> nil then
  begin
    FreeMem(p, size);
    p := nil;
    memoryusage := memoryusage - size;
  end;
end;

function IntToStrZfill(const z: integer; const x: integer): string;
var
  i: integer;
  len: integer;
begin
  Result := itoa(x);
  len := Length(Result);
  for i := len + 1 to z do
    Result := '0' + Result;
end;

function intval(const b: boolean): integer;
begin
  if b then
    Result := 1
  else
    Result := 0;
end;

function decided(const condition: boolean;
  const iftrue: double; const iffalse: double): double;
begin
  if condition then
    Result := iftrue
  else
    Result := iffalse;
end;

function decide(const condition: boolean;
  const iftrue: integer; const iffalse: integer): integer;
begin
  if condition then
    Result := iftrue
  else
    Result := iffalse;
end;

function decide(const condition: boolean;
  const iftrue: boolean; const iffalse: boolean): boolean;
begin
  if condition then
    Result := iftrue
  else
    Result := iffalse;
end;

function decide(const condition: boolean;
  const iftrue: string; const iffalse: string): string;
begin
  if condition then
    Result := iftrue
  else
    Result := iffalse;
end;

function decide(const condition: boolean;
  const iftrue: pointer; const iffalse: pointer): pointer;
begin
  if condition then
    Result := iftrue
  else
    Result := iffalse;
end;

function decide(const condition: integer;
  const iftrue: integer; const iffalse: integer): integer;
begin
  if condition <> 0 then
    Result := iftrue
  else
    Result := iffalse;
end;

function decide(const condition: integer;
  const iftrue: boolean; const iffalse: boolean): boolean;
begin
  if condition <> 0 then
    Result := iftrue
  else
    Result := iffalse;
end;

function decide(const condition: integer;
  const iftrue: string; const iffalse: string): string;
begin
  if condition <> 0 then
    Result := iftrue
  else
    Result := iffalse;
end;

function decide(const condition: integer;
  const iftrue: pointer; const iffalse: pointer): pointer;
begin
  if condition <> 0 then
    Result := iftrue
  else
    Result := iffalse;
end;

function decidef(const condition: boolean;
  const iftrue: single; const iffalse: single): single;
begin
  if condition then
    Result := iftrue
  else
    Result := iffalse;
end;

function incp(var p: pointer; const size: integer = 1): pointer;
begin
  Result := Pointer(integer(p) + size);
  p := Result;
end;

function pDiff(const p1, p2: pointer; const size: integer): integer;
begin
  Result := (Integer(p1) - Integer(p2)) div size;
end;

////////////////////////////////////////////////////////////////////////////////
// TStream
(*constructor TStream.Create;
begin
  FIOResult := 0;
end;

function TStream.IOResult: integer;
begin
  Result := FIOResult;
  FIOResult := 0;
end;

////////////////////////////////////////////////////////////////////////////////
// TMemoryStream
constructor TMemoryStream.Create;
begin
  Inherited Create;
  FSize := 0;
  FPosition := 0;
  FMemory := nil;
end;

destructor TMemoryStream.Destroy;
begin
  Resize(0);
  Inherited Destroy;
end;

procedure TMemoryStream.Resize(newsize: integer);
begin
  if FSize <> newsize then
  begin
    realloc(FMemory, FSize, newsize);
    FSize := newsize;
    if FPosition > FSize then
      FPosition := FSize;
  end;
end;

function TMemoryStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Count + FPosition > FSize then
    Result := FSize - FPosition
  else
    Result := Count;

  memcpy(@Buffer, pointer(integer(FMemory) + FPosition), Result);
  FPosition := FPosition + Result;
end;

function TMemoryStream.Write(const Buffer; Count: Longint): Longint;
begin
  if Count + FPosition > FSize then
    resize(Count + FPosition);
  memcpy(pointer(integer(FMemory) + FPosition), @Buffer, Count);
  FPosition := FPosition + Count;
  Result := Count;
end;

function TMemoryStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    sFromBeginning:
      Result := Offset;
    sFromCurrent:
      Result := FPosition + Offset;
    sFromEnd:
      Result := FPosition - Offset;
  else
    Result := 0;
  end;
  FPosition := Result;
end;

function TMemoryStream.Size: Longint;
begin
  Result := FSize;
end;

function TMemoryStream.Position: integer;
begin
  Result := FPosition;
end;

////////////////////////////////////////////////////////////////////////////////
// TFile
// File class
constructor TFile.Create(const FileName: string; const mode: integer);
begin
  Inherited Create;
  OnBeginBusy := nil;
  OnEndBusy := nil;

  fopen(f, FileName, mode);
end;

destructor TFile.Destroy;
begin
  close(f);
  Inherited;
end;

function TFile.Read(var Buffer; Count: Longint): Longint;
begin
  if Assigned(OnBeginBusy) then OnBeginBusy;

  {$I-}
  BlockRead(f, Buffer, Count, Result);
  {$I+}
  FIOResult := IOResult;

  if Assigned(OnEndBusy) then OnEndBusy;
end;

function TFile.Write(const Buffer; Count: Longint): Longint;
begin
  if Assigned(OnBeginBusy) then OnBeginBusy;

  {$I-}
  BlockWrite(f, Buffer, Count, Result);
  {$I+}
  FIOResult := IOResult;

  if Assigned(OnEndBusy) then OnEndBusy;
end;

function TFile.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    sFromBeginning:
      Result := Offset;
    sFromCurrent:
      Result := FilePos(f) + Offset;
    sFromEnd:
      Result := FileSize(f) - Offset;
  else
    Result := 0;
  end;
  {$I-}
  system.Seek(f, Result);
  {$I+}
  FIOResult := IOResult;
end;

function TFile.Size: Longint;
begin
  {$I-}
  Result := FileSize(f);
  {$I+}
  FIOResult := IOResult;
end;

function TFile.Position: integer;
begin
  {$I-}
  Result := FilePos(f);
  {$I+}
  FIOResult := IOResult;
end;

////////////////////////////////////////////////////////////////////////////////
// TCachedFile
// Cache read file class
constructor TCachedFile.Create(const FileName: string; mode: word; ABufSize: integer = $FFFF);
begin
  fInitialized := False;
  Inherited Create(FileName, mode);
  if ABufSize > Size then
    fBufSize := Size
  else
    fBufSize := ABufSize;
  fBuffer := malloc(fBufSize);
  fPosition := 0;
  ResetBuffer;
  fSize := Inherited Size;
  fInitialized := True;
end;

procedure TCachedFile.ResetBuffer;
begin
  fBufferStart := -1;
  fBufferEnd := -1;
end;

destructor TCachedFile.Destroy;
begin
  memfree(fBuffer, fBufSize);
  Inherited;
end;

function TCachedFile.Read(var Buffer; Count: Longint): Longint;
var
  x: Longint;
begin
// Buffer hit
  if (fPosition >= fBufferStart) and (fPosition + Count <= fBufferEnd) then
  begin
    x := LongInt(fBuffer) + fPosition - fBufferStart;
    Move(Pointer(x)^, Buffer, Count);
    fPosition := fPosition + Count;
    Result := Count;
  end
// Non Buffer hit, cache buffer
  else if Count <= fBufSize then
  begin
    fPosition := Inherited Seek(fPosition, sFromBeginning);
    x := Inherited Read(fBuffer^, fBufSize);
    if x < Count then
      Result := x
    else
      Result := Count;
    Move(fBuffer^, Buffer, Count);
    fBufferStart := fPosition;
    fBufferEnd := fPosition + x;
    fPosition := fPosition + Result;
  end
// Keep old buffer
  else
  begin
    fPosition := Inherited Seek(fPosition, sFromBeginning);
    Result := Inherited Read(Buffer, Count);
    fPosition := fPosition + Result;
  end;
end;

function TCachedFile.Write(const Buffer; Count: Longint): Longint;
begin
  fPosition := Inherited Seek(fPosition, sFromBeginning);
  Result := Inherited Write(Buffer, Count);
  fPosition := fPosition + Result;
  if fSize < fPosition then
    fSize := fPosition;
end;

function TCachedFile.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if fInitialized then
  begin
    case Origin of
      sFromBeginning: fPosition := Offset;
      sFromCurrent: Inc(fPosition, Offset);
      sFromEnd: fPosition := fSize + Offset;
    end;
    Result := fPosition;
  end
  else
    Result := Inherited Seek(Offset, Origin);
end;

procedure TCachedFile.SetSize(NewSize: Longint);
begin
  Inherited;
  fSize := NewSize;
end;

function TCachedFile.Position: integer;
begin
  Result := FPosition;
end;
*)
////////////////////////////////////////////////////////////////////////////////
// TDNumberList
constructor TDNumberList.Create;
begin
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
  Inherited;
end;

destructor TDNumberList.Destroy;
begin
  Clear;
  Inherited;
end;

function TDNumberList.Get(Index: Integer): integer;
begin
  if (Index < 0) or (Index >= fNumItems) then
    Result := 0
  else
    Result := fList[Index];
end;

procedure TDNumberList.Put(Index: Integer; const value: integer);
begin
  fList[Index] := value;
end;

procedure TDNumberList.Add(const value: integer);
var
  newrealitems: integer;
begin
  if fNumItems >= fRealNumItems then
  begin
    if fRealNumItems < 4 then
      newrealitems := 4
    else if fRealNumItems < 8 then
      newrealitems := 8
    else if fRealNumItems < 16 then
      newrealitems := 16
    else if fRealNumItems < 128 then
      newrealitems := fRealNumItems + 16
    else
      newrealitems := fRealNumItems + 32;
    realloc(pointer(fList), fRealNumItems * SizeOf(integer), newrealitems * SizeOf(integer));
    fRealNumItems := newrealitems;
  end;
  Put(fNumItems, value);
  inc(fNumItems);
end;

procedure TDNumberList.AddRange(const value1, value2: integer);
var
  i: integer;
begin
  if value1 < value2 then
  begin
    for i := value1 to value2 do
      Add(i);
  end
  else
  begin
    for i := value1 downto value2 do
      Add(i);
  end;
end;

procedure TDNumberList.Add(const nlist: TDNumberList);
var
  i: integer;
begin
  for i := 0 to nlist.Count - 1 do
    Add(nlist[i]);
end;

function TDNumberList.Delete(const Index: integer): boolean;
var
  i: integer;
begin
  if (Index < 0) or (Index >= fNumItems) then
  begin
    Result := False;
    Exit;
  end;

  for i := Index + 1 to fNumItems - 1 do
    fList[i - 1] := fList[i];

  dec(fNumItems);
  Result := True;
end;

function TDNumberList.IndexOf(const value: integer): integer;
var
  i: integer;
begin
  for i := 0 to fNumItems - 1 do
    if fList[i] = value then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

procedure TDNumberList.Clear;
begin
  realloc(pointer(fList), fRealNumItems * SizeOf(integer), 0);
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

procedure TDNumberList.Sort;
begin
  QSortIntegers(fList, fNumItems);
end;

////////////////////////////////////////////////////////////////////////////////
// TDTextList
constructor TDTextList.Create;
begin
  fList := nil;
  fNumItems := 0;
end;

destructor TDTextList.Destroy;
begin
  Clear;
end;

function TDTextList.Get(Index: Integer): string;
begin
  if (Index < 0) or (Index >= fNumItems) then
    Result := ''
  else
    Result := fList[Index];
end;

procedure TDTextList.Put(Index: Integer; const value: string);
begin
  fList[Index] := value;
end;

procedure TDTextList.Add(const value: string);
begin
  realloc(pointer(fList), fNumItems * 256, (fNumItems + 1) * 256);
  Put(fNumItems, value);
  inc(fNumItems);
end;

procedure TDTextList.Add(const nlist: TDTextList);
var
  i: integer;
begin
  for i := 0 to nlist.Count - 1 do
    Add(nlist[i]);
end;

function TDTextList.Delete(const Index: integer): boolean;
var
  i: integer;
begin
  if (Index < 0) or (Index >= fNumItems) then
  begin
    Result := False;
    Exit;
  end;

  for i := Index + 1 to fNumItems - 1 do
    fList[i - 1] := fList[i];

  realloc(pointer(fList), fNumItems * 256, (fNumItems - 1) * 256);
  dec(fNumItems);

  Result := True;
end;

function TDTextList.IndexOf(const value: string): integer;
var
  i: integer;
begin
  for i := 0 to fNumItems - 1 do
    if fList[i] = value then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

procedure TDTextList.Clear;
begin
  realloc(pointer(fList), fNumItems * 256, 0);
  fList := nil;
  fNumItems := 0;
end;

////////////////////////////////////////////////////////////////////////////////
// TDStrings
function TDStrings.Add(const S: string): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

function TDStrings.Add(const Fmt: string; const Args: array of const): integer;
var
  str: string;
begin
  sprintf(str, Fmt, Args);
  Result := Add(str);
end;

function TDStrings.AddObject(const S: string; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

procedure TDStrings.Append(const S: string);
begin
  Add(S);
end;

procedure TDStrings.AddStrings(Strings: TDStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    AddObject(Strings[I], Strings.Objects[I]);
end;

function TDStrings.Equals(Strings: TDStrings): Boolean;
var
  I, iCount: Integer;
begin
  Result := False;
  iCount := GetCount;
  if iCount <> Strings.GetCount then Exit;
  for I := 0 to iCount - 1 do if Get(I) <> Strings.Get(I) then Exit;
  Result := True;
end;

procedure TDStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: string;
begin
  TempString := Strings[Index1];
  TempObject := Objects[Index1];
  Strings[Index1] := Strings[Index2];
  Objects[Index1] := Objects[Index2];
  Strings[Index2] := TempString;
  Objects[Index2] := TempObject;
end;

function TDStrings.GetCapacity: Integer;
begin  // descendants may optionally override/replace this default implementation
  Result := Count;
end;

function TDStrings.GetCommaText: string;
var
  S: string;
  P: PChar;
  I, iCount: Integer;
begin
  iCount := GetCount;
  if (iCount = 1) and (Get(0) = '') then
    Result := '""'
  else
  begin
    Result := '';
    for I := 0 to iCount - 1 do
    begin
      S := Get(I);
      P := PChar(S);
      while not (P^ in [#0..' ','"',',']) do P := CharNext(P);
      if (P^ <> #0) then S := AnsiQuotedStr(S, '"');
      Result := Result + S + ',';
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

function TDStrings.GetName(Index: Integer): string;
var
  P: Integer;
begin
  Result := Get(Index);
  P := AnsiPos('=', Result);
  if P <> 0 then
    SetLength(Result, P-1)
  else
    SetLength(Result, 0);
end;

function TDStrings.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

function TDStrings.GetText: PChar;
begin
  Result := StrNew(PChar(GetTextStr));
end;

function TDStrings.GetTextStr: string;
var
  I, L, Size, iCount: Integer;
  P: PChar;
  S: string;
begin
  iCount := GetCount;
  Size := 0;
  for I := 0 to iCount - 1 do Inc(Size, Length(Get(I)) + 2);
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to iCount - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L);
      Inc(P, L);
    end;
    P^ := #13;
    Inc(P);
    P^ := #10;
    Inc(P);
  end;
end;

function TDStrings.GetValue(const Name: string): string;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := Copy(Get(I), Length(Name) + 2, MaxInt) else
    Result := '';
end;

function TDStrings.IndexOf(const S: string): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if AnsiCompareText(Get(Result), S) = 0 then Exit;
  Result := -1;
end;

function TDStrings.IndexOfName(const Name: string): Integer;
var
  P: Integer;
  S: string;
begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := AnsiPos('=', S);
    if (P <> 0) and (AnsiCompareText(Copy(S, 1, P - 1), Name) = 0) then Exit;
  end;
  Result := -1;
end;

function TDStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

procedure TDStrings.InsertObject(Index: Integer; const S: string;
  AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

function TDStrings.LoadFromFile(const FileName: string): boolean;
var
  f: file;
  Size: Integer;
  S: string;
begin
  if fopen(f, FileName, fOpenReadOnly) then
  begin
    {$I-}
    Size := FileSize(f);
    SetString(S, nil, Size);
    BlockRead(f, Pointer(S)^, Size);
    SetTextStr(S);
    close(f);
    {$I+}
    Result := IOresult = 0;
  end
  else
    Result := False;
end;

function TDStrings.LoadFromStream(const strm: TStream): boolean;
var
  Size: Integer;
  A: PByteArray;
begin
  {$I-}
  strm.Seek(0, sFromBeginning);
  Size := strm.Size;
  A := malloc(Size);
  strm.Read(A^, Size);
  SetByteStr(A, Size);
  memfree(pointer(A), Size);
  {$I+}
  Result := IOresult = 0;
end;

procedure TDStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: string;
begin
  if CurIndex <> NewIndex then
  begin
    TempString := Get(CurIndex);
    TempObject := GetObject(CurIndex);
    Delete(CurIndex);
    InsertObject(NewIndex, TempString, TempObject);
  end;
end;

procedure TDStrings.Put(Index: Integer; const S: string);
var
  TempObject: TObject;
begin
  TempObject := GetObject(Index);
  Delete(Index);
  InsertObject(Index, S, TempObject);
end;

procedure TDStrings.PutObject(Index: Integer; AObject: TObject);
begin
end;

function TDStrings.SaveToFile(const FileName: string): boolean;
var
  f: file;
  S: string;
begin
  if fopen(f, FileName, fCreate) then
  begin
    {$I-}
    S := GetTextStr;
    BlockWrite(f, Pointer(S)^, Length(S));
    close(f);
    {$I+}
    Result := IOresult = 0;
  end
  else
    Result := False;
end;

procedure TDStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendants may optionally implement this method
end;

procedure TDStrings.SetCommaText(const Value: string);
var
  P, P1: PChar;
  S: string;
begin
  Clear;
  P := PChar(Value);
  while P^ in [#1..' '] do P := CharNext(P);
  while P^ <> #0 do
  begin
    if P^ = '"' then
      S := AnsiExtractQuotedStr(P, '"')
    else
    begin
      P1 := P;
      while (P^ > ' ') and (P^ <> ',') do P := CharNext(P);
      SetString(S, P1, P - P1);
    end;
    Add(S);
    while P^ in [#1..' '] do P := CharNext(P);
    if P^ = ',' then
      repeat
        P := CharNext(P);
      until not (P^ in [#1..' ']);
  end;
end;

procedure TDStrings.SetText(Text: PChar);
begin
  SetTextStr(Text);
end;

procedure TDStrings.SetTextStr(const Value: string);
var
  P, Start: PChar;
  S: string;
begin
  Clear;
  P := Pointer(Value);
  if P <> nil then
    while P^ <> #0 do
    begin
      Start := P;
      while not (P^ in [#0, #10, #13]) do Inc(P);
      SetString(S, Start, P - Start);
      Add(S);
      if P^ = #13 then Inc(P);
      if P^ = #10 then Inc(P);
    end;
end;

procedure TDStrings.SetByteStr(const A: PByteArray; const Size: integer);
var
  P, Start: PChar;
  S: string;
begin
  Clear;
  P := PChar(@A[0]);
  if P <> nil then
    while (P^ <> #0) and (integer(P) <> integer(@A[Size])) do
    begin
      Start := P;
      while (not (P^ in [#0, #10, #13])) and (integer(P) <> integer(@A[Size])) do Inc(P);
      SetString(S, Start, P - Start);
      Add(S);
      if P^ = #13 then Inc(P);
      if P^ = #10 then Inc(P);
    end;
end;


procedure TDStrings.SetValue(const Name, Value: string);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then I := Add('');
    Put(I, Name + '=' + Value);
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TStringList
destructor TDStringList.Destroy;
begin
  inherited Destroy;
  if FCount <> 0 then Finalize(FList[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TDStringList.Add(const S: string): Integer;
begin
  Result := FCount;
  InsertItem(Result, S);
end;

procedure TDStringList.Clear;
begin
  if FCount <> 0 then
  begin
    Finalize(FList[0], FCount);
    FCount := 0;
    SetCapacity(0);
  end;
end;

procedure TDStringList.Delete(Index: Integer);
begin
  if (Index >= 0) and (Index < FCount) then
  begin
    Finalize(FList[Index]);
    Dec(FCount);
    if Index < FCount then
      System.Move(FList[Index + 1], FList[Index],
        (FCount - Index) * SizeOf(TStringItem));
  end;
end;

procedure TDStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Exit;
  if (Index2 < 0) or (Index2 >= FCount) then Exit;
  ExchangeItems(Index1, Index2);
end;

procedure TDStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Integer;
  Item1, Item2: PStringItem;
begin
  Item1 := @FList[Index1];
  Item2 := @FList[Index2];
  Temp := Integer(Item1.FString);
  Integer(Item1.FString) := Integer(Item2.FString);
  Integer(Item2.FString) := Temp;
  Temp := Integer(Item1.FObject);
  Integer(Item1.FObject) := Integer(Item2.FObject);
  Integer(Item2.FObject) := Temp;                
end;

function TDStringList.Get(Index: Integer): string;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FList[Index].FString
  else
    Result := '';
end;

function TDStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TDStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TDStringList.GetObject(Index: Integer): TObject;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FList[Index].FObject
  else
    Result := nil;
end;

procedure TDStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 8 else
    if FCapacity > 8 then Delta := 8 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure TDStringList.Insert(Index: Integer; const S: string);
begin
  if (Index >= 0) and (Index <= FCount) then
    InsertItem(Index, S);
end;

procedure TDStringList.InsertItem(Index: Integer; const S: string);
begin
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(TStringItem));
  with FList[Index] do
  begin
    Pointer(FString) := nil;
    FObject := nil;
    FString := S;
  end;
  Inc(FCount);
end;

procedure TDStringList.Put(Index: Integer; const S: string);
begin
  if (Index > 0) and (Index < FCount) then
    FList[Index].FString := S;
end;

procedure TDStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index >= 0) and (Index < FCount) then
    FList[Index].FObject := AObject;
end;

procedure TDStringList.SetCapacity(NewCapacity: Integer);
begin
  realloc(pointer(FList), FCapacity * SizeOf(TStringItem), NewCapacity * SizeOf(TStringItem));
  FCapacity := NewCapacity;
end;

////////////////////////////////////////////////////////////////////////////////

function getenv(const env: string): string;
var
  buf: array[0..255] of char;
begin
  ZeroMemory(@buf, SizeOf(buf));
  GetEnvironmentVariable(PChar(env), buf, 255);
  Result := Trim(StringVal(buf));
end;

function fexists(const filename: string): boolean;
begin
  if Trim(filename) = '' then
    Result := False
  else
    Result := FileExists(filename);
end;

function fexpand(const filename: string): string;
begin
  Result := ExpandFileName(filename);
end;

procedure fdelete(const filename: string);
begin
  if fexists(filename) then
    DeleteFile(filename);
end;

function fext(const filename: string): string;
begin
  Result := ExtractFileExt(filename);
end;

function fname(const filename: string): string;
begin
  Result := ExtractFileName(filename);
end;

function fmask(const mask: string): string;
begin
  Result := mask;
  if Result = '' then
    Result := '*.*';
end;

function findfile(const mask: string): string;
var
  sr: TSearchRec;
  mask1: string;
begin
  mask1 := fmask(mask);
  if FindFirst(mask1, faAnyFile, sr) = 0 then
  begin
    Result := sr.Name;
    FindClose(sr);
  end
  else
    Result := '';
end;

function findfiles(const mask: string): TDStringList;
var
  sr: TSearchRec;
  mask1: string;
begin
  Result := TDStringList.Create;
  mask1 := fmask(mask);
  if FindFirst(mask1, faAnyFile, sr) = 0 then
  begin
    Result.Add(sr.Name);
    while FindNext(sr) = 0 do
      Result.Add(sr.Name);
    FindClose(sr);
  end;
end;


function tan(const x: extended): extended;
var
  a: single;        
  b: single;
begin
  b := cos(x);
  if b <> 0 then
  begin
    a := sin(x);
    Result := a / b;
  end
  else
    Result := 0.0;
end;

function strupper(const S: string): string;
var
  Ch: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'a') and (Ch <= 'z') then Dec(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

function strlower(const S: string): string;
var
  Ch: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'A') and (Ch <= 'Z') then Inc(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

function toupper(ch: Char): Char;
asm
{ ->    AL      Character       }
{ <-    AL      Result          }

  cmp al, 'a'
  jb  @@Exit
  cmp al, 'z'
  ja  @@Exit
  sub al, 'a' - 'A'
@@Exit:
end;

function tolower(ch: Char): Char;
asm
{ ->    AL      Character       }
{ <-    AL      Result          }

  cmp al, 'A'
  jb  @@Exit
  cmp al, 'Z'
  ja  @@Exit
  sub al, 'A' - 'a'
@@Exit:
end;

function strremovespaces(const s: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    if s[i] <> ' ' then
      Result := Result + s[i];
end;

function _SHL(const x: integer; const bits: integer): integer; assembler;
asm
  mov ecx, edx
  sal eax, cl
end;

function _SHLW(const x: LongWord; const bits: LongWord): LongWord;
begin
  Result := x shl bits;
end;

function _SHR(const x: integer; const bits: integer): integer; assembler;
asm
  mov ecx, edx
  sar eax, cl
end;

function _SHR1(const x: integer): integer; assembler;
asm
  sar eax, 1
end;

function _SHR2(const x: integer): integer; assembler;
asm
  sar eax, 2
end;

function _SHR3(const x: integer): integer; assembler;
asm
  sar eax, 3
end;

function _SHR4(const x: integer): integer; assembler;
asm
  sar eax, 4
end;

function _SHR7(const x: integer): integer; assembler;
asm
  sar eax, 7
end;

function _SHR8(const x: integer): integer; assembler;
asm
  sar eax, 8
end;

function _SHR14(const x: integer): integer; assembler;
asm
  sar eax, 14
end;

function _SHRW(const x: LongWord; const bits: LongWord): LongWord;
begin
  Result := x shr bits;
end;

function StringVal(const Str: PChar): string;
begin
  Result := Str;
end;

procedure ZeroMemory(const dest0: pointer; const count0: integer);
var
  data: union_8b;
  pdat: pointer;
  dest: PByte;
  count: integer;
begin
  if mmxMachine = 0 then
  begin
    FillChar(dest0^, count0, 0);
    Exit;
  end;

  dest := PByte(dest0);
  count := count0;

  while (count > 0) and (integer(dest) and 7 <> 0) do
  begin
    dest^ := 0;
    inc(dest);
    dec(count);
  end;

  if count = 0 then
  begin
    Exit;
  end;

  data.dwords[0] := 0;
  data.dwords[1] := 0;
  pdat := @data;

  if count >= 64 then
  begin
    asm
      push esi
      push edi

      mov edi, dest
      mov esi, pdat

      mov ecx, count
      // 64 bytes per iteration
      shr ecx, 6
      // Read in source data
      movq mm1, [esi]
      movq mm2, mm1
      movq mm3, mm1
      movq mm4, mm1
      movq mm5, mm1
      movq mm6, mm1
      movq mm7, mm1
      movq mm0, mm1
@@loop1:
      // Non-temporal stores
      movntq [edi], mm1
      movntq [edi + 8], mm2
      movntq [edi + 16], mm3
      movntq [edi + 24], mm4
      movntq [edi + 32], mm5
      movntq [edi + 40], mm6
      movntq [edi + 48], mm7
      movntq [edi + 56], mm0

      add edi, 64
      dec ecx
      jnz @@loop1

      pop edi
      pop esi
    end;

    inc(dest, count and (not 63));
    count := count and 63;
  end;

  if count >= 8 then
  begin
    asm
      push esi
      push edi

      mov edi, dest
      mov esi, pdat

      mov ecx, count
      // 8 bytes per iteration
      shr ecx, 3
      // Read in source data
      movq mm1, [esi]
@@loop2:
      // Non-temporal stores
      movntq  [edi], mm1

      add edi, 8
      dec ecx
      jnz @@loop2

      pop edi
      pop esi
    end;
    inc(dest, count and (not 7));
    count := count and 7;
  end;

  while count > 0 do
  begin
    dest^ := 0;
    inc(dest);
    dec(count);
  end;

  asm
    emms
  end;

end;

function fopen(var f: file; const FileName: string; const mode: integer): boolean;
begin
  assign(f, FileName);
  {$I-}
  if mode = fCreate then
  begin
    FileMode := 2;
    rewrite(f, 1);
  end
  else if mode = fOpenReadOnly then
  begin
    FileMode := 0;
    reset(f, 1);
  end
  else if mode = fOpenReadWrite then
  begin
    FileMode := 2;
    reset(f, 1);
  end
  else
  begin
    Result := False;
    Exit;
  end;
  {$I+}
  Result := IOresult = 0;
end;

function fsize(const FileName: string): integer;
var
  f: file;
begin
  if fopen(f, FileName, fOpenReadOnly) then
  begin
  {$I-}
    Result := FileSize(f);
    close(f);
  {$I+}
  end
  else
    Result := 0;
end;

function fshortname(const FileName: string): string;
var
  i: integer;
begin
  Result := '';
  for i := Length(FileName) downto 1 do
  begin
    if FileName[i] in ['\', '/'] then
      break;
    Result := FileName[i] + Result;
  end;
end;

function strtrim(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Result := '' else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function capitalizedstring(const S: string; const splitter: char = ' '): string;
var
  i: integer;
  c: string;
begin
  if S = '' then
  begin
    Result := '';
    Exit;
  end;

  Result := strlower(S);
  Result[1] := toupper(Result[1]);
  c := tolower(splitter);
  for i := 2 to Length(Result) do
  begin
    if Result[i - 1] = c then
      Result[i] := toupper(Result[i])
  end;
end;

procedure splitstring(const inp: string; var out1, out2: string; const splitter: char);
var
  p: integer;
begin
  p := Pos(splitter, inp);
  if p = 0 then
  begin
    out1 := inp;
    out2 := '';
  end
  else
  begin
    out1 := strtrim(Copy(inp, 1, p - 1));
    out2 := strtrim(Copy(inp, p + 1, Length(inp) - p));
  end;
end;

procedure splitstring2nd(const inp: string; var out1, out2: string; const splitter: char); overload;
var
  i: integer;
  aa: integer;
begin
  out1 := '';
  out2 := '';
  aa := 1;
  for i := 1 to length(inp) do
  begin
    if (aa < 3) and (inp[i] = splitter) then
    begin
      inc(aa);
      if aa = 2 then
        out1 := out1 + ',';
    end
    else if (aa = 1) or (aa = 2) then
      out1 := out1 + inp[i]
    else
      out2 := out2 + inp[i]
  end;
end;

procedure splitstring(const inp: string; var out1, out2, out3: string; const splitter: char); overload;
var
  i: integer;
  aa: integer;
begin
  out1 := '';
  out2 := '';
  out3 := '';
  aa := 1;
  for i := 1 to length(inp) do
  begin
    if (aa < 3) and (inp[i] = splitter) then
      inc(aa)
    else if aa = 1 then
      out1 := out1 + inp[i]
    else if aa = 2 then
      out2 := out2 + inp[i]
    else
      out3 := out3 + inp[i]
  end;
end;

procedure splitstring(const inp: string; var out1, out2, out3, out4: string; const splitter: char); overload;
var
  i: integer;
  aa: integer;
begin
  out1 := '';
  out2 := '';
  out3 := '';
  out4 := '';
  aa := 1;
  for i := 1 to length(inp) do
  begin
    if (aa < 4) and (inp[i] = splitter) then
      inc(aa)
    else if aa = 1 then
      out1 := out1 + inp[i]
    else if aa = 2 then
      out2 := out2 + inp[i]
    else if aa = 3 then
      out3 := out3 + inp[i]
    else
      out4 := out4 + inp[i]
  end;
end;

procedure splitstring(const inp: string; var out1, out2, out3, out4, out5: string; const splitter: char); overload;
var
  i: integer;
  aa: integer;
begin
  out1 := '';
  out2 := '';
  out3 := '';
  out4 := '';
  out5 := '';
  aa := 1;
  for i := 1 to length(inp) do
  begin
    if (aa < 5) and (inp[i] = splitter) then
      inc(aa)
    else if aa = 1 then
      out1 := out1 + inp[i]
    else if aa = 2 then
      out2 := out2 + inp[i]
    else if aa = 3 then
      out3 := out3 + inp[i]
    else if aa = 4 then
      out4 := out4 + inp[i]
    else
      out5 := out5 + inp[i]
  end;
end;

procedure splitstring(const inp: string; var out1, out2, out3, out4, out5, out6: string; const splitter: char); overload;
var
  i: integer;
  aa: integer;
begin
  out1 := '';
  out2 := '';
  out3 := '';
  out4 := '';
  out5 := '';
  out6 := '';
  aa := 1;
  for i := 1 to length(inp) do
  begin
    if (aa < 6) and (inp[i] = splitter) then
      inc(aa)
    else if aa = 1 then
      out1 := out1 + inp[i]
    else if aa = 2 then
      out2 := out2 + inp[i]
    else if aa = 3 then
      out3 := out3 + inp[i]
    else if aa = 4 then
      out4 := out4 + inp[i]
    else if aa = 5 then
      out5 := out5 + inp[i]
    else
      out6 := out6 + inp[i]
  end;
end;

procedure splitstring(const inp: string; var out1, out2, out3, out4, out5, out6, out7: string; const splitter: char); overload;
var
  i: integer;
  aa: integer;
begin
  out1 := '';
  out2 := '';
  out3 := '';
  out4 := '';
  out5 := '';
  out6 := '';
  out7 := '';
  aa := 1;
  for i := 1 to length(inp) do
  begin
    if (aa < 7) and (inp[i] = splitter) then
      inc(aa)
    else if aa = 1 then
      out1 := out1 + inp[i]
    else if aa = 2 then
      out2 := out2 + inp[i]
    else if aa = 3 then
      out3 := out3 + inp[i]
    else if aa = 4 then
      out4 := out4 + inp[i]
    else if aa = 5 then
      out5 := out5 + inp[i]
    else if aa = 6 then
      out6 := out6 + inp[i]
    else
      out7 := out7 + inp[i]
  end;
end;

procedure splitstring(const inp: string; var out1, out2, out3, out4, out5, out6, out7, out8: string; const splitter: char); overload;
var
  i: integer;
  aa: integer;
begin
  out1 := '';
  out2 := '';
  out3 := '';
  out4 := '';
  out5 := '';
  out6 := '';
  out7 := '';
  out8 := '';
  aa := 1;
  for i := 1 to length(inp) do
  begin
    if (aa < 8) and (inp[i] = splitter) then
      inc(aa)
    else if aa = 1 then
      out1 := out1 + inp[i]
    else if aa = 2 then
      out2 := out2 + inp[i]
    else if aa = 3 then
      out3 := out3 + inp[i]
    else if aa = 4 then
      out4 := out4 + inp[i]
    else if aa = 5 then
      out5 := out5 + inp[i]
    else if aa = 6 then
      out6 := out6 + inp[i]
    else if aa = 7 then
      out7 := out7 + inp[i]
    else
      out8 := out8 + inp[i]
  end;
end;

procedure splitstring(const inp: string; var out1, out2: string; const splitters: charset_t);
var
  i: integer;
  p: integer;
  inp1: string;
begin
  inp1 := inp;
  for i := 1 to Length(inp1) do
    if inp1[i] in splitters then
      inp1[i] := ' ';
  p := CharPos(' ', inp1);
  if p = 0 then
  begin
    out1 := inp1;
    out2 := '';
  end
  else
  begin
    out1 := strtrim(Copy(inp1, 1, p - 1));
    out2 := strtrim(Copy(inp1, p + 1, Length(inp) - p));
  end;
end;

procedure splitstring(const inp: string; var out1, out2, out3: string; const splitters: charset_t); 
var
  tmp: string;
begin
  splitstring(inp, out1, tmp, splitters);
  splitstring(tmp, out2, out3, splitters);
end;

function firstword(const inp: string; const splitter: char = ' '): string;
var
  tmp: string;
begin
  splitstring(inp, Result, tmp, splitter);
end;

function firstword(const inp: string; const splitters: charset_t): string; overload;
var
  tmp: string;
begin
  splitstring(inp, Result, tmp, splitters);
end;

function secondword(const inp: string; const splitter: char = ' '): string;
var
  tmp1, tmp2: string;
begin
  splitstring(inp, tmp1, Result, tmp2, splitter);
end;

function secondword(const inp: string; const splitters: charset_t): string; 
var
  tmp1, tmp2: string;
begin
  splitstring(inp, tmp1, Result, tmp2, splitters);
end;

function lastword(const inp: string; const splitter: char = ' '): string;
var
  i: integer;
begin
  Result := '';
  i := length(inp);
  while i > 0 do
  begin
    if inp[i] = splitter then
      Exit
    else
    begin
      Result := inp[i] + Result;
      dec(i);
    end;
  end;
end;

function lastword(const inp: string; const splitters: charset_t): string; overload;
var
  i: integer;
begin
  Result := '';
  i := length(inp);
  while i > 0 do
  begin
    if inp[i] in splitters then
      Exit
    else
    begin
      Result := inp[i] + Result;
      dec(i);
    end;
  end;
end;

procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;

function StrLCopy(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EBX,ECX
        XOR     AL,AL
        TEST    ECX,ECX
        JZ      @@1
        REPNE   SCASB
        JNE     @@1
        INC     ECX
@@1:    SUB     EBX,ECX
        MOV     EDI,ESI
        MOV     ESI,EDX
        MOV     EDX,EDI
        MOV     ECX,EBX
        SHR     ECX,2
        REP     MOVSD
        MOV     ECX,EBX
        AND     ECX,3
        REP     MOVSB
        STOSB
        MOV     EAX,EDX
        POP     EBX
        POP     ESI
        POP     EDI
end;

function fabs(const f: float): float;
{var
  tmp: integer;
begin
  tmp := PInteger(@f)^;
  tmp := tmp and $7FFFFFFF;
  Result := Pfloat(@tmp)^;
end;}
begin
  if f >= 0 then
    Result := f
  else
    Result := -f;
end;

procedure MakeDir(const dir: string);
begin
  CreateDir(dir);
end;

function PascalText(src: PChar): string;
var
  prev: char;
begin
  Result := '';
  if src^ = #0 then
    Exit;
  repeat
    prev := src^;
    inc(src);
    if (src^ = #10) and (prev <> #13) then
      Result := Result + prev + #13#10
    else if not (prev in [#10, #13]) then
      Result := Result + prev;
  until src^ = #0;
end;

function CopyFile(const sname, dname: string): boolean;
var
  FromF, ToF: file;
  NumRead, NumWritten: Integer;
  Buf: array[1..8192] of Char;
  adir: string;
begin
  Result := False;

  if (Trim(sname) = '') or (Trim(dname) = '') then
    Exit;

  if fexists(sname) then
  begin
    {$I-}
    AssignFile(FromF, sname);
    Reset(FromF, 1);
    adir := Trim(ExtractFilePath(dname));
    if adir <> '' then
      if not DirectoryExists(adir) then
        ForceDirectories(adir);
    AssignFile(ToF, dname);
    Rewrite(ToF, 1);
    repeat
      BlockRead(FromF, Buf, SizeOf(Buf), NumRead);
      BlockWrite(ToF, Buf, NumRead, NumWritten);
    until (NumRead = 0) or (NumWritten <> NumRead);
    CloseFile(FromF);
    CloseFile(ToF);
    {$I+}
    Result := IOResult = 0;
  end;
end;

function CacheReadFile(const fname: string): boolean;
var
  f: file;
  NumRead: Integer;
  Buf: PByteArray;
  sz: integer;
begin
  Result := False;

  if Trim(fname) = '' then
    Exit;

  if fexists(fname) then
  begin
    sz := 4 * 1024 * 1024;
    GetMem(Buf, sz);
    {$I-}
    AssignFile(f, fname);
    Reset(f, 1);
    repeat
      BlockRead(f, Buf^, sz, NumRead);
    until NumRead = 0;
    CloseFile(f);
    {$I+}
    FreeMem(Buf, sz);
    Result := IOResult = 0;
  end;
end;

function HexToInt(const s: string): integer;
var
  tmp: string;
begin
  if s = '' then
  begin
    Result := 0;
    Exit;
  end;
  tmp := s;
  if tmp[1] = '#' then
    tmp[1] := '$'
  else
    tmp := '$' + tmp;
  Result := StrToIntDef(tmp, 0);
end;

function IsNumeric(const s: string): boolean;
var
  i: integer;
begin
  for i := 1 to Length(s) do
    if not (s[i] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) then
    begin
      Result := False;
      Exit;
    end;

  Result := True;
end;

function IsNumericC(const c: char): boolean;
begin
  Result := c in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
end;

function CountNumbers(const s: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    if s[i] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] then
      inc(Result);
end;

function between(const x: double; const x1, x2: double): Boolean;
begin
  if x1 < x2 then
    Result := (x >= x1) and (x <= x2)
  else
    Result := (x >= x2) and (x <= x1);
end;

constructor TDouble.Create(const avalue: double = 0.0);
begin
  fvalue := avalue;
  Inherited Create;
end;

procedure TDouble.Add(const avalue: double);
begin
  fvalue := fvalue + avalue;
end;

constructor TCInteger.Create(const avalue: integer = 0);
begin
  fvalue := avalue;
  Inherited Create;
end;

procedure TCInteger.Add(const avalue: integer);
begin
  fvalue := fvalue + avalue;
end;

procedure TCInteger.IncValue;
begin
  inc(fvalue);
end;

function string2stringlist(const s: string; const c: char): TStringList;
var
  i: integer;
  tmp: string;
begin
  tmp := '';
  for i := 1 to length(s) do
  begin
    if s[i] = c then
      tmp := tmp + #13#10
    else
      tmp := tmp + s[i];
  end;
  Result := TStringList.Create;
  Result.Text := tmp;
end;

function stringlist2string(const s: TStringList; const c: char): string;
var
  i: integer;
begin
  Result := '';
  if s <> nil then
    for i := 0 to s.Count - 1 do
    begin
      if i = 0 then
        Result := s.Strings[0]
      else
        Result := Result + c + s.Strings[i];
    end;
end;

const
  EPSILON_DBL = 0.001;

function dbl_equal(const dbl1, dbl2: double): boolean;
begin
  Result := abs(dbl1 - dbl2) < EPSILON_DBL;
end;

function dbl_safe_div(const a, b: double): double;
begin
  if dbl_equal(b, 0.0) then
  begin
    Result := 0.0;
    Exit;
  end;

  Result := a / b;
end;

procedure QSortIntegers(const A: PIntegerArray; const Len: integer);

  procedure qsortI(l, r: Integer);
  var
    i, j: integer;
    t: integer;
    d: integer;
  begin
    repeat
      i := l;
      j := r;
      d := A[(l + r) shr 1];
      repeat
        while A[i] < d do
          inc(i);
        while A[j] > d do
          dec(j);
        if i <= j then
        begin
          t := A[i];
          A[i] := A[j];
          A[j] := t;
          inc(i);
          dec(j);
        end;
      until i > j;
      if l < j then
        qsortI(l, j);
      l := i;
    until i >= r;
  end;

begin
  if Len > 1 then
    qsortI(0, Len - 1);
end;

function SaveStringToFile(const fname: string; const s: string): boolean;
var
  f: file;
begin
{$I-}
  AssignFile(f, fname);
  Rewrite(f, 1);
  BlockWrite(f, Pointer(s)^, Length(s));
  CloseFile(f);
{$I+}
  Result := IOResult = 0;
end;

function LoadStringFromFile(const fname: string): string;
var
  f: file;
  NumRead: Integer;                               
  Buf: array[1..8192] of Char;
  i: integer;
begin
  Result := '';
  if fexists(fname) then
  begin
    AssignFile(f, fname);
    Reset(f, 1);
    repeat
      BlockRead(f, Buf, SizeOf(Buf), NumRead);
      for i := 1 to NumRead do
        Result := Result + Buf[i];
    until NumRead = 0;
    CloseFile(f);
  end;
end;

function SaveDataToFile(const fname: string; const fdata: pointer; const size: integer): boolean;
var
  f: file;
begin
{$I-}
  AssignFile(f, fname);
  Rewrite(f, 1);
  BlockWrite(f, PByteArray(fdata)^, size);
  CloseFile(f);
{$I+}
  Result := IOResult = 0;
end;

function IsIntegerInRange(const test, f1, f2: integer): boolean;
begin
  if f1 < f2 then
    Result := (test >= f1) and (test <= f2)
  else
    Result := (test >= f2) and (test <= f1)
end;

function btoi(const b: boolean): integer;
begin
  if b then
    Result := 1
  else
    Result := 0;
end;

function itob(const i: integer): boolean;
begin
  Result := i <> 0;
end;

function btoa(const b: boolean): string;
begin
  if b then
    Result := '1'
  else
    Result := '0';
end;

function atob(const s: string): boolean;
begin
  Result := (s = '1') or (strupper(s) = 'TRUE');
end;

function RemoveSpaces(const s: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    if s[i] <> ' ' then
      Result := Result + s[i];
end;

function CharPos(const ch: Char; const s: string): integer;
var
  i: integer;
begin
  for i := 1 to Length(s) do
    if s[i] = ch then
    begin
      result := i;
      exit;
    end;
  result := 0;
end;

function Pos1(const subs, s: string): boolean;
var
  len1, len1b, len2: integer;
  i: integer;
begin
  len1 := Length(subs);
  len2 := Length(s);
  if len2 < len1 then
  begin
    Result := False;
    Exit;
  end;

  len1b := len1 - 4;
  i := 1;
  while i <= len1b do
  begin
    if PLongWord(@subs[i])^ <> PLongWord(@s[i])^ then
    begin
      Result := False;
      Exit;
    end;
    Inc(i, 4);
  end;

  while i <= len1 do
  begin
    if subs[i] <> s[i] then
    begin
      Result := False;
      Exit;
    end;
    Inc(i);
  end;

  Result := True;
end;

function MakePathString(const str: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(str) do
  begin
    if CharPos(str[i], 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_') > 0 then
      Result := Result + str[i]
    else
      Result := Result + '(' + itoa(Ord(str[i])) + ')';
  end;
end;

begin
  DecimalSeparator := '.';

end.

