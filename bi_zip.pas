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
//    ZIP file format
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_zip;

interface

uses
  bi_delphi, Classes;

const
  ZIPFILESIGNATURE = $04034b50;
  ZIPARCIEVESIGNATURE = $08064b50;

type
  TZipFileHeader = packed record
    Signature: integer; // $04034b50
    Version: word;
    BitFlag: word;
    CompressionMethod: word;
    DosDate: integer;
    crc32: integer;
    CompressedSize: integer;
    UnCompressedSize: integer;
    FileNameLen: word;
    ExtraFieldLen: word;
  end;

// This descriptor exists only if bit 3 of the general
// purpose bit flag is set (see below).
  TZipFileDescriptor = record
    crc32: integer;
    CompressedSize: integer;
    UnCompressedSize: integer;
  end;

  TZipArchieveExtraDataRecord = record
    Signature: integer; // $08064b50
    ExtraFieldLen: integer;
  end;

  TZipFile = class
  private
    fFileName: string;
    fFiles: TDStringList;
    f: TFileStream;
  protected
    function GetFile(Index: Integer): string; virtual;
    procedure Load; virtual;
    procedure Clear; virtual;
    procedure SetFileName(const Value: string); virtual;
    function GetFileCount: integer;
  public
    constructor Create(const aFileName: string); virtual;
    destructor Destroy; override;
    function GetZipFileData(const Index: integer; var p: pointer;
      var size: integer): boolean; overload; virtual;
    function GetZipFileData(const Name: string; var p: pointer;
      var size: integer): boolean; overload; virtual;
    property FileName: string read fFileName write SetFileName;
    property Files[Index: Integer]: string read GetFile;
    property FileCount: integer read GetFileCount;
  end;

type
  TZAlloc = function (opaque: Pointer; items, size: Integer): Pointer;
  TZFree  = procedure (opaque, block: Pointer);

  TZStreamRec = packed record
    next_in  : PChar;     // next input byte
    avail_in : Longint;   // number of bytes available at next_in
    total_in : Longint;   // total nb of input bytes read so far

    next_out : PChar;     // next output byte should be put here
    avail_out: Longint;   // remaining free space at next_out
    total_out: Longint;   // total nb of bytes output so far

    msg      : PChar;     // last error message, NULL if no error
    state    : Pointer;   // not visible by applications

    zalloc   : TZAlloc;   // used to allocate the internal state
    zfree    : TZFree;    // used to free the internal state
    opaque   : Pointer;   // private data object passed to zalloc and zfree

    data_type: Integer;   // best guess about the data type: ascii or binary
    adler    : Longint;   // adler32 value of the uncompressed data
    reserved : Longint;   // reserved for future use
  end;

function inflate(var strm: TZStreamRec; flush: Integer): Integer;

function inflateInit2_(var strm: TZStreamRec; windowBits: Integer;
  version: PChar; recsize: Integer): Integer;

function deflateInit_(var strm: TZStreamRec; level: Integer; version: PChar;
  recsize: Integer): Integer;

function deflate(var strm: TZStreamRec; flush: Integer): Integer;

function deflateEnd(var strm: TZStreamRec): Integer;

function inflateInit_(var strm: TZStreamRec; version: PChar;
  recsize: Integer): Integer;

function inflateEnd(var strm: TZStreamRec): Integer;

const
  ZLIB_VERSION = '1.2.3';


implementation

uses
  bi_system;


{$L obj/z123_deflate}
{$L obj/z123_inflate}
{$L obj/z123_inftrees}
{$L obj/z123_infback}
{$L obj/z123_inffast}
{$L obj/z123_trees}
{$L obj/z123_compress}
{$L obj/z123_adler32}
{$L obj/z123_crc32}

const
  {** flush constants *******************************************************}

  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;
  Z_BLOCK         = 5;

const
  _z_errmsg: array[0..9] of PChar = (
    'need dictionary',      // Z_NEED_DICT      (2)
    'stream end',           // Z_STREAM_END     (1)
    'ok',                   // Z_OK             (0)
    'file error',           // Z_ERRNO          (-1)
    'stream error',         // Z_STREAM_ERROR   (-2)
    'data error',           // Z_DATA_ERROR     (-3)
    'insufficient memory',  // Z_MEM_ERROR      (-4)
    'buffer error',         // Z_BUF_ERROR      (-5)
    'incompatible version', // Z_VERSION_ERROR  (-6)
    ''
  );

function zcalloc(opaque: Pointer; items, size: Integer): Pointer;
begin
  GetMem(result, items * size);
end;

procedure zcfree(opaque, block: Pointer);
begin
  FreeMem(block);
end;

procedure _memcpy(dest, source: Pointer; count: Integer); cdecl;
begin
  memcpy(dest, source, count);
end;

{** c function implementations **********************************************}

procedure _memset(p: Pointer; b: Byte; count: Integer); cdecl;
begin
  memset(p, b, count);
end;

function inflate(var strm: TZStreamRec; flush: Integer): Integer; external;

function inflateInit2_(var strm: TZStreamRec; windowBits: Integer;
  version: PChar; recsize: Integer): Integer; external;

function deflateInit_(var strm: TZStreamRec; level: Integer; version: PChar;
  recsize: Integer): Integer; external;

function deflate(var strm: TZStreamRec; flush: Integer): Integer; external;

function deflateEnd(var strm: TZStreamRec): Integer; external;

function inflateInit_(var strm: TZStreamRec; version: PChar;
  recsize: Integer): Integer; external;

function inflateEnd(var strm: TZStreamRec): Integer; external;

function InflateInit2(var stream: TZStreamRec; windowBits: Integer): Integer;
begin
  result := inflateInit2_(stream,windowBits, ZLIB_VERSION, SizeOf(TZStreamRec));
end;

procedure ZDecompress2(const inBuffer: Pointer; const inSize: Integer;
  const outSize: Integer; out outBuffer: Pointer);
var
  zstream: TZStreamRec;

  procedure CheckErr(err: integer);
  begin
    if err < 0 then
      I_Error('ZDecompress2(): Zip file error(%d)', [err]);
  end;

begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  outBuffer := malloc(outSize);

  CheckErr(InflateInit2(zstream, -15));


  zstream.next_in := inBuffer;
  zstream.avail_in := inSize;
  zstream.next_out := outBuffer;
  zstream.avail_out := outSize;

  CheckErr(inflate(zstream, Z_SYNC_FLUSH));

  inflateEnd(zstream);
end;

//------------------------------------------------------------------------------
type
  TZipFileEntryInfo = class
  private
    fSize: integer;
    fCompressedSize: integer;
    fPosition: integer;
    fCompressed: boolean;
  public
    constructor Create(const aSize, aCompressedSize, aPosition: integer;
      aCompressed: boolean); virtual;
    property Size: integer read fSize;
    property CompressedSize: integer read fCompressedSize;
    property Position: integer read fPosition;
    property Compressed: boolean read fCompressed;
  end;

constructor TZipFileEntryInfo.Create(const aSize, aCompressedSize, aPosition: integer;
      aCompressed: boolean);
begin
  fSize := aSize;
  fCompressedSize := aCompressedSize;
  fPosition := aPosition;
  fCompressed := aCompressed;
end;

//------------------------------------------------------------------------------
constructor TZipFile.Create(const aFileName: string);
begin
  Inherited Create;
  fFiles := TDStringList.Create;
  fFileName := aFileName;
  Load;
end;

destructor TZipFile.Destroy;
begin
  Clear;
  fFiles.Free;
  Inherited Destroy;
end;

function TZipFile.GetZipFileData(const Index: integer; var p: pointer;
  var size: integer): boolean;
var
  tmp: pointer;
  zinf: TZipFileEntryInfo;
  csize: integer;
begin
  if (Index >= 0) and (Index < fFiles.Count) then
  begin
    zinf := (fFiles.Objects[Index] as TZipFileEntryInfo);
    if zinf.Compressed then
    begin
      size := zinf.Size;
      csize := zinf.CompressedSize;
      tmp := malloc(csize);
      try
        f.Seek(zinf.Position, sFromBeginning);
        f.Read(tmp^, csize);
        ZDecompress2(tmp, csize, size, p);
      finally
        memfree(tmp, csize);
      end;
      result := true;
    end
    else
    begin
      size := zinf.Size;
      p := malloc(size);
      f.Seek(zinf.Position, sFromBeginning);
      f.Read(p^, size);
      result := true;
    end;
  end
  else
    result := false;
end;

function TZipFile.GetZipFileData(const Name: string; var p: pointer;
  var size: integer): boolean;
var
  Name2: string;
  i: integer;
begin
  Name2 := strupper(Name);
  for i := 1 to Length(Name) do
    if Name2[i] = '/' then
      Name2[i] := '\';
  result := GetZipFileData(fFiles.IndexOf(Name2), p, size);
end;

function TZipFile.GetFile(Index: Integer): string;
begin
  result := fFiles[Index];
end;

procedure TZipFile.Load;
var
  h: TZipFileHeader;
  str: string;
  i: integer;
begin
  Clear;
  if fFileName <> '' then
  begin
    f := TFileStream.Create(fFileName, fOpenReadOnly);
    while true do
    begin
      f.Read(h, SizeOf(h));
      if h.Signature = ZIPFILESIGNATURE then
      begin
        SetLength(str, h.FileNameLen);
        if h.FileNameLen > 0 then
        begin
          f.Read((@str[1])^, h.FileNameLen);
          str := strupper(str);
          for i := 1 to h.FileNameLen do
            if str[i] = '/' then
              str[i] := '\';
          fFiles.Objects[fFiles.Add(str)] :=
            TZipFileEntryInfo.Create(h.UnCompressedSize, h.CompressedSize,
              f.Position + h.ExtraFieldLen, h.CompressionMethod > 0);
          if (h.BitFlag and $4) <> 0 then
            f.Seek(h.ExtraFieldLen + h.CompressedSize + SizeOf(TZipFileDescriptor), sFromCurrent)
          else
            f.Seek(h.ExtraFieldLen + h.CompressedSize, sFromCurrent);
        end;
      end
      else
        break;
    end;
  end;
end;

procedure TZipFile.Clear;
var
  i: integer;
begin
  for i := 0 to fFiles.Count - 1 do
    fFiles.Objects[i].Free;
  fFiles.Clear;
  f.Free;
end;

procedure TZipFile.SetFileName(const Value: string);
begin
  if fFileName <> Value then
  begin
    fFileName := Value;
    Load;
  end;
end;

function TZipFile.GetFileCount: integer;
begin
  result := fFiles.Count;
end;

end.

