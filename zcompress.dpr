program zcompress;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, zlib;

procedure compress_file_ZIP(const fin, fout: string);
var
  f: TFileStream;
  buffer: string;
  outp: pointer;
  outsz: integer;
  len: integer;
begin
  f := TFileStream.Create(fin, fmOpenRead or fmShareDenyWrite);
  try
    len := f.Size;
    SetLength(buffer, len);
    f.Read((@buffer[1])^, f.Size);
  finally
    f.Free;
  end;

  CompressBuf(@buffer[1], len, outp, outsz);

  f := TFileStream.Create(fout, fmCreate);
  try
    f.Write(outsz, SizeOf(outsz));
    f.Write(outp^, outsz);
  finally
    f.Free;
  end;
  FreeMem(outp, outsz);
end;

function CheckParam(const parm: string): integer;
var
  i: integer;
begin
  for i := 1 to ParamCount do
    if parm = ParamStr(i) then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

procedure Readme;
begin
  writeln('zcompress v.1.0');
  writeln('Usage:');
  writeln('zcompress -[c/d] -infile FILEIN -outfile FILEOUT');
end;

var
  fin, fout: string;
  p: integer;
begin
  p := CheckParam('-infile');
  if (p < 0) or (p >= ParamCount) then
  begin
    Readme;
    Halt(1);
  end;
  fin := ParamStr(p + 1);

  p := CheckParam('-outfile');
  if (p < 0) or (p >= ParamCount) then
  begin
    Readme;
    Halt(1);
  end;
  fout := ParamStr(p + 1);

  p := CheckParam('-c');
  if p > 0 then
  begin
    compress_file_ZIP(fin, fout);
    Halt(0);
  end;
  
end.
