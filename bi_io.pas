unit bi_io;

interface

uses
  bi_delphi, Classes;

var
  debugfile: TFileStream = nil;
  stderr: TFileStream = nil;
  stdout: TFileStream = nil;
  stdoutbuffer: TDStringList = nil;

procedure I_InitializeIO;

procedure I_ShutDownIO;

procedure I_IOMessageBox(const s: string);

procedure I_IOErrorMessageBox(const s: string);

procedure I_IOprintf(const s: string);

implementation

uses
  Windows, Forms, main;

procedure I_IOMessageBox(const s: string);
begin
  MessageBox(MainForm.Handle, PChar(s), PChar(Application.Title), MB_OK);
end;

procedure I_IOErrorMessageBox(const s: string);
begin
  MessageBox(MainForm.Handle, PChar(s), PChar(Application.Title), MB_OK or MB_ICONERROR or MB_APPLMODAL);
end;

var
  io_lastNL: boolean = true;

procedure I_IOprintf(const s: string);
var
  p: integer;
  do_add: boolean;
  len: integer;
begin
  len := Length(s);
  if len = 0 then
    exit;

  do_add := false;
  if io_lastNL then
  begin
    p := Pos(#10, s);
    if (p = 0) or (p = len) then
      do_add := true
  end;

  io_lastNL := s[len] = #10;

  if do_add then
  begin
    if len >= 2 then
    begin
      if s[len - 1] = #13 then
      begin
        stdoutbuffer.Add(Copy(s, 1, len - 2));
      end
      else
      begin
        stdoutbuffer.Add(Copy(s, 1, len - 1));
      end
    end
    else
    begin
      stdoutbuffer.Add('');
    end;
  end
  else
  begin
    stdoutbuffer.Text := stdoutbuffer.Text + s;
  end;


  if IsConsole then
    write(s);
end;

procedure I_AddText(const txt: string);
begin
  if stdout = nil then
    Exit;
  fprintf(stdout, txt);
end;

procedure I_InitializeIO;
var
  dfilename: string;
  efilename: string;
  sfilename: string;
begin
  dfilename := 'bi_debug.txt';
  efilename := 'bi_stderr.txt';
  sfilename := 'bi_stdout.txt';

  printf(' error output to: %s' + #13#10, [efilename]);
  stderr := TFileStream.Create(efilename, fCreate);
  printf(' debug output to: %s' + #13#10, [dfilename]);
  debugfile := TFileStream.Create(dfilename, fCreate);
  printf(' standard output to: %s' + #13#10, [sfilename]);
  stdout := TFileStream.Create(sfilename, fCreate);

  fprintf(stdout, stdoutbuffer.Text);

  outproc := @I_AddText;
end;


procedure I_ShutDownIO;
begin
  stderr.Free;
  debugfile.Free;
  stdout.Free;
end;

initialization

  stdoutbuffer := TDStringList.Create;

finalization

  stdoutbuffer.Free;

end.
