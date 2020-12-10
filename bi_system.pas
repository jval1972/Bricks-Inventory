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
//    System functions
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_system;

interface

procedure I_Init;

procedure I_Quit;

procedure I_FlashCachedOutput;

procedure I_Error(const error: string; const Args: array of const); overload;

procedure I_Error(const error: string); overload;

procedure I_Warning(const warning: string; const Args: array of const); overload;

procedure I_Warning(const warning: string); overload;

procedure I_BeginDiskBusy;

function I_IsCDRomDrive(const drive: char = #0): boolean;

function I_GetExeImageSize(fname: string = ''): LongWord;

function I_VersionBuilt(fname: string = ''): string;

function I_DirectoryExists(const Name: string): Boolean;

procedure I_SetCriticalCPUPriority;

procedure I_SetNormalCPUPriority;

procedure I_DetectOS;

procedure I_DetectCPU;

procedure I_ClearInterface(var Dest: IInterface);

type
  process_t = function(p: pointer): LongInt; stdcall;

function I_CreateProcess(p: process_t; parm: pointer; suspended: boolean): integer;

procedure I_WaitForProcess(pid: integer; msec: integer);

procedure I_GoToWebPage(const cmd: string);

function I_GetTime: integer;

type
  osplatform_t = (os_unknown, os_Win95, os_WinNT4, os_Win2k);

var
  osplatform: osplatform_t;

var
  isdiskbusy: boolean = false;

var
  safemode: boolean = false;
  usemmx: boolean = true;
  usemultithread: boolean;
  criticalcpupriority: boolean;

function I_GetNumCPUs: integer;

const
  TICRATE = 200;

implementation

uses
  Windows, Messages, bi_delphi, bi_io, bi_tmp;

//
// I_GetTime
// returns time in 1/70th second tics
//
var
  basetime: int64;
  Freq: int64;

function I_GetSysTime: extended;
var
  _time: int64;
begin
  if Freq = 1000 then
    _time := GetTickCount
  else
  begin
    if not QueryPerformanceCounter(_time) then
    begin
      _time := GetTickCount;
      Freq := 1000;
      basetime := 0;
      I_Warning('QueryPerformanceCounter() failed, basetime reset.'#13#10);
    end;
  end;
  if basetime = 0 then
    basetime := _time;
  Result := (_time - basetime) / Freq;
end;

function I_GetTime: integer;
begin
  Result := trunc(I_GetSysTime * TICRATE);
end;

//
// I_Init
//
procedure I_Init;
begin
  outproc := @I_IOprintf;
  printf('I_Init: Initializing BrickInventory'#13#10);
  printf('I_DetectOS: Detecting operating system.'#13#10);
  I_DetectOS;
  printf('I_DetectCPU: Detecting CPU extensions.'#13#10);
  I_DetectCPU;
  I_InitializeIO;
  I_InitTempFiles;
end;

//
// I_Quit
//
procedure I_Quit;
begin
  printf('I_Quit: Terminating BrickInventory'#13#10);
  I_ShutDownTempFiles;
  I_ShutDownIO;
end;

procedure I_RestoreDesktop;
begin
  InvalidateRect(0, nil, true)
end;

procedure I_Destroy(const code: integer);
begin
  I_ShutDownTempFiles;
  Halt(code);
end;

procedure I_FlashCachedOutput;
begin
  if stdoutbuffer <> nil then
    stdoutbuffer.SaveToFile('bi_stdout.cachedbuffer.txt');
end;

//
// I_Error
//
var
  in_i_error: boolean = false;

procedure I_Error(const error: string; const Args: array of const);
var
  soutproc: TOutProc;
begin
// JVAL: Avoid recursive calls
  if stderr = nil then
    Exit;

  if in_i_error then
    Exit;

  I_FlashCachedOutput;

  in_i_error := true;

  fprintf(stderr, 'I_Error: ' + error + #13#10, Args);

  soutproc := outproc;
  outproc := I_IOErrorMessageBox;
  printf(error, Args);
  outproc := soutproc;
  printf('I_Error: ' + error + #13#10, Args);

  I_Destroy(1);
end;

procedure I_Error(const error: string);
begin
  I_Error(error, []);
end;

procedure I_Warning(const warning: string; const Args: array of const);
var
  msg: string;
begin
  sprintf(msg, warning, Args);
  I_Warning(msg);
end;

procedure I_Warning(const warning: string);
var
  wrstr: string;
begin
  if stderr = nil then
    Exit;
  wrstr := 'I_Warning: ' + warning;
  fprintf(stderr, wrstr);
  printf(wrstr);
end;

procedure I_BeginDiskBusy;
begin
  isdiskbusy := true;
end;

function I_IsCDRomDrive(const drive: char = #0): boolean;
var
  drv: array[0..3] of char;
  prm: string;
  i: integer;
begin
  if drive = #0 then
  begin
    prm := ParamStr(0);
    if length(prm) > 4 then
    begin
      for i := 0 to 2 do
        drv[i] := prm[i + 1];
      drv[3] := #0;
      Result := GetDriveType(drv) = DRIVE_CDROM;
    end
    else
      Result := GetDriveType(nil) = DRIVE_CDROM
  end
  else
  begin
    drv[0] := drive;
    drv[1] := ':';
    drv[2] := '\';
    drv[3] := #0;
    Result := GetDriveType(drv) = DRIVE_CDROM;
  end;
end;

const
  IMAGE_NT_OPTIONAL_HDR32_MAGIC = $10b;

function I_GetOptHeader(PEOptHeader: PImageOptionalHeader; fname: string = ''): boolean;
var
  f: file;
  PEHeaderOffset, PESig: Cardinal;
  EXESig: Word;
  PEHeader: TImageFileHeader;
begin
  if fname = '' then
    fname := ParamStr(0);

  if not fopen(f, fname, fOpenReadOnly) then
  begin
    Result := false;
    Exit;
  end;

  {$I-}
  BlockRead(f, EXESig, SizeOf(EXESig));
  if EXESig <> $5A4D {'MZ'} then
  begin
    close(f);
    Result := false;
    Exit;
  end;
  seek(f, $3C);
  BlockRead(f, PEHeaderOffset, SizeOf(PEHeaderOffset));
  if PEHeaderOffset = 0 then
  begin
    close(f);
    Result := false;
    Exit;
  end;
  seek(f, PEHeaderOffset);
  BlockRead(f, PESig, SizeOf(PESig));
  if PESig <> $00004550 {'PE'#0#0} then
  begin
    close(f);
    Result := false;
    Exit;
  end;
  BlockRead(f, PEHeader, SizeOf(PEHeader));
  if PEHeader.SizeOfOptionalHeader <> SizeOf(TImageOptionalHeader) then
  begin
    close(f);
    Result := false;
    Exit;
  end;
  BlockRead(f, PEOptHeader^, SizeOf(TImageOptionalHeader));
  if PEOptHeader.Magic <> IMAGE_NT_OPTIONAL_HDR32_MAGIC then
  begin
    close(f);
    Result := false;
    Exit;
  end;
  close(f);
  {$I+}
  Result := IOResult = 0;
end;

function I_GetExeImageSize(fname: string = ''): LongWord;
var
  PEOptHeader: TImageOptionalHeader;
begin
  if I_GetOptHeader(@PEOptHeader, fname) then
    Result := PEOptHeader.SizeOfImage
  else
    Result := 0;
end;

function I_VersionBuilt(fname: string = ''): string;
var
  vsize: LongWord;
  zero: LongWord;
  buffer: PByteArray;
  res: pointer;
  len: LongWord;
  i: integer;
begin
  if fname = '' then
    fname := ParamStr(0);
  vsize := GetFileVersionInfoSize(PChar(fname), zero);
  if vsize = 0 then
  begin
    Result := '';
    Exit;
  end;

  buffer := PByteArray(malloc(vsize + 1));
  GetFileVersionInfo(PChar(fname), 0, vsize, buffer);
  VerQueryValue(buffer, '\StringFileInfo\040904E4\FileVersion', res, len);
  Result := '';
  for i := 0 to len - 1 do
  begin
    if PChar(res)^ = #0 then
      break;
    Result := Result + PChar(res)^;
    res := pointer(integer(res) + 1);
  end;
  memfree(pointer(buffer), vsize + 1);
end;

function I_DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

var
  hThread: THandle;
  iPriority: integer = THREAD_PRIORITY_ERROR_RETURN;
  prioritycheck: integer = 0;

procedure I_SetCriticalCPUPriority;
begin
  if prioritycheck = 0 then
  begin
    iPriority := GetThreadPriority(hThread);
    if iPriority <> THREAD_PRIORITY_ERROR_RETURN then
      SetThreadPriority(hThread, THREAD_PRIORITY_TIME_CRITICAL);
    prioritycheck := 1;
  end;
end;

procedure I_SetNormalCPUPriority;
begin
  if prioritycheck = 1 then
  begin
    if iPriority <> THREAD_PRIORITY_ERROR_RETURN then
      SetThreadPriority(hThread, iPriority);
    prioritycheck := 0;
  end;
end;

procedure I_DetectOS;
var
  info: TOSVersionInfo;
  osname: string;
  osbuilt: integer;
begin
  memset(@info, 0, SizeOf(TOSVersionInfo));
  info.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(info);
  osname := '';
  case info.dwPlatformId of
    VER_PLATFORM_WIN32_WINDOWS:
      begin
        osplatform := os_Win95;
        if  info.dwMinorVersion < 10 then
          osname := '95'
        else if info.dwMinorVersion < 90 then
          osname := '98'
        else
          osname := 'Me';
      end;
    VER_PLATFORM_WIN32_NT:
      begin
        if info.dwMajorVersion < 5 then
          osplatform := os_WinNT4
        else
          osplatform := os_Win2k;
        if info.dwMajorVersion < 5 then
          osname := 'NT'
        else if info.dwMajorVersion = 5 then
        begin
          if info.dwMinorVersion = 0 then
            osname := '2000'
          else if info.dwMinorVersion = 1 then
            osname := 'XP'
          else if info.dwMinorVersion = 2 then
            osname := 'Server 2003';
        end
        else if (info.dwMajorVersion = 6) and (info.dwMinorVersion = 0) then
          osname := 'Vista';
      end;
    else
      begin
        OSPlatform := os_unknown;
        osname := 'Unknown OS';
      end;
  end;

  if osplatform = os_Win95 then
    osbuilt := info.dwBuildNumber and $FFFF
  else
    osbuilt := info.dwBuildNumber;
  printf(' OS: Windows %s %u.%u (Build %u)'#13#10, [osname,
      info.dwMajorVersion, info.dwMinorVersion, osbuilt]);
  if info.szCSDVersion[0] <> #0 then
    printf('     %s'#13#10, [info.szCSDVersion]);

end;

procedure I_ClearInterface(var Dest: IInterface);
var
  P: Pointer;
begin
  if safemode then
    Exit;
  if Dest <> nil then
  begin
    P := Pointer(Dest);
    Pointer(Dest) := nil;
    IInterface(P)._Release;
  end;
end;

function I_CreateProcess(p: process_t; parm: pointer; suspended: boolean): integer;
var
  id: LongWord;
begin
  if suspended then
    Result := CreateThread(nil, $1000, @p, parm, CREATE_SUSPENDED, id)
  else
    Result := CreateThread(nil, $1000, @p, parm, 0, id);
end;

procedure I_WaitForProcess(pid: integer; msec: integer);
begin
  WaitForSingleObject(pid, msec);
end;

type
  shellexecute_t = function (hWnd: HWND; Operation, FileName, Parameters,
    Directory: PChar; ShowCmd: Integer): HINST; stdcall;

//
// JVAL
// Dynamically get ShellExecute function to avoid malicius detection of
// some antivirus programs
//     
procedure I_GoToWebPage(const cmd: string);
var
  shellexecutefunc: shellexecute_t;
  inst: THandle;
begin
  inst := LoadLibrary('shell32');
  shellexecutefunc := GetProcAddress(inst, 'ShellExecuteA');
  shellexecutefunc(0, 'open', PChar(cmd), nil, nil, SW_SHOWNORMAL);
  FreeLibrary(inst);
end;

var
  numcpus: Integer = 0;

procedure I_DetectCPU;
var
  info: TSystemInfo;
begin
  try
  // detect MMX and 3DNow! capable CPU (adapted from AMD's "3DNow! Porting Guide")
    asm
      pusha
      mov  eax, $80000000
      cpuid
      cmp  eax, $80000000
      jbe @@NoMMX3DNow
      mov mmxMachine, 1
      mov  eax, $80000001
      cpuid
      test edx, $80000000
      jz @@NoMMX3DNow
      mov AMD3DNowMachine, 1
  @@NoMMX3DNow:
      popa
    end;
  except
  // trap for old/exotics CPUs
    mmxMachine := 0;
    AMD3DNowMachine := 0;
  end;

  if mmxMachine <> 0 then
    printf(' MMX extentions detected'#13#10);
  if AMD3DNowMachine <> 0 then
    printf(' AMD 3D Now! extentions detected'#13#10);

  GetSystemInfo(info);
  numcpus := info.dwNumberOfProcessors;

  if numcpus > 1 then
    printf(' Multi-core system detected (%d CPUs)'#13#10, [numcpus]);

  usemultithread := numcpus > 1;

  if usemultithread then
    printf(' Multithreding mode ON'#13#10)
  else
  begin
    if numcpus > 1 then
      printf(' Multithreding mode OFF (will not use all cores)'#13#10)
    else
      printf(' Multithreding mode OFF'#13#10);
  end;
end;

function I_GetNumCPUs: integer;
begin
  Result := numcpus;
end;


initialization
  basetime := 0;

  if not QueryPerformanceFrequency(Freq) then
    Freq := 1000;

  hThread := GetCurrentThread;

end.

