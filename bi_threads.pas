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
//    Multithreading support
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_threads;

interface

type
  threadfunc_t = function(p: pointer): integer; stdcall;

type
  TDThread = class;

  threadinfo_t = record
    thread: TDThread;
  end;
  Pthreadinfo_t = ^threadinfo_t;

  TDThread = class
  private
    suspended: boolean;
  protected
    ffunc: threadfunc_t;
    fparms: Pointer;
    fid: Integer;
    info: threadinfo_t;
    fstatus: integer;
    fterminated: boolean;
    frunning: boolean;
  public
    constructor Create(const func: threadfunc_t = nil);
    destructor Destroy; override;
    procedure Activate(const parms: pointer); overload;
    procedure Activate(const func: threadfunc_t; const parms: pointer); overload;
    procedure Wait;
    function CheckJobDone: Boolean;
    function IsIdle: Boolean;
  end;

const
  THR_DEAD = 0;
  THR_ACTIVE = 1;
  THR_IDLE = 2;

implementation

uses
  bi_delphi,
  Windows,
  bi_system;

function ThreadWorker(p: Pointer): integer; stdcall;
var
  th: TDThread;
begin
  result := 0;
  th := Pthreadinfo_t(p).thread;
  while true do
  begin
    while (th.fstatus = THR_IDLE) and not th.fterminated do
    begin
      sleep(0);
    end;
    if th.fterminated then
      exit;
    th.frunning := true;
    th.ffunc(th.fparms);
    th.frunning := false;
    if th.fterminated then
      exit;
    th.fstatus := THR_IDLE;
  end;
end;

constructor TDThread.Create(const func: threadfunc_t = nil);
begin
  fterminated := False;
  ffunc := func;
  fparms := nil;
  fstatus := THR_IDLE;
  frunning := false;
  info.thread := Self;
  fid := I_CreateProcess(@ThreadWorker, @info, True);
  suspended := True;
end;

destructor TDThread.Destroy;
begin
  while frunning do
    Sleep(0);
  fterminated := True;
  fstatus := THR_DEAD;
  I_WaitForProcess(fid, 1);
  Inherited Destroy;
end;

// JVAL: Should check for fstatus, but it is not called while active
procedure TDThread.Activate(const parms: pointer);
begin
  if not Assigned(ffunc) then
    I_Error('TDThread.Activate(): Null function pointer');
  fparms := parms;
  fstatus := THR_ACTIVE;
  suspended := False;
  ResumeThread(fid);
end;

procedure TDThread.Activate(const func: threadfunc_t; const parms: pointer);
begin
  ffunc := func;
  Activate(parms);
end;

procedure TDThread.Wait;
begin
  if suspended then
    Exit;

  while fstatus = THR_ACTIVE do
  begin
//    sleep(0);
  end;
  suspended := True;
  SuspendThread(fid);
end;

function TDThread.CheckJobDone: Boolean;
begin
  if fstatus = THR_IDLE then
  begin
    if not suspended then
    begin
      suspended := True;
      SuspendThread(fid);
    end;
    Result := True;
  end
  else
    Result := False;
end;

function TDThread.IsIdle: Boolean;
begin
  Result := fstatus = THR_IDLE;
end;

end.

