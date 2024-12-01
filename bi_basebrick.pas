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
//    Basebrick convertion for inventory content evaluation
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_basebrick;

interface

uses
  SysUtils, Classes, bi_delphi;

const
  BB_STRICT = 1;

type
  TBaseBrickInfo = class
  public
    part: string[16];
    num: integer;
    flags: LongWord;
    constructor Create(const apart: string; const anum: integer; const aflags: LongWord);
  end;

procedure BI_InitBaseBrick;

procedure BI_ShutDownBaseBrick;

function BI_GetBaseBrickInfo(const apart: string): TBaseBrickInfo;

implementation

uses
  bi_db, bi_utils;

constructor TBaseBrickInfo.Create(const apart: string; const anum: integer; const aflags: LongWord);
begin
  part := apart;
  num := anum;
  flags := aflags;
  Inherited Create;
end;

var
  basebricklst: TStringList;
  bbinit: boolean = False;

procedure BI_InitBaseBrick;
var
  i: integer;
  spart, sbasepart, snum, sstrict: string;
  lst: TStringList;
  bbi: TBaseBrickInfo;
begin
  if bbinit then
    Exit;

  basebricklst := TStringList.Create;

  lst := TStringList.Create;
  try
    if fexists(basedefault + 'db\db_basebrick.txt') then
      lst.LoadFromFile(basedefault + 'db\db_basebrick.txt');

    for i := 1 to lst.Count - 1 do
    begin
      splitstring(lst.Strings[i], spart, sbasepart, snum, sstrict, ',');
      bbi := TBaseBrickInfo.Create(sbasepart, atoi(snum), atoi(sstrict));
      basebricklst.AddObject(spart, bbi);
    end;
  finally
    lst.Free;
  end;

  basebricklst.Sorted := True;
  bbinit := True;
end;

function BI_GetBaseBrickInfo(const apart: string): TBaseBrickInfo;
var
  idx: integer;
begin
  Result := nil;
  if not bbinit then
    BI_InitBaseBrick;

  idx := basebricklst.IndexOf(apart);
  if idx >= 0 then
    Result := basebricklst.Objects[idx] as TBaseBrickInfo;
end;

procedure BI_ShutDownBaseBrick;
begin
  if bbinit then
  begin
    FreeList(basebricklst);
    bbinit := False;
  end;
end;

initialization

finalization
  BI_ShutDownBaseBrick;

end.
