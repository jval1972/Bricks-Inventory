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
//    Ready list
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_readylist;

interface

uses
  bi_db;

function BI_GetReadyListInv(const rlname: string): TBrickInventory;

procedure BI_AddToReadyList(const part: string; const color: Integer; const num: integer; const rlname: string);

procedure BI_RemoveFromReadyList(const part: string; const color: Integer; const num: integer; const rlname: string);

procedure BI_SetQtyToReadyList(const part: string; const color: Integer; const qty: integer; const rlname: string);

function BI_QueryQtyFromReadyList(const part: string; const color: Integer; const rlname: string): integer;

procedure BI_ClearReadyList(const rlname: string);

function BI_IsReadyList(const rlname: string): boolean;

const
  S_READYLIST_01 = 'ReadyList01';

implementation

uses
  Classes, bi_delphi, bi_globals;

function BI_GetReadyListInv(const rlname: string): TBrickInventory;
begin
  Result := db.GetSetInventory(rlname);
end;

procedure BI_AddToReadyList(const part: string; const color: Integer; const num: integer; const rlname: string);
var
  inv: TBrickInventory;
  sl: TStringList;
begin
  if num = 0 then
    exit;

  sl := TStringList.Create;

  inv := BI_GetReadyListInv(rlname);
  if inv <> nil then
  begin
    inv.AddLoosePart(part, color, num);
    inv.DoReorganize;
    sl.Text := inv.AsText;
  end
  else
  begin
    sl.Add('Part,Color,Quantity');
    sl.Add(part + ',' + itoa(color) + ',' + itoa(num));
  end;

  db.UpdateSet(rlname, sl.Text);

  sl.Free;
end;

procedure BI_RemoveFromReadyList(const part: string; const color: Integer; const num: integer; const rlname: string);
var
  inv: TBrickInventory;
  sl: TStringList;
begin
  if num = 0 then
    exit;

  sl := TStringList.Create;

  inv := BI_GetReadyListInv(rlname);
  if inv <> nil then
  begin
    inv.RemoveLoosePartOrZeroOnOverflow(part, color, num);
    inv.DoReorganize;
    sl.Text := inv.AsText;
  end
  else
    sl.Add('Part,Color,Quantity');

  if sl.Count = 1 then
    sl.Add(part + ',' + itoa(color) + ',0');

  db.UpdateSet(rlname, sl.Text);

  sl.Free;
end;

procedure BI_SetQtyToReadyList(const part: string; const color: Integer; const qty: integer; const rlname: string);
var
  oldqty: Integer;
begin
  oldqty := BI_QueryQtyFromReadyList(part, color, rlname);

  if oldqty > qty then  // Remove
    BI_RemoveFromReadyList(part, color, oldqty - qty, rlname)
  else
    BI_AddToReadyList(part, color, qty - oldqty, rlname);
end;

function BI_QueryQtyFromReadyList(const part: string; const color: Integer; const rlname: string): integer;
var
  inv: TBrickInventory;
  oldqty: Integer;
begin
  inv := BI_GetReadyListInv(rlname);
  if inv = nil then
    Result := 0
  else
  begin
    inv.DoReorganize;
    Result := inv.LoosePartCount(part, color);
  end;
end;

procedure BI_ClearReadyList(const rlname: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Add('Part,Color,Quantity');
  sl.Add('3001,0,0');
  db.UpdateSet(rlname, sl.Text);
  sl.Free;
end;

function BI_IsReadyList(const rlname: string): boolean;
begin
  Result := rlname = S_READYLIST_01;
end;

end.
