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
//    Hash list class (small capacity)
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_hash512;

interface

uses
  SysUtils, Classes, bi_delphi;

const
  HASHSIZE512 = 512;

type
  THashTable512 = class(TObject)
  private
    positions: array[0..HASHSIZE512 - 1] of TDNumberList;
    fList: TStringList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AssignStringList(const s: TStringList);
    procedure Insert(const str: string; const p: integer);
    procedure Clear;
    function GetPos(const value: string): integer;
    function CheckPos(const value: string): integer;
    property List: TStringList read fList;
  end;

type
  THashStringList512 = class(TStringList)
  protected
    fhash: THashTable512;
    procedure InsertItem(Index: Integer; const S: string; AObject: TObject); override;
  public
    constructor Create;
    destructor Destroy; override;
    function IndexOf(const S: string): Integer; override;
    procedure RebuiltHash;
  end;

procedure FreeHashList512(var s: THashStringList512);

implementation

function Hash(const name: string): integer;
var
  b: Byte;
  i: integer;
  len: integer;
begin
  len := Length(name);
  if len = 0 then
  begin
    Result := 0;
    Exit;
  end;

  b := Ord(name[1]);

  result := 5381 * 33 + b;

  for i := 2 to len do
  begin
    b := Ord(name[i]);
    result := result * 33 + b;
  end;

  result := result and (HASHSIZE512 - 1);
end;

constructor THashTable512.Create;
begin
  inherited;
  flist := nil;
  FillChar(positions, SizeOf(positions), 0);
end;

destructor THashTable512.Destroy;
var
  i: integer;
begin
  for i := 0 to HASHSIZE512 - 1 do
    if positions[i] <> nil then
      positions[i].Free;
  inherited;
end;

procedure THashTable512.Insert(const str: string; const p: integer);
var
  h: integer;
begin
  if flist = nil then
    Exit;

  h := Hash(str);
  if positions[h] = nil then
    positions[h] := TDNumberList.Create;
  positions[h].Add(p);
end;

procedure THashTable512.AssignStringList(const s: TStringList);
var
  i: integer;
  h: integer;
begin
  Clear;
  flist := s;
  for i := 0 to flist.Count - 1 do
  begin
    h := Hash(flist.Strings[i]);
    if positions[h] = nil then
      positions[h] := TDNumberList.Create;
    positions[h].Add(i);
  end;
end;

procedure THashTable512.Clear;
var
  i: integer;
begin
  for i := 0 to HASHSIZE512 - 1 do
    if positions[i] <> nil then
      positions[i].Clear;
end;

function THashTable512.GetPos(const value: string): integer;
var
  h: integer;
  i: integer;
  n: integer;
begin
  if flist = nil then
  begin
    result := -1;
    Exit;
  end;

  if flist.Count = 0 then
  begin
    result := -1;
    Exit;
  end;

  h := Hash(value);
  if positions[h] = nil then
  begin
    result := fList.IndexOf(value);
    Exit;
  end;

  for i := 0 to positions[h].Count - 1 do
  begin
    n := positions[h].Numbers[i];
    if (n > -1) and (n < fList.Count) then
      if flist.Strings[n] = value then
      begin
        result := n;
        Exit;
      end;
  end;

  result := fList.IndexOf(value);
end;

function THashTable512.CheckPos(const value: string): integer;
var
  h: integer;
  i: integer;
  n: integer;
begin
  h := Hash(value);
  if positions[h] = nil then
  begin
    result := -1;
    Exit;
  end;

  for i := 0 to positions[h].Count - 1 do
  begin
    n := positions[h].Numbers[i];
    if (n > -1) and (n < fList.Count) then
      if flist.Strings[n] = value then
      begin
        result := n;
        Exit;
      end;
  end;

  result := -1;
end;

constructor THashStringList512.Create;
begin
  fhash := THashTable512.Create;
  Inherited Create;
  fhash.AssignStringList(self);
end;

destructor THashStringList512.Destroy;
begin
  Inherited;
  fhash.Free;
end;

procedure THashStringList512.InsertItem(Index: Integer; const S: string; AObject: TObject);
var
  rebuildhash: boolean;
begin
  rebuildhash := Index < Count;
  inherited InsertItem(Index, S, AObject);
  if rebuildhash then
  begin
    if not Sorted then
      fhash.AssignStringList(self);
  end
  else
    fhash.Insert(s, Index);
end;

function THashStringList512.IndexOf(const S: string): Integer;
begin
  if Sorted then
  begin
    if not Find(S, Result) then Result := -1;
    Exit;
  end;

  if Count = 0 then
  begin
    Result := -1;
    Exit;
  end;

  result := fhash.CheckPos(S);
end;

procedure FreeHashList512(var s: THashStringList512);
var
  i: integer;
begin
  if s = nil then
    Exit;
  for i := 0 to s.Count - 1 do
    s.Objects[i].Free;
  s.Free;
  s := nil;
end;

procedure THashStringList512.RebuiltHash;
begin
  fhash.AssignStringList(self);
end;

end.
