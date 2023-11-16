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
//    Hash list class
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_hash;

interface

uses
  SysUtils, Classes, bi_delphi;

const
{$IFDEF CRAWLER}
  HASHSIZE = 512;
{$ELSE}
  HASHSIZE = 2048;
{$ENDIF}

type
  THashTable = class(TObject)
  private
    positions: array[0..HASHSIZE - 1] of TDNumberList;
    fList: TStringList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AssignStringList(const s: TStringList);
    procedure AssignStringListU(const s: TStringList);
    procedure Insert(const str: string; const p: integer);
    procedure Clear;
    function GetPos(const value: string): integer;
    function CheckPos(const value: string): integer;
    function CheckPosU(const value: string): integer;
    property List: TStringList read fList;
  end;

type
  THashStringList = class(TStringList)
  protected
    fhash: THashTable;
    fhashU: THashTable;
    procedure InsertItem(Index: Integer; const S: string; AObject: TObject); override;
  public
    constructor Create;
    destructor Destroy; override;
    function IndexOf(const S: string): Integer; override;
    function IndexOfUCS(const S: string): Integer;
    procedure RebuiltHash;
  end;

procedure FreeHashList(var s: THashStringList);

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

  result := result and (HASHSIZE - 1);
end;

constructor THashTable.Create;
begin
  inherited;
  flist := nil;
  FillChar(positions, SizeOf(positions), 0);
end;

destructor THashTable.Destroy;
var
  i: integer;
begin
  for i := 0 to HASHSIZE - 1 do
    if positions[i] <> nil then
      positions[i].Free;
  inherited;
end;

procedure THashTable.Insert(const str: string; const p: integer);
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

procedure THashTable.AssignStringList(const s: TStringList);
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

procedure THashTable.AssignStringListU(const s: TStringList);
var
  i: integer;
  h: integer;
begin
  Clear;
  flist := s;
  for i := 0 to flist.Count - 1 do
  begin
    h := Hash(UpperCase(flist.Strings[i]));
    if positions[h] = nil then
      positions[h] := TDNumberList.Create;
    positions[h].Add(i);
  end;
end;

procedure THashTable.Clear;
var
  i: integer;
begin
  for i := 0 to HASHSIZE - 1 do
    if positions[i] <> nil then
      positions[i].Clear;
end;

function THashTable.GetPos(const value: string): integer;
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

function THashTable.CheckPos(const value: string): integer;
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

function THashTable.CheckPosU(const value: string): integer;
var
  h: integer;
  i: integer;
  n: integer;
  check: string;
begin
  check := UpperCase(value);
  h := Hash(check);
  if positions[h] = nil then
  begin
    result := -1;
    Exit;
  end;

  for i := 0 to positions[h].Count - 1 do
  begin
    n := positions[h].Numbers[i];
    if (n > -1) and (n < fList.Count) then
      if UpperCase(flist.Strings[n]) = check then
      begin
        result := n;
        Exit;
      end;
  end;

  result := -1;
end;

constructor THashStringList.Create;
begin
  fhash := THashTable.Create;
  fhashU := THashTable.Create;
  Inherited Create;
  fhash.AssignStringList(self);
  fhashU.AssignStringListU(self);
end;

destructor THashStringList.Destroy;
begin
  Inherited;
  fhash.Free;
  fhashU.Free;
end;

procedure THashStringList.InsertItem(Index: Integer; const S: string; AObject: TObject);
var
  rebuildhash: boolean;
begin
  rebuildhash := Index < Count;
  inherited InsertItem(Index, S, AObject);
  if rebuildhash then
  begin
    if not Sorted then
    begin
      fhash.AssignStringList(self);
      fhashU.AssignStringListU(self);
    end;
  end
  else
  begin
    fhash.Insert(s, Index);
    fhashU.Insert(UpperCase(s), Index);
  end;
end;

function THashStringList.IndexOfUCS(const S: string): Integer;
begin
  if Sorted then
  begin
    if not Find(S, Result) then
      Result := -1
    else
      Exit;
  end;

  if Count = 0 then
  begin
    Result := -1;
    Exit;
  end;

  result := fhash.CheckPos(S);

  if result = -1 then
    result := fhashU.CheckPosU(S);
end;

function THashStringList.IndexOf(const S: string): Integer;
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

procedure FreeHashList(var s: THashStringList);
var
  i: integer;
begin
  if s = nil then
    Exit;
  for i := 0 to s.Count - 1 do
    try
      if s.Objects[i] <> nil then
      begin
        s.Objects[i].Free;
        s.Objects[i] := nil;
      end;
    except
      s.Objects[i] := nil;
    end;
  FreeAndNil(s);
end;

procedure THashStringList.RebuiltHash;
begin
  fhash.AssignStringList(self);
end;

end.
