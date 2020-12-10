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
//    Lugbulk class
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_lugbulk2017;

interface

uses
  SysUtils, Classes;

type
  TLugBulkDouble = class(TObject)
    value: Double;
  end;

type
  TLugBulk2017 = class(TObject)
  private
    fList: TStringList;
    function GetPart(index: integer): string;
    function GetColor(index: integer): integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromStream(const strm: TStream);
    procedure LoadFromFile(const fname: String);
    function ItemCost(const part: string; const color: integer): double;
    property Part[Index: Integer]: string read GetPart;
    property Color[Index: Integer]: integer read GetColor;
    property List: TStringList read fList;
  end;

implementation

uses
  bi_delphi,
  bi_db,
  bi_globals,
  bi_utils;

constructor TLugBulk2017.Create;
begin
  fList := TStringList.Create;
  fList.CaseSensitive := False;
  inherited;
end;

destructor TLugBulk2017.Destroy;
begin
  FreeList(fList);
  inherited;
end;

procedure TLugBulk2017.LoadFromStream(const strm: TStream);
var
  lst: TStringList;
  tdbl: TLugBulkDouble;
  spart, scolor, scost: string;
  i, j: integer;
begin
  lst := TStringList.Create;
  lst.LoadFromStream(strm);
  if lst.Count < 0 then
  begin
    lst.Free;
    Exit;
  end;

  fList.Clear;
  fList.Sorted := false;
  if lst.Strings[0] = 'Part,Color,Cost' then
  begin
    for i := 1 to lst.Count - 1 do
    begin
      splitstring(lst.Strings[i], spart, scolor, scost, ',');

      if Pos('BL ', spart) = 1 then
        spart := db.RebrickablePart(Copy(spart, 4, Length(spart) - 3))
      else
        spart := db.RebrickablePart(spart);

      if Pos('BL ', scolor) = 1 then
      begin
        scolor := Copy(scolor, 4, Length(scolor) - 3);
        scolor := IntToStr(db.BrickLinkColorToRebrickableColor(StrToIntDef(scolor, 0)));
      end
      else if Pos('BL', scolor) = 1 then
      begin
        scolor := Copy(scolor, 3, Length(scolor) - 2);
        scolor := IntToStr(db.BrickLinkColorToRebrickableColor(StrToIntDef(scolor, 0)));
      end;

      tdbl := TLugBulkDouble.Create;
      for j := 1 to Length(scost) do
        if scost[j] in [',', '.'] then
          scost[j] := DecimalSeparator;
      tdbl.value := StrToFloat(scost);
      fList.AddObject(spart + ',' + scolor, tdbl);
    end;
  end;
  fList.Sorted := true;
  lst.Free;
end;

procedure TLugBulk2017.LoadFromFile(const fname: String);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(fname, fmOpenRead);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

function TLugBulk2017.ItemCost(const part: string; const color: integer): double;
var
  srch: string;
  idx: integer;
begin
  srch := UpperCase(part) + ',' + IntToStr(color);
  idx := fList.IndexOf(srch);
  if idx < 0 then
  begin
    result := -1.0;
    Exit;
  end;
  result := (fList.Objects[idx] as TLugBulkDouble).value;
end;

function TLugBulk2017.GetPart(index: integer): string;
var
  spart, scolor: string;
begin
  if index < 0 then
  begin
    result := '';
    exit;
  end;
  if index >= fList.Count then
  begin
    result := '';
    exit;
  end;
  splitstring(fList.Strings[index], spart, scolor, ',');
  result := spart;
end;

function TLugBulk2017.GetColor(index: integer): integer;
var
  spart, scolor: string;
begin
  if index < 0 then
  begin
    result := -1;
    exit;
  end;
  if index >= fList.Count then
  begin
    result := -1;
    exit;
  end;
  splitstring(fList.Strings[index], spart, scolor, ',');
  result := StrToIntDef(scolor, -1);
end;

end.
