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
    fyear: integer;
    procedure yearfromfilename(const fn: string);
    function GetPart(index: integer): string;
    function GetColor(index: integer): integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromStream(const strm: TStream);
    procedure LoadFromFile(const fname: String);
    procedure SaveToStream(const strm: TStream);
    procedure SaveToFile(const fname: String);
    function ItemCost(const part: string; const color: integer): double;
    procedure SetItemCost(const part: string; const color: integer; const cost: double);
    property year: integer read fyear;
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
  fyear := 0;
  inherited;
end;

destructor TLugBulk2017.Destroy;
begin
  FreeList(fList);
  inherited;
end;

procedure TLugBulk2017.yearfromfilename(const fn: string);
var
  i, p1, p2: integer;
  syear: string;
  ayear: integer;
begin
  p1 := -1;
  for i := Length(fn) downto 1 do
    if fn[i] = '.' then
    begin
      p1 := i;
      break;
    end;

  p2 := -1;
  for i := Length(fn) downto 1 do
    if fn[i] in ['/', '\'] then
    begin
      p2 := i;
      break;
    end;

  if p2 > 0 then
    if p1 > p2 then
    begin
      syear := '';
      for i := p2 + 1 to p1 - 1 do
        syear := syear + fn[i];
      ayear := atoi(syear);
      if ayear >= 2000 then
        if ayear <= MAX_ACCEPTABLE_YEAR then
          fyear := ayear;
    end;
end;

procedure TLugBulk2017.LoadFromStream(const strm: TStream);
var
  lst: TStringList;
  tdbl: TLugBulkDouble;
  spart, scolor, scost, scode: string;
  docodes: boolean;
  cc: integer;
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
  docodes := lst.Strings[0] = 'Code,Cost';
  if (lst.Strings[0] = 'Part,Color,Cost') or (lst.Strings[0] = 'Code,Cost') then
  begin
    for i := 1 to lst.Count - 1 do
    begin
      if docodes then
      begin
        splitstring(lst.Strings[i], scode, scost, ',');
        if not db.GetPieceColorFromCode(scode, spart, cc) then
          Continue;
        scolor := itoa(cc);
      end
      else
      begin
        splitstring(lst.Strings[i], spart, scolor, scost, ',');

        if Pos1('BL ', spart) then
          spart := db.RebrickablePart(Copy(spart, 4, Length(spart) - 3))
        else
          spart := db.RebrickablePart(spart);

        if Pos1('BL ', scolor) then
        begin
          scolor := Copy(scolor, 4, Length(scolor) - 3);
          scolor := IntToStr(db.BrickLinkColorToSystemColor(StrToIntDef(scolor, 0)));
        end
        else if Pos1('BL', scolor) then
        begin
          scolor := Copy(scolor, 3, Length(scolor) - 2);
          scolor := IntToStr(db.BrickLinkColorToSystemColor(StrToIntDef(scolor, 0)));
        end
        else if Pos1('RB ', scolor) then
        begin
          scolor := Copy(scolor, 4, Length(scolor) - 3);
          scolor := IntToStr(db.RebrickableColorToSystemColor(StrToIntDef(scolor, 0)));
        end
        else if Pos1('RB', scolor) then
        begin
          scolor := Copy(scolor, 3, Length(scolor) - 2);
          scolor := IntToStr(db.RebrickableColorToSystemColor(StrToIntDef(scolor, 0)));
        end;
      end;

      tdbl := TLugBulkDouble.Create;
      for j := 1 to Length(scost) do
        if scost[j] in [',', '.'] then
          scost[j] := DecimalSeparator;
      tdbl.value := atof(scost);
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
    yearfromfilename(fname);
  finally
    fs.Free;
  end;
end;

procedure TLugBulk2017.SaveToStream(const strm: TStream);
var
  sL: TStringList;
  i: integer;
begin
  sL := TStringList.Create;
  try
    sL.Add('Part,Color,Cost');
    for i := 0 to fList.Count - 1 do
      sL.Add(Format('%s,%2.4f', [fList.Strings[i], (fList.Objects[i] as TLugBulkDouble).value]));
    sL.SaveToStream(strm);
  finally
    sL.Free;
  end;
end;

procedure TLugBulk2017.SaveToFile(const fname: String);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(fname, fmCreate);
  try
    SaveToStream(fs);
    yearfromfilename(fname);
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
    Result := -1.0;
    Exit;
  end;
  Result := (fList.Objects[idx] as TLugBulkDouble).value;
end;

procedure TLugBulk2017.SetItemCost(const part: string; const color: integer; const cost: double);
var
  srch: string;
  idx: integer;
begin
  srch := UpperCase(part) + ',' + IntToStr(color);
  idx := fList.IndexOf(srch);
  if idx < 0 then
    idx := fList.AddObject(part + ',' + IntToStr(color), TLugBulkDouble.Create);

  if idx >= 0 then
    (fList.Objects[idx] as TLugBulkDouble).value := cost;
end;

function TLugBulk2017.GetPart(index: integer): string;
var
  spart, scolor: string;
begin
  if index < 0 then
  begin
    Result := '';
    Exit;
  end;
  if index >= fList.Count then
  begin
    Result := '';
    Exit;
  end;
  splitstring(fList.Strings[index], spart, scolor, ',');
  Result := spart;
end;

function TLugBulk2017.GetColor(index: integer): integer;
var
  spart, scolor: string;
begin
  if index < 0 then
  begin
    Result := -1;
    Exit;
  end;
  if index >= fList.Count then
  begin
    Result := -1;
    Exit;
  end;
  splitstring(fList.Strings[index], spart, scolor, ',');
  Result := StrToIntDef(scolor, -1);
end;

end.
