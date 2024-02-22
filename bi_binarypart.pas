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
//    Binary filesystem for part inventories
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_binarypart;

interface

uses
  SysUtils, Classes, bi_delphi, bi_hash;

const
  RECPARTNAMESIZE = 15;

type
  TBinaryPartItem = packed record
    piece: string[RECPARTNAMESIZE];
    color: integer;
    num: integer;
    cost: double;
  end;

const
  MAXPARTITEMS = 5;

type
  TBinaryPartData = array[0..MAXPARTITEMS - 1] of TBinaryPartItem;
  PBinaryPartData = ^TBinaryPartData;

type
  TBinaryPartRecord = packed record
    name: string[RECPARTNAMESIZE];
    hasstubcolor: boolean;
    numitems: Integer;
    data: TBinaryPartData;
  end;
  PBinaryPartRecord = ^TBinaryPartRecord;

type
  TBinaryPartCollection = class(TObject)
  private
    fparts: THashStringList;
    fvoids: TDNumberList;
    fname: string;
    f: TFileStream;
    cacheidx: integer;
  public
    constructor Create(const aname: string); virtual;
    destructor Destroy; override;
    function IndexOf(const partname: string): integer;
    function GetPartAsText(const partname: string; const color: integer): string;
    function UpdatePartFromTextFile(const apart, aTextFileName: string): boolean;
    function UpdatePartFromText(const apart, aText: string): boolean; overload;
    function UpdatePartFromText(const apart: string; str: TStringList): boolean; overload;
    function DeletePart(const apart: string): boolean;
  end;


implementation

uses
  bi_globals, bi_utils, bi_db, bi_cachefile;

constructor TBinaryPartCollection.Create(const aname: string);
var
  i: integer;
  partname: string[RECPARTNAMESIZE];
begin
  cacheidx := 0;
  fname := aname;
  fparts := THashStringList.Create;
  fvoids := TDNumberList.Create;

  if not fexists(fname) then
  begin
    f := TFileStream.Create(fname, fmCreate or fmShareDenyWrite);
    f.Free;
  end;
  {$IFDEF CRAWLER}
  f := TFileStream.Create(fname, fmOpenRead or fmShareDenyNone);
  {$ELSE}
{  if fexists(fname) then
    backupfile(fname);}
  f := TFileStream.Create(fname, fmOpenReadWrite or fmShareDenyWrite);
  {$ENDIF}
  for i := 0 to (f.Size div SizeOf(TBinaryPartRecord)) - 1 do
  begin
    f.Position := i * SizeOf(TBinaryPartRecord);
    f.read(partname, SizeOf(partname));
    if partname = '' then
      fvoids.Add(i * SizeOf(TBinaryPartRecord))
    else
      fparts.AddObject(partname, TRecordInfo.Create(i * SizeOf(TBinaryPartRecord)));
  end;

  inherited Create;
end;

destructor TBinaryPartCollection.Destroy;
begin
  FreeHashList(fparts);
  f.Free;
  fvoids.Free;
  inherited;
end;

function TBinaryPartCollection.IndexOf(const partname: string): integer;
begin
  Result := fparts.IndexOf(partname);
  if Result < 0 then
  begin
    Result := fparts.IndexOf(db.GetBLNetPieceName(partname));
    if Result < 0 then
    begin
      Result := fparts.IndexOf(db.GetNewPieceName(partname));
      if Result < 0 then
        Result := fparts.IndexOf(db.RebrickablePart(partname));
    end;
  end;
end;

function TBinaryPartCollection.GetPartAsText(const partname: string; const color: integer): string;
var
  rec: TBinaryPartRecord;
  i, idx: integer;
begin
  Result := 'Part,Color,Num';
  idx := IndexOf(partname);
  if idx < 0 then
    Exit;

  f.Position := (fparts.Objects[idx] as TRecordInfo).position;
  f.Read(rec, SizeOf(TBinaryPartRecord));
  for i := 0 to rec.numitems - 1 do
  begin
    if rec.data[i].piece = '6141' then
      rec.data[i].piece := '4073'
    else if Pos1('Mx', rec.data[i].piece) then
      rec.data[i].piece[1] := 'm'
    else
      rec.data[i].piece := fixpartname(rec.data[i].piece);

    if rec.data[i].color = -2 then
      Result := Result + #13#10 + rec.data[i].piece + ',' + itoa(color) + ',' + itoa(rec.data[i].num)
    else
      Result := Result + #13#10 + rec.data[i].piece + ',' + itoa(rec.data[i].color) + ',' + itoa(rec.data[i].num);
  end;
end;

function TBinaryPartCollection.UpdatePartFromTextFile(const apart, aTextFileName: string): boolean;
var
  s: TStringList;
begin
  if not fexists(aTextFileName) then
  begin
    Result := False;
    Exit;
  end;
  s := TStringList.Create;
  try
    S_LoadFromFile(s, aTextFileName);
    Result := UpdatePartFromText(apart, s);
  finally
    s.Free;
  end;
end;

function TBinaryPartCollection.UpdatePartFromText(const apart, aText: string): boolean;
var
  s: TStringList;
begin
  s := TStringList.Create;
  s.Text := aText;
  Result := UpdatePartFromText(apart, s);
  s.Free;
end;

function TBinaryPartCollection.UpdatePartFromText(const apart: string; str: TStringList): boolean;
var
  rec: TBinaryPartRecord;
  i, j, idx: integer;
  spart, scolor, snum, scost, scode, sspare: string;
  cc: integer;
  tmppart: string;
begin
  {$IFDEF CRAWLER}
  Result := False;
  Exit;
  {$ENDIF}
  if str = nil then
  begin
    Result := False;
    Exit;
  end;
  if str.count = 0 then
  begin
    Result := False;
    Exit;
  end;
  if str.count > MAXPARTITEMS then
  begin
    Result := False;
    Exit;
  end;
  if Length(apart) > RECPARTNAMESIZE then
  begin
    Result := False;
    Exit;
  end;
  if fvoids.Count = 0 then
    if f.Size > 2147418112 - 2 * SizeOf(TBinaryPartRecord) then
    begin
      Result := False;
      Exit;
    end;

  Result := True;

  ZeroMemory(@rec, SizeOf(TBinaryPartRecord));
  rec.name := apart;
  rec.numitems := 0;
  rec.hasstubcolor := False;
  if (str.Strings[0] = 'Part,Color,Num') or (str.Strings[0] = 'Part,Color,Quantity') then
  begin
    j := 0;
    for i := 1 to str.count - 1 do
    begin
      splitstring(str.Strings[i], spart, scolor, snum, ',');
      spart := fixpartname(spart);
      trimproc(scolor);
      if spart <> '' then
      begin
        if Pos1('BL ', spart) then
          tmppart := db.RebrickablePart(Trim(Copy(spart, 4, Length(spart) - 3)))
        else
          tmppart := db.RebrickablePart(spart);
        rec.data[j].piece := tmppart;
        if length(tmppart) > RECPARTNAMESIZE then
          Result := False;

        if (scolor = 'BL 0') or (scolor = '-2') or (scolor = '') then
          rec.data[j].color := -2
        else if Pos1('BL', scolor) then
        begin
          scolor := Trim(Copy(scolor, 3, Length(scolor) - 2));

          rec.data[j].color := db.BrickLinkColorToSystemColor(StrToIntDef(scolor, 0));
        end
        else if Pos1('RB', scolor) then
        begin
          scolor := Trim(Copy(scolor, 3, Length(scolor) - 2));

          rec.data[j].color := db.RebrickableColorToSystemColor(StrToIntDef(scolor, 0));
        end
        else
        begin
          rec.data[j].color := StrToIntDef(scolor, 0);
        end;
        if rec.data[j].color = -2 then
          rec.hasstubcolor := True;

        rec.data[j].num := atoi(snum);

        rec.data[j].cost := 0.0;
        inc(j);
      end;
    end;
    rec.numitems := j;
  end
  else if str.Strings[0] = 'Code,Num' then
  begin
    j := 0;
    for i := 1 to str.count - 1 do
    begin
      splitstring(str.Strings[i], scode, snum, ',');
      if db.GetPieceColorFromCode(scode, spart, cc) then
      begin
        spart := fixpartname(spart);
        if spart <> '' then
        begin
          if Pos1('BL ', spart) then
            rec.data[j].piece := db.RebrickablePart(Copy(spart, 4, Length(spart) - 3))
          else
            rec.data[j].piece := db.RebrickablePart(spart);

          rec.data[j].color := cc;
          rec.data[j].num := atoi(snum);

          rec.data[j].cost := 0.0;
          inc(j);
        end;
      end;
    end;
    rec.numitems := j;
  end
  else if str.Strings[0] = 'Part,Color,Quantity,Is Spare' then
  begin
    j := 0;
    for i := 1 to str.count - 1 do
    begin
      splitstring(str.Strings[i], spart, scolor, snum, sspare, ',');
      spart := fixpartname(spart);
      trimproc(scolor);
      if spart <> '' then
      begin
        if Pos1('BL ', spart) then
          tmppart := db.RebrickablePart(Trim(Copy(spart, 4, Length(spart) - 3)))
        else
          tmppart := db.RebrickablePart(spart);
        rec.data[j].piece := tmppart;
        if length(tmppart) > RECPARTNAMESIZE then
          Result := False;

        if (scolor = 'BL 0') or (scolor = '-2') or (scolor = '') then
          rec.data[j].color := -2
        else if Pos1('BL', scolor) then
        begin
          scolor := Trim(Copy(scolor, 3, Length(scolor) - 2));

          rec.data[j].color := db.BrickLinkColorToSystemColor(StrToIntDef(scolor, 0));
        end
        else if Pos1('RB', scolor) then
        begin
          scolor := Trim(Copy(scolor, 3, Length(scolor) - 2));

          rec.data[j].color := db.RebrickableColorToSystemColor(StrToIntDef(scolor, 0));
        end
        else
        begin
          rec.data[j].color := StrToIntDef(scolor, 0);
        end;
        if rec.data[j].color = -2 then
          rec.hasstubcolor := True;

        rec.data[j].num := atoi(snum);

        rec.data[j].cost := 0.0;
        inc(j);
      end;
    end;
    rec.numitems := j;
  end
  else if (str.Strings[0] = 'Part,Color,Num,Cost') then
  begin
    j := 0;
    for i := 1 to str.count - 1 do
    begin
      splitstring(str.Strings[i], spart, scolor, snum, scost, ',');
      spart := fixpartname(spart);
      trimproc(scolor);
      if spart <> '' then
      begin
        if Pos1('BL ', spart) then
          tmppart := db.RebrickablePart(Copy(spart, 4, Length(spart) - 3))
        else
          tmppart := db.RebrickablePart(spart);
        rec.data[j].piece := tmppart;
        if length(tmppart) > RECPARTNAMESIZE then
          Result := False;

        if (scolor = 'BL 0') or (scolor = '-2') or (scolor = '') then
          rec.data[j].color := -2
        else if Pos1('BL', scolor) then
        begin
          scolor := Trim(Copy(scolor, 3, Length(scolor) - 2));

          rec.data[j].color := db.BrickLinkColorToSystemColor(StrToIntDef(scolor, 0));
        end
        else if Pos1('RB', scolor) then
        begin
          scolor := Trim(Copy(scolor, 3, Length(scolor) - 2));

          rec.data[j].color := db.RebrickableColorToSystemColor(StrToIntDef(scolor, 0));
        end
        else
        begin
          rec.data[j].color := StrToIntDef(scolor, 0);
        end;
        if rec.data[j].color = -2 then
          rec.hasstubcolor := True;

        rec.data[j].num := atoi(snum);

        rec.data[j].cost := atof(scost);
        inc(j);
      end;
    end;
    rec.numitems := j;
  end;

  if Result then
  begin
    idx := fparts.IndexOf(apart);
    if idx < 0 then
    begin
      if fvoids.Count > 0 then
      begin
        idx := fparts.AddObject(apart, TRecordInfo.Create(fvoids.Numbers[fvoids.Count - 1]));
        fvoids.Delete(fvoids.Count - 1)
      end
      else
        idx := fparts.AddObject(apart, TRecordInfo.Create(f.size));
    end;
    f.Position := (fparts.Objects[idx] as TRecordInfo).position;
    f.Write(rec, SizeOf(TBinaryPartRecord));
  end
  else
    DeletePart(apart);
end;

function TBinaryPartCollection.DeletePart(const apart: string): boolean;
var
  rec: TBinaryPartRecord;
  idx: integer;
begin
  idx := fparts.IndexOf(apart);
  if idx < 0 then
  begin
    Result := False;
    Exit;
  end;

  ZeroMemory(@rec, RECPARTNAMESIZE * SizeOf(Char));
  f.Position := (fparts.Objects[idx] as TRecordInfo).position;
  f.Write(rec, RECPARTNAMESIZE * SizeOf(Char));
  fparts.Objects[idx].Free;
  fparts.Delete(idx);
//  fparts.RebuiltHash;
  Result := True;
end;

end.
