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
//    Binary filesystem for set/minifig inventories
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_binaryset;

interface

uses
  SysUtils, Classes, bi_delphi, bi_hash;

const
  RECNAMESIZE = 15;
  BICACHESIZE = 8;

type
  TBinarySetItem = packed record
    piece: string[RECNAMESIZE];
    color: integer;
    num: integer;
    cost: double;
  end;

const
  BIMAXSETITEMS = 400;
  BIMIDSETITEMS = 100;
  BIMINSETITEMS = 20;

type
  TBinarySetData = array[0..BIMAXSETITEMS - 1] of TBinarySetItem;
  PBinarySetData = ^TBinarySetData;
  TBinarySetDataSM = array[0..BIMINSETITEMS - 1] of TBinarySetItem;
  PBinarySetDataSM = ^TBinarySetDataSM;
  TBinarySetDataMD = array[0..BIMIDSETITEMS - 1] of TBinarySetItem;
  PBinarySetDataMD = ^TBinarySetDataMD;

type
  TBinarySetRecord = record
    name: string[RECNAMESIZE];
    desc: string[128];
    year: integer;
    numitems: Integer;
    data: TBinarySetData;
  end;
  PBinarySetRecord = ^TBinarySetRecord;

type
  TBinarySetRecordSM = record
    name: string[RECNAMESIZE];
    desc: string[128];
    year: integer;
    numitems: Integer;
    data: TBinarySetDataSM;
  end;
  PBinarySetRecordSM = ^TBinarySetRecordSM;

type
  TBinarySetRecordMD = record
    name: string[RECNAMESIZE];
    desc: string[128];
    year: integer;
    numitems: Integer;
    data: TBinarySetDataMD;
  end;
  PBinarySetRecordMD = ^TBinarySetRecordMD;

type
  TBinarySetCollection = class(TObject)
  private
    fsets: THashStringList;
    fvoids: TDNumberList;
    fsetsSM: THashStringList;
    fvoidsSM: TDNumberList;
    fsetsMD: THashStringList;
    fvoidsMD: TDNumberList;
    fname: string;
    fnameSM: string;
    fnameMD: string;
    f: TFileStream;
    fSM: TFileStream;
    fMD: TFileStream;
    cacheidx: integer;
    cache: array[0..BICACHESIZE - 1] of TBinarySetRecord;
  protected
    function DeleteSet(const aset: string): boolean; virtual;
    function DeleteSetSM(const aset: string): boolean; virtual;
    function DeleteSetMD(const aset: string): boolean; virtual;
  public
    constructor Create(const aname1, aname2, aname3: string); virtual;
    destructor Destroy; override;
    function GetSet(const setname: string): PBinarySetRecord;
    function GetSetAsText(const setname: string): string;
    function UpdateSetFromTextFile(const aset, aTextFileName: string): boolean;
    function UpdateSetFromText(const aset, aText: string): boolean; overload;
    function UpdateSetFromText(const aset: string; str: TStringList): boolean; overload;
  end;


implementation

uses
  bi_globals, bi_utils, bi_db, bi_cachefile;

constructor TBinarySetCollection.Create(const aname1, aname2, aname3: string);
var
  i: integer;
  setname: string[RECNAMESIZE];
begin
  cacheidx := 0;
  ZeroMemory(@cache, SizeOf(cache));
  fname := aname1;
  fsets := THashStringList.Create;
  fvoids := TDNumberList.Create;
  fnameSM := aname2;
  fsetsSM := THashStringList.Create;
  fvoidsSM := TDNumberList.Create;
  fnameMD := aname3;
  fsetsMD := THashStringList.Create;
  fvoidsMD := TDNumberList.Create;

  if not fexists(fname) then
  begin
    f := TFileStream.Create(fname, fmCreate or fmShareDenyWrite);
    f.Free;
  end
  else
    CacheReadFile(fname);
  if not fexists(fnameSM) then
  begin
    fSM := TFileStream.Create(fnameSM, fmCreate or fmShareDenyWrite);
    fSM.Free;
  end
  else
    CacheReadFile(fnameSM);
  if not fexists(fnameMD) then
  begin
    fMD := TFileStream.Create(fnameMD, fmCreate or fmShareDenyWrite);
    fMD.Free;
  end
  else
    CacheReadFile(fnameMD);
  {$IFDEF CRAWLER}
  f := TFileStream.Create(fname, fmOpenRead or fmShareDenyNone);
  fSM := TFileStream.Create(fnameSM, fmOpenRead or fmShareDenyNone);
  fMD := TFileStream.Create(fnameMD, fmOpenRead or fmShareDenyNone);
  {$ELSE}
{  if fexists(fname) then
    backupfile(fname);}
  f := TFileStream.Create(fname, fmOpenReadWrite or fmShareDenyWrite);
  fSM := TFileStream.Create(fnameSM, fmOpenReadWrite or fmShareDenyWrite);
  fMD := TFileStream.Create(fnameMD, fmOpenReadWrite or fmShareDenyWrite);
  {$ENDIF}
  for i := 0 to (f.Size div SizeOf(TBinarySetRecord)) - 1 do
  begin
    f.Position := i * SizeOf(TBinarySetRecord);
    f.Read(setname, SizeOf(setname));
    if setname = '' then
      fvoids.Add(i * SizeOf(TBinarySetRecord))
    else
      fsets.AddObject(setname, TRecordInfo.Create(i * SizeOf(TBinarySetRecord)));
  end;
  for i := 0 to (fSM.Size div SizeOf(TBinarySetRecordSM)) - 1 do
  begin
    fSM.Position := i * SizeOf(TBinarySetRecordSM);
    fSM.Read(setname, SizeOf(setname));
    if setname = '' then
      fvoidsSM.Add(i * SizeOf(TBinarySetRecordSM))
    else
      fsetsSM.AddObject(setname, TRecordInfo.Create(i * SizeOf(TBinarySetRecordSM)));
  end;
  for i := 0 to (fMD.Size div SizeOf(TBinarySetRecordMD)) - 1 do
  begin
    fMD.Position := i * SizeOf(TBinarySetRecordMD);
    fMD.Read(setname, SizeOf(setname));
    if setname = '' then
      fvoidsMD.Add(i * SizeOf(TBinarySetRecordMD))
    else
      fsetsMD.AddObject(setname, TRecordInfo.Create(i * SizeOf(TBinarySetRecordMD)));
  end;

  inherited Create;
end;

destructor TBinarySetCollection.Destroy;
begin
  FreeHashList(fsets);
  f.Free;
  fvoids.Free;
  FreeHashList(fsetsSM);
  fSM.Free;
  fvoidsSM.Free;
  FreeHashList(fsetsMD);
  fMD.Free;
  fvoidsMD.Free;
  inherited;
end;

function TBinarySetCollection.GetSet(const setname: string): PBinarySetRecord;
var
  idx: integer;
  idxSM: integer;
  idxMD: integer;
  i: integer;
begin
  idx := fsets.IndexOf(setname);
  idxSM := fsetsSM.IndexOf(setname);
  idxMD := fsetsMD.IndexOf(setname);
  if (idx < 0) and (idxSM < 0) and (idxMD < 0) then
  begin
    Result := nil;
    Exit;
  end;

  if idx >= 0 then
  begin
    f.Position := (fsets.Objects[idx] as TRecordInfo).position;
    f.Read(cache[cacheidx], SizeOf(TBinarySetRecord));

    for i := 0 to cache[cacheidx].numitems - 1 do
      if cache[cacheidx].data[i].piece = '6141' then
        cache[cacheidx].data[i].piece := '4073'
      else if Pos1('Mx', cache[cacheidx].data[i].piece) then
        cache[cacheidx].data[i].piece[1] := 'm'
      else
        cache[cacheidx].data[i].piece := fixpartname(cache[cacheidx].data[i].piece);

    Result := @cache[cacheidx];
    Inc(cacheidx);
    if cacheidx >= BICACHESIZE then
      cacheidx := 0;
  end
  else if idxSM >= 0 then
  begin
    fSM.Position := (fsetsSM.Objects[idxSM] as TRecordInfo).position;
    fSM.Read(cache[cacheidx], SizeOf(TBinarySetRecordSM));

    for i := 0 to cache[cacheidx].numitems - 1 do
      if cache[cacheidx].data[i].piece = '6141' then
        cache[cacheidx].data[i].piece := '4073'
      else if Pos1('Mx', cache[cacheidx].data[i].piece) then
        cache[cacheidx].data[i].piece[1] := 'm'
      else
        cache[cacheidx].data[i].piece := fixpartname(cache[cacheidx].data[i].piece);

    Result := @cache[cacheidx];
    Inc(cacheidx);
    if cacheidx >= BICACHESIZE then
      cacheidx := 0;
  end
  else
  begin
    fMD.Position := (fsetsMD.Objects[idxMD] as TRecordInfo).position;
    fMD.Read(cache[cacheidx], SizeOf(TBinarySetRecordMD));

    for i := 0 to cache[cacheidx].numitems - 1 do
      if cache[cacheidx].data[i].piece = '6141' then
        cache[cacheidx].data[i].piece := '4073'
      else if Pos1('Mx', cache[cacheidx].data[i].piece) then
        cache[cacheidx].data[i].piece[1] := 'm'
      else
        cache[cacheidx].data[i].piece := fixpartname(cache[cacheidx].data[i].piece);

    Result := @cache[cacheidx];
    Inc(cacheidx);
    if cacheidx >= BICACHESIZE then
      cacheidx := 0;
  end;
end;

function TBinarySetCollection.GetSetAsText(const setname: string): string;
var
  rec: TBinarySetRecord;
  recSM: TBinarySetRecordSM;
  recMD: TBinarySetRecordMD;
  i, idx, idxSM, idxMD: integer;
begin
  Result := 'Part,Color,Num';
  idx := fsets.IndexOf(setname);
  idxSM := fsetsSM.IndexOf(setname);
  idxMD := fsetsMD.IndexOf(setname);
  if idx < 0 then
    if idxSM < 0 then
      if idxMD < 0 then
        Exit;

  if idx >= 0 then
  begin
    f.Position := (fsets.Objects[idx] as TRecordInfo).position;
    f.Read(rec, SizeOf(TBinarySetRecord));
    for i := 0 to rec.numitems - 1 do
    begin
      if rec.data[i].piece = '6141' then
        rec.data[i].piece := '4073'
      else if Pos1('Mx', rec.data[i].piece) then
        rec.data[i].piece[1] := 'm'
      else
        cache[cacheidx].data[i].piece := fixpartname(cache[cacheidx].data[i].piece);
      if rec.data[i].color = -2 then
        rec.data[i].color := -1;

      Result := Result + #13#10 + rec.data[i].piece + ',' + itoa(rec.data[i].color) + ',' + itoa(rec.data[i].num);
    end;
  end
  else if idxSM >= 0 then
  begin
    fSM.Position := (fsetsSM.Objects[idxSM] as TRecordInfo).position;
    fSM.Read(recSM, SizeOf(TBinarySetRecordSM));
    for i := 0 to recSM.numitems - 1 do
    begin
      if recSM.data[i].piece = '6141' then
        recSM.data[i].piece := '4073'
      else if Pos1('Mx', recSM.data[i].piece) then
        recSM.data[i].piece[1] := 'm'
      else
        cache[cacheidx].data[i].piece := fixpartname(cache[cacheidx].data[i].piece);
      if rec.data[i].color = -2 then
        rec.data[i].color := -1;

      Result := Result + #13#10 + recSM.data[i].piece + ',' + itoa(recSM.data[i].color) + ',' + itoa(recSM.data[i].num);
    end;
  end
  else
  begin
    fMD.Position := (fsetsMD.Objects[idxMD] as TRecordInfo).position;
    fMD.Read(recMD, SizeOf(TBinarySetRecordMD));
    for i := 0 to recMD.numitems - 1 do
    begin
      if recMD.data[i].piece = '6141' then
        recMD.data[i].piece := '4073'
      else if Pos1('Mx', recMD.data[i].piece) then
        recMD.data[i].piece[1] := 'm'
      else
        cache[cacheidx].data[i].piece := fixpartname(cache[cacheidx].data[i].piece);
      if rec.data[i].color = -2 then
        rec.data[i].color := -1;

      Result := Result + #13#10 + recMD.data[i].piece + ',' + itoa(recMD.data[i].color) + ',' + itoa(recMD.data[i].num);
    end;
  end;
end;

function TBinarySetCollection.UpdateSetFromTextFile(const aset, aTextFileName: string): boolean;
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
    Result := UpdateSetFromText(aset, s);
  finally
    s.Free;
  end;
end;

function TBinarySetCollection.UpdateSetFromText(const aset, aText: string): boolean;
var
  s: TStringList;
begin
  s := TStringList.Create;
  try
    s.Text := aText;
    Result := UpdateSetFromText(aset, s);
  finally
    s.Free;
  end;
end;

function TBinarySetCollection.UpdateSetFromText(const aset: string; str: TStringList): boolean;
var
  rec: TBinarySetRecord;
  i, j, idx: integer;
  spart, scolor, snum, scost, scode, sspare: string;
  cc: integer;
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
  if str.count > BIMAXSETITEMS then
  begin
    DeleteSet(aset);
    Result := False;
    Exit;
  end;
  if Length(aset) > RECNAMESIZE then
  begin
    Result := False;
    Exit;
  end;
  if fvoids.Count = 0 then
    if f.Size > 2147418112 - 2 * SizeOf(TBinarySetRecord) then
      if fsets.IndexOf(aset) < 0 then
      begin
        Result := False;
        Exit;
      end;
  if fvoidsSM.Count = 0 then
    if fSM.Size > 2147418112 - 2 * SizeOf(TBinarySetRecordSM) then
      if fsetsSM.IndexOf(aset) < 0 then
      begin
        Result := False;
        Exit;
      end;
  if fvoidsMD.Count = 0 then
    if fMD.Size > 2147418112 - 2 * SizeOf(TBinarySetRecordMD) then
      if fsetsMD.IndexOf(aset) < 0 then
      begin
        Result := False;
        Exit;
      end;

  ZeroMemory(@rec, SizeOf(TBinarySetRecord));
  rec.name := aset;
  rec.numitems := 0;
  if (str.Strings[0] = 'Part,Color,Num') or (str.Strings[0] = 'Part,Color,Quantity') then
  begin
    j := 0;
    for i := 1 to str.count - 1 do
    begin
      splitstring(str.Strings[i], spart, scolor, snum, ',');
      spart := fixpartname(spart);
      trimproc(scolor);
      if scolor = '-2' then
        scolor := '-1';
      if spart <> '' then
      begin
        if Pos1('BL ', spart) then
          rec.data[j].piece := db.RebrickablePart(Copy(spart, 4, Length(spart) - 3))
        else
          rec.data[j].piece := db.RebrickablePart(spart);

        if Pos1('BL', scolor) then
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
      if scolor = '-2' then
        scolor := '-1';
      if spart <> '' then
      begin
        if Pos1('BL ', spart) then
          rec.data[j].piece := db.RebrickablePart(Copy(spart, 4, Length(spart) - 3))
        else
          rec.data[j].piece := db.RebrickablePart(spart);

        if Pos1('BL', scolor) then
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
      if scolor = '-2' then
        scolor := '-1';
      if spart <> '' then
      begin
        if Pos1('BL ', spart) then
          rec.data[j].piece := db.RebrickablePart(Trim(Copy(spart, 4, Length(spart) - 3)))
        else
          rec.data[j].piece := db.RebrickablePart(Trim(spart));

        if Pos1('BL', scolor) then
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
        rec.data[j].num := atoi(snum);

        rec.data[j].cost := atof(scost);
        inc(j);
      end;
    end;
    rec.numitems := j;
  end;

  if rec.numitems >= BIMIDSETITEMS then
  begin
    idx := fsets.IndexOf(aset);
    if idx < 0 then
    begin
      if fvoids.Count > 0 then
      begin
        idx := fsets.AddObject(aset, TRecordInfo.Create(fvoids.Numbers[fvoids.Count - 1]));
        fvoids.Delete(fvoids.Count - 1)
      end
      else
        idx := fsets.AddObject(aset, TRecordInfo.Create(f.size));
    end;
    f.Position := (fsets.Objects[idx] as TRecordInfo).position;
    f.Write(rec, SizeOf(TBinarySetRecord));
    fsets.RebuiltHash;
    Result := True;
    DeleteSetSM(aset);
    DeleteSetMD(aset);
  end
  else if rec.numitems < BIMINSETITEMS then
  begin
    idx := fsetsSM.IndexOf(aset);
    if idx < 0 then
    begin
      if fvoidsSM.Count > 0 then
      begin
        idx := fsetsSM.AddObject(aset, TRecordInfo.Create(fvoidsSM.Numbers[fvoidsSM.Count - 1]));
        fvoidsSM.Delete(fvoidsSM.Count - 1)
      end
      else
        idx := fsetsSM.AddObject(aset, TRecordInfo.Create(fSM.size));
    end;
    fSM.Position := (fsetsSM.Objects[idx] as TRecordInfo).position;
    fSM.Write(rec, SizeOf(TBinarySetRecordSM));
    fsetsSM.RebuiltHash;
    Result := True;
    DeleteSet(aset);
    DeleteSetMD(aset);
  end
  else
  begin
    idx := fsetsMD.IndexOf(aset);
    if idx < 0 then
    begin
      if fvoidsMD.Count > 0 then
      begin
        idx := fsetsMD.AddObject(aset, TRecordInfo.Create(fvoidsMD.Numbers[fvoidsMD.Count - 1]));
        fvoidsMD.Delete(fvoidsMD.Count - 1)
      end
      else
        idx := fsetsMD.AddObject(aset, TRecordInfo.Create(fMD.size));
    end;
    fMD.Position := (fsetsMD.Objects[idx] as TRecordInfo).position;
    fMD.Write(rec, SizeOf(TBinarySetRecordMD));
    fsetsMD.RebuiltHash;
    Result := True;
    DeleteSet(aset);
    DeleteSetSM(aset);
  end;
end;

function TBinarySetCollection.DeleteSet(const aset: string): boolean;
var
  rec: TBinarySetRecord;
  idx: integer;
begin
  idx := fsets.IndexOf(aset);
  if idx < 0 then
  begin
    Result := False;
    Exit;
  end;

  ZeroMemory(@rec, RECNAMESIZE * SizeOf(Char));
  f.Position := (fsets.Objects[idx] as TRecordInfo).position;
  f.Write(rec, RECNAMESIZE * SizeOf(Char));
  fsets.Objects[idx].Free;
  fsets.Delete(idx);
  fsets.RebuiltHash;
  Result := True;
end;

function TBinarySetCollection.DeleteSetSM(const aset: string): boolean;
var
  rec: TBinarySetRecordSM;
  idx: integer;
begin
  idx := fsetsSM.IndexOf(aset);
  if idx < 0 then
  begin
    Result := False;
    Exit;
  end;

  ZeroMemory(@rec, RECNAMESIZE * SizeOf(Char));
  fSM.Position := (fsetsSM.Objects[idx] as TRecordInfo).position;
  fSM.Write(rec, RECNAMESIZE * SizeOf(Char));
  fsetsSM.Objects[idx].Free;
  fsetsSM.Delete(idx);
  fsetsSM.RebuiltHash;
  Result := True;
end;

function TBinarySetCollection.DeleteSetMD(const aset: string): boolean;
var
  rec: TBinarySetRecordSM;
  idx: integer;
begin
  idx := fsetsMD.IndexOf(aset);
  if idx < 0 then
  begin
    Result := False;
    Exit;
  end;

  ZeroMemory(@rec, RECNAMESIZE * SizeOf(Char));
  fMD.Position := (fsetsMD.Objects[idx] as TRecordInfo).position;
  fMD.Write(rec, RECNAMESIZE * SizeOf(Char));
  fsetsMD.Objects[idx].Free;
  fsetsMD.Delete(idx);
  fsetsMD.RebuiltHash;
  Result := True;
end;

end.
