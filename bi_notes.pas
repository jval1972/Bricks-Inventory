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
//    Piece notes stuff
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_notes;

interface

function BI_AutoNotes(const pcs: string; const path: string): string;

implementation

uses
  bi_delphi, SysUtils, Classes, StrUtils, bi_globals, bi_utils;

const
  DIMSEC_PROPS: array[0..3] of string[12] = (
    'Size: ',
    'Stud Dim.: ',
    'Pack. Dim.: ',
    'Item Dim.: '
  );
  FITS_PROPS: array[0..1] of string[24] = (
    'fits with the following ',
    'fits with and is usually'
  );

function _parse_list(const s: string): string;
const
  TYPE_SPIECE_CHARS = 'PSMCBG';
var
  tmp: string;
  i: integer;
begin
  tmp := StringReplace(s, '<ul>', '<ul>'#13#10, [rfReplaceAll, rfIgnoreCase]);
  tmp := StringReplace(s, '<UL>', '<ul>', [rfReplaceAll, rfIgnoreCase]);
  tmp := StringReplace(tmp, '</li>', '</li>'#13#10, [rfReplaceAll, rfIgnoreCase]);
  for i := 1 to Length(TYPE_SPIECE_CHARS) do
    tmp := StringReplace(tmp, '<a href="catalogitem.page?' + TYPE_SPIECE_CHARS[i] + '=', '<a href="spiece/', [rfReplaceAll, rfIgnoreCase]);
  tmp := StringReplace(tmp, '<a href="catalogitem.page?I=', '<a href="spieceI/', [rfReplaceAll, rfIgnoreCase]);
  tmp := StringReplace(tmp, '<a href="catalogitem.page?O=', '<a href="spieceO/', [rfReplaceAll, rfIgnoreCase]);
  Result := tmp;
end;

function BI_AutoNotes(const pcs: string; const path: string): string;
var
  sdata, sUdata: string;
  bpcs: string;
  i, j, k, p, p0, p1, p2: integer;
  check: string;
  tmp: string;
  sid: string;
  tmpret: string;
  sl: TStringList;
begin
  Result := '';

  bpcs := pcs;
  sdata := LoadStringFromFile(path + pcs + '.htm');
  if sdata = '' then
  begin
    bpcs := db.GetBLNetPieceName(pcs);
    if bpcs <> pcs then
      sdata := LoadStringFromFile(path + bpcs + '.htm');
    if sdata = '' then
      Exit;
  end;

  sUdata := UpperCase(sdata);

  check := 'Alternate Item No: <span';
  p := Pos(check, sData);
  if p > 0 then
  begin
    tmp := ExtractAfterDelimiters(sdata, p + Length(check) - 1, '>', '<');
    if tmp <> '' then
    begin
      tmp := stringreplace(tmp, ',', #13#10, [rfReplaceAll]);
      sl := TStringList.Create;
      sl.Text := tmp;
      if sl.Count > 0 then
      begin
        for i := 0 to sl.Count - 1 do
          sl.Strings[i] := strtrim(sl.Strings[i]);
        if bpcs = pcs then
          bpcs := db.GetBLNetPieceName(pcs);
        if (bpcs <> pcs) and (sl.IndexOf(pcs) >= 0) then
        begin
          Result := '<p><b>Alternate for:</b>'#13#10'<a href=spiece/' + bpcs + '>' + bpcs + '</a>'#13#10;
        end
        else
        begin
          Result := '<p><b>Alternate Item No:</b>'#13#10;
          for i := 0 to sl.Count - 1 do
            Result := Result + '<a href=spiece/' + sl.Strings[i] + '>' + sl.Strings[i] + '</a>' + decide(i = sl.Count - 1, '', ',') + #13#10;
        end;
        Result := Result + '</p>'#13#10;
      end;
      sl.Free;
    end;
  end;

  tmpret := '';
  for k := low(DIMSEC_PROPS) to high(DIMSEC_PROPS) do
  begin
    check := DIMSEC_PROPS[k] + '<span id="dimSec">';
    p := Pos(check, sdata);
    if p > 0 then
    begin
      tmp := ExtractAfterDelimiters(sdata, p + Length(check) - 1, '>', '<');
      trimproc(tmp);
      if tmp <> '' then
        tmpret := tmpret + '<b>' + DIMSEC_PROPS[k] + '</b>' + tmp + '<br>'#13#10;
    end;
  end;
  if tmpret <> '' then
  begin
    if Result <> '' then
      Result := Result + '<hr>'#13#10;
    Result := Result + '<p>' + tmpret + '</p>'#13#10
  end;

  check := '<br>Booklets: ';
  p := Pos(check, sdata);
  if p > 0 then
  begin
    p2 := PosEx('<', sdata, p);
    if p2 > 0 then
      if p2 - p < 200 then
      begin
        tmp := '';
        for i := p + Length(check) to p2 - 1 do
          if not (sdata[i] in [#9, #10, #13]) then
            tmp := tmp + sdata[i];
        trimproc(tmp);
        if tmp <> '' then
        begin
          if Result <> '' then
            Result := Result + '<hr>'#13#10;
          Result := Result + '<b>Booklets: </b>' + tmp + '<br>'#13#10;
        end;
      end;
  end;

  p := Pos('<b>Language:</', sdata);
  if p > 0 then
  begin
    check := '&langID=';
    p2 := PosEx(check, sdata, p);
    if p2 > 0 then
      if p2 - p < 200 then
      begin
        sid := ExtractAfterDelimiters(sdata, p2 + Length(check) - 2, '=', '"');
        tmp := ExtractAfterDelimiters(sdata, p2 + Length(check), '>', '<');
        trimproc(tmp);
        if tmp <> '' then
        begin
          if Result <> '' then
            Result := Result + '<hr>'#13#10;
          Result := Result + '<b>Language: </b>' + tmp + ' (' + sid + ')' + '<br>'#13#10;
        end;
      end;
  end;

  p := Pos('<b>Co-Branding:</', sdata);
  if p > 0 then
  begin
    check := 'promoCatID=';
    p2 := PosEx(check, sdata, p);
    if p2 > 0 then
      if p2 - p < 200 then
      begin
        sid := ExtractAfterDelimiters(sdata, p2 + Length(check) - 2, '=', '"');
        tmp := ExtractAfterDelimiters(sdata, p2 + Length(check), '>', '<');
        trimproc(tmp);
        if tmp <> '' then
        begin
          if Result <> '' then
            Result := Result + '<hr>'#13#10;
          Result := Result + '<b>Co-Branding: </b>' + tmp + ' (' + sid + ')' + '<br>'#13#10;
        end;
      end;
  end;

  check := '<b>This Item is a mold variant of the following Item(s):</b>';
  p := Pos(check, sdata);
  if p > 0 then
  begin
    p1 := PosEx('<UL>', sUdata, p + Length(check));
    if p1 > 0 then
    begin
      p2 := PosEx('</UL>', sUdata, p + Length(check) + 4);
      if p2 > p1 then
      begin
        tmp := '';
        for i := p1 to p2 + 4 do
          if not (sdata[i] in [#9, #10, #13]) then
            tmp := tmp + sdata[i];
        if Pos('<LI>', UpperCase(tmp)) > 0 then
        begin
          tmp := _parse_list(tmp);
          if tmp <> '' then
          begin
            if Result <> '' then
              Result := Result + '<hr>'#13#10;
            Result := Result +
              '<p><b>This Item is a mold variant of the following Item' + decide(CountOccurences('<li>', tmp) > 1, 's', '') + ': </b><br>'#13#10 +
                tmp + '</p>'#13#10;
          end;
        end;
      end;
    end;
  end;

  for k := low(FITS_PROPS) to high(FITS_PROPS) do
  begin
    check := FITS_PROPS[k];
    p0 := 1;
    for j := 0 to 2 do
    begin
      p := PosEx(check, sdata, p0);
      p0 := p + Length(check);
      if p > 0 then
      begin
        p1 := PosEx('<UL>', sUdata, p + Length(check));
        if (p1 > 0) and (p1 - p < 200) then
        begin
          p2 := PosEx('</UL>', sUdata, p + Length(check));
          if p2 > p1 then
          begin
            p0 := p2 + 5;
            tmp := '';
            for i := p1 to p2 + 4 do
              if not (sdata[i] in [#9, #10, #13]) then
                tmp := tmp + sdata[i];
            if Pos('<LI>', UpperCase(tmp)) > 0 then
            begin
              tmp := _parse_list(tmp);
              if tmp <> '' then
              begin
                if Result <> '' then
                  Result := Result + '<hr>'#13#10;
                Result := Result +
                  '<p><b>' + ExtractAtDelimiters(sdata, p, '>', '<') + ' </b><br>'#13#10 +
                    tmp + '</p>'#13#10;
              end;
            end;
          end;
        end;
      end
      else
        Break;
    end;
  end;

  check := '<div id="_idItemDescription" class="pciExpandMore">';
  p := Pos(check, sData);
  if p > 0 then
  begin
    tmp := '';
    for i := p + Length(check) to Length(sData) - 6 do
    begin
      if (sData[i] = '<') and (sData[i + 1] = '/') and (sData[i + 2] = 'd') and (sData[i + 3] = 'i') and (sData[i + 4] = 'v') and (sData[i + 5] = '>') then
        break;
      tmp := tmp + sData[i];
    end;
    if tmp <> '' then
    begin
      if Result <> '' then
        Result := Result + '<hr>'#13#10;
      Result := Result +
        '<p><b>Additional Notes: </b><br>' + tmp + '</p>'#13#10;
    end;
  end;

end;

end.
