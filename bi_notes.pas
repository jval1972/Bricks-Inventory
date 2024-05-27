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
//    Piece notes stuff
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_notes;

interface

function BI_AutoNotesBL(const pcs: string; const path: string): string;

function BI_AutoNotesBS(const pcs: string): string;

implementation

uses
  bi_delphi, SysUtils, Classes, StrUtils, bi_db, bi_globals, bi_utils;

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

  ITEMS_PROPS: array[0..5] of string[120] = (
    '<b>This Item is a mold variant of the following Item(s):</b>',
    '<b>This Item is similar to and has the same number as the following Item(s):</b>',
    '<b>This Item is similar to, but has a different number than the following Item(s):</b>',
    '<b>This Item pairs with the following Item(s):</b>',
    '<b>This Item is similar in pattern to the following Item(s):</b>',
    '<b>This Item has the same or similar content, but was published in a different language than the following Item(s):</b>'
  );

type
  bricksetprop_t = record
    propname: string[20];
    display: boolean;
  end;
  bricksetprop_p = ^bricksetprop_t;

const
  NUM_BRICKSET_PROPS = 18;

  BRICKSET_PROPS: array[0..NUM_BRICKSET_PROPS - 1] of bricksetprop_t = (
    (propname: 'Number'             ; display: False),
    (propname: 'Name'               ; display: False),
    (propname: 'Type'               ; display: False),
    (propname: 'Theme group'        ; display: True),
    (propname: 'Theme'              ; display: True),
    (propname: 'Subtheme'           ; display: True),
    (propname: 'Year released'      ; display: False),
    (propname: 'Launch/exit'        ; display: True),
    (propname: 'Pieces'             ; display: True),
    (propname: 'Designer'           ; display: True),
    (propname: 'RRP'                ; display: True),
    (propname: 'Price per piece'    ; display: False),
    (propname: 'Age range'          ; display: True),
    (propname: 'Packaging'          ; display: True),
    (propname: 'Dimensions'         ; display: True),
    (propname: 'Barcodes'           ; display: True),
    (propname: 'LEGO item numbers'  ; display: True),
    (propname: 'Availability'       ; display: True)
  );

function _parse_list(const s: string): string;
const
  TYPE_SPIECE_CHARS = 'PSMCBG';
  TYPE_SINV_CHARS = 'SMBG';
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
  for i := 1 to Length(TYPE_SINV_CHARS) do
    tmp := StringReplace(tmp, '<a href="http://www.bricklink.com/catalogItemInv.asp?' + TYPE_SINV_CHARS[i] + '=', '<a href="sinv/', [rfReplaceAll, rfIgnoreCase]);
  tmp := StringReplace(tmp, '<a href="http://www.bricklink.com/catalogItemInv.asp?P=', '<a href="spiececinv/', [rfReplaceAll, rfIgnoreCase]);
  Result := tmp;
end;

function BI_AutoNotesBL(const pcs: string; const path: string): string;
var
  sData, sUdata: string;
  bpcs: string;
  i, j, k, p, p0, p1, p2: integer;
  check: string;
  tmp, scob: string;
  sprop: string;
  sid: string;
  tmpret: string;
  sl: TStringList;
begin
  Result := '';

  bpcs := pcs;
  sData := LoadStringFromFile(path + pcs + '.htm');
  if sData = '' then
  begin
    bpcs := db.GetBLNetPieceName(pcs);
    if bpcs <> pcs then
      sData := LoadStringFromFile(path + bpcs + '.htm');
    if sData = '' then
      Exit;
  end;

  // bricklink notes
  if sData <> '' then
  begin
    sUdata := UpperCase(sData);

    check := 'Alternate Item No: <span';
    p := Pos(check, sData);
    if p > 0 then
    begin
      tmp := ExtractAfterDelimiters(sData, p + Length(check) - 1, '>', '<');
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
      p := Pos(check, sData);
      if p > 0 then
      begin
        tmp := ExtractAfterDelimiters(sData, p + Length(check) - 1, '>', '<');
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
    p := Pos(check, sData);
    if p > 0 then
    begin
      p2 := PosEx('<', sData, p);
      if p2 > 0 then
        if p2 - p < 200 then
        begin
          tmp := '';
          for i := p + Length(check) to p2 - 1 do
            if not (sData[i] in [#9, #10, #13]) then
              tmp := tmp + sData[i];
          trimproc(tmp);
          if tmp <> '' then
          begin
            if Result <> '' then
              Result := Result + '<hr>'#13#10;
            Result := Result + '<b>Booklets: </b>' + tmp + '<br>'#13#10;
          end;
        end;
    end;

    p := Pos('<b>Language:</', sData);
    if p > 0 then
    begin
      check := '&langID=';
      p2 := PosEx(check, sData, p);
      if p2 > 0 then
        if p2 - p < 200 then
        begin
          sid := ExtractAfterDelimiters(sData, p2 + Length(check) - 2, '=', '"');
          tmp := ExtractAfterDelimiters(sData, p2 + Length(check), '>', '<');
          trimproc(tmp);
          if tmp <> '' then
          begin
            if Result <> '' then
              Result := Result + '<hr>'#13#10;
            Result := Result + '<b>Language: </b>' + tmp + ' (' + sid + ')' + '<br>'#13#10;
          end;
        end;
    end;

    p := Pos('<b>Co-Branding:</', sData);
    if p > 0 then
    begin
      scob := '';
      check := 'promoCatID=';
      for i := 1 to 10 do
      begin
        p2 := PosEx(check, sData, p);
        if (p2 > 0) and (p2 - p < 200) then
        begin
          p := p2 + Length(check);
          sid := ExtractAfterDelimiters(sData, p2 + Length(check) - 2, '=', '"');
          tmp := ExtractAfterDelimiters(sData, p2 + Length(check), '>', '<');
          trimproc(tmp);
          if tmp <> '' then
          begin
            if i = 1 then
              scob := tmp + ' (' + sid + ')'
            else
              scob := scob + ', ' + tmp + ' (' + sid + ')'
          end;
        end
        else
          break;
      end;
      if scob <> '' then
      begin
        if Result <> '' then
          Result := Result + '<hr>'#13#10;
        Result := Result + '<b>Co-Branding: </b>' + scob + '<br>'#13#10;
      end;
    end;

    for k := low(ITEMS_PROPS) to high(ITEMS_PROPS) do
    begin
      check := ITEMS_PROPS[k];
      p := Pos(check, sData);
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
              if not (sData[i] in [#9, #10, #13]) then
                tmp := tmp + sData[i];
            if Pos('<LI>', UpperCase(tmp)) > 0 then
            begin
              tmp := _parse_list(tmp);
              if tmp <> '' then
              begin
                if Result <> '' then
                  Result := Result + '<hr>'#13#10;
                if CountOccurences('<li>', tmp) > 1 then
                  check := StringReplace(check, 'Item(s)', 'Items', [rfReplaceAll, rfIgnoreCase])
                else
                  check := StringReplace(check, 'Item(s)', 'Item', [rfReplaceAll, rfIgnoreCase]);
                Result := Result + '<p>' + check + '<br>'#13#10 + tmp + '</p>'#13#10;
              end;
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
        p := PosEx(check, sData, p0);
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
                if not (sData[i] in [#9, #10, #13]) then
                  tmp := tmp + sData[i];
              if Pos('<LI>', UpperCase(tmp)) > 0 then
              begin
                tmp := _parse_list(tmp);
                if tmp <> '' then
                begin
                  if Result <> '' then
                    Result := Result + '<hr>'#13#10;
                  sprop := ExtractAtDelimiters(sData, p, '>', '<');
                  if CountOccurences('<li>', tmp) > 1 then
                    sprop := StringReplace(sprop, 'Item(s)', 'Items', [rfReplaceAll, rfIgnoreCase])
                  else
                    sprop := StringReplace(sprop, 'Item(s)', 'Item', [rfReplaceAll, rfIgnoreCase]);
                  Result := Result + '<p><b>' + sprop + ' </b><br>'#13#10 + tmp + '</p>'#13#10;
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
  end; // End bricklink notes

end;

function BI_AutoNotesBS(const pcs: string): string;
var
  i: integer;
  sData: string;
  tmp: string;
  p, p0, p1, p2: integer;
  check: string;
  sbr: string;
begin
  Result := '';
  // BrickSet notes
  if db.IsLikeSetNumber(pcs) then
    if db.IsSet(pcs) and not db.IsMoc(pcs) then
    begin
      if fsize(BrickSetCachePath(pcs)) <= 0 then
        db.DownloadSetPageFromBrickSet(pcs);
      if fsize(BrickSetCachePath(pcs)) > 0 then
      begin
        sData := LoadStringFromFile(BrickSetCachePath(pcs));
        sData := Utf8ToAnsi(sData);
        p := Pos('<section class="featurebox ">', sData);
        if p > 0 then
        begin
          sbr := '';
          for i := 0 to NUM_BRICKSET_PROPS - 1 do
            if BRICKSET_PROPS[i].display then
            begin
              check := '<dt>' + BRICKSET_PROPS[i].propname + '</dt>';
              p0 := PosEx(check, sData, p);
              if p0 > p then
              begin
                p1 := PosEx('<dd>', sData, p0);
                p2 := PosEx('</dd>', sData, p1);
                if (p1 > p0) and (p2 > p1 + 4) then
                begin
                  tmp := Copy(sData, p1 + 4, p2 - p1 - 4);
                  tmp := RemoveHTMLTagsNoBR(tmp);
                  trimproc(tmp);
                  if tmp <> '' then
                  begin
                    if sbr = '' then
                      sbr := '<p><b>BrickSet information</b><br>'#13#10'<dl>'#13#10'<table>';
                    sbr := sbr + '<tr><td width="100px"><b>' + BRICKSET_PROPS[i].propname + ': </b></td>'#13#10;
                    sbr := sbr + '<td>' + tmp + '</td></tr>'#13#10;
                  end;
                end;
              end;
            end;
          if sbr <> '' then
          begin
            sbr := sbr + '</table></p>'#13#10;
            if Result <> '' then
              Result := Result + '<hr>'#13#10;
            Result := Result + sbr;
          end;
        end;
      end;
    end;  // End BrickSet notes
end;

end.
