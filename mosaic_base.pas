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
//    Mosaic Base
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit mosaic_base;

interface

uses
  Windows, Classes, Graphics, bi_delphi;

type
  mosaiccolorindex_t = record
    idx: integer;
    pieces: TStringList;
  end;

const
  NUMCOLORINDEXES = 36;

type
  mosaicbrick_t = record
    brick: string[16];
    x, y: Integer;
    colors: TDNumberList;
    use: Boolean;
  end;
  mosaicbrick_a = array[0..$FF] of mosaicbrick_t;
  mosaicbrick_pa = ^mosaicbrick_a;

const
  MAXDIMENTION = 256;
  MOSAICBITMAPZOOM = 16;

type
  mosaicitem_t = record
    coloridx: integer;
    checked: Boolean;
  end;
  mosaicitem_p = ^mosaicitem_t;

  mosaic_t = array[0..MAXDIMENTION - 1, 0..MAXDIMENTION - 1] of mosaicitem_t;
  mosaic_p = ^mosaic_t;

function brickindex(const C: mosaicbrick_pa; const nbricks: integer; const brick: string): Integer;

function brickdimentionx(const C: mosaicbrick_pa; const nbricks: integer; const brick: string): Integer;

function brickdimentiony(const C: mosaicbrick_pa; const nbricks: integer; const brick: string): Integer;

function brickdimentions(const C: mosaicbrick_pa; const nbricks: integer; const brick: string): Integer;

function SortListByPrice(const C: mosaicbrick_pa; const nbricks: integer; List: TStringList; Index1, Index2: Integer): Integer;

type
  THSV = record  // hue saturation value (HSV)
    Hue, Sat, Val: Double;
  end;

type
  TYUV = record  // hue saturation value (HSV)
    Y, U, V: Double;
  end;

const
  C_colorindexes: array[0..NUMCOLORINDEXES - 1] of mosaiccolorindex_t = (
    (idx: 0),
    (idx: 1),
    (idx: 2),
    (idx: 3),
    (idx: 4),
    (idx: 5),
    (idx: 6),
    (idx: 7),
    (idx: 8),
    (idx: 13),
    (idx: 14),
    (idx: 15),
    (idx: 19),
    (idx: 22),
    (idx: 25),
    (idx: 27),
    (idx: 28),
    (idx: 29),
    (idx: 70),
    (idx: 71),
    (idx: 72),
    (idx: 73),
    (idx: 84),
    (idx: 85),
    (idx: 191),
    (idx: 226),
    (idx: 272),
    (idx: 288),
    (idx: 297),
    (idx: 308),
    (idx: 320),
    (idx: 321),
    (idx: 326),
    (idx: 378),
    (idx: 379),
    (idx: 484)
  );

function RGB2HSV(const c: Tcolor): THSV;

function RGB2YUV(const c: Tcolor): TYUV;

implementation

uses
  bi_db, bi_utils;

function brickindex(const C: mosaicbrick_pa; const nbricks: integer; const brick: string): Integer;
var
  i: integer;
begin
  for i := 0 to nbricks - 1 do
    if brick = C[i].brick then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

function brickdimentionx(const C: mosaicbrick_pa; const nbricks: integer; const brick: string): Integer;
var
  i: integer;
begin
  for i := 0 to nbricks - 1 do
    if brick = C[i].brick then
    begin
      Result := C[i].x;
      Exit;
    end;
  Result := -1;
end;

function brickdimentiony(const C: mosaicbrick_pa; const nbricks: integer; const brick: string): Integer;
var
  i: integer;
begin
  for i := 0 to nbricks - 1 do
    if brick = C[i].brick then
    begin
      Result := C[i].y;
      Exit;
    end;
  Result := -1;
end;

function brickdimentions(const C: mosaicbrick_pa; const nbricks: integer; const brick: string): Integer;
var
  i: integer;
begin
  for i := 0 to nbricks - 1 do
    if brick = C[i].brick then
    begin
      Result := C[i].x * C[i].y;
      Exit;
    end;
  Result := -1;
end;

function SortListByPrice(const C: mosaicbrick_pa; const nbricks: integer; List: TStringList; Index1, Index2: Integer): Integer;
var
  price1, price2: Double;
begin
  if Index1 = index2 then
  begin
    Result := 0;
    Exit;
  end;
  price1 := (List.Objects[Index1] as TPieceColorInfo).EvaluatePriceUsed / brickdimentions(C, nbricks, List.Strings[index1]);
  price2 := (List.Objects[Index2] as TPieceColorInfo).EvaluatePriceUsed / brickdimentions(C, nbricks, List.Strings[index2]);
  if price1 > price2 then
    Result := 1
  else if price1 = price2 then
    Result := 0
  else
    Result := -1;
end;

function RGB2HSV(const c: Tcolor): THSV;
var
  Min_, Max_, Delta: Double;
  H , S , V: Double;
  R, G, B : Byte;
begin
  R := GetRValue(c);
  G := GetGValue(c);
  B := GetBValue(c);
  H := 0.0 ;
  Min_ := MinI(MinI(R, G), B);
  Max_ := MaxI(MaxI(R, G), B);
  Delta := (Max_ - Min_);
  V := Max_ ;
  If ( Max_ <> 0.0 ) then
    S := 255.0 * Delta / Max_
  else
    S := 0.0 ;
  If (S <> 0.0) then
    begin
      If R = Max_ then
        H := (G - B) / Delta
      else
        If G = Max_ then
          H := 2.0 + (B - R) / Delta
        else
          If B = Max_ then
            H := 4.0 + (R - G) / Delta
    End
  else
    H := -1.0 ;
  H := H * 60 ;
  If H < 0.0 then
    H := H + 360.0;
  with Result do
    begin
      Hue := H ;            // Hue -> 0..360
      Sat := S * 100 / 255; // Saturation -> 0..100 %
      Val := V * 100 / 255; // Value - > 0..100 %
    end;
end;

function RGB2YUV(const c: Tcolor): TYUV;
var
  r, g, b: integer;
begin
  r := GetRValue(c);
  g := GetGValue(c);
  b := GetBValue(c);
  result.Y := 0.299 * r + 0.587 * g + 0.114 * b;
  result.U := -0.14713 * r - 0.28886 * g + 0.436 * b;
  result.V := 0.615 * r - 0.51499 * g - 0.10001 * b;
end;

end.
