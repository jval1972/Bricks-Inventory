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
//    Dimensions xyz manipulation
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_dimensions;

interface

type
  dimensions_t = packed record
    x, y, z: double;
  end;
  dimensions_p = ^dimensions_t;

procedure init_dimensions(var p: dimensions_p);

procedure dispose_dimensions(var p: dimensions_p);

procedure wd_setx(var p: dimensions_p; const v: double);

function wd_getx(const p: dimensions_p): double;

procedure wd_sety(var p: dimensions_p; const v: double);

function wd_gety(const p: dimensions_p): double;

procedure wd_setz(var p: dimensions_p; const v: double);

function wd_getz(const p: dimensions_p): double;

implementation

uses
  bi_delphi;

const
  NUM_WI_BUFFER = 32768;

var
  num_dimensions: integer = 0;
  wi_buffer: packed array[0..NUM_WI_BUFFER - 1] of dimensions_t;

function add_dimensionrec: dimensions_p;
begin
  if num_dimensions >= NUM_WI_BUFFER then
    Result := malloc(SizeOf(dimensions_t))
  else
    Result := @wi_buffer[num_dimensions];
  Result.x := 0.0;
  Result.y := 0.0;
  Result.z := 0.0;
  inc(num_dimensions);
end;

procedure init_dimensions(var p: dimensions_p);
begin
  p := nil;
end;

procedure dispose_dimensions(var p: dimensions_p);
var
  idx: int64;
begin
  if p = nil then
    Exit;
  idx := (int64(LongWord(p)) - int64(LongWord(@wi_buffer[0]))) div SizeOf(dimensions_t);
  if (idx >=0) and (idx < NUM_WI_BUFFER) then
  begin
    p := nil;
    Exit;
  end
  else
    memfree(pointer(p), SizeOf(dimensions_t));
end;

procedure wd_setx(var p: dimensions_p; const v: double);
begin
  if p = nil then
  begin
    if v = 0.0 then
      Exit;
    p := add_dimensionrec;
  end;
  p.x := v;
end;

function wd_getx(const p: dimensions_p): double;
begin
  if p = nil then
    Result := 0.0
  else
    Result := p.x;
end;

procedure wd_sety(var p: dimensions_p; const v: double);
begin
  if p = nil then
  begin
    if v = 0.0 then
      Exit;
    p := add_dimensionrec;
  end;
  p.y := v;
end;

function wd_gety(const p: dimensions_p): double;
begin
  if p = nil then
    Result := 0.0
  else
    Result := p.y;
end;

procedure wd_setz(var p: dimensions_p; const v: double);
begin
  if p = nil then
  begin
    if v = 0.0 then
      Exit;
    p := add_dimensionrec;
  end;
  p.z := v;
end;

function wd_getz(const p: dimensions_p): double;
begin
  if p = nil then
    Result := 0.0
  else
    Result := p.z;
end;

end.
