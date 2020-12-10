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
//  MultiThreading Iterators
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_iterators;

interface

uses
  Classes, bi_multithread, bi_threads;

// Multithreading parameters
type
  iterator_base_t = record
    idx: integer;
    numidxs: integer;
    list: TStringList;
  end;
  iterator_base_p = ^iterator_base_t;

function MT_Iterate_Base(const func: threadfunc_t): TStringList;

type
  iterator_double_t = record
    idx: integer;
    numidxs: integer;
    param: double;
    list: TStringList;
  end;
  iterator_double_p = ^iterator_double_t;

function MT_Iterate_Double(const func: threadfunc_t; const parm: double): TStringList;

implementation

uses
  bi_system;

function MT_Iterate_Base(const func: threadfunc_t): TStringList;
var
  parm1, parm2, parm3, parm4, parm5, parm6, parm7, parm8: iterator_base_t;
  oldm: boolean;
begin
  result := TStringList.Create;

  if I_GetNumCPUs <= 4 then
  begin
    parm1.idx := 0;
    parm1.numidxs := 4;
    parm1.list := result;

    parm2.idx := 1;
    parm2.numidxs := 4;
    parm2.list := TStringList.Create;

    parm3.idx := 2;
    parm3.numidxs := 4;
    parm3.list := TStringList.Create;

    parm4.idx := 3;
    parm4.numidxs := 4;
    parm4.list := TStringList.Create;

    oldm := ismultithread;
    ismultithread := true;
    MT_Execute(
      func, @parm1,
      func, @parm2,
      func, @parm3,
      func, @parm4
    );
    ismultithread := oldm;

    result.AddStrings(parm2.list);
    result.AddStrings(parm3.list);
    result.AddStrings(parm4.list);

    parm2.list.Free;
    parm3.list.Free;
    parm4.list.Free;
  end
  else if I_GetNumCPUs <= 6 then
  begin
    parm1.idx := 0;
    parm1.numidxs := 6;
    parm1.list := result;

    parm2.idx := 1;
    parm2.numidxs := 6;
    parm2.list := TStringList.Create;

    parm3.idx := 2;
    parm3.numidxs := 6;
    parm3.list := TStringList.Create;

    parm4.idx := 3;
    parm4.numidxs := 6;
    parm4.list := TStringList.Create;

    parm5.idx := 4;
    parm5.numidxs := 6;
    parm5.list := TStringList.Create;

    parm6.idx := 5;
    parm6.numidxs := 6;
    parm6.list := TStringList.Create;

    oldm := ismultithread;
    ismultithread := true;
    MT_Execute(
      func, @parm1,
      func, @parm2,
      func, @parm3,
      func, @parm4,
      func, @parm5,
      func, @parm6
    );
    ismultithread := oldm;

    result.AddStrings(parm2.list);
    result.AddStrings(parm3.list);
    result.AddStrings(parm4.list);
    result.AddStrings(parm5.list);
    result.AddStrings(parm6.list);

    parm2.list.Free;
    parm3.list.Free;
    parm4.list.Free;
    parm5.list.Free;
    parm6.list.Free;
  end
  else
  begin
    parm1.idx := 0;
    parm1.numidxs := 8;
    parm1.list := result;

    parm2.idx := 1;
    parm2.numidxs := 8;
    parm2.list := TStringList.Create;

    parm3.idx := 2;
    parm3.numidxs := 8;
    parm3.list := TStringList.Create;

    parm4.idx := 3;
    parm4.numidxs := 8;
    parm4.list := TStringList.Create;

    parm5.idx := 4;
    parm5.numidxs := 8;
    parm5.list := TStringList.Create;

    parm6.idx := 5;
    parm6.numidxs := 8;
    parm6.list := TStringList.Create;

    parm7.idx := 6;
    parm7.numidxs := 8;
    parm7.list := TStringList.Create;

    parm8.idx := 7;
    parm8.numidxs := 8;
    parm8.list := TStringList.Create;

    oldm := ismultithread;
    ismultithread := true;
    MT_Execute(
      func, @parm1,
      func, @parm2,
      func, @parm3,
      func, @parm4,
      func, @parm5,
      func, @parm6,
      func, @parm7,
      func, @parm8
    );
    ismultithread := oldm;

    result.AddStrings(parm2.list);
    result.AddStrings(parm3.list);
    result.AddStrings(parm4.list);
    result.AddStrings(parm5.list);
    result.AddStrings(parm6.list);
    result.AddStrings(parm7.list);
    result.AddStrings(parm8.list);

    parm2.list.Free;
    parm3.list.Free;
    parm4.list.Free;
    parm5.list.Free;
    parm6.list.Free;
    parm7.list.Free;
    parm8.list.Free;
  end;
end;
  
function MT_Iterate_Double(const func: threadfunc_t; const parm: double): TStringList;
var
  parm1, parm2, parm3, parm4, parm5, parm6, parm7, parm8: iterator_double_t;
  oldm: boolean;
begin
  result := TStringList.Create;

  if I_GetNumCPUs <= 4 then
  begin
    parm1.idx := 0;
    parm1.numidxs := 4;
    parm1.param := parm;
    parm1.list := result;

    parm2.idx := 1;
    parm2.numidxs := 4;
    parm2.param := parm;
    parm2.list := TStringList.Create;

    parm3.idx := 2;
    parm3.numidxs := 4;
    parm3.param := parm;
    parm3.list := TStringList.Create;

    parm4.idx := 3;
    parm4.numidxs := 4;
    parm4.param := parm;
    parm4.list := TStringList.Create;

    oldm := ismultithread;
    ismultithread := true;
    MT_Execute(
      func, @parm1,
      func, @parm2,
      func, @parm3,
      func, @parm4
    );
    ismultithread := oldm;

    result.AddStrings(parm2.list);
    result.AddStrings(parm3.list);
    result.AddStrings(parm4.list);

    parm2.list.Free;
    parm3.list.Free;
    parm4.list.Free;
  end
  else if I_GetNumCPUs <= 6 then
  begin
    parm1.idx := 0;
    parm1.numidxs := 6;
    parm1.param := parm;
    parm1.list := result;

    parm2.idx := 1;
    parm2.numidxs := 6;
    parm2.param := parm;
    parm2.list := TStringList.Create;

    parm3.idx := 2;
    parm3.numidxs := 6;
    parm3.param := parm;
    parm3.list := TStringList.Create;

    parm4.idx := 3;
    parm4.numidxs := 6;
    parm4.param := parm;
    parm4.list := TStringList.Create;

    parm5.idx := 4;
    parm5.numidxs := 6;
    parm5.param := parm;
    parm5.list := TStringList.Create;

    parm6.idx := 5;
    parm6.numidxs := 6;
    parm6.param := parm;
    parm6.list := TStringList.Create;

    oldm := ismultithread;
    ismultithread := true;
    MT_Execute(
      func, @parm1,
      func, @parm2,
      func, @parm3,
      func, @parm4,
      func, @parm5,
      func, @parm6
    );
    ismultithread := oldm;

    result.AddStrings(parm2.list);
    result.AddStrings(parm3.list);
    result.AddStrings(parm4.list);
    result.AddStrings(parm5.list);
    result.AddStrings(parm6.list);

    parm2.list.Free;
    parm3.list.Free;
    parm4.list.Free;
    parm5.list.Free;
    parm6.list.Free;
  end
  else
  begin
    parm1.idx := 0;
    parm1.numidxs := 8;
    parm1.param := parm;
    parm1.list := result;

    parm2.idx := 1;
    parm2.numidxs := 8;
    parm2.param := parm;
    parm2.list := TStringList.Create;

    parm3.idx := 2;
    parm3.numidxs := 8;
    parm3.param := parm;
    parm3.list := TStringList.Create;

    parm4.idx := 3;
    parm4.numidxs := 8;
    parm4.param := parm;
    parm4.list := TStringList.Create;

    parm5.idx := 4;
    parm5.numidxs := 8;
    parm5.param := parm;
    parm5.list := TStringList.Create;

    parm6.idx := 5;
    parm6.numidxs := 8;
    parm6.param := parm;
    parm6.list := TStringList.Create;

    parm7.idx := 6;
    parm7.numidxs := 8;
    parm7.param := parm;
    parm7.list := TStringList.Create;

    parm8.idx := 7;
    parm8.numidxs := 8;
    parm8.param := parm;
    parm8.list := TStringList.Create;

    oldm := ismultithread;
    ismultithread := true;
    MT_Execute(
      func, @parm1,
      func, @parm2,
      func, @parm3,
      func, @parm4,
      func, @parm5,
      func, @parm6,
      func, @parm7,
      func, @parm8
    );
    ismultithread := oldm;

    result.AddStrings(parm2.list);
    result.AddStrings(parm3.list);
    result.AddStrings(parm4.list);
    result.AddStrings(parm5.list);
    result.AddStrings(parm6.list);
    result.AddStrings(parm7.list);
    result.AddStrings(parm8.list);

    parm2.list.Free;
    parm3.list.Free;
    parm4.list.Free;
    parm5.list.Free;
    parm6.list.Free;
    parm7.list.Free;
    parm8.list.Free;
  end;
end;

end.
