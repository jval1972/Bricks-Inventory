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
//  Load and save configurable file - Default values
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_defs;

interface

type
  ttype_t = (tText, tInteger, tBoolean, tGroup);

  default_t = record
    name: string;
    location: pointer;
    setable: boolean;
    defaultsvalue: string;
    defaultivalue: integer;
    defaultbvalue: boolean;
    _type: ttype_t;
  end;
  Pdefault_t = ^default_t;


var
  dodraworderinfo: boolean;
  usemultithread: boolean;
  domultipagedocuments: boolean;
  dpagesize: integer;
  savealwayspartinvinfo: boolean;
  generatethumbnailsondemand: boolean;

const
  NUMDEFAULTS = 8;

  defaults: array[0..NUMDEFAULTS - 1] of default_t = (
    (name: 'General';
     location: nil;
     setable: false;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'dodraworderinfo';
     location: @dodraworderinfo;
     setable: true;
     defaultsvalue: '1';
     defaultivalue: 1;
     defaultbvalue: True;
     _type: tBoolean),

    (name: 'usemultithread';
     location: @usemultithread;
     setable: true;
     defaultsvalue: '1';
     defaultivalue: 1;
     defaultbvalue: True;
     _type: tBoolean),

    (name: 'savealwayspartinvinfo';
     location: @savealwayspartinvinfo;
     setable: true;
     defaultsvalue: '0';
     defaultivalue: 0;
     defaultbvalue: False;
     _type: tBoolean),

    (name: 'generatethumbnailsondemand';
     location: @generatethumbnailsondemand;
     setable: true;
     defaultsvalue: '0';
     defaultivalue: 0;
     defaultbvalue: True;
     _type: tBoolean),

    (name: 'MultiPage';
     location: nil;
     setable: false;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'domultipagedocuments';
     location: @domultipagedocuments;
     setable: true;
     defaultsvalue: '1';
     defaultivalue: 1;
     defaultbvalue: True;
     _type: tBoolean),

    (name: 'dpagesize';
     location: @dpagesize;
     setable: true;
     defaultsvalue: '100';
     defaultivalue: 100;
     defaultbvalue: True;
     _type: tInteger)

  );


procedure BI_LoadDefaults(const fname: string);

procedure BI_SaveDefaults(const fname: string);

implementation

uses
  bi_delphi, Classes, bi_utils;

procedure BI_LoadDefaults(const fname: string);
var
  i: integer;
  j: integer;
  idx: integer;
  pd: Pdefault_t;
  s: TStringList;
  n: string;
begin
  // set everything to base values
  for i := 0 to NUMDEFAULTS - 1 do
    if defaults[i]._type = tInteger then
      PInteger(defaults[i].location)^ := defaults[i].defaultivalue
    else if defaults[i]._type = tBoolean then
      PBoolean(defaults[i].location)^ := defaults[i].defaultbvalue
    else if defaults[i]._type = tText then
      PString(defaults[i].location)^ := defaults[i].defaultsvalue;

  if fexists(fname) then
  begin
    s := TStringList.Create;
    try
      // read the file in, overriding any set defaults
      s.LoadFromFile(fname);

      for i := 0 to s.Count - 1 do
      begin
        idx := -1;
        n := strlower(s.Names[i]);
        for j := 0 to NUMDEFAULTS - 1 do
          if strlower(defaults[j].name) = n then
          begin
            idx := j;
            break;
          end;

        if idx > -1 then
        begin
          pd := @defaults[idx];
          if pd._type = tInteger then
            PInteger(pd.location)^ := atoi(s.Values[n], PInteger(pd.location)^)
          else if pd._type = tBoolean then
             PBoolean(pd.location)^ := atoi(s.Values[n]) <> 0
          else if pd._type = tText then
             PString(pd.location)^ := s.Values[n];
        end;
      end;
    finally
      s.Free;
    end;
  end;
end;

procedure BI_SaveDefaults(const fname: string);
var
  i: integer;
  pd: Pdefault_t;
  s: TStringList;
begin
  s := TStringList.Create;
  try
    pd := @defaults[0];
    for i := 0 to NUMDEFAULTS - 1 do
    begin
      if pd._type = tInteger then
        s.Add(pd.name + '=' + itoa(PInteger(pd.location)^))
      else if pd._type = tText then
        s.Add(pd.name + '=' + PString(pd.location)^)
      else if pd._type = tBoolean then
        s.Add(pd.name + '=' + itoa(intval(PBoolean(pd.location)^)))
      else if pd._type = tGroup then
      begin
        if i <> 0 then
          s.Add('');
        s.Add('[' + pd.name + ']');
      end;
      inc(pd);
    end;

    backupfile(fname);
    s.SaveToFile(fname);

  finally
    s.Free;
  end;
end;

end.
