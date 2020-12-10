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
//    Compare sets Form
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit compare2sets;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TfrmCompare2Sets = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function Compare2SetsQuery(var set1, set2: string): boolean;

implementation

{$R *.dfm}

uses
  searchset;

function Compare2SetsQuery(var set1, set2: string): boolean;
var
  f: TfrmCompare2Sets;
begin
  Result := false;
  f := TfrmCompare2Sets.Create(nil);
  try
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      set1 := f.Edit1.Text;
      set2 := f.Edit2.Text;
      result := True;
    end;
  finally
    f.Free;
  end;
end;

procedure TfrmCompare2Sets.SpeedButton1Click(Sender: TObject);
var
  setid: string;
begin
  setid := Edit1.Text;
  if GetSetID(setid) then
    Edit1.Text := setid;
end;

procedure TfrmCompare2Sets.SpeedButton2Click(Sender: TObject);
var
  setid: string;
begin
  setid := Edit2.Text;
  if GetSetID(setid) then
    Edit2.Text := setid;
end;

end.
