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
//    Select sets Form
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit frm_selectsets;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, CheckLst;

type
  TSelectSetsForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel3: TPanel;
    CheckListBox1: TCheckListBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function SelectSets(const lst: TStringList): boolean;

implementation

{$R *.dfm}

function SelectSets(const lst: TStringList): boolean;
var
  f: TSelectSetsForm;
  i: integer;
begin
  result := false;
  f := TSelectSetsForm.Create(nil);
  try
    f.CheckListBox1.Items.Clear;
    f.CheckListBox1.Items.AddStrings(lst);
    for i := 0 to f.CheckListBox1.Items.Count - 1 do
      f.CheckListBox1.Checked[i] := true;

    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      lst.Clear;
      for i := 0 to f.CheckListBox1.Items.Count - 1 do
        if f.CheckListBox1.Checked[i] then
          lst.Add(f.CheckListBox1.Items.Strings[i]);
      result := true;
    end;
  finally
    f.Free;
  end;
end;

end.
