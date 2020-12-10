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
//    Multiple sets input Form
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit frm_multiplesets;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TMultipleSetsForm = class(TForm)
    Label1: TLabel;
    ListBox1: TListBox;
    Button1: TButton;
    Button2: TButton;
    AddButton: TButton;
    RemoveButton: TButton;
    Button3: TButton;
    procedure AddButtonClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function GetMultipleSetsList(const l: TStringList): Boolean;

implementation

uses
  searchset;

{$R *.dfm}

function GetMultipleSetsList(const l: TStringList): Boolean;
var
  f: TMultipleSetsForm;
begin
  result := false;
  f := TMultipleSetsForm.Create(nil);
  try
    f.ListBox1.Items.AddStrings(l);
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      l.Clear;
      l.AddStrings(f.ListBox1.Items);
      Result := true;
    end;
  finally
    f.Free;
  end;
end;

procedure TMultipleSetsForm.AddButtonClick(Sender: TObject);
var
  setid: string;
begin
  if GetSetID(setid) then
    ListBox1.Items.Add(setid);
end;

procedure TMultipleSetsForm.RemoveButtonClick(Sender: TObject);
begin
  if ListBox1.ItemIndex >= 0 then
    ListBox1.Items.Delete(ListBox1.ItemIndex);
end;

procedure TMultipleSetsForm.Button3Click(Sender: TObject);
begin
  ListBox1.Items.Clear;
end;

end.
