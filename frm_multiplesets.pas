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
  Dialogs, StdCtrls, ExtCtrls;

type
  TMultipleSetsForm = class(TForm)
    Label1: TLabel;
    ListBox1: TListBox;
    Button1: TButton;
    Button2: TButton;
    AddButton: TButton;
    RemoveButton: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    OpenList1: TButton;
    SaveList1: TButton;
    procedure AddButtonClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure OpenList1Click(Sender: TObject);
    procedure SaveList1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function GetMultipleSetsList(const l: TStringList): Boolean;

implementation

uses
  bi_utils, searchset, bi_delphi;

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

procedure TMultipleSetsForm.OpenList1Click(Sender: TObject);
var
  sL: TStringList;
  s1, s2, s3: string;
  i, j: integer;
begin
  if OpenDialog1.Execute then
  begin
    ListBox1.Items.Clear;
    sL := TStringList.Create;
    sL.LoadFromFile(OpenDialog1.FileName);
    sl.Text := strremovespaces(sl.Text);
    if sL.Count = 0 then
    begin
      sL.Free;
      Exit;
    end;
    if CharPos(',', sL.Text) > 0 then
    begin
      for i := 0 to sL.Count - 1 do
      begin
        splitstring(sL.Strings[i], s1, s2, s3, ',');
        if itoa(atoi(s2)) = s2 then
        begin
          for j := 0 to atoi(s2) - 1 do
            ListBox1.Items.Add(s1);
        end
        else
          ListBox1.Items.Add(s1);
      end;
    end
    else
      ListBox1.Items.LoadFromFile(OpenDialog1.FileName);
    sL.Free;
  end;
end;

procedure TMultipleSetsForm.SaveList1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    backupfile(SaveDialog1.FileName);
    ListBox1.Items.SaveToFile(SaveDialog1.FileName);
  end;
end;

end.
