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
//    Search set/minifigure Form
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit searchset;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TSearchSetForm = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    ListBox1: TListBox;
    Panel1: TPanel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    SaveDialog1: TSaveDialog;
    SaveListButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure SaveListButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function GetSetID(var setid: string): boolean;

implementation

{$R *.dfm}

uses
  bi_db, bi_globals, bi_delphi;

function GetSetID(var setid: string): boolean;
var
  f: TSearchSetForm;
  idx: integer;
begin
  Result := false;
  f := TSearchSetForm.Create(nil);
  try
    f.Edit1.Text := setid;
    idx := f.ListBox1.Items.IndexOf(setid);
    if idx >= 0 then
      f.ListBox1.ItemIndex := idx;
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      result := True;
      setid := f.Edit1.Text;
    end;
  finally
    f.Free;
  end;
end;

procedure TSearchSetForm.FormCreate(Sender: TObject);
begin
  Label2.Caption := '';
  
  if db = nil then
    Exit;

  Label2.Caption := itoa(db.AllSets.Count) + ' sets found';
  ListBox1.Items.Clear;
  ListBox1.Items.AddStrings(db.AllSets);
end;

procedure TSearchSetForm.ListBox1Click(Sender: TObject);
begin
  if ListBox1.Itemindex >= 0 then
    Edit1.Text := ListBox1.Items.Strings[ListBox1.Itemindex];
end;

procedure TSearchSetForm.SaveListButtonClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    ListBox1.Items.SaveToFile(SaveDialog1.FileName);
end;

end.
