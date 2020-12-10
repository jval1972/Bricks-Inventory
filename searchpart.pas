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
//    Search part Form
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit searchpart;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TSearchPartForm = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    ListBox1: TListBox;
    Panel1: TPanel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    SaveListButton: TButton;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SaveListButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    pieceret: string;
    existingpiece: boolean;
  end;

function GetPieceID(var pieceid: string): boolean;

function GetExistingPieceID(var pieceid: string): boolean;

implementation

{$R *.dfm}

uses
  bi_db, bi_globals, bi_delphi;

function GetPieceID(var pieceid: string): boolean;
var
  f: TSearchPartForm;
  idx: integer;
begin
  Result := false;
  f := TSearchPartForm.Create(nil);
  try
    f.Edit1.Text := pieceid;
    idx := f.ListBox1.ItemIndex;
    if idx >= 0 then
      f.ListBox1.ItemIndex := idx;
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      result := True;
      pieceid := f.pieceret;
    end;
  finally
    f.Free;
  end;
end;

function GetExistingPieceID(var pieceid: string): boolean;
var
  f: TSearchPartForm;
  idx: integer;
begin
  Result := false;
  f := TSearchPartForm.Create(nil);
  try
    f.existingpiece := true;
    f.Edit1.Text := pieceid;
    idx := f.ListBox1.ItemIndex;
    if idx >= 0 then
      f.ListBox1.ItemIndex := idx;
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      result := True;
      pieceid := f.pieceret;
    end;
  finally
    f.Free;
  end;
end;

procedure TSearchPartForm.FormCreate(Sender: TObject);
begin
  Label2.Caption := '';

  if db = nil then
    Exit;

  Label2.Caption := itoa(db.AllPieces.Count) + ' pieces found';
  ListBox1.Items.Clear;
  ListBox1.Items.AddStrings(db.AllPieces);

  existingpiece := false;
  pieceret := '';
end;

procedure TSearchPartForm.ListBox1Click(Sender: TObject);
begin
  if ListBox1.Itemindex >= 0 then
    Edit1.Text := ListBox1.Items.Strings[ListBox1.Itemindex];
end;

procedure TSearchPartForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  check: string;
  i: integer;
begin
  if modalresult <> mrOK then
  begin
    CanClose := true;
    exit;
  end;
  
  check := Trim(Edit1.Text);
  pieceret := check;
  check := UpperCase(check);

  for i := 0 to ListBox1.Items.Count - 1 do
    if UpperCase(ListBox1.Items.Strings[i]) = check then
      pieceret := ListBox1.Items.Strings[i];

  if not existingpiece then
  begin
    CanClose := true;
    exit;
  end;

  CanClose := ListBox1.Items.IndexOf(pieceret) >= 0;
  if not CanClose then
    ShowMessage('Please select an existing piece!');
end;

procedure TSearchPartForm.SaveListButtonClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    ListBox1.Items.SaveToFile(SaveDialog1.FileName);
end;

end.
