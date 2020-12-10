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
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit frm_update1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TUpdateForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Button3: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    Button4: TButton;
    Label2: TLabel;
    Edit2: TEdit;
    Button5: TButton;
    Label3: TLabel;
    Edit3: TEdit;
    Button6: TButton;
    Label4: TLabel;
    Edit4: TEdit;
    Label5: TLabel;
    Edit5: TEdit;
    Button7: TButton;
    Panel3: TPanel;
    Memo1: TMemo;
    Panel4: TPanel;
    Label6: TLabel;
    Label7: TLabel;
    procedure LevelNumEditKeyPress(Sender: TObject; var Key: Char);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
    procedure AddInvSetRec(const s: string);
    procedure AddNoInvSetRec(const s: string);
    procedure UpdateLabel;
  public
    { Public declarations }
  end;

function GetUpdateLinks1(const lst: TStringList): boolean;

implementation

{$R *.dfm}

uses
  bi_db, bi_delphi, bi_globals;

function GetUpdateLinks1(const lst: TStringList): boolean;
var
  f: TUpdateForm1;
begin
  result := false;
  f := TUpdateForm1.Create(nil);
  try
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      lst.AddStrings(f.Memo1.Lines);
      result := true;
    end;
  finally
    f.Free;
  end;
end;

procedure TUpdateForm1.LevelNumEditKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
  begin
    Key := #0;
    exit;
  end;
  if Key in ['0'..'9'] then
    if Length((Sender as TEdit).Text) > 3 then
    begin
      Key := #0;
      exit;
    end;
end;

procedure TUpdateForm1.AddInvSetRec(const s: string);
var
  s1: string;
begin
  s1 := 'downloadsetnorefresh/' + s;
  if Memo1.Lines.IndexOf(s1) = -1 then
    Memo1.Lines.Add(s1);
  s1 := 'sinv/' + s;
  if Memo1.Lines.IndexOf(s1) = -1 then
    Memo1.Lines.Add(s1);
  s1 := 'spiece/' + s;
  if Memo1.Lines.IndexOf(s1) = -1 then
    Memo1.Lines.Add(s1);
  UpdateLabel;
end;

procedure TUpdateForm1.AddNoInvSetRec(const s: string);
var
  s1: string;
begin
  s1 := 'UpdateSetAsPartFromBricklinknorefresh/' + s;
  if Memo1.Lines.IndexOf(s1) = -1 then
    Memo1.Lines.Add(s1);
  s1 := 'spiece/' + s;
  if Memo1.Lines.IndexOf(s1) = -1 then
    Memo1.Lines.Add(s1);
  UpdateLabel;
end;

procedure TUpdateForm1.Button3Click(Sender: TObject);
var
  lnk: string;
  i, j: integer;
  num: integer;
  lst: TStringList;
begin
  num := StrToIntDef(Edit1.Text, -1);
  if num <= 0 then
  begin
    MessageBeep(MB_ICONERROR);
    Exit;
  end;
  Screen.Cursor := crHourglass;
  try
    for i := 1 to num do
    begin
      lnk := 'https://' + BL_NET + '/catalogList.asp?pg=' + itoa(i) + '&viewInv=Y&sortBy=D&sortAsc=D&itemBrand=1000&catType=S';
      lst := db.QryNewInventoriesFromBricklink(lnk, '<a href="catalogItemInv.asp?S=');
      for j := 0 to lst.Count - 1 do
        AddInvSetRec(lst.Strings[j]);
      lst.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TUpdateForm1.Button4Click(Sender: TObject);
var
  lnk: string;
  i, j: integer;
  num: integer;
  lst: TStringList;
begin
  num := StrToIntDef(Edit2.Text, -1);
  if num <= 0 then
  begin
    MessageBeep(MB_ICONERROR);
    Exit;
  end;
  Screen.Cursor := crHourglass;
  try
    for i := 1 to num do
    begin
      lnk := 'https://' + BL_NET + '/catalogList.asp?pg=' + itoa(i) + '&viewInv=Y&sortBy=D&sortAsc=D&itemBrand=1000&catType=M';
      lst := db.QryNewInventoriesFromBricklink(lnk, '<a href="catalogItemInv.asp?M=');
      for j := 0 to lst.Count - 1 do
        AddInvSetRec(lst.Strings[j]);
      lst.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TUpdateForm1.Button5Click(Sender: TObject);
var
  lnk: string;
  i, j: integer;
  num: integer;
  lst: TStringList;
begin
  num := StrToIntDef(Edit3.Text, -1);
  if num <= 0 then
  begin
    MessageBeep(MB_ICONERROR);
    Exit;
  end;
  Screen.Cursor := crHourglass;
  try
    for i := 1 to num do
    begin
      lnk := 'https://' + BL_NET + '/catalogList.asp?pg=' + itoa(i) + '&viewInv=Y&sortBy=D&sortAsc=D&itemBrand=1000&catType=B';
      lst := db.QryNewInventoriesFromBricklink(lnk, '<a href="catalogItemInv.asp?B=');
      for j := 0 to lst.Count - 1 do
        AddInvSetRec(lst.Strings[j]);
      lst.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TUpdateForm1.Button6Click(Sender: TObject);
var
  lnk: string;
  i, j: integer;
  num: integer;
  lst: TStringList;
begin
  num := StrToIntDef(Edit4.Text, -1);
  if num <= 0 then
  begin
    MessageBeep(MB_ICONERROR);
    Exit;
  end;
  Screen.Cursor := crHourglass;
  try
    for i := 1 to num do
    begin
      lnk := 'https://' + BL_NET + '/catalogList.asp?pg=' + itoa(i) + '&viewInv=Y&sortBy=D&sortAsc=D&itemBrand=1000&catType=G';
      lst := db.QryNewInventoriesFromBricklink(lnk, '<a href="catalogItemInv.asp?G=');
      for j := 0 to lst.Count - 1 do
        AddInvSetRec(lst.Strings[j]);
      lst.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TUpdateForm1.Button7Click(Sender: TObject);
var
  lnk: string;
  i, j: integer;
  num: integer;
  lst: TStringList;
begin
  num := StrToIntDef(Edit5.Text, -1);
  if num <= 0 then
  begin
    MessageBeep(MB_ICONERROR);
    Exit;
  end;
  Screen.Cursor := crHourglass;
  try
    for i := 1 to num do
    begin
      lnk := 'https://' + BL_NET + '/catalogList.asp?v=1&pg=' + itoa(i) + '&itemInvUserID=0&sortBy=D&sortAsc=D&itemBrand=1000&catType=S';
      lst := db.QryNewSetAsPartFromBricklink(lnk, '<a href="/v2/catalog/catalogitem.page?S=');
      for j := 0 to lst.Count - 1 do
        AddNoInvSetRec(lst.Strings[j]);
      lst.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TUpdateForm1.UpdateLabel;
begin
  Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
  Label7.Update;
end;

end.
