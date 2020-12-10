//------------------------------------------------------------------------------
//
//  BrickInventory: A tool for managing your brick collection
//  Copyright (C) 2014-2019 by Jim Valavanis
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

unit frm_update4;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TTUpdateNewPartsFromBLForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Button3: TButton;
    Panel3: TPanel;
    Memo1: TMemo;
    Panel4: TPanel;
    Label6: TLabel;
    Label7: TLabel;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Button4: TButton;
    Panel5: TPanel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Button5: TButton;
    Label3: TLabel;
    Edit3: TEdit;
    Button6: TButton;
    Label4: TLabel;
    Edit4: TEdit;
    Button7: TButton;
    Label5: TLabel;
    Edit5: TEdit;
    procedure Button3Click(Sender: TObject);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
    procedure AddNewCatalogRec(const s: string);
  public
    { Public declarations }
  end;

function UpdateNewPartsFromBricklink(const lst: TStringList): boolean;

implementation

{$R *.dfm}

uses
  bi_db, bi_globals, bi_delphi;

function UpdateNewPartsFromBricklink(const lst: TStringList): boolean;
var
  f: TTUpdateNewPartsFromBLForm;
begin
  result := false;
  if lst = nil then
    Exit;
  f := TTUpdateNewPartsFromBLForm.Create(nil);
  try
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      lst.AddStrings(f.Memo1.Lines);
      result := True;
    end;
  finally
    f.Free;
  end;
end;

procedure MergeSList(const s1, s2: TStringList);
var
  s3: TStringList;
  i: integer;
begin
  s3 := TStringList.Create;
  s1.Sorted := True;
  for i := 0 to s2.Count - 1 do
    if s1.IndexOf(s2.Strings[i]) < 0 then
      s3.Add(s2.Strings[i]);
  s1.Sorted := False;
  s1.AddStrings(s3);
  s1.Sorted := True;
  s3.Free;
end;

procedure TTUpdateNewPartsFromBLForm.Button3Click(Sender: TObject);
var
  i: integer;
  b: boolean;
  tmp: TStringList;
  stmp: string;
  num: integer;
  newparts: TStringList;
  allparts: TStringList;
  allparts2: TStringList;
  dbUpieces: TStringList;
  overwrite: boolean;
  fname: string;
  searchinvs: TStringList;
begin
  if not DirectoryExists(basedefault + 'db\parts') then
    ForceDirectories(basedefault + 'db\parts');
  Screen.Cursor := crHourGlass;
  tmp := TStringList.Create;
  allparts := TStringList.Create;
  allparts2 := TStringList.Create;
  searchinvs := TStringList.Create;
  overwrite := CheckBox2.Checked;
  try
    b := CheckBox1.Checked;
    num := atoi(Edit1.Text);
    if num < 1 then
      num := 1;
    for i := 1 to num do
    begin
      newparts := db.QryPartsFromBricklink(
        'https://' + BL_NET + '/catalogList.asp?pg=' + itoa(i) + '&viewInv=Y&sortBy=D&sortAsc=D&itemBrand=1000&catType=P',
        'catalogitem.page?P=');
      MergeSList(allparts, newparts);
      newparts.Free;
    end;
    for i := 0 to allparts.Count - 1 do
    begin
      if not db.PieceInfo(allparts.Strings[i]).hasinventory then
        searchinvs.Add(allparts.Strings[i]);
      fname := basedefault + 'db\parts\' + allparts.Strings[i] + '.htm';
      if not overwrite and fexists(fname) then
        newparts := db.QryNewPartsFromFile(
          fname, 'catalogitem.page?P=')
      else
        newparts := db.QryNewPartsFromBricklink(
          'https://' + BL_NET + '/catalogItemInv.asp?P=' + allparts.Strings[i] + '&viewType=P&bt=0&sortBy=0&sortAsc=A',
          'catalogitem.page?P=',
          fname);
      MergeSList(allparts2, newparts);
      newparts.Free;
    end;
    dbUpieces := TStringList.Create;
    dbUpieces.AddStrings(db.AllPieces);
    dbUpieces.Text := UpperCase(dbUpieces.Text);
    allparts.Sorted := false;
    for i := allparts.Count - 1 downto 0 do
    begin
      if db.AllPieces.IndexOf(allparts.Strings[i]) >= 0 then
        allparts.Delete(i)
      else if db.AllPieces.IndexOf(db.RebrickablePart(allparts.Strings[i])) >= 0 then
        allparts.Delete(i)
      else if dbUpieces.IndexOf(UpperCase(allparts.Strings[i])) >= 0 then
        allparts.Delete(i)
      else if dbUpieces.IndexOf(UpperCase(db.RebrickablePart(allparts.Strings[i]))) >= 0 then
        allparts.Delete(i)
    end;
    dbUpieces.Free;

    MergeSList(allparts, allparts2);

    for i := 0 to allparts.Count - 1 do
    begin
      tmp.Add('refreshpiecefrombricklinknorefresh/' + allparts.Strings[i]);
      if b then
        tmp.Add('spiece/' + allparts.Strings[i]);
      if (tmp.Count > 200) or ((tmp.Count > 30) and (random > 0.6)) then
      begin
        stmp := tmp.Strings[tmp.Count - 1];
        tmp.Delete(tmp.Count - 1);
        Memo1.Lines.AddStrings(tmp);
        Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
        Label7.Update;
        Memo1.Lines.Add(stmp);
        Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
        Label7.Update;
        tmp.Clear;
      end;
    end;
    if tmp.Count > 0 then
    begin
      stmp := tmp.Strings[tmp.Count - 1];
      tmp.Delete(tmp.Count - 1);
      if tmp.Count > 0 then
        Memo1.Lines.AddStrings(tmp);
      Memo1.Lines.Add(stmp);
      Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
      Label7.Update;
    end;

    for i := 0 to searchinvs.Count - 1 do
    begin
      tmp.Add('UpdatePartInventorynorefresh/' + searchinvs.Strings[i]);
      if b then
        tmp.Add('spiece/' + searchinvs.Strings[i]);
      if (tmp.Count > 200) or ((tmp.Count > 30) and (random > 0.6)) then
      begin
        stmp := tmp.Strings[tmp.Count - 1];
        tmp.Delete(tmp.Count - 1);
        Memo1.Lines.AddStrings(tmp);
        Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
        Label7.Update;
        Memo1.Lines.Add(stmp);
        Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
        Label7.Update;
        tmp.Clear;
      end;
    end;
    if tmp.Count > 0 then
    begin
      stmp := tmp.Strings[tmp.Count - 1];
      tmp.Delete(tmp.Count - 1);
      if tmp.Count > 0 then
        Memo1.Lines.AddStrings(tmp);
      Memo1.Lines.Add(stmp);
      Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
      Label7.Update;
    end;


  finally
    tmp.Free;
    allparts.Free;
    allparts2.Free;
    searchinvs.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TTUpdateNewPartsFromBLForm.EditKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
  begin
    Key := #0;
    Exit;
  end;
  if Key in ['0'..'9'] then
    if Length((Sender as TEdit).Text) > 3 then
    begin
      Key := #0;
      Exit;
    end;
end;

procedure TTUpdateNewPartsFromBLForm.Button4Click(Sender: TObject);
var
  i: integer;
  b: boolean;
  tmp: TStringList;
  stmp: string;
  num: integer;
  newparts: TStringList;
  allparts: TStringList;
  dbUpieces: TStringList;
begin
  Screen.Cursor := crHourGlass;
  tmp := TStringList.Create;
  allparts := TStringList.Create;
  try
    b := CheckBox1.Checked;
    num := atoi(Edit2.Text);
    if num < 1 then
      num := 1;
    for i := 1 to num do
    begin
      newparts := db.QryPartsFromBricklink(
        'https://' + BL_NET + '/catalogList.asp?pg=' + itoa(i) + '&itemInvUserID=0&sortBy=D&sortAsc=D&itemBrand=1000&catType=P',
        'catalogitem.page?P=');
      MergeSList(allparts, newparts);
      newparts.Free;
    end;
    dbUpieces := TStringList.Create;
    dbUpieces.AddStrings(db.AllPieces);
    dbUpieces.Text := UpperCase(dbUpieces.Text);
    allparts.Sorted := false;
    for i := allparts.Count - 1 downto 0 do
    begin
      if db.AllPieces.IndexOf(allparts.Strings[i]) >= 0 then
        allparts.Delete(i)
      else if db.AllPieces.IndexOf(db.RebrickablePart(allparts.Strings[i])) >= 0 then
        allparts.Delete(i)
      else if dbUpieces.IndexOf(UpperCase(allparts.Strings[i])) >= 0 then
        allparts.Delete(i)
      else if dbUpieces.IndexOf(UpperCase(db.RebrickablePart(allparts.Strings[i]))) >= 0 then
        allparts.Delete(i)
    end;
    dbUpieces.Free;

    for i := 0 to allparts.Count - 1 do
    begin
      tmp.Add('refreshpiecefrombricklinknorefresh/' + allparts.Strings[i]);
      if b then
        tmp.Add('spiece/' + allparts.Strings[i]);
      if (tmp.Count > 200) or ((tmp.Count > 30) and (random > 0.6)) then
      begin
        stmp := tmp.Strings[tmp.Count - 1];
        tmp.Delete(tmp.Count - 1);
        Memo1.Lines.AddStrings(tmp);
        Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
        Label7.Update;
        Memo1.Lines.Add(stmp);
        Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
        Label7.Update;
        tmp.Clear;
      end;
    end;
    if tmp.Count > 0 then
    begin
      stmp := tmp.Strings[tmp.Count - 1];
      tmp.Delete(tmp.Count - 1);
      if tmp.Count > 0 then
        Memo1.Lines.AddStrings(tmp);
      Memo1.Lines.Add(stmp);
      Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
      Label7.Update;
    end;
    if allparts.Count > 0 then
    begin
      Memo1.Lines.Add('AutoCorrectUnknownPieceYears');
      Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
      Label7.Update;
    end;
  finally
    tmp.Free;
    allparts.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TTUpdateNewPartsFromBLForm.FormCreate(Sender: TObject);
begin
    Memo1.Lines.Clear;
end;

procedure TTUpdateNewPartsFromBLForm.AddNewCatalogRec(const s: string);
var
  s1: string;
begin
  s1 := 'UpdateCatalogFromBricklinknorefresh/' + s;
  if Memo1.Lines.IndexOf(s1) = -1 then
    Memo1.Lines.Add(s1);
  if CheckBox1.Checked then
  begin
    s1 := 'spiece/' + s;
    if Memo1.Lines.IndexOf(s1) = -1 then
      Memo1.Lines.Add(s1);
  end;
  Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
  Label7.Update;
end;

procedure TTUpdateNewPartsFromBLForm.Button5Click(Sender: TObject);
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
      lnk := 'https://' + BL_NET + '/catalogList.asp?pg=' + itoa(i) + '&sortBy=D&sortAsc=D&itemBrand=1000&catType=C&v=1';
      lst := db.QryNewCatalogsFromBricklink(lnk, '<a href="/v2/catalog/catalogitem.page?C=');
      for j := 0 to lst.Count - 1 do
        AddNewCatalogRec(lst.Strings[j]);
      lst.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TTUpdateNewPartsFromBLForm.Button6Click(Sender: TObject);
var
  i: integer;
  b: boolean;
  tmp: TStringList;
  stmp: string;
  num: integer;
  newparts: TStringList;
  allparts: TStringList;
  dbUpieces: TStringList;
begin
  Screen.Cursor := crHourGlass;
  tmp := TStringList.Create;
  allparts := TStringList.Create;
  try
    b := CheckBox1.Checked;
    num := atoi(Edit4.Text);
    if num < 1 then
      num := 1;
    for i := 1 to num do
    begin
      newparts := db.QryPartsFromBricklink(
        'https://' + BL_NET + '/catalogList.asp?pg=' + itoa(i) + '&itemInvUserID=0&sortBy=D&sortAsc=D&itemBrand=1000&catType=G',
        'catalogitem.page?G=');
      MergeSList(allparts, newparts);
      newparts.Free;
    end;
    dbUpieces := TStringList.Create;
    dbUpieces.AddStrings(db.AllPieces);
    dbUpieces.Text := UpperCase(dbUpieces.Text);
    allparts.Sorted := false;
    for i := allparts.Count - 1 downto 0 do
    begin
      if db.AllPieces.IndexOf(allparts.Strings[i]) >= 0 then
        allparts.Delete(i)
      else if db.AllPieces.IndexOf(db.RebrickablePart(allparts.Strings[i])) >= 0 then
        allparts.Delete(i)
      else if dbUpieces.IndexOf(UpperCase(allparts.Strings[i])) >= 0 then
        allparts.Delete(i)
      else if dbUpieces.IndexOf(UpperCase(db.RebrickablePart(allparts.Strings[i]))) >= 0 then
        allparts.Delete(i)
    end;
    dbUpieces.Free;

    for i := 0 to allparts.Count - 1 do
    begin
      tmp.Add('refreshgearfrombricklinknorefresh/' + allparts.Strings[i]);
      if b then
        tmp.Add('spiece/' + allparts.Strings[i]);
      if (tmp.Count > 200) or ((tmp.Count > 30) and (random > 0.6)) then
      begin
        stmp := tmp.Strings[tmp.Count - 1];
        tmp.Delete(tmp.Count - 1);
        Memo1.Lines.AddStrings(tmp);
        Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
        Label7.Update;
        Memo1.Lines.Add(stmp);
        Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
        Label7.Update;
        tmp.Clear;
      end;
    end;
    if tmp.Count > 0 then
    begin
      stmp := tmp.Strings[tmp.Count - 1];
      tmp.Delete(tmp.Count - 1);
      if tmp.Count > 0 then
        Memo1.Lines.AddStrings(tmp);
      Memo1.Lines.Add(stmp);
      Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
      Label7.Update;
    end;
  finally
    tmp.Free;
    allparts.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TTUpdateNewPartsFromBLForm.Button7Click(Sender: TObject);
var
  i: integer;
  b: boolean;
  tmp: TStringList;
  stmp: string;
  num: integer;
  newparts: TStringList;
  allparts: TStringList;
  dbUpieces: TStringList;
begin
  Screen.Cursor := crHourGlass;
  tmp := TStringList.Create;
  allparts := TStringList.Create;
  try
    b := CheckBox1.Checked;
    num := atoi(Edit5.Text);
    if num < 1 then
      num := 1;
    for i := 1 to num do
    begin
      newparts := db.QryPartsFromBricklink(
        'https://' + BL_NET + '/catalogList.asp?pg=' + itoa(i) + '&itemInvUserID=0&sortBy=D&sortAsc=D&itemBrand=1000&catType=B',
        'catalogitem.page?B=');
      MergeSList(allparts, newparts);
      newparts.Free;
    end;
    dbUpieces := TStringList.Create;
    dbUpieces.AddStrings(db.AllPieces);
    dbUpieces.Text := UpperCase(dbUpieces.Text);
    allparts.Sorted := false;
    for i := allparts.Count - 1 downto 0 do
    begin
      if db.AllPieces.IndexOf(allparts.Strings[i]) >= 0 then
        allparts.Delete(i)
      else if db.AllPieces.IndexOf(db.RebrickablePart(allparts.Strings[i])) >= 0 then
        allparts.Delete(i)
      else if dbUpieces.IndexOf(UpperCase(allparts.Strings[i])) >= 0 then
        allparts.Delete(i)
      else if dbUpieces.IndexOf(UpperCase(db.RebrickablePart(allparts.Strings[i]))) >= 0 then
        allparts.Delete(i)
    end;
    dbUpieces.Free;

    for i := 0 to allparts.Count - 1 do
    begin
      tmp.Add('refreshbookfrombricklinknorefresh/' + allparts.Strings[i]);
      if b then
        tmp.Add('spiece/' + allparts.Strings[i]);
      if (tmp.Count > 200) or ((tmp.Count > 30) and (random > 0.6)) then
      begin
        stmp := tmp.Strings[tmp.Count - 1];
        tmp.Delete(tmp.Count - 1);
        Memo1.Lines.AddStrings(tmp);
        Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
        Label7.Update;
        Memo1.Lines.Add(stmp);
        Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
        Label7.Update;
        tmp.Clear;
      end;
    end;
    if tmp.Count > 0 then
    begin
      stmp := tmp.Strings[tmp.Count - 1];
      tmp.Delete(tmp.Count - 1);
      if tmp.Count > 0 then
        Memo1.Lines.AddStrings(tmp);
      Memo1.Lines.Add(stmp);
      Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
      Label7.Update;
    end;
  finally
    tmp.Free;
    allparts.Free;
    Screen.Cursor := crDefault;
  end;
end;

end.
