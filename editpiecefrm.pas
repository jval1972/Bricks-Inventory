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
//    Edit piece Form
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit editpiecefrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons;

type
  TEditPieceForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Label3: TLabel;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Label4: TLabel;
    Panel1: TPanel;
    AliasLabel: TLabel;
    AliasEdit: TEdit;
    Edit2: TEdit;
    AutodetectButton: TButton;
    LinkEdit: TEdit;
    Label5: TLabel;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    Label6: TLabel;
    Label7: TLabel;
    NewNameEdit: TEdit;
    PartTypePanel: TPanel;
    PartTypeLabel: TLabel;
    SelectPartTypeSpeedButton: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AutodetectButtonClick(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure SelectPartTypeSpeedButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function EditPiece(const apart: string; const color: integer): boolean;

implementation

{$R *.dfm}

uses
  bi_db, bi_delphi, bi_utils, bi_crawler, bi_globals, frm_selectparttype;

function EditPiece(const apart: string; const color: integer): boolean;
var
  f: TEditPieceForm;
  pci: TPieceColorInfo;
  num: integer;
  newnum: integer;
  s: TStringList;
  initialalias: string;
  initnewname: string;
  part: string;
  pt: string;
begin
  result := false;
  f := TEditPieceForm.Create(nil);
  try
    part := db.RebrickablePart(apart);
    if (color = -1) and (Pos('-', part) > 0) then
    begin
      f.Label1.Caption := db.SetDesc(part);
      f.Label3.Visible := false;
      f.Label4.Visible := false;
      f.Memo1.Visible := false;
      f.Panel1.Visible := false;
      f.AliasLabel.Visible := false;
      f.AliasEdit.Visible := false;
      f.AliasEdit.Text := '';
      f.NewNameEdit.Text := db.GetNewPieceName(part);
      if f.NewNameEdit.Text = part then
        f.NewNameEdit.Text := '';
      f.AutodetectButton.Visible := True;
    end
    else
    begin
      f.Label1.Caption := db.PieceDesc(part);
      f.Label3.Visible := true;
      f.Label4.Visible := true;
      f.Memo1.Visible := true;
      if color = BOXCOLORINDEX then
        f.Panel1.Caption := 'B'
      else if color = INSTRUCTIONCOLORINDEX then
        f.Panel1.Caption := 'I'
      else if color = CATALOGCOLORINDEX then
        f.Panel1.Caption := 'C'
      else if color = -1 then
        f.Panel1.Caption := '-'
      else
        f.Panel1.Caption := '';
      f.Panel1.Visible := true;
      f.Label4.Caption := db.colors(color).name;
      f.AliasLabel.Visible := true;
      f.AliasEdit.Visible := true;
      if StrUpper(part) <> StrUpper(db.BrickLinkPart(part)) then
        f.AliasEdit.Text := db.BrickLinkPart(part)
      else
        f.AliasEdit.Text := '';
      f.NewNameEdit.Text := db.GetNewPieceName(part);
      if f.NewNameEdit.Text = part then
        f.NewNameEdit.Text := '';
      f.AutodetectButton.Visible := true;
    end;
    f.LinkEdit.Text := db.CrawlerLink(part, color);
    f.Edit2.Text := part;
    initialalias := f.AliasEdit.Text;
    initnewname := f.NewNameEdit.Text;
    pci := db.PieceColorInfo(part, color);
    PieceToImage(f.Image1, part, color);
    if pci <> nil then
    begin
      f.Panel1.Color := RGBInvert(db.colors(color).RGB);
      f.PartTypePanel.Caption := pci.sparttype;
      f.PartTypeLabel.Caption := PartTypeToPartTypeName(pci.sparttype);
      if Trim(f.PartTypeLabel.Caption) = '' then
        f.PartTypeLabel.Caption := '(Default)';
      f.Memo1.Lines.Text := pci.storage.Text;
      num := inventory.LoosePartCount(part, color);
      f.Edit1.Text := itoa(num);
      f.ShowModal;
      if f.ModalResult = mrOK then
      begin
        result := true;
        if strupper(strtrim(f.AliasEdit.Text)) <> strupper(strtrim(initialalias)) then
          db.AddPieceAlias(f.AliasEdit.Text, part);
        if strupper(strtrim(f.NewNameEdit.Text)) <> strupper(strtrim(initnewname)) then
          db.SetNewPieceName(part, strtrim(f.NewNameEdit.Text));
        newnum := atoi(f.Edit1.Text);
        inventory.AddLoosePart(part, color, newnum - num);
        pt := f.PartTypePanel.Caption;
        if Length(pt) = 1 then
          if pci.sparttype <> pt then
          begin
            db.SetPartType(part, color, pt[1]);
            db.FlashPartTypes;
          end;
        s := TStringList.Create;
        s.Text := f.Memo1.Lines.Text;
        db.AddCrawlerLink(part, color, f.LinkEdit.Text);
        db.SetPieceStorage(part, color, s);
        db.CrawlerPriorityPart(part, color);
        s.Free;
      end;
    end;
  finally
    f.Free;
  end;
end;

var
  last_pos: string = 'BOX01';

procedure TEditPieceForm.SpeedButton1Click(Sender: TObject);
var
  value: string;
begin
  value := '1';
  if InputQuery2(Caption, 'Add pieces', 'Storage bin', value, last_pos) then
  begin
    Edit1.Text := itoa(atoi(Edit1.Text) + atoi(value));
    if last_pos <> '' then
      Memo1.Lines.Add(last_pos + ':' + value);
  end;
end;

procedure TEditPieceForm.SpeedButton2Click(Sender: TObject);
var
  value: string;
  num: integer;
begin
  value := '1';
  if InputQuery2(Caption, 'Remove pieces', 'Storage bin', value, last_pos) then
  begin
    num := atoi(Edit1.Text) - atoi(value);
    if num < 0 then
      num := 0;
    Edit1.Text := itoa(num);
    if last_pos <> '' then
      Memo1.Lines.Add(last_pos + ':-' + value);
  end;
end;

procedure TEditPieceForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  s: string;
  i: integer;
  p: integer;
  s1, s2: string;
begin
  for i := Memo1.Lines.Count - 1 downto 0 do
  begin
    Memo1.Lines.Strings[i] := trim(Memo1.Lines.Strings[i]);
    if Memo1.Lines.Strings[i] = '' then
      Memo1.Lines.Delete(i);
  end;
  if Memo1.Lines.Count = 0 then
    exit;
  s := Memo1.Lines.Strings[Memo1.Lines.Count - 1];
  p := Pos(':', s);
  if p <= 0 then
    exit;
  splitstring(s, s1, s2, ':');
  if s1 <> '' then
    last_pos := s1;
end;

procedure TEditPieceForm.AutodetectButtonClick(Sender: TObject);
var
  s: string;
  spiece: string;
begin
  spiece := Trim(Edit2.Text);
  Screen.Cursor := crHourglass;
  s := NET_GetBricklinkAlias(Edit2.Text);
  Screen.Cursor := crDefault;
  if s = '' then
    if NewNameEdit.Text <> '' then
      exit;
  NewNameEdit.Text := s;
end;

procedure TEditPieceForm.Image1DblClick(Sender: TObject);
begin
  try
    if Image1.Align = alClient then
    begin
      Image1.Align := alNone;
      Image1.AutoSize := True;
      Image1.Stretch := False;
    end
    else
    begin
      Image1.Align := alClient;
      Image1.AutoSize := False;
      Image1.Stretch := True;
    end;
  except
  end;
end;

procedure TEditPieceForm.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
  begin
    Key := #0;
    exit;
  end;
end;

procedure TEditPieceForm.SelectPartTypeSpeedButtonClick(Sender: TObject);
var
  pt: char;
begin
  pt := PartTypeNameToPartType(PartTypeLabel.Caption);
  if SelectPartType(pt) then
  begin
    PartTypeLabel.Caption := PartTypeToPartTypeName(pt);
    if PartTypeLabel.Caption = '' then
      PartTypeLabel.Caption := '(Default)';
    PartTypePanel.Caption := pt;
  end;
end;

end.
