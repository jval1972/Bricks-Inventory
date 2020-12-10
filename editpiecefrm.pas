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
    NumPiecesEdit: TEdit;
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
    NameEdit: TEdit;
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
    Label8: TLabel;
    YearEdit: TEdit;
    UpdateYearSpeedButton: TSpeedButton;
    Label9: TLabel;
    PartCodeEdit: TEdit;
    EditBLLinkSpeedButton: TSpeedButton;
    WeightLabel: TLabel;
    WeightEdit: TEdit;
    UpdateWeightSpeedButton: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AutodetectButtonClick(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure NumPiecesEditKeyPress(Sender: TObject; var Key: Char);
    procedure SelectPartTypeSpeedButtonClick(Sender: TObject);
    procedure YearEditKeyPress(Sender: TObject; var Key: Char);
    procedure UpdateYearSpeedButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditBLLinkSpeedButtonClick(Sender: TObject);
    procedure WeightEditKeyPress(Sender: TObject; var Key: Char);
    procedure UpdateWeightSpeedButtonClick(Sender: TObject);
  private
    { Private declarations }
  protected
    parttype: char;
    partcolor: integer;
    partname: string;
  public
    { Public declarations }
  end;

function EditPiece(const apart: string; const color: integer): boolean;

implementation

{$R *.dfm}

uses
  urlmon,
  bi_db, bi_delphi, bi_utils, bi_crawler, bi_globals, frm_selectparttype,
  frm_editbllink;

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
  initialstorage: string;
  initialyear: integer;
  initialcode: string;
  initialweight: string;
begin
  Result := False;
  if Trim(apart) = '' then
    Exit;

  f := TEditPieceForm.Create(nil);
  try
    part := db.RebrickablePart(apart);
    f.YearEdit.ReadOnly := True;
    f.UpdateYearSpeedButton.Visible := False;
    initialstorage := '';
    initialcode := '';
    f.partcolor := color;
    f.partname := part;
    f.WeightLabel.Visible := (color = 9997) or (color = 9998);
    f.WeightEdit.Visible := (color = 9997) or (color = 9998);
    f.UpdateWeightSpeedButton.Visible := (color = 9997) or (color = 9998);
    if (color = 9997) or (color = 9998) then
      f.WeightEdit.Text := Format('%2.4f', [db.GetAssetWeight(part, color)])
    else
      f.WeightEdit.Text := ftoadot(0.0);
    initialweight := f.WeightEdit.Text;
    if (color = -1) and (Pos('-', part) > 0) then
    begin
      f.Label1.Caption := db.SetDesc(part);
      if f.Label1.Caption = '' then
        f.Label1.Caption := db.PieceDesc(part);
      f.Label3.Visible := True;
      f.Label4.Visible := False;
      f.Memo1.Visible := True;
      f.Panel1.Visible := False;
      f.AliasLabel.Visible := False;
      f.AliasEdit.Visible := False;
      f.AliasEdit.Text := '';
      f.NewNameEdit.Text := db.GetNewPieceName(part);
      if f.NewNameEdit.Text = part then
        f.NewNameEdit.Text := '';
      f.AutodetectButton.Visible := True;
    end
    else
    begin
      f.Label1.Caption := db.PieceDesc(part);
      f.Label3.Visible := True;
      f.Label4.Visible := True;
      f.Memo1.Visible := True;
      if color = BOXCOLORINDEX then
        f.Panel1.Caption := 'O'
      else if color = INSTRUCTIONCOLORINDEX then
        f.Panel1.Caption := 'I'
      else if color = CATALOGCOLORINDEX then
        f.Panel1.Caption := 'C'
      else if color = -1 then
        f.Panel1.Caption := '-'
      else
        f.Panel1.Caption := '';
      f.Panel1.Visible := True;
      f.Label4.Caption := db.colors(color).name;
      f.AliasLabel.Visible := True;
      f.AliasEdit.Visible := True;
      if StrUpper(part) <> StrUpper(db.BrickLinkPart(part)) then
        f.AliasEdit.Text := db.BrickLinkPart(part)
      else
        f.AliasEdit.Text := '';
      f.NewNameEdit.Text := db.GetNewPieceName(part);
      if f.NewNameEdit.Text = part then
        f.NewNameEdit.Text := '';
      f.AutodetectButton.Visible := True;
    end;
    f.LinkEdit.Text := db.CrawlerLink(part, color);
    f.NameEdit.Text := part;
    initialalias := f.AliasEdit.Text;
    initnewname := f.NewNameEdit.Text;
    pci := db.PieceColorInfo(part, color);
    PieceToImage(f.Image1, part, color);
    if pci <> nil then
    begin
      initialcode := pci.code;
      f.PartCodeEdit.Text := initialcode;

      f.parttype := pci.sparttype;

      f.YearEdit.ReadOnly := (pci.year <> 0) and not pci.canedityear;
      initialyear := pci.year;
      f.YearEdit.Text := itoa(initialyear);
      if f.YearEdit.ReadOnly then
        f.YearEdit.Color := clBtnFace;
      if not f.YearEdit.ReadOnly then
        f.UpdateYearSpeedButton.Visible := db.HasSetColorsOnly(pci.piece) or (pci.sparttype = 'P') or (pci.sparttype = 'M') or (pci.sparttype = ' ');

      f.Panel1.Color := RGBInvert(db.colors(color).RGB);
      f.PartTypePanel.Caption := pci.sparttype;
      f.PartTypeLabel.Caption := PartTypeToPartTypeName(pci.sparttype);
      if Trim(f.PartTypeLabel.Caption) = '' then
        f.PartTypeLabel.Caption := '(Default)';
      initialstorage := pci.storage.Text;
      f.Memo1.Lines.Text := initialstorage;
      num := inventory.LoosePartCount(part, color);
      f.NumPiecesEdit.Text := itoa(num);
      f.ShowModal;
      if f.ModalResult = mrOK then
      begin
        Screen.Cursor := crHourglass;
        try
          Result := True;
          if initialyear <> atoi(strtrim(f.YearEdit.Text)) then
          begin
            if db.HasSetColorsOnly(pci.piece) then
              db.UpdateYearForAllColors(pci.piece, atoi(strtrim(f.YearEdit.Text)))
            else
              db.SetItemYear(pci, atoi(strtrim(f.YearEdit.Text)));
          end;
          if strupper(strtrim(f.AliasEdit.Text)) <> strupper(strtrim(initialalias)) then
            db.AddPieceAlias(f.AliasEdit.Text, part);
          if strupper(strtrim(f.NewNameEdit.Text)) <> strupper(strtrim(initnewname)) then
            db.SetNewPieceName(part, strtrim(f.NewNameEdit.Text));
          newnum := atoi(f.NumPiecesEdit.Text);
          inventory.AddLoosePart(part, color, newnum - num);
          pt := f.PartTypePanel.Caption;
          if Length(pt) = 1 then
            if pci.sparttype <> pt then
            begin
              db.SetPartType(part, color, pt[1]);
              db.FlashPartTypes;
            end;
          db.AddCrawlerLink(part, color, f.LinkEdit.Text);

          s := TStringList.Create;
          try
            s.Text := f.Memo1.Lines.Text;
            if s.Text <> initialstorage then
              db.SetPieceStorage(part, color, s);
          finally
            s.Free;
          end;

          f.PartCodeEdit.Text := Trim(f.PartCodeEdit.Text);
          if f.PartCodeEdit.Text <> initialcode then
            db.SetPieceCode(pci, f.PartCodeEdit.Text);

          if (color = 9997) or (color = 9998) then
            if f.WeightEdit.Text <> initialweight then
              db.SetAssetWeight(part, color, atof(f.WeightEdit.Text));

          db.CrawlerPriorityPart(part, color);
        finally
          Screen.Cursor := crDefault;
        end;
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
    NumPiecesEdit.Text := itoa(atoi(NumPiecesEdit.Text) + atoi(value));
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
    num := atoi(NumPiecesEdit.Text) - atoi(value);
    if num < 0 then
      num := 0;
    NumPiecesEdit.Text := itoa(num);
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
    Exit;
  s := Memo1.Lines.Strings[Memo1.Lines.Count - 1];
  p := Pos(':', s);
  if p <= 0 then
    Exit;
  splitstring(s, s1, s2, ':');
  if s1 <> '' then
    last_pos := s1;
end;

procedure TEditPieceForm.AutodetectButtonClick(Sender: TObject);
var
  s: string;
  spiece: string;
begin
  spiece := Trim(NameEdit.Text);
  Screen.Cursor := crHourglass;
  s := NET_GetBricklinkAlias(NameEdit.Text);
  Screen.Cursor := crDefault;
  if s = '' then
    if NewNameEdit.Text <> '' then
      Exit;
  if s = 'Not Listed' then
    Exit;

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

procedure TEditPieceForm.NumPiecesEditKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
  begin
    Key := #0;
    Exit;
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
    if not YearEdit.ReadOnly then
      if not UpdateYearSpeedButton.Visible then
        UpdateYearSpeedButton.Visible := pt in ['P', 'S', 'M', ' '];
  end;
end;

procedure TEditPieceForm.YearEditKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
  begin
    Key := #0;
    Exit;
  end;
end;

procedure TEditPieceForm.UpdateYearSpeedButtonClick(Sender: TObject);
var
  fname: string;
  urlstr: string;
  yearnum: integer;
  ret: integer;
  didnet: boolean;
begin
  if PartTypePanel.Caption = 'B' then
  begin
    Screen.Cursor := crHourglass;
    try
      fname := basedefault + 'db\books\' + Trim(NameEdit.Text) + '.htm';
      urlstr := sbricklink + 'v2/catalog/catalogitem.page?B=' + db.GetBLNetPieceName(Trim(NameEdit.Text));
      ret := UrlDownloadToFile(nil, PChar(urlstr), PChar(fname), 0, nil);
    finally
      Screen.Cursor := crDefault;
    end;

    if ret <> 0 then
      if not fexists(fname) then
      begin
        fname := basedefault + 'db\molds\' + Trim(NameEdit.Text) + '.htm';
        if not fexists(fname) then
        begin
          fname := basedefault + 'db\minifigs\' + Trim(NameEdit.Text) + '.htm';
          if not fexists(fname) then
          begin
            fname := basedefault + 'db\gears\' + Trim(NameEdit.Text) + '.htm';
            if not fexists(fname) then
              Exit;
          end;
        end;
      end;

    yearnum := db.GetSetYearFromDiskCache(fname);
    if (yearnum >= 1932) and (yearnum <= 2050) then
      YearEdit.Text := itoa(yearnum);
  end
  else if db.HasSetColorsOnly(NameEdit.Text) then
  begin
    Screen.Cursor := crHourglass;
    try
      fname := basedefault + 'db\setmolds\' + Trim(NameEdit.Text) + '.htm';
      urlstr := sbricklink + 'v2/catalog/catalogitem.page?S=' + db.GetBLNetPieceName(Trim(NameEdit.Text));
      ret := UrlDownloadToFile(nil, PChar(urlstr), PChar(fname), 0, nil);
    finally
      Screen.Cursor := crDefault;
    end;

    if ret <> 0 then
      if not fexists(fname) then
      begin
        fname := basedefault + 'db\molds\' + Trim(NameEdit.Text) + '.htm';
        if not fexists(fname) then
        begin
          fname := basedefault + 'db\minifigs\' + Trim(NameEdit.Text) + '.htm';
          if not fexists(fname) then
          begin
            fname := basedefault + 'db\gears\' + Trim(NameEdit.Text) + '.htm';
            if not fexists(fname) then
              Exit;
          end;
        end;
      end;

    yearnum := db.GetSetYearFromDiskCache(fname);
    if (yearnum >= 1932) and (yearnum <= 2050) then
      YearEdit.Text := itoa(yearnum);
  end
  else if PartTypePanel.Caption = 'M' then
  begin
    didnet := False;
    Screen.Cursor := crHourglass;
    try
      fname := basedefault + 'db\minifigs\' + Trim(NameEdit.Text) + '.htm';
      if not fexists(fname) then
        fname := basedefault + 'db\molds\' + Trim(NameEdit.Text) + '.htm';
      if not fexists(fname) then
      begin
        urlstr := sbricklink + 'v2/catalog/catalogitem.page?M=' + db.GetBLNetPieceName(Trim(NameEdit.Text));
        UrlDownloadToFile(nil, PChar(urlstr), PChar(fname), 0, nil);
        didnet := True;
      end;
    finally
      Screen.Cursor := crDefault;
    end;

    if not fexists(fname) then
      Exit;

    yearnum := db.GetSetYearFromDiskCache(fname);
    if (yearnum >= 1932) and (yearnum <= 2050) then
    begin
      YearEdit.Text := itoa(yearnum);
      Exit;
    end;

    if didnet then
      Exit;

    Screen.Cursor := crHourglass;
    try
      fname := basedefault + 'db\molds\' + Trim(NameEdit.Text) + '.htm';
      urlstr := sbricklink + 'v2/catalog/catalogitem.page?M=' + db.GetBLNetPieceName(Trim(NameEdit.Text));
      UrlDownloadToFile(nil, PChar(urlstr), PChar(fname), 0, nil);
    finally
      Screen.Cursor := crDefault;
    end;

    if not fexists(fname) then
      Exit;

    yearnum := db.GetSetYearFromDiskCache(fname);
    if (yearnum >= 1932) and (yearnum <= 2050) then
    begin
      YearEdit.Text := itoa(yearnum);
      Exit;
    end;

  end
  else
  begin
    Screen.Cursor := crHourglass;
    try
      if db.GetPartYearFromNet(Trim(NameEdit.Text), partcolor, yearnum) then
        if (yearnum >= 1932) and (yearnum <= 2050) then
          YearEdit.Text := itoa(yearnum);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TEditPieceForm.FormCreate(Sender: TObject);
begin
  parttype := 'P';
  partcolor := 0;
  partname := '';
  YearEdit.Text := '0';
end;

procedure TEditPieceForm.EditBLLinkSpeedButtonClick(Sender: TObject);
var
  link: string;
begin
  link := LinkEdit.Text;
  if EditBricklinkLink(PartTypePanel.Caption, NameEdit.Text, partcolor, link) then
    LinkEdit.Text := link;
end;

procedure TEditPieceForm.WeightEditKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (Key in ['.', #8, '0'..'9']) then
  begin
    Key := #0;
    Exit;
  end;
end;

procedure TEditPieceForm.UpdateWeightSpeedButtonClick(Sender: TObject);
begin
  if (partcolor = 9997) or (partcolor = 9998) then
  begin
    Screen.Cursor := crHourglass;
    try
      WeightEdit.Text := Format('%2.4f', [db.GetAssetWeightFromNET(partname, partcolor)]);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

end.
