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
//    Edit mold Form
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit editmoldfrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, CheckLst, ComCtrls;

type
  TEditMoldForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    AliasLabel: TLabel;
    AliasEdit: TEdit;
    Edit2: TEdit;
    AutodetectButton: TButton;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    ComboBox1: TComboBox;
    Label2: TLabel;
    Label4: TLabel;
    WeightEdit: TEdit;
    SpeedButton1: TSpeedButton;
    Edit1: TEdit;
    Label1: TLabel;
    NewNameEdit: TEdit;
    Label5: TLabel;
    ColorAssetsLabel: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    ColorLabel1: TLabel;
    ColorLabel2: TLabel;
    ColorLabel3: TLabel;
    CheckListBox1: TCheckListBox;
    ColorPanel: TPanel;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    SpeedButton2: TSpeedButton;
    procedure AutodetectButtonClick(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure CheckListBox1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure AliasEditChange(Sender: TObject);
    procedure NewNameEditChange(Sender: TObject);
  private
    { Private declarations }
    procedure AliasUpdate;
  public
    { Public declarations }
    procedure UpdateColorPanel;
  end;

function EditMold(const apart: string): boolean;

implementation

{$R *.dfm}

uses
  bi_db, bi_delphi, bi_utils, bi_crawler, bi_globals;

function RGBInvert(const cl: LongWord): LongWord;
var
  r, g, b: byte;
begin
  r := GetRValue(cl);
  g := GetGValue(cl);
  b := GetBValue(cl);
  Result := RGB(b, g, r);
end;

function EditMold(const apart: string): boolean;
var
  f: TEditMoldForm;
  initialalias: string;
  initnewname: string;
  kc: TDNumberList;
  i: integer;
  idxcat: string;
  idx: integer;
  cc: integer;
  catcheck: string;
  part: string;
  pi: TPieceInfo;
  pci: TPieceColorInfo;
  initialinv, afterinv: string;
  stmp: string;
  hassetinv: boolean;
  hadinstructions: boolean;
  hadbox: boolean;
  sinv: TBrickInventory;
  yyyy: integer;

  procedure PanelCaption(const p: TPanel; const c: integer);
  begin
    if c = BOXCOLORINDEX then
      p.Caption := 'B'
    else if c = INSTRUCTIONCOLORINDEX then
      p.Caption := 'I'
    else if c = CATALOGCOLORINDEX then
      p.Caption := 'C'
    else if c = -1 then
      p.Caption := '-'
    else
      p.Caption := '';
  end;

begin
  Result := False;
  if Trim(apart) = '' then
    Exit;

  f := TEditMoldForm.Create(nil);
  try
    part := db.RebrickablePart(apart);
    kc := db.GetMoldKnownColors(part);
    f.Panel1.Visible := False;
    f.Panel2.Visible := False;
    f.Panel3.Visible := False;
    f.Panel4.Visible := False;
    f.Panel5.Visible := False;
    f.Panel6.Visible := False;
    f.Panel7.Visible := False;
    if kc.Count > 0 then
    begin
      f.Panel1.Visible := True;
      f.Panel1.Color := RGBInvert(db.Colors(kc.Numbers[0]).RGB);
      f.Panel1.Hint := db.Colors(kc.Numbers[0]).name;
      PanelCaption(f.Panel1, kc.Numbers[0]);
      if kc.Count > 1 then
      begin
        f.Panel2.Visible := true;
        f.Panel2.Color := RGBInvert(db.Colors(kc.Numbers[1]).RGB);
        f.Panel2.Hint := db.Colors(kc.Numbers[1]).name;
        PanelCaption(f.Panel2, kc.Numbers[1]);
        if kc.Count > 2 then
        begin
          f.Panel3.Visible := true;
          f.Panel3.Color := RGBInvert(db.Colors(kc.Numbers[2]).RGB);
          f.Panel3.Hint := db.Colors(kc.Numbers[2]).name;
          PanelCaption(f.Panel3, kc.Numbers[2]);
          if kc.Count > 3 then
          begin
            f.Panel4.Visible := true;
            f.Panel4.Color := RGBInvert(db.Colors(kc.Numbers[3]).RGB);
            f.Panel4.Hint := db.Colors(kc.Numbers[3]).name;
            PanelCaption(f.Panel4, kc.Numbers[3]);
            if kc.Count > 4 then
            begin
              f.Panel5.Visible := true;
              f.Panel5.Color := RGBInvert(db.Colors(kc.Numbers[4]).RGB);
              f.Panel5.Hint := db.Colors(kc.Numbers[4]).name;
              PanelCaption(f.Panel5, kc.Numbers[4]);
              if kc.Count > 5 then
              begin
                f.Panel6.Visible := true;
                f.Panel6.Color := RGBInvert(db.Colors(kc.Numbers[5]).RGB);
                f.Panel6.Hint := db.Colors(kc.Numbers[5]).name;
                PanelCaption(f.Panel6, kc.Numbers[5]);
                if kc.Count > 6 then
                begin
                  f.Panel7.Visible := true;
                  if kc.Count = 7 then
                  begin
                    f.Panel7.Color := RGBInvert(db.Colors(kc.Numbers[6]).RGB);
                    f.Panel7.Hint := db.Colors(kc.Numbers[6]).name;
                    PanelCaption(f.Panel7, kc.Numbers[6]);
                  end
                  else
                  begin
                    f.Panel7.Font.Color := clBlack;
                    f.Panel7.Caption := '...';
                    f.Panel7.Hint := itoa(kc.Count) + ' total known colors';
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;

    f.CheckListBox1.Items.Clear;
    f.CheckListBox1.Sorted := True;
    f.CheckListBox1.Items.Add('(Not Applicable)');
    f.CheckListBox1.Checked[0] := db.Colors(-1).knownpieces.IndexOf(part) >= 0;

    for i := 0 to MAXINFOCOLOR do
    begin
      if i = LASTNORMALCOLORINDEX then
        f.CheckListBox1.Sorted := False;
      if db.Colors(i).id = i then
        if db.Colors(i).knownpieces <> nil then
          if Trim(db.Colors(i).name) <> '' then
          begin
            idx := f.CheckListBox1.Items.Add(db.Colors(i).name);
            f.CheckListBox1.Checked[idx] := db.Colors(i).knownpieces.IndexOfUCS(part) >= 0;
          end;
    end;
    hadinstructions := db.Colors(INSTRUCTIONCOLORINDEX).knownpieces.IndexOfUCS(part) >= 0;
    hadbox := db.Colors(BOXCOLORINDEX).knownpieces.IndexOfUCS(part) >= 0;

    for i := 0 to f.CheckListBox1.Items.Count - 1 do
      if f.CheckListBox1.Checked[i] then
        f.CheckListBox1.ItemEnabled[i] := False;
    if f.CheckListBox1.Items.Count > 0 then
    begin
      f.CheckListBox1.ItemIndex := 0;
      f.UpdateColorPanel;
    end;

    f.ComboBox1.Items.Clear;
    idxcat := '';
    for i := 0 to MAXCATEGORIES - 1 do
    begin
      if db.categories[i].knownpieces <> nil then
      begin
        f.ComboBox1.Items.Add(db.categories[i].name);
        if idxcat = '' then
          if db.categories[i].knownpieces.IndexOf(part) >= 0 then
            idxcat := db.categories[i].name;
      end;
    end;
    pi := db.PieceInfo(part);
    if idxcat = '' then
    begin
      if pi <> nil then
        if pi.category >= 0 then
          if pi.category < MAXCATEGORIES then
            idxcat := db.categories[pi.category].name;
    end;

    sinv := db.GetSetInventory(part);
    if sinv <> nil then
    begin
      hassetinv := True;
      f.Memo1.Lines.Text := sinv.AsText;
    end
    else
    begin
      hassetinv := False;
      f.Memo1.Lines.Text := pi.InventoryAsText(-2);
    end;

    if f.Memo1.Lines.Count <= 1 then
      initialinv := ''
    else
      initialinv := Trim(f.Memo1.Lines.Text);
    if idxcat <> '' then
    begin
      idx := f.ComboBox1.Items.IndexOf(idxcat);
      if idx >= 0 then
        f.ComboBox1.ItemIndex := idx;
    end;

    f.Edit1.Text := db.PieceDesc(part);
    f.AliasLabel.Visible := true;
    f.AliasEdit.Visible := true;
    f.NewNameEdit.Visible := true;
    if StrUpper(part) <> StrUpper(db.BrickLinkPart(part)) then
      f.AliasEdit.Text := db.BrickLinkPart(part)
    else
      f.AliasEdit.Text := '';
    f.NewNameEdit.Text := db.GetNewPieceName(part);
    if f.NewNameEdit.Text = part then
      f.NewNameEdit.Text := '';
    f.AutodetectButton.Visible := true;

    f.WeightEdit.Text := Format('%2.4f', [db.GetPartWeight(part)]);
    f.Edit2.Text := part;
    initialalias := f.AliasEdit.Text;
    initnewname := f.NewNameEdit.Text;

    for i := 0 to kc.Count - 1 do
      if PieceToImage(f.Image1, part, kc.Numbers[i]) then
        Break;
        
    kc.Free;

    f.AliasUpdate;
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      Screen.Cursor := crHourglass;
      try
        Result := True;
        db.SetMoldName(part, f.Edit1.Text);
        if strupper(strtrim(f.AliasEdit.Text)) <> strupper(strtrim(initialalias)) then
          db.AddPieceAlias(f.AliasEdit.Text, part);
        if strupper(strtrim(f.NewNameEdit.Text)) <> strupper(strtrim(initnewname)) then
          db.SetNewPieceName(part, strtrim(f.NewNameEdit.Text));
        catcheck := UpperCase(Trim(f.ComboBox1.Text));
        for i := 0 to MAXCATEGORIES - 1 do
        begin
          if db.categories[i].knownpieces <> nil then
          begin
            if UpperCase(Trim(db.categories[i].name)) = catcheck then
            begin
              db.SetPartCategory(part, i);
              break;
            end;
          end;
        end;
        for i := 0 to f.CheckListBox1.Items.Count - 1 do
          if f.CheckListBox1.Checked[i] then
            if db.GetColorIdFromName(f.CheckListBox1.Items.Strings[i], cc) then
              db.AddMoldColor(part, cc);
        db.UpdatePartWeight(part, atof(f.WeightEdit.Text));
        if f.Memo1.Lines.Count <= 1 then
          afterinv := ''
        else
        begin
          stmp := f.Memo1.Lines.Strings[0];
          if not Pos1('Part,', stmp) then
            f.Memo1.Lines.Insert(0, 'Part,Color,Quantity');
          afterinv := Trim(f.Memo1.Lines.Text);
        end;
        if afterinv <> initialinv then
        begin
          if hassetinv then
            db.UpdateSet(part, afterinv)
          else
            db.SetPartInventory(part, afterinv);
        end;
        // Update new instructions and box year
        if not hadinstructions then
          if db.Colors(INSTRUCTIONCOLORINDEX).knownpieces.IndexOfUCS(part) >= 0 then
          begin
            yyyy := db.SetYear(part);
            if (yyyy >= MIN_ACCEPRABLE_YEAR) and (yyyy <= MAX_ACCEPTABLE_YEAR) then
            begin
              pci := db.PieceColorInfo(part, INSTRUCTIONCOLORINDEX);
              if pci <> nil then
                db.SetItemYear(pci, yyyy);
            end;
          end;
        if not hadbox then
          if db.Colors(BOXCOLORINDEX).knownpieces.IndexOfUCS(part) >= 0 then
          begin
            yyyy := db.SetYear(part);
            if (yyyy >= MIN_ACCEPRABLE_YEAR) and (yyyy <= MAX_ACCEPTABLE_YEAR) then
            begin
              pci := db.PieceColorInfo(part, BOXCOLORINDEX);
              if pci <> nil then
                db.SetItemYear(pci, yyyy);
            end;
          end;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  finally
    f.Free;
  end;
end;

procedure TEditMoldForm.AutodetectButtonClick(Sender: TObject);
var
  s: string;
begin
  Screen.Cursor := crHourglass;
  try
    s := NET_GetBricklinkAliasRB(Edit2.Text);
    if s = '' then
    begin
      if Trim(AliasEdit.Text) <> '' then
        s := NET_GetBricklinkAliasBL(AliasEdit.Text);
      if s = '' then
        s := NET_GetBricklinkAliasBL(Edit2.Text);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
  if s = '' then
    if NewNameEdit.Text <> '' then
      Exit;
  if s = 'Not Listed' then
    Exit;

  NewNameEdit.Text := s;
end;

procedure TEditMoldForm.Image1DblClick(Sender: TObject);
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

procedure TEditMoldForm.UpdateColorPanel;
var
  idx: integer;
  cc: integer;
begin
  idx := CheckListBox1.ItemIndex;
  if idx < 0 then
  begin
    ColorPanel.Visible := False;
    Exit;
  end;

  if not db.GetColorIdFromName(CheckListBox1.Items.Strings[idx], cc) then
  begin
    ColorPanel.Visible := False;
    Exit;
  end;

  ColorPanel.Color := RGBInvert(db.Colors(cc).RGB);
  ColorLabel1.Caption := itoa(cc);
  ColorLabel2.Caption := '(BL=' + itoa(db.colors(cc).BrickLinkColor) + ')';
  ColorLabel3.Caption := '(RB=' + itoa(db.colors(cc).RebrickableColor) + ')'
end;

procedure TEditMoldForm.CheckListBox1Click(Sender: TObject);
begin
  UpdateColorPanel;
end;

procedure TEditMoldForm.SpeedButton1Click(Sender: TObject);
var
  tmpweight: double;
  tmpcat: integer;
  parttyp: char;
  stmp: string;
  ok: boolean;
begin
  tmpweight := -1.0;
  tmpcat := -2;

  NewNameEdit.Text := Trim(NewNameEdit.Text);
  AliasEdit.Text := Trim(AliasEdit.Text);
  
  Screen.Cursor := crHourglass;
  try
    stmp := db.BrickLinkPart(Edit2.Text);
    ok := NET_GetBricklinkCategory(stmp, tmpcat, tmpweight, parttyp);
    if not ok then
    begin
      if (stmp <> NewNameEdit.Text) and (NewNameEdit.Text <> '') then
        ok := NET_GetBricklinkCategory(NewNameEdit.Text, tmpcat, tmpweight, parttyp);
      if not ok then
        if (stmp <> AliasEdit.Text) and (AliasEdit.Text <> '') and (AliasEdit.Text <> NewNameEdit.Text) then
          NET_GetBricklinkCategory(AliasEdit.Text, tmpcat, tmpweight, parttyp);
    end;
  finally
    Screen.Cursor := crDefault;
  end;

  if tmpweight > 0 then
  begin
    WeightEdit.Text := Format('%2.4f', [tmpweight]);
    Exit;
  end;
  Beep;
end;

procedure TEditMoldForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
end;

procedure TEditMoldForm.SpeedButton2Click(Sender: TObject);
begin
  if Trim(AliasEdit.Text) <> '' then
    if Trim(NewNameEdit.Text) = '' then
    begin
      NewNameEdit.Text := Trim(AliasEdit.Text);
      AliasEdit.Text := '';
    end;
end;

procedure TEditMoldForm.AliasUpdate;
begin
  SpeedButton2.Enabled := (Trim(AliasEdit.Text) <> '') and (Trim(NewNameEdit.Text) = '');
end;

procedure TEditMoldForm.AliasEditChange(Sender: TObject);
begin
  AliasUpdate;
end;

procedure TEditMoldForm.NewNameEditChange(Sender: TObject);
begin
  AliasUpdate;
end;

end.
