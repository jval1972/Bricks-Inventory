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
  Dialogs, StdCtrls, ExtCtrls, Buttons, ComCtrls, Consts, Menus, Clipbrd,
  pngimage;

type
  TEditPieceForm = class(TForm)
    Label1: TLabel;
    NumPiecesLabel: TLabel;
    NumPiecesEdit: TEdit;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
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
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label3: TLabel;
    ReadyListLabel: TLabel;
    RemoveReadyListSpeedButton: TSpeedButton;
    AddReadyListSpeedButton: TSpeedButton;
    Memo1: TMemo;
    NumReadyEdit: TEdit;
    TagsTabSheet: TTabSheet;
    TagsListBox: TListBox;
    AddTagButton: TButton;
    RemoveTagButton: TButton;
    LugbulkTabSheet: TTabSheet;
    PreferLugbulkButton: TButton;
    LugbulksListBox: TListBox;
    OrdersTabSheet: TTabSheet;
    PreferOrderButton: TButton;
    OrdersListBox: TListBox;
    Label2: TLabel;
    PrefLocationEdit: TEdit;
    ClearPrefSpeedButton: TSpeedButton;
    AliasNameChangeButton1: TSpeedButton;
    PopupMenu1: TPopupMenu;
    Paste1: TMenuItem;
    Copy1: TMenuItem;
    ScrollBox2: TScrollBox;
    LBImage: TImage;
    ScrollBox3: TScrollBox;
    OrImage: TImage;
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
    procedure NumReadyEditKeyPress(Sender: TObject; var Key: Char);
    procedure AddReadyListSpeedButtonClick(Sender: TObject);
    procedure RemoveReadyListSpeedButtonClick(Sender: TObject);
    procedure AddTagButtonClick(Sender: TObject);
    procedure RemoveTagButtonClick(Sender: TObject);
    procedure PreferLugbulkButtonClick(Sender: TObject);
    procedure PreferOrderButtonClick(Sender: TObject);
    procedure LugbulksListBoxClick(Sender: TObject);
    procedure OrdersListBoxClick(Sender: TObject);
    procedure ClearPrefSpeedButtonClick(Sender: TObject);
    procedure AliasNameChangeButton1Click(Sender: TObject);
    procedure AliasEditChange(Sender: TObject);
    procedure NewNameEditChange(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    { Private declarations }
  protected
    parttype: char;
    partcolor: integer;
    partname: string;
    imgchanged: boolean;
    procedure AliasUpdate;
    procedure UpdateImageThumbs(const flags: integer);
  public
    { Public declarations }
  end;

function EditPiece(const apart: string; const color: integer): boolean;

implementation

{$R *.dfm}

uses
  urlmon,
  bi_db, bi_delphi, bi_utils, bi_crawler, bi_globals, frm_selectparttype,
  frm_editbllink, bi_readylist, bi_orders, bl_orderxml, bi_keepfile;

function sortorderlist(List: TStringList; Index1, Index2: Integer): Integer;
var
  o1, o2: string;
  s1, s2: string;
begin
  splitstring(List.Strings[Index1], o1, s1, ':');
  splitstring(List.Strings[Index2], o2, s2, ':');
  Result := atoi(o1) - atoi(o2);
end;

function EditPiece(const apart: string; const color: integer): boolean;
var
  f: TEditPieceForm;
  pci: TPieceColorInfo;
  num, num2: integer;
  newnum: integer;
  s: TStringList;
  initialalias: string;
  initnewname: string;
  part: string;
  pt: string;
  linv: TBrickInventory;
  lnum: integer;
  initialstorage: string;
  initialyear, newyear: integer;
  initialcode: string;
  initialweight: string;
  oldpreflocation: string;
  oldreadylistqty: integer;
  newreadylistqty: integer;
  oldtags: TStringList;
  newtags: TStringList;
  olst: TStringList;
  i: integer;
  tagchange: boolean;
  islikeset: Boolean;
  editingset: boolean;
  oinf: TStringList;
  oitem: TOrderItemInfo;
  P: TPNGObject;
  sstrm: TStream;
  savename: string;
begin
  Result := False;
  if Trim(apart) = '' then
    Exit;

  P := nil;

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
    if (color = -1) and (Pos('-', part) > 0) and (db.GetSetInventory(part) <> nil) then
    begin
      islikeset := True;
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
      f.ReadyListLabel.Visible := False;
      f.NumReadyEdit.Visible := False;
      f.AddReadyListSpeedButton.Visible := False;
      f.RemoveReadyListSpeedButton.Visible := False;
    end
    else
    begin
      islikeset := False;
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
      f.ReadyListLabel.Visible := True;
      f.NumReadyEdit.Visible := True;
      f.AddReadyListSpeedButton.Visible := True;
      f.RemoveReadyListSpeedButton.Visible := True;
    end;
    oldreadylistqty := BI_QueryQtyFromReadyList(part, color, S_READYLIST_01);
    f.NumReadyEdit.Text := itoa(oldreadylistqty);
    f.LinkEdit.Text := db.CrawlerLink(part, color);
    f.NameEdit.Text := part;
    initialalias := f.AliasEdit.Text;
    initnewname := f.NewNameEdit.Text;
    pci := db.PieceColorInfo(part, color);
    PieceToImage(f.Image1, part, color);
    if pci <> nil then
    begin
      oldpreflocation := pci.preferedlocation;
      f.PrefLocationEdit.Text := oldpreflocation;

      oldtags := TStringList.Create;
      oldtags.Text := pci.tags.Text;
      oldtags.Sorted := True;

      for i := 2015 to 2015 + 31 do
        if pci.GetLugbulk(i) then
        begin
          lnum := 0;
          linv := db.GetSetInventory('LUGBULK-' + itoa(i));
          if linv <> nil then
            lnum := linv.LoosePartCount(part, color);
          if lnum > 0 then
            f.LugbulksListBox.Items.Add(itoa(i) + ': ' + itoa(lnum))
          else
            f.LugbulksListBox.Items.Add(itoa(i));
        end;
      if f.LugbulksListBox.Items.Count > 0 then
        try f.LugbulksListBox.ItemIndex := 0 except end;
      f.PreferLugbulkButton.Enabled := f.LugbulksListBox.Items.Count > 0;

      olst := TStringList.Create;
      oinf := orders.ItemInfo(apart, color);
      if oinf <> nil then
        for i := 0 to oinf.Count - 1 do
        begin
          oitem := oinf.Objects[i] as TOrderItemInfo;
          if (oitem.orderstatus <> 'NSS') and (oitem.orderstatus <> 'Canceled') and (oitem.orderstatus <> 'Cancelled') then
            olst.Add(itoa(oitem.orderid) + ': ' + itoa(oitem.num));
        end;
      if olst.Count > 0 then
      begin
        olst.CustomSort(sortorderlist);
        for i := 0 to olst.Count - 1 do
          f.OrdersListBox.Items.Add(olst.Strings[i]);
        try f.OrdersListBox.ItemIndex := 0 except end;
      end;
      olst.Free;

      f.PreferOrderButton.Enabled := f.OrdersListBox.Items.Count > 0;

      for i := 0 to oldtags.Count - 1 do
        f.TagsListBox.Items.Add(oldtags.Strings[i]);

      initialcode := pci.code;
      f.PartCodeEdit.Text := initialcode;

      f.parttype := pci.sparttype;

      f.YearEdit.ReadOnly := (pci.year <> 0) and not pci.canedityear;
      initialyear := pci.year;
      f.YearEdit.Text := itoa(initialyear);
      if f.YearEdit.ReadOnly then
        f.YearEdit.Color := clBtnFace;
      if not f.YearEdit.ReadOnly then
        f.UpdateYearSpeedButton.Visible := db.HasSetColorsOnly(pci.piece) or (pci.sparttype = 'P') or (pci.sparttype = 'M') or (pci.sparttype = ' ') or (pci.sparttype = 'G');

      f.Panel1.Color := RGBInvert(db.colors(color).RGB);
      f.PartTypePanel.Caption := pci.sparttype;
      f.PartTypeLabel.Caption := PartTypeToPartTypeName(pci.sparttype);
      if Trim(f.PartTypeLabel.Caption) = '' then
        f.PartTypeLabel.Caption := '(Default)';
      f.Memo1.Lines.Text := pci.storage.Text;
      initialstorage := f.Memo1.Lines.Text;
      num := inventory.LoosePartCount(part, color);
      if islikeset then
        num2 := inventory.BuildSetCount(part)
      else
        num2 := 0;
      editingset := false;
      if num <> 0 then
      begin
        f.NumPiecesEdit.Text := itoa(num);
        f.NumPiecesLabel.Caption := 'Num Pieces: ';
      end
      else if (num2 <> 0) or islikeset then
      begin
        editingset := true;
        f.NumPiecesEdit.Text := itoa(num2);
        f.NumPiecesLabel.Caption := 'Builted sets: ';
      end
      else
      begin
        f.NumPiecesEdit.Text := itoa(num);
        f.NumPiecesLabel.Caption := 'Num Pieces: ';
      end;
      f.AliasUpdate;
      f.ShowModal;
      if f.ModalResult = mrOK then
      begin
        Screen.Cursor := crHourglass;
        try
          Result := True;
          newyear := atoi(strtrim(f.YearEdit.Text));
          if initialyear <> newyear then
          begin
            if db.HasSetColorsOnly(pci.piece) then
              db.UpdateYearForAllColors(pci.piece, newyear)
            else
              db.SetItemYear(pci, newyear);
          end;
          if strupper(strtrim(f.AliasEdit.Text)) <> strupper(strtrim(initialalias)) then
            db.AddPieceAlias(f.AliasEdit.Text, part);
          if strupper(strtrim(f.NewNameEdit.Text)) <> strupper(strtrim(initnewname)) then
            db.SetNewPieceName(part, strtrim(f.NewNameEdit.Text));
          if strupper(strtrim(f.PrefLocationEdit.Text)) <> strupper(strtrim(oldpreflocation)) then
            db.SetPreferedLocation(part, color, strtrim(f.PrefLocationEdit.Text));

          newnum := atoi(f.NumPiecesEdit.Text);
          if editingset then
          begin
            if newnum > num2 then
            begin
              for i := num2 + 1 to newnum do
                inventory.AddSet(part, AS_NORMAL);
            end
            else if newnum < num2 then
            begin
              for i := newnum + 1 to num2 do
                inventory.RemoveSet(part, AS_NORMAL);
            end;
          end
          else
            inventory.AddLoosePart(part, color, newnum - num);
          pt := f.PartTypePanel.Caption;
          if Length(pt) = 1 then
            if pci.sparttype <> pt then
            begin
              db.SetPartType(part, color, pt[1]);
              db.FlashPartTypes;
            end;
          db.AddCrawlerLink(part, color, f.LinkEdit.Text);

          if f.Memo1.Lines.Text <> initialstorage then
          begin
            s := TStringList.Create;
            try
              s.Text := f.Memo1.Lines.Text;
              db.SetPieceStorage(part, color, s);
            finally
              s.Free;
            end;
          end;

          f.PartCodeEdit.Text := Trim(f.PartCodeEdit.Text);
          if f.PartCodeEdit.Text <> initialcode then
            db.SetPieceCode(pci, f.PartCodeEdit.Text);

          if (color = 9997) or (color = 9998) then
            if f.WeightEdit.Text <> initialweight then
              db.SetAssetWeight(part, color, atof(f.WeightEdit.Text));

          newreadylistqty := atoi(f.NumReadyEdit.Text);
          if newreadylistqty <> oldreadylistqty then
            BI_SetQtyToReadyList(part, color, newreadylistqty, S_READYLIST_01);

          db.CrawlerPriorityPart(part, color);

          newtags := TStringList.Create;

          for i := 0 to f.TagsListBox.Items.Count - 1 do
            newtags.Add(f.TagsListBox.Items.Strings[i]);
          newtags.Sorted := True;

          tagchange := false;

          for i := 0 to oldtags.Count - 1 do
            if newtags.IndexOf(oldtags.Strings[i]) < 0 then
            begin
              db.RemovePieceTag(pci, oldtags.Strings[i]);
              tagchange := True;
            end;

          for i := 0 to newtags.Count - 1 do
            if oldtags.IndexOf(newtags.Strings[i]) < 0 then
            begin
              db.AddPieceTag(pci, newtags.Strings[i]);
              tagchange := True;
            end;

          if tagchange then
            db.SaveTags;

          newtags.Free;

          if f.imgchanged then
          begin
            P := TPNGObject.Create;
            try
              P.CompressionLevel := DEF_PNG_COMPRESSION_LEVEL;
              P.Assign(f.Image1.Picture.Bitmap);
            finally
            end;
          end;
        finally
          Screen.Cursor := crDefault;
        end;
      end;
      oldtags.Free;
    end;
  finally
    f.Free;
  end;
  if P <> nil then
  try
    savename := basedefault + itoa(color) + '\' + part + '.png';
    try
      P.SaveToFile(savename);
    except
      sstrm := BI_KeepFileStream(savename);
      P.SaveToStream(sstrm);
      sstrm.Free;
    end;
  finally
    P.Free;
  end;
end;

var
  last_pos: string = 'BOX01';

procedure TEditPieceForm.SpeedButton1Click(Sender: TObject);
var
  value: string;
  storages: TStringList;
begin
  value := '1';
  storages := db.StorageBins;
  if InputQuery3(Caption, 'Add pieces', 'Storage bin', storages.Text, value, last_pos) then
  begin
    NumPiecesEdit.Text := itoa(atoi(NumPiecesEdit.Text) + atoi(value));
    if last_pos <> '' then
      Memo1.Lines.Add(last_pos + ':' + value);
  end;
  storages.Free;
end;

procedure TEditPieceForm.SpeedButton2Click(Sender: TObject);
var
  value: string;
  num: integer;
  storages: TStringList;
begin
  value := '1';
  storages := db.StorageBins;
  if InputQuery3(Caption, 'Remove pieces', 'Storage bin', storages.Text, value, last_pos) then
  begin
    num := atoi(NumPiecesEdit.Text) - atoi(value);
    if num < 0 then
      num := 0;
    NumPiecesEdit.Text := itoa(num);
    if last_pos <> '' then
      Memo1.Lines.Add(last_pos + ':-' + value);
  end;
  storages.Free;
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
  p := CharPos(':', s);
  if p <= 0 then
    Exit;
  splitstring(s, s1, s2, ':');
  if s1 <> '' then
    last_pos := s1;
end;

procedure TEditPieceForm.AutodetectButtonClick(Sender: TObject);
var
  s: string;
begin
  Screen.Cursor := crHourglass;
  try
    s := NET_GetBricklinkAliasRB(NameEdit.Text);
    if s = '' then
    begin
      if Trim(AliasEdit.Text) <> '' then
        s := NET_GetBricklinkAliasBL(Trim(AliasEdit.Text));
      if Pos('pr', NameEdit.Text) > 0 then
      begin
        if s = '' then
          if Trim(NewNameEdit.Text) <> '' then
            s := NET_GetBricklinkAliasBL(Trim(NewNameEdit.Text));
        if s = '' then
          s := NET_GetBricklinkAliasBL(NameEdit.Text);
      end
      else
      begin
        if s = '' then
          s := NET_GetBricklinkAliasBL(NameEdit.Text);
        if s = '' then
          if Trim(NewNameEdit.Text) <> '' then
            s := NET_GetBricklinkAliasBL(Trim(NewNameEdit.Text));
      end;
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
    if (yearnum >= MIN_ACCEPRABLE_YEAR) and (yearnum <= MAX_ACCEPTABLE_YEAR) then
      YearEdit.Text := itoa(yearnum);
  end
  else if PartTypePanel.Caption = 'G' then
  begin
    Screen.Cursor := crHourglass;
    try
      if db.GetPartYearFromNet(Trim(NameEdit.Text), partcolor, yearnum) then
        if (yearnum >= MIN_ACCEPRABLE_YEAR) and (yearnum <= MAX_ACCEPTABLE_YEAR) then
          YearEdit.Text := itoa(yearnum);
    finally
      Screen.Cursor := crDefault;
    end;
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
    if (yearnum >= MIN_ACCEPRABLE_YEAR) and (yearnum <= MAX_ACCEPTABLE_YEAR) then
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
    if (yearnum >= MIN_ACCEPRABLE_YEAR) and (yearnum <= MAX_ACCEPTABLE_YEAR) then
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
    if (yearnum >= MIN_ACCEPRABLE_YEAR) and (yearnum <= MAX_ACCEPTABLE_YEAR) then
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
        if (yearnum >= MIN_ACCEPRABLE_YEAR) and (yearnum <= MAX_ACCEPTABLE_YEAR) then
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
  imgchanged := false;
  PageControl1.ActivePageIndex := 0;
  TagsListBox.Clear;
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

procedure TEditPieceForm.NumReadyEditKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
  begin
    Key := #0;
    Exit;
  end;
end;

procedure TEditPieceForm.AddReadyListSpeedButtonClick(Sender: TObject);
var
  value: string;
begin
  value := '1';
  if InputQuery(Caption, 'Add pieces to Ready List', value) then
    NumReadyEdit.Text := itoa(atoi(NumReadyEdit.Text) + atoi(value));
end;

procedure TEditPieceForm.RemoveReadyListSpeedButtonClick(Sender: TObject);
var
  value: string;
  num: integer;
begin
  value := '1';
  if InputQuery(Caption, 'Remove pieces from Ready List', value) then
  begin
    num := atoi(NumReadyEdit.Text) - atoi(value);
    if num < 0 then
      num := 0;
    NumReadyEdit.Text := itoa(num);
  end;
end;

procedure TEditPieceForm.AddTagButtonClick(Sender: TObject);
var
  newtag: string;
  idx: integer;
begin
  newtag := '';
  if InputListQuery(Caption, 'Add tag', db.AllTags.Text, newtag) then
  begin
    if TagsListBox.Items.IndexOf(newtag) < 0 then
    begin
      idx := TagsListBox.Items.Add(newtag);
      TagsListBox.ItemIndex := idx;
    end
    else
      MessageBeep(MB_ICONERROR);
  end;
end;

procedure TEditPieceForm.RemoveTagButtonClick(Sender: TObject);
var
  idx: integer;
begin
  idx := TagsListBox.ItemIndex;
  if not IsIntegerInRange(idx, 0, TagsListBox.Items.Count - 1) then
  begin
    MessageBeep(MB_ICONERROR);
    Exit;
  end;
  TagsListBox.Items.Delete(idx);
end;

procedure TEditPieceForm.PreferLugbulkButtonClick(Sender: TObject);
begin
  if LugbulksListBox.ItemIndex > -1 then
    PrefLocationEdit.Text := 'LUGBULK ' + firstword(LugbulksListBox.Items.Strings[LugbulksListBox.ItemIndex], ':');
end;

procedure TEditPieceForm.PreferOrderButtonClick(Sender: TObject);
begin
  if OrdersListBox.ItemIndex > -1 then
    PrefLocationEdit.Text := 'ORDER ' + firstword(OrdersListBox.Items.Strings[OrdersListBox.ItemIndex], ':');
end;

procedure TEditPieceForm.LugbulksListBoxClick(Sender: TObject);
begin
  PreferLugbulkButton.Enabled := (LugbulksListBox.Items.Count > 0) and (LugbulksListBox.ItemIndex > -1);
  UpdateImageThumbs(1);
end;

procedure TEditPieceForm.OrdersListBoxClick(Sender: TObject);
begin
  PreferOrderButton.Enabled := (OrdersListBox.Items.Count > 0) and (OrdersListBox.ItemIndex > -1);
  UpdateImageThumbs(2);
end;

procedure TEditPieceForm.ClearPrefSpeedButtonClick(Sender: TObject);
begin
  PrefLocationEdit.Text := '';
end;

procedure TEditPieceForm.AliasNameChangeButton1Click(Sender: TObject);
begin
  if Trim(AliasEdit.Text) <> '' then
    if Trim(NewNameEdit.Text) = '' then
    begin
      NewNameEdit.Text := Trim(AliasEdit.Text);
      AliasEdit.Text := '';
    end;
end;

procedure TEditPieceForm.AliasUpdate;
begin
  AliasNameChangeButton1.Enabled := (Trim(AliasEdit.Text) <> '') and (Trim(NewNameEdit.Text) = '');
end;

procedure TEditPieceForm.AliasEditChange(Sender: TObject);
begin
  AliasUpdate;
end;

procedure TEditPieceForm.NewNameEditChange(Sender: TObject);
begin
  AliasUpdate;
end;

procedure TEditPieceForm.PopupMenu1Popup(Sender: TObject);
begin
  Paste1.Enabled := Clipboard.HasFormat(CF_PICTURE);
  Copy1.Enabled := Image1.Picture.Bitmap.Width > 0;
end;

procedure TEditPieceForm.Paste1Click(Sender: TObject);
var
  b: boolean;
begin
  if Clipboard.HasFormat(CF_PICTURE) then
  begin
    try
      Image1.Picture.Bitmap.Assign(Clipboard);
      b := true;
    except
      b := false;
    end;
    imgchanged := imgchanged or b;
  end;
end;

procedure TEditPieceForm.Copy1Click(Sender: TObject);
begin
  if Image1.Picture.Bitmap.Width > 0 then
  try
    Clipboard.Assign(Image1.Picture.Bitmap);
  finally
  end;
end;

procedure TEditPieceForm.UpdateImageThumbs(const flags: integer);
var
  idx: integer;
  fninput, fnoutput, lugbulkid, orderid: string;

  procedure ClearImage(const img: TImage);
  var
    C: TCanvas;
  begin
    C := img.Picture.Bitmap.Canvas;
    C.Pen.Color := clWhite;
    C.Pen.Style := psSolid;
    C.Brush.Color := clWhite;
    C.Brush.Style := bsSolid;
    C.Rectangle(0, 0, img.Picture.Bitmap.Width + 1, img.Picture.Bitmap.Height + 1);
  end;

begin
  if flags and 1 <> 0 then
  begin
    idx := LugbulksListBox.ItemIndex;
    if idx >= 0 then
    begin
      lugbulkid := 'LUGBULK-' + firstword(LugbulksListBox.Items.Strings[idx], ':');
      fninput := basedefault + '-1\' + lugbulkid + '.png';
      fnoutput := basedefault + '-1\' + lugbulkid + '.th128.png';
      if not fexists(fnoutput) then
        if fexists(fninput) then
          ResizePng2Png(fninput, fnoutput, false, 128, 128);
      if fexists(fnoutput) then
        LBImage.Picture.LoadFromFile(fnoutput)
      else
        ClearImage(LBImage);
    end
    else
      ClearImage(LBImage);
  end;

  if flags and 2 <> 0 then
  begin
    idx := OrdersListBox.ItemIndex;
    if idx >= 0 then
    begin
      orderid := firstword(OrdersListBox.Items.Strings[idx], ':');
      fninput := basedefault + 'orders\' + orderid + '.png';
      fnoutput := basedefault + 'orders\' + orderid + '.th128.png';
      if not fexists(fnoutput) then
        if fexists(fninput) then
          ResizePng2Png(fninput, fnoutput, false, 128, 128);
      if fexists(fnoutput) then
        OrImage.Picture.LoadFromFile(fnoutput)
      else
        ClearImage(OrImage);
    end
    else
      ClearImage(OrImage);
  end;
end;

procedure TEditPieceForm.PageControl1Change(Sender: TObject);
begin
  UpdateImageThumbs(3);
end;

end.
