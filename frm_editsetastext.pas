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
//    Edit set Form
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit frm_editsetastext;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons;

type
  TEditSetAsTextForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    YearEdit: TEdit;
    CheckBox1: TCheckBox;
    EditPanel: TPanel;
    Label3: TLabel;
    txtPart: TEdit;
    SpeedButton1: TSpeedButton;
    boxColor: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    txtNum: TEdit;
    AddButton: TButton;
    ModifyButton: TButton;
    ImportButton: TButton;
    OpenDialog1: TOpenDialog;
    UpdateYearSpeedButton: TSpeedButton;
    AppendButton: TButton;
    RemoveButton: TButton;
    TabSheet3: TTabSheet;
    CostMemo: TMemo;
    AddPurchaseButton1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure ModifyButtonClick(Sender: TObject);
    procedure txtNumKeyPress(Sender: TObject; var Key: Char);
    procedure ImportButtonClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure UpdateYearSpeedButtonClick(Sender: TObject);
    procedure YearEditKeyPress(Sender: TObject; var Key: Char);
    procedure AppendButtonClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure AddPurchaseButton1Click(Sender: TObject);
  private
    { Private declarations }
    procedure PopulateColors;
    function GetEditLineInfo(var spart, scolor, snum: string): boolean;
  public
    { Public declarations }
    setid: string;
  end;

function EditSetAsTextForm(const setid: string; var data: string; var desc: string;
  var year: integer; var ismoc: boolean): boolean;

implementation

{$R *.dfm}

uses
  urlmon,
  bi_delphi, bi_db, bi_globals, bi_utils, searchpart, ImportFileForm;

function EditSetAsTextForm(const setid: string; var data: string; var desc: string;
  var year: integer; var ismoc: boolean): boolean;
var
  f: TEditSetAsTextForm;
  pcostdata, ncostdata: string;
  costfname: string;
begin
  Result := False;
  f := TEditSetAsTextForm.Create(nil);
  try
    f.Caption := f.Caption + ' - ' + setid;
    f.PageControl1.ActivePageIndex := 0;
    f.setid := setid;
    f.Memo1.Text := data;
    f.Edit1.Text := desc;
    f.YearEdit.Text := itoa(year);
    f.CheckBox1.Checked := ismoc;
    costfname := basedefault + 'out\' + setid + '\' + setid + '_cost.txt';
    pcostdata := LoadStringFromFile(costfname);
    f.CostMemo.Text := pcostdata;
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      data := f.Memo1.Text;
      desc := f.Edit1.Text;
      year := atoi(trim(f.YearEdit.Text));
      ismoc := f.CheckBox1.Checked;
      ncostdata := f.CostMemo.Text;
      if ncostdata <> pcostdata then
      begin
        if not DirectoryExists(basedefault + 'out') then
          MkDir(basedefault + 'out');
        if not DirectoryExists(basedefault + 'out\' + setid) then
          MkDir(basedefault + 'out\' + setid);
        Backupfile(costfname);
        SaveStringToFile(costfname, ncostdata);
      end;
      Result := True;
    end;
  finally
    f.Free;
  end;
end;

procedure TEditSetAsTextForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
  setid := '';
  PopulateColors;
end;

procedure TEditSetAsTextForm.PopulateColors;
var
  n: integer;
  color: colorinfo_p;
begin
  boxColor.Items.Clear;
  for n := 0 to MAXINFOCOLOR do
  begin
    color := db.Colors(n);
    if (color.id = n) and (Length(Trim(color.name)) > 0) then
      boxColor.AddItem(LeftPad(IntToStr(color.id), 4, ' ') + ' : ' + color.name, TObject(color.id));
  end;
  if boxColor.Items.Count > 0 then
    boxColor.ItemIndex := 0;
end;

procedure TEditSetAsTextForm.SpeedButton1Click(Sender: TObject);
var
  sp: string;
begin
  sp := txtPart.Text;
  if GetPieceID(sp) then
    txtPart.Text := sp;
end;

function TEditSetAsTextForm.GetEditLineInfo(var spart, scolor, snum: string): boolean;
var
  pi: TPieceInfo;
  p: integer;
begin
  Result := False;
  spart := txtPart.Text;
  pi := db.PieceInfo(spart);
  if pi.desc = '(Unknown)' then
  begin
    ShowMessage(Format('Unknown piece %s', [spart]));
    try txtPart.SetFocus; except end;
    Exit;
  end;
  p := CharPos(':', boxColor.Text);
  if p = 0 then
  begin
    ShowMessage('Please specify the color');
    try boxColor.SetFocus; except end;
    Exit;
  end;
  scolor :=  Trim(Copy(boxColor.Text, 0, p - 1));
  snum := txtNum.Text;
  if StrToIntDef(snum, -1) <= 0 then
  begin
    ShowMessage('Please type the quantity');
    try txtNum.SetFocus; except end;
    Exit;
  end;
  Result := True;
end;

procedure TEditSetAsTextForm.AddButtonClick(Sender: TObject);
var
  spart, scolor, snum: string;
begin
  if GetEditLineInfo(spart, scolor, snum) then
    Memo1.Lines.Insert(1, spart + ',' + scolor + ',' + snum);
end;

procedure TEditSetAsTextForm.ModifyButtonClick(Sender: TObject);
var
  spart, scolor, snum: string;
  stmp, stmpU: string;
  i: integer;
  idx: integer;
begin
  if GetEditLineInfo(spart, scolor, snum) then
  begin
    stmp := spart + ',' + scolor + ',';
    stmpU := UpperCase(stmp);
    idx := 0;

    for i := 1 to Memo1.Lines.Count - 1 do
    begin
      if Pos1(stmpU, UpperCase(Memo1.Lines.Strings[i])) then
      begin
        idx := i;
        break;
      end;
    end;

    if idx > 0 then
    begin
      if StrToIntDef(snum, 0) > 0 then
        Memo1.Lines.Strings[idx] := stmp + snum
      else
        Memo1.Lines.Delete(idx);
    end
    else
      ShowMessage('Current inventory does not contain the specified part');
  end;
end;

procedure TEditSetAsTextForm.txtNumKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
    Key := #0;
end;

procedure TEditSetAsTextForm.ImportButtonClick(Sender: TObject);
var
  data: TStringList;
  ordValue: integer;
  source: sourcetype_t;
begin
  if OpenDialog1.Execute then
  begin
    data := TStringList.Create;
    try
      ordValue := OpenDialog1.FilterIndex;
      if (ordValue >= Ord(Low(sourcetype_t))) and (ordValue <= Ord(High(sourcetype_t))) then
        source := sourcetype_t(ordValue)
      else
        source := stBrickLink;

      Memo1.Lines.Clear;
      if source = stLDraw then
        Memo1.Lines.Add(LDrawToCSV(OpenDialog1.FileName))
      else if source = stLDCad then
        Memo1.Lines.Add(LDCadToCSV(OpenDialog1.FileName))
      else if GetDataFromFile(OpenDialog1.FileName, source, data) then
        Memo1.Lines.AddStrings(data);
    finally
      data.Free;
    end;
  end;
end;

procedure TEditSetAsTextForm.Memo1Change(Sender: TObject);
var
  s1: string;
begin
  if Memo1.Lines.Count = 0 then
  begin
    EditPanel.Visible := True;
    Exit;
  end;

  s1 := Trim(Memo1.Lines.Strings[0]);
  EditPanel.Visible := Pos('Code,Num', s1) < 1;
end;

procedure TEditSetAsTextForm.UpdateYearSpeedButtonClick(Sender: TObject);
var
  fname: string;
  urlstr: string;
  yearnum: integer;
  pci: TPieceColorInfo;
  stry: string;
begin
  fname := basedefault + 'db\setmolds\' + Trim(setid) + '.htm';
  if not fexists(fname) then
  begin
    fname := basedefault + 'db\molds\' + Trim(setid) + '.htm';
    if not fexists(fname) then
      fname := basedefault + 'db\minifigs\' + Trim(setid) + '.htm';
  end;
  if fexists(fname) then
  begin
    yearnum := db.GetSetYearFromDiskCache(fname);
    if (yearnum >= MIN_ACCEPRABLE_YEAR) and (yearnum <= MAX_ACCEPTABLE_YEAR) then
    begin
      YearEdit.Text := itoa(yearnum);
      Exit;
    end;
  end;

  pci := db.PieceColorInfo(setid, -1);
  if pci <> nil then
  begin
    if pci.sparttype = 'S' then
      stry := 'SM'
    else
      stry := 'MS';
  end
  else
    stry := 'SM';

  Screen.Cursor := crHourglass;
  try
    fname := basedefault + 'db\molds\' + Trim(setid) + '.htm';
    urlstr := sbricklink + 'v2/catalog/catalogitem.page?' + stry[1] + '=' + db.GetBLNetPieceName(Trim(setid));
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

  Screen.Cursor := crHourglass;
  try
    fname := basedefault + 'db\molds\' + Trim(setid) + '.htm';
    urlstr := sbricklink + 'v2/catalog/catalogitem.page?' + stry[2] + '=' + db.GetBLNetPieceName(Trim(setid));
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
end;

procedure TEditSetAsTextForm.YearEditKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
  begin
    Key := #0;
    Exit;
  end;
end;

procedure TEditSetAsTextForm.AppendButtonClick(Sender: TObject);
var
  data: TStringList;
  ordValue: integer;
  source: sourcetype_t;
  sL: TStringList;
  inv: TBrickInventory;
begin
  if OpenDialog1.Execute then
  begin
    data := TStringList.Create;
    try
      ordValue := OpenDialog1.FilterIndex;
      if (ordValue >= Ord(Low(sourcetype_t))) and (ordValue <= Ord(High(sourcetype_t))) then
        source := sourcetype_t(ordValue)
      else
        source := stBrickLink;

      sL := TStringList.Create;
      sL.AddStrings(Memo1.Lines);
      if source = stLDraw then
        sL.Add(LDrawToCSV(OpenDialog1.FileName))
      else if source = stLDCad then
        sL.Add(LDCadToCSV(OpenDialog1.FileName))
      else if GetDataFromFile(OpenDialog1.FileName, source, data) then
        sL.AddStrings(data);
      inv := TBrickInventory.Create;
      inv.LoadLooseParts(sL);
      inv.DoReorganize;
      sL.Clear;
      inv.GetLoosePartsInStringList(sL, False);
      Memo1.Lines.Clear;
      Memo1.Lines.Add('Part,Color,Num');
      Memo1.Lines.AddStrings(sL);
      inv.Free;
      sL.Free;
    finally
      data.Free;
    end;
  end;
end;

procedure TEditSetAsTextForm.RemoveButtonClick(Sender: TObject);
var
  data: TStringList;
  ordValue: integer;
  source: sourcetype_t;
  sL: TStringList;
  inv, inv2: TBrickInventory;
begin
  if OpenDialog1.Execute then
  begin
    data := TStringList.Create;
    try
      ordValue := OpenDialog1.FilterIndex;
      if (ordValue >= Ord(Low(sourcetype_t))) and (ordValue <= Ord(High(sourcetype_t))) then
        source := sourcetype_t(ordValue)
      else
        source := stBrickLink;

      sL := TStringList.Create;
      if source = stLDraw then
        sL.Add(LDrawToCSV(OpenDialog1.FileName))
      else if source = stLDCad then
        sL.Add(LDCadToCSV(OpenDialog1.FileName))
      else if GetDataFromFile(OpenDialog1.FileName, source, data) then
        sL.AddStrings(data);

      sL.Insert(0, 'Part,Color,Num');

      inv := TBrickInventory.Create;
      inv.LoadLooseParts(sL);

      inv2 := TBrickInventory.Create;
      sL.Clear;
      sL.AddStrings(Memo1.Lines);
      inv2.LoadLooseParts(sL);

      inv2.TryToRemoveInventory(inv);

      sL.Clear;
      inv2.GetLoosePartsInStringList(sL, False);
      Memo1.Lines.Clear;
      Memo1.Lines.Add('Part,Color,Num');
      Memo1.Lines.AddStrings(sL);
      inv.Free;
      inv2.Free;
      sL.Free;
    finally
      data.Free;
    end;
  end;
end;

procedure TEditSetAsTextForm.AddPurchaseButton1Click(Sender: TObject);
var
  nsets: integer;
  ncost: Double;
  ssets: string;
  scost: string;
begin
  nsets := 1;
  ncost := 9.99;
  ssets := itoa(nsets);
  scost := Format('%2.2f', [ncost]);
  if InputQuery2(Caption, 'Num sets', 'Cost per set', ssets, scost) then
  begin
    nsets := atoi(ssets);
    ncost := atof(scost);
    if nsets > 0 then
      CostMemo.Lines.Add(Format('%d,%2.2f', [nsets, ncost]));
  end;
end;

end.
