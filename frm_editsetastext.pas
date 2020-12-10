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
    Edit2: TEdit;
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
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure ModifyButtonClick(Sender: TObject);
    procedure txtNumKeyPress(Sender: TObject; var Key: Char);
    procedure ImportButtonClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
  private
    { Private declarations }
    procedure PopulateColors;
    function GetEditLineInfo(var spart, scolor, snum: string): boolean;
  public
    { Public declarations }
  end;

function EditSetAsTextForm(const setid: string; var data: string; var desc: string; var year: integer; var ismoc: boolean): boolean;

implementation

{$R *.dfm}

uses
  bi_delphi, bi_db, bi_globals, bi_utils, searchpart, ImportFileForm;

function EditSetAsTextForm(const setid: string; var data: string; var desc: string; var year: integer; var ismoc: boolean): boolean;
var
  f: TEditSetAsTextForm;
begin
  result := false;
  f := TEditSetAsTextForm.Create(nil);
  try
    f.Caption := f.Caption + ' - ' + setid;
    f.Memo1.Text := data;
    f.Edit1.Text := desc;
    f.Edit2.Text := itoa(year);
    f.CheckBox1.Checked := ismoc;
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      data := f.Memo1.Text;
      desc := f.Edit1.Text;
      year := atoi(trim(f.Edit2.Text));
      ismoc := f.CheckBox1.Checked;
      result := true;
    end;
  finally
    f.Free;
  end;
end;

procedure TEditSetAsTextForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
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
    exit;
  end;
  p := Pos(':', boxColor.Text);
  if p = 0 then
  begin
    ShowMessage('Please specify the color');
    try boxColor.SetFocus; except end;
    exit;
  end;
  scolor :=  Trim(Copy(boxColor.Text, 0, p - 1));
  snum := txtNum.Text;
  if StrToIntDef(snum, -1) <= 0 then
  begin
    ShowMessage('Please type the quantity');
    try txtNum.SetFocus; except end;
    exit;
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
      if Pos(stmpU, UpperCase(Memo1.Lines.Strings[i])) = 1 then
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

end.
