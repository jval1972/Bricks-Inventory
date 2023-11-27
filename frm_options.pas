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
//   Options Form
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit frm_options;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TOptionsForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    RadioGroup1: TRadioGroup;
    GroupBox1: TGroupBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function BI_EditOptions: boolean;

implementation

{$R *.dfm}

uses
  bi_delphi, bi_defs, bi_system, bi_currency;

function BI_EditOptions: boolean;
var
  f: TOptionsForm;
  oldpagesize: integer;
  i, idx: integer;
  s1, s2: string;
begin
  result := false;
  f := TOptionsForm.Create(nil);
  try
    f.CheckBox1.Checked := domultipagedocuments;
    oldpagesize := dpagesize;
    f.Edit1.Text := itoa(dpagesize);
    f.CheckBox2.Checked := dodraworderinfo;
    f.CheckBox3.Checked := usemultithread;
    f.CheckBox4.Checked := savealwayspartinvinfo;
    f.CheckBox5.Checked := generatethumbnailsondemand;
    f.CheckBox6.Checked := silentwarnings;
    if IsIntegerInRange(searchdownloadimg, 0, f.ComboBox1.Items.Count - 1) then
      f.ComboBox1.ItemIndex := searchdownloadimg;
    f.CheckBox8.Checked := optlocationslugbulk;
    f.CheckBox9.Checked := optlocationsorders;
    f.CheckBox10.Checked := optlocationsreadylist;
    f.CheckBox11.Checked := dodraworderinfolite;
    f.CheckBox12.Checked := quantizeimagetosavemem;
    f.CheckBox13.Checked := savealwayswantedlists;
    f.CheckBox7.Checked := optshowusernotes;
    f.CheckBox14.Checked := optshowautonotes;

    idx := 0;
    optdefaultcurrency := strupper(optdefaultcurrency);
    for i := 0 to currency_names.Count - 1 do
    begin
      splitstring(currency_names.strings[i], s1, s2, '=');
      f.ListBox1.AddItem(s1 + ' (' + s2 + ')', TStringInit.Create(s1));
      if s1 = optdefaultcurrency then
        idx := i;
    end;
    f.ListBox1.ItemIndex := idx;

    if IsIntegerInRange(inventorysortmethod, 0, f.RadioGroup1.items.Count - 1) then
      f.RadioGroup1.ItemIndex := inventorysortmethod;
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      result := true;
      domultipagedocuments := f.CheckBox1.Checked;
      dpagesize := atoi(f.Edit1.Text, oldpagesize);
      dodraworderinfo := f.CheckBox2.Checked;
      usemultithread := f.CheckBox3.Checked;
      savealwayspartinvinfo := f.CheckBox4.Checked;
      generatethumbnailsondemand := f.CheckBox5.Checked;
      silentwarnings := f.CheckBox6.Checked;
      searchdownloadimg := f.ComboBox1.ItemIndex;
      inventorysortmethod := f.RadioGroup1.ItemIndex;
      optlocationslugbulk := f.CheckBox8.Checked;
      optlocationsorders := f.CheckBox9.Checked;
      optlocationsreadylist := f.CheckBox10.Checked;
      dodraworderinfolite := f.CheckBox11.Checked;
      quantizeimagetosavemem := f.CheckBox12.Checked;
      savealwayswantedlists := f.CheckBox13.Checked;
      optshowusernotes := f.CheckBox7.Checked;
      optshowautonotes := f.CheckBox14.Checked;
      idx := f.ListBox1.ItemIndex;
      if idx >= 0 then
        optdefaultcurrency := (f.ListBox1.Items.Objects[idx] as TStringInit).text;
    end;
  finally
    f.Free;
  end;
end;

procedure TOptionsForm.Edit1KeyPress(Sender: TObject; var Key: Char);
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

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
end;

procedure TOptionsForm.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to ListBox1.Items.Count - 1 do
    ListBox1.Items.Objects[i].Free;
end;

end.
