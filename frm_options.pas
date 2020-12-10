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
    Label1: TLabel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function BI_EditOptions: boolean;

implementation

{$R *.dfm}

uses
  bi_delphi, bi_defs;

function BI_EditOptions: boolean;
var
  f: TOptionsForm;
  oldpagesize: integer;
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
    exit;
  end;
  if Key in ['0'..'9'] then
    if Length((Sender as TEdit).Text) > 3 then
    begin
      Key := #0;
      exit;
    end;
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
end;

end.
