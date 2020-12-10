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
//    Edit Lugbulk Price (Form)
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit frm_editlugbulkprice;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TEditLugbulkPriceForm = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label5: TLabel;
    YearLabel: TLabel;
    Label1: TLabel;
    Panel1: TPanel;
    Label4: TLabel;
    Label2: TLabel;
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function EditLugbulkPrice(const part: string; const color: integer; const year: integer): boolean;

implementation

{$R *.dfm}

uses
  bi_delphi, bi_utils, bi_db, bi_globals, bi_lugbulk2017;

function EditLugbulkPrice(const part: string; const color: integer; const year: integer): boolean;
var
  f: TEditLugbulkPriceForm;
  pci: TPieceColorInfo;
  lb: TLugBulk2017;
  cost, cost2: double;
  fname: string;
begin
  Result := False;

  if not DirectoryExists(basedefault + 'lugbulks') then
    MkDir(basedefault + 'lugbulks');
  lb := TLugbulk2017.Create;
  fname := basedefault + 'lugbulks\' + itoa(year) + '.txt';
  if fexists(fname) then
    lb.LoadFromFile(fname);
  cost := lb.ItemCost(part, color);

  f := TEditLugbulkPriceForm.Create(nil);
  try
    f.Caption := 'Lugbulk Price ' + itoa(year);
    f.Label1.Caption := db.PieceDesc(part);
    f.YearLabel.Caption := itoa(year);
    f.YearLabel.Visible := true;
    f.Label4.Visible := true;
    f.Label5.Visible := true;
    f.Panel1.Visible := true;
    f.Label4.Caption := db.colors(color).name;
    f.Edit1.Text := Format('%2.4f', [cost]);
    pci := db.PieceColorInfo(part, color);
    PieceToImage(f.Image1, part, color);
    if pci <> nil then
    begin
      f.Panel1.Color := RGBInvert(db.colors(color).RGB);
      f.ShowModal;
      if f.ModalResult = mrOK then
      begin
        cost2 := atof(f.Edit1.Text);
        if cost <> cost2 then
        begin
          lb.SetItemCost(part, color, cost2);
          backupfile(fname);
          lb.SaveToFile(fname);
          Result := True;
        end;
      end;
    end;

  finally
    f.Free;
  end;

  lb.Free;
end;

procedure TEditLugbulkPriceForm.Edit1KeyPress(Sender: TObject;
  var Key: Char);
var
  i: integer;
  s: string;
begin
  if not (Key in [#8, '0'..'9', DecimalSeparator, ',', '.']) then
  begin
    Key := #0;
    Exit;
  end;

  if Key in [DecimalSeparator, ',', '.'] then
  begin
    s := Edit1.Text;
    for i := 1 to Length(s) do
      if s[i] in [DecimalSeparator, ',', '.'] then
      begin
        Key := #0;
        Exit;
      end;
  end;
end;

end.
