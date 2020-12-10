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
//    Remove piece from storage bin (Form)
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit removepiecefromstoragefrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TRemovePieceFromStorageForm = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label5: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    Panel1: TPanel;
    Label4: TLabel;
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function RemovePieceFromStorageForSet(const part: string; const color: integer; const setid: string; const storage: string): boolean;
function RemovePieceFromStorage(const part: string; const color: integer; num: integer; const storage: string): boolean;

implementation

{$R *.dfm}

uses
  bi_delphi, bi_utils, bi_db, bi_globals;

function RemovePieceFromStorageForSet(const part: string; const color: integer; const setid: string; const storage: string): boolean;
var
  inv: TBrickInventory;
  num: integer;
begin
  inv := db.GetSetInventory(setid);
  if inv = nil then
  begin
    result := False;
    Exit;
  end;
  num := inv.LoosePartCount(part, color);
  if num = 0 then
  begin
    Result := False;
    Exit;
  end;
  Result := RemovePieceFromStorage(part, color, num, storage);
end;

function RemovePieceFromStorage(const part: string; const color: integer; num: integer; const storage: string): boolean;
var
  f: TRemovePieceFromStorageForm;
  pci: TPieceColorInfo;
  inv: TBrickInventory;
  s: TStringList;
  invmax: integer;
begin
  result := False;
  if color = -1 then
    Exit;
  inv := db.InventoryForStorageBin(storage);
  if inv = nil then
    Exit;
  invmax := inv.LoosePartCount(part, color);
  if invmax < num then
    num := invmax;
  inv.Free;
  if num <= 0 then
    Exit;

  f := TRemovePieceFromStorageForm.Create(nil);
  try
    f.Label1.Caption := db.PieceDesc(part);
    f.Label2.Visible := True;
    f.Label4.Visible := True;
    f.Label5.Visible := True;
    f.Panel1.Visible := True;
    f.Label4.Caption := db.colors(color).name;
    f.Edit1.Text := itoa(num);
    if num = 1 then
      f.Label2.Caption := 'piece from storage ' + storage + '?'
    else
      f.Label2.Caption := 'pieces from storage ' + storage + '?';
    pci := db.PieceColorInfo(part, color);
    PieceToImage(f.Image1, part, color);
    if pci <> nil then
    begin
      f.Panel1.Color := RGBInvert(db.colors(color).RGB);
      f.ShowModal;
      if f.ModalResult = mrOK then
      begin
        s := TStringList.Create;
        s.Text := pci.storage.Text;

        num := atoi(f.Edit1.Text);
        if num > invmax then
          num := invmax;
        if num > 0 then
        begin
          s.Add(storage + ':-' + itoa(num));
          db.SetPieceStorage(part, color, s);
          Result := True;
        end
        else
        begin
          Result := False;
        end;
        s.Free;
      end;
    end;

  finally
    f.Free;
  end;
end;

procedure TRemovePieceFromStorageForm.Edit1KeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
  begin
    Key := #0;
    Exit;
  end;
end;

end.
