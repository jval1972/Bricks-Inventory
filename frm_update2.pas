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
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit frm_update2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TUpdatePartNamesRebrForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Button3: TButton;
    Panel3: TPanel;
    Memo1: TMemo;
    Panel4: TPanel;
    Label6: TLabel;
    Label7: TLabel;
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function UpdatePartNamesFromRebrickable(const lst: TStringList): boolean;

implementation

{$R *.dfm}

uses
  bi_db, bi_globals;

function UpdatePartNamesFromRebrickable(const lst: TStringList): boolean;
var
  f: TUpdatePartNamesRebrForm;
begin
  result := false;
  if lst = nil then
    Exit;
  f := TUpdatePartNamesRebrForm.Create(nil);
  try
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      lst.AddStrings(f.Memo1.Lines);
      result := True;
    end;
  finally
    f.Free;
  end;
end;

procedure TUpdatePartNamesRebrForm.Button3Click(Sender: TObject);
var
  i: integer;
  pi: TPieceInfo;
begin
  Screen.Cursor := crHourGlass;
  try
    Memo1.Lines.Clear;
    for i := 0 to db.AllPieces.Count - 1 do
    begin
      pi := db.AllPieces.Objects[i] as TPieceInfo;
      if UpperCase(pi.name) = UpperCase(pi.desc) then
      begin
        Memo1.Lines.Add('updatepartnamerebrickable/' + pi.name);
        Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
        Label7.Update;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
