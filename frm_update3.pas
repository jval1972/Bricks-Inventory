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

unit frm_update3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TTUpdatePartColorBLForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Button3: TButton;
    Panel3: TPanel;
    Memo1: TMemo;
    Panel4: TPanel;
    Label6: TLabel;
    CheckBox1: TCheckBox;
    Label7: TLabel;
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    dobricklink: boolean;
    { Public declarations }
  end;

function UpdatePartColorsFromBricklink(const lst: TStringList): boolean;

function UpdatePartColorsFromRebrickable(const lst: TStringList): boolean;

implementation

{$R *.dfm}

uses
  bi_db, bi_globals;

function UpdatePartColorsFromBricklink(const lst: TStringList): boolean;
var
  f: TTUpdatePartColorBLForm;
begin
  result := false;
  if lst = nil then
    Exit;
  f := TTUpdatePartColorBLForm.Create(nil);
  try
    f.Caption := 'Update Parts without known Colors (bricklink.com)';
    f.dobricklink := True;
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

function UpdatePartColorsFromRebrickable(const lst: TStringList): boolean;
var
  f: TTUpdatePartColorBLForm;
begin
  result := false;
  if lst = nil then
    Exit;
  f := TTUpdatePartColorBLForm.Create(nil);
  try
    f.Caption := 'Update Parts without known Colors (rebrickable.com)';
    f.dobricklink := False;
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

procedure TTUpdatePartColorBLForm.Button3Click(Sender: TObject);
var
  i: integer;
  b: boolean;
  tmp: TStringList;
  stmp: string;
begin
  Screen.Cursor := crHourGlass;
  tmp := TStringList.Create;
  try
    b := CheckBox1.Checked;
    Memo1.Lines.Clear;
    if dobricklink then
    begin
      for i := 0 to db.AllPieces.Count - 1 do
        if db.MoldHasNoColors(db.AllPieces.Strings[i]) then
        begin
          tmp.Add('refreshpiecefrombricklinknorefresh/' + db.AllPieces.Strings[i]);
          if b then
            tmp.Add('spiece/' + db.AllPieces.Strings[i]);
          if (tmp.Count > 200) or ((tmp.Count > 30) and (random > 0.6)) then
          begin
            stmp := tmp.Strings[tmp.Count - 1];
            tmp.Delete(tmp.Count - 1);
            Memo1.Lines.AddStrings(tmp);
            Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
            Label7.Update;
            Memo1.Lines.Add(stmp);
            Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
            Label7.Update;
            tmp.Clear;
          end;
        end;
    end
    else
    begin
      for i := 0 to db.AllPieces.Count - 1 do
        if Pos('pr', db.AllPieces.Strings[i]) > 1 then
          if db.MoldHasNoColors(db.AllPieces.Strings[i]) then
          begin
            tmp.Add('refreshpieceorgearfromrebrickablenorefresh/' + db.AllPieces.Strings[i]);
            if b then
              tmp.Add('spiece/' + db.AllPieces.Strings[i]);
            if (tmp.Count > 200) or ((tmp.Count > 30) and (random > 0.6)) then
            begin
              stmp := tmp.Strings[tmp.Count - 1];
              tmp.Delete(tmp.Count - 1);
              Memo1.Lines.AddStrings(tmp);
              Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
              Label7.Update;
              Memo1.Lines.Add(stmp);
              Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
              Label7.Update;
              tmp.Clear;
            end;
          end;
    end;
    if tmp.Count > 0 then
    begin
      stmp := tmp.Strings[tmp.Count - 1];
      tmp.Delete(tmp.Count - 1);
      if tmp.Count > 0 then
        Memo1.Lines.AddStrings(tmp);
      Memo1.Lines.Add(stmp);
      Label7.Caption := Format('(%d actions)', [Memo1.Lines.Count]);
      Label7.Update;
    end;
  finally
    tmp.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TTUpdatePartColorBLForm.FormCreate(Sender: TObject);
begin
  dobricklink := True;
end;

end.
