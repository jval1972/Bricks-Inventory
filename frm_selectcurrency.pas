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
//   Select Currency Form
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit frm_selectcurrency;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TSelectCurrencyForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    ListBox1: TListBox;
    Label1: TLabel;
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function SelectCurrency(var cur: string): boolean;

implementation

{$R *.dfm}

uses
  bi_delphi, bi_currency;

function SelectCurrency(var cur: string): boolean;
var
  f: TSelectCurrencyForm;
  i, idx: integer;
  s1, s2: string;
begin
  Result := False;
  f := TSelectCurrencyForm.Create(nil);
  try
    idx := 0;
    for i := 0 to currency_names.Count - 1 do
    begin
      splitstring(currency_names.strings[i], s1, s2, '=');
      f.ListBox1.AddItem(s1 + ' (' + s2 + ')', TStringInit.Create(s1));
      if s1 = cur then
        idx := i;
    end;
    f.ListBox1.ItemIndex := idx;
    f.ShowModal;
    if f.ModalResult =  mrOK then
    begin
      idx := f.ListBox1.ItemIndex;
      if idx >= 0 then
      begin
        cur := (f.ListBox1.Items.Objects[idx] as TStringInit).text;
        Result := True;
      end;
    end;
  finally
    f.Free;
  end;
end;

procedure TSelectCurrencyForm.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to ListBox1.Items.Count - 1 do
    ListBox1.Items.Objects[i].Free;
end;

end.
