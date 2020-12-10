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
//    Import file (based on Jarema@eurobricks.com fork) 
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit ImportFileForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TImportFileForm = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    csvDBL: TStringList;
  public
    { Public declarations }
  end;

type
  sourcetype_t = (stUnsuported ,stBrickLink ,stRebrickable ,stLDCad ,stLDraw);

function GetDataFromFile(const filename: string; const source: sourcetype_t; var data: TStringList): boolean;

implementation

{$R *.dfm}

uses
  bi_globals, bi_utils;

function GetDataFromFile(const filename: string; const source: sourcetype_t; var data: TStringList): boolean;
var
  delimeter: char;
  l: integer;
  csvLine, partType, partPrefix: string;
  f: TImportFileForm;
  partData: TStringList;
begin
  Result := false;
  f := TImportFileForm.Create(nil);

  case source of
    stBrickLink: delimeter := #9;
    stRebrickable: delimeter := ',';
  else
    delimeter := ' ';
  end;
  try
    f.csvDBL.loadFromFile(filename);
    f.Edit1.Text := filename;

    partData := TStringList.Create;
    try
      ExtractStrings([delimeter], [], PChar(f.csvDBL[0]), partData);
      f.ComboBox1.Items.AddStrings(partData);
      f.ComboBox2.Items.AddStrings(partData);
      f.ComboBox3.Items.AddStrings(partData);
      f.ComboBox1.ItemIndex := 0;
      f.ComboBox2.ItemIndex := MinI(1, partData.Count - 1);
      f.ComboBox3.ItemIndex := MinI(2, partData.Count - 1);
    finally
      partData.Free;
    end;

    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      Screen.Cursor := crHourglass;
      partData := TStringList.Create;
      try
        for l := 1 to f.csvDBL.Count - 1 do
        begin
          ExtractStrings([delimeter], [], PChar(f.csvDBL[l]), partData);
          if partData.Count = f.ComboBox1.Items.Count then
          begin
            partPrefix := '';
            partType := Trim(partData[f.ComboBox1.itemIndex]);
            case source of
              stBrickLink: partPrefix := 'BL ';
            end;
            csvLine := partPrefix + partType + ',' +
                partPrefix + Trim(partData[f.ComboBox2.itemIndex]) + ',' +
                Trim(partData[f.ComboBox3.itemIndex]);
            data.Add(csvLine);
          end;
          partData.Clear;
        end;
      finally
        partData.Free;
        Screen.Cursor := crDefault;
      end;
      Result := true;
    end;
  finally
    f.Free;
  end;
end;

procedure TImportFileForm.FormCreate(Sender: TObject);
begin
  SendMessage(GetWindow(ComboBox1.Handle,GW_CHILD), EM_SETREADONLY, 1, 0);
  SendMessage(GetWindow(ComboBox2.Handle,GW_CHILD), EM_SETREADONLY, 1, 0);
  SendMessage(GetWindow(ComboBox3.Handle,GW_CHILD), EM_SETREADONLY, 1, 0);
  csvDBL := TStringList.Create;
end;

procedure TImportFileForm.FormDestroy(Sender: TObject);
begin
  csvDBL.Free;
end;

end.
