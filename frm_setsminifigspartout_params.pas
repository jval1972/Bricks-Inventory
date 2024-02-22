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
// DESCRIPTION:
//    Query input Form
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit frm_setsminifigspartout_params;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TQueryMinifigPartOutParametersForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    TrackBar1: TTrackBar;
    Label3: TLabel;
    TrackBar2: TTrackBar;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Edit3: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    TrackBar3: TTrackBar;
    Label10: TLabel;
    Label11: TLabel;
    TrackBar4: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure Edit2KeyPress(Sender: TObject; var Key: Char);
    procedure Edit3KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  protected
    procedure UpdateControls;
  public
    { Public declarations }
  end;


var qpm_minyear: integer = 2010;
var qpm_minavailablelots: integer = 10;
var qpm_minminifigs: integer = 1;
var qpm_mindemand: double = 0.5;
var qpm_minpartsmultiplier: double = 0.5;
var qpm_minminifigsmultiplier: double = 0.5;
var qpm_partoutmultiplier: double = 1.0;

function QueryMinifigPartOutParameters(
  var minyear: integer;
  var minavailablelots: integer;
  var minminifigs: integer;
  var mindemand: double;
  var minpartsmultiplier: double;
  var minminifigsmultiplier: double;
  var partoutmultiplier: double): boolean;

implementation

{$R *.dfm}

uses
  bi_delphi;

function QueryMinifigPartOutParameters(
  var minyear: integer;
  var minavailablelots: integer;
  var minminifigs: integer;
  var mindemand: double;
  var minpartsmultiplier: double;
  var minminifigsmultiplier: double;
  var partoutmultiplier: double): boolean;
var
  f: TQueryMinifigPartOutParametersForm;
begin
  result := False;
  f := TQueryMinifigPartOutParametersForm.Create(nil);
  try
    f.Edit1.Text := IntToStr(minyear);
    f.Edit2.Text := IntToStr(minavailablelots);
    f.Edit3.Text := IntToStr(minminifigs);
    f.TrackBar1.Position := Round(mindemand * f.TrackBar1.Max / 2);
    f.TrackBar2.Position := Round(minpartsmultiplier * f.TrackBar2.Max / 2);
    f.TrackBar3.Position := Round(minminifigsmultiplier * f.TrackBar3.Max / 2);
    f.TrackBar4.Position := Round(partoutmultiplier * f.TrackBar4.Max / 2);
    f.UpdateControls;
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      minyear := atoi(f.Edit1.Text);
      minavailablelots := atoi(f.Edit2.Text);
      minminifigs := atoi(f.Edit3.Text);
      mindemand := f.TrackBar1.Position / f.TrackBar1.Max * 2;
      minpartsmultiplier := f.TrackBar2.Position / f.TrackBar2.Max * 2;
      minminifigsmultiplier := f.TrackBar3.Position / f.TrackBar3.Max * 2;
      partoutmultiplier := f.TrackBar4.Position / f.TrackBar4.Max * 2;
      result := True;
    end;
  finally
    f.Free;
  end;
end;

procedure TQueryMinifigPartOutParametersForm.FormCreate(Sender: TObject);
begin
  UpdateControls;
end;

procedure TQueryMinifigPartOutParametersForm.UpdateControls;
begin
  Label5.Caption := Format('%2.2f', [TrackBar1.Position / TrackBar1.Max * 2]);
  Label6.Caption := Format('%2.2f', [TrackBar2.Position / TrackBar2.Max * 2]);
  Label9.Caption := Format('%2.2f', [TrackBar3.Position / TrackBar3.Max * 2]);
  Label10.Caption := Format('%2.2f', [TrackBar4.Position / TrackBar4.Max * 2]);
end;

procedure TQueryMinifigPartOutParametersForm.TrackBar1Change(Sender: TObject);
begin
  UpdateControls;
end;

procedure TQueryMinifigPartOutParametersForm.TrackBar2Change(Sender: TObject);
begin
  UpdateControls;
end;

procedure TQueryMinifigPartOutParametersForm.TrackBar3Change(
  Sender: TObject);
begin
  UpdateControls;
end;

procedure TQueryMinifigPartOutParametersForm.TrackBar4Change(
  Sender: TObject);
begin
  UpdateControls;
end;

procedure TQueryMinifigPartOutParametersForm.Edit1KeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
  begin
    Key := #0;
    Exit;
  end;
  if Key in ['0'..'9'] then
    if Length((Sender as TEdit).Text) > 4 then
    begin
      Key := #0;
      Exit;
    end;
end;

procedure TQueryMinifigPartOutParametersForm.Edit2KeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
  begin
    Key := #0;
    Exit;
  end;
end;

procedure TQueryMinifigPartOutParametersForm.Edit3KeyPress(Sender: TObject;
  var Key: Char);
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

end.
