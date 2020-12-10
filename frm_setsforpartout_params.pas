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
//    Query input Form
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit frm_setsforpartout_params;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TQueryPartOutParametersForm = class(TForm)
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
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure Edit2KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  protected
    procedure UpdateControls;
  public
    { Public declarations }
  end;


var qpa_minyear: integer = 2010;
var qpa_minavailablelots: integer = 10;
var qpa_mindemand: double = 0.5;
var qpa_mincostmultiplier: double = 1.25;

function QueryPartOutParameters(
  var minyear: integer;
  var minavailablelots: integer;
  var mindemand: double;
  var mincostmultiplier: double): boolean;

implementation

{$R *.dfm}

uses
  bi_delphi;

function QueryPartOutParameters(
  var minyear: integer;
  var minavailablelots: integer;
  var mindemand: double;
  var mincostmultiplier: double): boolean;
var
  f: TQueryPartOutParametersForm;
begin
  result := False;
  f := TQueryPartOutParametersForm.Create(nil);
  try
    f.Edit1.Text := IntToStr(minyear);
    f.Edit2.Text := IntToStr(minavailablelots);
    f.TrackBar1.Position := Round(mindemand * f.TrackBar1.Max / 2);
    f.TrackBar2.Position := Round(mincostmultiplier * f.TrackBar2.Max / 2);
    f.UpdateControls;
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      minyear := atoi(f.Edit1.Text);
      minavailablelots := atoi(f.Edit2.Text);
      mindemand := f.TrackBar1.Position / f.TrackBar1.Max * 2;
      mincostmultiplier := f.TrackBar2.Position / f.TrackBar2.Max * 2;
      result := True;
    end;
  finally
    f.Free;
  end;
end;

procedure TQueryPartOutParametersForm.FormCreate(Sender: TObject);
begin
  UpdateControls;
end;

procedure TQueryPartOutParametersForm.UpdateControls;
begin
  Label5.Caption := Format('%2.2f', [TrackBar1.Position / TrackBar1.Max * 2]);
  Label6.Caption := Format('%2.2f', [TrackBar2.Position / TrackBar2.Max * 2]);
end;

procedure TQueryPartOutParametersForm.TrackBar1Change(Sender: TObject);
begin
  UpdateControls;
end;

procedure TQueryPartOutParametersForm.TrackBar2Change(Sender: TObject);
begin
  UpdateControls;
end;

procedure TQueryPartOutParametersForm.Edit1KeyPress(Sender: TObject;
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

procedure TQueryPartOutParametersForm.Edit2KeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
  begin
    Key := #0;
    Exit;
  end;
end;

end.
