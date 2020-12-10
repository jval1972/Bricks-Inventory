//------------------------------------------------------------------------------
//
//  BrickInventory: A tool for managing your brick collection
//  Copyright (C) 2014-2018 by Jim Valavanis
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
//    Progress Form
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit slpash;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TSplashForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    Label2: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SplashForm: TSplashForm;

procedure ShowSplash;
procedure HideSplash;

procedure SplashProgress(const msg: string; d: Double);

implementation

{$R *.dfm}

procedure ShowSplash;
begin
  SplashForm.Label1.Caption := '';
  SplashForm.ProgressBar1.Position := 0;
  SplashForm.Show;
  SplashForm.BringToFront;
  SplashForm.Repaint;
end;

procedure HideSplash;
begin
  SplashForm.Hide;
end;

procedure SplashProgress(const msg: string; d: Double);

  function makec(const d: double): integer;
  begin
    Result := round(d * 255);
    if Result < 0 then
      Result := 0
    else if Result > 255 then
      Result := 255;
  end;

begin
  if SplashForm.Label1.Caption <> msg then
  begin
    SplashForm.Label1.Caption := msg;
    SplashForm.Repaint;
  end;
  SplashForm.ProgressBar1.Position := Round(d * 100);
  SplashForm.Label2.Caption := Format('%2.2f%s', [d * 100, '%']);
  SplashForm.Label2.Font.Color := RGB(makec(1.0 - d), makec(d), 0);
  SplashForm.Repaint;
end;

end.
