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
//    Crawler Timer Form
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit timing;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, bi_threadtimer;

type
  TTimingForm = class(TForm)
    CrawlerTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure CrawlerTimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
 //   tr: TThreadComponent;
    fails: integer;
    hits: integer;
    cancrowl: boolean;
  public
    { Public declarations }
    crawling: Boolean;
  end;

var
  TimingForm: TTimingForm;

implementation

{$R *.dfm}

uses
  bi_db, bi_globals, bi_crawler, bi_utils, main;

procedure TTimingForm.FormCreate(Sender: TObject);
begin
  crawling := False;
  cancrowl := True;
  fails := 0;
  hits := 0;
 { tr := TThreadComponent.Create(nil);
  tr.Sleep := CrawlerTimer.Interval;
  tr.OnExecute := CrawlerTimerTimer;
  tr.Enabled := true;  }
end;

procedure TTimingForm.CrawlerTimerTimer(Sender: TObject);
begin
  if not cancrowl then
    Exit;

  if db = nil then
    Exit;

  if crawling then
    Exit;

  if MainForm.activebits > 0 then
    Exit;

  if not MainForm.CheckBox1.Checked then
    Exit;

  if IsValidBLTime then
  begin
    crawling := True;
    MainForm.CrawlerPanel.Color := RGB(GetIntInRange(64 + 2 * fails, 64, 255), GetIntInRange(255 - 2 * fails, 64, 255), 64);
    MainForm.CrawlerPanel.Update;
    try
      if db.Crawler then
      begin
        inc(hits);
        fails := 0
      end
      else
      begin
        inc(fails);
        hits := 0;
      end;
    finally
      MainForm.CrawlerPanel.Color := clBtnFace;
      MainForm.CrawlerPanel.Update;
      crawling := False;
    end;
    if fails = 0 then
    begin
      if hits = 1 then
        MainForm.CrawlerPanel.Hint := '1 continuous hit'
      else
        MainForm.CrawlerPanel.Hint := Format('%d continuous hits', [hits])
    end
    else if fails = 1 then
      MainForm.CrawlerPanel.Hint := '1 continuous fail'
    else
      MainForm.CrawlerPanel.Hint := Format('%d continuous fails', [fails]);
  end;
end;

procedure TTimingForm.FormDestroy(Sender: TObject);
begin
  cancrowl := False;
end;

end.
