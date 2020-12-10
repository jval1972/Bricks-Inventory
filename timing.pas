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
  bi_db, bi_globals, main;

procedure TTimingForm.FormCreate(Sender: TObject);
begin
  crawling := False;
  cancrowl := True;
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

  crawling := True;
  db.Crawler;
  crawling := False;
end;

procedure TTimingForm.FormDestroy(Sender: TObject);
begin
  cancrowl := False;
end;

end.
