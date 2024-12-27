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
//   Database Installation Form
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit frm_install;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TInstallationForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    ButtonPanel1: TPanel;
    StartButton: TButton;
    CancelButton: TButton;
    Memo1: TMemo;
    BricksPanel: TPanel;
    Image1: TImage;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    iwidth, iheight: integer;
    task_id: integer;
    procedure Draw1x1(const x, y: integer; const color: LongWord; const border: boolean);
    procedure DrawBasePlate;
    procedure DrawRandomBrick;
  public
    { Public declarations }
  end;

function CheckInstallation: boolean;

function InstallDataBase: boolean;

implementation

{$R *.dfm}

uses
  bi_delphi, bi_db, bi_utils, bi_threads, bi_multithread, bi_wadfile, slpash;

const
  STUDSIZE = 16;
  NUMCOLORS = 38;
  SPLASH_TIT = 'Installing Databse';

function CheckInstallation: boolean;
begin
  Result :=
    fexists(basedefault + 'cache\cache.db') and
    fexists(basedefault + 'db\db_basebrick.txt') and
    fexists(basedefault + 'db\db_books.txt') and
    fexists(basedefault + 'db\db_boxes.txt') and
    fexists(basedefault + 'db\db_catalogs.txt') and
    fexists(basedefault + 'db\db_categories.txt') and
    fexists(basedefault + 'db\db_codes.txt') and
    fexists(basedefault + 'db\db_colors.txt') and
    fexists(basedefault + 'db\db_crawlerlinks.txt') and
    fexists(basedefault + 'db\db_cross_stats.txt') and
    fexists(basedefault + 'db\db_currency.txt') and
    fexists(basedefault + 'db\db_currencyconvert.txt') and
    fexists(basedefault + 'db\db_gears.txt') and
    fexists(basedefault + 'db\db_instructions.txt') and
    fexists(basedefault + 'db\db_knownpieces.txt') and
    fexists(basedefault + 'db\db_lddcolors.txt') and
    fexists(basedefault + 'db\db_lddmaterials.txt') and
    fexists(basedefault + 'db\db_lddprimitives.txt') and
    fexists(basedefault + 'db\db_newnames.txt') and
    fexists(basedefault + 'db\db_parttypes.txt') and
    fexists(basedefault + 'db\db_pieces.txt') and
    fexists(basedefault + 'db\db_pieces_alias.txt') and
    fexists(basedefault + 'db\db_pieces_bl.txt') and
    fexists(basedefault + 'db\db_pieces_categories.txt') and
    fexists(basedefault + 'db\db_pieces_inventories.txt') and
    fexists(basedefault + 'db\db_pieces_weight.txt') and
    fexists(basedefault + 'db\db_pieces_years.txt') and
    fexists(basedefault + 'db\db_relationships.txt') and
    fexists(basedefault + 'db\db_sets.txt') and
    fexists(basedefault + 'db\db_set_assets.txt') and
    fexists(basedefault + 'db\db_set_pieces.txt') and
    fexists(basedefault + 'db\parts.db') and
    fexists(basedefault + 'db\sets1.db') and
    fexists(basedefault + 'db\sets2.db') and
    fexists(basedefault + 'db\sets3.db');
end;

function InstallDataBase: boolean;
var
  f: TInstallationForm;
begin
  Result := False;
  f := TInstallationForm.Create(nil);
  try
    f.ShowModal;
    if (f.ModalResult = mrOK) and CheckInstallation then
      Result := True;
  finally
    f.Free;
  end;
end;

var
  bColors: array[0..NUMCOLORS - 1] of LongWord;
  installation_progress: double = 0.0;
  install_tot, install_cur: integer;
  installstage: integer = 0;

procedure _install_thr;
var
  wad: TWADReader;
  fname: string;
  i: integer;
begin
  fname := basedefault + 'database.dat';
  if not fexists(fname) then
    Exit;

  ThreadSet(install_tot, 1);
  ThreadSet(install_cur, 0);

  wad := TWADReader.Create;
  wad.OpenWadFile(fname);

  for i := 0 to wad.NumEntries - 1 do
    ThreadSet(install_tot, install_tot + wad.EntryInfo(i).size);

  for i := 0 to wad.NumEntries - 1 do
  begin
    wad.SaveEntry(i, basedefault + wad.EntryName(i));
    ThreadSet(install_cur, install_cur + wad.EntryInfo(i).size);
  end;

  wad.Free;

  ThreadSet(installstage, 2);
end;

procedure TInstallationForm.Draw1x1(const x, y: integer; const color: LongWord; const border: boolean);
var
  rect: TRect;
  r, g, b: integer;
  C: TCanvas;
  dxy: integer;
begin
  C := Image1.Picture.Bitmap.Canvas;
  C.Brush.Style := bsSolid;
  C.Brush.Color := color;
  if border then
  begin
    C.Pen.Style := psSolid;
    C.Pen.Color := RGBInvert(color);
  end
  else
  begin
    C.Pen.Style := psSolid;
    C.Pen.Color := color;
  end;

  rect.Left := x * STUDSIZE;
  rect.Right := (x + 1) * STUDSIZE;
  rect.Top := y * STUDSIZE;
  rect.Bottom := (y + 1) * STUDSIZE;
  C.Rectangle(rect);

  dxy := STUDSIZE div 5;
  rect.Left := rect.Left + dxy;
  rect.Right := rect.Right - dxy;
  rect.Top := rect.Top + dxy;
  rect.Bottom := rect.Bottom - dxy;

  r := 255 - (255 - GetRValue(color)) div 2;
  if r > 200 then
    r := r div 2;
  g := 255 - (255 - GetGValue(color)) div 2;
  if g > 200 then
    g := g div 2;
  b := 255 - (255 - GetBValue(color)) div 2;
  if b > 200 then
    b := b div 2;

  C.Pen.Style := psSolid;
  C.Pen.Color := RGB(r, g, b);

  C.Ellipse(rect);
end;

procedure TInstallationForm.DrawBasePlate;
var
  i, j: integer;
begin
  for i := 0 to iwidth - 1 do
    for j := 0 to iheight - 1 do
      Draw1x1(i, j, bColors[0], False);
end;

procedure TInstallationForm.DrawRandomBrick;
var
  x, y: integer;
  c: LongWord;
begin
  x := Random(iwidth);
  y := Random(iheight);
  c := bColors[Random(NUMCOLORS)];
  Draw1x1(x, y, c, True);
end;

procedure TInstallationForm.FormCreate(Sender: TObject);
begin
  Randomize;

  installstage := 0;

  task_id := -1;

  Image1.Picture.Bitmap.Width := Image1.Width;
  Image1.Picture.Bitmap.Height := Image1.Height;
  Image1.Picture.Bitmap.PixelFormat := pf32bit;

  iwidth := (Image1.Width + STUDSIZE) div STUDSIZE;
  iheight := (Image1.Height + STUDSIZE) div STUDSIZE;

  bColors[0] := RGB(35, 120, 65);
  bColors[1] := RGB(5, 19, 29);
  bColors[2] := RGB(0, 85, 191);
  bColors[3] := RGB(0, 143, 155);
  bColors[4] := RGB(201, 26, 9);
  bColors[5] := RGB(200, 112, 160);
  bColors[6] := RGB(155, 161, 157);
  bColors[7] := RGB(109, 110, 92);
  bColors[8] := RGB(180, 210, 227);
  bColors[9] := RGB(228, 205, 158);
  bColors[10] := RGB(129, 0, 123);
  bColors[11] := RGB(32, 50, 176);
  bColors[12] := RGB(254, 138, 24);
  bColors[13] := RGB(187, 233, 11);
  bColors[14] := RGB(172, 120, 186);
  bColors[15] := RGB(225, 213, 237);
  bColors[16] := RGB(88, 42, 18);
  bColors[17] := RGB(160, 165, 169);
  bColors[18] := RGB(108, 110, 104);
  bColors[19] := RGB(90, 147, 219);
  bColors[20] := RGB(115, 220, 161);
  bColors[21] := RGB(137, 155, 95);
  bColors[22] := RGB(204, 112, 42);
  bColors[23] := RGB(248, 187, 61);
  bColors[24] := RGB(179, 16, 4);
  bColors[25] := RGB(225, 240, 58);
  bColors[26] := RGB(10, 52, 99);
  bColors[27] := RGB(24, 70, 50);
  bColors[28] := RGB(53, 33, 0);
  bColors[29] := RGB(114, 14, 15);
  bColors[30] := RGB(7, 139, 201);
  bColors[31] := RGB(54, 174, 91);
  bColors[32] := RGB(155, 154, 90);
  bColors[33] := RGB(169, 85, 0);
  bColors[34] := RGB(160, 188, 172);
  bColors[35] := RGB(96, 116, 161);
  bColors[36] := RGB(142, 205, 55);
  bColors[37] := RGB(255, 255, 255);

  DrawBasePlate;
end;

procedure TInstallationForm.StartButtonClick(Sender: TObject);
begin
  Timer1.Enabled := True;

  installstage := 1;
  installation_progress := 0.0;

  StartButton.Visible := False;
  CancelButton.Enabled := False;

  task_id := MT_ExecuteTask(_install_thr);

  ShowSplash;
  SplashProgress(SPLASH_TIT, 0.0);
end;

var
  cc: integer = 1;

procedure TInstallationForm.Timer1Timer(Sender: TObject);
var
  i: integer;
  cap: string;
begin
  DrawRandomBrick;

  if cc = 3 then
    cc := 0
  else
    inc(cc);

  cap := '';
  for i := 0 to cc - 1 do
    cap := cap + '.';

  CancelButton.Caption := cap;

  installation_progress := dbl_safe_div(install_cur, install_tot);

  SplashProgress(SPLASH_TIT, installation_progress);

  if installstage = 2 then
  begin
    MT_WaitTask(task_id);
    CancelButton.ModalResult := MrOK;
    CancelButton.Enabled := True;
    CancelButton.Caption := 'Continue';
    HideSplash;
    Timer1.Enabled := False;
    if CheckInstallation then
      ShowMessage('Installation Complete!')
    else
      ShowMessage('Installation Failed! Make sure you that database.dat file exists and there is enough free space on your hard drive.');
  end;

end;

end.
