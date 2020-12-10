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
//    Import PDF instructions
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit frm_pdfinstructions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, jpeg, bi_pdf2jpeg, Buttons, CheckLst;

type
  TImportPDFForm = class(TForm)
    ToolbarPanel1: TPanel;
    OpenButton1: TButton;
    AppendButton1: TButton;
    DonwloadButton1: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Button3: TButton;
    Button4: TButton;
    ScrollBox1: TScrollBox;
    PaintBox1: TPaintBox;
    PrevButton1: TButton;
    NextButton1: TButton;
    ExportButton1: TButton;
    PageLabel: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    RotateLeftButton1: TSpeedButton;
    RotateRightButton1: TSpeedButton;
    Timer1: TTimer;
    InfoLabel: TLabel;
    Panel3: TPanel;
    Panel4: TPanel;
    CheckListBox1: TCheckListBox;
    Splitter1: TSplitter;
    Panel5: TPanel;
    CopyButton1: TButton;
    procedure OpenButton1Click(Sender: TObject);
    procedure AppendButton1Click(Sender: TObject);
    procedure DonwloadButton1Click(Sender: TObject);
    procedure ExportButton1Click(Sender: TObject);
    procedure PrevButton1Click(Sender: TObject);
    procedure NextButton1Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RotateLeftButton1Click(Sender: TObject);
    procedure RotateRightButton1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CopyButton1Click(Sender: TObject);
  private
    { Private declarations }
    oldidle: TIdleEvent;
    procedure IdleEventHandler(Sender: TObject; var Done: Boolean);
    procedure UpdateControls;
    procedure InfoMessage(const s: string; const warn: boolean);
  public
    pdf2jpeg: TPDF2Jpeg;
    globalid: integer;
    sset: string;
    didsavepdf: boolean;
    { Public declarations }
  end;

var
  ImportPDFForm: TImportPDFForm;

function UpdateInstructionsFromPdf(const pcs: string): boolean;

implementation

{$R *.dfm}

uses
  bi_delphi, bi_db, bi_crawler, bi_imagerotate;

function UpdateInstructionsFromPdf(const pcs: string): boolean;
var
  f: TImportPDFForm;
begin
  Result := False;
  f := TImportPDFForm.Create(nil);
  try
    f.sset := pcs;
    f.Caption := 'Import PDF Instructions - ' + pcs;
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      Result := f.pdf2jpeg.Count > 0;
      if Result then
      begin
        Screen.Cursor := crHourglass;
        try
          f.pdf2jpeg.SaveImagesToDir(basedefault + '9997\' + pcs + '\');
        finally
          Screen.Cursor := crDefault;
        end;
      end;
      Result := Result or f.didsavepdf;
    end
    else if f.didsavepdf then
      Result := True;
  finally
    f.Free;
  end;
end;

procedure TImportPDFForm.OpenButton1Click(Sender: TObject);
var
  i, idx: integer;
  lst: TStringList;
  cnt: integer;
  ts: TString;
begin
  if OpenDialog1.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      cnt := 0;
      for i := 0 to CheckListBox1.Items.Count - 1 do
        CheckListBox1.Items.Objects[i].Free;
      CheckListBox1.Items.Clear;
      lst := TStringList.Create;
      lst.AddStrings(OpenDialog1.Files);
      lst.Sort;
      pdf2jpeg.Clear;
      globalid := 0;
      for i := 0 to lst.Count - 1 do
      begin
        cnt := cnt + pdf2jpeg.AppendPDFFromFile(lst.Strings[i]);
        idx := CheckListBox1.Items.IndexOf(ExtractFileName(lst.Strings[i]));
        if idx < 0 then
        begin
          ts := TString.Create;
          ts.text := lst.Strings[i];
          idx := CheckListBox1.Items.AddObject(ExtractFileName(lst.Strings[i]), ts);
        end;
        if idx >= 0 then
          CheckListBox1.Checked[idx] := True;
      end;
      lst.Free;
    finally
      Screen.Cursor := crDefault;
    end;
    if cnt = 0 then
      InfoMessage('No images were retrieved', True)
    else
      InfoMessage(IntToStr(cnt) + ' images were retrieved', False);
    PaintBox1.Invalidate;
    UpdateControls;
  end;
end;

procedure TImportPDFForm.AppendButton1Click(Sender: TObject);
var
  i, idx: integer;
  lst: TStringList;
  cnt: integer;
  ts: TString;
begin
  if OpenDialog1.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      cnt := 0;
      lst := TStringList.Create;
      lst.AddStrings(OpenDialog1.Files);
      lst.Sort;
      globalid := pdf2jpeg.Count - 1;
      for i := 0 to lst.Count - 1 do
      begin
        cnt := cnt + pdf2jpeg.AppendPDFFromFile(lst.Strings[i]);
        idx := CheckListBox1.Items.IndexOf(ExtractFileName(lst.Strings[i]));
        if idx < 0 then
        begin
          ts := TString.Create;
          ts.text := lst.Strings[i];
          idx := CheckListBox1.Items.AddObject(ExtractFileName(lst.Strings[i]), ts);
        end;
        if idx >= 0 then
          CheckListBox1.Checked[idx] := True;
      end;
      lst.Free;
    finally
      Screen.Cursor := crDefault;
    end;
    if cnt = 0 then
      InfoMessage('No images were retrieved', True)
    else
      InfoMessage(IntToStr(cnt) + ' images were retrieved', False);
    PaintBox1.Invalidate;
    UpdateControls;
  end;
end;

procedure TImportPDFForm.DonwloadButton1Click(Sender: TObject);
var
  data: string;
  aa: integer;
  link: string;
  s1, s2: string;
  p1, p2: integer;
  cnt1, cnt2: integer;
  i, idx: integer;
  ts: TString;
begin
  Screen.Cursor := crHourglass;
  try
    splitstring(Trim(sset), s1, s2, '-');
    if Trim(s1) = '' then
      Exit;
    if Trim(s2) <> '1' then
      Exit;
    pdf2jpeg.Clear;
    aa := 0;
    cnt1 := 0;
    cnt2 := 0;
    for i := 0 to CheckListBox1.Items.Count - 1 do
      CheckListBox1.Items.Objects[i].Free;
    CheckListBox1.Items.Clear;
    repeat
      inc(aa);
      link := 'http://lego.brickinstructions.com/pdfdrop/' + Trim(s1) + '_' + IntToStr(aa) + '.pdf';
      data := NET_GetURLString(link);
      p1 := Pos('stream', data);
      p2 := Pos('endstream', data);
      if (p1 > 0) and (p2 > p1) then
      begin
        if not DirectoryExists(basedefault + '9997\' + Trim(sset)) then
          ForceDirectories(basedefault + '9997\' + Trim(sset));
        SaveStringToFile(basedefault + '9997\' + Trim(sset) + '\' + Trim(s1) + '_' + IntToStr(aa) + '.pdf', data);
        didsavepdf := True;
        idx := CheckListBox1.Items.IndexOf(Trim(s1) + '_' + IntToStr(aa) + '.pdf');
        if idx < 0 then
        begin
          ts := TString.Create;
          ts.text := basedefault + '9997\' + Trim(sset) + '\' + Trim(s1) + '_' + IntToStr(aa) + '.pdf';
          idx := CheckListBox1.Items.AddObject(Trim(s1) + '_' + IntToStr(aa) + '.pdf', ts);
        end;
        if idx >= 0 then
          CheckListBox1.Checked[idx] := True;
        inc(cnt2);
      end
      else
        Break;
      cnt1 := cnt1 + pdf2jpeg.AppendPDFFromData(data);
    until aa = 6;
    if cnt1 + cnt2 = 0 then
      InfoMessage('No images were retrieved', True)
    else
      InfoMessage(IntToStr(cnt1) + ' images and ' + IntToStr(cnt2) + ' pdf files were retrieved', False);
    PaintBox1.Invalidate;
    UpdateControls;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TImportPDFForm.ExportButton1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      pdf2jpeg.SaveImagesToZip(SaveDialog1.FileName);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TImportPDFForm.PrevButton1Click(Sender: TObject);
begin
  if globalid > 0 then
  begin
    dec(globalid);
    PaintBox1.Invalidate;
    UpdateControls;
  end;
end;

procedure TImportPDFForm.NextButton1Click(Sender: TObject);
begin
  if globalid < pdf2jpeg.Count - 1 then
  begin
    inc(globalid);
    PaintBox1.Invalidate;
    UpdateControls;
  end;
end;

procedure TImportPDFForm.PaintBox1Paint(Sender: TObject);
var
  jp: TJPEGImage;
begin
  if (globalid < 0) or (globalid >= pdf2jpeg.Count) or (pdf2jpeg.Count = 0) then
  begin
    PaintBox1.Width := 1;
    PaintBox1.Height := 1;
    PaintBox1.Canvas.Pen.Color := clWhite;
    PaintBox1.Canvas.Pen.Style := psSolid;
    PaintBox1.Canvas.Brush.Color := clWhite;
    PaintBox1.Canvas.Brush.Style := bsSolid;
    PaintBox1.Canvas.Rectangle(0, 0, 1, 1);
    Exit;
  end;
  jp := pdf2jpeg.JPEG(globalid);
  PaintBox1.Width := jp.Width;
  PaintBox1.Height := jp.Height;
  PaintBox1.Canvas.Draw(0, 0, jp);
end;

procedure TImportPDFForm.FormCreate(Sender: TObject);
begin
  pdf2jpeg := TPDF2Jpeg.Create;
  globalid := 0;
  sset := '';
  oldidle := Application.OnIdle;
  Application.OnIdle := IdleEventHandler;
  didsavepdf := False;
end;

procedure TImportPDFForm.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  pdf2jpeg.Free;
  Application.OnIdle := oldidle;
  for i := 0 to CheckListBox1.Items.Count - 1 do
    CheckListBox1.Items.Objects[i].Free;
  CheckListBox1.Items.Clear;
end;

procedure TImportPDFForm.IdleEventHandler(Sender: TObject; var Done: Boolean);
begin
  UpdateControls;
end;

procedure TImportPDFForm.UpdateControls;
var
  s1, s2: string;
  i: integer;
begin
  DonwloadButton1.Enabled := Trim(sset) <> '';
  splitstring(sset, s1, s2, '-');
  if Trim(s1) = '' then
    DonwloadButton1.Enabled := False;
  if Trim(s2) <> '1' then
    DonwloadButton1.Enabled := False;
  PrevButton1.Enabled := globalid > 0;
  NextButton1.Enabled := globalid < pdf2jpeg.Count - 1;
  if pdf2jpeg.Count = 0 then
  begin
    PageLabel.Caption := '0/0';
    ExportButton1.Enabled := False;
  end
  else
  begin
    PageLabel.Caption := Format('%d/%d', [globalid + 1, pdf2jpeg.Count]);
    ExportButton1.Enabled := True;
  end;
  CopyButton1.Enabled := False;
  for i := 0 to CheckListBox1.Items.Count - 1 do
    if CheckListBox1.ItemEnabled[i] then
    begin
      CopyButton1.Enabled := True;
      Break;
    end;
end;

procedure TImportPDFForm.InfoMessage(const s: string; const warn: boolean);
begin
  if warn then
  begin
    InfoLabel.Font.Color := clRed;
    InfoLabel.Color := RGB(255, 255, 200);
  end
  else
  begin
    InfoLabel.Font.Color := clBlack;
    InfoLabel.Color := RGB(255, 255, 192);
  end;
  InfoLabel.Caption := s;
end;

procedure TImportPDFForm.RotateLeftButton1Click(Sender: TObject);
var
  i: integer;
begin
  Screen.Cursor := crHourglass;
  try
    for i := 0 to pdf2jpeg.Count - 1 do
      RotateJPEG90DegreesCounterClockwise(pdf2jpeg.JPEG(i));
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TImportPDFForm.RotateRightButton1Click(Sender: TObject);
var
  i: integer;
begin
  Screen.Cursor := crHourglass;
  try
    for i := 0 to pdf2jpeg.Count - 1 do
      RotateJPEG90DegreesClockwise(pdf2jpeg.JPEG(i));
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TImportPDFForm.Timer1Timer(Sender: TObject);
begin
  UpdateControls;
end;

procedure TImportPDFForm.CopyButton1Click(Sender: TObject);
var
  i: integer;
  cnt: integer;
  sname, dname: string;
begin
  Screen.Cursor := crHourglass;
  try
    cnt := 0;
    if not DirectoryExists(basedefault + '9997\' + Trim(sset)) then
      ForceDirectories(basedefault + '9997\' + Trim(sset));
    for i := 0 to CheckListBox1.Items.Count - 1 do
      if CheckListBox1.ItemEnabled[i] then
      begin
        sname := (CheckListBox1.Items.Objects[i] as TString).text;
        dname := basedefault + '9997\' + Trim(sset) + '\' + CheckListBox1.Items.Strings[i];
        if UpperCase(sname) <> UpperCase(dname) then
          if CopyFile(sname, dname) then
            inc(cnt);
      end;
    if cnt = 0 then
      InfoMessage('No pdf files were copied', True)
    else
    begin
      InfoMessage(IntToStr(cnt) + ' pdf files were copied', False);
      didsavepdf := True;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
