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
//    Mosaic Form (bricks)
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

{$J+}
unit mosaicfrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, jpeg, ExtDlgs, ComCtrls, StdCtrls, ExtCtrls, bi_delphi, bi_db,
  CheckLst, Buttons, mosaic_base;

const
  NUMBRICKS = 15;

type
  TBrickMosaic = class
  private
    colorindexes: array[0..NUMCOLORINDEXES - 1] of mosaiccolorindex_t;
    bricks: array[0..NUMBRICKS - 1] of mosaicbrick_t;
    fwidth, fheight: integer;
    fmosaic: mosaic_p;
    fbitmap: TBitmap;
    fplanebitmap: TBitmap;
    fcolormode: integer;
  protected
    procedure nearestcolorRGB(const c: LongWord; var ret: TColor; var color: integer);
    procedure nearestcolorYUV(const c: LongWord; var ret: TColor; var color: integer);
    procedure CheckMosaicItemPass1(const x, y: integer);
    procedure CheckMosaicItemPass2(const x, y: integer);
    procedure BitmapDraw(const x, y, x2, y2: Integer; const color: Integer);
  public
    inv: TBrickInventory;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure BitmapColorTransform(const bmp: TBitmap);
    property Bitmap: TBitmap read fbitmap;
    property PlaneBitmap: TBitmap read fplanebitmap;
    property width: Integer read fwidth;
    property height: integer read fheight;
    property colormode: Integer read fcolormode write fcolormode;
  end;

type
  TMosaicForm = class(TForm)
    Panel1: TPanel;
    OpenPictureDialog1: TOpenPictureDialog;
    Edit1: TEdit;
    Button1: TButton;
    Label3: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    Button2: TButton;
    Button3: TButton;
    Panel4: TPanel;
    Panel5: TPanel;
    CheckListBox1: TCheckListBox;
    Panel6: TPanel;
    Panel7: TPanel;
    CheckListBox2: TCheckListBox;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel10: TPanel;
    Label4: TLabel;
    Label5: TLabel;
    EditCostNew: TEdit;
    EditCostUsed: TEdit;
    Panel11: TPanel;
    ScrollBox1: TScrollBox;
    Panel12: TPanel;
    Panel13: TPanel;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    TrackBar2: TTrackBar;
    Panel14: TPanel;
    Panel15: TPanel;
    RadioGroup1: TRadioGroup;
    Image1: TImage;
    Image2: TImage;
    Panel16: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckListBox1Click(Sender: TObject);
    procedure CheckListBox2Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Image2Resize(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Panel13DblClick(Sender: TObject);
  private
    { Private declarations }
    basedir: string;
    pic: TPicture;
    updatelocked: Boolean;
    function MosaicColorIndexFromName(const name: string): integer;
    procedure MyUpdate;
  public
    { Public declarations }
    mosaic: TBrickMosaic;
    function name: string;
  end;

implementation

{$R *.dfm}

uses
  clipbrd, bi_utils, bi_tmp, bi_globals, bi_currency;

const
  C_bricks: array[0..NUMBRICKS - 1] of mosaicbrick_t = (
    (brick: '3001'; x: 2; y: 4;  use: true),
    (brick: '3002'; x: 2; y: 3;  use: true),
    (brick: '3003'; x: 2; y: 2;  use: true),
    (brick: '3004'; x: 1; y: 2;  use: true),
    (brick: '3005'; x: 1; y: 1;  use: true),
    (brick: '3006'; x: 2; y: 10; use: true),
    (brick: '3007'; x: 2; y: 8;  use: true),
    (brick: '3008'; x: 1; y: 8;  use: true),
    (brick: '3009'; x: 1; y: 6;  use: true),
    (brick: '3010'; x: 1; y: 4;  use: true),
    (brick: '3622'; x: 1; y: 3;  use: true),
    (brick: '6111'; x: 1; y: 10; use: true),
    (brick: '6112'; x: 1; y: 12; use: true),
    (brick: '2465'; x: 1; y: 16; use: true),
    (brick: '2456'; x: 2; y: 6;  use: true)
  );

function SortListByPrice_Brick(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := SortListByPrice(@C_bricks, NUMBRICKS, List, Index1, Index2);
end;

constructor TBrickMosaic.Create;
var
  i, j: integer;
  pci: TPieceColorInfo;
  price_per_stud: Double;
begin
  inherited;

  fcolormode := 0;

  fbitmap := TBitmap.Create;
  fplanebitmap := TBitmap.Create;

  inv := TBrickInventory.Create;

  fwidth := 0;
  fheight := 0;
  GetMem(fmosaic, SizeOf(mosaic_t));

  for i := 0 to NUMBRICKS - 1 do
  begin
    bricks[i] := C_bricks[i];
    bricks[i].colors := TDNumberList.Create;
  end;
  for i := 0 to NUMCOLORINDEXES - 1 do
  begin
    colorindexes[i] := C_colorindexes[i];
    colorindexes[i].pieces := TStringList.Create;
    pci := db.PieceColorInfo('3005', colorindexes[i].idx);
    if pci <> nil then
    begin
      colorindexes[i].pieces.AddObject('3005', pci);
{      if pci.EvaluatePriceUsed > 0.10 then
        colorindexes[i].idx := -1;}
    end
    else
      colorindexes[i].idx := -1;
  end;
  for i := 0 to NUMCOLORINDEXES - 1 do
    if colorindexes[i].idx <> -1 then
    begin
      price_per_stud := (colorindexes[i].pieces.Objects[0] as TPieceColorInfo).EvaluatePriceUsed;
      for j := 0 to NUMBRICKS - 1 do
        if colorindexes[i].pieces.IndexOf(bricks[j].brick) = -1 then
        begin
          pci := db.PieceColorInfo(bricks[j].brick, colorindexes[i].idx);
          if pci <> nil then
            if pci.EvaluatePriceUsed <= bricks[j].x *  bricks[j].y * price_per_stud then
              colorindexes[i].pieces.AddObject(bricks[j].brick, pci);
        end;
      colorindexes[i].pieces.CustomSort(SortListByPrice_Brick);
    end;
end;

destructor TBrickMosaic.Destroy;
var
  i: integer;
begin
  for i := 0 to NUMBRICKS - 1 do
    bricks[i].colors.Free;
  for i := 0 to NUMCOLORINDEXES - 1 do
    colorindexes[i].pieces.Free;
  FreeMem(fmosaic, SizeOf(mosaic_t));
  inv.Free;
  fbitmap.Free;
  fplanebitmap.Free;
end;

procedure TBrickMosaic.nearestcolorRGB(const c: LongWord; var ret: TColor; var color: integer);
var
  r, g, b: integer;
  rc, gc, bc: integer;
  cr, cg, cb: integer;
  i: integer;
  cc: LongWord;
  dist: double;
  mindist: double;
begin
  r := c and $FF;
  g := (c shr 8) and $FF;
  b := (c shr 16) and $FF;
  mindist := $ffffffff;
  ret := 0;
  color := 0;
  for i := 0 to NUMCOLORINDEXES - 1 do
    if colorindexes[i].idx >= 0 then
    begin
      cc := RGBInvert(db.colors(colorindexes[i].idx).RGB);
      rc := cc and $FF;
      gc := (cc shr 8) and $FF;
      bc := (cc shr 16) and $FF;
      cr := r - rc;
      cg := g - gc;
      cb := b - bc;
      dist := 0.30 * cr * cr + 0.59 * cg * cg + 0.11 * cb * cb;
      if dist < mindist then
      begin
        ret := cc;
        color := i;
        mindist := dist;
      end;
    end;
end;

procedure TBrickMosaic.nearestcolorYUV(const c: LongWord; var ret: TColor; var color: integer);
var
  A: TYUV;
  i: integer;
  cc: TYUV;
  dist: double;
  mindist: double;
begin
  A := RGB2YUV(c);
  mindist := $ffffffff;
  ret := 0;
  color := 0;
  for i := 0 to NUMCOLORINDEXES - 1 do
    if colorindexes[i].idx >= 0 then
    begin
      cc := RGB2YUV(RGBInvert(db.colors(colorindexes[i].idx).RGB));
      dist := sqr(A.Y - cc.Y) + sqr(A.U - cc.U) + sqr(A.V - cc.V);
      if dist < mindist then
      begin
        ret := RGBInvert(db.colors(colorindexes[i].idx).RGB);
        color := i;
        mindist := dist;
      end;
    end;
end;

procedure TBrickMosaic.BitmapDraw(const x, y, x2, y2: Integer; const color: Integer);
var
  bx1, bx2, by1, by2: integer;
  cinfo: colorinfo_p;
  r, g, b: integer;
  i, j: integer;
  dxy: Integer;
begin
  bx1 := x * MOSAICBITMAPZOOM;
  by1 := y * MOSAICBITMAPZOOM;
  bx2 := x2 * MOSAICBITMAPZOOM;
  by2 := y2 * MOSAICBITMAPZOOM;
  cinfo := db.Colors(colorindexes[color].idx);
  if cinfo.id = 0 then
    fbitmap.Canvas.Pen.Color := RGB(128, 128, 128)
  else
    fbitmap.Canvas.Pen.Color := RGB(0, 0, 0);
  fbitmap.Canvas.Pen.Style := psSolid;

  fbitmap.Canvas.Brush.Color := RGBInvert(cinfo.RGB);
  fbitmap.Canvas.Brush.Style := bsSolid;
  fbitmap.Canvas.Rectangle(bx1, by1, bx2, by2);

  fplanebitmap.Canvas.Pen.Style := psClear;
  fplanebitmap.Canvas.Brush.Color := RGBInvert(cinfo.RGB);
  fplanebitmap.Canvas.Brush.Style := bsSolid;
  fplanebitmap.Canvas.Rectangle(bx1, by1, bx2, by2);

  r := 255 - (255 - GetRValue(RGBInvert(cinfo.RGB))) div 2;
  if r > 200 then
    r := r div 2;
  g := 255 - (255 - GetGValue(RGBInvert(cinfo.RGB))) div 2;
  if g > 200 then
    g := g div 2;
  b := 255 - (255 - GetBValue(RGBInvert(cinfo.RGB))) div 2;
  if b > 200 then
    b := b div 2;
  fbitmap.Canvas.Pen.Color := RGB(r, g, b);
  dxy := MOSAICBITMAPZOOM div 5;
  for i := x to x2 - 1 do
    for j := y to y2 - 1 do
      fbitmap.Canvas.Ellipse(
        i * MOSAICBITMAPZOOM + dxy,
        j * MOSAICBITMAPZOOM + dxy,
        (i + 1) * MOSAICBITMAPZOOM - dxy,
        (j + 1) * MOSAICBITMAPZOOM - dxy);
end;

procedure TBrickMosaic.CheckMosaicItemPass1(const x, y: integer);
var
  i, j, k: integer;
  dx, dy: integer;
  x2, y2: integer;
  c: integer;
  pcs: TStringList;
  ok: Boolean;
  piece: string;
  idx: integer;
begin
  c := fmosaic[x, y].coloridx;
  pcs := colorindexes[c].pieces;
  if pcs.Count = 0 then
    Exit;
  for k := 0 to 0 do
  begin
    piece := (pcs.Objects[k] as TPieceColorInfo).piece;
    idx := brickindex(@C_bricks, NUMBRICKS, piece);
    if idx < 0 then
      Continue;
    if not bricks[idx].use then
      Continue;

    dx := brickdimentionx(@C_bricks, NUMBRICKS, piece);
    dy := brickdimentiony(@C_bricks, NUMBRICKS, piece);

    x2 := x + dx;
    y2 := y + dy;
    if x2 <= MAXDIMENTION then
      if y2 <= MAXDIMENTION then
      begin
        ok := true;
        for i := x to x2 - 1 do
        begin
          for j := y to y2 - 1 do
          begin
            if fmosaic[i, j].checked then
            begin
              ok := false;
              break;
            end;
            if fmosaic[i, j].coloridx <> c then
            begin
              ok := false;
              break;
            end;
          end;
          if not ok then
            break;
        end;
        if ok then
        begin
          BitmapDraw(x, y, x2, y2, c);
          inv.AddLoosePart((pcs.Objects[k] as TPieceColorInfo).piece, db.Colors(colorindexes[c].idx).id, 1);
          for i := x to x2 - 1 do
            for j := y to y2 - 1 do
              fmosaic[i, j].checked := true;
        end;
      end;

    x2 := x + dy;
    y2 := y + dx;
    if x2 <= MAXDIMENTION then
      if y2 <= MAXDIMENTION then
      begin
        ok := true;
        for i := x to x2 - 1 do
        begin
          for j := y to y2 - 1 do
          begin
            if fmosaic[i, j].checked then
            begin
              ok := false;
              break;
            end;
            if fmosaic[i, j].coloridx <> c then
            begin
              ok := false;
              break;
            end;
          end;
          if not ok then
            break;
        end;
        if ok then
        begin
          BitmapDraw(x, y, x2, y2, c);
          inv.AddLoosePart((pcs.Objects[k] as TPieceColorInfo).piece, db.Colors(colorindexes[c].idx).id, 1);
          for i := x to x2 - 1 do
            for j := y to y2 - 1 do
              fmosaic[i, j].checked := true;
        end;
      end;

  end;

end;

procedure TBrickMosaic.CheckMosaicItemPass2(const x, y: integer);
var
  i, j, k: integer;
  dx, dy: integer;
  x2, y2: integer;
  c: integer;
  pcs: TStringList;
  ok: Boolean;
  piece: string;
  idx: integer;
begin
  c := fmosaic[x, y].coloridx;
  pcs := colorindexes[c].pieces;
  for k := 0 to pcs.Count - 1 do
  begin
    piece := (pcs.Objects[k] as TPieceColorInfo).piece;
    idx := brickindex(@C_bricks, NUMBRICKS, piece);
    if idx < 0 then
      Continue;
    if not bricks[idx].use then
      Continue;

    dx := brickdimentionx(@C_bricks, NUMBRICKS, piece);
    dy := brickdimentiony(@C_bricks, NUMBRICKS, piece);

    x2 := x + dx;
    y2 := y + dy;
    if x2 <= MAXDIMENTION then
      if y2 <= MAXDIMENTION then
      begin
        ok := true;
        for i := x to x2 - 1 do
        begin
          for j := y to y2 - 1 do
          begin
            if fmosaic[i, j].checked then
            begin
              ok := false;
              break;
            end;
            if fmosaic[i, j].coloridx <> c then
            begin
              ok := false;
              break;
            end;
          end;
          if not ok then
            break;
        end;
        if ok then
        begin
          BitmapDraw(x, y, x2, y2, c);
          inv.AddLoosePart((pcs.Objects[k] as TPieceColorInfo).piece, db.Colors(colorindexes[c].idx).id, 1);
          for i := x to x2 - 1 do
            for j := y to y2 - 1 do
              fmosaic[i, j].checked := true;
        end;
      end;

    x2 := x + dy;
    y2 := y + dx;
    if x2 <= MAXDIMENTION then
      if y2 <= MAXDIMENTION then
      begin
        ok := true;
        for i := x to x2 - 1 do
        begin
          for j := y to y2 - 1 do
          begin
            if fmosaic[i, j].checked then
            begin
              ok := false;
              break;
            end;
            if fmosaic[i, j].coloridx <> c then
            begin
              ok := false;
              break;
            end;
          end;
          if not ok then
            break;
        end;
        if ok then
        begin
          BitmapDraw(x, y, x2, y2, c);
          inv.AddLoosePart((pcs.Objects[k] as TPieceColorInfo).piece, db.Colors(colorindexes[c].idx).id, 1);
          for i := x to x2 - 1 do
            for j := y to y2 - 1 do
              fmosaic[i, j].checked := true;
        end;
      end;

  end;

end;

procedure TBrickMosaic.BitmapColorTransform(const bmp: TBitmap);
var
  x, y: integer;
  col: TColor;
begin
  fwidth := bmp.Width;
  if fwidth > MAXDIMENTION then
    fwidth := MAXDIMENTION;
  fheight := bmp.Height;
  if fheight > MAXDIMENTION then
    fheight := MAXDIMENTION;
  fbitmap.Width := fwidth * MOSAICBITMAPZOOM;
  fbitmap.Height := fheight * MOSAICBITMAPZOOM;
  fplanebitmap.Width := fwidth * MOSAICBITMAPZOOM;
  fplanebitmap.Height := fheight * MOSAICBITMAPZOOM;
  BitmapDraw(0, 0, fwidth, fheight, 0);
  for x := 0 to fwidth - 1 do
    for y := 0 to fheight - 1 do
    begin
      if fcolormode = 0 then
        nearestcolorRGB(bmp.Canvas.Pixels[x, y], col, fmosaic[x, y].coloridx)
      else
        nearestcolorYUV(bmp.Canvas.Pixels[x, y], col, fmosaic[x, y].coloridx);
      bmp.Canvas.Pixels[x, y] := col;
    end;

  for x := 0 to fwidth - 1 do
  begin
    for y := 0 to fheight - 1 do
      fmosaic[x, y].checked := false;
    for y := fheight to MAXDIMENTION - 1 do
      fmosaic[x, y].checked := true;
  end;
  for x := fwidth to MAXDIMENTION - 1 do
  begin
    for y := 0 to MAXDIMENTION - 1 do
      fmosaic[x, y].checked := true;
  end;

  inv.Clear;
  for x := 0 to fwidth - 1 do
    for y := 0 to fheight - 1 do
      if not fmosaic[x, y].checked then
        CheckMosaicItemPass1(x, y);
  for x := 0 to fwidth - 1 do
    for y := 0 to fheight - 1 do
      if not fmosaic[x, y].checked then
        CheckMosaicItemPass2(x, y);
end;

////////////////////////////////////////////////////////////////////////////////
function TMosaicForm.MosaicColorIndexFromName(const name: string): integer;
var
  i: integer;
begin
  for i := 0 to NUMCOLORINDEXES - 1 do
    if C_colorindexes[i].idx >= 0 then
      if db.Colors(C_colorindexes[i].idx).name = name then
      begin
        Result := i;
        Exit;
      end;
  Result := 0;
end;

procedure TMosaicForm.MyUpdate;
var
  i, idx: integer;
  nchecked: integer;
  tmpname: string;
begin
  if updatelocked then
    Exit;

  Screen.Cursor := crHourGlass;
  updatelocked := true;
  try
    Label1.Caption := IntToStr(TrackBar1.Position);
    Label2.Caption := IntToStr(TrackBar2.Position);
    Image1.Width := TrackBar1.Position;
    Image1.Height := TrackBar2.Position;
    Image1.Picture.Bitmap.Width := TrackBar1.Position;
    Image1.Picture.Bitmap.Height := TrackBar2.Position;

    mosaic.colormode := RadioGroup1.ItemIndex;
    if mosaic.colormode < 0 then
      mosaic.colormode := 0;
    nchecked := 0;
    for i := 0 to CheckListBox1.Items.Count - 1 do
      if CheckListBox1.Checked[i] then
        inc(nchecked);
    if nchecked = 0 then
      CheckListBox1.Checked[0] := true;

    for i := 0 to CheckListBox1.Items.Count - 1 do
    begin
      idx := MosaicColorIndexFromName(CheckListBox1.Items.Strings[i]);
      if idx >= 0 then
      begin
        if CheckListBox1.Checked[i] then
          mosaic.colorindexes[idx].idx := C_colorindexes[idx].idx
        else
          mosaic.colorindexes[idx].idx := -1
      end;
    end;

    CheckListBox2.Checked[brickindex(@C_bricks, NUMBRICKS, '3005')] := true;
    for i := 0 to CheckListBox2.Items.Count - 1 do
      mosaic.bricks[i].use := CheckListBox2.Checked[i];

    if Edit1.Text <> '' then
    begin
      Image1.Picture.Bitmap.Canvas.StretchDraw(
        Rect(0, 0, TrackBar1.Position, TrackBar2.Position), pic.Graphic);

      mosaic.BitmapColorTransform(Image1.Picture.Bitmap);

      tmpname := I_NewTempFile('mosaic') + '.bmp';
      mosaic.Bitmap.SaveToFile(tmpname);
      Image2.Picture.LoadFromFile(tmpname);
      DeleteFile(tmpname);

      if not Image2.Autosize then
      begin
        Image2.Width := ScrollBox1.Width;
        Image2.Height := Round(ScrollBox1.Width * TrackBar2.Position / TrackBar1.Position);
      end;

      EditCostNew.ReadOnly := False;
      EditCostNew.Text := moneyhtml(mosaic.inv.SoldPartOutValue_nQtyAvg.value, 2);
      EditCostNew.ReadOnly := True;
      EditCostUsed.ReadOnly := False;
      EditCostUsed.Text := moneyhtml(mosaic.inv.SoldPartOutValue_uQtyAvg.value, 2);
      EditCostUsed.ReadOnly := True;

    end;
  finally
    updatelocked := false;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMosaicForm.Button1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    updatelocked := true;
    Edit1.ReadOnly := False;
    Edit1.Text := OpenPictureDialog1.FileName;
    pic.LoadFromFile(Edit1.Text);
    if pic.Graphic <> nil then
    begin
      if (pic.Graphic.Width <= MAXDIMENTION) and (pic.Graphic.Height <= MAXDIMENTION) then
      begin
        TrackBar1.Position := pic.Graphic.Width;
        TrackBar2.Position := pic.Graphic.Height;
      end
      else
      begin
        if pic.Graphic.Width > pic.Graphic.Height then
        begin
          TrackBar1.Position := MAXDIMENTION;
          TrackBar2.Position := Round(MAXDIMENTION * pic.Graphic.Height / pic.Graphic.Width);
        end
        else
        begin
          TrackBar1.Position := Round(MAXDIMENTION * pic.Graphic.Width / pic.Graphic.Height);
          TrackBar2.Position := MAXDIMENTION;
        end
      end;
    end;
    Edit1.ReadOnly := True;
    updatelocked := false;
    MyUpdate;
  end;
  ChDir(basedir);
end;

procedure TMosaicForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  basedir := GetCurrentDir;
  updatelocked := False;
  pic := TPicture.Create;
  TrackBar1.Min := 1;
  TrackBar1.Max := MAXDIMENTION;
  TrackBar2.Min := 1;
  TrackBar2.Max := MAXDIMENTION;
  mosaic := TBrickMosaic.Create;
  CheckListBox1.Items.Clear;
  for i := 0 to NUMCOLORINDEXES - 1 do
    if mosaic.colorindexes[i].idx >= 0 then
      CheckListBox1.Checked[CheckListBox1.Items.Add(db.Colors(mosaic.colorindexes[i].idx).name)] := true;
  CheckListBox2.Items.Clear;
  for i := 0 to NUMBRICKS - 1 do
    CheckListBox2.Checked[CheckListBox2.Items.Add(mosaic.bricks[i].brick + ' (' + db.PieceDesc(mosaic.bricks[i].brick) + ')')] := mosaic.bricks[i].use;
  MyUpdate;
end;

procedure TMosaicForm.TrackBar1Change(Sender: TObject);
begin
  MyUpdate;
end;

procedure TMosaicForm.TrackBar2Change(Sender: TObject);
begin
  MyUpdate;
end;

procedure TMosaicForm.FormDestroy(Sender: TObject);
begin
  mosaic.Free;
  pic.Free;
end;

procedure TMosaicForm.CheckListBox1Click(Sender: TObject);
begin
  MyUpdate;
end;

function TMosaicForm.name: string;
begin
  result := ExtractFileName(Edit1.Text);
  if CharPos('.', Result) > 0 then
    while CharPos('.', Result) > 0 do
      SetLength(Result, Length(Result) - 1);
end;

procedure TMosaicForm.CheckListBox2Click(Sender: TObject);
begin
  MyUpdate;
end;

procedure TMosaicForm.RadioGroup1Click(Sender: TObject);
begin
  MyUpdate;
end;

procedure TMosaicForm.Image2Resize(Sender: TObject);
begin
  if Image2.AutoSize then
  begin
    Image2.Top := 0;
    Image2.Left := 0;
    Image2.AutoSize := false;
    Image2.Stretch := True;
    Image2.Width := ScrollBox1.Width;
    Image2.Height := Round(ScrollBox1.Width * TrackBar2.Position / TrackBar1.Position);
  end
  else
  begin
    Image2.Top := 0;
    Image2.Left := 0;
    Image2.AutoSize := true;
    Image2.Stretch := false;
  end
end;

procedure TMosaicForm.SpeedButton1Click(Sender: TObject);
begin
  Clipboard.Assign(Image2.Picture.Bitmap);
end;

procedure TMosaicForm.Panel13DblClick(Sender: TObject);
var
  value1, value2: integer;
begin
  value1 := TrackBar1.Position;
  value2 := TrackBar2.Position;
  if InputTwoIntegers('Dimentions', 'x:', 'y:', value1, value2) then
  begin
    if value1 < TrackBar1.Min then
      Exit;
    if value1 > TrackBar1.Max then
      Exit;
    if value2 < TrackBar2.Min then
      Exit;
    if value2 > TrackBar2.Max then
      Exit;
    TrackBar1.Position := value1;
    TrackBar2.Position := value2;
    MyUpdate;
  end;
end;

end.
