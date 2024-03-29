{Version 9.45}

{$i htmlcons.inc}

unit GDIPL2A;

interface

uses Windows, SysUtils, ActiveX, Graphics;

var
  GDIPlusActive: boolean;

type
  TGpImage = class(TObject)
  private
    fHandle: integer;
    fWidth, fHeight: integer;
    fFilename: string;
    function GetHeight: integer;
    function GetWidth: integer;
  public
    constructor Create(Filename: string; TmpFile: boolean = False); overload;
    constructor Create(IStr: IStream); overload;
    destructor Destroy; override;
    function GetTBitmap: TBitmap;
    property Height: integer read GetHeight;
    property Width: integer read GetWidth;
  end;

  TGpGraphics = class;

  TGpBitmap = class(TGpImage)
  public
    constructor Create(W, H: integer); overload;
    constructor Create(IStr: IStream); overload;
    constructor Create(W, H: integer; Graphics: TGpGraphics); overload;
    function GetPixel(X, Y: integer): DWord;
    procedure SetPixel(X, Y: integer; Color: DWord);
  end;

  TGpGraphics = class(TObject)
  private
    fGraphics: integer;
    procedure DrawSmallStretchedImage(Image: TGPImage; X, Y, Width, Height: integer);
  public
    constructor Create(Handle: HDC); overload;
    constructor Create(Image: TGpImage); overload;
    destructor Destroy; override;
    procedure DrawImage(Image: TGPImage; X, Y: integer); overload;
    procedure DrawImage(Image: TGPImage; X, Y, Width, Height: integer); overload;
    procedure DrawImage(Image: TGpImage; x, y, srcx, srcy, srcwidth, srcheight: integer);
      overload;
    procedure DrawImage(Image: TGpImage; dx, dy, dw, dh, sx, sy, sw, sh: integer);
      overload;
    procedure Clear(Color: cardinal);
    procedure ScaleTransform(sx, sy: single);
  end;

procedure CheckInitGDIPlus;
procedure CheckExitGDIPlus;

implementation

const
  GdiPlusLib = 'GdiPlus.dll';

type
  EGDIPlus = class(Exception);

  TRectF = record
    X: single;
    Y: single;
    Width: single;
    Height: single;
  end;

  ImageCodecInfo = packed record
    Clsid: TGUID;
    FormatID: TGUID;
    CodecName: PWCHAR;
    DllName: PWCHAR;
    FormatDescription: PWCHAR;
    FilenameExtension: PWCHAR;
    MimeType: PWCHAR;
    Flags: DWORD;
    Version: DWORD;
    SigCount: DWORD;
    SigSize: DWORD;
    SigPattern: PBYTE;
    SigMask: PBYTE;
  end;
  TImageCodecInfo = ImageCodecInfo;
  PImageCodecInfo = ^TImageCodecInfo;

var
{$ifndef NoGDIPlus}
  GdiplusStartup: function(var Token: DWord; const Input, Output: Pointer): integer;
  stdcall;
  GdiplusShutdown: procedure(Token: DWord); stdcall;
  GdipDrawImageI: function(Graphics, Image, X, Y: integer): integer; stdcall;
  GdipCreateHBITMAPFromBitmap: function(bitmap: integer; out hbmReturn: HBITMAP;
  background: DWord): integer; stdcall;
  GdipGetInterpolationMode: function(Graphics: integer;
  var interpolationMode: integer): integer; stdcall;
{$endif$}
  GdipDeleteGraphics: function(Graphics: integer): integer; stdcall;
  GdipCreateFromHDC: function(hdc: HDC; var Graphics: integer): integer; stdcall;
  GdipDrawImageRectI: function(Graphics, Image, X, Y, Width, Height: integer): integer;
  stdcall;
  GdipLoadImageFromFile: function(const FileName: PWideChar;
  var Image: integer): integer; stdcall;
  GdipLoadImageFromStream: function(stream: ISTREAM;
  out image: integer): integer; stdcall;
  GdipCreateBitmapFromStream: function(stream: ISTREAM; out bitmap: integer): integer;
  stdcall;
  GdipDisposeImage: function(Image: integer): integer; stdcall;
  GdipGetImageWidth: function(Image: integer; var Width: integer): integer; stdcall;

  GdipGetImageHeight: function(Image: integer; var Height: integer): integer; stdcall;
  GdipGetImageGraphicsContext: function(Image: integer; out Graphics: integer): integer;
  stdcall;
  GdipGraphicsClear: function(Graphics: integer; Color: cardinal): integer; stdcall;
  GdipCreateBitmapFromScan0: function(Width: integer; Height: integer;
  stride: integer; pixelformat: dword; scan0: Pointer;
  out bitmap: integer): integer; stdcall;
  GdipDrawImagePointRect: function(Graphics: integer; image: integer;
  x: single; y: single; srcx: single; srcy: single; srcwidth: single;
  srcheight: single; srcUnit: integer): integer; stdcall;
  GdipScaleWorldTransform: function(Graphics: integer; sx: single;
  sy: single; order: integer): integer; stdcall;
  GdipCreateBitmapFromGraphics: function(Width, Height: integer;
  Graphics: integer; out Bitmap: integer): integer; stdcall;
  GdipBitmapGetPixel: function(bitmap, x, y: integer; var color: DWord): integer;
  stdcall;
  GdipDrawImageRectRectI: function(Graphics, image, dstx, dsty,
  dstwidth, dstheight, srcx, srcy, srcwidth, srcheight, srcUnit,
  imageAttributes: integer; callback: Pointer; callbackData: integer): integer;
  stdcall;

  GdipSetInterpolationMode: function(Graphics, interpolationMode: integer): integer;
  stdcall;
  GdipBitmapSetPixel: function(bitmap, x, y: integer; color: DWord): integer; stdcall;

type
  TGDIStartup = packed record
    Version: integer;                       // Must be one
    DebugEventCallback: Pointer;            // Only for debug builds
    SuppressBackgroundThread: Bool;
    // True if replacing GDI+ background processing
    SuppressExternalCodecs: Bool;           // True if only using internal codecs
  end;

var
  Err: integer;

{ TGpGraphics }

constructor TGpGraphics.Create(Handle: HDC);
var
  err: integer;
begin
  inherited Create;
  err := GdipCreateFromHDC(Handle, fGraphics);
  if err <> 0 then
    raise EGDIPlus.Create('Can''t Create Graphics');
end;

constructor TGpGraphics.Create(Image: TGpImage);
var
  err: integer;
begin
  inherited Create;
  err := GdipGetImageGraphicsContext(image.fHandle, fgraphics);
  if err <> 0 then
    raise EGDIPlus.Create('Can''t Create Graphics');
end;

destructor TGpGraphics.Destroy;
begin
  if fGraphics <> 0 then
    GdipDeleteGraphics(fGraphics);
  inherited;
end;

procedure TGpGraphics.DrawImage(Image: TGPImage; X, Y, Width, Height: integer);
begin
  if ((Image.Width <= 10) and (Width > Image.Width)) or
    ((Image.Height <= 10) and (Height > Image.Height)) then
    DrawSmallStretchedImage(Image, X, Y, Width, Height)
  else
    GdipDrawImageRectI(fGraphics, Image.fHandle, X, Y, Width, Height);
end;

procedure TGpGraphics.DrawSmallStretchedImage(Image: TGPImage;
  X, Y, Width, Height: integer);
{when a small image is getting enlarged, add a row and column to it copying
 the last row/column to the new row/column.  This gives much better interpolation.}
const
  NearestNeighbor = 5;
var
  g1, g2: TGpGraphics;
  BM1, BM2: TGpBitmap;
  W, H: integer;
  Pixel: DWord;
begin
  W := Image.Width + 1;  {new dimensions}
  H := Image.Height + 1;
  BM1 := TGpBitmap.Create(W, H);  {new bitmap with extra row and column}
  try
    g1 := TGpGraphics.Create(BM1);
    try
      g1.DrawImage(Image, 0, 0);  {draw the original image}
      g1.DrawImage(Image, W - 1, 0, 1, H,   {copy the column, then the row}
        W - 2, 0, 1, H);
      g1.DrawImage(Image, 0, H - 1, W, 1,
        0, H - 2, W, 1);
      Pixel := BM1.GetPixel(W - 2, H - 2);
      {for some reason also need to set the lower right pixel}
      BM1.SetPixel(W - 1, H - 1, Pixel);
      BM2 := TGpBitmap.Create(Width, Height);
      try
        g2 := TGpGraphics.Create(BM2);
        try
          GdipSetInterpolationMode(g2.fGraphics, NearestNeighbor);
          g2.DrawImage(BM1, 0, 0, Width, Height,
            {now draw the image stretched where needed}
            0, 0, Image.Width, Image.Height);
          DrawImage(BM2, X, Y);     {now draw the image stretched where needed}
        finally
          g2.Free;
        end;
      finally
        BM2.Free;
      end;
    finally
      g1.Free;
    end;
  finally
    BM1.Free;
  end;
end;

procedure TGpGraphics.DrawImage(Image: TGPImage; X, Y: integer);
begin
  GdipDrawImageRectI(fGraphics, Image.fHandle, X, Y, Image.Width, Image.Height);
end;

procedure TGPGraphics.DrawImage(Image: TGpImage;
  x, y, srcx, srcy, srcwidth, srcheight: integer);
const
  UnitPixel = 2;
begin
  GdipDrawImagePointRect(fGraphics, Image.fHandle, x, y,
    srcx, srcy, srcwidth, srcheight, UnitPixel);
end;

procedure TGPGraphics.DrawImage(Image: TGpImage;
  dx, dy, dw, dh, sx, sy, sw, sh: integer);
const
  UnitPixel = 2;
begin
  GdipDrawImageRectRectI(fGraphics, Image.fHandle, dx, dy, dw, dh,
    sx, sy, sw, sh, UnitPixel, 0, nil, 0);

end;

procedure TGpGraphics.Clear(Color: cardinal);
begin
  GdipGraphicsClear(fGraphics, Color);
end;

procedure TGPGraphics.ScaleTransform(sx, sy: single);
const
  MatrixOrderPrepend = 0;
begin
  GdipScaleWorldTransform(fGraphics, sx, sy, MatrixOrderPrepend);
end;

{ TGpImage }

constructor TGpImage.Create(Filename: string; TmpFile: boolean = False);
var
  err: integer;
  Buffer: array [0..511] of widechar;
begin
  inherited Create;
  if not FileExists(FileName) then
    raise EGDIPlus.Create(Format('Image file %s not found.', [FileName]));
  err := GdipLoadImageFromFile(StringToWideChar(FileName, Buffer,
    SizeOf(Buffer)), fHandle);
  if err <> 0 then
    raise EGDIPlus.Create(Format('Can''t load image file %s.', [FileName]));
  if TmpFile then
    fFilename := Filename;
end;

constructor TGpImage.Create(IStr: IStream);
var
  err: integer;
begin
  inherited Create;
  err := GdipLoadImageFromStream(IStr, fHandle);
  if err <> 0 then
    raise EGDIPlus.Create('Can''t load image stream');
end;

destructor TGpImage.Destroy;
begin
  GdipDisposeImage(fHandle);
  if Length(fFilename) > 0 then
    try
      DeleteFile(fFilename);
    except
    end;
  inherited;
end;

function TGpImage.GetWidth: integer;
begin
  if fWidth = 0 then
    GdipGetImageWidth(fHandle, fWidth);
  Result := fWidth;
end;

function TGpImage.GetHeight: integer;
begin
  if fHeight = 0 then
    GdipGetImageHeight(fHandle, fHeight);
  Result := fHeight;
end;

function TGpImage.GetTBitmap: TBitmap;
var
  g: TGpGraphics;
begin
  Result := TBitmap.Create;
  Result.Width := GetWidth;
  Result.Height := GetHeight;
  PatBlt(Result.Canvas.Handle, 0, 0, Result.Width, Result.Height, Whiteness);
  g := TGpGraphics.Create(Result.Canvas.Handle);
  g.DrawImage(Self, 0, 0, Result.Width, Result.Height);
  g.Free;
end;

constructor TGpBitmap.Create(W, H: integer);
const
  PixelFormatGDI = $00020000; // Is a GDI-supported format
  PixelFormatAlpha = $00040000; // Has an alpha component
  PixelFormatCanonical = $00200000;
  PixelFormat32bppARGB = (10 or (32 shl 8) or PixelFormatAlpha or
    PixelFormatGDI or PixelFormatCanonical);
var
  err: integer;
begin
  inherited Create;
  err := GdipCreateBitmapFromScan0(W, H, 0, PixelFormat32bppARGB, nil, fHandle);
  if err <> 0 then
    raise EGDIPlus.Create('Can''t create bitmap');
end;

constructor TGpBitmap.Create(IStr: IStream);
var
  err: integer;
begin
  inherited Create;
  err := GdipCreateBitmapFromStream(IStr, fHandle);
  if err <> 0 then
    raise EGDIPlus.Create('Can''t create bitmap');
end;

constructor TGpBitmap.Create(W, H: integer; Graphics: TGpGraphics);
begin
  inherited Create;
  err := GdipCreateBitmapFromGraphics(W, H, Graphics.fGraphics, fHandle);
  if err <> 0 then
    raise EGDIPlus.Create('Can''t create bitmap');
end;

function TGpBitmap.GetPixel(X, Y: integer): DWord;
begin
  GdipBitmapGetPixel(fHandle, X, Y, Result);
end;

procedure TGpBitmap.SetPixel(X, Y: integer; Color: DWord);
begin
  GdipBitmapSetPixel(fHandle, X, Y, Color);
end;

{$ifndef NoGDIPlus}
var
  InitToken: DWord;
  Startup: TGDIStartup;
  LibHandle: THandle;
  GDIPlusCount: integer;

{$endif}

procedure CheckInitGDIPlus;
begin
{$ifndef NoGDIPlus}
  if GDIPlusCount = 0 then
  begin
    LibHandle := LoadLibrary(GdiPlusLib);
    if LibHandle <> 0 then
    begin
      @GdiplusStartup := GetProcAddress(LibHandle, 'GdiplusStartup');
      @GdiplusShutdown := GetProcAddress(LibHandle, 'GdiplusShutdown');
      @GdipDeleteGraphics := GetProcAddress(LibHandle, 'GdipDeleteGraphics');
      @GdipCreateFromHDC := GetProcAddress(LibHandle, 'GdipCreateFromHDC');
      @GdipDrawImageI := GetProcAddress(LibHandle, 'GdipDrawImageI');
      @GdipDrawImageRectI := GetProcAddress(LibHandle, 'GdipDrawImageRectI');
      @GdipLoadImageFromFile := GetProcAddress(LibHandle, 'GdipLoadImageFromFile');
      @GdipLoadImageFromStream := GetProcAddress(LibHandle, 'GdipLoadImageFromStream');
      @GdipCreateBitmapFromStream := GetProcAddress(LibHandle, 'GdipCreateBitmapFromStream');
      @GdipDisposeImage := GetProcAddress(LibHandle, 'GdipDisposeImage');
      @GdipGetImageWidth := GetProcAddress(LibHandle, 'GdipGetImageWidth');
      @GdipGetImageHeight := GetProcAddress(LibHandle, 'GdipGetImageHeight');
      @GdipGetImageGraphicsContext := GetProcAddress(LibHandle, 'GdipGetImageGraphicsContext');
      @GdipGraphicsClear := GetProcAddress(LibHandle, 'GdipGraphicsClear');
      @GdipCreateBitmapFromScan0 := GetProcAddress(LibHandle, 'GdipCreateBitmapFromScan0');
      @GdipDrawImagePointRect := GetProcAddress(LibHandle, 'GdipDrawImagePointRect');
      @GdipScaleWorldTransform := GetProcAddress(LibHandle, 'GdipScaleWorldTransform');
      @GdipCreateBitmapFromGraphics :=
        GetProcAddress(LibHandle, 'GdipCreateBitmapFromGraphics');
      @GdipBitmapGetPixel := GetProcAddress(LibHandle, 'GdipBitmapGetPixel');
      @GdipDrawImageRectRectI := GetProcAddress(LibHandle, 'GdipDrawImageRectRectI');
      @GdipCreateHBITMAPFromBitmap :=
        GetProcAddress(LibHandle, 'GdipCreateHBITMAPFromBitmap');
      @GdipSetInterpolationMode := GetProcAddress(LibHandle, 'GdipSetInterpolationMode');
      @GdipGetInterpolationMode := GetProcAddress(LibHandle, 'GdipGetInterpolationMode');
      @GdipBitmapSetPixel := GetProcAddress(LibHandle, 'GdipBitmapSetPixel');

      FillChar(Startup, SizeOf(Startup), 0);
      Startup.Version := 1;
      Err := GdiPlusStartup(InitToken, @Startup, nil);
      GDIPlusActive := Err = 0;
      if not GDIPlusActive then
        FreeLibrary(LibHandle);
    end;
  end;
  Inc(GDIPlusCount);
{$endif}
end;

procedure CheckExitGDIPlus;
begin
{$ifndef NoGDIPlus}
  Dec(GDIPlusCount);
  if GDIPlusCount = 0 then
    if GDIPlusActive then
    begin
      GdiplusShutdown(InitToken);
      FreeLibrary(LibHandle);
      GDIPlusActive := False;
    end;
{$endif}
end;

end.
