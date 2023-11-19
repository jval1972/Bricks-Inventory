{$ifdef ver140}
{$warn Symbol_Platform Off}
{$endif}

unit HTMLEd1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, Htmlview, StdCtrls, ComCtrls, ShellAPI, Buttons,
  bi_colorpickerbutton, bi_dropdownbutton, bi_undo;

type
  TEditHtmlForm = class(TForm)
    Panel1: TPanel;
    Panel: TPanel;
    Splitter: TSplitter;
    RichEdit: TRichEdit;
    Panel2: TPanel;
    Viewer: THTMLViewer;
    Panel3: TPanel;
    PopupMenu1: TPopupMenu;
    Cut2: TMenuItem;
    Copy2: TMenuItem;
    Paste2: TMenuItem;
    Panel4: TPanel;
    SaveAndExitButton: TButton;
    CancelButton: TButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton6: TSpeedButton;
    ColorPanel1: TPanel;
    ColorDialog1: TColorDialog;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    N1: TMenuItem;
    UndoTimer: TTimer;
    LinkButtonImage: TImage;
    LinkPanel1: TPanel;
    LinkPopupMenu: TPopupMenu;
    Piece1: TMenuItem;
    PieceInventory1: TMenuItem;
    SetInventory1: TMenuItem;
    MakListButton1: TSpeedButton;
    MakeListButton2: TSpeedButton;
    BRSpeedButton: TSpeedButton;
    procedure RichEdChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RichEditSelectionChange(Sender: TObject);
    procedure ViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure EditCopy(Sender: TObject);
    procedure EditCut(Sender: TObject);
    procedure EditPaste(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure ButtonClick(Sender: TObject);
    procedure ViewerHotSpotClick(Sender: TObject; const SRC: string;
      var Handled: boolean);
    procedure ViewerHotSpotCovered(Sender: TObject; const SRC: string);
    procedure FormResize(Sender: TObject);
    procedure SplitterMoved(Sender: TObject);
    procedure SaveAndExitButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure Redo1Click(Sender: TObject);
    procedure UndoTimerTimer(Sender: TObject);
    procedure Piece1Click(Sender: TObject);
    procedure PieceInventory1Click(Sender: TObject);
    procedure SetInventory1Click(Sender: TObject);
    procedure MakListButton1Click(Sender: TObject);
    procedure MakeListButton2Click(Sender: TObject);
    procedure BRSpeedButtonClick(Sender: TObject);
  private
    { Private declarations }
    ViewerOK, RichOK: boolean;
    SizeRatio: double;
    CurrentFile: string;
    ColorPickerButton1: TColorPickerButton;
    undoManager: TUndoRedoManager;
    floaded: boolean;
    fbackuptext: string;
    fSelStart: integer;
    fSelLength: integer;
    P: TPoint;
    LinkButton: TMenuButton;
    procedure CheckFileSave;
    procedure DoSave;
    procedure ColorPickerButtonChange(Sender: TObject);
    procedure ColorPickerButtonClick(Sender: TObject);
    procedure DoLoadUndoFromStream(s: TStream);
    procedure DoSaveUndoToStream(s: TStream);
    procedure SaveUndo;
    procedure Undo;
    procedure Redo;
    procedure SaveUndoData;
    function MakeLink(const lnk: string; const desc: string): string;
    function MakeSimpleLink(const lnk: string; const desc: string): string;
    procedure LinkClick(Sender: TObject);
    function MakeList(const intext: string; const ol, li: string): string;
  public
    { Public declarations }
    didsave: boolean;
    procedure Load(const fname: string);
  end;

function PieceColorNotesFName(piece, color: string): string;
function PieceNotesFName(piece: string): string;

function EditPieceColorNotes(piece, color: string): boolean;

function EditPieceNotes(piece: string): boolean;

implementation

{$R *.DFM}

uses
  bi_delphi, bi_db, bi_utils, PNGZLIB1;

function PieceColorNotesFName(piece, color: string): string;
var
  cpath: string;
begin
  piece := Trim(piece);
  color := Trim(color);

  Result := basedefault + 'notes\' + piece;

  if color = '-1' then
    cpath := '9999'
  else
    cpath := color;

  Result := Result + '\' + piece + '_' + cpath + '.html';
end;

function PieceNotesFName(piece: string): string;
begin
  piece := Trim(piece);

  Result := basedefault + 'notes\' + piece;

  Result := Result + '\' + piece + '.html';
end;

function EditPieceColorNotes(piece, color: string): boolean;
var
  f: TEditHtmlForm;
  sdir: string;
begin
  Result := False;

  Screen.Cursor := crHourglass;

  piece := Trim(piece);

  sdir := basedefault + 'notes';
  if not DirectoryExists(sdir) then
    MkDir(sdir);

  sdir := sdir + '\' + piece;
  if not DirectoryExists(sdir) then
    MkDir(sdir);

  f := TEditHtmlForm.Create(nil);
  try
    f.Caption := 'Notes for ' + piece + ' (' + color + ')';
    f.Load(PieceColorNotesFName(piece, color));
    f.ShowModal;
    if f.didsave then
      Result := True;
  finally
    f.Free;
    Screen.Cursor := crDefault;
  end;
end;

function EditPieceNotes(piece: string): boolean;
var
  f: TEditHtmlForm;
  sdir: string;
begin
  Result := False;

  Screen.Cursor := crHourglass;

  piece := Trim(piece);

  sdir := basedefault + 'notes';
  if not DirectoryExists(sdir) then
    MkDir(sdir);

  sdir := sdir + '\' + piece;
  if not DirectoryExists(sdir) then
    MkDir(sdir);

  f := TEditHtmlForm.Create(nil);
  try
    f.Caption := 'Notes for ' + piece;
    f.Load(PieceNotesFName(piece));
    f.ShowModal;
    if f.didsave then
      Result := True;
  finally
    f.Free;
    Screen.Cursor := crDefault;
  end;
end;

const
  InitText = '<div>'#13#10'</div>';
  MAX_HTML_LEN = 10 * 1024 * 1024; // 10 MB

procedure TEditHtmlForm.FormCreate(Sender: TObject);
begin
  RichEdit.Modified := False;
  RichEdit.MaxLength := MAX_HTML_LEN;
  SizeRatio := 0.5;
  CurrentFile := '';
  didsave := False;
  fbackuptext := '';
  fSelStart := 0;
  fSelLength := 0;
  P.X := 0;
  P.Y := 0;
  floaded := False;

// Create ColorPickerButton
  ColorPickerButton1 := TColorPickerButton.Create(nil);
  ColorPickerButton1.Parent := ColorPanel1;
  ColorPickerButton1.Align := alClient;
  ColorPickerButton1.Color := RGB(0, 0, 0);
  ColorPickerButton1.OnChange := ColorPickerButtonChange;
  ColorPickerButton1.OnClick := ColorPickerButtonClick;

  undoManager := TUndoRedoManager.Create;
  undoManager.OnLoadFromStream := DoLoadUndoFromStream;
  undoManager.OnSaveToStream := DoSaveUndoToStream;
  undoManager.StreamType := sstFile;
  undoManager.UndoLimit := 1000;
  undoManager.CompressionLevel := zcFastest;

  LinkButton := TMenuButton.Create(nil);
  LinkButton.Parent := LinkPanel1;
  LinkButton.Align := alClient;
  LinkButton.Flat := False;
  LinkButton.Glyph.Assign(LinkButtonImage.Picture.Bitmap);
  LinkButton.Glyph.Transparent := True;
  LinkButton.OnClick := LinkClick;
  LinkButton.Menu := LinkPopupMenu;
end;

procedure TEditHtmlForm.FormDestroy(Sender: TObject);
begin
  ColorPickerButton1.Free;
  LinkButton.Free;
  undoManager.Free;
end;

procedure TEditHtmlForm.Load(const fname: string);
var
  Stream: TMemoryStream;
  S: string;
begin
  CurrentFile := fname;
  if fexists(fname) then
  begin
    Stream := TMemoryStream.Create;
    try
      Stream.LoadFromFile(fname);
      SetLength(S, Stream.Size);
      Move(Stream.Memory^, S[1], Stream.Size);
      RichEdit.Text := S;
      RichOK := True;
      ViewerOK := True;
      RichEdit.Modified := False;
    finally
      Stream.Free;
    end;
  end
  else
  begin
    RichEdit.Text := InitText;
    RichOK := True;
    ViewerOK := True;
    RichEdit.Modified := False;
  end;
  RichEdChange(Self);
  undoManager.Clear;
  SaveUndoData;
  floaded := True;
  UndoTimer.Enabled := True;
end;

procedure TEditHtmlForm.RichEdChange(Sender: TObject);
{TRichEdit OnChange handler}
var
  Position: integer;
begin
  if RichOK then
  begin
    Position := Viewer.Position;
    Viewer.LoadFromBuffer(PChar(RichEdit.Text), Length(RichEdit.Text), '');
    ViewerOK := True;
    Viewer.Position := Position;
    RichEditSelectionChange(nil);
  end;
end;

procedure TEditHtmlForm.RichEditSelectionChange(Sender: TObject);
{TRichEdit OnSelectionChange handler}
var
  Pos1, Pos2, X, Y, VPos, SStr, SLen: integer;
begin
  if ViewerOK then
  begin
    SStr := RichEdit.SelStart;
    SLen := RichEdit.SelLength;
    if SStr + SLen > Length(RichEdit.Text) then
      SLen := Length(RichEdit.Text) - SStr;
    Pos1 := Viewer.FindDisplayPos(SStr, False);
    if Pos1 < 0 then  {means it's past end}
      Pos1 := Viewer.FindDisplayPos(SStr, True);
    if SLen <> 0 then
    begin
      Pos2 := Viewer.FindDisplayPos(SStr + SLen, False);
      if Pos2 < 0 then
        Pos2 := Viewer.FindDisplayPos(SStr + SLen - 1, False); {fix for above}
    end
    else
      Pos2 := Pos1;
    if (Pos1 >= 0) and Viewer.DisplayPosToXY(Pos1, X, Y) then
    begin
      {see if Viewer is positioned properly}
      VPos := Viewer.VScrollBarPosition;
      if (Y < VPos) or (Y > VPos + Viewer.ClientHeight - 20) then
        Viewer.VScrollBarPosition := (Y - Viewer.ClientHeight div 2);
      Viewer.SelStart := Pos1;
      Viewer.SelLength := Pos2 - Pos1;
    end;
  end;
end;

procedure TEditHtmlForm.ViewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  Pos, Pos2, VSelLength: integer;
begin
  if not ViewerOK then
    Exit;
  ViewerOK := False;
  try
    VSelLength := Viewer.SelLength;
    if VSelLength >= 0 then
      Pos := Viewer.FindSourcePos(Viewer.SelStart)
    else
      Pos := Viewer.FindSourcePos(Viewer.SelStart + VSelLength);
    if Pos >= 0 then
    begin
      RichEdit.SelStart := Pos;
      if VSelLength = 0 then
        RichEdit.SelLength := 0
      else if VSelLength > 0 then
      begin
        Pos2 := Viewer.FindSourcePos(Viewer.SelStart + VSelLength - 1) + 1;
        RichEdit.SelLength := Pos2 - Pos;
      end
      else
      begin
        Pos2 := Viewer.FindSourcePos(Viewer.SelStart - 1) + 1;
        RichEdit.SelLength := Pos2 - Pos;
      end;
      RichEdit.SetFocus;
      PostMessage(RichEdit.handle, em_scrollcaret, 0, 0);   {8.03}
    end;
  finally
    ViewerOK := True;
  end;
end;

procedure TEditHtmlForm.EditCut(Sender: TObject);
begin
  RichEdit.CutToClipboard;
end;

procedure TEditHtmlForm.EditCopy(Sender: TObject);
begin
  RichEdit.CopyToClipboard;
end;

procedure TEditHtmlForm.EditPaste(Sender: TObject);
begin
  RichEdit.PasteFromClipboard;
end;

procedure TEditHtmlForm.CheckFileSave;
var
  SaveResp: integer;
begin
  if not RichEdit.Modified then
    Exit;
  SaveResp := MessageDlg(Format('Save changes to %s?', [CurrentFile]),
    mtConfirmation, mbYesNoCancel, 0);
  case SaveResp of
    idYes: DoSave;
    idNo: {Nothing};
    idCancel: Abort;
  end;
end;

procedure TEditHtmlForm.ExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TEditHtmlForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  try
    CheckFileSave;
  except
    CanClose := False;
  end;
end;

procedure TEditHtmlForm.ButtonClick(Sender: TObject);
var
  C: string;
begin
  case TSpeedButton(Sender).Tag of
    0: C := 'B';
    1: C := 'I';
    2: C := 'U';
    3: C := 'h1';
    4: C := 'h2';
    5: C := 'h3';
    else
      C := 'B';
  end;
  RichEdit.SelText := '<' + C + '>' + RichEdit.SelText + '</' + C + '>';
  try RichEdit.SetFocus except end;
end;

procedure TEditHtmlForm.ViewerHotSpotClick(Sender: TObject; const SRC: string;
  var Handled: boolean);
{HotspotClick handler}
var
  I: integer;
  ID: string;
begin
  I := Pos('IDEXPAND_', Uppercase(SRC));
  if I = 1 then
  begin
    ID := Copy(SRC, 10, Length(SRC) - 9);
    Viewer.IDDisplay[ID + 'Plus'] := not Viewer.IDDisplay[ID + 'Plus'];
    Viewer.IDDisplay[ID + 'Minus'] := not Viewer.IDDisplay[ID + 'Minus'];
    Viewer.Reformat;
    Handled := True;
    Exit;
  end;

  {allow only local links to work}
  if (Length(SRC) > 0) and (Trim(SRC)[1] = '#') then
    Handled := False
  else
    Handled := True;
end;

procedure TEditHtmlForm.ViewerHotSpotCovered(Sender: TObject; const SRC: string);
begin
  if SRC = '' then
    Panel3.Caption := ''
  else if Viewer.Target <> '' then
    Panel3.Caption := 'Target: ' + Viewer.Target + '     URL: ' + SRC
  else
    Panel3.Caption := 'URL: ' + SRC;
end;

procedure TEditHtmlForm.FormResize(Sender: TObject);
begin
  RichEdit.Height := Round((Panel.Height - Splitter.Height) * SizeRatio);
end;

procedure TEditHtmlForm.SplitterMoved(Sender: TObject);
begin
  SizeRatio := RichEdit.Height / (Panel.Height - Splitter.Height);
end;

procedure TEditHtmlForm.DoSave;
begin
  RichEdit.Lines.SaveToFile(CurrentFile);
  RichEdit.Modified := False;
  didsave := True;
end;

procedure TEditHtmlForm.SaveAndExitButtonClick(Sender: TObject);
begin
  DoSave;
  Close;
end;

procedure TEditHtmlForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TEditHtmlForm.ColorPickerButtonChange(Sender: TObject);
begin
  RichEdit.SelText := '<font color=#' + IntToHex(RGBInvert(ColorToRGB((Sender as TColorPickerButton).Color)), 6) + '>' + RichEdit.SelText + '</font>';
  try RichEdit.SetFocus except end;
end;

procedure TEditHtmlForm.ColorPickerButtonClick(Sender: TObject);
begin
  ColorDialog1.Color := (Sender as TColorPickerButton).Color;
  if ColorDialog1.Execute then
    (Sender as TColorPickerButton).Color := ColorDialog1.Color;
end;

procedure TEditHtmlForm.PopupMenu1Popup(Sender: TObject);
begin
  Undo1.Enabled := undoManager.CanUndo;
  Redo1.Enabled := undoManager.CanRedo;
  Cut2.Enabled := RichEdit.SelText <> '';
  Copy2.Enabled := Cut2.Enabled;
end;

procedure TEditHtmlForm.Undo1Click(Sender: TObject);
begin
  Undo;
end;

procedure TEditHtmlForm.Redo1Click(Sender: TObject);
begin
  Redo;
end;

const
  EM_GETSCROLLPOS = $04DD;
  EM_SETSCROLLPOS = $04DE;

procedure TEditHtmlForm.DoLoadUndoFromStream(s: TStream);
var
  SStr, SLen: Integer;
begin
  s.Read(SStr, SizeOf(Integer));
  s.Read(SLen, SizeOf(Integer));
  s.Read(P, SizeOf(TPoint));
  RichEdit.Lines.LoadFromStream(s);
  RichEdit.SelStart := SStr;
  RichEdit.SelLength := SLen;
  RichEdit.Perform(EM_SETSCROLLPOS, 0, Integer(@P));
end;

procedure TEditHtmlForm.DoSaveUndoToStream(s: TStream);
begin
  s.Write(fSelStart, SizeOf(Integer));
  s.Write(fSelLength, SizeOf(Integer));
  s.Write(P, SizeOf(TPoint));

  s.Write(fbackuptext[1], Length(fbackuptext));
end;

procedure TEditHtmlForm.SaveUndo;
begin
  if floaded then
    undoManager.SaveUndo;
end;

procedure TEditHtmlForm.Undo;
begin
  undoManager.Undo;
  SaveUndoData;
end;

procedure TEditHtmlForm.Redo;
begin
  undoManager.Redo;
  SaveUndoData;
end;

procedure TEditHtmlForm.UndoTimerTimer(Sender: TObject);
var
  strtest: string;
begin
  strtest := RichEdit.Text;
  if strtest <> fbackuptext then
  begin
    SaveUndo;
    SaveUndoData;
  end;
end;

procedure TEditHtmlForm.SaveUndoData;
begin
  fbackuptext := RichEdit.Text;
  fSelStart := RichEdit.SelStart;
  fSelLength := RichEdit.SelLength;
  RichEdit.Perform(EM_GETSCROLLPOS, 0, Integer(@P));
end;

function TEditHtmlForm.MakeLink(const lnk: string; const desc: string): string;
begin
  Result := '<a href=' + lnk + '>' + desc + '</a>';
end;

function TEditHtmlForm.MakeSimpleLink(const lnk: string; const desc: string): string;
begin
  Result := '<a href=' + lnk + '/' + RichEdit.SelText + '>' + desc + '</a>';
end;

procedure TEditHtmlForm.LinkClick(Sender: TObject);
var
  lnk, desc: string;
begin
  lnk := '';
  desc := RichEdit.SelText;
  if InputQuery2('Link', 'URL', 'Link text', lnk, desc) then
  begin
    RichEdit.SelText := MakeLink(lnk, desc);
    try RichEdit.SetFocus except end;
  end;
end;

procedure TEditHtmlForm.Piece1Click(Sender: TObject);
begin
  RichEdit.SelText := MakeSimpleLink('spiece', RichEdit.SelText);
  try RichEdit.SetFocus except end;
end;

procedure TEditHtmlForm.PieceInventory1Click(Sender: TObject);
begin
  RichEdit.SelText := MakeSimpleLink('spiececinv', RichEdit.SelText);
  try RichEdit.SetFocus except end;
end;

procedure TEditHtmlForm.SetInventory1Click(Sender: TObject);
begin
  RichEdit.SelText := MakeSimpleLink('sinv', RichEdit.SelText);
  try RichEdit.SetFocus except end;
end;

function TEditHtmlForm.MakeList(const intext: string; const ol, li: string): string;
var
  i, len: integer;
begin
  len := Length(intext);
  if len = 0 then
  begin
    Result := '<' + ol + '>'#13#10'<' + li + '></' + li + '>'#13#10'</' + ol + '>';
    Exit;
  end;

  Result := '<' + ol + '><' + li + '>';
  for i := 1 to len do
  begin
    if intext[i] = #10 then
    begin
      Result := Result + '</' + li + '>';
      if i <> len then
        Result := Result + '<' + li + '>'
    end;
    Result := Result + intext[i];
  end;
  if intext[len] <> #10 then
    Result := Result + '</' + li + '>';
  Result := Result + '</' + ol + '>';
end;

procedure TEditHtmlForm.MakListButton1Click(Sender: TObject);
begin
  RichEdit.SelText := MakeList(RichEdit.SelText, 'ul', 'li');
  try RichEdit.SetFocus except end;
end;

procedure TEditHtmlForm.MakeListButton2Click(Sender: TObject);
begin
  RichEdit.SelText := MakeList(RichEdit.SelText, 'ol', 'li');
  try RichEdit.SetFocus except end;
end;

procedure TEditHtmlForm.BRSpeedButtonClick(Sender: TObject);
begin
  RichEdit.SelText := '<br>';
  try RichEdit.SetFocus except end;
end;

end.
