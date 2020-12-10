unit CR_timing;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, bi_threadtimer, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    CrawlerTimer: TTimer;
    Button1: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    TrackBar1: TTrackBar;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CrawlerTimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    { Private declarations }
 //   tr: TThreadComponent;
    doabort: boolean;
    cancrowl: boolean;
  public
    { Public declarations }
    crawling: Boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  bi_db, bi_globals, bi_crawler, main;

function InputBox(const ACaption, APrompt, ADefault: string): string;
begin
  Result := ADefault;
  InputQuery(ACaption, APrompt, Result);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  crfile: string;
  ACaption, APrompt, ADefault: string;
begin
  cancrowl := false;
  ACaption := 'Crawler file:';
  APrompt := 'Give the crawler file';
  ADefault := 'CR_CRAWLER';
  doabort := not InputQuery(ACaption, APrompt, ADefault);
  if not doabort then
  begin
    crfile := ADefault;
    Label1.Caption := crfile;
    Memo1.Lines.Clear;
    db := TSetsDatabase.Create;
    db.InitCreate(crfile);
    db.LoadFromDisk(basedefault + 'db\db_set_pieces.txt');
    crawling := false;
    cancrowl := true;
  end
  else
  begin
    Label1.Caption := '';
    Memo1.Lines.Clear;
    CrawlerTimer.Interval := 1;
    Close;
  end;

  CrawlerTimer.Enabled := true;
 { tr := TThreadComponent.Create(nil);
  tr.Sleep := CrawlerTimer.Interval;
  tr.OnExecute := CrawlerTimerTimer;
  tr.Enabled := true;  }
end;

procedure TForm1.CrawlerTimerTimer(Sender: TObject);
begin
  if doabort then
  begin
    Close;
    exit;
  end;

  Label1.Font.Color := clBlack;
  Label1.Update;

  if CheckBox1.Checked then
  begin
    Caption := 'Bricklink Crawler (paused)';
    Exit;
  end
  else
    Caption := 'Bricklink Crawler';

  try
    if not cancrowl then
      exit;

    if db = nil then
      Exit;

    if crawling then
      exit;

    crawling := true;
    Label1.Font.Color := clMaroon;
    Label1.Update;
    db.Crawler;
    if USDwarning then
    begin
      Beep;
      Memo1.Lines.Add('Warnign - USD values (' + db.lastcrawlpiece + ') - time=' + FormatDateTime('c', Now));
    end;
    if FAILwarning then
    begin
      Beep;
      Memo1.Lines.Add('Warnign - Failed to access priceguide (' + db.lastcrawlpiece + ') - time=' + FormatDateTime('c', Now));
    end;
    while Memo1.Lines.Count > 200 do
      Memo1.Lines.Delete(0);
    Label1.Font.Color := clBlack;
    Label1.Update;
    Label2.Caption := db.lastcrawlpiece;
    Label2.Update;
    crawling := false;
  except
    crawling := false;
    cancrowl := true;
    Memo1.Lines.Add('Exception at ' + FormatDateTime('c', Now));
    if Memo1.Lines.Count > 5 then
      Memo1.Lines.Delete(0);
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if not doabort then
  begin
    cancrowl := false;
    CrawlerTimer.Enabled := false;
    db.Free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  CrawlerTimer.Interval := 6000 - (TrackBar1.Position - 5) * 500;
end;

procedure TForm1.FormHide(Sender: TObject);
begin
  if doabort then
  begin
    Application.Terminate;
    exit;
  end;
end;

end.
