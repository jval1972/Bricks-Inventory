unit CR_timing_slow;

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
    Label2: TLabel;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CrawlerTimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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
  bi_db, bi_globals, bi_crawler, bi_tmp, bi_multithread;

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
  I_InitTempFiles;
  MT_Init;
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

  if random(10) < 2 then
    exit;
    
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
      Memo1.Lines.Add('Warning - USD values (' + db.lastcrawlpiece + ') - time=' + FormatDateTime('c', Now));
    end;
    if FAILwarning then
    begin
      Beep;
      Memo1.Lines.Add('Warning - Failed to access priceguide (' + db.lastcrawlpiece + ') - time=' + FormatDateTime('c', Now));
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
    if Memo1.Lines.Count > 200 then
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
  MT_ShutDown;
  I_ShutDownTempFiles;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  db.SaveCrawlerData;
  Close;
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
