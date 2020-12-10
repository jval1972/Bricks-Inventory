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
    procedure FormCreate(Sender: TObject);
    procedure CrawlerTimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
 //   tr: TThreadComponent;
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
  bi_db, bi_globals, main;

procedure TForm1.FormCreate(Sender: TObject);
var
  crfile: string;
begin
  cancrowl := false;
  crfile := Inputbox('Crawler file:', 'Give the crawler file', 'CR_CRAWLER');
  Label1.Caption := crfile;
  db := TSetsDatabase.Create;
  db.InitCreate(crfile);
  db.LoadFromDisk(basedefault + 'db\db_set_pieces.txt');
  crawling := false;
  cancrowl := true;
  CrawlerTimer.Enabled := true;
  Memo1.Lines.Clear;
 { tr := TThreadComponent.Create(nil);
  tr.Sleep := CrawlerTimer.Interval;
  tr.OnExecute := CrawlerTimerTimer;
  tr.Enabled := true;  }
end;

procedure TForm1.CrawlerTimerTimer(Sender: TObject);
begin
  Label1.Font.Color := clBlack;
  Label1.Update;

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
  cancrowl := false;
  CrawlerTimer.Enabled := false;
  db.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  CrawlerTimer.Interval := 6000 - (TrackBar1.Position - 5) * 500;
end;

end.
