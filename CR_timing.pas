unit CR_timing;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, bi_threadtimer, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button1: TButton;
    TrackBar1: TTrackBar;
    CheckBox1: TCheckBox;
    CrawlerTimer: TTimer;
    Panel2: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Memo1: TMemo;
    TabSheet2: TTabSheet;
    Memo2: TMemo;
    TabSheet3: TTabSheet;
    Label5: TLabel;
    Edit1: TEdit;
    Label6: TLabel;
    Edit2: TEdit;
    StatisticsLabel: TLabel;
    CheckBox2: TCheckBox;
    SuccessLabel: TLabel;
    CheckBox3: TCheckBox;
    Label7: TLabel;
    Edit3: TEdit;
    CheckBoxRandom: TCheckBox;
    TrackBarRandom: TTrackBar;
    RetryOnFailCheckBox: TCheckBox;
    Label8: TLabel;
    Edit4: TEdit;
    SpeedLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CrawlerTimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure CheckBoxRandomClick(Sender: TObject);
    procedure RetryOnFailCheckBoxClick(Sender: TObject);
  private
    { Private declarations }
 //   tr: TThreadComponent;
    doabort: boolean;
    cancrowl: boolean;
    cnt_total, cnt_success: integer;
    cont_success, cont_fail: integer;
    last_time: extended;
    last_total: integer;
    procedure CrawlerStep;
    procedure UpdateControls;
  public
    { Public declarations }
    crawling: Boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  bi_db, bi_globals, bi_crawler, bi_tmp, bi_multithread, bi_system;

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
    Memo2.Lines.Clear;
    db := TSetsDatabase.Create;
    db.InitCreate(crfile);
    db.LoadFromDisk(basedefault + 'db\db_set_pieces.txt');
    crawling := false;
    cancrowl := true;
    Edit3.Text := IntToStr(db.crawlerpriority.Count);
    Edit3.Update;
  end
  else
  begin
    db := nil;
    Label1.Caption := '';
    Memo1.Lines.Clear;
    Memo2.Lines.Clear;
    CrawlerTimer.Interval := 1;
    Close;
  end;

  UpdateControls;

  cnt_total := 0;
  cnt_success := 0;

  cont_success := 0;
  cont_fail := 0;

  last_time := 0.0;
  last_total := 0;

  CrawlerTimer.Enabled := true;
 { tr := TThreadComponent.Create(nil);
  tr.Sleep := CrawlerTimer.Interval;
  tr.OnExecute := CrawlerTimerTimer;
  tr.Enabled := true;  }
end;

procedure TForm1.CrawlerStep;
var
  now_time: extended;
begin
  if doabort then
  begin
    Close;
    Exit;
  end;

  if last_time = 0.0 then
    last_time := I_GetSysTime;

  Sleep(0);

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

    UpdateControls;

    crawling := true;
    Label1.Font.Color := clMaroon;
    Label1.Update;
    db.Crawler;
    if USDwarning then
    begin
      if not CheckBox2.Checked then
        Beep;
      Memo1.Lines.Add('Warning - USD values (' + db.lastcrawlpiece + ') - time=' + FormatDateTime('c', Now));
    end;

    if FAILwarning then
    begin
      cont_success := 0;
      inc(cont_fail);
    end
    else
    begin
      inc(cont_success);
      cont_fail := 0;
    end;

    if FAILwarning then
    begin
      if not CheckBox2.Checked then
        Beep;
      Memo1.Lines.Add('Warning - Failed to access priceguide (' + db.lastcrawlpiece + ') - time=' + FormatDateTime('c', Now));
    end;
    while Memo1.Lines.Count > 210 do
    repeat
      Memo1.Lines.Delete(0);
    until Memo1.Lines.Count <= 200;
    if not FAILwarning then
    begin
      inc(cnt_success);
      Memo2.Lines.Add('Read priceguide: ' + db.lastcrawlpiece);
    end;
    while Memo2.Lines.Count > 210 do
    repeat
      Memo2.Lines.Delete(0);
    until Memo2.Lines.Count <= 200;
    inc(cnt_total);

    Label1.Font.Color := clBlack;
    Label1.Update;
    Label2.Caption := db.lastcrawlpiece;
    Label2.Update;
    crawling := false;
    Edit1.Text := IntToStr(type1_pg_hits);
    Edit1.Update;
    Edit2.Text := IntToStr(type2_pg_hits);
    Edit2.Update;
    Edit3.Text := IntToStr(db.crawlerpriority.Count);
    Edit3.Update;
    Edit4.Text := IntToStr(retry_success);
    Edit4.Update;

    if cnt_total > 0 then
    begin
      StatisticsLabel.Caption := 'Success hits = ' + IntToStr(cnt_success) + '/' + IntToStr(cnt_total) + Format(' (%2.3f%s)', [cnt_success / cnt_total * 100, '%']);
      StatisticsLabel.Update;
      now_time := I_GetSysTime;
      if now_time - last_time > 5.0 then
      begin
        SpeedLabel.Caption := IntToStr(Round(60 * (cnt_total - last_total) / (now_time - last_time))) + ' hits/min';
        SpeedLabel.Update;
        last_time := now_time;
        last_total := cnt_total;
      end;
    end;

    if cont_success > 0 then
    begin
      SuccessLabel.Caption := Format('%d continuous hits succeded', [cont_success]);
      SuccessLabel.Font.Color := RGB(0, 128, 0);
      SuccessLabel.Update;
    end
    else if cont_fail > 0 then
    begin
      SuccessLabel.Caption := Format('%d continuous hits failed', [cont_fail]);
      SuccessLabel.Font.Color := RGB(255, 0, 0);
      SuccessLabel.Update;
    end;
  except
    crawling := false;
    cancrowl := true;
    Memo1.Lines.Add('Exception at ' + FormatDateTime('c', Now));
    if Memo1.Lines.Count > 210 then
    repeat
      Memo1.Lines.Delete(0);
    until Memo1.Lines.Count <= 200;
  end;
end;

procedure TForm1.CrawlerTimerTimer(Sender: TObject);
begin
  CrawlerStep;
  if not CheckBox3.Checked then
  begin
    if (last_pg_hit = 2) {and (not FAILWarning)} then
    begin
      CrawlerStep;
      if (last_pg_hit = 2) {and (not FAILWarning)} then
      begin
        CrawlerStep;
        if (last_pg_hit = 2) {and (not FAILWarning)} then
        begin
          CrawlerStep;
          if (last_pg_hit = 2) {and (not FAILWarning)} then
          begin
            CrawlerStep;
          end;
        end;
      end;
    end;
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

procedure TForm1.UpdateControls;
begin
  TrackBarRandom.Visible := CheckBoxRandom.Checked;
  if db <> nil then
  begin
    if CheckBoxRandom.Checked then
      db.crawlerrandom := TrackBarRandom.Position
    else
      db.crawlerrandom := 0;
    allow_crawler_retry := RetryOnFailCheckBox.Checked;
  end;
end;

procedure TForm1.CheckBoxRandomClick(Sender: TObject);
begin
  UpdateControls;
end;

procedure TForm1.RetryOnFailCheckBoxClick(Sender: TObject);
begin
  UpdateControls;
end;

end.
