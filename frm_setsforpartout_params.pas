unit frm_setsforpartout_params;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TQueryPartOutParametersForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    TrackBar1: TTrackBar;
    Label3: TLabel;
    TrackBar2: TTrackBar;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure UpdateControls;
  public
    { Public declarations }
  end;


var qpa_minyear: integer = 2000;
var qpa_minavailablelots: integer = 10;
var qpa_mindemand: double = 0.5;
var qpa_mincostmultiplier: double = 1.25;

function QueryPartOutParameters(
  var minyear: integer;
  var minavailablelots: integer;
  var mindemand: double;
  var mincostmultiplier: double): boolean;

implementation

{$R *.dfm}

uses
  bi_delphi;

function QueryPartOutParameters(
  var minyear: integer;
  var minavailablelots: integer;
  var mindemand: double;
  var mincostmultiplier: double): boolean;
var
  f: TQueryPartOutParametersForm;
begin
  result := False;
  f := TQueryPartOutParametersForm.Create(nil);
  try
    f.Edit1.Text := IntToStr(minyear);
    f.Edit2.Text := IntToStr(minavailablelots);
    f.TrackBar1.Position := Round(mindemand * f.TrackBar1.Max / 2);
    f.TrackBar2.Position := Round(mincostmultiplier * f.TrackBar2.Max / 2);
    f.UpdateControls;
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      minyear := atoi(f.Edit1.Text);
      minavailablelots := atoi(f.Edit2.Text);
      mindemand := f.TrackBar1.Position / f.TrackBar1.Max * 2;
      mincostmultiplier := f.TrackBar2.Position / f.TrackBar2.Max * 2;
      result := True;
    end;
  finally
    f.Free;
  end;
end;

procedure TQueryPartOutParametersForm.FormCreate(Sender: TObject);
begin
  UpdateControls;
end;

procedure TQueryPartOutParametersForm.UpdateControls;
begin
  Label5.Caption := Format('%2.2f', [TrackBar1.Position / TrackBar1.Max * 2]);
  Label6.Caption := Format('%2.2f', [TrackBar2.Position / TrackBar2.Max * 2]);
end;

procedure TQueryPartOutParametersForm.TrackBar1Change(Sender: TObject);
begin
  UpdateControls;
end;

procedure TQueryPartOutParametersForm.TrackBar2Change(Sender: TObject);
begin
  UpdateControls;
end;

end.
