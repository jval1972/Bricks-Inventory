unit frm_lugbulksuggest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TLugbulkSuggestForm = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    TrackBar1: TTrackBar;
    Label2: TLabel;
    Label3: TLabel;
    TrackBar2: TTrackBar;
    Label4: TLabel;
    TrackBar3: TTrackBar;
    FEDIT1: TEdit;
    FEDIT2: TEdit;
    FEDIT3: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
  private
    { Private declarations }
    procedure DoUpdate;
  public
    { Public declarations }
  end;

function GetLugbulkSuggestParams(var years: string; var demand, sold: integer; var price: double): boolean;

implementation

{$R *.dfm}

uses
  bi_delphi;

procedure TLugbulkSuggestForm.FormCreate(Sender: TObject);
var
  y: integer;
begin
  y := atoi(FormatDateTime('yyyy', Now));
  Edit1.Text := itoa(y - 1) + ' ' + itoa(y);
  DoUpdate;
end;

procedure TLugbulkSuggestForm.DoUpdate;
begin
  FEDIT1.Text := itoa(Trackbar1.Position);
  FEDIT2.Text := itoa(Trackbar2.Position * 1000);
  FEDIT3.Text := itoa(Trackbar3.Position);
end;

procedure TLugbulkSuggestForm.TrackBar1Change(Sender: TObject);
begin
  DoUpdate;
end;

procedure TLugbulkSuggestForm.TrackBar2Change(Sender: TObject);
begin
  DoUpdate;
end;

procedure TLugbulkSuggestForm.TrackBar3Change(Sender: TObject);
begin
  DoUpdate;
end;

function GetLugbulkSuggestParams(var years: string; var demand, sold: integer; var price: double): boolean;
var
  f: TLugbulkSuggestForm;
begin
  result := false;
  f := TLugbulkSuggestForm.Create(nil);
  try
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      result := true;
      years := StringReplace(f.Edit1.Text, '/', ' ', [rfReplaceAll]);
      years := StringReplace(years, '\', ' ', [rfReplaceAll]);
      demand := f.TrackBar1.Position;
      sold := f.TrackBar2.Position * 1000;
      price := f.TrackBar3.Position;
    end;
  finally
    f.Free;
  end;
end;

end.
