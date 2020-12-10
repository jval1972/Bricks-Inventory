unit compare2sets;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TfrmCompare2Sets = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function Compare2SetsQuery(var set1, set2: string): boolean;

implementation

{$R *.dfm}

uses
  searchset;

function Compare2SetsQuery(var set1, set2: string): boolean;
var
  f: TfrmCompare2Sets;
begin
  Result := false;
  f := TfrmCompare2Sets.Create(nil);
  try
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      set1 := f.Edit1.Text;
      set2 := f.Edit2.Text;
      result := True;
    end;
  finally
    f.Free;
  end;
end;

procedure TfrmCompare2Sets.SpeedButton1Click(Sender: TObject);
var
  setid: string;
begin
  setid := Edit1.Text;
  if GetSetID(setid) then
    Edit1.Text := setid;
end;

procedure TfrmCompare2Sets.SpeedButton2Click(Sender: TObject);
var
  setid: string;
begin
  setid := Edit2.Text;
  if GetSetID(setid) then
    Edit2.Text := setid;
end;

end.
