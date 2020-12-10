unit compare2sets;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmCompare2Sets = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function Compare2SetsQuery(var set1, set2: string): boolean;

implementation

{$R *.dfm}

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

end.
