unit frm_selectparttype;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TSelectPartTypeForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    RadioGroup1: TRadioGroup;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function SelectPartType(var pt: char): boolean;

implementation

{$R *.dfm}

uses
  bi_delphi, bi_db;

function SelectPartType(var pt: char): boolean;
var
  f: TSelectPartTypeForm;
  sparttype: string;
  i: integer;
  pt2: char;
begin
  Result := False;

  f := TSelectPartTypeForm.Create(nil);
  try
    sparttype := strupper(PartTypeToPartTypeName(pt));
    f.RadioGroup1.ItemIndex := 0;
    for i := 0 to f.RadioGroup1.Items.Count - 1 do
      if strupper(f.RadioGroup1.Items.Strings[i]) = sparttype then
      begin
        f.RadioGroup1.ItemIndex := i;
        Break;
      end;
    f.ShowModal;
    if f.ModalResult = mrOK then
      if f.RadioGroup1.ItemIndex > -1 then
      begin
        pt2 := PartTypeNameToPartType(f.RadioGroup1.Items.Strings[f.RadioGroup1.ItemIndex]);
        if pt <> pt2 then
        begin
          pt := pt2;
          Result := True;
        end;
      end;
  finally
    f.Free;
  end;
end;

end.
