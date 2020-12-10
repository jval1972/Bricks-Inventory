unit frm_editsetastext;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls;

type
  TEditSetAsTextForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function EditSetAsTextForm(const setid: string; var data: string; var desc: string; var year: integer): boolean;

implementation

{$R *.dfm}

uses bi_delphi;

function EditSetAsTextForm(const setid: string; var data: string; var desc: string; var year: integer): boolean;
var
  f: TEditSetAsTextForm;
begin
  result := false;
  f := TEditSetAsTextForm.Create(nil);
  try
    f.Caption := f.Caption + ' - ' + setid;
    f.Memo1.Text := data;
    f.Edit1.Text := desc;
    f.Edit2.Text := itoa(year);
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      data := f.Memo1.Text;
      desc := f.Edit1.Text;
      year := atoi(trim(f.Edit2.Text));
      result := true;
    end;
  finally
    f.Free;
  end;
end;

end.
