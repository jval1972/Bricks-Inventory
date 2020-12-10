unit frm_selectsets;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, CheckLst;

type
  TSelectSetsForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel3: TPanel;
    CheckListBox1: TCheckListBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function SelectSets(const lst: TStringList): boolean;

implementation

{$R *.dfm}

function SelectSets(const lst: TStringList): boolean;
var
  f: TSelectSetsForm;
  i: integer;
begin
  result := false;
  f := TSelectSetsForm.Create(nil);
  try
    f.CheckListBox1.Items.Clear;
    f.CheckListBox1.Items.AddStrings(lst);
    for i := 0 to f.CheckListBox1.Items.Count - 1 do
      f.CheckListBox1.Checked[i] := true;

    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      lst.Clear;
      for i := 0 to f.CheckListBox1.Items.Count - 1 do
        if f.CheckListBox1.Checked[i] then
          lst.Add(f.CheckListBox1.Items.Strings[i]);
      result := true;
    end;
  finally
    f.Free;
  end;
end;

end.
