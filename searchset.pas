unit searchset;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TSearchSetForm = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    ListBox1: TListBox;
    Panel1: TPanel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function GetSetID(var setid: string): boolean;

implementation

{$R *.dfm}

uses
  bi_db, bi_globals, bi_delphi;

function GetSetID(var setid: string): boolean;
var
  f: TSearchSetForm;
  idx: integer;
begin
  Result := false;
  f := TSearchSetForm.Create(nil);
  try
    f.Edit1.Text := setid;
    idx := f.ListBox1.Items.IndexOf(setid);
    if idx >= 0 then
      f.ListBox1.ItemIndex := idx;
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      result := True;
      setid := f.Edit1.Text;
    end;
  finally
    f.Free;
  end;
end;

procedure TSearchSetForm.FormCreate(Sender: TObject);
begin
  Label2.Caption := '';
  
  if db = nil then
    Exit;

  Label2.Caption := itoa(db.AllSets.Count) + ' sets found';
  ListBox1.Items.Clear;
  ListBox1.Items.AddStrings(db.AllSets);
end;

procedure TSearchSetForm.ListBox1Click(Sender: TObject);
begin
  if ListBox1.Itemindex >= 0 then
    Edit1.Text := ListBox1.Items.Strings[ListBox1.Itemindex];
end;

end.
