unit searchpart;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TSearchPartForm = class(TForm)
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

function GetPieceID(var pieceid: string): boolean;

implementation

{$R *.dfm}

uses
  bi_db, bi_globals, bi_delphi;

function GetPieceID(var pieceid: string): boolean;
var
  f: TSearchPartForm;
  idx: integer;
begin
  Result := false;
  f := TSearchPartForm.Create(nil);
  try
    f.Edit1.Text := pieceid;
    idx := f.ListBox1.ItemIndex;
    if idx >= 0 then
      f.ListBox1.ItemIndex := idx;
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      result := True;
      pieceid := f.Edit1.Text;
    end;
  finally
    f.Free;
  end;
end;

procedure TSearchPartForm.FormCreate(Sender: TObject);
begin
  Label2.Caption := '';

  if db = nil then
    Exit;

  Label2.Caption := itoa(db.AllPieces.Count) + ' pieces found';
  ListBox1.Items.Clear;
  ListBox1.Items.AddStrings(db.AllPieces);
end;

procedure TSearchPartForm.ListBox1Click(Sender: TObject);
begin
  if ListBox1.Itemindex >= 0 then
    Edit1.Text := ListBox1.Items.Strings[ListBox1.Itemindex];
end;

end.
