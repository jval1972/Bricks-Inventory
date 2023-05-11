//------------------------------------------------------------------------------
//
//  BrickInventory: A tool for managing your brick collection
//  Copyright (C) 2014-2019 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//    Edit Bricklink Link
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit frm_editbllink;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons;

type
  TEditBricklinkLinkForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Panel1: TPanel;
    Label4: TLabel;
    Label2: TLabel;
    BricklinkPartEdit: TEdit;
    Label3: TLabel;
    BricklinkColorEdit: TEdit;
    PartTypeRadioGroup: TRadioGroup;
    CurrencyRadioGroup: TRadioGroup;
    VATCheckBox: TCheckBox;
    Label5: TLabel;
    LinkEdit: TEdit;
    Timer1: TTimer;
    CopySpeedButton: TSpeedButton;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    procedure BricklinkColorEditKeyPress(Sender: TObject; var Key: Char);
    procedure Timer1Timer(Sender: TObject);
    procedure CopySpeedButtonClick(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure SetLink(const value: string);
    function GetLink: string;
  public
    { Public declarations }
    partcolor: integer;
    procedure UpdateControls;
    property link: string read GetLink write SetLink;
  end;

function EditBricklinkLink(const parttype, part: string; const color: integer; var link: string): boolean;

implementation

{$R *.dfm}

uses
  bi_delphi, bi_utils, bi_db, bi_globals, bi_lugbulk2017;

function htmlgetparam(const src, param: string): string;
var
  src1, src2: string;
  sl: TStringList;
begin
  splitstring(src, src1, src2, '?');
  sl := string2stringlist(src2, '&');
  try
    sl.CaseSensitive := False;
    Result := sl.Values[param];
  finally
    sl.Free
  end;
end;

function htmlgetparamdef(const src, param, def: string): string;
begin
  Result := htmlgetparam(src, param);
  if Trim(Result) = '' then
    Result := def;
end;

function htmlsetparam(const src, param, value: string): string;
var
  src1, src2: string;
  sl: TStringList;
begin
  Result := '';
  splitstring(src, src1, src2, '?');
  sl := string2stringlist(src2, '&');
  try
    sl.CaseSensitive := False;
    sl.Values[param] := value;
    Result := src1 + '?' + stringlist2string(sl, '&');
  finally
    sl.Free
  end;
end;

function fixbllink(const src: string): string;
var
  src1, src2: string;
  sl: TStringList;
begin
  Result := src;
  if Pos(UpperCase('priceGuideSummary.asp'), UpperCase(src)) > 0 then
    Exit;

  if (Pos(UpperCase('catalogPriceGuide.asp'), UpperCase(src)) > 0) or (Pos(UpperCase('catalogPG.asp'), UpperCase(src)) > 0) then
  begin
    splitstring(src, src1, src2, '?');
    src2 := Trim(src2);
    if Length(src2) > 10 then
      if Pos(src2[1], 'PSMBGCIO') > 0 then
        if src2[2] = '=' then
          if Pos(UpperCase('&colorID='), UpperCase(src2)) > 0 then
          begin
            sl := string2stringlist(src2, '&');
            sl.CaseSensitive := False;
            Result := 'http://' + BL_NET + '/' + 'priceGuideSummary.asp' + '?vcID=2&vatInc=y&' +
                      'a=' + src2[1] + '&' +
                      'itemID=' + sl.Values[src2[1]] + '&' +
                      'colorID=' + sl.Values['colorID'] + '&' +
                      'viewdec=6';
            sl.Free;
            Exit;
          end;
  end;

  Result := src;
end;

function EditBricklinkLink(const parttype, part: string; const color: integer; var link: string): boolean;
var
  f: TEditBricklinkLinkForm;
  pci: TPieceColorInfo;
  tmp: string;
begin
  Result := False;

  f := TEditBricklinkLinkForm.Create(nil);
  try
    f.Caption := 'Bricklink Link - ' + part;
    f.Label1.Caption := db.PieceDesc(part);
    f.Label4.Visible := true;
    f.Label5.Visible := true;
    f.Panel1.Visible := true;
    f.Label4.Caption := db.colors(color).name;
    f.partcolor := color;
    pci := db.PieceColorInfo(part, color);
    if pci <> nil then
    begin
      PieceToImage(f.Image1, part, color);
      f.Panel1.Color := RGBInvert(db.colors(color).RGB);

      tmp := fixbllink(Trim(link));

      if (Pos('BRICKLINK.COM', UpperCase(tmp)) <= 0) or (Pos('CATALOGPG.ASP', UpperCase(tmp)) > 0) or (Pos('/PRICEGUIDE.ASP', UpperCase(tmp)) > 0) then
      begin
        if (parttype = 's') or (parttype = 'S') then
          tmp := 'http://' + BL_NET + '/' + 'priceGuideSummary.asp' + '?vcID=2&vatInc=y&' +
                 'a=S' + '&itemID=' + db.GetBLNetPieceName(pci.piece) +
                 '&colorID=0' + '&viewdec=6' + '&viewMy=&viewExclude=Y'
        else if (parttype = 'b') or (parttype = 'B') then
          tmp := 'http://' + BL_NET + '/' + 'priceGuideSummary.asp' + '?vcID=2&vatInc=y&' +
                 'a=B' + '&itemID=' + db.GetBLNetPieceName(pci.piece) +
                 '&colorID=0' + '&viewdec=6'
        else if (parttype = 'i') or (parttype = 'I') then
          tmp := 'http://' + BL_NET + '/' + 'priceGuideSummary.asp' + '?vcID=2&vatInc=y&' +
                 'a=I' + '&itemID=' + db.GetBLNetPieceName(pci.piece) +
                 '&colorID=0' + '&viewdec=6'
        else if (parttype = 'o') or (parttype = 'O') then
          tmp := 'http://' + BL_NET + '/' + 'priceGuideSummary.asp' + '?vcID=2&vatInc=y&' +
                 'a=O' + '&itemID=' + db.GetBLNetPieceName(pci.piece) +
                 '&colorID=0' + '&viewdec=6'
        else if (parttype = 'c') or (parttype = 'C') then
          tmp := 'http://' + BL_NET + '/' + 'priceGuideSummary.asp' + '?vcID=2&vatInc=y&' +
                 'a=C' + '&itemID=' + db.GetBLNetPieceName(pci.piece) +
                 '&colorID=0' + '&viewdec=6'
        else
          tmp := 'http://' + BL_NET + '/' + 'priceGuideSummary.asp' + '?vcID=2&vatInc=y&' +
                 'a=' + parttype + '&itemID=' + db.GetBLNetPieceName(pci.piece) +
                 '&colorID=' + itoa(db.colors(color).BrickLinkColor) + '&viewdec=6';
      end;

      f.link := tmp;
      f.Timer1.Enabled := True;
      f.UpdateControls;

      f.ShowModal;
      if f.ModalResult = mrOK then
      begin
        link := f.link;
        Result := True;
      end;
    end;

  finally
    f.Free;
  end;
end;

procedure TEditBricklinkLinkForm.BricklinkColorEditKeyPress(
  Sender: TObject; var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
  begin
    Key := #0;
    Exit;
  end;
end;

procedure TEditBricklinkLinkForm.SetLink(const value: string);
var
  pp: string;
begin
  BricklinkPartEdit.Text := htmlgetparamdef(value, 'itemID', BricklinkPartEdit.Text);
  BricklinkColorEdit.Text := htmlgetparamdef(value, 'colorID', BricklinkColorEdit.Text);
  VATCheckBox.Checked := UpperCase(htmlgetparamdef(value, 'vatInc', 'y')) = 'Y';
  if htmlgetparamdef(value, 'vcID', '2') = '2' then
    CurrencyRadioGroup.ItemIndex := 1
  else
    CurrencyRadioGroup.ItemIndex := 0;
  pp := UpperCase(htmlgetparamdef(value, 'a', 'p'));
  if Length(pp) = 1 then
  begin
    case pp[1] of
      'S': PartTypeRadioGroup.ItemIndex := 1;
      'M': PartTypeRadioGroup.ItemIndex := 2;
      'B': PartTypeRadioGroup.ItemIndex := 3;
      'G': PartTypeRadioGroup.ItemIndex := 4;
      'C': PartTypeRadioGroup.ItemIndex := 5;
      'I': PartTypeRadioGroup.ItemIndex := 6;
      'O': PartTypeRadioGroup.ItemIndex := 7;
    else
      PartTypeRadioGroup.ItemIndex := 0;
    end;
  end
  else
    PartTypeRadioGroup.ItemIndex := 0;
end;

function TEditBricklinkLinkForm.GetLink: string;
const
  C_PARTTYPES = 'PSMBGCIO';
var
  vcID: string;
  vatInc: string;
  a: string;
  itemID: string;
  colorID: string;
begin
  if CurrencyRadioGroup.ItemIndex = 1 then
    vcID := '2'
  else
    vcID := '1';
  if VATCheckBox.Checked then
    vatInc := 'y'
  else
    vatInc := 'n';
  a := 'P';
  if (PartTypeRadioGroup.ItemIndex >= 0) and (PartTypeRadioGroup.ItemIndex < 7) then
    a := C_PARTTYPES[PartTypeRadioGroup.ItemIndex + 1];
  itemID := BricklinkPartEdit.Text;
  if PartTypeRadioGroup.ItemIndex in [1, 2, 3, 5, 6, 7] then // Set, minifig, book, catalog, instructions, original box
    colorID := '0'
  else
    colorID := itoa(atoi(BricklinkColorEdit.Text));
  Result := 'http://' + BL_NET + '/' + 'priceGuideSummary.asp' + '?' +
            'vcID=' + vcID + '&' +
            'vatInc=' + vatInc + '&' +
            'a=' + a + '&' +
            'itemID=' + itemID + '&' +
            'colorID=' + colorID + '&' +
            'viewdec=6';
  if PartTypeRadioGroup.ItemIndex = 1 then // Set
    Result := Result + '&viewMy=&viewExclude=Y';
end;

procedure TEditBricklinkLinkForm.UpdateControls;
  function _GetBlColor(const scolor: integer): integer;
  begin
    if scolor <= LASTNORMALCOLORINDEX then
      Result := db.Colors(scolor).BrickLinkColor
    else
      Result := 0;
  end;
begin
  LinkEdit.Text := GetLink;
  if atoi(BricklinkColorEdit.Text) <> _GetBlColor(partcolor) then
  begin
    BricklinkColorEdit.Color := clRed;
    BricklinkColorEdit.Font.Color := clWhite;
  end
  else
  begin
    BricklinkColorEdit.Color := clWindow;
    BricklinkColorEdit.Font.Color := clWindowText;
  end;
end;

procedure TEditBricklinkLinkForm.Timer1Timer(Sender: TObject);
begin
  UpdateControls;
end;

procedure TEditBricklinkLinkForm.CopySpeedButtonClick(Sender: TObject);
begin
  SetClipboardTextWideString(GetLink);
end;

procedure TEditBricklinkLinkForm.Image1DblClick(Sender: TObject);
begin
  try
    if Image1.Align = alClient then
    begin
      Image1.Align := alNone;
      Image1.AutoSize := True;
      Image1.Stretch := False;
    end
    else
    begin
      Image1.Align := alClient;
      Image1.AutoSize := False;
      Image1.Stretch := True;
    end;
  except
  end;
end;

procedure TEditBricklinkLinkForm.FormCreate(Sender: TObject);
begin
  partcolor := 0;
end;

end.
