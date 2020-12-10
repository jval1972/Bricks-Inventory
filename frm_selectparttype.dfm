object SelectPartTypeForm: TSelectPartTypeForm
  Left = 463
  Top = 211
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Select Part Type'
  ClientHeight = 291
  ClientWidth = 213
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 223
    Width = 213
    Height = 68
    Align = alBottom
    BevelOuter = bvNone
    Caption = ' '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object Button1: TButton
      Left = 48
      Top = 30
      Width = 65
      Height = 27
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 124
      Top = 30
      Width = 61
      Height = 27
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object RadioGroup1: TRadioGroup
    Left = 16
    Top = 16
    Width = 185
    Height = 201
    Caption = ' Part Type '
    Font.Charset = GREEK_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemIndex = 0
    Items.Strings = (
      '(Default)'
      'Part'
      'Set'
      'Minifigure'
      'Catalog'
      'Book'
      'Instructions'
      'Original Box'
      'Gear')
    ParentFont = False
    TabOrder = 1
  end
end
