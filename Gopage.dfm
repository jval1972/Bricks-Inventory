object GoPageForm: TGoPageForm
  Left = 305
  Top = 180
  ActiveControl = PageNum
  BorderStyle = bsDialog
  Caption = 'Go to Page Number'
  ClientHeight = 116
  ClientWidth = 231
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 11
    Top = 8
    Width = 209
    Height = 57
    Shape = bsFrame
    IsControl = True
  end
  object OKBtn: TBitBtn
    Left = 35
    Top = 76
    Width = 77
    Height = 27
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Kind = bkOK
    Spacing = 3
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 119
    Top = 76
    Width = 82
    Height = 27
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Kind = bkCancel
    Spacing = 3
    IsControl = True
  end
  object PageNum: TSpinEdit
    Left = 87
    Top = 27
    Width = 57
    Height = 26
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    MaxValue = 0
    MinValue = 1
    ParentFont = False
    TabOrder = 0
    Value = 1
    OnEnter = PageNumEnter
    OnKeyDown = PageNumKeyDown
  end
end
