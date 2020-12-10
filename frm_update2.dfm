object UpdatePartNamesRebrForm: TUpdatePartNamesRebrForm
  Left = 461
  Top = 254
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Update Part Names (rebrickable.com)'
  ClientHeight = 403
  ClientWidth = 292
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 335
    Width = 292
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
      Left = 56
      Top = 30
      Width = 77
      Height = 27
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 156
      Top = 30
      Width = 77
      Height = 27
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 292
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 1
    object Button3: TButton
      Left = 32
      Top = 16
      Width = 225
      Height = 25
      Caption = 'Add Unknown Part Names'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = Button3Click
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 49
    Width = 292
    Height = 286
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 8
    Caption = ' '
    TabOrder = 2
    object Memo1: TMemo
      Left = 8
      Top = 33
      Width = 276
      Height = 245
      Align = alClient
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
    object Panel4: TPanel
      Left = 8
      Top = 8
      Width = 276
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      Caption = ' '
      TabOrder = 1
      object Label6: TLabel
        Left = 0
        Top = 1
        Width = 29
        Height = 16
        Caption = ' List: '
        FocusControl = Memo1
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label7: TLabel
        Left = 48
        Top = 1
        Width = 61
        Height = 16
        Caption = '(0 actions)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
    end
  end
end
