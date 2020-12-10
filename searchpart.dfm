object SearchPartForm: TSearchPartForm
  Left = 305
  Top = 126
  BorderStyle = bsDialog
  Caption = 'Seach part'
  ClientHeight = 569
  ClientWidth = 231
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 18
    Top = 6
    Width = 61
    Height = 16
    Caption = 'Enter Part:'
    FocusControl = Edit1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Edit1: TEdit
    Left = 94
    Top = 6
    Width = 123
    Height = 24
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object ListBox1: TListBox
    Left = 12
    Top = 36
    Width = 205
    Height = 421
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 16
    ParentFont = False
    TabOrder = 1
    OnClick = ListBox1Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 501
    Width = 231
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
    TabOrder = 2
    object Label2: TLabel
      Left = 16
      Top = 5
      Width = 41
      Height = 16
      Caption = 'Label2'
    end
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
  object SaveListButton: TButton
    Left = 16
    Top = 472
    Width = 193
    Height = 25
    Caption = 'Save List'
    Font.Charset = GREEK_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = SaveListButtonClick
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 176
    Top = 448
  end
end
