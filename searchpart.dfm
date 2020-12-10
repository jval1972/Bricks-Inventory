object SearchPartForm: TSearchPartForm
  Left = 305
  Top = 126
  Width = 272
  Height = 198
  Caption = 'Seach part'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 18
    Top = 6
    Width = 49
    Height = 12
    Caption = 'Enter Part:'
    FocusControl = Edit1
  end
  object Edit1: TEdit
    Left = 78
    Top = 6
    Width = 91
    Height = 24
    TabOrder = 0
  end
  object ListBox1: TListBox
    Left = 12
    Top = 36
    Width = 229
    Height = 73
    ItemHeight = 12
    TabOrder = 1
    OnClick = ListBox1Click
  end
  object Button1: TButton
    Left = 54
    Top = 126
    Width = 56
    Height = 19
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object Button2: TButton
    Left = 132
    Top = 126
    Width = 56
    Height = 19
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
