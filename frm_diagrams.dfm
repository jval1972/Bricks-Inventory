object DiagramForm: TDiagramForm
  Left = 41
  Top = 40
  Width = 1261
  Height = 659
  BorderIcons = [biSystemMenu]
  Caption = 'Diagrams'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 240
    Top = 16
    Width = 329
    Height = 81
    AutoSize = False
    Caption = '       '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object Label4: TLabel
    Left = 280
    Top = 96
    Width = 297
    Height = 25
    AutoSize = False
    Caption = '       '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 240
    Top = 144
    Width = 76
    Height = 16
    Caption = 'Num Pieces:'
    FocusControl = Edit1
    Font.Charset = GREEK_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Chart1: TChart
    Left = 16
    Top = 184
    Width = 865
    Height = 409
    Cursor = crCross
    AnimatedZoom = True
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Title.Text.Strings = (
      'Diagram')
    BottomAxis.AxisValuesFormat = '#,##0.####'
    DepthAxis.AxisValuesFormat = '#,##0.####'
    LeftAxis.AxisValuesFormat = '#,##0.####'
    RightAxis.AxisValuesFormat = '#,##0.####'
    TopAxis.AxisValuesFormat = '#,##0.####'
    View3D = False
    TabOrder = 0
    object Series1: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clRed
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1.000000000000000000
      XValues.Order = loAscending
      YValues.DateTime = False
      YValues.Name = 'Y'
      YValues.Multiplier = 1.000000000000000000
      YValues.Order = loNone
    end
  end
  object Panel1: TPanel
    Left = 240
    Top = 96
    Width = 33
    Height = 33
    Caption = ' '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 328
    Top = 144
    Width = 249
    Height = 24
    BiDiMode = bdLeftToRight
    Font.Charset = GREEK_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentBiDiMode = False
    ParentFont = False
    ReadOnly = True
    TabOrder = 2
    Text = '0'
  end
  object Notebook1: TNotebook
    Left = 888
    Top = 0
    Width = 337
    Height = 601
    PageIndex = 1
    TabOrder = 3
    object TPage
      Left = 0
      Top = 0
      Caption = 'page1'
      object Button30: TButton
        Left = 24
        Top = 16
        Width = 137
        Height = 25
        Caption = 'PG_nAvgPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = Button30Click
      end
      object Button31: TButton
        Left = 24
        Top = 54
        Width = 137
        Height = 25
        Caption = 'PG_nQtyAvgPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = Button31Click
      end
      object Button32: TButton
        Left = 24
        Top = 93
        Width = 137
        Height = 25
        Caption = 'PG_uAvgPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = Button32Click
      end
      object Button33: TButton
        Left = 24
        Top = 133
        Width = 137
        Height = 25
        Caption = 'PG_uQtyAvgPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = Button33Click
      end
      object Button34: TButton
        Left = 184
        Top = 16
        Width = 137
        Height = 25
        Caption = 'AV_nAvgPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        OnClick = Button34Click
      end
      object Button35: TButton
        Left = 184
        Top = 54
        Width = 137
        Height = 25
        Caption = 'AV_nQtyAvgPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
        OnClick = Button35Click
      end
      object Button36: TButton
        Left = 184
        Top = 93
        Width = 137
        Height = 25
        Caption = 'AV_uAvgPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 6
        OnClick = Button36Click
      end
      object Button37: TButton
        Left = 184
        Top = 133
        Width = 137
        Height = 25
        Caption = 'AV_uQtyAvgPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 7
        OnClick = Button37Click
      end
      object Button38: TButton
        Left = 24
        Top = 173
        Width = 137
        Height = 25
        Caption = 'N_Demand'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 8
        OnClick = Button38Click
      end
      object Button39: TButton
        Left = 24
        Top = 213
        Width = 137
        Height = 25
        Caption = 'U_Demand'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 9
        OnClick = Button39Click
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'page2'
      object Button1: TButton
        Left = 24
        Top = 16
        Width = 137
        Height = 25
        Caption = 'PG_nTimesSold'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 24
        Top = 54
        Width = 137
        Height = 25
        Caption = 'PG_nTotalQty'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 24
        Top = 93
        Width = 137
        Height = 25
        Caption = 'PG_nMinPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 24
        Top = 131
        Width = 137
        Height = 25
        Caption = 'PG_nAvgPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = Button4Click
      end
      object Button5: TButton
        Left = 24
        Top = 170
        Width = 137
        Height = 25
        Caption = 'PG_nQtyAvgPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        OnClick = Button5Click
      end
      object Button6: TButton
        Left = 24
        Top = 208
        Width = 137
        Height = 25
        Caption = 'PG_nMaxPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
        OnClick = Button6Click
      end
      object Button7: TButton
        Left = 24
        Top = 247
        Width = 137
        Height = 25
        Caption = 'PG_uTimesSold'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 6
        OnClick = Button7Click
      end
      object Button8: TButton
        Left = 24
        Top = 285
        Width = 137
        Height = 25
        Caption = 'PG_uTotalQty'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 7
        OnClick = Button8Click
      end
      object Button9: TButton
        Left = 24
        Top = 324
        Width = 137
        Height = 25
        Caption = 'PG_uMinPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 8
        OnClick = Button9Click
      end
      object Button10: TButton
        Left = 24
        Top = 362
        Width = 137
        Height = 25
        Caption = 'PG_uAvgPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 9
        OnClick = Button10Click
      end
      object Button11: TButton
        Left = 24
        Top = 401
        Width = 137
        Height = 25
        Caption = 'PG_uQtyAvgPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 10
        OnClick = Button11Click
      end
      object Button12: TButton
        Left = 24
        Top = 440
        Width = 137
        Height = 25
        Caption = 'PG_uMaxPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 11
        OnClick = Button12Click
      end
      object Button13: TButton
        Left = 184
        Top = 16
        Width = 137
        Height = 25
        Caption = 'AV_nTotalLots'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 12
        OnClick = Button13Click
      end
      object Button14: TButton
        Left = 184
        Top = 54
        Width = 137
        Height = 25
        Caption = 'AV_nTotalQty'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 13
        OnClick = Button14Click
      end
      object Button15: TButton
        Left = 184
        Top = 93
        Width = 137
        Height = 25
        Caption = 'AV_nMinPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 14
        OnClick = Button15Click
      end
      object Button16: TButton
        Left = 184
        Top = 131
        Width = 137
        Height = 25
        Caption = 'AV_nAvgPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 15
        OnClick = Button16Click
      end
      object Button17: TButton
        Left = 184
        Top = 170
        Width = 137
        Height = 25
        Caption = 'AV_nQtyAvgPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 16
        OnClick = Button17Click
      end
      object Button18: TButton
        Left = 184
        Top = 208
        Width = 137
        Height = 25
        Caption = 'AV_nMaxPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 17
        OnClick = Button18Click
      end
      object Button19: TButton
        Left = 184
        Top = 247
        Width = 137
        Height = 25
        Caption = 'AV_uTotalLots'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 18
        OnClick = Button19Click
      end
      object Button20: TButton
        Left = 184
        Top = 285
        Width = 137
        Height = 25
        Caption = 'AV_uTotalQty'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 19
        OnClick = Button20Click
      end
      object Button21: TButton
        Left = 184
        Top = 324
        Width = 137
        Height = 25
        Caption = 'AV_uMinPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 20
        OnClick = Button21Click
      end
      object Button22: TButton
        Left = 184
        Top = 362
        Width = 137
        Height = 25
        Caption = 'AV_uAvgPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 21
        OnClick = Button22Click
      end
      object Button23: TButton
        Left = 184
        Top = 401
        Width = 137
        Height = 25
        Caption = 'AV_uQtyAvgPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 22
        OnClick = Button23Click
      end
      object Button24: TButton
        Left = 184
        Top = 440
        Width = 137
        Height = 25
        Caption = 'AV_uMaxPrice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 23
        OnClick = Button24Click
      end
      object Button25: TButton
        Left = 24
        Top = 480
        Width = 137
        Height = 25
        Caption = 'N_Demand'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 24
        OnClick = Button25Click
      end
      object Button26: TButton
        Left = 24
        Top = 520
        Width = 137
        Height = 25
        Caption = 'U_Demand'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 25
        OnClick = Button26Click
      end
      object Button27: TButton
        Left = 24
        Top = 560
        Width = 137
        Height = 25
        Caption = 'My Inventory'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 26
        OnClick = Button27Click
      end
      object Button28: TButton
        Left = 184
        Top = 480
        Width = 137
        Height = 25
        Caption = 'N_partout'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 27
        OnClick = Button28Click
      end
      object Button29: TButton
        Left = 184
        Top = 520
        Width = 137
        Height = 25
        Caption = 'U_partout'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 28
        OnClick = Button29Click
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'page3'
    end
  end
  object ScrollBox1: TScrollBox
    Left = 16
    Top = 8
    Width = 185
    Height = 169
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 105
      Height = 105
      AutoSize = True
      OnDblClick = Image1DblClick
    end
  end
end
