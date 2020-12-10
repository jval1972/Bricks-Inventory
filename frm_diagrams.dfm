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
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 136
    Top = 16
    Width = 329
    Height = 81
    AutoSize = False
    Caption = '       '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object Label4: TLabel
    Left = 176
    Top = 96
    Width = 297
    Height = 25
    AutoSize = False
    Caption = '       '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object Image1: TImage
    Left = 16
    Top = 16
    Width = 105
    Height = 105
    AutoSize = True
  end
  object Label2: TLabel
    Left = 32
    Top = 144
    Width = 58
    Height = 13
    Caption = 'Num Pieces:'
    FocusControl = Edit1
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
    Left = 136
    Top = 96
    Width = 33
    Height = 33
    Caption = ' '
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 96
    Top = 144
    Width = 273
    Height = 21
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
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
        TabOrder = 0
        OnClick = Button30Click
      end
      object Button31: TButton
        Left = 24
        Top = 54
        Width = 137
        Height = 25
        Caption = 'PG_nQtyAvgPrice'
        TabOrder = 1
        OnClick = Button31Click
      end
      object Button32: TButton
        Left = 24
        Top = 93
        Width = 137
        Height = 25
        Caption = 'PG_uAvgPrice'
        TabOrder = 2
        OnClick = Button32Click
      end
      object Button33: TButton
        Left = 24
        Top = 133
        Width = 137
        Height = 25
        Caption = 'PG_uQtyAvgPrice'
        TabOrder = 3
        OnClick = Button33Click
      end
      object Button34: TButton
        Left = 184
        Top = 16
        Width = 137
        Height = 25
        Caption = 'AV_nAvgPrice'
        TabOrder = 4
        OnClick = Button34Click
      end
      object Button35: TButton
        Left = 184
        Top = 54
        Width = 137
        Height = 25
        Caption = 'AV_nQtyAvgPrice'
        TabOrder = 5
        OnClick = Button35Click
      end
      object Button36: TButton
        Left = 184
        Top = 93
        Width = 137
        Height = 25
        Caption = 'AV_uAvgPrice'
        TabOrder = 6
        OnClick = Button36Click
      end
      object Button37: TButton
        Left = 184
        Top = 133
        Width = 137
        Height = 25
        Caption = 'AV_uQtyAvgPrice'
        TabOrder = 7
        OnClick = Button37Click
      end
      object Button38: TButton
        Left = 24
        Top = 173
        Width = 137
        Height = 25
        Caption = 'N_Demand'
        TabOrder = 8
        OnClick = Button38Click
      end
      object Button39: TButton
        Left = 24
        Top = 213
        Width = 137
        Height = 25
        Caption = 'U_Demand'
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
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 24
        Top = 54
        Width = 137
        Height = 25
        Caption = 'PG_nTotalQty'
        TabOrder = 1
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 24
        Top = 93
        Width = 137
        Height = 25
        Caption = 'PG_nMinPrice'
        TabOrder = 2
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 24
        Top = 131
        Width = 137
        Height = 25
        Caption = 'PG_nAvgPrice'
        TabOrder = 3
        OnClick = Button4Click
      end
      object Button5: TButton
        Left = 24
        Top = 170
        Width = 137
        Height = 25
        Caption = 'PG_nQtyAvgPrice'
        TabOrder = 4
        OnClick = Button5Click
      end
      object Button6: TButton
        Left = 24
        Top = 208
        Width = 137
        Height = 25
        Caption = 'PG_nMaxPrice'
        TabOrder = 5
        OnClick = Button6Click
      end
      object Button7: TButton
        Left = 24
        Top = 247
        Width = 137
        Height = 25
        Caption = 'PG_uTimesSold'
        TabOrder = 6
        OnClick = Button7Click
      end
      object Button8: TButton
        Left = 24
        Top = 285
        Width = 137
        Height = 25
        Caption = 'PG_uTotalQty'
        TabOrder = 7
        OnClick = Button8Click
      end
      object Button9: TButton
        Left = 24
        Top = 324
        Width = 137
        Height = 25
        Caption = 'PG_uMinPrice'
        TabOrder = 8
        OnClick = Button9Click
      end
      object Button10: TButton
        Left = 24
        Top = 362
        Width = 137
        Height = 25
        Caption = 'PG_uAvgPrice'
        TabOrder = 9
        OnClick = Button10Click
      end
      object Button11: TButton
        Left = 24
        Top = 401
        Width = 137
        Height = 25
        Caption = 'PG_uQtyAvgPrice'
        TabOrder = 10
        OnClick = Button11Click
      end
      object Button12: TButton
        Left = 24
        Top = 440
        Width = 137
        Height = 25
        Caption = 'PG_uMaxPrice'
        TabOrder = 11
        OnClick = Button12Click
      end
      object Button13: TButton
        Left = 184
        Top = 16
        Width = 137
        Height = 25
        Caption = 'AV_nTotalLots'
        TabOrder = 12
        OnClick = Button13Click
      end
      object Button14: TButton
        Left = 184
        Top = 54
        Width = 137
        Height = 25
        Caption = 'AV_nTotalQty'
        TabOrder = 13
        OnClick = Button14Click
      end
      object Button15: TButton
        Left = 184
        Top = 93
        Width = 137
        Height = 25
        Caption = 'AV_nMinPrice'
        TabOrder = 14
        OnClick = Button15Click
      end
      object Button16: TButton
        Left = 184
        Top = 131
        Width = 137
        Height = 25
        Caption = 'AV_nAvgPrice'
        TabOrder = 15
        OnClick = Button16Click
      end
      object Button17: TButton
        Left = 184
        Top = 170
        Width = 137
        Height = 25
        Caption = 'AV_nQtyAvgPrice'
        TabOrder = 16
        OnClick = Button17Click
      end
      object Button18: TButton
        Left = 184
        Top = 208
        Width = 137
        Height = 25
        Caption = 'AV_nMaxPrice'
        TabOrder = 17
        OnClick = Button18Click
      end
      object Button19: TButton
        Left = 184
        Top = 247
        Width = 137
        Height = 25
        Caption = 'AV_uTotalLots'
        TabOrder = 18
        OnClick = Button19Click
      end
      object Button20: TButton
        Left = 184
        Top = 285
        Width = 137
        Height = 25
        Caption = 'AV_uTotalQty'
        TabOrder = 19
        OnClick = Button20Click
      end
      object Button21: TButton
        Left = 184
        Top = 324
        Width = 137
        Height = 25
        Caption = 'AV_uMinPrice'
        TabOrder = 20
        OnClick = Button21Click
      end
      object Button22: TButton
        Left = 184
        Top = 362
        Width = 137
        Height = 25
        Caption = 'AV_uAvgPrice'
        TabOrder = 21
        OnClick = Button22Click
      end
      object Button23: TButton
        Left = 184
        Top = 401
        Width = 137
        Height = 25
        Caption = 'AV_uQtyAvgPrice'
        TabOrder = 22
        OnClick = Button23Click
      end
      object Button24: TButton
        Left = 184
        Top = 440
        Width = 137
        Height = 25
        Caption = 'AV_uMaxPrice'
        TabOrder = 23
        OnClick = Button24Click
      end
      object Button25: TButton
        Left = 24
        Top = 480
        Width = 137
        Height = 25
        Caption = 'N_Demand'
        TabOrder = 24
        OnClick = Button25Click
      end
      object Button26: TButton
        Left = 24
        Top = 520
        Width = 137
        Height = 25
        Caption = 'U_Demand'
        TabOrder = 25
        OnClick = Button26Click
      end
      object Button27: TButton
        Left = 24
        Top = 560
        Width = 137
        Height = 25
        Caption = 'My Inventory'
        TabOrder = 26
        OnClick = Button27Click
      end
      object Button28: TButton
        Left = 184
        Top = 480
        Width = 137
        Height = 25
        Caption = 'N_partout'
        TabOrder = 27
        OnClick = Button28Click
      end
      object Button29: TButton
        Left = 184
        Top = 520
        Width = 137
        Height = 25
        Caption = 'U_partout'
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
end
