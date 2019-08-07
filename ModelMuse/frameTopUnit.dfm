object frameView: TframeView
  Left = 0
  Top = 0
  Width = 489
  Height = 264
  VertScrollBar.Range = 0
  HorzScrollBar.Range = 0
  TabOrder = 0
  OnMouseMove = rulHorizontalMouseMove
  object rulVertical: TRbwRuler
    Left = 0
    Top = 54
    Width = 49
    Height = 210
    Align = alLeft
    OnMouseMove = rulHorizontalMouseMove
    RulerLinePosition = 30
    RulerDesiredSpacing = 100
    RulerMajorTickLength = 10
    RulerMinorTickLength = 5
    RulerPosition = rpLeft
    RulerTextOffset = 5
    RulerTextPosition = tpOutside
    RulerEnds.Lower = 10
    RulerEnds.Upper = 230
    RulerStart = sBottomRight
    RulerValues.Upper = 100
  end
  object ZoomBox: TQrbwZoomBox
    Left = 49
    Top = 54
    Width = 440
    Height = 210
    Align = alClient
    BevelOuter = bvLowered
    PopupMenu = OrderMenu
    TabOrder = 0
    OnExit = ZoomBoxExit
    OnMouseMove = rulHorizontalMouseMove
    BottomPaintBox.Left = 0
    BottomPaintBox.Top = 0
    BottomPaintBox.Width = 422
    BottomPaintBox.Height = 192
    BottomPaintBox.OnPaint = ZoomBoxBottomPaintBoxPaint
    Exaggeration = 1
    HorizontalDirection = hdRight
    HorzScrollBar.Left = 0
    HorzScrollBar.Top = 192
    HorzScrollBar.Width = 422
    HorzScrollBar.Height = 18
    HorzScrollBar.LargeChange = 422
    HorzScrollBar.Max = 0
    HorzScrollBar.ParentColor = False
    HorzScrollBar.TabOrder = 1
    HorzScrollBar.OnScroll = ZoomBoxHorzScrollBarScroll
    Image.Left = 0
    Image.Top = 0
    Image.Width = 422
    Image.Height = 192
    MaxX = 10000
    MaxY = 15000
    MinX = -2000
    MinY = -1000
    PaintBox.Left = 0
    PaintBox.Top = 0
    PaintBox.Width = 422
    PaintBox.Height = 192
    PaintBox.OnDblClick = ZoomBoxPaintBoxDblClick
    PaintBox.OnMouseDown = ZoomBoxPaintBoxMouseDown
    PaintBox.OnMouseMove = ZoomBoxPaintBoxMouseMove
    PaintBox.OnMouseUp = ZoomBoxPaintBoxMouseUp
    PaintBox.OnPaint = ZoomBoxPaintBoxPaint
    VertScrollBar.Left = 422
    VertScrollBar.Top = 0
    VertScrollBar.Width = 18
    VertScrollBar.Height = 192
    VertScrollBar.Kind = sbVertical
    VertScrollBar.LargeChange = 192
    VertScrollBar.Max = 0
    VertScrollBar.ParentColor = False
    VertScrollBar.TabOrder = 0
    VertScrollBar.OnScroll = ZoomBoxVertScrollBarScroll
    VerticalDirection = vdUp
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 489
    Height = 54
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object ModelCube: TRbwModelCube
      Left = 0
      Top = 0
      Width = 49
      Height = 54
      Hint = 
        'Click to change layer|Click to change the selected layer up or d' +
        'own.'
      Align = alLeft
      ParentShowHint = False
      ShowHint = True
      OnMouseDown = ModelCubeMouseDown
      OnMouseMove = ModelCubeMouseMove
      OnMouseUp = ModelCubeMouseUp
      OnPaint = ModelCubePaint
      CubeLeftX = 5
      CubeTopY = 20
      CubeHeight = 25
      CubeVanishingPointX = 118
      CubeVanishingPointY = -40
      CubeFraction = 0.15
      CubeWidth = 25
      SelectedFace = faTop
      Selection2 = 0.25
      CanClickFace = False
      ShowSelection = True
      Opaque = False
      XOrigin = xoWest
      YOrigin = yoSouth
      ZOrigin = zoBottom
    end
    object rulHorizontal: TRbwRuler
      Left = 49
      Top = 0
      Width = 440
      Height = 54
      Align = alClient
      OnMouseMove = rulHorizontalMouseMove
      RulerLinePosition = 30
      RulerDesiredSpacing = 100
      RulerMajorTickLength = 10
      RulerMinorTickLength = 5
      RulerPosition = rpTop
      RulerTextOffset = 5
      RulerTextPosition = tpOutside
      RulerEnds.Lower = 60
      RulerEnds.Upper = 480
      RulerStart = sTopLeft
      RulerValues.Upper = 100
    end
  end
  object OrderMenu: TPopupMenu
    OnPopup = OrderMenuPopup
    Left = 56
    Top = 64
    object ToFront1: TMenuItem
      Caption = 'To Front'
      OnClick = ToFront1Click
    end
    object ToBack1: TMenuItem
      Caption = 'To Back'
      OnClick = ToBack1Click
    end
    object ForwardOne1: TMenuItem
      Caption = 'Forward One'
      OnClick = ForwardOne1Click
    end
    object BackOne1: TMenuItem
      Caption = 'Back One'
      OnClick = BackOne1Click
    end
  end
end
