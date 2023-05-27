inherited frmGoPhast: TfrmGoPhast
  Left = 91
  Top = 50
  HelpType = htKeyword
  HelpKeyword = 'Main_Window'
  Caption = 'GoPhast'
  ClientHeight = 560
  ClientWidth = 772
  KeyPreview = True
  Menu = mmMainMenu
  Position = poScreenCenter
  ShowHint = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnDeactivate = FormDeactivate
  OnKeyUp = FormKeyUp
  OnMouseMove = pnlLowerRightMouseMove
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  ExplicitWidth = 784
  ExplicitHeight = 618
  TextHeight = 18
  object splitHoriz: TJvNetscapeSplitter
    Left = 0
    Top = 389
    Width = 772
    Height = 10
    Cursor = crVSplit
    Hint = 'Click and drag to resize the windows.'
    Align = alBottom
    MinSize = 1
    OnMoved = splitHorizMoved
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ExplicitTop = 329
    ExplicitWidth = 47
  end
  object pnlTop: TPanel
    Left = 0
    Top = 196
    Width = 772
    Height = 193
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    object splitVertTop: TJvNetscapeSplitter
      Left = 533
      Top = 0
      Height = 193
      Hint = 'Click and drag to resize the windows.'
      Align = alRight
      MinSize = 1
      OnMoved = splitVertTopMoved
      Maximized = False
      Minimized = False
      ButtonCursor = crDefault
      ExplicitLeft = 408
      ExplicitTop = 120
      ExplicitHeight = 100
    end
    inline frameTopView: TframeView
      Left = 0
      Top = 0
      Width = 533
      Height = 193
      Align = alClient
      TabOrder = 0
      TabStop = True
      ExplicitWidth = 533
      ExplicitHeight = 193
      inherited rulVertical: TRbwRuler
        Height = 139
        ExplicitTop = 39
        ExplicitHeight = 173
      end
      inherited pnlTop: TPanel
        Width = 533
        ExplicitWidth = 529
        inherited rulHorizontal: TRbwRuler
          Width = 484
          ExplicitLeft = 49
          ExplicitTop = 2
          ExplicitWidth = 471
        end
      end
      inherited ZoomBox: TQRbwZoomBox2
        Width = 484
        Height = 139
        Image32.Left = 1
        Image32.Top = 1
        Image32.Width = 482
        Image32.Height = 137
        Image32.Align = alClient
        Image32.ExplicitTop = 1
        Image32.ExplicitWidth = 482
        Image32.ExplicitHeight = 137
        ExplicitWidth = 480
        ExplicitHeight = 138
      end
      inherited OrderMenu: TPopupMenu
        Left = 80
        Top = 8
      end
    end
    inline frameSideView: TframeView
      Left = 543
      Top = 0
      Width = 229
      Height = 193
      Align = alRight
      TabOrder = 1
      TabStop = True
      ExplicitLeft = 543
      ExplicitWidth = 229
      ExplicitHeight = 193
      inherited rulVertical: TRbwRuler
        Left = 180
        Height = 139
        Align = alRight
        RulerPosition = rpRight
        ExplicitLeft = 194
        ExplicitHeight = 173
      end
      inherited pnlTop: TPanel
        Width = 229
        ExplicitWidth = 229
        inherited ModelCube: TRbwModelCube
          Left = 180
          Hint = 
            'Click to change column|Click to change the selected column left ' +
            'or right.'
          Align = alRight
          SelectionColor = clBlue
          SelectedFace = faSide
          Selection2 = 0.010000000000000000
          ExplicitLeft = 177
        end
        inherited rulHorizontal: TRbwRuler
          Left = 0
          Width = 180
          RulerEnds.Lower = 10
          RulerEnds.Upper = 80
          RulerStart = sBottomRight
          ExplicitLeft = 0
          ExplicitWidth = 194
          ExplicitHeight = 39
        end
      end
      inherited ZoomBox: TQRbwZoomBox2
        Left = 0
        Width = 180
        Height = 139
        ExaggerationDirection = edHorizontal
        HorizontalDirection = hdLeft
        Image32.Left = 1
        Image32.Top = 1
        Image32.Width = 178
        Image32.Height = 137
        Image32.Align = alClient
        Image32.ExplicitTop = 1
        Image32.ExplicitWidth = 178
        Image32.ExplicitHeight = 137
        ExplicitLeft = 0
        ExplicitWidth = 180
        ExplicitHeight = 138
      end
      inherited OrderMenu: TPopupMenu
        Left = 48
        Top = 128
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 399
    Width = 772
    Height = 133
    Align = alBottom
    BevelOuter = bvNone
    DoubleBuffered = False
    ParentBackground = False
    ParentColor = True
    ParentDoubleBuffered = False
    TabOrder = 2
    OnMouseMove = pnlLowerRightMouseMove
    object splitVertBottom: TJvNetscapeSplitter
      Left = 533
      Top = 0
      Height = 133
      Hint = 'Click and drag to resize the windows.'
      Align = alRight
      MinSize = 1
      OnMoved = splitVertBottomMoved
      Maximized = False
      Minimized = False
      ButtonCursor = crDefault
      ExplicitLeft = 441
      ExplicitTop = -4
    end
    inline frameFrontView: TframeView
      Left = 0
      Top = 0
      Width = 533
      Height = 133
      Align = alClient
      TabOrder = 0
      TabStop = True
      ExplicitWidth = 533
      ExplicitHeight = 133
      inherited rulVertical: TRbwRuler
        Top = 0
        Height = 79
        ExplicitTop = 0
        ExplicitHeight = 79
      end
      inherited pnlTop: TPanel
        Top = 79
        Width = 533
        Align = alBottom
        TabOrder = 1
        ExplicitTop = 79
        ExplicitWidth = 529
        inherited ModelCube: TRbwModelCube
          Hint = 
            'Click to change row|Click to change the selected row forward or ' +
            'back.'
          SelectionColor = clLime
          SelectedFace = faFront
          Selection2 = 0.010000000000000000
        end
        inherited rulHorizontal: TRbwRuler
          Width = 484
          RulerMajorTickLength = -10
          RulerMinorTickLength = -5
          RulerTextPosition = tpInside
          ExplicitLeft = 53
          ExplicitTop = 6
          ExplicitWidth = 427
        end
      end
      inherited ZoomBox: TQRbwZoomBox2
        Top = 0
        Width = 484
        Height = 79
        TabOrder = 0
        Image32.Left = 1
        Image32.Top = 1
        Image32.Width = 482
        Image32.Height = 77
        Image32.Align = alClient
        Image32.ExplicitTop = 1
        Image32.ExplicitWidth = 482
        Image32.ExplicitHeight = 77
        ExplicitTop = 0
        ExplicitWidth = 480
        ExplicitHeight = 79
      end
    end
    inline frame3DView: Tframe3DView
      Left = 543
      Top = 0
      Width = 229
      Height = 133
      Align = alRight
      DoubleBuffered = False
      ParentBackground = False
      ParentDoubleBuffered = False
      TabOrder = 1
      TabStop = True
      ExplicitLeft = 539
      ExplicitWidth = 229
      ExplicitHeight = 133
      inherited glWidModelView: TGLWidget
        Width = 229
        Height = 133
        ExplicitWidth = 229
        ExplicitHeight = 133
      end
    end
  end
  object sbMain: TStatusBar
    Left = 0
    Top = 532
    Width = 772
    Height = 28
    Hint = 'Drag with the mouse to resize status bar panels'
    Panels = <
      item
        Width = 400
      end
      item
        Width = 160
      end
      item
        Width = 200
      end>
    ParentColor = True
    ParentFont = True
    ParentShowHint = False
    ShowHint = True
    UseSystemFont = False
    OnMouseDown = sbMainMouseDown
    OnMouseMove = sbMainMouseMove
    OnMouseUp = sbMainMouseUp
    OnDrawPanel = sbMainDrawPanel
    ExplicitTop = 531
    ExplicitWidth = 768
  end
  object cbControlBar: TControlBar
    Left = 0
    Top = 0
    Width = 772
    Height = 196
    Align = alTop
    AutoSize = True
    ParentShowHint = False
    RowSize = 32
    ShowHint = True
    TabOrder = 0
    OnMouseMove = pnlLowerRightMouseMove
    ExplicitWidth = 768
    object tbarEdit: TToolBar
      Left = 152
      Top = 34
      Width = 146
      Height = 28
      ButtonHeight = 23
      Caption = 'Edit'
      Constraints.MinWidth = 146
      DisabledImages = ilDisabledImageList
      DragMode = dmAutomatic
      Images = ilImageList
      TabOrder = 1
      Wrapable = False
      OnMouseMove = pnlLowerRightMouseMove
      object tbUndo: TToolButton
        Left = 0
        Top = 0
        Action = acUndo
        OnMouseMove = pnlLowerRightMouseMove
      end
      object tbRedo: TToolButton
        Left = 23
        Top = 0
        Action = acRedo
        OnMouseMove = pnlLowerRightMouseMove
      end
      object tbCut: TToolButton
        Left = 46
        Top = 0
        Action = acCut
        OnMouseMove = pnlLowerRightMouseMove
      end
      object tbCopy: TToolButton
        Left = 69
        Top = 0
        Action = acCopy
        OnMouseMove = pnlLowerRightMouseMove
      end
      object tbPaste: TToolButton
        Left = 92
        Top = 0
        Action = acPaste
        OnMouseMove = pnlLowerRightMouseMove
      end
      object tbMeasure: TToolButton
        Left = 115
        Top = 0
        Action = acMeasure
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
      end
    end
    object tbarFile: TToolBar
      Left = 14
      Top = 2
      Width = 159
      Height = 28
      ButtonHeight = 23
      Caption = 'File'
      Constraints.MinWidth = 159
      DisabledImages = ilDisabledImageList
      DragMode = dmAutomatic
      Images = ilImageList
      TabOrder = 0
      Wrapable = False
      OnMouseMove = pnlLowerRightMouseMove
      object tbNew: TToolButton
        Left = 0
        Top = 0
        Action = acFileNewModflowModel
        OnMouseMove = pnlLowerRightMouseMove
      end
      object tbOpen: TToolButton
        Left = 23
        Top = 0
        Action = acFileOpen
        OnMouseMove = pnlLowerRightMouseMove
      end
      object tbSave: TToolButton
        Left = 46
        Top = 0
        Action = acFileSave
        OnMouseMove = pnlLowerRightMouseMove
      end
      object btnRunModel: TJvArrowButton
        Left = 69
        Top = 0
        Width = 37
        Height = 23
        Action = acExportPhastInputFile
        ArrowWidth = 12
        Flat = True
        FillFont.Charset = DEFAULT_CHARSET
        FillFont.Color = clWindowText
        FillFont.Height = -11
        FillFont.Name = 'Tahoma'
        FillFont.Style = []
        Glyph.Data = {
          36050000424D3605000000000000360400002800000010000000100000000100
          0800000000000001000000000000000000000001000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
          A6000020400000206000002080000020A0000020C0000020E000004000000040
          20000040400000406000004080000040A0000040C0000040E000006000000060
          20000060400000606000006080000060A0000060C0000060E000008000000080
          20000080400000806000008080000080A0000080C0000080E00000A0000000A0
          200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
          200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
          200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
          20004000400040006000400080004000A0004000C0004000E000402000004020
          20004020400040206000402080004020A0004020C0004020E000404000004040
          20004040400040406000404080004040A0004040C0004040E000406000004060
          20004060400040606000406080004060A0004060C0004060E000408000004080
          20004080400040806000408080004080A0004080C0004080E00040A0000040A0
          200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
          200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
          200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
          20008000400080006000800080008000A0008000C0008000E000802000008020
          20008020400080206000802080008020A0008020C0008020E000804000008040
          20008040400080406000804080008040A0008040C0008040E000806000008060
          20008060400080606000806080008060A0008060C0008060E000808000008080
          20008080400080806000808080008080A0008080C0008080E00080A0000080A0
          200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
          200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
          200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
          2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
          2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
          2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
          2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
          2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
          2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
          2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000707070707A4
          070707070707070707070707070707FAA40707070707070707070707070707FA
          FAA407070707070707070707070707FAFAFAA4070707070707070707070707FA
          FAFAFAA40707070707070707070707FAFAFAFAFAA407070707070707070707FA
          FAFAFAFAFAA4070707070707070707FAFAFAFAFAFAFAA40707070707070707FA
          FAFAFAFAFAFAFA0707070707070707FAFAFAFAFAFAFA070707070707070707FA
          FAFAFAFAFA07070707070707070707FAFAFAFAFA0707070707070707070707FA
          FAFAFA070707070707070707070707FAFAFA07070707070707070707070707FA
          FA0707070707070707070707070707FA07070707070707070707}
        PressBoth = False
      end
      object tbImportModelResults: TToolButton
        Left = 106
        Top = 0
        Action = acImportModelResults
      end
      object tbPrint: TToolButton
        Left = 129
        Top = 0
        Action = acExportImage
        OnMouseMove = pnlLowerRightMouseMove
      end
    end
    object tbarEditScreenObjects: TToolBar
      Left = 73
      Top = 66
      Width = 225
      Height = 28
      ButtonHeight = 23
      Caption = 'Edit objects'
      Constraints.MinWidth = 225
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = 20
      Font.Name = 'Arial'
      Font.Pitch = fpVariable
      Font.Style = []
      Images = ilImageList
      ParentFont = False
      TabOrder = 2
      Wrapable = False
      OnMouseMove = pnlLowerRightMouseMove
      object tbSelect: TToolButton
        Left = 0
        Top = 0
        Action = acSelectObjects
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbSelectPoint: TToolButton
        Left = 23
        Top = 0
        Action = acSelectNode
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbLasso: TToolButton
        Left = 46
        Top = 0
        Action = acSelectWithLasso
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
      end
      object tbSeparator1: TToolButton
        Left = 69
        Top = 0
        Width = 8
        Caption = 'tbSeparator1'
        ImageIndex = 24
        Style = tbsSeparator
      end
      object tbDeleteSegment: TToolButton
        Left = 77
        Top = 0
        Action = acDeleteSegment
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
      end
      object tbInsertPoint: TToolButton
        Left = 100
        Top = 0
        Action = acInsertNode
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
      end
      object tbVertexValue: TToolButton
        Left = 123
        Top = 0
        Action = acVertexValue
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbSeparator2: TToolButton
        Left = 146
        Top = 0
        Width = 8
        Caption = 'tbSeparator2'
        ImageIndex = 40
        Style = tbsSeparator
      end
      object tbShowHideObjects: TToolButton
        Left = 154
        Top = 0
        Action = acShowHideObjects
        OnMouseMove = pnlLowerRightMouseMove
      end
      object btnDisplayData: TToolButton
        Left = 177
        Top = 0
        Action = acDisplayData
      end
      object tbShowGridValues: TToolButton
        Left = 200
        Top = 0
        Action = acShowGridValues
      end
    end
    object tbarView: TToolBar
      Left = 128
      Top = 98
      Width = 170
      Height = 28
      ButtonHeight = 23
      ButtonWidth = 24
      Caption = 'Navigate'
      Constraints.MinWidth = 163
      DragMode = dmAutomatic
      Images = ilImageList
      TabOrder = 3
      Wrapable = False
      OnMouseMove = pnlLowerRightMouseMove
      object tbZoom: TToolButton
        Left = 0
        Top = 0
        Action = acZoom
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbZoomIn: TToolButton
        Left = 24
        Top = 0
        Action = acZoomIn
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbZoomOut: TToolButton
        Left = 48
        Top = 0
        Action = acZoomOut
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbPan: TToolButton
        Left = 72
        Top = 0
        Action = acPan
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbPositionUndo: TToolButton
        Left = 96
        Top = 0
        Action = acPositionBackward
        OnMouseMove = pnlLowerRightMouseMove
      end
      object tbRestoreDefault2DView: TToolButton
        Left = 120
        Top = 0
        Action = acRestoreDefault2DView
        OnMouseMove = pnlLowerRightMouseMove
      end
      object tbPositionRedo: TToolButton
        Left = 144
        Top = 0
        Action = acPositionForward
        OnMouseMove = pnlLowerRightMouseMove
      end
    end
    object tbarEditGrid: TToolBar
      Left = 465
      Top = 130
      Width = 212
      Height = 28
      ButtonHeight = 23
      Caption = 'Grid'
      Constraints.MinWidth = 212
      DisabledImages = ilDisabledImageList
      DragMode = dmAutomatic
      Images = ilImageList
      TabOrder = 5
      Wrapable = False
      OnMouseMove = pnlLowerRightMouseMove
      object tbDeleteColumnRow: TToolButton
        Left = 0
        Top = 0
        Action = acDeleteColumnRow
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbMove: TToolButton
        Left = 23
        Top = 0
        Action = acMoveColumnOrRow
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbAddVerticalBoundary: TToolButton
        Left = 46
        Top = 0
        Action = acAddColumn
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbAddHorizontalBoundary: TToolButton
        Left = 69
        Top = 0
        Action = acAddRow
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbSubdivide: TToolButton
        Left = 92
        Top = 0
        Action = acSubdivide
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbSpacing: TToolButton
        Left = 115
        Top = 0
        Action = acSetSpacing
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbGridAngle: TToolButton
        Left = 138
        Top = 0
        Action = acGridDragRotate
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbGenerateGrid: TToolButton
        Left = 161
        Top = 0
        Action = acGenerateGrid
        OnMouseMove = pnlLowerRightMouseMove
      end
      object tbSelectColRowLayer: TToolButton
        Left = 184
        Top = 0
        Action = acSelectColRowLay
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
    end
    object tbarCreateScreenObject: TToolBar
      Left = 208
      Top = 130
      Width = 244
      Height = 28
      ButtonHeight = 26
      Caption = 'Create objects'
      Constraints.MinWidth = 244
      DragMode = dmAutomatic
      Images = ilImageList
      TabOrder = 8
      Wrapable = False
      object tbPoint: TToolButton
        Left = 0
        Top = 0
        Action = acCreatePoint
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbLine: TToolButton
        Left = 23
        Top = 0
        Action = acCreateLine
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbPolygon: TToolButton
        Left = 46
        Top = 0
        Action = acCreatePolygon
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbStraightLine: TToolButton
        Left = 69
        Top = 0
        Action = acCreateStraightLine
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbRectangle: TToolButton
        Left = 92
        Top = 0
        Action = acCreateRectangle
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbSeparator3: TToolButton
        Left = 115
        Top = 0
        Width = 8
        Caption = 'tbSeparator3'
        ImageIndex = 68
        Style = tbsSeparator
      end
      object tbAddPointsToObject: TToolButton
        Left = 123
        Top = 0
        Action = acAddPointsToObject
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbAddLinesToObjects: TToolButton
        Left = 146
        Top = 0
        Action = acAddLinesToObject
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbAddPartsToObject: TToolButton
        Left = 169
        Top = 0
        Action = acAddPolygonsToObject
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object comboZCount: TComboBox
        Left = 192
        Top = 0
        Width = 40
        Height = 26
        Hint = 'Number of elevations in new object'
        Style = csDropDownList
        ItemIndex = 2
        TabOrder = 0
        Text = '2'
        Items.Strings = (
          '0'
          '1'
          '2')
      end
    end
    object tbarView3D: TToolBar
      Left = 54
      Top = 130
      Width = 141
      Height = 28
      ButtonHeight = 26
      Caption = '3D View'
      Constraints.MinWidth = 141
      DragMode = dmAutomatic
      Images = ilImageList
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      Wrapable = False
      object tbShell: TToolButton
        Left = 0
        Top = 0
        Action = acShowGridShell
        AllowAllUp = True
        Style = tbsCheck
        OnMouseMove = pnlLowerRightMouseMove
      end
      object tbTopGrid: TToolButton
        Left = 23
        Top = 0
        Action = acShowTopGrid
        AllowAllUp = True
        Style = tbsCheck
        OnMouseMove = pnlLowerRightMouseMove
      end
      object tbFrontGrid: TToolButton
        Left = 46
        Top = 0
        Action = acShowFrontGrid
        AllowAllUp = True
        Style = tbsCheck
        OnMouseMove = pnlLowerRightMouseMove
      end
      object tbSideGrid: TToolButton
        Left = 69
        Top = 0
        Action = acShowSideGrid
        AllowAllUp = True
        Style = tbsCheck
        OnMouseMove = pnlLowerRightMouseMove
      end
      object tb3DColors: TToolButton
        Left = 92
        Top = 0
        Action = acColoredGrid
        Style = tbsCheck
        OnMouseMove = pnlLowerRightMouseMove
      end
      object tb3DObjects: TToolButton
        Left = 115
        Top = 0
        Action = acShow3DObjects
        AllowAllUp = True
        Style = tbsCheck
        OnMouseMove = pnlLowerRightMouseMove
      end
    end
    object tlbMesh: TToolBar
      Left = 192
      Top = 162
      Width = 138
      Height = 28
      Caption = 'tlbMesh'
      Constraints.MinWidth = 138
      DisabledImages = ilDisabledImageList
      Images = ilImageList
      TabOrder = 9
      Visible = False
      object tbCrossSection: TToolButton
        Left = 0
        Top = 0
        Action = acMoveCrossSection
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseUp = ToolButtonMouseUp
      end
      object tbRotateCrossSection: TToolButton
        Left = 23
        Top = 0
        Action = acRotateCrossSection
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseUp = ToolButtonMouseUp
      end
      object tbMoveNodes: TToolButton
        Left = 46
        Top = 0
        Action = acMoveNodesOrElements
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseUp = ToolButtonMouseUp
      end
      object tbDrawElement: TToolButton
        Left = 69
        Top = 0
        Action = acDrawElement
        AllowAllUp = True
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseUp = ToolButtonMouseUp
      end
      object btnFishnet: TToolButton
        Left = 92
        Top = 0
        Hint = 
          'Draw fishnet-mesh quadilaterals|Draw fishnet-mesh quadrilaterals' +
          ' to be used in generating a mesh.'
        Action = acFishnet
        AllowAllUp = True
        Caption = 'Draw Fishnet-Mesh Quadrilaterals'
        Grouped = True
        Style = tbsCheck
        OnMouseDown = tbPointMouseDown
        OnMouseUp = ToolButtonMouseUp
      end
      object btnGenerateMesh: TToolButton
        Left = 115
        Top = 0
        Action = acGenerateGrid
      end
    end
    object tlb3dViewMesh: TToolBar
      Left = 127
      Top = 162
      Width = 49
      Height = 28
      Caption = 'tlb3dViewMesh'
      Constraints.MinWidth = 49
      Images = ilImageList
      TabOrder = 4
      Visible = False
      object btnShowTopMesh: TToolButton
        Left = 0
        Top = 0
        Action = acShowTopMesh
        AllowAllUp = True
        Style = tbsCheck
        OnMouseMove = pnlLowerRightMouseMove
      end
      object btnShowFrontMesh: TToolButton
        Left = 23
        Top = 0
        Action = acShowFrontMesh
        AllowAllUp = True
        Style = tbsCheck
        OnMouseMove = pnlLowerRightMouseMove
      end
      object tbShow2DMesh: TToolButton
        Left = 46
        Top = 0
        Hint = 
          'Show or hide 2D cell outlines|Show or hide cell outlines in 2D v' +
          'iews.'
        Caption = 'tbShow2DMesh'
        DropdownMenu = menuGridLineChoice
        ImageIndex = 71
      end
    end
    object tbarEditDisv: TToolBar
      Left = 14
      Top = 162
      Width = 97
      Height = 28
      Caption = 'tbarEditDisv'
      Constraints.MinWidth = 97
      Images = ilImageList
      TabOrder = 7
      Visible = False
      object btnMoveCrossSection: TToolButton
        Left = 0
        Top = 0
        Action = acMoveCrossSection
      end
      object btnRotateCrossSection: TToolButton
        Left = 23
        Top = 0
        Action = acRotateCrossSection
      end
      object btnQuadmesh: TToolButton
        Left = 46
        Top = 0
        Action = acQuadmesh
      end
      object tbGenerateGridDisv: TToolButton
        Left = 69
        Top = 0
        Action = acGenerateGrid
      end
    end
    object tbarShowGrid: TToolBar
      Left = 11
      Top = 130
      Width = 30
      Height = 28
      ButtonHeight = 26
      Caption = 'Show/HIde Grid'
      Constraints.MinWidth = 30
      DragMode = dmAutomatic
      Images = ilImageList
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      object tbShow2DGrid: TToolButton
        Left = 0
        Top = 0
        Hint = 'Show or hide 2D gridlines|Show or hide gridlines in 2D views.'
        Caption = 'Show or Hide 2-D Grid'
        Down = True
        DropdownMenu = menuGridLineChoice
        ImageIndex = 71
        OnMouseMove = pnlLowerRightMouseMove
      end
    end
    object tbarPilotPoints: TToolBar
      Left = 14
      Top = 98
      Width = 50
      Height = 28
      ButtonHeight = 23
      Caption = 'tbarPilotPoints'
      Images = ilImageList
      TabOrder = 11
      object tbAddPilotPoint: TToolButton
        Left = 0
        Top = 0
        Action = acAddPilotPoint
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
      object tbDeletePilotPoint: TToolButton
        Left = 23
        Top = 0
        Action = acDeletePilotPoint
        OnMouseDown = tbPointMouseDown
        OnMouseMove = pnlLowerRightMouseMove
        OnMouseUp = ToolButtonMouseUp
      end
    end
  end
  object mmMainMenu: TMainMenu
    Images = ilImageList
    Left = 264
    object miFile: TMenuItem
      AutoHotkeys = maAutomatic
      Caption = '&File'
      HelpContext = 1550
      OnClick = miFileClick
      object miNew: TMenuItem
        Caption = '&New'
        HelpContext = 1550
        Hint = 'New'
        object miNewModflowModel: TMenuItem
          Action = acFileNewModflowModel
        end
        object miNewPHASTModel: TMenuItem
          Action = acFileNewPhastModel
        end
        object acNewSutraModel1: TMenuItem
          Action = acNewSutraModel
        end
        object miNewFootprintModel: TMenuItem
          Action = acNewFootprintModel
        end
      end
      object miOpen: TMenuItem
        Action = acFileOpen
      end
      object miSave: TMenuItem
        Action = acFileSave
      end
      object miSaveAs: TMenuItem
        Action = acFileSaveAs
      end
      object miImport: TMenuItem
        Caption = '&Import'
        HelpContext = 1550
        object miModelResults: TMenuItem
          Action = acImportModelResults
        end
        object miImportShapefile: TMenuItem
          Caption = '&Shapefile...'
          HelpContext = 1560
          Hint = 'Import the shapes in a Shapefile into the current model'
          OnClick = miImportShapefileClick
        end
        object miImportDXFFile: TMenuItem
          Caption = 'DXF File...'
          HelpContext = 1570
          Hint = 'Import objects from a DXF file'
          OnClick = miImportDXFFileClick
        end
        object SurferGridFile1: TMenuItem
          Caption = 'Surfer Grid File...'
          HelpContext = 3563
          Hint = 'Import objects from a SURFER grid file'
          OnClick = SurferGridFile1Click
        end
        object SampleDEMData1: TMenuItem
          Caption = 'Sample DEM Data...'
          HelpContext = 3564
          Hint = 'Sample data points from a DEM grid file'
          OnClick = SampleDEMData1Click
        end
        object miImportPoints: TMenuItem
          Caption = 'Points...'
          HelpContext = 1580
          Hint = 'Import points and associated data a new objects'
          OnClick = miImportPointsClick
        end
        object miImportDistributedDatabyZone: TMenuItem
          Caption = '&Distributed Data by Zone...'
          HelpContext = 1590
          Hint = 'Import PHAST zones'
          OnClick = miImportDistributedDatabyZoneClick
        end
        object miGriddedData: TMenuItem
          Caption = 'Gridded Data...'
          HelpContext = 3460
          Hint = 'Import gridded data as new objects'
          OnClick = miGriddedDataClick
        end
        object miImportGriddedDataFiles: TMenuItem
          Action = acImportGriddedDataFiles
        end
        object miASCII_RasterFile: TMenuItem
          Caption = 'ASCII Raster File(s)...'
          HelpContext = 3594
          Hint = 'Import data from ASCII raster files.'
          OnClick = miASCII_RasterFileClick
        end
        object ImportTPROGSBinaryGridFile1: TMenuItem
          Action = acImportTprogs
        end
        object miImportBitmap: TMenuItem
          Caption = 'Image...'
          HelpContext = 1600
          Hint = 'Import a background image for your model'
          OnClick = miImportBitmapClick
        end
        object miModflow2005Model: TMenuItem
          Caption = 'MODFLOW-2005 or -NWT Model...'
          HelpContext = 3385
          Hint = 'Create a new model by importing and existing MODFLOW-2005 model'
          OnClick = acFileNewModflowModelExecute
        end
        object miImportModelMate: TMenuItem
          Action = acImportModelMate
        end
        object miImportSutraMesh: TMenuItem
          Action = acImportSutraMesh
        end
        object miImportSutraFiles: TMenuItem
          Action = acImportSutraFiles
        end
        object ImportModelFeaturefromPEST1: TMenuItem
          Action = acImportMf6FeatureFromPest
        end
        object ImportModelFeaturefromPEST2: TMenuItem
          Action = acImportSutraFeaturesFromPest
        end
      end
      object miLinkedRasters: TMenuItem
        Caption = 'Linked Rasters'
        OnClick = miLinkedRastersClick
      end
      object miExport: TMenuItem
        Caption = 'Export'
        HelpContext = 1550
        object miExportPhast: TMenuItem
          Action = acExportPhastInputFile
        end
        object miExportModflow: TMenuItem
          Action = acRunModflow
        end
        object miRunModflow6: TMenuItem
          Action = acRunModflow6
        end
        object miExportModflowLgr: TMenuItem
          Action = acRunModflowLgr
        end
        object miExportModflowNwt: TMenuItem
          Action = acRunModflowNwt
        end
        object miMODFLOWFMP2InputFiles: TMenuItem
          Action = acRunModflowFmp
        end
        object miRunModflowCfp: TMenuItem
          Action = acRunModflowCfp
        end
        object miExportModpath: TMenuItem
          Action = acExportModpath
        end
        object miZONEBUDGETInputFiles: TMenuItem
          Action = acExportZoneBudget
        end
        object miRunMt3dms: TMenuItem
          Action = acRunMt3dms
        end
        object miRunSutra: TMenuItem
          Action = acRunSutra
        end
        object miPEST: TMenuItem
          Caption = 'PEST'
          object PESTControlfile1: TMenuItem
            Action = acRunPest
          end
          object miCalcSuperParameters: TMenuItem
            Action = acCalcSuperParameters
          end
          object miRunSutraPrep: TMenuItem
            Action = acRunSvdaPrep
          end
          object miRunParRep: TMenuItem
            Action = acExportParRep
          end
        end
        object miRunFootprint: TMenuItem
          Action = acRunFootprint
        end
        object miShapefile: TMenuItem
          Caption = '&Shapefile'
          HelpContext = 1550
          object miExportShapefile: TMenuItem
            Caption = '&Grid Data to Shapefile...'
            HelpContext = 3384
            Hint = 'Export data sets or boundaries as Shapefiles'
            OnClick = miExportShapefileClick
          end
          object miObjectstoShapefile: TMenuItem
            Caption = '&Objects to Shapefile...'
            HelpContext = 3521
            Hint = 'Export objects as Shapefiles'
            OnClick = miObjectstoShapefileClick
          end
          object miContourstoShapefile: TMenuItem
            Caption = '&Contours to Shapefile'
            HelpContext = 1550
            Hint = 'Export contour lines as Shapefiles'
            OnClick = miContourstoShapefileClick
          end
          object miPathlinestoShapefile: TMenuItem
            Caption = '&Pathlines to Shapefile'
            Enabled = False
            Hint = 'Export MODPATH pathlines as Shapefiles'
            OnClick = miPathlinestoShapefileClick
          end
          object miEndpointsatStartingLocationstoShapefile: TMenuItem
            Caption = 'Endpoints at &Starting Locations to Shapefile'
            Enabled = False
            HelpContext = 1550
            Hint = 'Export MODPATH endpoint starting locations as Shapefiles'
            OnClick = miEndpointsatStartingLocationstoShapefileClick
          end
          object miEndpointsatEndingLocationstoShapefile: TMenuItem
            Caption = 'Endpoints at &Ending Locations to Shapefile'
            Enabled = False
            HelpContext = 1550
            Hint = 'Export MODPATH endpoint ending locations as Shapefiles'
            OnClick = miEndpointsatEndingLocationstoShapefileClick
          end
          object miTimeSeriestoShapefile: TMenuItem
            Caption = '&Time Series to Shapefile'
            Enabled = False
            HelpContext = 1550
            Hint = 'Export MODPATH time series as Shapefiles'
            OnClick = miTimeSeriestoShapefileClick
          end
          object miHeadObsToShapefile: TMenuItem
            Action = acHeadObsToShapefile
          end
          object miOutlineToShapefile: TMenuItem
            Action = acOutlineToShapefile
          end
        end
        object miDataSetstoCSV: TMenuItem
          Caption = 'Data Sets to CSV'
          HelpContext = 1550
          Hint = 
            'Export data set values to CSV file|Export data set values to com' +
            'ma-separated values (CSV) file'
          OnClick = miDataSetstoCSVClick
        end
        object ExportImage1: TMenuItem
          Action = acExportImage
        end
        object ModelMateFile1: TMenuItem
          Action = acExportModelMate
        end
        object miExportSutra2DMesh: TMenuItem
          Action = acExportSutra2DMesh
        end
        object WellsToCsv1: TMenuItem
          Caption = 'Wells to CSV'
          Visible = False
          OnClick = WellsToCsv1Click
        end
      end
      object miFilesToArchive: TMenuItem
        Caption = 'Files to Archive...'
        HelpContext = 1610
        Hint = 'Specify which files are saved in an archive'
        OnClick = miFilesToArchiveClick
      end
      object miPrint: TMenuItem
        Action = acFilePrint
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object miExit: TMenuItem
        Action = acExit
      end
    end
    object miEdit: TMenuItem
      Caption = '&Edit'
      HelpContext = 1630
      object miUndo: TMenuItem
        Action = acUndo
      end
      object miRedo: TMenuItem
        Action = acRedo
      end
      object miCut: TMenuItem
        Action = acCut
      end
      object miCopy: TMenuItem
        Action = acCopy
      end
      object miPaste: TMenuItem
        Action = acPaste
      end
      object miSelectAll: TMenuItem
        Caption = 'Select &All'
        HelpContext = 1630
        Hint = 'Select All'
        object miSelectAllTop: TMenuItem
          Action = acSelectAllTop
        end
        object miSelectAllFront: TMenuItem
          Action = acSelectAllFront
        end
        object miSelectAllSide: TMenuItem
          Action = acSelectAllSide
        end
      end
      object miInvertSelection: TMenuItem
        Caption = '&Invert Selection'
        Enabled = False
        HelpContext = 1630
        Hint = 
          'Toggle all visible objects on the current view between selected ' +
          'and unselected'
        OnClick = miInvertSelectionClick
      end
      object miEditBitmaps: TMenuItem
        Caption = 'Edit Image...'
        HelpContext = 1600
        Hint = 'Change the location of a background image'
        OnClick = miEditBitmapsClick
      end
      object miShowHideBitmaps: TMenuItem
        Caption = 'Show or Hide Images...'
        HelpContext = 1640
        Hint = 'Show or hide a background image'
        OnClick = miShowHideBitmapsClick
      end
      object miDeleteImage: TMenuItem
        Caption = 'Delete Image'
        HelpContext = 3520
        Hint = 'Delete a background image'
        OnClick = miDeleteImageClick
      end
      object miClearUndoRedostack: TMenuItem
        Caption = 'Clear Undo/Redo Stack'
        Hint = 
          'Delete all undo and redo items|Reclaim memory by deleting all un' +
          'do and redo items'
        OnClick = miClearUndoRedostackClick
      end
      object AddPilotPoint1: TMenuItem
        Action = acAddPilotPoint
      end
      object DeletePilotPoint1: TMenuItem
        Action = acDeletePilotPoint
      end
    end
    object miMesh: TMenuItem
      Caption = 'Mesh'
      object miMeshGenerationControls: TMenuItem
        Action = acGenerateGrid
      end
      object miFishnet: TMenuItem
        Action = acFishnet
      end
      object miViewMeshInformation1: TMenuItem
        Caption = '&View Mesh Information'
        Hint = 'View information relating to the mesh and the mesh quality'
        OnClick = miViewMeshInformation1Click
      end
      object miRenumberMesh: TMenuItem
        Caption = '&Renumber Mesh...'
        Hint = 
          'reassign cell and element numbers to (possibly) reduce the bandw' +
          'idth'
        OnClick = miRenumberMeshClick
      end
      object miSpecifyMesh: TMenuItem
        Caption = 'Specify Mesh...'
        Hint = 'Directly specify the mesh'
        OnClick = miSpecifyMeshClick
      end
      object miSpecifyFishnetQuadrilateral: TMenuItem
        Caption = 'Specify Fishnet-Mesh Quadrilaterals...'
        Hint = 'Specify the properties of the fishnet mesh quadrilaterals'
        OnClick = miSpecifyFishnetQuadrilateralClick
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object SetDefaultCrossSection1: TMenuItem
        Action = acDefaultCrossSection
      end
      object miMoveCrossSection: TMenuItem
        Action = acMoveCrossSection
      end
      object miRotateCrossSection: TMenuItem
        Action = acRotateCrossSection
      end
      object miSpecifyCrossSection: TMenuItem
        Action = acSpecifyCrossSection
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object miMoveNodesOrElements: TMenuItem
        Action = acMoveNodesOrElements
      end
      object miDrawElement: TMenuItem
        Action = acDrawElement
      end
    end
    object miGrid: TMenuItem
      Caption = '&Grid'
      HelpContext = 1650
      object miGenerateGrid: TMenuItem
        Action = acGenerateGrid
      end
      object miDISV_Choice: TMenuItem
        Action = acDisvGrid
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object miDeleteGridLine: TMenuItem
        Action = acDeleteColumnRow
      end
      object miMoveColumnOrRow: TMenuItem
        Action = acMoveColumnOrRow
      end
      object miAddVerticalGridLine: TMenuItem
        Action = acAddColumn
      end
      object miAddHorizontalGridLine: TMenuItem
        Action = acAddRow
      end
      object miEditGridLines: TMenuItem
        Action = acEditGridLines
      end
      object miSubdivide: TMenuItem
        Action = acSubdivide
      end
      object miSetSpacing: TMenuItem
        Action = acSetSpacing
      end
      object miSmoothGrid: TMenuItem
        Action = acSmoothGrid
      end
      object miDragtoRotate: TMenuItem
        Action = acGridDragRotate
      end
      object miGridAngle: TMenuItem
        Action = acGridAngle
      end
      object miRotateAroundGridOrigin: TMenuItem
        Action = acRotateAroundGridOrigin
      end
      object miMoveGrid: TMenuItem
        Action = acMoveGrid
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object miSetSelectedColRowLayer: TMenuItem
        Caption = 'Set Selected &Col, Row, Layer...'
        HelpContext = 1730
        Hint = 'Set the selected column, row, and layer numerically'
        OnClick = miSetSelectedColRowLayerClick
      end
      object miSelectColRowLayer: TMenuItem
        Action = acSelectColRowLay
      end
    end
    object miDISV: TMenuItem
      Caption = 'DISV'
      object miStructured: TMenuItem
        Action = acStructuredGrid
      end
      object miMoveCrossSection1: TMenuItem
        Action = acMoveCrossSection
      end
      object miRotateCrossSection1: TMenuItem
        Action = acRotateCrossSection
      end
      object miQuadmesh: TMenuItem
        Action = acQuadmesh
      end
      object miGenerateGrid1: TMenuItem
        Action = acGenerateGrid
      end
      object miDefaultCrossSection: TMenuItem
        Action = acDefaultCrossSection
      end
      object miSpecifyCrossSection1: TMenuItem
        Action = acSpecifyCrossSection
      end
    end
    object miData: TMenuItem
      Caption = '&Data'
      HelpContext = 1740
      object miEditGlobalVariables: TMenuItem
        Caption = 'Edit &Global Variables...'
        HelpContext = 1750
        Hint = 'Create, change, or delete global variables'
        OnClick = miEditGlobalVariablesClick
      end
      object miEditDataSet: TMenuItem
        Action = acEditDataSets
      end
      object miDisplayData: TMenuItem
        Action = acDisplayData
      end
      object miShowGridValues: TMenuItem
        Action = acShowGridValues
      end
      object miDisplayDataSetValues: TMenuItem
        Caption = 'Display Data &Set Values'
        HelpContext = 3519
        Hint = 'Show the values of a data set'
        OnClick = miDisplayDataSetValuesClick
      end
      object DeleteModelResults1: TMenuItem
        Caption = 'Delete Model &Results'
        Hint = 'Delete model-result data sets'
        OnClick = DeleteModelResults1Click
      end
    end
    object miObject: TMenuItem
      Caption = '&Object'
      HelpContext = 1890
      object miSelectObjects: TMenuItem
        Action = acSelectObjects
      end
      object miSelectNodes: TMenuItem
        Action = acSelectNode
      end
      object miSelectWithLasso: TMenuItem
        Action = acSelectWithLasso
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Create1: TMenuItem
        Caption = '&Create'
        HelpContext = 1890
        object miCreatePoint: TMenuItem
          Action = acCreatePoint
          Caption = '&Point'
        end
        object miCreateLine: TMenuItem
          Action = acCreateLine
          Caption = 'Poly&line'
        end
        object miCreatePolygon: TMenuItem
          Action = acCreatePolygon
          Caption = 'Poly&gon'
        end
        object miCreateStraightLine: TMenuItem
          Action = acCreateStraightLine
          Caption = '&Straight Line'
        end
        object miCreateRectangle: TMenuItem
          Action = acCreateRectangle
          Caption = '&Rectangle'
        end
      end
      object Edit1: TMenuItem
        Caption = '&Edit'
        HelpContext = 1890
        object miEditSelectedObjects: TMenuItem
          Action = acEditSelecteObjects
        end
        object miSelectObjectsforEditing: TMenuItem
          Caption = 'Select Objects for Editing or Deletion...'
          HelpContext = 3525
          Hint = 'Select objects without displaying them'
          OnClick = miSelectObjectsforEditingClick
        end
        object AddPointstoObject1: TMenuItem
          Action = acAddPointsToObject
        end
        object AddLinesToObject1: TMenuItem
          Action = acAddLinesToObject
        end
        object AddPartstoObject1: TMenuItem
          Action = acAddPolygonsToObject
        end
        object miInsertNode: TMenuItem
          Action = acInsertNode
        end
        object miDeleteSegment: TMenuItem
          Action = acDeleteSegment
        end
        object miRearrangeObjects: TMenuItem
          Caption = 'Rearrange &Objects...'
          HelpContext = 2210
          Hint = 'Change the order of objects'
          OnClick = miRearrangeObjectsClick
        end
        object miScaleRotateorMoveObjects: TMenuItem
          Caption = 'Scale, Rotate, and Move Objects...'
          Enabled = False
          HelpContext = 3417
          Hint = 'Change the geometry of the selected objects'
          OnClick = miScaleRotateorMoveObjectsClick
        end
        object miMergeObjects: TMenuItem
          Caption = 'Merge Objects'
          Enabled = False
          HelpContext = 1890
          Hint = 'Join objects whose ends match into a single object'
          OnClick = miMergeObjectsClick
        end
        object miReverseSelectedObjects: TMenuItem
          Caption = 'Re&verse Selected Objects'
          Enabled = False
          HelpContext = 1890
          Hint = 'Reverse the order of the verticies in the selected objects'
          OnClick = miReverseSelectedObjectsClick
        end
        object EditVertexValues1: TMenuItem
          Action = acVertexValue
        end
        object miInvertSelectedVertices: TMenuItem
          Caption = 'Invert Selected Vertices'
          HelpContext = 1520
          Hint = 
            'Switch vertices in selected object between selected and unselect' +
            'ed'
          OnClick = miInvertSelectedVerticesClick
        end
        object miSplitSelectedObjects: TMenuItem
          Caption = 'Split Selected Objects'
          HelpContext = 1530
          Hint = 'Convert each part of the selected objects into a separate object'
          OnClick = miSplitSelectedObjectsClick
        end
        object miMakeSelectedVerticesASeparateObject: TMenuItem
          Caption = 'Make Selected Vertices a Separate Object'
          HelpContext = 1530
          Hint = 'Convert the selected nodes of an object to a separate object'
          OnClick = miMakeSelectedVerticesASeparateObjectClick
        end
        object miSplitObjectAtSelectedVertices: TMenuItem
          Caption = 'Split Object at Selected Vertices'
          HelpContext = 1530
          Hint = 
            'Split an object into two separate objects at the selected vertic' +
            'es of the object'
          OnClick = miSplitObjectAtSelectedVerticesClick
        end
        object miLockSelectedObjects: TMenuItem
          Caption = 'Lock Selected Objects'
          Enabled = False
          Hint = 'Prevent object(s) from being moved'
          OnClick = miLockSelectedObjectsClick
        end
        object miUnlockSelectedObjects: TMenuItem
          Caption = 'Unlock Selected Objects'
          Enabled = False
          Hint = 'Allow object(s) to be moved'
          OnClick = miUnlockSelectedObjectsClick
        end
        object SimplifySelectedObjects1: TMenuItem
          Action = acSimplifyScreenObjects
        end
        object miAnonymizeObjects: TMenuItem
          Action = acAnonymizeObjects
        end
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miSearchForObject: TMenuItem
        Caption = 'Search &for Objects...'
        HelpContext = 2220
        Hint = 'Select objects that specify a data set'
        OnClick = miSearchForObjectClick
      end
      object miShowHideObjects: TMenuItem
        Action = acShowHideObjects
      end
      object ShallAllObjects1: TMenuItem
        Caption = 'Show All Objects'
        HelpContext = 1890
        Hint = 'Display all the objects'
        OnClick = ShallAllObjects1Click
      end
      object HideAllObjects1: TMenuItem
        Caption = 'Hide All Objects'
        HelpContext = 1890
        Hint = 'Hide all the objects'
        OnClick = HideAllObjects1Click
      end
      object miSelectObjectsByName: TMenuItem
        Caption = 'Select Objects by &Name...'
        HelpContext = 2250
        Hint = 'Search for and select an object based on its name.'
        ShortCut = 16462
        OnClick = miSelectObjectsByNameClick
      end
    end
    object Navigation1: TMenuItem
      Caption = '&Navigation'
      HelpContext = 3486
      object miZoom: TMenuItem
        Action = acZoom
        HelpContext = 3486
      end
      object miZoomIn: TMenuItem
        Action = acZoomIn
        HelpContext = 3486
      end
      object miZoomOut: TMenuItem
        Action = acZoomOut
        HelpContext = 3486
      end
      object miPan: TMenuItem
        Action = acPan
        HelpContext = 3486
      end
      object RestoreDefault2DView1: TMenuItem
        Action = acRestoreDefault2DView
        HelpContext = 3486
      end
      object miUndoPosition: TMenuItem
        Action = acPositionBackward
        HelpContext = 3486
      end
      object RedoPosition: TMenuItem
        Action = acPositionForward
        HelpContext = 3486
      end
      object miGoTo: TMenuItem
        Action = acMoveTo
      end
      object miMeasure: TMenuItem
        Action = acMeasure
      end
    end
    object miView: TMenuItem
      Caption = '&View'
      HelpContext = 2260
      Hint = 
        'Make sure the the ratio of sizes between adjacent columns, rows,' +
        ' or layers is less than a specified value'
      object miVerticalExaggeration: TMenuItem
        Action = acEditVerticalExaggeration
      end
      object miShowFormulaErrors: TMenuItem
        Caption = 'Show &Formula Errors'
        HelpContext = 2330
        Hint = 'Display a list of the errors in formulas'
        OnClick = miShowFormulaErrorsClick
      end
      object WarningsandErrors1: TMenuItem
        Caption = 'Errors and Warnings'
        HelpContext = 2340
        Hint = 'Show errors or warnings about the model'
        OnClick = WarningsandErrors1Click
      end
      object miShowSelectedObjects: TMenuItem
        Caption = '&List Selected Objects'
        HelpContext = 2230
        Hint = 'Display a list of the selected objects'
        OnClick = miShowSelectedObjectsClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object miShowGridShell: TMenuItem
        Action = acShowGridShell
      end
      object miShowTopGrid: TMenuItem
        Action = acShowTopGrid
      end
      object miShowFrontGrid: TMenuItem
        Action = acShowFrontGrid
      end
      object miShowSideGrid: TMenuItem
        Action = acShowSideGrid
      end
      object miShowColoredGrid: TMenuItem
        Action = acColoredGrid
      end
      object ShowTopMesh1: TMenuItem
        Action = acShowTopMesh
      end
      object ShowFrontMesh1: TMenuItem
        Action = acShowFrontMesh
      end
      object miShow3DObjects: TMenuItem
        Action = acShow3DObjects
      end
      object miShow2DGridlines: TMenuItem
        AutoCheck = True
        Caption = 'Show or Hide 2-D Grid'
        Checked = True
        HelpContext = 2260
        Hint = 'Show or hide 2D gridlines|Show or hide gridlines in 2D views.'
        ImageIndex = 71
        object ShowAll2: TMenuItem
          Action = acShowAllGridLines
          AutoCheck = True
          HelpContext = 2260
        end
        object Showexterior2: TMenuItem
          Action = acShowExteriorGridLines
          AutoCheck = True
          HelpContext = 2260
        end
        object Showactive2: TMenuItem
          Action = acShowActiveGridLines
          AutoCheck = True
          HelpContext = 2260
        end
        object Showactiveedge2: TMenuItem
          Action = acShowActiveEdge
          AutoCheck = True
          HelpContext = 2260
        end
      end
      object miRestoreDefaultView: TMenuItem
        Action = acRestoreDefaultView
      end
      object mi3D_Colors: TMenuItem
        Caption = '3D &Lighting...'
        HelpContext = 2350
        Hint = 'Change lighting of the 3D view'
        OnClick = mi3D_ColorsClick
      end
      object miCustomizeSutraMesh: TMenuItem
        Caption = 'Customize SUTRA Mesh...'
        Hint = 'Change how the SUTRA mesh is displayed'
        OnClick = miCustomizeSutraMeshClick
      end
      object Action11: TMenuItem
        Action = acShowCellNumbers
      end
    end
    object miCustomize: TMenuItem
      Caption = '&Customize'
      HelpContext = 2360
      object miFont: TMenuItem
        Action = acFont
      end
      object miColor: TMenuItem
        Action = acColor
      end
      object miHintDelay: TMenuItem
        Caption = '&Hint Display Time...'
        HelpContext = 2370
        Hint = 'Set the time for which hint windows are visible.'
        OnClick = miHintDelayClick
      end
      object miRulerFormat: TMenuItem
        Caption = 'Ruler Format...'
        HelpContext = 2380
        Hint = 'Change the number of digits displaye on te rulers'
        OnClick = miRulerFormatClick
      end
      object miBatchFileAdditions: TMenuItem
        Caption = 'Batch File Additions...'
        HelpContext = 3513
        Hint = 'Add lines to the batch file used to run the model'
        OnClick = miBatchFileAdditionsClick
      end
      object miShowVideoTips: TMenuItem
        Caption = 'Show Tips'
        Checked = True
        HelpContext = 2360
        Hint = 'Show a tip each day when ModelMuse starts'
        OnClick = miShowVideoTipsClick
      end
      object miShowOrHideRulers: TMenuItem
        Action = acShowOrHideRulers
      end
      object Archivemodelbydefault1: TMenuItem
        Action = acArchiveModel
      end
    end
    object miModel: TMenuItem
      Caption = '&Model'
      HelpContext = 2390
      object miEditGeoRef: TMenuItem
        Caption = 'Edit &Geo Reference'
        GroupIndex = 1
        Hint = 'Specify the location and projection of the model'
        OnClick = miEditGeoRefClick
      end
      object miPackages: TMenuItem
        Caption = 'MODFLOW Packages and Programs...'
        GroupIndex = 1
        HelpContext = 2540
        Hint = 'Select the packages and programs to use with MODFLOW'
        OnClick = miPackagesClick
      end
      object miLayers: TMenuItem
        Action = acLayers
        GroupIndex = 1
      end
      object miMF_HydrogeologicUnits: TMenuItem
        Caption = 'MODFLOW &Hydrogeologic Units...'
        GroupIndex = 1
        HelpContext = 3515
        Hint = 'Edit the hydrogeologic units in the HUF package'
        OnClick = miMF_HydrogeologicUnitsClick
      end
      object miTime: TMenuItem
        Caption = 'MODFLOW &Time...'
        GroupIndex = 1
        HelpContext = 2490
        Hint = 'Edit the stress periods'
        OnClick = miTimeClick
      end
      object miOutputControl: TMenuItem
        Caption = 'MODFLOW &Output Control...'
        GroupIndex = 1
        HelpContext = 2500
        Hint = 'Edit the type and format of the output'
        OnClick = miOutputControlClick
      end
      object miGeneral: TMenuItem
        Caption = 'MODFLOW &Options...'
        GroupIndex = 1
        HelpContext = 2450
        Hint = 'Modify units, wetting, and other options'
        OnClick = miGeneralClick
      end
      object miProgramLocations: TMenuItem
        Caption = 'MODFLOW Program &Locations...'
        GroupIndex = 1
        HelpContext = 2810
        Hint = 'Specify the locations of the modeling program files'
        OnClick = miProgramLocationsClick
      end
      object miLinkSFRStreams: TMenuItem
        Caption = 'Link &Streams...'
        GroupIndex = 1
        HelpContext = 3372
        Hint = 'Link streams in the SFR package based on proximity'
        OnClick = miLinkSFRStreamsClick
      end
      object miManageParameters: TMenuItem
        Caption = 'Manage Parameters...'
        GroupIndex = 1
        HelpContext = 3584
        Hint = 'Change the value of any parameter'
        OnClick = miManageParametersClick
      end
      object miManageHeadObservations: TMenuItem
        Caption = 'Manage &Head Observations...'
        GroupIndex = 1
        HelpContext = 3586
        Hint = 'Edit head observations in a table'
        OnClick = miManageHeadObservationsClick
      end
      object miManageFluxObservations: TMenuItem
        Caption = 'Manage &Flow Observations...'
        GroupIndex = 1
        HelpContext = 3586
        Hint = 'Create, edit, or delete flow obsevations'
        OnClick = miManageFluxObservationsClick
      end
      object miModflowNameFile: TMenuItem
        Caption = 'MODFLOW &Name File...'
        GroupIndex = 1
        HelpContext = 3447
        Hint = 'Add additional lines to the MODFLOW name file'
        OnClick = miModflowNameFileClick
      end
      object miObservationType: TMenuItem
        Caption = '&Observation Type'
        GroupIndex = 1
        HelpContext = 2390
        Hint = 'Change the type of observations that will be exported to MODFLOW'
        object miObservations: TMenuItem
          AutoCheck = True
          Caption = 'Observations'
          Checked = True
          GroupIndex = 1
          Hint = 'Use observations as MODFLOW observations'
          RadioItem = True
          OnClick = miObservationsClick
        end
        object miPredictions: TMenuItem
          AutoCheck = True
          Caption = 'Predictions'
          GroupIndex = 1
          Hint = 'Use predictions as MODFLOW observations'
          RadioItem = True
          OnClick = miObservationsClick
        end
      end
      object miChildModels: TMenuItem
        Caption = '&Child Models...'
        GroupIndex = 1
        HelpContext = 3598
        Hint = 'Edit child models'
        OnClick = miChildModelsClick
      end
      object miSWR: TMenuItem
        Caption = '&SWR Dialog Boxes'
        GroupIndex = 1
        Hint = 'SWR package dialog boxes'
        object miSWR_ReachGeometry: TMenuItem
          Action = acSWR_ReachGeometry
          GroupIndex = 1
        end
        object miSwrStructures: TMenuItem
          Action = acSwrStructures
          GroupIndex = 1
        end
        object miSWR_Tabfiles: TMenuItem
          Action = acSWR_Tabfiles
          GroupIndex = 1
        end
        object miSwrObservations: TMenuItem
          Action = acSwrObservations
          GroupIndex = 1
        end
      end
      object miFarmProcessDialogBoxes: TMenuItem
        Caption = '&Farm Process'
        GroupIndex = 1
        Hint = 'Farm process dialog boxes'
        object IrrigationTypes1: TMenuItem
          Action = acFarmIrrigationTypes
          GroupIndex = 1
        end
        object miFarmCrops: TMenuItem
          Action = acFarmCrops
          GroupIndex = 1
        end
        object FarmSoils1: TMenuItem
          Action = acFarmSoils
          GroupIndex = 1
        end
        object miFarmClimate: TMenuItem
          Action = acFarmClimate
          GroupIndex = 1
        end
        object miFarmAllotment: TMenuItem
          Action = acFarmAllotment
          GroupIndex = 1
        end
        object miEditFarms: TMenuItem
          Action = acEditFarms
          GroupIndex = 1
        end
      end
      object miRipareanETPlantGroups: TMenuItem
        Action = acRipPlantGroups
        GroupIndex = 1
      end
      object EditContaminantTreatmentSystems1: TMenuItem
        Action = acEditCTS
        Enabled = False
        GroupIndex = 1
      end
      object EditTimeSeries1: TMenuItem
        Action = acTimeSeries
        GroupIndex = 1
      end
      object N8: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object miTitleAndUnits: TMenuItem
        Caption = 'PHAST Title and &Units...'
        GroupIndex = 1
        HelpContext = 2820
        Hint = 'Specify the title and units for PHAST'
        OnClick = miTitleAndUnitsClick
      end
      object miGridOptions: TMenuItem
        Caption = 'PHAST &Grid Options...'
        GroupIndex = 1
        HelpContext = 2830
        Hint = 'Specify the chemistry dimensions and print orientation'
        OnClick = miGridOptionsClick
      end
      object miChemistryOptions: TMenuItem
        Caption = 'PHAST &Chemistry Options...'
        GroupIndex = 1
        HelpContext = 2840
        Hint = 'Specify the types of reactions to use'
        OnClick = miChemistryOptionsClick
      end
      object miSolutionMethod: TMenuItem
        Caption = 'PHAST Solution &Method...'
        GroupIndex = 1
        HelpContext = 2850
        Hint = 'Specify the solution algorithm'
        OnClick = miSolutionMethodClick
      end
      object miSteadyFlow: TMenuItem
        Caption = 'PHAST Steady Flow...'
        GroupIndex = 1
        HelpContext = 2860
        Hint = 'Specify "steady flow" and related options'
        OnClick = miSteadyFlowClick
      end
      object miTimeControl: TMenuItem
        Caption = 'PHAST &Time Control...'
        GroupIndex = 1
        HelpContext = 2870
        Hint = 'Specify stress periods'
        OnClick = miTimeControlClick
      end
      object miFreeSurface: TMenuItem
        Caption = 'PHAST Free &Surface...'
        GroupIndex = 1
        HelpContext = 2880
        Hint = 'Specify unconfined conditions'
        OnClick = miFreeSurfaceClick
      end
      object miPrintInitial: TMenuItem
        Caption = 'PHAST Print &Initial...'
        GroupIndex = 1
        HelpContext = 2890
        Hint = 'Specify print options for initial conditions'
        OnClick = miPrintInitialClick
      end
      object miPrintFrequency: TMenuItem
        Caption = 'PHAST &Print Frequency...'
        GroupIndex = 1
        HelpContext = 2900
        Hint = 'Specify print options for time varying data'
        OnClick = miPrintFrequencyClick
      end
      object miPHASTProgramLocation: TMenuItem
        Caption = 'PHAST Program Location...'
        GroupIndex = 1
        HelpContext = 3540
        Hint = 'Specify the location of the PHAST executable'
        OnClick = miPHASTProgramLocationClick
      end
      object N10: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object miSutraOptions: TMenuItem
        Action = acSutraOptions
        GroupIndex = 1
      end
      object miSUTRALayerGroups: TMenuItem
        Action = acSutraLayers
        GroupIndex = 1
      end
      object miSutraTimes: TMenuItem
        Action = acSutraTimes
        GroupIndex = 1
      end
      object miSutraOutputControl: TMenuItem
        Action = acSutraOutputControl
        GroupIndex = 1
      end
      object miSutraProgramLocations: TMenuItem
        Action = acSutraProgramLocations
        GroupIndex = 1
      end
      object miEditSutraFluxObs: TMenuItem
        Action = acEditSutraFluxObs
        GroupIndex = 1
      end
      object miFootprintProperties: TMenuItem
        Action = acFootprintProperties
        GroupIndex = 1
      end
      object miFootprintProgramLocation: TMenuItem
        Action = acFootprintProgramLocation
        GroupIndex = 1
      end
      object miEditObservationComparisons: TMenuItem
        Action = acEditObservationComparisons
        GroupIndex = 1
      end
      object miPESTProperties: TMenuItem
        Action = acPEST
        GroupIndex = 1
      end
    end
    object ModelSelection1: TMenuItem
      Caption = 'Model Selection'
      object miModflow6Active: TMenuItem
        Action = acModflow6Active
        GroupIndex = 1
      end
      object miModflow: TMenuItem
        Action = acModflowActive
        GroupIndex = 1
        RadioItem = True
      end
      object miModflowLgr: TMenuItem
        Action = acModflowLgrActive
        GroupIndex = 1
        RadioItem = True
      end
      object MODFLOWLGRV21: TMenuItem
        Action = acModflowLgr2Active
        GroupIndex = 1
      end
      object miModflowNwt: TMenuItem
        Action = acModflowNwtActive
        GroupIndex = 1
        RadioItem = True
      end
      object miModflowCfpActive: TMenuItem
        Action = acModflowCfpActive
        GroupIndex = 1
        RadioItem = True
      end
      object miModflowFmpActive: TMenuItem
        Action = acModflowFmpActive
        GroupIndex = 1
        RadioItem = True
      end
      object MODFLOWOWHMV21: TMenuItem
        Action = acModflowOwhmV2
        GroupIndex = 1
        RadioItem = True
      end
      object miPhast: TMenuItem
        Action = acPhastActive
        GroupIndex = 1
        RadioItem = True
      end
      object miSUTRA: TMenuItem
        Action = acSutra22Active
        GroupIndex = 1
        RadioItem = True
      end
      object mniSutra30Active: TMenuItem
        Action = acSutra30Active
        GroupIndex = 1
      end
      object miSutra40Active: TMenuItem
        Action = acSutra40Active
        GroupIndex = 1
      end
      object miFootPrintActive: TMenuItem
        Action = acFootPrintActive
        GroupIndex = 1
      end
    end
    object miHelp: TMenuItem
      Caption = '&Help'
      HelpContext = 2910
      object miContents: TMenuItem
        Action = acHelpContents
      end
      object miHelpOnMainWindow: TMenuItem
        Caption = 'Help On Main Window'
        HelpContext = 2910
        Hint = 'Display help about the main window'
        ShortCut = 112
        OnClick = miHelpOnMainWindowClick
      end
      object ModflowReference1: TMenuItem
        Caption = 'Modflow Reference'
        HelpContext = 2910
        Hint = 'Open the MODFLOW online guide'
        OnClick = ModflowReference1Click
      end
      object IntroductoryVideo1: TMenuItem
        Caption = '&Introductory Video'
        HelpContext = 2910
        Hint = 'Display the ModelMuse introductory video'
        OnClick = IntroductoryVideo1Click
      end
      object miAllVideos: TMenuItem
        Caption = 'All &Videos'
        HelpContext = 2910
        Hint = 'Open a web page with a list of all the videos'
        OnClick = miAllVideosClick
      end
      object miExamples: TMenuItem
        Caption = 'Examples'
        HelpContext = 2910
        Hint = 'Open a description of the example models'
        OnClick = miExamplesClick
      end
      object miUseLocalHelp: TMenuItem
        Caption = 'Use Local Help'
        GroupIndex = 1
        Hint = 'Use help files on local computer'
        RadioItem = True
        OnClick = miUseLocalHelpClick
      end
      object miUseOnlineHelp: TMenuItem
        Caption = 'Use Online Help'
        GroupIndex = 1
        Hint = 'Use help via the Internet'
        RadioItem = True
        OnClick = miUseOnlineHelpClick
      end
      object miFileTypes: TMenuItem
        Caption = 'File Types'
        GroupIndex = 1
        Hint = 'Display descriptions of input and output file types'
        OnClick = miFileTypesClick
      end
      object miAbout: TMenuItem
        Caption = '&About'
        GroupIndex = 1
        HelpContext = 2910
        Hint = 'Display the about box with contact information'
        OnClick = miAboutClick
      end
    end
  end
  object alActionList: TActionList
    Images = ilImageList
    Left = 104
    Top = 32
    object acCut: TAction
      Category = 'Edit'
      Caption = '&Cut'
      HelpContext = 1630
      HelpKeyword = 'Edit'
      Hint = 'Cut objects to the clipboard'
      ImageIndex = 0
      ShortCut = 16472
      OnExecute = acCutExecute
    end
    object acPaste: TAction
      Category = 'Edit'
      Caption = '&Paste'
      HelpContext = 1630
      HelpKeyword = 'Edit'
      Hint = 'Paste objects from the clipboard'
      ImageIndex = 2
      ShortCut = 16470
      OnExecute = acPasteExecute
    end
    object acCopy: TAction
      Category = 'Edit'
      Caption = '&Copy'
      HelpContext = 1630
      HelpKeyword = 'Edit'
      Hint = 'Copy objects to the clipboard'
      ImageIndex = 1
      ShortCut = 16451
      OnExecute = acCopyExecute
    end
    object acSelectAllTop: TAction
      Category = 'Edit'
      Caption = 'Select All &Top'
      HelpContext = 1630
      HelpKeyword = 'Edit'
      Hint = 'Select all visible screen objects on top view'
      ShortCut = 16449
      OnExecute = acSelectAllTopExecute
    end
    object acFileNewModflowModel: TAction
      Category = 'File'
      Caption = 'New &MODFLOW Model'
      HelpContext = 1550
      HelpKeyword = 'File'
      Hint = 'New MODFLOW model|Create a new MODFLOW model'
      ImageIndex = 54
      OnExecute = acFileNewModflowModelExecute
    end
    object acFileSave: TAction
      Category = 'File'
      Caption = '&Save'
      HelpContext = 1550
      HelpKeyword = 'File'
      Hint = 'Save|Save the current model in a ModelMuse file'
      ImageIndex = 8
      ShortCut = 16467
      OnExecute = acFileSaveExecute
    end
    object acFileSaveAs: TAction
      Category = 'File'
      Caption = 'Save &As'
      HelpContext = 1550
      HelpKeyword = 'File'
      Hint = 
        'Save As|Save|Save the current model in a ModelMuse file with a n' +
        'ew file name'
      OnExecute = acFileSaveAsExecute
    end
    object acFileOpen: TAction
      Category = 'File'
      Caption = '&Open'
      HelpContext = 1550
      HelpKeyword = 'File'
      Hint = 'Open|Open an existing ModelMuse file'
      ImageIndex = 7
      OnExecute = acFileOpenExecute
    end
    object acFilePrint: TAction
      Category = 'File'
      Caption = '&Print'
      Hint = 'Print'
      ImageIndex = 14
      Visible = False
    end
    object acExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      HelpContext = 1550
      HelpKeyword = 'File'
      Hint = 'Exit'
      OnExecute = acExitExecute
    end
    object acHelpContents: THelpContents
      Category = 'Help'
      Caption = '&Contents'
      HelpContext = 2910
      HelpKeyword = 'Help'
      Hint = 'Contents|Display the help contents'
      OnExecute = acHelpContentsExecute
    end
    object acEditVerticalExaggeration: TAction
      Category = 'View'
      Caption = '&Vertical Exaggeration...'
      HelpContext = 2320
      HelpKeyword = 'Vertical_Exaggeration_Dialog_Box'
      Hint = 'Set the vertical exaggeration.'
      OnExecute = miVerticalExaggerationClick
    end
    object acZoomIn: TAction
      Category = 'Navigation'
      Caption = 'Zoom &In'
      HelpContext = 2270
      HelpKeyword = 'Changing_the_Magnification'
      Hint = 'Zoom in|Click to zoom in by a factor of 2.'
      ImageIndex = 26
      ShortCut = 16457
      OnExecute = miZoomInClick
    end
    object acZoomOut: TAction
      Category = 'Navigation'
      Caption = 'Zoom &Out'
      HelpContext = 2270
      HelpKeyword = 'Changing_the_Magnification'
      Hint = 'Zoom out|Click to zoom out by a factor of 2.'
      ImageIndex = 24
      ShortCut = 16463
      OnExecute = tbZoomOutClick
    end
    object acZoom: TAction
      Category = 'Navigation'
      Caption = '&Zoom'
      HelpContext = 2270
      HelpKeyword = 'Changing_the_Magnification'
      Hint = 
        'Click and drag to zoom in|Click down and drag to zoom in on a sp' +
        'ecific area.'
      ImageIndex = 29
      OnExecute = tbZoomClick
    end
    object acPan: TAction
      Category = 'Navigation'
      Caption = '&Pan'
      HelpContext = 2260
      HelpKeyword = 'View'
      Hint = 'Move displayed area|Click down and drag to pan in any direction.'
      ImageIndex = 28
      OnExecute = tbPanClick
    end
    object acMoveTo: TAction
      Category = 'Navigation'
      Caption = '&Go To...'
      HelpContext = 2280
      HelpKeyword = 'Go_To_Dialog_Box'
      Hint = 'Go to a specific location, cell or object'
      ShortCut = 16455
      OnExecute = acMoveToExecute
    end
    object acDeleteColumnRow: TAction
      Category = 'Grid'
      Caption = '&Delete Grid Line'
      HelpContext = 1660
      HelpKeyword = 'Editing_the_Grid'
      Hint = 'Delete grid lines|Click on a grid line to delete it.'
      ImageIndex = 5
      OnExecute = acDeleteColumnRowExecute
    end
    object acMoveColumnOrRow: TAction
      Category = 'Grid'
      Caption = '&Move Grid Line'
      HelpContext = 1660
      HelpKeyword = 'Editing_the_Grid'
      Hint = 'Drag grid lines|Click down on a grid line and drag to move it.'
      ImageIndex = 31
      OnExecute = acMoveColumnOrRowExecute
    end
    object acAddColumn: TAction
      Category = 'Grid'
      Caption = 'Add &Vertical Grid Line'
      HelpContext = 1660
      HelpKeyword = 'Editing_the_Grid'
      Hint = 
        'Add vertical grid line|Click on the grid to insert a vertical gr' +
        'id line.'
      ImageIndex = 32
      OnExecute = tbAddVerticalBoundaryClick
    end
    object acAddRow: TAction
      Category = 'Grid'
      Caption = 'Add &Horizontal Grid Line'
      HelpContext = 1660
      HelpKeyword = 'Editing_the_Grid'
      Hint = 
        'Add horizontal grid line|Click on the grid to insert a horizonta' +
        'l grid line.'
      ImageIndex = 33
      OnExecute = tbAddHorizontalBoundaryClick
    end
    object acSubdivide: TAction
      Category = 'Grid'
      Caption = 'Subdivide Grid &Elements...'
      HelpContext = 1670
      HelpKeyword = 'Subdivide_Columns_Rows_and_Layers'
      Hint = 
        'Subdivide grid elements|Click down and drag to select elements t' +
        'o be subdivided.'
      ImageIndex = 34
      OnExecute = acSubdivideExecute
    end
    object acGridAngle: TAction
      Category = 'Grid'
      Caption = 'Specify Grid &Angle...'
      HelpContext = 1690
      HelpKeyword = 'Grid_Angle_Dialog_Box'
      Hint = 'Set the grid angle to a specific value'
      OnExecute = acGridAngleExecute
    end
    object acEditGridLines: TAction
      Category = 'Grid'
      Caption = 'Specify Grid &Lines...'
      HelpContext = 1710
      HelpKeyword = 'Grid_Spacing_Dialog_Box'
      Hint = 'Specify the exact postions of column, row, and layer grid lines'
      OnExecute = acEditGridLinesExecute
    end
    object acSmoothGrid: TAction
      Category = 'Grid'
      Caption = '&Smooth Grid...'
      HelpContext = 1720
      HelpKeyword = 'Smooth_Grid_Dialog_Box'
      Hint = 
        'Make sure the the ratio of sizes between adjacent columns, rows,' +
        ' or layers is less than a specified value'
      OnExecute = acSmoothGridExecute
    end
    object acFont: TAction
      Category = 'Customize'
      Caption = '&Font...'
      HelpContext = 2360
      HelpKeyword = 'Customize'
      Hint = 'Set the font of the main window'
      OnExecute = acFontExecute
    end
    object acColor: TAction
      Category = 'Customize'
      Caption = '&Color...'
      HelpContext = 2360
      HelpKeyword = 'Customize'
      Hint = 'Set the background color of the entire application'
      OnExecute = acColorExecute
    end
    object acCreatePoint: TAction
      Category = 'Object'
      Caption = '&Create Point'
      HelpContext = 1470
      HelpKeyword = 'Points'
      Hint = 'Create point object|Click on the model to create a point object.'
      ImageIndex = 19
      OnExecute = tbPointClick
    end
    object acCreateLine: TAction
      Category = 'Object'
      Caption = 'Create &Polyline'
      HelpContext = 1480
      HelpKeyword = 'Polylines'
      Hint = 
        'Create polyline object|Click on the model to create a polyline o' +
        'bject.'
      ImageIndex = 20
      OnExecute = tbLineClick
    end
    object acCreatePolygon: TAction
      Category = 'Object'
      Caption = 'Create &Polygon'
      HelpContext = 1490
      HelpKeyword = 'Polygons'
      Hint = 
        'Create polygon object|Click on the model to create a polygon obj' +
        'ect.'
      ImageIndex = 21
      OnExecute = tbPolygonClick
    end
    object acCreateStraightLine: TAction
      Category = 'Object'
      Caption = 'Create &Straight Line'
      HelpContext = 1500
      HelpKeyword = 'Straight-Lines'
      Hint = 
        'Create straight-line object|Click on the model to create a strai' +
        'ght-line object.'
      ImageIndex = 22
      OnExecute = tbStraightLineClick
    end
    object acCreateRectangle: TAction
      Category = 'Object'
      Caption = 'Create &Rectangle'
      HelpContext = 1510
      HelpKeyword = 'Rectangles'
      Hint = 
        'Create rectangle object|Click on the model to create a rectangle' +
        ' object.'
      ImageIndex = 23
      OnExecute = tbRectangleClick
    end
    object acInsertNode: TAction
      Category = 'Object'
      Caption = '&Insert Vertex'
      HelpContext = 1530
      HelpKeyword = 'Editing_Objects'
      Hint = 'Insert vertex|Click on the edge of an object to insert a vertex.'
      ImageIndex = 39
      OnExecute = tbInsertPointClick
    end
    object acDeleteSegment: TAction
      Category = 'Object'
      Caption = '&Delete Segment'
      HelpContext = 1530
      HelpKeyword = 'Editing_Objects'
      Hint = 
        'Delete segment|Click on an edge of an object to delete that edge' +
        '.'
      ImageIndex = 40
      OnExecute = tbDeleteSegmentClick
    end
    object acSelectObjects: TAction
      Category = 'Object'
      Caption = 'Select &Objects'
      HelpContext = 1520
      HelpKeyword = 'Selecting_Objects'
      Hint = 
        'Select objects|Click on objects to select them; double-click to ' +
        'edit.'
      ImageIndex = 35
      OnExecute = tbSelectClick
    end
    object acSelectNode: TAction
      Category = 'Object'
      Caption = 'Select &Vertices'
      HelpContext = 1520
      HelpKeyword = 'Selecting_Objects'
      Hint = 
        'Select vertices|Click on vertices of selected contours to select' +
        ' the vertices.'
      ImageIndex = 38
      OnExecute = tbSelectPointClick
    end
    object acSelectWithLasso: TAction
      Category = 'Object'
      Caption = 'Select &With Lasso'
      HelpContext = 1520
      HelpKeyword = 'Selecting_Objects'
      Hint = 'Select objects with lasso|Select objects by circling them.'
      ImageIndex = 37
      OnExecute = tbLassoClick
    end
    object acGridDragRotate: TAction
      Category = 'Grid'
      Caption = 'Drag to &Rotate'
      HelpContext = 1660
      HelpKeyword = 'Editing_the_Grid'
      Hint = 
        'Drag to rotate grid|Click down on the grid and drag to rotate th' +
        'e grid.'
      ImageIndex = 41
      OnExecute = tbGridAngleClick
    end
    object acSetSpacing: TAction
      Category = 'Grid'
      Caption = 'Set &Width...'
      HelpContext = 1680
      HelpKeyword = 'Set_Widths_of_Columns_Rows_and_Layers'
      Hint = 
        'Set spacing of selected elements|Click down and drag to select e' +
        'lements and set their widths.'
      ImageIndex = 42
      OnExecute = acSetSpacingExecute
    end
    object acEditDataSets: TAction
      Category = 'Data'
      Caption = '&Edit Data Sets...'
      HelpContext = 1760
      HelpKeyword = 'Data_Sets_Dialog_Box'
      Hint = 'Create, change, or delete data sets'
      ShortCut = 16452
      OnExecute = acEditDataSetsExecute
    end
    object acGenerateGrid: TAction
      Category = 'Grid'
      Caption = '&Generate Grid...'
      HelpContext = 1700
      HelpKeyword = 'Generate_Grid_Dialog_Box'
      Hint = 'Generate grid'
      ImageIndex = 58
      OnExecute = acGenerateGridExecute
    end
    object acExportPhastInputFile: TAction
      Category = 'File'
      Caption = '&PHAST Input File'
      HelpContext = 1550
      Hint = 'Run PHAST'
      ImageIndex = 72
      OnExecute = acExportPhastInputFileExecute
    end
    object acShowGridShell: TAction
      Category = 'View'
      Caption = 'Show &Grid Shell'
      Checked = True
      HelpContext = 2260
      HelpKeyword = 'View'
      Hint = 'Show grid shell|Show the grid shell'
      ImageIndex = 47
      OnExecute = acShowGridShellExecute
    end
    object acShowTopGrid: TAction
      Category = 'View'
      Caption = 'Show &Top Grid'
      HelpContext = 2260
      HelpKeyword = 'View'
      Hint = 'Show top grid|Show the grid lines of the selected layer in 3D'
      ImageIndex = 61
      OnExecute = acShowTopGridExecute
    end
    object acShowFrontGrid: TAction
      Category = 'View'
      Caption = 'Show &Front Grid'
      HelpContext = 2260
      HelpKeyword = 'View'
      Hint = 'Show front grid|Show the grid lines of the selected row in 3D'
      ImageIndex = 62
      OnExecute = acShowFrontGridExecute
    end
    object acShowSideGrid: TAction
      Category = 'View'
      Caption = 'Show &Side Grid'
      HelpContext = 2260
      HelpKeyword = 'View'
      Hint = 'Show side grid|Show the grid lines of the selected column in 3D'
      ImageIndex = 63
      OnExecute = acShowSideGridExecute
    end
    object acRestoreDefaultView: TAction
      Category = 'Navigation'
      Caption = '&Restore Default 3D View'
      HelpContext = 2260
      HelpKeyword = 'View'
      Hint = 'Restore default view of model in 3D'
      OnExecute = acRestoreDefaultViewExecute
    end
    object acColoredGrid: TAction
      Category = 'View'
      Caption = 'Show 3D &Colored Grid'
      Enabled = False
      HelpContext = 2260
      HelpKeyword = 'View'
      Hint = 'Show colored 3D grid|Show colored grid in 3D view (slow)'
      ImageIndex = 64
      OnExecute = tb3DColorsClick
    end
    object acShow3DObjects: TAction
      Category = 'View'
      Caption = 'Show &3D Objects'
      Checked = True
      HelpContext = 2260
      HelpKeyword = 'View'
      Hint = 'Show 3D objects|Show objects in 3D and cross-sectional views'
      ImageIndex = 52
      OnExecute = tb3DObjectsClick
    end
    object acShowHideObjects: TAction
      Category = 'Object'
      Caption = 'Show or &Hide Objects...'
      HelpContext = 2240
      HelpKeyword = 'Show_or_Hide_Objects_Dialog_Box'
      Hint = 'Show or hide objects|Display "Show or Hide Objects" dialog box'
      ImageIndex = 65
      ShortCut = 49231
      OnExecute = miShowHideObjectsClick
    end
    object acModflowActive: TAction
      Category = 'ModelSelection'
      Caption = 'MODFLOW-2005'
      GroupIndex = 1
      HelpContext = 2390
      HelpKeyword = 'Model'
      Hint = 'Make MODFLOW-2005 the selected model type'
      OnExecute = acModflowActiveExecute
    end
    object acPhastActive: TAction
      Category = 'ModelSelection'
      Caption = 'PHAST'
      Checked = True
      GroupIndex = 1
      HelpContext = 2390
      HelpKeyword = 'Model'
      Hint = 'Make PHAST the selected model type'
      OnExecute = acPhastActiveExecute
    end
    object acFileNewPhastModel: TAction
      Category = 'File'
      Caption = 'New &PHAST Model'
      HelpContext = 1550
      HelpKeyword = 'File'
      Hint = 'New PHAST model|Create a new PHAST model'
      OnExecute = acFileNewModflowModelExecute
    end
    object acLayers: TAction
      Category = 'Modflow Options'
      Caption = 'MODFLOW &Layer Groups...'
      HelpContext = 2400
      HelpKeyword = 'MODFLOW_Layer_Groups_Dialog_Box'
      Hint = 'Edit the layer properties'
      OnExecute = acLayersExecute
    end
    object acSelectColRowLay: TAction
      Category = 'Grid'
      Caption = 'Select Column, Row, or Layer'
      HelpContext = 1730
      HelpKeyword = 'Select_Column_Row_and_Layer'
      Hint = 
        'Select column, row, or layer|Click on grid to set selected colum' +
        'n, row, and layer.'
      ImageIndex = 66
      OnExecute = acSelectColRowLayExecute
    end
    object acSelectAllFront: TAction
      Category = 'Edit'
      Caption = 'Select All &Front'
      HelpContext = 1630
      HelpKeyword = 'Edit'
      Hint = 'Select all visible screen objects on front view'
      OnExecute = acSelectAllFrontExecute
    end
    object acSelectAllSide: TAction
      Category = 'Edit'
      Caption = 'Select All &Side'
      HelpContext = 1630
      HelpKeyword = 'Edit'
      Hint = 'Select all visible screen objects on side view'
      OnExecute = acSelectAllSideExecute
    end
    object acAddPolygonsToObject: TAction
      Category = 'Object'
      Caption = 'Add Polygon Sections'
      Enabled = False
      HelpContext = 1530
      HelpKeyword = 'Editing_Objects'
      Hint = 
        'Add polygon sections|Add a new polygon section to the selected o' +
        'bject'
      ImageIndex = 67
      OnExecute = acAddPolygonsToObjectExecute
    end
    object acAddLinesToObject: TAction
      Category = 'Object'
      Caption = 'Add Polyline Sections'
      Enabled = False
      HelpContext = 1530
      HelpKeyword = 'Editing_Objects'
      Hint = 
        'Add polyline sections|Add a new polyline section to the selected' +
        ' object'
      ImageIndex = 68
      OnExecute = acAddLinesToObjectExecute
    end
    object acAddPointsToObject: TAction
      Category = 'Object'
      Caption = 'Add Point Sections'
      Enabled = False
      HelpContext = 1530
      HelpKeyword = 'Editing_Objects'
      Hint = 
        'Add point sections|Add a new point sections to the selected obje' +
        'ct'
      ImageIndex = 69
      OnExecute = acAddPointsToObjectExecute
    end
    object acRunModflow: TAction
      Category = 'File'
      Caption = '&MODFLOW-2005 Input Files'
      HelpContext = 1550
      Hint = 'Run MODFLOW-2005'
      ImageIndex = 72
      OnExecute = miExportModflowClick
    end
    object acPositionForward: TAction
      Category = 'Navigation'
      Caption = 'Redo Position'
      Hint = 'Redo change in view'
      ImageIndex = 74
      OnExecute = acPositionForwardExecute
    end
    object acPositionBackward: TAction
      Category = 'Navigation'
      Caption = 'Undo Position'
      Hint = 'Undo change in view'
      ImageIndex = 75
      OnExecute = acPositionBackwardExecute
    end
    object acExportModpath: TAction
      Category = 'File'
      Caption = 'MODPATH Input Files'
      HelpContext = 1550
      Hint = 'Run MODPATH'
      OnExecute = acExportModpathExecute
    end
    object acExportModelMate: TAction
      Category = 'File'
      Caption = 'Export or Update ModelMate File'
      HelpContext = 1550
      Hint = 'Export parameters and observations to a ModelMate file'
      OnExecute = acExportModelMateExecute
    end
    object acImportModelMate: TAction
      Category = 'File'
      Caption = 'ModelMate Values'
      HelpContext = 1550
      Hint = 'Import parameters and observations from a ModelMate file'
      OnExecute = acImportModelMateExecute
    end
    object acEditSelecteObjects: TAction
      Category = 'Object'
      Caption = 'Edit Selected Object(s) Properties'
      Enabled = False
      HelpContext = 1900
      Hint = 
        'Edit the location, data sets, features and other properties of t' +
        'he selected objects'
      OnExecute = miEditSelectedObjectsClick
    end
    object acVertexValue: TAction
      Category = 'Object'
      Caption = '&Vertex Values'
      HelpContext = 3570
      HelpKeyword = 'Vertex_Values_Dialog_Box'
      Hint = 
        'Edit vertex values|Double-click on the vertex of a selected obje' +
        'ct to edit the values associated with that vertex.'
      ImageIndex = 77
      OnExecute = acVertexValueExecute
    end
    object acShowAllGridLines: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Show All'
      Checked = True
      GroupIndex = 2
      Hint = 'Show all the grid lines'
      ImageIndex = 71
      OnExecute = SetGridLineDrawingChoice
    end
    object acShowExteriorGridLines: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Show Exterior'
      GroupIndex = 2
      Hint = 'Show only the outermost grid lines'
      ImageIndex = 78
      OnExecute = SetGridLineDrawingChoice
    end
    object acShowActiveGridLines: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Show Active'
      GroupIndex = 2
      Hint = 'Show the grid lines for active cells'
      ImageIndex = 80
      OnExecute = SetGridLineDrawingChoice
    end
    object acShowActiveEdge: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Show Active Edge'
      GroupIndex = 2
      Hint = 'Show outline of the actve area'
      ImageIndex = 79
      OnExecute = SetGridLineDrawingChoice
    end
    object acRestoreDefault2DView: TAction
      Category = 'Navigation'
      Caption = 'Restore Default 2D View'
      Hint = 'Restore default 2D view|Restore default view of model'
      ImageIndex = 73
      ShortCut = 16466
      OnExecute = RestoreDefault2DView1Click
    end
    object acExportImage: TAction
      Category = 'File'
      Caption = '&Image...'
      HelpContext = 1600
      Hint = 'Export image|Export an image of the top, front, or side view'
      ImageIndex = 83
      OnExecute = acExportImageExecute
    end
    object acShowGridValues: TAction
      Category = 'Data'
      Caption = 'Show Grid or Mesh &Values'
      HelpContext = 1880
      Hint = 'Show grid or mesh values'
      ImageIndex = 84
      ShortCut = 49235
      OnExecute = acShowGridValuesClick
    end
    object acModflowLgrActive: TAction
      Category = 'ModelSelection'
      Caption = 'MODFLOW-LGR V1'
      GroupIndex = 1
      HelpContext = 2390
      HelpKeyword = 'Model'
      Hint = 'Make MODFLOW-LGR V1 the selected model type'
      OnExecute = acModflowLgrActiveExecute
    end
    object acImportModelResults: TAction
      Category = 'File'
      Caption = 'Model Results...'
      HelpContext = 3387
      Hint = 'Import and display model results'
      ImageIndex = 85
      ShortCut = 16461
      OnExecute = miModelResultsClick
    end
    object acRunModflowLgr: TAction
      Category = 'File'
      Caption = '&MODFLOW-LGR Input Files'
      HelpContext = 1550
      Hint = 'Run MODFLOW-LGR'
      ImageIndex = 72
      OnExecute = acRunModflowLgrExecute
    end
    object acUndo: TAction
      Category = 'Edit'
      Caption = '&Undo'
      Enabled = False
      HelpContext = 1630
      ImageIndex = 3
      ShortCut = 16474
      OnExecute = tbUndoClick
    end
    object acRedo: TAction
      Category = 'Edit'
      Caption = '&Redo'
      Enabled = False
      HelpContext = 1630
      ImageIndex = 4
      ShortCut = 16473
      OnExecute = tbRedoClick
    end
    object acModflowLgr2Active: TAction
      Category = 'ModelSelection'
      Caption = 'MODFLOW-LGR V2'
      GroupIndex = 1
      HelpContext = 2390
      HelpKeyword = 'Model'
      Hint = 'Make MODFLOW-LGR V2 the selected model type'
      OnExecute = acModflowLgr2ActiveExecute
    end
    object acModflowNwtActive: TAction
      Category = 'ModelSelection'
      Caption = 'MODFLOW-NWT'
      GroupIndex = 1
      HelpContext = 2390
      HelpKeyword = 'Model'
      Hint = 'Make MODFLOW-NWT the selected model type'
      OnExecute = acModflowNwtActiveExecute
    end
    object acRunModflowNwt: TAction
      Category = 'File'
      Caption = '&MODFLOW-NWT Input Files'
      HelpContext = 1550
      Hint = 'Run MODFLOW-NWT'
      ImageIndex = 72
      OnExecute = acRunModflowNwtExecute
    end
    object acMeasure: TAction
      Category = 'Navigation'
      Caption = 'Measure'
      Hint = 
        'Measure distance|click on the model to measure distances between' +
        ' points'
      ImageIndex = 86
      OnExecute = acMeasureExecute
    end
    object acDisplayData: TAction
      Category = 'Data'
      Caption = 'Data Visualization...'
      Hint = 'Data visualization|Color the grid, contour data, etc.'
      ImageIndex = 87
      OnExecute = acDisplayDataExecute
    end
    object acRunMt3dms: TAction
      Category = 'File'
      Caption = 'MT3D Input Files'
      Hint = 'Run Mt3DMS'
      OnExecute = acRunMt3dmsExecute
    end
    object acExportZoneBudget: TAction
      Category = 'File'
      Caption = '&ZONEBUDGET Input Files'
      Hint = 'Run ZoneBudget'
      OnExecute = miZONEBUDGETInputFilesClick
    end
    object acModflowCfpActive: TAction
      Category = 'ModelSelection'
      Caption = 'MODFLOW-CFP'
      GroupIndex = 1
      HelpContext = 2390
      HelpKeyword = 'Model'
      Hint = 'Make MODFLOW-CFP the selected model type'
      OnExecute = acModflowCfpActiveExecute
    end
    object acModflowFmpActive: TAction
      Category = 'ModelSelection'
      Caption = 'MODFLOW-OWHM V1'
      GroupIndex = 1
      HelpContext = 2390
      HelpKeyword = 'Model'
      Hint = 'Make MODFLOW-OWHM version 1 the selected model type'
      OnExecute = acModflowFmpActiveExecute
    end
    object acSutra22Active: TAction
      Category = 'ModelSelection'
      Caption = 'SUTRA 2.2'
      GroupIndex = 1
      HelpContext = 2390
      HelpKeyword = 'Model'
      Hint = 'Make SUTRA 2.2 the selected model type'
      OnExecute = acSutra22ActiveExecute
    end
    object acSutra30Active: TAction
      Category = 'ModelSelection'
      Caption = 'SUTRA 3.0'
      GroupIndex = 1
      HelpContext = 2390
      HelpKeyword = 'Model'
      Hint = 'Make SUTRA 3.0 the selected model type'
      OnExecute = acSutra30ActiveExecute
    end
    object acFootPrintActive: TAction
      Category = 'ModelSelection'
      Caption = 'WellFootprint'
      GroupIndex = 1
      HelpContext = 2390
      HelpKeyword = 'Model'
      Hint = 'Define Well Footprints'
      OnExecute = acFootPrintActiveExecute
    end
    object acSutraLayers: TAction
      Category = 'Model'
      Caption = 'SUTRA Layer Groups...'
      Hint = 'Edit SUTRA layer groups'
      OnExecute = acSutraLayersExecute
    end
    object acSutraOptions: TAction
      Category = 'Model'
      Caption = 'Sutra Options...'
      Hint = 'Edit options in SUTRA'
      OnExecute = acSutraOptionsExecute
    end
    object acSutraTimes: TAction
      Category = 'Model'
      Caption = 'SUTRA Time Controls...'
      Hint = 'Edit time controls for SUTRA'
      OnExecute = acSutraTimesExecute
    end
    object acRunSutra: TAction
      Category = 'File'
      Caption = 'SUTRA Input Files'
      Hint = 'Run SUTRA'
      ImageIndex = 72
      OnExecute = acRunSutraExecute
    end
    object acSutraOutputControl: TAction
      Category = 'Model'
      Caption = 'SUTRA Output Control...'
      Hint = 'Control output from SUTRA'
      OnExecute = acSutraOutputControlExecute
    end
    object acSutraProgramLocations: TAction
      Category = 'Model'
      Caption = 'SUTRA Program Location...'
      Hint = 'Specify the location of the SUTRA program'
      OnExecute = acSutraProgramLocationsExecute
    end
    object acImportTprogs: TAction
      Category = 'File'
      Caption = 'T-PROGS Binary Grid File...'
      Hint = 'Import data from a T-Progs binary grid file'
      OnExecute = acImportTprogsExecute
    end
    object acImportSutraModelResults: TAction
      Category = 'File'
      Caption = 'SUTRA Model Results...'
      Hint = 'Import model results from SUTRA'
    end
    object acShowTopMesh: TAction
      Category = 'View'
      Caption = 'Show &Top Mesh'
      Checked = True
      HelpKeyword = 'View'
      Hint = 'Show top mesh|Show the elements of the selected layer in 3D'
      ImageIndex = 91
      OnExecute = acShowTopMeshExecute
    end
    object acShowFrontMesh: TAction
      Category = 'View'
      Caption = 'Show &Front Cross Section'
      Checked = True
      HelpKeyword = 'View'
      Hint = 
        'Show front cross section|Show the elements along the cross secti' +
        'on in 3D'
      ImageIndex = 92
      OnExecute = acShowFrontMeshExecute
    end
    object acNewSutraModel: TAction
      Category = 'File'
      Caption = 'New &SUTRA Model'
      Hint = 'New SUTRA model|Create a new SUTRA model'
      OnExecute = acFileNewModflowModelExecute
    end
    object acFishnet: TAction
      Category = 'Mesh'
      Caption = 'Draw Fishnet Mesh Quadrilaterals'
      Hint = 
        'Draw fishnet mesh quadilaterals|Draw fishnet mesh quadrilaterals' +
        ' to be used in generating a mesh.'
      ImageIndex = 93
      OnExecute = acFishnetExecute
    end
    object acMoveCrossSection: TAction
      Category = 'Mesh'
      Caption = '&Move Cross Section'
      Hint = 
        'Move cross section|Drag the cross section or one of its endpoint' +
        's to a new position'
      ImageIndex = 88
      OnExecute = tbCrossSectionClick
    end
    object acRotateCrossSection: TAction
      Category = 'Mesh'
      Caption = '&Rotate Cross Section'
      Hint = 'Rotate cross section|Rotate the cross section by dragging it.'
      ImageIndex = 89
      OnExecute = tbRotateCrossSectionClick
    end
    object acMoveNodesOrElements: TAction
      Category = 'Mesh'
      Caption = '&Move or Delete Nodes or Elements'
      Hint = 
        'Move or delete nodes or elements|Click on nodes or elements to s' +
        'elect and then delete or move them. Double-click to edit node po' +
        'sitions numerically.'
      ImageIndex = 90
      OnExecute = tbMoveNodesClick
    end
    object acDrawElement: TAction
      Category = 'Mesh'
      Caption = 'Draw Elements'
      Hint = 'Draw elements|Click at four locations to draw a new element'
      ImageIndex = 94
      OnExecute = tbDrawElementClick
    end
    object acDefaultCrossSection: TAction
      Category = 'Mesh'
      Caption = 'Set Default Cross Section'
      Hint = 'Restore cross section to the default location'
      OnExecute = acDefaultCrossSectionExecute
    end
    object acRunModflowFmp: TAction
      Category = 'File'
      Caption = 'MODFLOW-OWHM Input Files'
      Hint = 'Run MODFLOW-OWHM'
      ImageIndex = 72
      OnExecute = acRunModflowFmpExecute
    end
    object acFarmCrops: TAction
      Category = 'Model'
      Caption = 'Farm Crops...'
      Hint = 'Edit Crops'
      OnExecute = acFarmCropsExecute
    end
    object acFarmSoils: TAction
      Category = 'Model'
      Caption = 'Farm Soils...'
      Hint = 'Edit soils'
      OnExecute = acFarmSoilsExecute
    end
    object acFarmClimate: TAction
      Category = 'Model'
      Caption = 'Farm Climate...'
      Hint = 'Edit climate variables'
      OnExecute = acFarmClimateExecute
    end
    object acFarmAllotment: TAction
      Category = 'Model'
      Caption = 'Farm Allotment...'
      Hint = 'Edit allowable allotment of farms'
      OnExecute = acFarmAllotmentExecute
    end
    object acRunModflowCfp: TAction
      Category = 'File'
      Caption = 'MODFLOW-CFP Input Files'
      Hint = 'Run MODFLOW-CFP'
      ImageIndex = 72
      OnExecute = acRunModflowCfpExecute
    end
    object acHeadObsToShapefile: TAction
      Category = 'File'
      Caption = 'Head Observations to Shapefile'
      Hint = 'Export head observations as Shapefiles'
      OnExecute = acHeadObsToShapefileExecute
    end
    object acSWR_Tabfiles: TAction
      Category = 'Model'
      Caption = 'Tab Files...'
      Hint = 'Edit tab files used in sWR'
      OnExecute = acSWR_TabfilesExecute
    end
    object acSWR_ReachGeometry: TAction
      Category = 'Model'
      Caption = 'Reach Geometry...'
      Hint = 'Edit the geometry assigned to reaches'
      OnExecute = acSWR_ReachGeometryExecute
    end
    object acSwrStructures: TAction
      Category = 'Model'
      Caption = 'Structures...'
      Hint = 'Edit structures'
      OnExecute = acSwrStructuresExecute
    end
    object acSwrObservations: TAction
      Category = 'Model'
      Caption = 'Observations...'
      Hint = 'Edit SWR observations'
      OnExecute = acSwrObservationsExecute
    end
    object acImportGriddedDataFiles: TAction
      Category = 'File'
      Caption = 'Gridded Data Files...'
      Hint = 'Import data from multiple files containing gridded data'
      OnExecute = acImportGriddedDataFilesExecute
    end
    object acImportSutraMesh: TAction
      Category = 'File'
      Caption = 'SUTRA Mesh'
      Enabled = False
      Hint = 'Import a SUTRA mesh created outside of ModelMuse'
      OnExecute = acImportSutraMeshExecute
    end
    object acExportSutra2DMesh: TAction
      Category = 'File'
      Caption = 'SUTRA 2D Mesh'
      Hint = 
        'Export SUTRA Mesh to Argus ONE|Export SUTRA Mesh to a file in th' +
        'e format used by Argus ONE'
      OnExecute = acExportSutra2DMeshExecute
    end
    object acEditFarms: TAction
      Category = 'Model'
      Caption = 'Water Balance Subregions (Farms)...'
      Hint = 'Edit water balance subregions'
      OnExecute = acEditFarmsExecute
    end
    object acFootprintProperties: TAction
      Category = 'Model'
      Caption = 'WellFootprint Properties...'
      Hint = 
        'Speciy variables controlling the generation of the well footprin' +
        'ts'
      OnExecute = acFootprintPropertiesExecute
    end
    object acNewFootprintModel: TAction
      Category = 'File'
      Caption = 'New &WellFootprint Project'
      Hint = 'Create a new footprint model'
      OnExecute = acFileNewModflowModelExecute
    end
    object acRunFootprint: TAction
      Category = 'File'
      Caption = 'WellFootprint Input File'
      Hint = 'Run WellFootprint'
      ImageIndex = 72
      OnExecute = acRunFootprintExecute
    end
    object acFootprintProgramLocation: TAction
      Category = 'Model'
      Caption = 'WellFootprint Program Location...'
      Hint = 'Specify where the well footprint program is located'
      OnExecute = acFootprintProgramLocationExecute
    end
    object acModflow6Active: TAction
      Category = 'Modflow Options'
      Caption = 'MODFLOW-6'
      GroupIndex = 1
      HelpContext = 2390
      HelpKeyword = 'Model'
      Hint = 'Make MODFLOW 6 the selected model type'
      OnExecute = acModflow6ActiveExecute
    end
    object acRunModflow6: TAction
      Category = 'File'
      Caption = 'MODFLOW 6 Input Files'
      HelpContext = 1550
      Hint = 'Run MODFLOW 6'
      ImageIndex = 72
      OnExecute = acRunModflow6Execute
    end
    object acRipPlantGroups: TAction
      Category = 'Model'
      Caption = 'Riparean ET Plant Groups'
      Enabled = False
      Hint = 'Display the Riparian ET Plant Groups dialog box'
      OnExecute = acRipPlantGroupsExecute
    end
    object acOutlineToShapefile: TAction
      Category = 'File'
      Caption = 'Model Outline or Grid Lines...'
      Hint = 
        'Export the outline of the model or the model grid lines to a sha' +
        'pefile.'
      OnExecute = acOutlineToShapefileExecute
    end
    object acShowOrHideRulers: TAction
      Category = 'Customize'
      Caption = 'Show or Hide Rulers'
      Hint = 'Show or hide rulers in 2D views|Show or hide rulers in 2D views'
      OnExecute = acShowOrHideRulersExecute
    end
    object acQuadmesh: TAction
      Category = 'Mesh'
      Caption = 'Refresh Quadtree Refinement'
      Hint = 
        'Refresh DISV grid|Refresh DISV grid from structured grid using q' +
        'uadtree refinement'
      ImageIndex = 96
      OnExecute = acQuadmeshExecute
    end
    object acSpecifyCrossSection: TAction
      Category = 'Mesh'
      Caption = 'Specify &Cross Section...'
      Hint = 'Specify the endpoints of the cross section'
      OnExecute = acSpecifyCrossSectionExecute
    end
    object acStructuredGrid: TAction
      Category = 'Mesh'
      Caption = 'Switch to Structured Grid'
      GroupIndex = 2
      Hint = 'Switch from a DISV grid to a Structured grid'
      OnExecute = acStructuredGridExecute
    end
    object acDisvGrid: TAction
      Category = 'Grid'
      Caption = 'Switch to DISV'
      GroupIndex = 2
      Hint = 'Switch to a DISV grid'
      OnExecute = acDisvGridExecute
    end
    object acRotateAroundGridOrigin: TAction
      Category = 'Grid'
      Caption = 'Rotate Grid Around Grid Origin'
      HelpContext = 1690
      HelpKeyword = 'Grid_Angle_Dialog_Box'
      Hint = 'Rotate the grid angle around its origin to a specific value'
      OnExecute = acRotateAroundGridOriginExecute
    end
    object acMoveGrid: TAction
      Category = 'Grid'
      Caption = 'Move Grid'
      Hint = 
        'Move the grid to a new position|Change where the grid origin is ' +
        'located'
      OnExecute = acMoveGridExecute
    end
    object acShowCellNumbers: TAction
      Category = 'View'
      Caption = 'Show Cell Numbers'
      Hint = 'Display cell numbers on the top view of the grid'
      OnExecute = acShowCellNumbersExecute
    end
    object acSimplifyScreenObjects: TAction
      Category = 'Object'
      Caption = 'Simplify Selected Objects'
      Enabled = False
      Hint = 
        'Delete un-needed vertices|Delete un-needed vertices from the sel' +
        'ected object(s)'
      OnExecute = acSimplifyScreenObjectsExecute
    end
    object acEditCTS: TAction
      Category = 'Model'
      Caption = 'Contaminant Treatment Systems...'
      Hint = 'Display the Contaminant Treatment Systems dialog box'
      OnExecute = acEditCTSExecute
    end
    object acEditObservationComparisons: TAction
      Category = 'Model'
      Caption = 'Edit Comparison Observations...'
      OnExecute = acEditObservationComparisonsExecute
    end
    object acAnonymizeObjects: TAction
      Category = 'Object'
      Caption = 'Anonymize Selected Point Objects'
      OnExecute = acAnonymizeObjectsExecute
    end
    object acEditSutraFluxObs: TAction
      Category = 'Model'
      Caption = 'Manage SUTRA Boundary Observations...'
      OnExecute = acEditSutraFluxObsExecute
    end
    object acPEST: TAction
      Category = 'Model'
      Caption = '&PEST Properties...'
      OnExecute = acPESTExecute
    end
    object acRunPest: TAction
      Category = 'File'
      Caption = 'Export PEST Control File'
      Hint = 'Run PEST|Export PEST control file and run PEST'
      OnExecute = acRunPestExecute
    end
    object acArchiveModel: TAction
      Category = 'Customize'
      Caption = 'Archive model by default'
      Checked = True
      OnExecute = acArchiveModelExecute
    end
    object acImportSutraFiles: TAction
      Category = 'File'
      Caption = 'SUTRA Files'
      OnExecute = acImportSutraFilesExecute
    end
    object acAddPilotPoint: TAction
      Category = 'Edit'
      Caption = 'Add Pilot Point'
      Enabled = False
      Hint = 'Add pilot point|Click on the top view to add a pilot point'
      ImageIndex = 97
      OnExecute = acAddPilotPointExecute
    end
    object acDeletePilotPoint: TAction
      Category = 'Edit'
      Caption = 'Delete Pilot Point(s)'
      Enabled = False
      Hint = 
        'delete pilot point(s)|Click or click and drag to delete pilot po' +
        'ints.'
      ImageIndex = 98
      OnExecute = acDeletePilotPointExecute
    end
    object acExportParRep: TAction
      Category = 'File'
      Caption = 'Replace Parameters in PEST Control File'
      OnExecute = acExportParRepExecute
    end
    object acRunSvdaPrep: TAction
      Category = 'File'
      Caption = 'Modify PEST Control File with SVDAPREP'
      OnExecute = acRunSvdaPrepExecute
    end
    object acCalcSuperParameters: TAction
      Category = 'File'
      Caption = 'Calculate Number of Super-Parameters'
      Hint = 'Run SUPCALC'
      OnExecute = acCalcSuperParametersExecute
    end
    object acImportMf6FeatureFromPest: TAction
      Category = 'File'
      Caption = 'MODFLOW 6 Feature'
      OnExecute = acImportMf6FeatureFromPestExecute
    end
    object acImportSutraFeaturesFromPest: TAction
      Category = 'File'
      Caption = 'SUTRA Feature'
      OnExecute = acImportSutraFeaturesFromPestExecute
    end
    object acTimeSeries: TAction
      Category = 'Model'
      Caption = 'Edit Time Series...'
      OnExecute = acTimeSeriesExecute
    end
    object acSutra40Active: TAction
      Category = 'ModelSelection'
      Caption = 'SUTRA 4.0'
      GroupIndex = 1
      HelpContext = 2390
      HelpKeyword = 'Model'
      Hint = 'Make SUTRA 4.0 the selected model type'
      OnExecute = acSutra40ActiveExecute
    end
    object acModflowOwhmV2: TAction
      Category = 'ModelSelection'
      Caption = 'MODFLOW-OWHM V2'
      GroupIndex = 1
      HelpContext = 2390
      HelpKeyword = 'Model'
      Hint = 'Make MODFLOW-OWHM version 2 the selected model type'
      OnExecute = acModflowOwhmV2Execute
    end
    object acRunModflowOWHM_V2: TAction
      Category = 'File'
      Caption = 'Run MODFLOW-OWHM V2'
      Hint = 'Run MODFLOW-OWHM version 2'
      ImageIndex = 72
      OnExecute = acRunModflowOWHM_V2Execute
    end
    object acFarmIrrigationTypes: TAction
      Category = 'Model'
      Caption = 'Irrigation Types...'
      OnExecute = acFarmIrrigationTypesExecute
    end
  end
  object ilDisabledImageList: TImageList
    Left = 104
    Top = 128
    Bitmap = {
      494C010148004D00040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003001000001002000000000000030
      0100000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF00000000FFFF0000FFFF0000FFFF0000FFFF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF00000000FFFF0000FFFF0000FFFF0000FFFF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF00000000FFFF0000FFFF0000FFFF0000FFFF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF00000000FFFF0000FFFF0000FFFF0000FFFF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0040C0E00040C0E00040C0E00040C0E000FFFF0000FFFF
      0000FFFF0000FFFF000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0040C0E00040C0E00040C0E00040C0E000FFFF0000FFFF
      0000FFFF0000FFFF000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0040C0E00040C0E00040C0E00040C0E000FFFF0000FFFF
      0000FFFF0000FFFF000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0040C0E00040C0E00040C0E00040C0E000FFFF0000FFFF
      0000FFFF0000FFFF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF0000FF000000FF000000FF000000FF0000FF000000FF00
      0000FF000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF0000FF000000FF000000FF000000FF0000FF000000FF00
      0000FF000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF0000FF000000FF000000FF000000FF0000FF000000FF00
      0000FF000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF0000FF000000FF000000FF000000FF0000FF000000FF00
      0000FF000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF0000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF0000000000FF000000FF000000FF000000FF00000000000000FF00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF0000000000FF000000FF000000FF000000FF00000000000000FF00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF0000000000FF000000FF000000FF000000FF00000000000000FF00
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000800000008000000080
      00000080000000800000FF000000FF000000FF00000000800000008000000080
      0000008000000080000000800000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000000000000000FF000000FF000000FF000000FF00000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000800000008000000080
      0000008000000080000000800000FF0000000080000000800000008000000080
      0000008000000080000000800000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000000000000000FF000000FF000000FF000000FF00000000000000
      FF00000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000800000008000000080
      000000800000008000000080000000800000FF00000000800000008000000080
      0000008000000080000000800000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000000000000000FF000000FF000000FF000000FF00000000000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000800000008000000080
      0000008000000080000000800000FF000000FF000000FF000000008000000080
      0000008000000080000000800000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF0000000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF000000000000000000FF000000FF000000FF000000FF000000
      000000000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF0000000000FF000000FF000000FF00
      0000FF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000000000000000
      000000000000000000000000000000000000FF00000000000000000000000000
      0000000000000000000000000000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF0000000000
      000000000000000000000000000000000000FF00000000000000000000000000
      00000000000000000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF0000000000
      000000000000000000000000000000000000FF00000000000000000000000000
      00000000000000000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000000000000000
      000000000000000000000000000000000000FF00000000000000000000000000
      0000000000000000000000000000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B3B3B300B3B3
      B300B3B3B300B3B3B300B3B3B300B3B3B300B3B3B300B3B3B300B3B3B300B3B3
      B300B3B3B300B3B3B30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7D7B0000000000000000000000
      00000000000000000000000000007B7D7B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B3B3B300FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00B3B3B3000000000000000000000000007B7D7B007B7D7B000000
      00000000000000000000000000007B7D7B007B7D7B0000000000000000000000
      0000000000007B7D7B007B7D7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B3B3B300FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00B3B3B300000000000000000000000000000000007B7D7B007B7D
      7B007B7D7B007B7D7B007B7D7B007B7D7B007B7D7B007B7D7B007B7D7B007B7D
      7B007B7D7B007B7D7B0000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000084828400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B3B3B300FCFC
      FC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFC
      FC00FCFCFC00B3B3B30000000000000000000000000000000000000000007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      00007B7D7B000000000000000000000000000000000000000000848284008482
      8400848284000000000000000000000000008482840084828400848284000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B3B3B300F5F5
      F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5
      F500F5F5F500B3B3B30000000000000000000000000000000000000000007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      00007B7D7B000000000000000000000000000000000000000000848284008482
      8400848284000000000000000000000000008482840084828400848284000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B3B3B300F5F5
      F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5
      F500F5F5F500B3B3B30000000000000000000000000000000000000000007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      00007B7D7B000000000000000000000000000000000000000000000000008482
      8400848284000000000000000000000000000000000084828400848284000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B3B3B300F5F5
      F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5
      F500F5F5F500B3B3B30000000000000000000000000000000000000000007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      00007B7D7B000000000000000000000000000000000000000000000000008482
      8400848284000000000000000000000000000000000084828400848284000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B3B3B300F5F5
      F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5F500F5F5
      F500F5F5F500B3B3B30000000000000000007B7D7B007B7D7B007B7D7B007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      00007B7D7B007B7D7B007B7D7B00000000000000000000000000000000008482
      8400848284008482840000000000000000000000000084828400848284008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B3B3B300EDED
      ED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDED
      ED00EDEDED00B3B3B3000000000000000000000000007B7D7B007B7D7B007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      00007B7D7B007B7D7B007B7D7B007B7D7B000000000000000000000000008482
      8400848284008482840000000000000000000000000084828400848284008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B3B3B300EDED
      ED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00D1D1D100D1D1D100D1D1
      D100D1D1D100B3B3B30000000000000000000000000000000000000000007B7D
      7B00000000000000000000000000000000007B7D7B007B7D7B007B7D7B007B7D
      7B007B7D7B000000000000000000000000000000000000000000000000000000
      0000848284000000000000000000000000000000000000000000848284000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B3B3B300EDED
      ED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00BABABA00BABABA00BABA
      BA00BABABA00B3B3B30000000000000000000000000000000000000000007B7D
      7B00000000000000000000000000000000007B7D7B0000000000000000007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B3B3B300EDED
      ED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00AEAEAE00AEAEAE00AEAE
      AE00AEAEAE00B3B3B30000000000000000000000000000000000000000007B7D
      7B00000000000000000000000000000000007B7D7B00000000007B7D7B007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B3B3B300E4E4
      E400E4E4E400E4E4E400E4E4E400E4E4E400BABABA00FCFCFC00F5F5F500DDDD
      DD00B3B3B3000000000000000000000000000000000000000000000000007B7D
      7B00000000000000000000000000000000007B7D7B007B7D7B00000000007B7D
      7B007B7D7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B3B3B300E4E4
      E400E4E4E400E4E4E400E4E4E400EDEDED00BABABA00F5F5F500D1D1D100B3B3
      B3000000000000000000000000000000000000000000000000007B7D7B007B7D
      7B007B7D7B007B7D7B007B7D7B007B7D7B007B7D7B0000000000000000000000
      00007B7D7B007B7D7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B3B3B300E4E4
      E400E4E4E400E4E4E400E4E4E400E4E4E400BFBFBF00D1D1D100B3B3B3000000
      000000000000000000000000000000000000000000007B7D7B007B7D7B000000
      00000000000000000000000000007B7D7B007B7D7B0000000000000000000000
      0000000000007B7D7B007B7D7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B3B3B300B3B3
      B300B3B3B300B3B3B300B3B3B300B3B3B300B3B3B300B3B3B300000000000000
      0000000000000000000000000000000000007B7D7B0000000000000000000000
      00000000000000000000000000007B7D7B000000000000000000000000000000
      00000000000000000000000000007B7D7B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C3C6000000000000000000000000000000000000000000C6C3C6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400C6C3C6000000000000000000000000000000000084828400C6C3C6000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848284008482
      840084828400C6C3C6000000000000000000848284008482840084828400C6C3
      C600000000000000000000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400848284000000000000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848284008482
      840084828400C6C3C6000000000000000000848284008482840084828400C6C3
      C600000000000000000000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400848284008482840000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      840084828400C6C3C6000000000000000000000000008482840084828400C6C3
      C600000000000000000000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400848284008482840000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      840084828400C6C3C6000000000000000000000000008482840084828400C6C3
      C600000000000000000000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400848284008482840000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      84008482840084828400C6C3C600000000000000000084828400848284008482
      8400C6C3C6000000000000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400848284008482840000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400848284008482840000000000000000000000000084828400848284008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000C6C3C600C6C3C600C6C3C600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400848284008482840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848284000000000000000000000000000000000000000000848284000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C3C600C6C3C600C6C3C600C6C3C600C6C3C60000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400848284008482840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C3
      C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C6000000
      000000000000000000000000000000000000000000000000000000000000C6C3
      C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C6000000
      0000848284008482840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C3
      C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3
      C600000000000000000000000000000000000000000000000000000000000000
      0000C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3
      C600000000008482840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C6C3C600C6C3C600C6C3C600C6C3C6000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3
      C600C6C3C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000808080008080800080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000000000000000000080808000808080008080800000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000000000008080800000000000000000000000000080808000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000000000008080800000000000000000000000000080808000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000000000008080800080808000000000000000000080808000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000000000008080800000000000808080008080800000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000000000008080800000000000000000000000000080808000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000000000000000000080808000808080008080800000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000808080008080800080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7D
      7B007B7D7B007B7D7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7D7B007B7D
      7B00000000007B7D7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7D7B000000000000000000000000007B7D7B007B7D7B000000
      00007B7D7B007B7D7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7D7B007B7D7B00000000007B7D7B007B7D7B00000000007B7D
      7B007B7D7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7D7B00000000007B7D7B007B7D7B00000000007B7D7B007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B7D7B00000000000000000000000000000000007B7D7B007B7D7B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B7D7B00000000000000000000000000000000007B7D7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7D
      7B000000000000000000000000000000000000000000000000007B7D7B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7D
      7B0000000000000000000000000000000000000000007B7D7B007B7D7B007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7D7B000000
      00000000000000000000000000007B7D7B007B7D7B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7D7B000000
      0000000000007B7D7B007B7D7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7D7B00000000007B7D
      7B007B7D7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7D7B007B7D7B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7D
      7B00000000007B7D7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7D7B000000
      00000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7D7B000000000000000000000000007B7D7B00000000000000
      FF00000000007B7D7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7D7B00000000000000FF000000
      00007B7D7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF0000000000000000000000FF00000000007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF00000000007B7D7B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000808080008080800080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF000000FF000000FF0000000000000000007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084868400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BD000000BD000000BD000000BD000000BD000000BD000000BD00
      0000BD0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008486840084868400848684000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848684008486840084868400000000000000000000000000000000000000
      0000000000000000000080808000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008486
      8400848684008486840000000000000000000000000000000000000000000000
      0000000000000000000080808000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848684008486
      8400848684000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008486
      8400000000000000000000000000848684000000000000FFFF00848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848684008486840084868400000000000000000084868400848684008486
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008486
      840084868400848684008486840084868400000000000000000000FFFF000000
      0000000000000000000000000000000000000000000000000000848684008486
      8400000000000000000000000000848684008486840084868400848684000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000000000000000000080808000000000000000000080808000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084868400C6C7
      C60000000000C6C7C60000000000C6C7C6008486840000000000000000000000
      0000000000000000000000000000000000000000000084868400000000000000
      0000000000000000000000000000000000000000000084868400000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000080808000000000000000000080808000000000000000
      0000000000000000000080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008486840084868400C6C7C6000000
      0000C6C7C60000000000C6C7C60000000000C6C7C60084868400848684000000
      0000000000000000000000000000000000000000000084868400000000000000
      0000000000000000000000000000000000000000000084868400000000000000
      0000000000000000000000000000000000008080800080808000808080008080
      8000808080000000000080808000000000000000000080808000000000008080
      8000808080008080800080808000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008486840000000000C6C7
      C60000000000C6C7C60000000000C6C7C6000000000084868400000000000000
      0000000000000000000000000000000000008486840000000000000000000000
      0000000000000000000000000000000000000000000000000000848684000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000080808000000000000000000080808000000000000000
      0000000000000000000080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084868400C6C7C6000000
      0000C6C7C60000000000C6C7C60000000000C6C7C60084868400000000000000
      0000000000000000000000000000000000008486840000000000000000000000
      0000000000000000000000000000000000000000000000000000848684000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000000000000000000080808000000000000000000080808000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008486840000000000C6C7
      C60000000000C6C7C60000000000C6C7C6000000000084868400000000000000
      0000000000000000000000000000000000008486840000000000000000000000
      0000000000000000000000000000000000000000000000000000848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008486840084868400C6C7C6000000
      0000C6C7C60000000000C6C7C60000000000C6C7C60084868400848684000000
      0000000000000000000000000000000000000000000084868400000000000000
      0000000000000000000000000000000000000000000084868400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084868400C6C7
      C60000000000C6C7C60000000000C6C7C6008486840000000000000000000000
      0000000000000000000000000000000000000000000084868400000000000000
      0000000000000000000000000000000000000000000084868400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008486
      8400848684008486840084868400848684000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848684008486
      8400000000000000000000000000848684008486840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008486
      8400000000000000000000000000848684000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848684008486840084868400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B797B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B797B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B797B007B797B007B797B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B797B007B797B007B797B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B797B007B797B007B797B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B797B007B797B007B797B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B79
      7B007B797B007B797B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B79
      7B007B797B007B797B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B797B007B79
      7B007B797B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B797B007B79
      7B007B797B000000000000000000000000000000000000000000000000007B79
      7B000000000000000000000000007B797B000000000000FFFF007B797B000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B797B007B797B007B797B0000000000000000007B797B007B797B007B79
      7B00000000000000000000000000000000000000000000000000000000007B79
      7B000000000000000000000000007B797B000000000000FFFF007B797B000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B797B007B797B007B797B0000000000000000007B797B007B797B007B79
      7B00000000000000000000000000000000000000000000000000000000007B79
      7B007B797B007B797B007B797B007B797B00000000000000000000FFFF000000
      00000000000000000000000000000000000000000000000000007B797B007B79
      7B000000000000000000000000007B797B007B797B007B797B007B797B000000
      0000000000000000000000000000000000000000000000000000000000007B79
      7B007B797B007B797B007B797B007B797B00000000000000000000FFFF000000
      00000000000000000000000000000000000000000000000000007B797B007B79
      7B000000000000000000000000007B797B007B797B007B797B007B797B000000
      00000000000000000000000000000000000000000000000000007B797B00BDBE
      BD0000000000BDBEBD0000000000BDBEBD007B797B0000000000000000000000
      000000000000000000000000000000000000000000007B797B00000000000000
      000000000000000000000000000000000000000000007B797B00000000000000
      00000000000000000000000000000000000000000000000000007B797B000000
      0000BDBEBD0000000000BDBEBD00000000007B797B0000000000000000000000
      000000000000000000000000000000000000000000007B797B00000000000000
      000000000000000000000000000000000000000000007B797B00000000000000
      0000000000000000000000000000000000007B797B007B797B00BDBEBD000000
      0000BDBEBD0000000000BDBEBD0000000000BDBEBD007B797B007B797B000000
      000000000000000000000000000000000000000000007B797B00000000000000
      000000000000000000000000000000000000000000007B797B00000000000000
      0000000000000000000000000000000000007B797B007B797B0000000000BDBE
      BD00000000000000FF0000000000BDBEBD00000000007B797B007B797B000000
      000000000000000000000000000000000000000000007B797B00000000000000
      0000000000007B797B000000000000000000000000007B797B00000000000000
      000000000000000000000000000000000000000000007B797B0000000000BDBE
      BD0000000000BDBEBD0000000000BDBEBD00000000007B797B00000000000000
      0000000000000000000000000000000000007B797B0000000000000000000000
      00000000000000000000000000000000000000000000000000007B797B000000
      000000000000000000000000000000000000000000007B797B00BDBEBD000000
      0000BDBEBD000000FF00BDBEBD0000000000BDBEBD007B797B00000000000000
      0000000000000000000000000000000000007B797B0000000000000000000000
      0000000000007B797B00000000000000000000000000000000007B797B000000
      000000000000000000000000000000000000000000007B797B00BDBEBD000000
      FF000000FF000000FF000000FF000000FF00BDBEBD007B797B00000000000000
      0000000000000000000000000000000000007B797B0000000000000000007B79
      7B007B797B007B797B007B797B007B797B0000000000000000007B797B000000
      000000000000000000000000000000000000000000007B797B00000000000000
      FF000000FF000000FF000000FF000000FF00000000007B797B00000000000000
      0000000000000000000000000000000000007B797B0000000000000000007B79
      7B007B797B007B797B007B797B007B797B0000000000000000007B797B000000
      000000000000000000000000000000000000000000007B797B0000000000BDBE
      BD0000000000BDBEBD0000000000BDBEBD00000000007B797B00000000000000
      0000000000000000000000000000000000007B797B0000000000000000000000
      00000000000000000000000000000000000000000000000000007B797B000000
      000000000000000000000000000000000000000000007B797B00BDBEBD000000
      0000BDBEBD000000FF00BDBEBD0000000000BDBEBD007B797B00000000000000
      0000000000000000000000000000000000007B797B0000000000000000000000
      0000000000007B797B00000000000000000000000000000000007B797B000000
      0000000000000000000000000000000000007B797B007B797B00BDBEBD000000
      0000BDBEBD0000000000BDBEBD0000000000BDBEBD007B797B007B797B000000
      000000000000000000000000000000000000000000007B797B00000000000000
      000000000000000000000000000000000000000000007B797B00000000000000
      0000000000000000000000000000000000007B797B007B797B0000000000BDBE
      BD00000000000000FF0000000000BDBEBD00000000007B797B007B797B000000
      000000000000000000000000000000000000000000007B797B00000000000000
      0000000000007B797B000000000000000000000000007B797B00000000000000
      00000000000000000000000000000000000000000000000000007B797B00BDBE
      BD0000000000BDBEBD0000000000BDBEBD007B797B0000000000000000000000
      000000000000000000000000000000000000000000007B797B00000000000000
      000000000000000000000000000000000000000000007B797B00000000000000
      00000000000000000000000000000000000000000000000000007B797B000000
      0000BDBEBD0000000000BDBEBD00000000007B797B0000000000000000000000
      000000000000000000000000000000000000000000007B797B00000000000000
      000000000000000000000000000000000000000000007B797B00000000000000
      0000000000000000000000000000000000000000000000000000000000007B79
      7B007B797B007B797B007B797B007B797B000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B797B007B79
      7B000000000000000000000000007B797B007B797B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B79
      7B007B797B007B797B007B797B007B797B000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B797B007B79
      7B000000000000000000000000007B797B007B797B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B79
      7B000000000000000000000000007B797B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B797B007B797B007B797B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B79
      7B000000000000000000000000007B797B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B797B007B797B007B797B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084868400C6C7C600C6C7
      C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7
      C600C6C7C6000000000000000000000000000000000084868400C6C7C600C6C7
      C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7
      C600C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008486840000000000000000008486
      8400000000000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848684000000000000000000FFFF00008486
      8400848684000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008486
      8400000000000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF000000000000000000008486
      8400000000000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084868400FFFF0000FFFF0000000000008486
      8400848684000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008486840000000000000000008486
      8400000000000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084868400848684008486
      8400848684008486840084868400848684008486840084868400848684008486
      8400C6C7C6000000000000000000000000000000000084868400848684008486
      8400848684008486840084868400848684008486840084868400848684008486
      8400C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084868400848684008486
      8400848684008486840084868400848684008486840084868400848684008486
      8400848684000000000000000000000000000000000084868400848684008486
      8400848684008486840084868400848684008486840084868400848684008486
      8400848684000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000C6C7C6008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084000000C6C7
      C600840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000C6C7C600C6C7
      C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C6000000
      0000C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000C6C7C6008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C7C60000000000000000000000000084868400848684008486
      8400848684008486840084868400848684008486840084868400848684008486
      8400848684008486840000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084000000C6C7C600840000008400
      000084000000000000000000000000000000000000000000000084868400C6C7
      C600000000008486840000000000840000000000000000000000000000000000
      00000000840000000000000000000000000000000000C6C7C600C6C7C600C6C7
      C600C6C7C600C6C7C600C6C7C60000FFFF0000FFFF0000FFFF00C6C7C600C6C7
      C60000000000000000000000000000000000000000008486840000FFFF000000
      000000FFFF000000000000FFFF000000000000FFFF000000000000FFFF000000
      000000FFFF008486840000000000000000000000000000000000000000000000
      0000C6C7C600C6C7C600C6C7C600000000008486840084000000840000008400
      0000000000000000000000000000000000000000000084868400C6C7C600C6C7
      C600C6C7C6000000000084868400000000000000000000000000000000000000
      84000000840000000000000000000000000000000000C6C7C600C6C7C600C6C7
      C600C6C7C600C6C7C600C6C7C600848684008486840084868400C6C7C600C6C7
      C60000000000C6C7C600000000000000000000000000848684000000000000FF
      FF000000000000FFFF000000000000FFFF000000000000FFFF000000000000FF
      FF0000000000848684000000000000000000000000000000000084868400C6C7
      C600C6C7C600C6C7C600C6C7C600C6C7C6000000000084868400000000000000
      00000000000000000000000000000000000000000000C6C7C600C6C7C600C6C7
      C600C6C7C600C6C7C60000000000000000000000000000000000000084000000
      8400000084000000840000008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C7C600C6C7C60000000000000000008486840000FFFF000000
      000000FFFF000000000000FFFF000000000000FFFF000000000000FFFF000000
      000000FFFF008486840000000000000000000000000000000000C6C7C600C6C7
      C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C60000000000000000000000
      00000000000000000000000000000000000000000000C6C7C60000000000FFFF
      0000C6C7C600C6C7C600C6C7C600000000000000000000000000000000000000
      84000000840000000000000000000000840000000000C6C7C600C6C7C600C6C7
      C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C6000000
      0000C6C7C60000000000C6C7C6000000000000000000848684000000000000FF
      FF000000000000FFFF000000000000FFFF000000000000FFFF000000000000FF
      FF000000000084868400000000000000000000000000C6C7C600C6C7C600C6C7
      C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C6000000
      0000000000000000000000000000000000000000000084868400000000000000
      0000C6C7C600C6C7C60084868400000000000000000000000000000000000000
      0000000084000000000000000000000084000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C7
      C60000000000C6C7C6000000000000000000000000008486840000FFFF000000
      000000FFFF000000000000FFFF000000000000FFFF000000000000FFFF000000
      000000FFFF0084868400000000000000000000000000C6C7C600C6C7C600C6C7
      C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C6000000
      000000000000000000000000000000000000000000000000000084868400C6C7
      C600C6C7C6008486840000000000000000000000000000000000000000000000
      0000000000000000000000000000000084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C60000000000C6C7C6000000000000000000848684000000000000FF
      FF000000000000FFFF000000000000FFFF000000000000FFFF000000000000FF
      FF000000000084868400000000000000000000000000C6C7C60000000000FFFF
      0000C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008486840000FFFF000000
      000000FFFF000000000000FFFF000000000000FFFF000000000000FFFF000000
      000000FFFF0084868400000000000000000000000000C6C7C60000000000FFFF
      0000C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      0000000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000848684000000000000FF
      FF000000000000FFFF000000000000FFFF000000000000FFFF000000000000FF
      FF00000000008486840000000000000000000000000000000000000000000000
      0000FFFF0000FFFF0000C6C7C600C6C7C600C6C7C600C6C7C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      0000000084000000840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084868400848684008486
      8400848684008486840084868400848684008486840084868400848684008486
      8400848684008486840000000000000000000000000000000000848684000000
      00000000000000000000C6C7C600C6C7C600C6C7C60084868400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000084000000
      8400000084000000840000008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C600C6C7C600C6C7C600C6C7C6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008E8E8E008E8E
      8E00BEBEBE00BEBEBE00BEBEBE00BEBEBE00BEBEBE00BEBEBE00BEBEBE008686
      86008E8E8E000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008E8E8E00B8B8B800AEAE
      AE00ECECEC008383830083838300F0F0F000EEEEEE00E8E8E800DADADA007E7E
      7E009A9A9A008E8E8E0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008E8E8E00B4B4B400ACAC
      AC00F0F0F0008383830083838300EDEDED00F0F0F000ECECEC00DCDCDC007F7F
      7F00999999008E8E8E0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      000084000000000000000000000000000000000000008E8E8E00B4B4B400ABAB
      AB00F2F2F2008383830083838300EAEAEA00EFEFEF00EEEEEE00E0E0E0007C7C
      7C00989898008E8E8E0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084868400C6C7C600C6C7C6008486
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      000084000000000000000000000000000000000000008E8E8E00B4B4B400AAAA
      AA00F4F4F400F3F3F300ECECEC00EAEAEA00ECECEC00ECECEC00E1E1E1008585
      85009D9D9D008E8E8E0000000000000000000000000000000000000000000000
      000000000000000000000000000084868400C6C7C600C6C7C600FFFF00008486
      8400848684000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008E8E8E00B1B1B100AFAF
      AF00B2B2B200B8B8B800B6B6B600B1B1B100AFAFAF00B5B5B500B2B2B200ACAC
      AC00B3B3B3008E8E8E0000000000000000000000000000000000000000000000
      0000000000000000000000000000C6C7C600C6C7C600C6C7C600C6C7C6008486
      8400C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      000084000000000000000000000000000000000000008E8E8E00A2A2A200B6B6
      B600CBCBCB00D0D0D000D1D1D100D0D0D000CECECE00CDCDCD00D1D1D100D3D3
      D300B3B3B3008E8E8E0000000000000000000000000000000000000000000000
      0000000000000000000000000000C6C7C600FFFF0000C6C7C600C6C7C6008486
      8400C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      000084000000000000000000000000000000000000008E8E8E00B3B3B300FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00B3B3B3008E8E8E0000000000000000000000000000000000000000000000
      000000000000000000000000000084868400FFFF0000FFFF0000C6C7C6008486
      8400848684000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6C7C60000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      000084000000840000000000000000000000000000008E8E8E00B3B3B300FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00B3B3B3008E8E8E0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084868400C6C7C600C6C7C6008486
      840000000000000000000000000000000000000000000000000000000000C6C7
      C600000000000000000000000000C6C7C60000000000C6C7C600000000000000
      0000000000000000000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084000000840000008400000000000000000000008E8E8E00B3B3B300FFFF
      FF00DCDCDC00DCDCDC00DCDCDC00DCDCDC00DCDCDC00DCDCDC00DCDCDC00FFFF
      FF00B3B3B3008E8E8E0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C60000000000C6C7C60000000000C6C7C60000000000C6C7C600C6C7
      C600C6C7C6000000000084000000840000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      000000000000840000008400000084000000000000008E8E8E00B3B3B300FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00B3B3B3008E8E8E0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C7C60000000000C6C7C60000000000C6C7C600C6C7C600C6C7
      C600C6C7C600C6C7C60084000000840000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      000000000000840000008400000084000000000000008E8E8E00B3B3B300FFFF
      FF00DCDCDC00DCDCDC00DCDCDC00DCDCDC00DCDCDC00DCDCDC00DCDCDC00FFFF
      FF00B3B3B3008E8E8E0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C6C7C60000000000C6C7C600C6C7C600C6C7C600C6C7
      C600C6C7C600C6C7C60084000000840000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      000000000000840000008400000084000000000000008E8E8E00B3B3B300FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00B3B3B3008E8E8E0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C7C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C6C7C600C6C7C600C6C7C600C6C7C600C6C7
      C600C6C7C6000000000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      00008400000084000000840000000000000000000000000000008E8E8E00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF008E8E8E000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000081818100818181000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000878787008787
      8700828282007F7F7F007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007979790000000000000000000000000081818100818181000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008181810081818100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D1D1D100C9C9C900AAAA
      AA00969696009696960096969600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000095959500A0A0A0009E9E
      9E009B9B9B009898980096969600959595009696960096969600969696009696
      9600989898008D8D8D0079797900000000000000000081818100818181008181
      8100000000000000000000000000000000000000000000000000000000000000
      0000818181008181810000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C7C7C700E1E1E100D0D0
      D000D0D0D000C7C7C700ADADAD00969696009696960096969600000000000000
      000000000000000000000000000000000000000000009F9F9F00AAAAAA00A6A6
      A600A4A4A400A2A2A2009F9F9F009C9C9C009C9C9C009C9C9C009C9C9C009C9C
      9C009E9E9E00989898007E7E7E00000000000000000081818100818181008181
      8100818181000000000000000000000000000000000000000000000000008181
      8100818181000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A3A3A300D2D2D200CDCD
      CD00CECECE00CECECE00CECECE00CECECE00CECECE00D2D2D200D1D1D100D0D0
      D0009696960000000000000000000000000000000000A2A2A200ADADAD00A9A9
      A900A7A7A700B1B1B100C0C0C000C6C6C600C4C4C400B6B6B6009A9A9A009A9A
      9A009C9C9C00969696007E7E7E00000000000000000000000000888888008282
      8200818181008181810000000000000000000000000000000000818181008181
      8100000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000ABABAB00B4B4B400CECE
      CE00D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D2D2
      D200C7C7C70000000000000000000000000000000000A4A4A400B5B5B500B0B0
      B000CCCCCC00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E2E2E2009B9B9B009A9A
      9A009C9C9C00969696007E7E7E00000000000000000000000000000000000000
      0000818181008181810081818100000000008181810081818100818181000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C2C2C200A3A3A300E8E8
      E800D2D2D200D3D3D300D3D3D300D3D3D300D3D3D300D3D3D300D3D3D300D5D5
      D500C2C2C20096969600000000000000000000000000A4A4A400C1C1C100B7B7
      B700FFFFFF00E1E1E100B3B3B300AFAFAF00ABABAB00A3A3A3009E9E9E009B9B
      9B009C9C9C00969696007E7E7E00000000000000000000000000000000000000
      0000000000008181810085858500858585008686860080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C7C7C700B3B3B300E7E7
      E700D8D8D800D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D7D7
      D700C6C6C60096969600000000000000000000000000A4A4A400CECECE00BDBD
      BD00FFFFFF00B7B7B700ABABAB00A9A9A900A7A7A700E4E4E400A1A1A1009F9F
      9F009F9F9F00979797007E7E7E00000000000000000000000000000000000000
      0000000000000000000083838300848484008989890000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CFCFCF00C5C5C500A3A3
      A300E2E2E200EEEEEE00EFEFEF00E7E7E700DDDDDD00D9D9D900D9D9D900BBBB
      BB0094949400E0E0E000B4B4B4000000000000000000A4A4A400D7D7D700C1C1
      C100FFFFFF00D1D1D100AEAEAE00ADADAD00AAAAAA00FFFFFF00E1E1E100A2A2
      A200A1A1A1009A9A9A0081818100000000000000000000000000000000000000
      000000000000818181008888880086868600888888008B8B8B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D7D7D700C8C8C800ACAC
      AC009C9C9C009C9C9C009C9C9C00D2D2D200F6F6F600E1E1E100E0E0E0009494
      9400B4B4B400DEDEDE00E5E5E5000000000000000000A4A4A400DCDCDC00C5C5
      C500E7E7E700FFFFFF00F7F7F700F7F7F700FFFFFF00FFFFFF00FFFFFF00ECEC
      EC00A5A5A5009D9D9D0087878700000000000000000000000000000000000000
      00008B8B8B00898989008787870000000000000000008B8B8B008C8C8C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000DEDEDE00CECECE00D0D0
      D000D0D0D000CFCFCF00CECECE00C5C5C500A3A3A300C9C9C90094949400D1D1
      D100CFCFCF00B9B9B900949494009696960000000000A4A4A400DFDFDF00C9C9
      C900C0C0C000D7D7D700E9E9E900E8E8E800F5F5F500FFFFFF00FFFFFF00E9E9
      E900A8A8A800A1A1A1008C8C8C00000000000000000000000000000000008E8E
      8E00898989008D8D8D00000000000000000000000000000000008E8E8E008E8E
      8E00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E1E1E100D0D0D000D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200CDCDCD0094949400AEAEAE00C9C9
      C900CCCCCC00D0D0D000B0B0B0009292920000000000A4A4A400E5E5E500D2D2
      D200C4C4C400C0C0C000BBBBBB00B8B8B800B6B6B600FFFFFF00E1E1E100ADAD
      AD00ABABAB00A4A4A400909090000000000000000000000000008E8E8E008E8E
      8E008E8E8E000000000000000000000000000000000000000000000000008E8E
      8E008E8E8E000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000ECECEC00D4D4D400D9D9
      D900D9D9D900D8D8D800DEDEDE00E4E4E400E3E3E300DFDFDF00DEDEDE00B3B3
      B300C5C5C50094949400000000000000000000000000A4A4A400E6E6E600E3E3
      E300DADADA00D5D5D500CECECE00C7C7C700C0C0C000E1E1E100B1B1B100AFAF
      AF00AFAFAF00A8A8A8009595950000000000000000008E8E8E008E8E8E008E8E
      8E00000000000000000000000000000000000000000000000000000000000000
      0000000000008E8E8E0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000DBDBDB00DEDEDE00D9D9
      D900D9D9D900DCDCDC00D2D2D20096969600969696009696960096969600AFAF
      AF00C1C1C10094949400000000000000000000000000A4A4A400DBDBDB00E7E7
      E700E8E8E800E5E5E500DEDEDE00D5D5D500C9C9C900BCBCBC00B3B3B300B0B0
      B000B1B1B100ABABAB0098989800000000008E8E8E008E8E8E008E8E8E000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000969696009696
      960096969600969696000000000000000000000000000000000000000000B0B0
      B000ACACAC000000000000000000000000000000000000000000ADADAD00B8B8
      B800B9B9B900B9B9B900B6B6B600B3B3B300ADADAD00A8A8A800A6A6A600A5A5
      A500A4A4A4009F9F9F0000000000000000008E8E8E008E8E8E00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000094949400A8A8
      A800949494000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000949494009494940094949400949494000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000878787008787
      8700828282007F7F7F007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007979790000000000000000000000000000000000000000000000
      0000840000000000000000000000840000000000000000000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000084000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000095959500A0A0A0009E9E
      9E009B9B9B009898980096969600959595009696960096969600969696009696
      9600989898008D8D8D0079797900000000000000000000000000000000000000
      0000840000000000000000000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000000000
      0000000000000000000000000000840000000000000084868400008684008486
      8400008684008486840084000000000000000000000000000000000000000000
      000000000000000000000000000084000000000000009F9F9F00AAAAAA00A6A6
      A600A4A4A400A2A2A2009F9F9F009C9C9C009C9C9C009C9C9C009C9C9C009C9C
      9C009E9E9E00989898007E7E7E00000000000000000000000000000000000000
      0000840000000000000000000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000000000
      0000000000000000000000000000840000000000000000868400848684000086
      8400848684000086840084000000000000000000000000000000000000000000
      00000000000000000000000000008400000000000000A2A2A200ADADAD00A9A9
      A900A7A7A700A3A3A300BBBBBB00C5C5C500C4C4C400BCBCBC00A8A8A8009A9A
      9A009C9C9C00969696007E7E7E00000000000000000000000000000000000000
      0000000000008400000084000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000000000
      0000000000000000000000000000840000000000000084868400008684008486
      8400008684008486840084000000000000000000000000000000000000000000
      00008400000084000000840000008400000000000000A4A4A400B5B5B500B0B0
      B000ABABAB00A7A7A700E5E5E500FFFFFF00FFFFFF00FFFFFF00FFFFFF00C1C1
      C1009C9C9C00969696007E7E7E00000000000000000000000000000000000000
      0000000000000000000000000000840000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000000000
      0000000000000000000000000000840000000000000000868400848684000086
      8400848684000086840084000000000000000000000000000000000000000000
      00008400000000000000840000000000000000000000A4A4A400C1C1C100B7B7
      B700B1B1B100ABABAB00AAAAAA00ADADAD00ADADAD00ADADAD00DBDBDB00FFFF
      FF009C9C9C00969696007E7E7E00000000000000000000000000000000000000
      0000000000000000000000000000840000000000000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000000000
      0000000000000000000000000000840000000000000084868400008684008486
      8400008684008486840084000000000000000000000000000000000000000000
      00008400000084000000000000000000000000000000A4A4A400CECECE00BDBD
      BD00B4B4B400B0B0B000E7E7E700A9A9A900A7A7A700A4A4A400AAAAAA00FFFF
      FF009F9F9F00979797007E7E7E00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000000000
      0000840000008400000084000000840000000000000000868400848684000086
      8400848684000086840084000000840000008400000084000000840000008400
      00008400000000000000000000000000000000000000A4A4A400D7D7D700C1C1
      C100B6B6B600E6E6E600FFFFFF00ADADAD00AAAAAA00A7A7A700C9C9C900FFFF
      FF00A1A1A1009A9A9A0081818100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000000000
      0000840000000000000084000000000000000000000084868400008684008486
      8400008684008486840000868400848684000086840084868400008684008486
      84000086840000000000000000000000000000000000A4A4A400DCDCDC00C5C5
      C500F3F3F300FFFFFF00FFFFFF00FFFFFF00F7F7F700F6F6F600FFFFFF00DEDE
      DE00A5A5A5009D9D9D0087878700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000000000
      0000840000008400000000000000000000000000000000868400848684000000
      0000000000000000000000000000000000000000000000000000000000008486
      84008486840000000000000000000000000000000000A4A4A400DFDFDF00C9C9
      C900F1F1F100FFFFFF00FFFFFF00F6F6F600E7E7E700E6E6E600CDCDCD00A9A9
      A900A8A8A800A1A1A1008C8C8C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000084868400848684000000
      0000C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600000000008486
      84000086840000000000000000000000000000000000A4A4A400E5E5E500D2D2
      D200C4C4C400E9E9E900FFFFFF00B8B8B800B6B6B600B2B2B200AFAFAF00ADAD
      AD00ABABAB00A4A4A40090909000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000868400848684000086
      84000000000000FFFF00000000000000000000FFFF0000000000848684000086
      84008486840000000000000000000000000000000000A4A4A400E6E6E600E3E3
      E300DADADA00D5D5D500ECECEC00C7C7C700C0C0C000B6B6B600B1B1B100AFAF
      AF00AFAFAF00A8A8A80095959500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000A4A4A400DBDBDB00E7E7
      E700E8E8E800E5E5E500DEDEDE00D5D5D500C9C9C900BCBCBC00B3B3B300B0B0
      B000B1B1B100ABABAB0098989800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000ADADAD00B8B8
      B800B9B9B900B9B9B900B6B6B600B3B3B300ADADAD00A8A8A800A6A6A600A5A5
      A500A4A4A4009F9F9F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300100000100010000000000800900000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFFFFFFFF9FFFFFFF8001FFFF
      E7FFFFFF8001FFFFF9FFFCFF8001FFBFFE7FFCFF8001FF1FFF9FFF9F8001FE0F
      FFE7F99F8001FC079FF9F9F38001EE0FE7FFFF338001EF1FF9FFF33F8001EFBF
      FE7FF3E78001EFFFFF9FFE678001EFFFFFE7FE7F8001AFFFFFF9FFCF8001CFFF
      FFFFFFCF8001EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF801FFFFFFC3FFF3F
      800FE3FFFC3FFECF80078BFFFC3FFEF38003A3FFFC3FFDFC8001BFFFFC3FFE7D
      8001BFE38001FF9B8001B00B8001FFE78001B7E38001E7FF8001B63F8001D9FF
      800180BFFC3FDE7FC001BE3FFC3FBF9FE001A3FFFC3FCFBFF0018BFFFC3FF37F
      F801E3FFFC3FFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF801F801F801F
      FFFFBFCFBBCFBFCFC007BFD7BBD7BFD7DFF7BFDBBBDBBFD3DFF7BFDDBBDDBFD5
      DFF7BFDD801DBFD5DC77BFDDBBDDBFC5DC77BFDDBBDDBFD5DC77BFDDBBDDBFD1
      DFF7801D801D8015DFF7DDEDDFEDDFE5DFF7E005EFF5EFF5C007F779F7F9F7F9
      FFFFF801F801F801FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC1FFC1FFBF7EFFFF
      FE0FDE0FDF7D8FE3FFEFDFEFFFFF8003FFEFBFEFF0078FE3FFDFBFDFF777DFF7
      FFDFBFDFF777DFF79FDF9FDFF777DFF7E7BFE7BF9004DFF7F9BFF9BFF777DFF7
      FE6DFE6DF777DFF7FFC0FFC0F777DFF7FFEDFFEDF0078FE3FFEDFFEDFFFF8003
      FFC0FFC0DF7D8FE3FFEDFFEDBF7EFFFFFFFFFFFFC003FF7EF3CF80FFC0039E79
      EDAFFEFFC003C0038E2FFEFFC003EFF78717FEFFC003EFF78717FEFFC003EFF7
      C797FE07C003EFF7C797F7F7C0030FF1C38BB7F7C0038FF0C083D7F7C003EF07
      E34FE7F7C003EF6FE79F0077C003EF4FFFFFF3F7C007EF27FFFFF5F7C00FC073
      FFFFF6F1C01F9E79FFFFF7FFC03F7EFEFFFFF1FFFFFFF1FFF3CFE67F801FE67F
      E58FE79FBFCFE79F860FC7E7BFC7C7E78307C7F7BFC3C7F78307C7F7BFC1C7F7
      C387C7F7BFC1C7F7C387C7F7BFC1C7F7C183C1F7BFC1C1F7C083C077BFC1CE77
      E34FC0178001CF97E79FC007C001DFE7FFFFC007E001DFF7FFFFC00FF001C1EF
      FFFFFE0FF801FE0FFFFFFFFFFFFFFFFFF7FFFF3FF1FFFFFFB7FFF88FE67F801F
      D40FC7D3E79FBFCFE7F13FDCD7E7BFD7007DCFDDD7F7BFDBF3FBF3DDD7F7BFDD
      F5FBFCDBD7F7BFDDF6FBF707D7F7BFDDF7FBB7DFD1F7BFDDFFF7D7FFCE77BFDD
      CFF7E7FFCF97801DF3F7007FDFE7DFEDFCF7F3FFDFF7EFF5FF2FF5FFC1EFF7F9
      FFCFF6FFFE0FF801FFFFF7FFFFFFFFFFFFFFFFFFFFFFFEFFBFFFFF87DFFBBEFF
      DFFFFF9F8001DEFFE3FFFFAFDFFBEEFFE3FFFFB7DC7BF6FFE3FF80F7DBBBFBFF
      FFFFB6FBDBBBFE7FFEFFB6FBD9BB0420FF7F80F9DA7BFC3FFFFFB6FBDBFBFE7F
      FFC7B6FBDBBBFFDF77C780F7DC7BFEEFAFC7FFB7DFFBFEF7DFFBFFAF8001FEFB
      AFFDFF9FDFFBFEFD77FFFF87FFFFFEFFFFFFFFFFFFFFFFFFFFE3FFFFFEFFBFFF
      FFCBF07FBE7FDFFFFB93CFFFBE07EFFFF927DFFFDE0FF7FFFA4FDFFFDE1FFBFF
      F79FC0FFEE3FFC3FF7BFF71FEE7FFC3FEFDFEFEFF6FFFC3FEF8FDFF7F1FFFC3F
      DE7FDFFBF1FFFFDFD9FFDFF7F0FFDFEFA7FFDF8FFF3FDFF79FFFE07FFFCF07FB
      FFFFFFFFFFF3DFFDFFFFFFFFFFFFDFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFE3
      FEFFFFFFEFEFFFC3FEFFFFFFEFEFFB83FEFFFFFF8003F907FEFFFFFFEEEFF80F
      FEFFFFFFEEEFF01FFEFFFFFFEEEFF03FFEFF8001E00FE01FFEFFFFFFEEEFE00F
      FEFFFFFFEEEFC07FFEFFFFFFEEEFC1FFFEFFFFFF800387FFFEFFFFFFEFEF9FFF
      FEFFFFFFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFFFDFFFFF807FFF8FFF8FFFF
      FBF7FFF1FFF1FDBFF7FBFFE3FFE3FDBFEFFBFFC7FFC7FDBFEFFBE08FF18FFDBF
      DBFBC01FCE1FDDBBB2AB8A3FBFBFBDBD8AAB151FBFBF05A0FAAB2A9F7FDFBDBD
      FAAB151F7FDFDDBBFAA72A9F7FDFFDBFFAAF151FBFBFFDBFFC9F8A3FBFBFFDBF
      FE7FC07FCE7FFDBFFFFFE0FFF1FFFFFFFFFDFFFDFFFDFFFDFFF8FFF8FFF8FFF8
      FFF1FFF1FFF1FFF1FFE3FFE3FFE3FFE3FFC7FFC7FFC7FFC7E08FF18FE08FF18F
      C01FCE1FC01FCE1F8A3FBFBF953FBFBF151FBFBF2A9FBBBF2A9F7FDF111F7BDF
      001F60DF209F60DF2A9F7FDF111F7BDF151FBFBF2A9FBBBF8A3FBFBF953FBFBF
      C07FCE7FC07FCE7FE0FFF1FFE0FFF1FFFFFFFFFFFFFFFFFFE3FFE3FF83FF8001
      FC0FEC0FFBFFBFFDFFF1EFF1FBFFBFFDFFFDEFFDFBFFBFFDFFFBDFFBFBFFBFFD
      FFFBDFFBF807BFFDFFFBDFFBFFF7BFFDFFFBDFFBFFF7BFFDFFF7DFF7FFF7BFFD
      CFF7CFF7FFF7BFFDF3F7F3F7FFF7BFFDFCF7FCF7FFF7BFFDFF2FFF2FFFF7BFFD
      FFCFFFCFFFF18001FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF80078007FFFFFF6F9FF79FF7FFFFFEC79FF79DF7FE7F
      FFEF9FF798F7FC3FFF6F9FF79077F81FFE279FF79237F81FFF6F9FF79717FC3F
      FFFF9FF79F97FE7FFFFF9FF79FD7FFFFFFFF9FF79FF7FFFFFFFF80078007FFFF
      FFFF80078007FFFFFFFFFFFFFFFFFFFFFFF3FFFFFFFFFFFFFFE1FF3FC007FFFF
      FFC1FE3F8003C001FF83C07F00018001F00788F700019551C10F04E700018AA1
      809F02C100008441805F20E60000A009000F30F680009111000F81FEDFE0A289
      200FC3BFE8218541200FFFB7EFF78AA1B01FFFB3F41780039C1FFFC1F7FBFFFF
      C03FFFF3F803FFFFF0FFFFF7FFFFFFFFFFFFFFFFFFFFFFFFC007000C000FF9FF
      80037FE87FEFF9FF80037F01482FF3C780037E037FEF73C780037C03482F27FF
      80037C037FEF07C780037C037F6F00C780037C034E2F01E380037E07440403F1
      80037F0F6000063880037FEF00000E3880037F0FF8001E3880037F1FFC003F01
      C0077F3FFE047F83FFFF007FFFFFFFFFFFFFFFFCFFFFFFFFC0039FF9FFFF81FF
      80018FF3C007803F800187E7DFF780078001C3CFDFF780078001F11FDFF78003
      8001F83FDFF780038001FC7FDFF780018001F83FDFF780018001F19FDFF78000
      8001E3CFDFF780008001C7E7DF87800380018FFBDFAF800380011FFFDF9FC3E7
      C0033FFFC03FFFC7FFFFFFFFFFFFFE1FFFFFFFFFFFFFFFFFF9FFFFFFFC00C003
      F6CFFE0081FE8001F6B7FEFE01028001F6B7FE8201FE8001F8B780FE01108001
      FE8FBE8201F58001FE3FA0FE01F38001FF7FBE9000038001FE3FA0F500038001
      FEBFBEF300038001FC9FA40700038001FDDFBD7F00038001FDDFBCFF80078001
      FDDF81FFF87FC003FFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object dcMoveColCursor: TRbwDynamicCursor
    Cursor = 41
    HotPointX = 16
    HotPointY = 16
    OnDrawCursor = dcMoveColCursorDrawCursor
    Left = 8
    Top = 112
  end
  object dcMoveRowCursor: TRbwDynamicCursor
    Cursor = 42
    HotPointX = 16
    HotPointY = 16
    OnDrawCursor = dcMoveRowCursorDrawCursor
    Left = 40
    Top = 112
  end
  object dcAddColCursor: TRbwDynamicCursor
    Cursor = 43
    HotPointX = 16
    HotPointY = 16
    OnDrawCursor = dcAddColCursorDrawCursor
    Left = 8
    Top = 144
  end
  object dcAddRowCursor: TRbwDynamicCursor
    Cursor = 44
    HotPointX = 16
    HotPointY = 16
    OnDrawCursor = dcAddRowCursorDrawCursor
    Left = 40
    Top = 144
  end
  object dcSubdivide: TRbwDynamicCursor
    Cursor = 45
    HotPointX = 16
    HotPointY = 16
    OnDrawCursor = dcSubdivideDrawCursor
    Left = 8
    Top = 176
  end
  object fdFontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = 12
    Font.Name = 'helvetica'
    Font.Pitch = fpVariable
    Font.Style = []
    Left = 680
    Top = 8
  end
  object cdColorDialog: TColorDialog
    Left = 648
    Top = 8
  end
  object timTimer: TTimer
    Enabled = False
    Left = 576
    Top = 48
  end
  object dcSetSpacing: TRbwDynamicCursor
    Cursor = 46
    HotPointX = 16
    HotPointY = 16
    OnDrawCursor = dcSetSpacingDrawCursor
    Left = 40
    Top = 176
  end
  object sdSaveDialog: TSaveDialog
    OnClose = sdSaveDialogClose
    OnShow = sdSaveDialogShow
    DefaultExt = 'gpt'
    Filter = 
      'ModelMuse text file (*.gpt)|*.gpt|ModelMuse binary file (*.gpb)|' +
      '*.gpb|ModelMuse XML file (*.xml)|*.xml|ModelMuse ZLib file (*.mm' +
      'ZLib)|*.mmZLib'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save ModelMuse file'
    OnTypeChange = sdSaveDialogTypeChange
    Left = 416
    Top = 152
  end
  object odOpenDialog: TOpenDialog
    DefaultExt = 'gpt'
    Filter = 
      'ModelMuse text file (*.gpt)|*.gpt|ModelMuse binary file (*.gpb)|' +
      '*.gpb|ModelMuse XML file (*.xml)|*.xml|ModelMuse ZLib file (*.mm' +
      'ZLib)|*.mmZLib|ModelMuse files (*.gpt, *.gpb, *.xml, *.mmZLib)|*' +
      '.gpt; *.gpb; *.xml; *.mmZLib'
    FilterIndex = 5
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Open ModelMuse file'
    OnTypeChange = sdSaveDialogTypeChange
    Left = 616
    Top = 56
  end
  object sdPhastInput: TSaveDialog
    OnClose = sdPhastInputClose
    OnShow = sdPhastInputShow
    DefaultExt = '.trans.dat'
    Filter = 'PHAST input file (*.trans.dat)|*.trans.dat|All File (*.*)|*.*'
    FilterIndex = 0
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save PHAST transport-data file'
    Left = 176
    Top = 110
  end
  object MostRecentlyUsed: TvRbwMostRecentlyUsed
    Capacity = 5
    MenuItemPosition = mipSibling
    OnClick = OpenMostRecentlyUsed
    PreviousItem = N5
    ShowHint = True
    Left = 464
    Top = 14
  end
  object sdModflowInput: TSaveDialog
    OnClose = sdModflowInputClose
    OnShow = sdModflowInputShow
    DefaultExt = '.nam'
    Filter = 'MODFLOW Name Files (*.nam)|*.nam|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save MODFLOW input files'
    Left = 176
    Top = 160
  end
  object ilImageList: TImageList
    Left = 616
    Top = 8
    Bitmap = {
      494C010163006500040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000009001000001002000000000000090
      0100000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF00000000000000FF0000
      00FF000000FF000000FF000000FF00000000000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF0000FF00000000FF0000
      00FF000000FF000000FF000000FF0000FF00000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000000000
      00FF000000FF000000FF00000000000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF0000FF000000
      00FF000000FF000000FF0000FF00000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      0000000000FF00000000000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      FF00000000FF0000FF00000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF00000000000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF0000FF00000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      0000000000FF00000000000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      FF00000000FF0000FF00000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000000000
      00FF000000FF000000FF00000000000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF0000FF000000
      00FF000000FF000000FF0000FF00000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF00000000000000000000000000000000FF00000000000000FF0000
      00FF000000FF000000FF000000FF00000000000000FF00000000000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF0000FF00000000FF0000
      00FF000000FF000000FF000000FF0000FF00000000FF00000000000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF00000000000000000000000000FF0000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF00000000000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF00000000000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF00000000000000000000000000FF0000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF00000000000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF00000000000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF000000FF000000FF0000000000FF0000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF00000000000000000000
      0000000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF00000000000000000000
      0000000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF00000000000000000000000000FF0000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF00000000000000FF0000
      00FF00000000000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF00000000000000FF0000
      00FF00000000000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF00000000000000000000000000FF0000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF00000000000000FF0000
      00FF00000000000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF00000000000000FF0000
      00FF00000000000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF00000000000000FF0000
      00FF00000000000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF00000000000000FF0000
      00FF00000000000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF00000000000000000000
      0000000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF00000000000000000000
      0000000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080008000
      8000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      8000800080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000800080008000
      8000800080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008000800000000000800080008000
      8000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000008080800080008000800080008000
      8000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008000800080008000800080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800080008000800000000000808080008000800080008000800080008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008000800080008000800080008000
      8000800080000000000000000000000000000000000000000000000000000000
      0000800080008000800080008000800080008000800080008000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008000800080008000800080000000
      0000000000000000000000000000000000000000000000000000000000008000
      8000800080008000800080008000800080008000800080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008000800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      8000800080008000800080008000800080008000800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800080008000
      8000800080008000800080008000800080008000800080008000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800080008000
      8000800080008000800080008000800080008000800080008000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080008000800080008000
      8000800080008000800080008000800080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080008000800080008000
      8000800080008000800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008000800080008000800080008000
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008000800080008000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080008000800080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080008000
      8000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000800080008000800080008000000000000000000000000000000000000000
      0000000000000000000000000000000000008000800080008000800080008000
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000800080008000
      8000800080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000000000000000000000000000808080008000
      8000800080008000800080808000000000000000000000000000000000000000
      0000000000000000000000000000000000008000800080008000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000008080800080008000800080008000
      8000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080008000800080000000000080808000800080008000
      8000800080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008000800000000000800080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800080008000800000000000808080008000800080008000800080008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080008000800080008000800080008000800080008000
      8000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008000800000000000000000008000
      8000000000000000000000000000000000000000000000000000000000000000
      0000800080008000800080008000800080008000800080008000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000800080008000800080008000800080008000800080008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      8000000000000000000000000000000000000000000000000000000000008000
      8000800080008000800080008000800080008000800080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000800080008000800080008000800080008000800080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      8000000000000000000000000000000000000000000000000000000000008000
      8000800080008000800080008000800080008000800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800080008000800080008000800080008000800080008000800080008000
      8000000000000000000000000000000000008000800080008000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080008000800080000000000000000000800080008000
      8000800080008000800080008000800080008000800080008000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800080008000800080008000800080008000800080008000800080008000
      8000808080000000000000000000000000008000800080008000800080008000
      8000800080008000800080008000800080008000800080008000800080008000
      8000800080008000800080008000800080000000000000000000800080008000
      8000800080008000800080008000800080008000800080008000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      8000800080008000800080008000800080008000800080008000000000000000
      0000000000000000000000000000000000008000800080008000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080008000800080000000000080008000800080008000
      8000800080008000800080008000800080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      8000800080008000800080008000800080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      8000000000000000000000000000000000000000000080008000800080008000
      8000800080008000800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800080008000
      8000800080008000800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      8000000000000000000000000000000000008000800080008000800080008000
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800080008000
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008000800000000000000000008000
      8000000000000000000000000000000000008000800080008000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008000800080008000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000800080008000000000000000000000000000000000000000
      0000000000000000000000000000000000008000800000000000800080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008000800080008000800080008000
      8000800080008000800080008000800080008000800080008000800080008000
      8000800080008000800080008000000000000000000000000000000000000000
      0000000000000000000000000000000000008000800080008000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008000800080008000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000800080008000000000000000000000000000000000000000
      0000000000000000000000000000000000008000800080008000800080008000
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFF0000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF000000FFFFFF00FFFFFF00FFFFFF0000FF000000FF
      000000FF000000FF0000FFFFFF00FFFFFF00FF000000FF000000FF000000FF00
      0000FF000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      0000FF000000FF000000FF000000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFF0000FFFF0000FFFF
      FF00FFFFFF00FF000000FFFFFF00FFFFFF00FFFFFF0000FF0000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000FF0000FFFFFF00FF000000FF000000FF000000FF00
      0000FF000000FF000000C0808000FFFFFF00FFFFFF00F0CAA600FF000000FF00
      0000FF000000FF000000FF000000FF0000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF0000FFFF0000FFFF0000FF
      FF0000FF000000FF000000FF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFF0000FFFF0000FFFF
      0000FFFFFF00FF000000FFFFFF00FFFFFF0000FF0000FFFFFF00FFFFFF000000
      FF000000FF00FFFFFF00FFFFFF0000FF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FFFFFF00FFFFFF00FF000000FF000000FF00
      0000FF000000FF000000FF000000FF0000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF0000FFFF0000FFFF0000FF
      FF0000FF000000FF000000FF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FF000000FFFFFF00FFFFFF0000FF0000FFFFFF000000FF00FFFF
      FF00FFFFFF000000FF00FFFFFF0000FF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FFFFFF00FFFFFF00FF000000FF000000FF00
      0000FF000000FF000000FF000000FF0000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF0000FFFF0000FFFF0000FF
      FF0000FF000000FF000000FF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFFFF00FFFFFF0000FF0000FFFFFF000000FF00FFFF
      FF00FFFFFF000000FF00FFFFFF0000FF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FFFFFF00FFFFFF00FF000000FF000000FF00
      0000FF000000FF000000FF000000FF0000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF0000FFFF000000FF000000FF000000
      FF0000FFFF0000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF000099CC00FFFFFF0000FF0000FFFFFF00FFFFFF000000
      FF000000FF00FFFFFF00FFFFFF0000FF0000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FFFFFF00FFFFFF00FF000000FF000000FF00
      0000FF000000FF000000FF000000FF0000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF0000FFFF000000FF000000FF000000
      FF0000FFFF0000FFFF0000FFFF00000000000000000000FFFF0000FFFF000000
      000000FFFF0000FFFF000000000000FFFF0000FFFF000000000000FFFF0000FF
      FF000000000000FFFF0000FFFF00000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF000099CC000099CC00FFFFFF0000FF0000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000FF0000FFFFFF00FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FFFFFF00FFFFFF00FF000000FF000000FF00
      0000FF000000FF000000FF000000FF0000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF0000FFFF000000FF000000FF000000
      FF0000FFFF0000FFFF0000FFFF00000000000000000000FFFF0000FFFF000000
      000000FFFF0000FFFF000000000000FFFF0000FFFF000000000000FFFF0000FF
      FF000000000000FFFF0000FFFF00000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF000099CC000099CC000099CC00FFFFFF0000FF000000FF
      000000FF000000FF0000FFFFFF00FFFFFF00FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FFFFFF00FFFFFF00FF000000FF000000FF00
      0000FF000000FF000000FF000000FF0000000000000000000000000000000000
      0000000000000000000000FF000000FF000000FF000000FF000000FF000000FF
      00000000FF000000FF000000FF00000000000000000000FFFF0000FFFF000000
      000000FFFF0000FFFF000000000000FFFF0000FFFF000000000000FFFF0000FF
      FF000000000000FFFF0000FFFF00000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF000099CC000099CC000099CC000099CC00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF000000FF000000FF000000FF00
      0000FF000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF000000FF000000FF00
      0000FF000000FF000000FF000000FF0000000000000000000000000000000000
      0000000000000000000000FF000000FF000000FF000000FF000000FF000000FF
      00000000FF000000FF000000FF00000000000000000000FFFF0000FFFF000000
      000000FFFF0000FFFF000000000000FFFF0000FFFF000000000000FFFF0000FF
      FF000000000000FFFF0000FFFF00000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF000099CC000099CC000099CC000099CC000099CC00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF000000FF000000FF000000FF000000FF00
      0000FF000000FF000000C0808000F0CAA600FFFFFF00FF000000FF000000FF00
      0000FF000000FF000000FF000000FF0000000000000000000000000000000000
      0000000000000000000000FF000000FF000000FF000000FF000000FF000000FF
      00000000FF000000FF000000FF00000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000000000000000000000FF000000FF000000
      FF000000FF000000FF00FF000000FF000000FF000000FF000000FF00000000FF
      0000FF000000FF000000FF000000FFFFFF00FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000000000000000000000FF000000FF000000
      FF000000FF000000FF00FF000000FF000000FF000000FF000000FF00000000FF
      000000FF0000FFFFFF00FFFFFF00FFFFFF00FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF00FF000000FF000000FF000000FF000000FF00000000FF
      000000FF000000FF0000FFFFFF00FFFFFF00FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FFFFFF00FFFFFF00FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF00FF000000FF000000FF000000FF000000FF00000000FF
      000000FF000000FF000000FF0000FFFFFF00FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FFFFFF00FFFFFF00FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF00FF000000FF000000FF000000FF000000FF00000000FF
      000000FF000000FF000000FF000000FF000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000006C676800979696001C1C
      1C0000000000000000000000000000000000C8C6C600A3A1A1009E9C9C00C7C5
      C5000606060000000000000000000000000000000000F0E8F000E8E0E600DFD7
      DA00D6CED000CEC6C600C8C0BE00C1B9B600B8B0AA00ABA19800A79D9300AEA5
      9D00B0A8A000B0A8A0007A61580000000000000000008E8A8A009D999C00C9C2
      C600DAD2D400D2CACA00CAC2C200C4BCB900B2A9A300968A7E0091857900A398
      8F00AFA69E00B0A8A00092827A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000070606006F6A6B00626262002C2C
      2C0058545500A9A6A700A29F9F00ABA9A9002F2F2F0003030300060606000808
      08009C9A9A00777374000F0F0F000000000000000000A2A2A200505050009693
      9500DFD7DA00D6CED000CEC6C600BDB3AE0093857800D4D2D000B6B0AA00877A
      6D00ACA39A00B0A8A000705850000000000000000000858585005C5C5C006160
      6100E3DBE000DAD2D400D2CACA00BAB0AB00ABA19800D2D0CE00D0CDCB009E95
      8C009F948900AFA79F0090807800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000F0E0E006F6A6B00606060002D2D
      2D005353530058575700ACABAB008180800014140E0049523A005A5E59001614
      14000E0E0E00BEBCBC00B8B3B4000404040000000000A2A2A200505050005050
      5000E8E0E600DFD7DA00D6CED0009F918300CFCECD00423A3200817B7400DAD9
      D80093877A00B0A8A000705850000000000000000000858585005C5C5C005050
      5000ECE4EB00E3DBE000DAD2D400AB9F9300BFBCBB007F695100826F5700C5C3
      C20093887D00ACA39B0090807800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000141313006E696A00606060002B2B
      2B0054545400D8D6D600CECCCC000A0A0A002F3324001B284300242A3A002E34
      3F0006050500A5A4A400ACA8A9003C3A3A0000000000A2A2A200505050005050
      5000F0E8F000E8E0E600DBD3D500DDDCDC00443C3300E5AC6B00DFB07300DDDC
      DC007B6E5F00B0A8A000705850000000000000000000858585005C5C5C005050
      5000F0E8F000ECE4EB00E1D9DD00B5ACA2008E8A8500CD935A00E8B574009791
      8C00A79E9500ADA49C0090807800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000151414006E696A00626262002D2D
      2D0053535300DBD9D900BCBBBB00484848001E1D1D002A3248001F5378003147
      610006060600AAA9A900908B8C005652530000000000A2A2A200505050005050
      5000F0E8F000F0E8F000E8E0E600CBC7C200736C6500B47D4C0063503F00DDDC
      DC008D7E7100B5ADA600705850000000000000000000858585005C5C5C005050
      5000F0E8F000F0E8F000ECE4EB00B0A59B00C0BEBC00735B460077624D00C5C3
      C2009C908500B6ADA70091817A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000151414006F6A6B00606060002C2C
      2C0053535300DAD9D9007E797A00ABA9A900060606001D1C1D001F2630000A09
      0A00D3D0D100B8B6B600999496005856560000000000A2A2A200505050005050
      5000F0E8F000F0E8F000F0E8F000A2938500DFDFDF00DBDADA00DEDEDE00A397
      8B00BDB4B000BBB3AE00705850000000000000000000858585005C5C5C005050
      5000F0E8F000F0E8F000F0E8F000D7CECF00B4AAA000D5D2D000D3D0CD00ACA1
      9700B6ACA600BDB5B20094847E00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000151414006F6A6B00616161002C2C
      2C0054545400DAD8D8009794950087848400A4A3A3008D8C8C00B9B7B7009D9C
      9C00BDBBBB003D393A00B7B2B4005E5C5C0000000000A2A2A200505050005050
      5000F0E8F000F0E8F000F0E8F000F0E8F000BCAFA9008D7B67009A8A7A00CBC3
      C200C8C0BE00C1B9B600705850000000000000000000858585005C5C5C005050
      5000F0E8F000F4EEF300F4EEF300F0E8F000E0D7DB00BCAFA900B7AAA200CAC1
      BF00D0C8C700CDC5C30097878200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000141313006B666700686767002D2D
      2D00DAD7D700D9D7D700DBD9D900918C8D008D878800D4D1D100CAC6C7009E98
      9900534F5000BBB7B800B7B2B4005551520000000000A2A2A200505050005050
      5000F0E8F000B0B0B000F8F4F700F0E8F000F0E8F000E8E0E600E4B57700F0CE
      9900C5824100D7CFCF00705850000000000000000000858585005C5C5C006261
      6200F0E8F00081818100D4D2D300F0E8F000F0E8F000ECE4EB00DDBDA000EECA
      9300DCAA6D00C8A387009B8B8600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000A0A0A0064606000929191009592
      930085848400A9A7A7007C7B7B00A9A7A7004D4A4B0018161600262324003B38
      39008E8D8D001A1818001B1919000B0A0A00000000005050500050505000D7D0
      D700F0E8F00057575700F8F4F700F0E8F000F0E8F000F0E8F000B0704000E4B5
      7700F0CE9900D7CFCF007058500000000000000000007C797600A09CA000D4CD
      D400F0E8F000A19DA100D6D1D500F0E8F000F0E8F000F0E8F000DEC8C200DDBD
      A000E7D2B900D7C5B50094857F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000202020032313100F0F0F0003E3E
      3E004A464700DBD9D9008F8E8E00DBD9D900636161006F6F6F00717070006967
      67005C5A5A00C0BBBC00827E7F003D3A3B00000000009088800090888000B0A8
      A000686868009088800090888000908880009088800090888000908880009088
      8000908880009088800000000000000000000000000000000000000000008787
      850072706E000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A4748009D9C9C008C8A
      8A0073727200929191008E8D8D0086828300AFACAD00D7D6D600DFDEDE00C2C0
      C1004B4849009F9FA00006060600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BEBCBC004343
      430045444400BEBABA009391910005050500DAD7D700D7D4D400C1BDBE00A6A1
      A2002A2828000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000000000000000
      000000000000000000000000000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000000000000000
      000000000000000000000000000000FF00000000000000000000000000000000
      0000000000000000000000FF0000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00000000000000000000000000
      00000000000000FF000000FF0000000000000000000000000000000000000000
      000000000000000000000000000000FF00000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00000000000000000000000000
      00000000000000FF000000000000000000000000000000000000000000000000
      000000000000000000000000000000FF00000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00000000000000000000000000
      000000FF00000000000000000000000000000000000000000000000000000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00000000000000000000000000
      000000FF000000000000000000000000000000000000000000000000FF000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00000000000000000000000000
      000000FF0000000000000000000000000000000000000000FF00000000000000
      000000000000000000000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000000000000000
      000000FF0000000000000000000000000000000000000000FF00000000000000
      000000000000000000000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000000000000000
      00000000000000FF0000000000000000000000000000000000000000FF000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000000000000000
      00000000000000FF000000000000000000000000000000000000000000000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF0000000000
      0000000000000000000000FF000000FF00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF0000000000
      00000000000000000000000000000000000000FF000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF0000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFF0000FFFF0000FFFF000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF000000FF00008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF0000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF000000FF000000FF000080808000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF0000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF000000FF000000FF000000FF0000808080000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF0000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000080808000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF000000FF000000FF000000FF000000FF00008080
      8000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFF0000FFFF0000FFFF000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      00000000000080808000FFFF0000FFFF00008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000080808000FFFF0000FFFF00008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      0000808080000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000080808000FFFF0000FFFF0000FFFF0000FFFF000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000080808000FFFF0000FFFF0000FFFF0000FFFF000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF00000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      000080808000FFFF0000FFFF0000FFFF0000FFFF000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000080808000FFFF0000FFFF0000FFFF0000FFFF000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000080808000FFFF0000FFFF00008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000080808000FFFF0000FFFF00008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF000000FF000000FF000000FF000000FF00000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF000000FF000000FF000000FF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF00000000000000000000000000000000000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF000000FF000000FF000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF0000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF000000FF00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      00000000000000FF000000FF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF0000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF00000000000000000000000000000000000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF00000000FFFF0000FFFF0000FFFF0000FFFF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF00000000FFFF0000FFFF0000FFFF0000FFFF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF00000000FFFF0000FFFF0000FFFF0000FFFF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF00000000FFFF0000FFFF0000FFFF0000FFFF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0040C0E00040C0E00040C0E00040C0E000FFFF0000FFFF
      0000FFFF0000FFFF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0040C0E00040C0E00040C0E00040C0E000FFFF0000FFFF
      0000FFFF0000FFFF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0040C0E00040C0E00040C0E00040C0E000FFFF0000FFFF
      0000FFFF0000FFFF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0040C0E00040C0E00040C0E00040C0E000FFFF0000FFFF
      0000FFFF0000FFFF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF0000FF000000FF000000FF000000FF0000FF000000FF00
      0000FF000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF0000FF000000FF000000FF000000FF0000FF000000FF00
      0000FF000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF0000FF000000FF000000FF000000FF0000FF000000FF00
      0000FF000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF0000FF000000FF000000FF000000FF0000FF000000FF00
      0000FF000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF0000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF0000000000FF000000FF000000FF000000FF00000000000000FF00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF0000000000FF000000FF000000FF000000FF00000000000000FF00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF0000000000FF000000FF000000FF000000FF00000000000000FF00
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000800000008000000080
      00000080000000800000FF000000FF000000FF00000000800000008000000080
      0000008000000080000000800000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000000000000000FF000000FF000000FF000000FF00000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000800000008000000080
      0000008000000080000000800000FF0000000080000000800000008000000080
      0000008000000080000000800000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000000000000000FF000000FF000000FF000000FF00000000000000
      FF00000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000800000008000000080
      000000800000008000000080000000800000FF00000000800000008000000080
      0000008000000080000000800000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000000000000000FF000000FF000000FF000000FF00000000000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000800000008000000080
      0000008000000080000000800000FF000000FF000000FF000000008000000080
      0000008000000080000000800000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF0000000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF000000000000000000FF000000FF000000FF000000FF000000
      000000000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF0000000000FF000000FF000000FF00
      0000FF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000000000000000
      000000000000000000000000000000000000FF00000000000000000000000000
      0000000000000000000000000000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF0000000000
      000000000000000000000000000000000000FF00000000000000000000000000
      00000000000000000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF0000000000
      000000000000000000000000000000000000FF00000000000000000000000000
      00000000000000000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000000000000000
      000000000000000000000000000000000000FF00000000000000000000000000
      0000000000000000000000000000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B48D8A00B18A
      8700B1888500AF868300AE858200AD838000AB817F00AA807C00A97F7B00A87C
      7900A77B7800A77A7700A6797600000000000000000000000000000000000000
      0000000000000000000000000000000000007B7D7B0000000000000000000000
      00000000000000000000000000007B7D7B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B58F8C00FEFE
      FD00FEFEFD00FEFEFD00FEFEFD00FEFEFD00FEFEFD00FEFEFD00FEFEFD00FEFE
      FD00FEFEFD00FEFEFD00A77B780000000000000000007B7D7B007B7D7B000000
      00000000000000000000000000007B7D7B007B7D7B0000000000000000000000
      0000000000007B7D7B007B7D7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B7928E00FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFC
      FB00FEFCFB00FEFCFB00A97D79000000000000000000000000007B7D7B007B7D
      7B007B7D7B007B7D7B007B7D7B007B7D7B007B7D7B007B7D7B007B7D7B007B7D
      7B007B7D7B007B7D7B0000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000084828400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B9949200FEFA
      F600FEFAF600FEFAF600FEFAF600FEFAF600FEFAF600FEFAF600FEFAF600FEFA
      F600FEFAF600FEFAF600AA7F7C00000000000000000000000000000000007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      00007B7D7B000000000000000000000000000000000000000000848284008482
      8400848284000000000000000000000000008482840084828400848284000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BB979300FEF9
      F400FEF9F400FEF9F400FEF9F400FEF9F400FEF9F400FEF9F400FEF9F400FEF9
      F400FEF9F400FEF9F400AA807D00000000000000000000000000000000007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      00007B7D7B000000000000000000000000000000000000000000848284008482
      8400848284000000000000000000000000008482840084828400848284000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BD999600FEF6
      EF00FEF6EF00FEF6EF00FEF6EF00FEF6EF00FEF6EF00FEF6EF00FEF6EF00FEF6
      EF00FEF6EF00FEF6EF00AD838000000000000000000000000000000000007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      00007B7D7B000000000000000000000000000000000000000000000000008482
      8400848284000000000000000000000000000000000084828400848284000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BD9B9800FEF5
      ED00FEF5ED00FEF5ED00FEF5ED00FEF5ED00FEF5ED00FEF5ED00FEF5ED00FEF5
      ED00FEF5ED00FDF4EC00AD848100000000000000000000000000000000007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      00007B7D7B000000000000000000000000000000000000000000000000008482
      8400848284000000000000000000000000000000000084828400848284000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C09E9B00FEF3
      E800FEF3E800FEF3E800FEF3E800FEF3E800FEF3E800FEF3E800FEF3E800FEF3
      E800FDF2E700FDF1E600AF878400000000007B7D7B007B7D7B007B7D7B007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      00007B7D7B007B7D7B007B7D7B00000000000000000000000000000000008482
      8400848284008482840000000000000000000000000084828400848284008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C1A09C00FFF2
      E600FFF2E600FFF2E600FFF2E600FFF2E600FFF2E600FFF2E600FFF2E600FEF1
      E500FEF1E500F9ECDE00B188860000000000000000007B7D7B007B7D7B007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      00007B7D7B007B7D7B007B7D7B007B7D7B000000000000000000000000008482
      8400848284008482840000000000000000000000000084828400848284008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4A39F00FFEF
      E100FFEFE100FFEFE100FFEFE100FFEFE100FFEFE100FFEFE100FEEEE000FDED
      DF00F8E8D800E5D6C100B38C8900000000000000000000000000000000007B7D
      7B00000000000000000000000000000000007B7D7B007B7D7B007B7D7B007B7D
      7B007B7D7B000000000000000000000000000000000000000000000000000000
      0000848284000000000000000000000000000000000000000000848284000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4A4A100FFEE
      DF00FFEEDF00FFEEDF00FFEEDF00FFEEDF00FFEEDF00FEEDDE00FEEDDE00F7E6
      D500EEDECB00D9C9B100B58E8B00000000000000000000000000000000007B7D
      7B00000000000000000000000000000000007B7D7B0000000000000000007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6A7A300FFEC
      DA00FFECDA00FFECDA00FFECDA00FFEBDA00FEEBD900FCE9D700F6E3CF00E2D0
      B800D7C6AB00E5D4C100BB989400000000000000000000000000000000007B7D
      7B00000000000000000000000000000000007B7D7B00000000007B7D7B007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C7A8A400FFEA
      D800FFEAD800FFEAD800FFEAD800FEE9D700FEE9D700F5E1CC00F7EEE400FEFE
      FD00FEF8F200C5A59F0000000000000000000000000000000000000000007B7D
      7B00000000000000000000000000000000007B7D7B007B7D7B00000000007B7D
      7B007B7D7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C8AAA700FFE8
      D300FFE8D300FFE7D200FEE7D200FBE4CE00F3DDC600DFCAAF00FAF6F100DFC6
      BE00D2BAB70000000000000000000000000000000000000000007B7D7B007B7D
      7B007B7D7B007B7D7B007B7D7B007B7D7B007B7D7B0000000000000000000000
      00007B7D7B007B7D7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C9ABA800FFE7
      D100FFE7D100FEE6D000FEE6CF00F3DBC300E9D3B800E5D6C200FAF0E700D9C5
      C40000000000000000000000000000000000000000007B7D7B007B7D7B000000
      00000000000000000000000000007B7D7B007B7D7B0000000000000000000000
      0000000000007B7D7B007B7D7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CAACA900C9AB
      A800C9AAA700C8A8A500C7A8A500C5A6A300C5A5A200CFB4B200E7DAD9000000
      0000000000000000000000000000000000007B7D7B0000000000000000000000
      00000000000000000000000000007B7D7B000000000000000000000000000000
      00000000000000000000000000007B7D7B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C3C6000000000000000000000000000000000000000000C6C3C6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400C6C3C6000000000000000000000000000000000084828400C6C3C6000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848284008482
      840084828400C6C3C6000000000000000000848284008482840084828400C6C3
      C600000000000000000000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400848284000000000000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848284008482
      840084828400C6C3C6000000000000000000848284008482840084828400C6C3
      C600000000000000000000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400848284008482840000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      840084828400C6C3C6000000000000000000000000008482840084828400C6C3
      C600000000000000000000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400848284008482840000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      840084828400C6C3C6000000000000000000000000008482840084828400C6C3
      C600000000000000000000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400848284008482840000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      84008482840084828400C6C3C600000000000000000084828400848284008482
      8400C6C3C6000000000000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400848284008482840000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400848284008482840000000000000000000000000084828400848284008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000C6C3C600C6C3C600C6C3C600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400848284008482840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848284000000000000000000000000000000000000000000848284000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C3C600C6C3C600C6C3C600C6C3C600C6C3C60000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400848284008482840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C3
      C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C6000000
      000000000000000000000000000000000000000000000000000000000000C6C3
      C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C6000000
      0000848284008482840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C3
      C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3
      C600000000000000000000000000000000000000000000000000000000000000
      0000C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3
      C600000000008482840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C6C3C600C6C3C600C6C3C600C6C3C6000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3C600C6C3
      C600C6C3C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7D
      7B007B7D7B007B7D7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7D7B007B7D
      7B00000000007B7D7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7D7B000000000000000000000000007B7D7B007B7D7B000000
      00007B7D7B007B7D7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7D7B007B7D7B00000000007B7D7B007B7D7B00000000007B7D
      7B007B7D7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7D7B00000000007B7D7B007B7D7B00000000007B7D7B007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B7D7B00000000000000000000000000000000007B7D7B007B7D7B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B7D7B00000000000000000000000000000000007B7D7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7D
      7B000000000000000000000000000000000000000000000000007B7D7B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7D
      7B0000000000000000000000000000000000000000007B7D7B007B7D7B007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7D7B000000
      00000000000000000000000000007B7D7B007B7D7B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7D7B000000
      0000000000007B7D7B007B7D7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7D7B00000000007B7D
      7B007B7D7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7D7B007B7D7B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7D
      7B00000000007B7D7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7D7B000000
      00000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7D7B000000000000000000000000007B7D7B00000000000000
      FF00000000007B7D7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7D7B00000000000000FF000000
      00007B7D7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF0000000000000000000000FF00000000007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF00000000007B7D7B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF000000FF000000FF0000000000000000007B7D
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084868400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BD000000BD000000BD000000BD000000BD000000BD000000BD00
      0000BD0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008486840084868400848684000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848684008486840084868400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008486
      8400848684008486840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848684008486
      8400848684000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000808080000000000000FFFF00808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848684008486840084868400000000000000000084868400848684008486
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      800080808000808080008080800080808000000000000000000000FFFF000000
      0000000000000000000000000000000000000000000000000000848684008486
      8400000000000000000000000000848684008486840084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000080808000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF00008080800000000000000000000000
      0000000000000000000000000000000000000000000084868400000000000000
      0000000000000000000000000000000000000000000084868400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800080808000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000080808000808080000000
      0000000000000000000000000000000000000000000084868400000000000000
      0000000000000000000000000000000000000000000084868400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000080808000000000000000
      0000000000000000000000000000000000008486840000000000000000000000
      0000000000000000000000000000000000000000000000000000848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000080808000000000000000
      0000000000000000000000000000000000008486840000000000000000000000
      0000000000000000000000000000000000000000000000000000848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000080808000000000000000
      0000000000000000000000000000000000008486840000000000000000000000
      0000000000000000000000000000000000000000000000000000848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800080808000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000080808000808080000000
      0000000000000000000000000000000000000000000084868400000000000000
      0000000000000000000000000000000000000000000084868400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000080808000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF00008080800000000000000000000000
      0000000000000000000000000000000000000000000084868400000000000000
      0000000000000000000000000000000000000000000084868400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000808080008080800080808000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848684008486
      8400000000000000000000000000848684008486840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848684008486840084868400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B797B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B797B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B797B007B797B007B797B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B797B007B797B007B797B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B797B007B797B007B797B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B797B007B797B007B797B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B79
      7B007B797B007B797B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B79
      7B007B797B007B797B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B797B007B79
      7B007B797B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B797B007B79
      7B007B797B000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000808080000000000000FFFF00808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B797B007B797B007B797B0000000000000000007B797B007B797B007B79
      7B00000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000808080000000000000FFFF00808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B797B007B797B007B797B0000000000000000007B797B007B797B007B79
      7B00000000000000000000000000000000000000000000000000000000008080
      800080808000808080008080800080808000000000000000000000FFFF000000
      00000000000000000000000000000000000000000000000000007B797B007B79
      7B000000000000000000000000007B797B007B797B007B797B007B797B000000
      0000000000000000000000000000000000000000000000000000000000008080
      800080808000808080008080800080808000000000000000000000FFFF000000
      00000000000000000000000000000000000000000000000000007B797B007B79
      7B000000000000000000000000007B797B007B797B007B797B007B797B000000
      000000000000000000000000000000000000000000000000000080808000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF00008080800000000000000000000000
      000000000000000000000000000000000000000000007B797B00000000000000
      000000000000000000000000000000000000000000007B797B00000000000000
      000000000000000000000000000000000000000000000000000080808000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF00008080800000000000000000000000
      000000000000000000000000000000000000000000007B797B00000000000000
      000000000000000000000000000000000000000000007B797B00000000000000
      0000000000000000000000000000000000008080800080808000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000080808000808080000000
      000000000000000000000000000000000000000000007B797B00000000000000
      000000000000000000000000000000000000000000007B797B00000000000000
      0000000000000000000000000000000000008080800080808000FFFF0000FFFF
      0000FFFF00000000FF00FFFF0000FFFF0000FFFF000080808000808080000000
      000000000000000000000000000000000000000000007B797B00000000000000
      0000000000007B797B000000000000000000000000007B797B00000000000000
      0000000000000000000000000000000000000000000080808000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000080808000000000000000
      0000000000000000000000000000000000007B797B0000000000000000000000
      00000000000000000000000000000000000000000000000000007B797B000000
      0000000000000000000000000000000000000000000080808000FFFF0000FFFF
      0000FFFF00000000FF00FFFF0000FFFF0000FFFF000080808000000000000000
      0000000000000000000000000000000000007B797B0000000000000000000000
      0000000000007B797B00000000000000000000000000000000007B797B000000
      0000000000000000000000000000000000000000000080808000FFFF00000000
      FF000000FF000000FF000000FF000000FF00FFFF000080808000000000000000
      0000000000000000000000000000000000007B797B0000000000000000007B79
      7B007B797B007B797B007B797B007B797B0000000000000000007B797B000000
      0000000000000000000000000000000000000000000080808000FFFF00000000
      FF000000FF000000FF000000FF000000FF00FFFF000080808000000000000000
      0000000000000000000000000000000000007B797B0000000000000000007B79
      7B007B797B007B797B007B797B007B797B0000000000000000007B797B000000
      0000000000000000000000000000000000000000000080808000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000080808000000000000000
      0000000000000000000000000000000000007B797B0000000000000000000000
      00000000000000000000000000000000000000000000000000007B797B000000
      0000000000000000000000000000000000000000000080808000FFFF0000FFFF
      0000FFFF00000000FF00FFFF0000FFFF0000FFFF000080808000000000000000
      0000000000000000000000000000000000007B797B0000000000000000000000
      0000000000007B797B00000000000000000000000000000000007B797B000000
      0000000000000000000000000000000000008080800080808000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000080808000808080000000
      000000000000000000000000000000000000000000007B797B00000000000000
      000000000000000000000000000000000000000000007B797B00000000000000
      0000000000000000000000000000000000008080800080808000FFFF0000FFFF
      0000FFFF00000000FF00FFFF0000FFFF0000FFFF000080808000808080000000
      000000000000000000000000000000000000000000007B797B00000000000000
      0000000000007B797B000000000000000000000000007B797B00000000000000
      000000000000000000000000000000000000000000000000000080808000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF00008080800000000000000000000000
      000000000000000000000000000000000000000000007B797B00000000000000
      000000000000000000000000000000000000000000007B797B00000000000000
      000000000000000000000000000000000000000000000000000080808000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF00008080800000000000000000000000
      000000000000000000000000000000000000000000007B797B00000000000000
      000000000000000000000000000000000000000000007B797B00000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000808080008080800080808000808080000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B797B007B79
      7B000000000000000000000000007B797B007B797B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000808080008080800080808000808080000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B797B007B79
      7B000000000000000000000000007B797B007B797B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B797B007B797B007B797B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B797B007B797B007B797B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084868400C6C7C600C6C7
      C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7
      C600C6C7C6000000000000000000000000000000000084868400C6C7C600C6C7
      C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7
      C600C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008486840000000000000000008486
      8400000000000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848684000000000000000000FFFF00008486
      8400848684000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008486
      8400000000000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF000000000000000000008486
      8400000000000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084868400FFFF0000FFFF0000000000008486
      8400848684000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008486840000000000000000008486
      8400000000000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000084868400848684000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084868400848684008486
      8400848684008486840084868400848684008486840084868400848684008486
      8400C6C7C6000000000000000000000000000000000084868400848684008486
      8400848684008486840084868400848684008486840084868400848684008486
      8400C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084868400848684008486
      8400848684008486840084868400848684008486840084868400848684008486
      8400848684000000000000000000000000000000000084868400848684008486
      8400848684008486840084868400848684008486840084868400848684008486
      8400848684000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000C6C7C6008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084000000C6C7
      C600840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000C6C7C600C6C7
      C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C6000000
      0000C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000C6C7C6008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C7C60000000000000000000000000084868400848684008486
      8400848684008486840084868400848684008486840084868400848684008486
      8400848684008486840000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084000000C6C7C600840000008400
      000084000000000000000000000000000000000000000000000084868400C6C7
      C600000000008486840000000000840000000000000000000000000000000000
      00000000840000000000000000000000000000000000C6C7C600C6C7C600C6C7
      C600C6C7C600C6C7C600C6C7C60000FFFF0000FFFF0000FFFF00C6C7C600C6C7
      C60000000000000000000000000000000000000000008486840000FFFF000000
      000000FFFF000000000000FFFF000000000000FFFF000000000000FFFF000000
      000000FFFF008486840000000000000000000000000000000000000000000000
      0000C6C7C600C6C7C600C6C7C600000000008486840084000000840000008400
      0000000000000000000000000000000000000000000084868400C6C7C600C6C7
      C600C6C7C6000000000084868400000000000000000000000000000000000000
      84000000840000000000000000000000000000000000C6C7C600C6C7C600C6C7
      C600C6C7C600C6C7C600C6C7C600848684008486840084868400C6C7C600C6C7
      C60000000000C6C7C600000000000000000000000000848684000000000000FF
      FF000000000000FFFF000000000000FFFF000000000000FFFF000000000000FF
      FF0000000000848684000000000000000000000000000000000084868400C6C7
      C600C6C7C600C6C7C600C6C7C600C6C7C6000000000084868400000000000000
      00000000000000000000000000000000000000000000C6C7C600C6C7C600C6C7
      C600C6C7C600C6C7C60000000000000000000000000000000000000084000000
      8400000084000000840000008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C7C600C6C7C60000000000000000008486840000FFFF000000
      000000FFFF000000000000FFFF000000000000FFFF000000000000FFFF000000
      000000FFFF008486840000000000000000000000000000000000C6C7C600C6C7
      C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C60000000000000000000000
      00000000000000000000000000000000000000000000C6C7C60000000000FFFF
      0000C6C7C600C6C7C600C6C7C600000000000000000000000000000000000000
      84000000840000000000000000000000840000000000C6C7C600C6C7C600C6C7
      C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C6000000
      0000C6C7C60000000000C6C7C6000000000000000000848684000000000000FF
      FF000000000000FFFF000000000000FFFF000000000000FFFF000000000000FF
      FF000000000084868400000000000000000000000000C6C7C600C6C7C600C6C7
      C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C6000000
      0000000000000000000000000000000000000000000084868400000000000000
      0000C6C7C600C6C7C60084868400000000000000000000000000000000000000
      0000000084000000000000000000000084000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C7
      C60000000000C6C7C6000000000000000000000000008486840000FFFF000000
      000000FFFF000000000000FFFF000000000000FFFF000000000000FFFF000000
      000000FFFF0084868400000000000000000000000000C6C7C600C6C7C600C6C7
      C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C6000000
      000000000000000000000000000000000000000000000000000084868400C6C7
      C600C6C7C6008486840000000000000000000000000000000000000000000000
      0000000000000000000000000000000084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C60000000000C6C7C6000000000000000000848684000000000000FF
      FF000000000000FFFF000000000000FFFF000000000000FFFF000000000000FF
      FF000000000084868400000000000000000000000000C6C7C60000000000FFFF
      0000C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008486840000FFFF000000
      000000FFFF000000000000FFFF000000000000FFFF000000000000FFFF000000
      000000FFFF0084868400000000000000000000000000C6C7C60000000000FFFF
      0000C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      0000000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000848684000000000000FF
      FF000000000000FFFF000000000000FFFF000000000000FFFF000000000000FF
      FF00000000008486840000000000000000000000000000000000000000000000
      0000FFFF0000FFFF0000C6C7C600C6C7C600C6C7C600C6C7C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      0000000084000000840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084868400848684008486
      8400848684008486840084868400848684008486840084868400848684008486
      8400848684008486840000000000000000000000000000000000848684000000
      00000000000000000000C6C7C600C6C7C600C6C7C60084868400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000084000000
      8400000084000000840000008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C600C6C7C600C6C7C600C6C7C6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C1696700D0656500C45C
      5C00A2747400D5CCCC00D1C0BF00F6F0ED00F6F1EF00E3E7E600E3E7E600932B
      2A009A373700BF5F600097433F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C46C6A00D0656500CA60
      6100A256570092292A0092292A00F7EDEA00FFFFFD00F3F8F600F3F8F6009227
      27009A363600C463630097433F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C36B6900D0656500C75E
      5E00A55B5B0092292A0092292A00DACECD00F0EFED00FFFFFF00FDFFFF009227
      27009A363600C362620097433F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      00008400000000000000000000000000000000000000C36B6900D0656500C75E
      5E00A75D5C0092292A0092292A00C1B4B300DEDEDC00FFFFFF00FDFFFF009126
      26009B363600C362620097433F00000000000000000000000000000000000000
      00000000000000000000000000000000000084868400C6C7C600C6C7C6008486
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      00008400000000000000000000000000000000000000C36B6900D0656500C75F
      5F00B05B5C00E9E2E200E9E2E200E9E2E200E9E2E200E9E2E200E9E2E2009B31
      3100A33E3E00C361620097433F00000000000000000000000000000000000000
      000000000000000000000000000084868400C6C7C600C6C7C600FFFF00008486
      8400848684000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C36C6900D0656500CC65
      6400C7616100D9909000DB949400D9888900D7868600D1828200CE757500C45B
      5B00C65E5E00C260610097433F00000000000000000000000000000000000000
      0000000000000000000000000000C6C7C600C6C7C600C6C7C600C6C7C6008486
      8400C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      00008400000000000000000000000000000000000000D0656500D0656500D6A3
      A100D7A5A300D8A6A400D8A6A400D8A6A400D8A6A400D8A6A500D8A6A500D8A7
      A500D7A6A400D065650097433F00000000000000000000000000000000000000
      0000000000000000000000000000C6C7C600FFFF0000C6C7C600C6C7C6008486
      8400C6C7C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      00008400000000000000000000000000000000000000D0656500D0656500FEFD
      FD00FEFDFD00FFFEFE00FFFEFE00FFFEFE00FFFEFE00FFFEFE00FFFEFE00FDFC
      FC00E5C7C600D065650097433F00000000000000000000000000000000000000
      000000000000000000000000000084868400FFFF0000FFFF0000C6C7C6008486
      8400848684000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6C7C60000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      00008400000084000000000000000000000000000000D0656500D0656500FEFE
      FE00FEFEFE00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00FEFE
      FE00E5C7C600D065650097433F00000000000000000000000000000000000000
      00000000000000000000000000000000000084868400C6C7C600C6C7C6008486
      840000000000000000000000000000000000000000000000000000000000C6C7
      C600000000000000000000000000C6C7C60000000000C6C7C600000000000000
      0000000000000000000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008400000084000000840000000000000000000000D0656500D0656500FEFE
      FE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFE
      FE00E5C7C600D065650097433F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C7C60000000000C6C7C60000000000C6C7C60000000000C6C7C600C6C7
      C600C6C7C6000000000084000000840000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      00000000000084000000840000008400000000000000D0656500D0656500FEFE
      FE00FEFEFE00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00FEFE
      FE00E5C7C600D065650097433F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C7C60000000000C6C7C60000000000C6C7C600C6C7C600C6C7
      C600C6C7C600C6C7C60084000000840000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      00000000000084000000840000008400000000000000D0656500D0656500FEFE
      FE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFE
      FE00E5C7C600D065650097433F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C6C7C60000000000C6C7C600C6C7C600C6C7C600C6C7
      C600C6C7C600C6C7C60084000000840000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      00000000000084000000840000008400000000000000D0656500D0656500FEFE
      FE00FEFEFE00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00FEFE
      FE00E5C7C600D065650097433F00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C7C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C6C7C600C6C7C600C6C7C600C6C7C600C6C7
      C600C6C7C6000000000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      00008400000084000000840000000000000000000000D0656500D0656500FEFE
      FE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFE
      FE00E5C8C600D065650097433F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B48D8A00B18A
      8700B1888500AF868300AE858200AD838000AB817F00AA807C00A97F7B00A87C
      7900A77B7800A77A7700A6797600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000006B2A00006828
      00005C230000521F00004E1E00004E1F00004E1F00004E1F00004E1F00004F1F
      00004F1F00004018000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000632E1000632E100000000000000000000000000B58F8C00FEFE
      FD00FEFEFD00FEFEFD00FEFEFD00FEFEFD00FEFEFD00FEFEFD00FEFEFD00FEFE
      FD00FEFEFD00FEFEFD00A77B780000000000000000008BD4EE006BD3F80028B0
      E000019ACF00019ACF00019ACF00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000963A0000BC4A0000B546
      0000AA420000A13F00009A3D0000983C0000993C0000993C0000993C0000993C
      00009F3F00007D3100004018000000000000000000000732DE000732DE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000632E100153EE100000000000000000000000000B7928E00FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFC
      FB00FEFCFB00FEFCFB00A97D7900000000000000000076C8E500A9E9FE0075DB
      FF0077DCFF0063D1F90030B3E300019ACF00019ACF00019ACF00000000000000
      00000000000000000000000000000000000000000000B7480000E2580000D352
      0000CA4F0000C24C0000B8480000B0450000AD440000AC430000AD430000AD43
      0000B44600009F3F00004F1F000000000000000000000732DE000732DE000632
      E100000000000000000000000000000000000000000000000000000000000632
      E1000533E9000000000000000000000000000000000000000000B9949200FEFA
      F600FEFAF600FEFAF600FEFAF600FEFAF600FEFAF600FEFAF600FEFAF600FEFA
      F600FEFAF600FEFAF600AA7F7C0000000000000000001FA9D6008FD3EB006FD9
      FE0071D9FE0071D9FE0071D9FE0071D9FE0073DAFE007BDFFF007ADEFF0077DC
      FF00019ACF0000000000000000000000000000000000C44D0000E75B0000D956
      0000D1530000CF621000D07A3200D0824100CA7F4000BD6A2800A6410000A741
      0000AD430000993C00004F1F0000000000000000000000000000153EE1000632
      E1000632E1000000000000000000000000000000000000000000637FF0000533
      E900153EE1000000000000000000000000000000000000000000BB979300FEF9
      F400FEF9F400FEF9F400FEF9F400FEF9F400FEF9F400FEF9F400FEF9F400FEF9
      F400FEF9F400FEF9F400AA807D00000000000000000031B1DC0049B7DE0071DD
      FE0077DEFE0077DEFE0077DEFE0077DEFE0077DEFE0076DEFE0076DEFE007CE1
      FF0065D2F80000000000000000000000000000000000CA4F0000F3680700E960
      0400E98E4000FEFEFE00FEFEFE00FEFEFE00FEFEFE00E1B28800AA420000A741
      0000AD430000993C00004E1F0000000000000000000000000000000000005472
      E9000632E1000533E9000000000000000000000000000533E9000533E9000000
      0000000000000000000000000000000000000000000000000000BD999600FEF6
      EF00FEF6EF00FEF6EF00FEF6EF00FEF6EF00FEF6EF00FEF6EF00FEF6EF00FEF6
      EF00FEF6EF00FEF6EF00AD838000000000000000000060CAEF001FA8D800BBF4
      FE007DE7FE0081E8FE0081E8FE0081E8FE0081E8FE0081E8FE0081E8FE0084E9
      FE005ED3F100019ACF00000000000000000000000000CA4F0000F67A1C00F06B
      0C00FEFEFE00F0AF7500DA650E00D35F0B00CA5A0900BF4E0200B5470000AB43
      0000AD430000993C00004E1F0000000000000000000000000000000000000000
      00008298F1000533E9000533E900000000003459EE000533E900143FEC000000
      0000000000000000000000000000000000000000000000000000BD9B9800FEF5
      ED00FEF5ED00FEF5ED00FEF5ED00FEF5ED00FEF5ED00FEF5ED00FEF5ED00FEF5
      ED00FEF5ED00FDF4EC00AD848100000000000000000065CFF5003EB7E500BFEE
      F8008DEFFF0085EDFF0085EDFF0086EDFF0086EDFF0086EDFF0086EDFF0089EE
      FF0065D9F300019ACF00000000000000000000000000CA4F0000F7913C00F075
      1600FEFEFE00EE6C0C00E5590000DC560000D4530000EAB68700C04B0000B848
      0000B64700009D3D00004F1F0000000000000000000000000000000000000000
      00000000000000000000244CEE000434F5000434F50000000000000000000000
      0000000000000000000000000000000000000000000000000000C09E9B00FEF3
      E800FEF3E800FEF3E800FEF3E800FEF3E800FEF3E800FEF3E800FEF3E800FEF3
      E800FDF2E700FDF1E600AF878400000000000000000077D5FC005CC8FB0020A7
      D500B6E6F300D1F5FA00D2F6FA00B9FBFE009BF8FF0091F6FF0092F6FF006BD0
      B7000C851800A4FFFF0043C1E2000000000000000000CA4F0000F79F5400F07B
      1F00FEFEFE00F3964500EE5D0000E95B0000DE570000FEFEFE00EAB17E00C44D
      0000C14B0000A541000058230000000000000000000000000000000000000000
      000000000000000000001440F0000533E9000434F50000000000000000000000
      0000000000000000000000000000000000000000000000000000C1A09C00FFF2
      E600FFF2E600FFF2E600FFF2E600FFF2E600FFF2E600FFF2E600FFF2E600FEF1
      E500FEF1E500F9ECDE00B188860000000000000000008BDBFF005FCDFF002CAF
      E3000D9FD30010A0D30010A0D3008ED7EC00E2FDFE00A5F8FF00A3F8FF000C85
      180038B55700ABF3EB00B5FCFD000000000000000000CA4F0000F8A76000F281
      2800F8BB8200FEFEFE00FAD8B700FAD8B700FBE6D100FEFEFE00FEFEFE00EFC5
      9D00CE500000B145000068280000000000000000000000000000000000000000
      0000000000000434F5000533E90000000000000000000434F500325BF9000000
      0000000000000000000000000000000000000000000000000000C4A39F00FFEF
      E100FFEFE100FFEFE100FFEFE100FFEFE100FFEFE100FFEFE100FEEEE000FDED
      DF00F8E8D800E5D6C100B38C890000000000000000009FE9FF0070DCFF0076DD
      FE0076DDFE0074DCFE0073DCFE0061CEF6001CA8D9008CCED7000C8518005BE6
      8C0059E189003EBD60000C851800019ACF0000000000CA4F0000F8AD6B00F388
      3200F07A1E00F49F5600F7BF8A00F7BC8600FAD4B100FEFEFE00FEFEFE00F2BF
      8F00DA550000BF4A0000772E0000000000000000000000000000000000006180
      F9000434F5000533E900335AF20000000000000000001341F7000335F9000000
      0000000000000000000000000000000000000000000000000000C4A4A100FFEE
      DF00FFEEDF00FFEEDF00FFEEDF00FFEEDF00FFEEDF00FEEDDE00FEEDDE00F7E6
      D500EEDECB00D9C9B100B58E8B000000000000000000A7EFFF0076E5FF007CE5
      FF007CE5FF007DE5FF007DE5FF007DE3FF0072DDFB000C8518002DAD470050D9
      7B0055DE83005AE38B0033AF51000197C30000000000CA4F0000FAB77B00F496
      4600F27F2400F07A1E00F0721300EF6C0D00EF6A0A00FEFEFE00F7B07200E95B
      0000E55A0000CB4F0000873500000000000000000000000000006180F9000434
      F5000434F5000000000000000000000000000000000000000000000000000335
      F900325BF9000000000000000000000000000000000000000000C6A7A300FFEC
      DA00FFECDA00FFECDA00FFECDA00FFEBDA00FEEBD900FCE9D700F6E3CF00E2D0
      B800D7C6AB00E5D4C100BB9894000000000000000000C7FFFF0082F5FF008FF5
      FF008FF5FF008DF4FF00A0FDFF00AFFFFF00AEFFFF00A5FBF800A3FCFA0032B7
      4F0048D670000C851800000000000000000000000000CA4F0000FABA8000F7B4
      7700F6A45C00F49A4E00F38F3C00F2842C00F07A1E00F7B07000EE620300EF5E
      0000F25E0000D9550000963A00000000000000000000325BF9000335F9000434
      F500335AF2000000000000000000000000000000000000000000000000008099
      FC000335F9000000000000000000000000000000000000000000C7A8A400FFEA
      D800FFEAD800FFEAD800FFEAD800FEE9D700FEE9D700F5E1CC00F7EEE400FEFE
      FD00FEF8F200C5A59F00000000000000000000000000A4E0F000A0FDFF0090FC
      FF0090FCFF0099FDFF0086E8F500019ACF00019ACF00019ACF00019ACF002CB3
      470041D166000C851800000000000000000000000000CB4F0000FAA65D00FABB
      8200FABD8700FAB77B00F8AC6900F79D4F00F6872D00F4741300F3660400F360
      0000FA620000E3590000A140000000000000000000000335F9000335F9000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C8AAA700FFE8
      D300FFE8D300FFE7D200FEE7D200FBE4CE00F3DDC600DFCAAF00FAF6F100DFC6
      BE00D2BAB7000000000000000000000000000000000000000000019ACF00019A
      CF00019ACF00019ACF00000000000000000000000000000000000000000028BB
      410026B13E000000000000000000000000000000000000000000CF5C0A00D46C
      1C00D46E1E00D46E1E00D36A1800D1651300CF5C0A00CC550400CB510100CA50
      0000CB500000BA4800000000000000000000000000000335F900325BF9000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C9ABA800FFE7
      D100FFE7D100FEE6D000FEE6CF00F3DBC300E9D3B800E5D6C200FAF0E700D9C5
      C400000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000C8518001CAE
      31000C8518000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CAACA900C9AB
      A800C9AAA700C8A8A500C7A8A500C5A6A300C5A5A200CFB4B200E7DAD9000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000C8518000C8518000C8518000C8518000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000084000000840000008400
      00008400000084000000840000008400000000000000000000006B2A00006828
      00005C230000521F00004E1E00004E1F00004E1F00004E1F00004E1F00004F1F
      00004F1F00004018000000000000000000000000000000000000000000000000
      0000840000000000000000000000840000000000000000000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000084000000000000000000000000000000000000000000
      00000000000000000000000000008400000000000000963A0000BC4A0000B546
      0000AA420000A13F00009A3D0000983C0000993C0000993C0000993C0000993C
      00009F3F00007D31000040180000000000000000000000000000000000000000
      0000840000000000000000000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000000000
      0000000000000000000000000000840000000000000084868400008684008486
      8400008684008486840084000000000000000000000000000000000000000000
      00000000000000000000000000008400000000000000B7480000E2580000D352
      0000CA4F0000C24C0000B8480000B0450000AD440000AC430000AD430000AD43
      0000B44600009F3F00004F1F0000000000000000000000000000000000000000
      0000840000000000000000000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000000000
      0000000000000000000000000000840000000000000000868400848684000086
      8400848684000086840084000000000000000000000000000000000000000000
      00000000000000000000000000008400000000000000C44D0000E75B0000D956
      0000D1530000C94E0000CE732800CF824000CA804100C1733200B1551000A741
      0000AD430000993C00004F1F0000000000000000000000000000000000000000
      0000000000008400000084000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000000000
      0000000000000000000000000000840000000000000084868400008684008486
      8400008684008486840084000000000000000000000000000000000000000000
      00008400000084000000840000008400000000000000CA4F0000F3680700E960
      0400DE5A0100D4530000EAB78800FEFEFE00FEFEFE00FEFEFE00FEFEFE00C47A
      3D00AD430000993C00004E1F0000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000000000
      0000000000000000000000000000840000000000000000868400848684000086
      8400848684000086840084000000000000000000000000000000000000000000
      00008400000000000000840000000000000000000000CA4F0000F67A1C00F06B
      0C00EB620400E1590000D8580200D35C0900CB5C0B00C55B0E00DDA67400FEFE
      FE00AD430000993C00004E1F0000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000000000
      0000000000000000000000000000840000000000000084868400008684008486
      8400008684008486840084000000000000000000000000000000000000000000
      00008400000084000000000000000000000000000000CA4F0000F7913C00F075
      1600EF670700ED620200F4BB8700DC560000D4530000CB4F0000C5570900FEFE
      FE00B64700009D3D00004F1F0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000000000
      0000840000008400000084000000840000000000000000868400848684000086
      8400848684000086840084000000840000008400000084000000840000008400
      00008400000000000000000000000000000000000000CA4F0000F79F5400F07B
      1F00EF6A0A00F7BA8100FEFEFE00E95B0000DE570000D4530000DE884100FEFE
      FE00C14B0000A541000058230000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000000000
      0000840000000000000084000000000000000000000084868400008684008486
      8400008684008486840000868400848684000086840084868400008684008486
      84000086840000000000000000000000000000000000CA4F0000F8A76000F281
      2800FAD0A900FEFEFE00FEFEFE00FBE6D100FAD8B700F7D7B700FEFEFE00E9AB
      7300CE500000B145000068280000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000000000
      0000840000008400000000000000000000000000000000868400848684000000
      0000000000000000000000000000000000000000000000000000000000008486
      84008486840000000000000000000000000000000000CA4F0000F8AD6B00F388
      3200FACCA400FEFEFE00FEFEFE00FAD4B200F7BB8400F7BA8200EE8E3E00DD56
      0000DA550000BF4A0000772E0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000084868400848684000000
      0000C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600C6C7C600000000008486
      84000086840000000000000000000000000000000000CA4F0000FAB77B00F496
      4600F27F2400F8BF8A00FEFEFE00EF6C0D00EF6A0A00EF640400EE5E0000E95B
      0000E55A0000CB4F000087350000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000868400848684000086
      84000000000000FFFF00000000000000000000FFFF0000000000848684000086
      84008486840000000000000000000000000000000000CA4F0000FABA8000F7B4
      7700F6A45C00F49A4E00F8C59500F2842C00F07A1E00EF6A0A00EE620300EF5E
      0000F25E0000D9550000963A0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000CB4F0000FAA65D00FABB
      8200FABD8700FAB77B00F8AC6900F79D4F00F6872D00F4741300F3660400F360
      0000FA620000E3590000A1400000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CF5C0A00D46C
      1C00D46E1E00D46E1E00D36A1800D1651300CF5C0A00CC550400CB510100CA50
      0000CB500000BA48000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000900100000100010000000000800C00000000000000000000
      000000000000000000000000FFFFFF00DFFBFFFFFFFF0000DFFBBEFFBEFF0000
      0000DDFFDDFF0000DF7BEBFFEBFF0000DF7BF7FFF7FF0000DF7BEBFFEBFF0000
      DF7BDDFFDDFF0000C003BEBFBEBF0000DB7BFFBFFFBF0000DB7BFFBFFFBF0000
      C07BFF8FFF8F0000DB7BFFB7FFB70000DB7BFFB7FFB700000000FFB7FFB70000
      DFFBFF8FFF8F0000DFFBFFFFFFFF0000FFFFFFF7FFC7FFFFFFFFFFE7FF87CFFF
      FFFFFF4FF707B7FFFFFFFF1FF20FB7FF8003FF07F01FCFFFBBBBFF1FE03FFEFF
      BBBBCF7FE07FFF7FBBBBB3FFC03FFA418003BCFFC01FFBEFBBBB9B3F80EFFBF7
      BBBB63BF8397FBFBBBBB79BF0F77FBFD8003367F3CFBF3DDFFFFC77FFBF1FBE3
      FFFFF37FFB0FFFFFFFFFFCFFFCFFFFFFFFF1FFFFFFC7FFFFFFE1FF0FFF87FFFF
      FDC1FF3FF707FFFFFC83FF5FF20FFFFFFC07FF6FF01FFFFFF80FFFEFE03FFFFF
      F81FFFEFE07FFFFFF00F3FFCC03FFFFFF0070000C01FFFFFE03F3FFC80FFFFFF
      E0FFFFEF83FF0007C3FFFFEF0FFF7777CFFFFF6F3CFFBBBB3FF9FF5FFB7FDDDD
      0001FF3FFB7FC0013FF9FF0FFCFFFFFF8001FFFFFFFF80000000F800FFFF8000
      0000F800FFFF80000000F800FFFF80000000F800FFFF80000000F80000008000
      0000F80000008000000080000000800000008000000080000000800000008000
      0000800000008000000080000000800000008007FFFF800000008007FFFF8000
      00008007FFFF80008001FFFFFFFFFFFFFFFFFFFFFFFFFFFF83FFFFFFFFFFFFFF
      BBFF8F0780018001BBFF000180018001BBFF0000800180018003000080018001
      BBBB000080018001BBBB000080018001BBBB0000800180018003000080018001
      BBBF000080018001BBBF00008003E7FFBBBF8001FFFFFFFF803FC007FFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFF800383FF
      C7FEFFFFBFFBBBFF9FFFFFBFBFFBBBFFBE03FF1FBFFBBBFF3EFDFE0FBFFBB803
      79FEFC07BFFBBFFB7BFEEE0FBFFBBFFB77E7EF1FBFFBBFFB77DBEFBFBFFBBF83
      77BDEFFFBFFBBFBF37BDEFFFBFFBBFBFBBDBAFFFBFFBBFBFBBE7CFFF8003803F
      9CFFEFFFFFFFFFFFDF7FFFFFFFFFFFFFFBFF8001FFF9FFF9F9FF8001FFF1FFF1
      F8FF8001FFE3FFE3F87F8001FFC7FFC7F83F8001FC8FFC8FF81F8001F01FF01F
      F80F8001F03FF03FF8078001E01FE01FF8078003E01FE01FF80F8007F03FF03F
      F81F800FF03FF03FF83F801FFCFBDCFFF87FFFFFFFFDBFFFF8FFDFFB80000001
      F9FF8001FFFDBFFFFBFFDFFBFFFBDFFFFFFFFFFFFFFFFFFF9FFFFFFF80018003
      E7FFFFFF8001BBBBF9FFFCFF8001BBBBFE7FFCFF8001BBBBFF9FFF9F80018003
      FFE7F99F8001BBBB9FF9F9F38001BBBBE7FFFF338001BBBBF9FFF33F80018003
      FE7FF3E78001BBBBFF9FFE678001BBBBFFE7FE7F8001BBBBFFF9FFCF80018003
      FFFFFFCF8001FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF801FFFFFFC3FFF3F
      800FE3FFFC3FFECF80078BFFFC3FFEF38003A3FFFC3FFDFC8001BFFFFC3FFE7D
      8001BFE38001FF9B8001B00B8001FFE78001B7E38001E7FF8001B63F8001D9FF
      800180BFFC3FDE7FC001BE3FFC3FBF9FE001A3FFFC3FCFBFF0018BFFFC3FF37F
      F801E3FFFC3FFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF801F801F801F
      FFFFBFCFBBCFBFCFC007BFD7BBD7BFD7DFF7BFDBBBDBBFD3DFF7BFDDBBDDBFD5
      DFF7BFDD801DBFD5DC77BFDDBBDDBFC5DC77BFDDBBDDBFD5DC77BFDDBBDDBFD1
      DFF7801D801D8015DFF7DDEDDFEDDFE5DFF7E005EFF5EFF5C007F779F7F9F7F9
      FFFFF801F801F801FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC1FFC1FFBF7EFFFF
      FE0FDE0FDF7D8FE3FFEFDFEFFFFF8003FFEFBFEFF0078FE3FFDFBFDFF777DFF7
      FFDFBFDFF777DFF79FDF9FDFF777DFF7E7BFE7BF9004DFF7F9BFF9BFF777DFF7
      FE6DFE6DF777DFF7FFC0FFC0F777DFF7FFEDFFEDF0078FE3FFEDFFEDFFFF8003
      FFC0FFC0DF7D8FE3FFEDFFEDBF7EFFFFFFFFFFFFC001FF7EF3CF80FFC0019E79
      EDAFFEFFC001C0038E2FFEFFC001EFF78717FEFFC001EFF78717FEFFC001EFF7
      C797FE07C001EFF7C797F7F7C0010FF1C38BB7F7C0018FF0C083D7F7C001EF07
      E34FE7F7C001EF6FE79F0077C001EF4FFFFFF3F7C003EF27FFFFF5F7C007C073
      FFFFF6F1C00F9E79FFFFF7FFC01F7EFEFFFFF1FFFFFFF1FFF3CFE67F801FE67F
      E58FE79FBFCFE79F860FC7E7BFC7C7E78307C7F7BFC3C7F78307C7F7BFC1C7F7
      C387C7F7BFC1C7F7C387C7F7BFC1C7F7C183C1F7BFC1C1F7C083C077BFC1CE77
      E34FC0178001CF97E79FC007C001DFE7FFFFC007E001DFF7FFFFC00FF001C1EF
      FFFFFE0FF801FE0FFFFFFFFFFFFFFFFFF7FFFF3FF1FFFFFFB7FFF88FE67F801F
      D40FC7D3E79FBFCFE7F13FDCD7E7BFD7007DCFDDD7F7BFDBF3FBF3DDD7F7BFDD
      F5FBFCDBD7F7BFDDF6FBF707D7F7BFDDF7FBB7DFD1F7BFDDFFF7D7FFCE77BFDD
      CFF7E7FFCF97801DF3F7007FDFE7DFEDFCF7F3FFDFF7EFF5FF2FF5FFC1EFF7F9
      FFCFF6FFFE0FF801FFFFF7FFFFFFFFFFFFFFFFFFFFFFFEFFBFFFFF87DFFBBEFF
      DFFFFF9F8001DEFFE3FFFFAFDFFBEEFFE3FFFFB7DC7BF6FFE3FF80F7DBBBFBFF
      FFFFB6FBDBBBFE7FFEFFB6FBD9BB0420FF7F80F9DA7BFC3FFFFFB6FBDBFBFE7F
      FFC7B6FBDBBBFFDF77C780F7DC7BFEEFAFC7FFB7DFFBFEF7DFFBFFAF8001FEFB
      AFFDFF9FDFFBFEFD77FFFF87FFFFFEFFFFFFFFFFFFFFFFFFFFE3FFFFFEFFBFFF
      FFCBF07FBE7FDFFFFB93CFFFBE07EFFFF927DFFFDE0FF7FFFA4FDFFFDE1FFBFF
      F79FC0FFEE3FFC3FF7BFF71FEE7FFC3FEFDFEFEFF6FFFC3FEF8FDFF7F1FFFC3F
      DE7FDFFBF1FFFFDFD9FFDFF7F0FFDFEFA7FFDF8FFF3FDFF79FFFE07FFFCF07FB
      FFFFFFFFFFF3DFFDFFFFFFFFFFFFDFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFE3
      FEFFFFFFEFEFFFC3FEFFFFFFEFEFFB83FEFFFFFF8003F907FEFFFFFFEEEFF80F
      FEFFFFFFEEEFF01FFEFFFFFFEEEFF03FFEFF8001E00FE01FFEFFFFFFEEEFE00F
      FEFFFFFFEEEFC07FFEFFFFFFEEEFC1FFFEFFFFFF800387FFFEFFFFFFEFEF9FFF
      FEFFFFFFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFFFDFFFFF807FFF8FFF8FFFF
      FBF7FFF1FFF1FDBFF7FBFFE3FFE3FDBFEFFBFFC7FFC7FDBFEFFBE08FF18FFDBF
      DBFBC01FCE1FDDBBB2AB803FBFBFBDBD8AAB001FBFBF05A0FAAB001F7FDFBDBD
      FAAB001F7FDFDDBBFAA7001F7FDFFDBFFAAF001FBFBFFDBFFC9F803FBFBFFDBF
      FE7FC07FCE7FFDBFFFFFE0FFF1FFFFFFFFFDFFFDFFFDFFFDFFF8FFF8FFF8FFF8
      FFF1FFF1FFF1FFF1FFE3FFE3FFE3FFE3FFC7FFC7FFC7FFC7E08FF18FE08FF18F
      C01FCE1FC01FCE1F803FBFBF803FBFBF001FBFBF001FBBBF001F7FDF001F7BDF
      001F60DF001F60DF001F7FDF001F7BDF001FBFBF001FBBBF803FBFBF803FBFBF
      C07FCE7FC07FCE7FE0FFF1FFE0FFF1FFFFFFFFFFFFFFFFFFE3FFE3FF83FF8001
      FC0FEC0FFBFFBFFDFFF1EFF1FBFFBFFDFFFDEFFDFBFFBFFDFFFBDFFBFBFFBFFD
      FFFBDFFBF807BFFDFFFBDFFBFFF7BFFDFFFBDFFBFFF7BFFDFFF7DFF7FFF7BFFD
      CFF7CFF7FFF7BFFDF3F7F3F7FFF7BFFDFCF7FCF7FFF7BFFDFF2FFF2FFFF7BFFD
      FFCFFFCFFFF18001FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF80078007FFFFFF6F9FF79FF7FFFFFEC79FF79DF7FE7F
      FFEF9FF798F7FC3FFF6F9FF79077F81FFE279FF79237F81FFF6F9FF79717FC3F
      FFFF9FF79F97FE7FFFFF9FF79FD7FFFFFFFF9FF79FF7FFFFFFFF80078007FFFF
      FFFF80078007FFFFFFFFFFFFFFFFFFFFFFF3FFFFFFFFFFFFFFE1FF3FC007FFFF
      FFC1FE3F8003C001FF83C07F00018001F00788F700019551C10F04E700018AA1
      809F02C100008441805F20E60000A009000F30F680009111000F81FEDFE0A289
      200FC3BFE8218541200FFFB7EFF78AA1B01FFFB3F41780039C1FFFC1F7FBFFFF
      C03FFFF3F803FFFFF0FFFFF7FFFFFFFFFFFFFFFFFFFFFFFF8001000C000FF9FF
      80017FE87FEFF9FF80017F01482FF3C780017E037FEF73C780017C03482F27FF
      80017C037FEF07C780017C037F6F00C780017C034E2F01E380017E07440403F1
      80017F0F6000063880017FEF00000E3880017F0FF8001E3880017F1FFC003F01
      80017F3FFE047F83FFFF007FFFFFFFFFFFFFFFFFC001FFFFC003FFF9C00181FF
      80019FF9C001803F80018FE7C00180078001C7C7C00180078001E39FC0018003
      8001F11FC00180038001FC7FC00180018001FC7FC00180018001F99FC0018000
      8001E19FC00180008001C7E7C0018003800187E7C003800380019FFFC007C3E7
      C0039FFFC00FFFC7FFFFFFFFC01FFE1FFFFFFFFFFFFFFFFFF9FFFFFFFC00C003
      F6CFFE0081FE8001F6B7FEFE01028001F6B7FE8201FE8001F8B780FE01108001
      FE8FBE8201F58001FE3FA0FE01F38001FF7FBE9000038001FE3FA0F500038001
      FEBFBEF300038001FC9FA40700038001FDDFBD7F00038001FDDFBCFF80078001
      FDDF81FFF87FC003FFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object sdModpathInput: TSaveDialog
    OnClose = sdModpathInputClose
    OnShow = sdModpathInputShow
    DefaultExt = '.mpn'
    Filter = 'MODPATH Name Files (*.mpn)|*.mpn|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save MODPATH input files'
    Left = 176
    Top = 216
  end
  object odModelMate: TOpenDialog
    DefaultExt = '.mtc'
    Filter = 'ModelMate Files (*.mtc)|*.mtc'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 328
    Top = 96
  end
  object sdZonebudgetInput: TSaveDialog
    OnClose = sdZonebudgetInputClose
    OnShow = sdZonebudgetInputShow
    DefaultExt = '.zb_zones'
    Filter = 
      'ZONEBUDGET Zone Files (*.zb_zones)|*.zb_zones|All Files (*.*)|*.' +
      '*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save ZONEBUDGET input files'
    Left = 272
    Top = 120
  end
  object sdModelMate: TSaveDialog
    OnClose = sdModelMateClose
    OnShow = sdModelMateShow
    DefaultExt = '.mtc'
    Filter = 'ModelMate Files (*.mtc)|*.mtc'
    Left = 264
    Top = 168
  end
  object menuGridLineChoice: TPopupMenu
    Images = ilImageList
    Left = 504
    Top = 48
    object Showall1: TMenuItem
      Action = acShowAllGridLines
      AutoCheck = True
    end
    object Showexterior1: TMenuItem
      Action = acShowExteriorGridLines
      AutoCheck = True
    end
    object Showactive1: TMenuItem
      Action = acShowActiveGridLines
      AutoCheck = True
    end
    object Showactiveedge1: TMenuItem
      Action = acShowActiveEdge
      AutoCheck = True
    end
  end
  object sdShapefile: TSaveDialog
    OnClose = sdShapefileClose
    OnShow = sdShapefileShow
    DefaultExt = '.shp'
    Filter = 'Shapefiles|*.shp'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Export Contours'
    Left = 264
    Top = 216
  end
  object sdModflowLgr: TSaveDialog
    OnClose = sdModflowInputClose
    OnShow = sdModflowInputShow
    DefaultExt = '.lgr'
    Filter = 
      'MODFLOW-LGR Control Files (*.lgr)|*.lgr|MODFLOW Name Files (*.na' +
      'm)|*.nam|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save MODFLOW-LGR input files'
    Left = 336
    Top = 176
  end
  object dlgSaveMt3dms: TSaveDialog
    OnClose = dlgSaveMt3dmsClose
    OnShow = dlgSaveMt3dmsShow
    DefaultExt = '.mt_nam'
    Filter = 'MT3DMS Name Files (*.mt_nam)|*.mt_nam|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save MT3DMS input files'
    Left = 336
    Top = 232
  end
  object pmExportModel: TPopupMenu
    Alignment = paCenter
    Left = 400
    Top = 72
    object miExportModpathPopUp: TMenuItem
      Action = acExportModpath
      Caption = 'Export MODPATH Input Files'
    end
    object miExportZoneBudgetPopup: TMenuItem
      Action = acExportZoneBudget
      Caption = 'Export &ZONEBUDGET Input Files'
    end
    object miRunMt3dmsPopup: TMenuItem
      Action = acRunMt3dms
      Caption = 'Export MT3DMS Input Files'
    end
    object miRunPEST: TMenuItem
      Action = acRunPest
    end
  end
  object sdSutraInput: TSaveDialog
    OnClose = sdSutraInputClose
    OnShow = sdSutraInputShow
    DefaultExt = '.inp'
    Filter = 'SUTRA input files|*.inp|All Files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save SUTRA input files'
    Left = 544
    Top = 8
  end
  object dlgSaveHeadObsToShapefile: TSaveDialog
    DefaultExt = '.shp'
    Filter = 'Shapefiles|*.shp'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Export Head Observations'
    Left = 416
    Top = 240
  end
  object dlgOpenImportSutraMesh: TOpenDialog
    Filter = 
      'Known mesh types|*.exp;*.msh|Argus ONE quadrilateral mesh (*.exp' +
      ')|*.exp|Gmsh mesh (*.msh)|*.msh|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 424
    Top = 8
  end
  object tmrImportErrors: TTimer
    Enabled = False
    OnTimer = tmrImportErrorsTimer
    Left = 576
    Top = 16
  end
  object sdSaveSutraMesh: TSaveDialog
    DefaultExt = 'exp'
    Left = 240
    Top = 272
  end
  object sdFootprint: TSaveDialog
    OnClose = sdFootprintClose
    OnShow = sdFootprintShow
    DefaultExt = '.fpi'
    Filter = 'Footprint input files (.fpi)|*.fpi|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save Footprint input file'
    Left = 424
    Top = 192
  end
  object bhntMeasureRuler: TJvBalloonHint
    Options = []
    ApplicationHintOptions = []
    OnMouseMove = bhntMeasureRulerMouseMove
    Left = 392
    Top = 8
  end
  object dlgSavePest: TSaveDialog
    OnClose = dlgSavePestClose
    OnShow = dlgSavePestShow
    DefaultExt = '.pst'
    Filter = 'PEST Control Files (*.pst)|*.pst'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 560
    Top = 120
  end
  object odSutraFiles: TOpenDialog
    DefaultExt = '.14B'
    Filter = 
      'SUTRA Files (*.14B, *.15B, *.PVEC;*.UVEC)|*.14B;*.15B;*.PVEC;*.U' +
      'VEC|SUTRA Data Set 14B (*.14B)|*.14B|SUTRA Data Set 15B (*.15B)|' +
      '*.15B|SUTRA Inital Pressure file (*.PVEC)|*.PVEC|SUTRA Inital U ' +
      'file (*.UVEC)|*.UVEC'
    InitialDir = '(Directory)'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    OnTypeChange = odSutraFilesTypeChange
    Left = 600
    Top = 128
  end
  object odRunParRep: TOpenDialog
    OnClose = odRunParRepClose
    OnShow = odRunParRepShow
    Filter = 
      'Best parameter value files (*.bpa)|*.bpa|Iteration Parameter val' +
      'ue files (*.par.*)|*.par.*|All parameter value files (*.par.*, *' +
      '.bpa)|*.par.*;*.bpa|Any file|*.*'
    InitialDir = '(Directory)'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 656
    Top = 120
  end
  object pmExportModelSutra: TPopupMenu
    Left = 432
    Top = 72
    object miExportPESTcontrolfile: TMenuItem
      Action = acRunPest
    end
  end
  object dlgSaveBoundaryCsv: TSaveDialog
    DefaultExt = '.csv'
    Filter = 'CSV files|*.csv'
    Left = 720
    Top = 72
  end
end
