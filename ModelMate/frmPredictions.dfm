object FormPredictions: TFormPredictions
  Left = 0
  Top = 0
  Caption = 'Predictions'
  ClientHeight = 680
  ClientWidth = 941
  Color = clBtnFace
  Constraints.MinHeight = 378
  Constraints.MinWidth = 710
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 18
  object JvNetscapeSplitter1: TJvNetscapeSplitter
    Left = 0
    Top = 479
    Width = 941
    Height = 6
    Cursor = crVSplit
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    Color = clSilver
    MinSize = 112
    ParentColor = False
    ResizeStyle = rsUpdate
    OnMoved = JvNetscapeSplitter1Moved
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ShowButton = False
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 658
    Width = 941
    Height = 22
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    Constraints.MaxHeight = 27
    Constraints.MinHeight = 22
    Panels = <
      item
        Width = 50
      end>
    UseSystemFont = False
    OnClick = StatusBar1Click
  end
  object pnlLower: TPanel
    Left = 0
    Top = 485
    Width = 941
    Height = 173
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    Constraints.MinHeight = 131
    TabOrder = 1
    DesignSize = (
      941
      173)
    object lblPredGpsTable: TLabel
      Left = 5
      Top = 13
      Width = 210
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Prediction Groups Table'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -18
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnAddPredGp: TButton
      Left = 241
      Top = 8
      Width = 143
      Height = 30
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Add Group...'
      TabOrder = 0
      OnClick = btnAddPredGpClick
    end
    object btnDeleteSelPredGps: TButton
      Left = 392
      Top = 8
      Width = 205
      Height = 30
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Delete Selected Group(s)'
      TabOrder = 1
      OnClick = btnDeleteSelPredGpsClick
    end
    object btnCancel: TBitBtn
      Left = 473
      Top = 139
      Width = 89
      Height = 30
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akBottom]
      DoubleBuffered = True
      Kind = bkCancel
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 4
    end
    object btnOK: TBitBtn
      Left = 376
      Top = 139
      Width = 90
      Height = 30
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akBottom]
      Caption = 'OK'
      Default = True
      DoubleBuffered = True
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 3
      OnClick = btnOKClick
    end
    object rbwdgPredGps: TRbwDataGrid4
      Left = 1
      Top = 42
      Width = 937
      Height = 93
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabStop = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      DefaultColWidth = 50
      FixedCols = 1
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goEditing, goAlwaysShowEditor]
      TabOrder = 5
      OnExit = rbwdgPredGpsExit
      OnGetEditText = rbwdgPredGpsGetEditText
      OnMouseDown = rbwdgPredGpsMouseDown
      OnSelectCell = rbwdgPredGpsSelectCell
      OnSetEditText = rbwdgPredGpsSetEditText
      ExtendedAutoDistributeText = False
      AutoMultiEdit = False
      AutoDistributeText = True
      AutoIncreaseColCount = False
      AutoIncreaseRowCount = False
      SelectedRowOrColumnColor = clAqua
      UnselectableColor = clBtnFace
      OnColSize = rbwdgPredGpsColSize
      OnStateChange = rbwdgPredGpsStateChange
      ColorRangeSelection = True
      Columns = <
        item
          AutoAdjustRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4String
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          AutoAdjustColWidths = False
        end
        item
          AutoAdjustRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4String
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          AutoAdjustColWidths = False
        end
        item
          AutoAdjustRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4String
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          AutoAdjustColWidths = False
        end
        item
          AutoAdjustRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4String
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          AutoAdjustColWidths = False
        end
        item
          AutoAdjustRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4String
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          AutoAdjustColWidths = False
        end>
      OnEndUpdate = rbwdgPredGpsEndUpdate
    end
    object btnRenameGp: TButton
      Left = 605
      Top = 8
      Width = 141
      Height = 30
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Rename Group...'
      TabOrder = 2
      OnClick = btnRenameGpClick
    end
  end
  object pnlUpper: TPanel
    Left = 0
    Top = 0
    Width = 941
    Height = 479
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    Constraints.MinHeight = 95
    TabOrder = 0
    DesignSize = (
      941
      479)
    object lblPredTable: TLabel
      Left = 5
      Top = 10
      Width = 151
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Predictions Table'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -18
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnAddPred: TButton
      Left = 187
      Top = 5
      Width = 143
      Height = 29
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Add Prediction...'
      TabOrder = 0
      OnClick = btnAddPredClick
    end
    object btnDeleteSelectedPredictions: TButton
      Left = 338
      Top = 5
      Width = 230
      Height = 29
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Delete Selected Prediction(s)'
      TabOrder = 1
      OnClick = btnDeleteSelectedPredictionsClick
    end
    object btnConfigure: TButton
      Left = 576
      Top = 5
      Width = 164
      Height = 29
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Configure Tables...'
      TabOrder = 2
      OnClick = btnConfigureClick
    end
    object rbwdgPred: TRbwDataGrid4
      Left = 1
      Top = 38
      Width = 937
      Height = 438
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabStop = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      DefaultColWidth = 50
      FixedCols = 1
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goEditing, goAlwaysShowEditor]
      TabOrder = 3
      OnExit = rbwdgPredExit
      OnMouseDown = rbwdgPredMouseDown
      OnMouseUp = rbwdgPredMouseUp
      OnSelectCell = rbwdgPredSelectCell
      ExtendedAutoDistributeText = False
      AutoMultiEdit = False
      AutoDistributeText = True
      AutoIncreaseColCount = False
      AutoIncreaseRowCount = True
      SelectedRowOrColumnColor = clAqua
      UnselectableColor = clBtnFace
      OnStateChange = rbwdgPredStateChange
      ColorRangeSelection = True
      Columns = <
        item
          AutoAdjustRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4String
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          AutoAdjustColWidths = False
        end
        item
          AutoAdjustRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4String
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          AutoAdjustColWidths = False
        end
        item
          AutoAdjustRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4String
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          AutoAdjustColWidths = False
        end
        item
          AutoAdjustRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4String
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          AutoAdjustColWidths = False
        end
        item
          AutoAdjustRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4String
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          AutoAdjustColWidths = False
        end>
    end
  end
end
