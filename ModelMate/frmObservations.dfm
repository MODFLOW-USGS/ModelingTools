object FormObservations: TFormObservations
  Left = 0
  Top = 0
  Caption = 'Observations'
  ClientHeight = 680
  ClientWidth = 941
  Color = clBtnFace
  Constraints.MinHeight = 373
  Constraints.MinWidth = 724
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
    Panels = <
      item
        Width = 5000
      end>
    ParentFont = True
    UseSystemFont = False
    OnClick = StatusBar1Click
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
    TabOrder = 1
    DesignSize = (
      941
      479)
    object lblObsTable: TLabel
      Left = 5
      Top = 10
      Width = 169
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Observations Table'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -18
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnAddObs: TButton
      Left = 185
      Top = 5
      Width = 150
      Height = 29
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Add Observation...'
      TabOrder = 0
      OnClick = btnAddObsClick
    end
    object btnDeleteSelectedObservations: TButton
      Left = 343
      Top = 5
      Width = 235
      Height = 29
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Delete Selected Observation(s)'
      TabOrder = 1
      OnClick = btnDeleteSelectedObservationsClick
    end
    object rbwdgObs: TRbwDataGrid4
      Left = 0
      Top = 38
      Width = 941
      Height = 442
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
      OnExit = rbwdgObsExit
      OnMouseDown = rbwdgObsMouseDown
      OnMouseUp = rbwdgObsMouseUp
      OnSelectCell = rbwdgObsSelectCell
      ExtendedAutoDistributeText = False
      AutoMultiEdit = False
      AutoDistributeText = True
      AutoIncreaseColCount = False
      AutoIncreaseRowCount = True
      SelectedRowOrColumnColor = clAqua
      UnselectableColor = clBtnFace
      OnStateChange = rbwdgObsStateChange
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
          AutoAdjustColWidths = True
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
    object btnConfigure: TButton
      Left = 586
      Top = 5
      Width = 156
      Height = 29
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Configure Tables...'
      TabOrder = 2
      OnClick = btnConfigureClick
    end
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
    TabOrder = 2
    DesignSize = (
      941
      173)
    object lblObsGpsTable: TLabel
      Left = 5
      Top = 10
      Width = 228
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Observation Groups Table'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -18
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnDeleteSelObsGps: TButton
      Left = 419
      Top = 5
      Width = 206
      Height = 29
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Delete Selected Group(s)'
      TabOrder = 2
      OnClick = btnDeleteSelObsGpsClick
    end
    object btnAddObsGp: TButton
      Left = 275
      Top = 5
      Width = 136
      Height = 29
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Add Group...'
      TabOrder = 1
      OnClick = btnAddObsGpClick
    end
    object btnCancel: TBitBtn
      Left = 480
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
      TabOrder = 5
      OnClick = btnCancelClick
    end
    object btnOK: TBitBtn
      Left = 384
      Top = 139
      Width = 89
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
      TabOrder = 4
      OnClick = btnOKClick
    end
    object rbwdgObsGps: TRbwDataGrid4
      Left = 1
      Top = 38
      Width = 937
      Height = 94
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabStop = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      DefaultColWidth = 50
      FixedCols = 1
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goEditing, goAlwaysShowEditor]
      TabOrder = 0
      OnEnter = rbwdgObsGpsEnter
      OnExit = rbwdgObsGpsExit
      OnGetEditText = rbwdgObsGpsGetEditText
      OnMouseDown = rbwdgObsGpsMouseDown
      OnSelectCell = rbwdgObsGpsSelectCell
      OnSetEditText = rbwdgObsGpsSetEditText
      ExtendedAutoDistributeText = False
      AutoMultiEdit = False
      AutoDistributeText = True
      AutoIncreaseColCount = False
      AutoIncreaseRowCount = False
      SelectedRowOrColumnColor = clAqua
      UnselectableColor = clBtnFace
      OnColSize = rbwdgObsGpsColSize
      OnStateChange = rbwdgObsGpsStateChange
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
      OnEndUpdate = rbwdgObsGpsEndUpdate
    end
    object btnRenameGp: TButton
      Left = 633
      Top = 5
      Width = 142
      Height = 29
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Rename Group...'
      TabOrder = 3
      OnClick = btnRenameGpClick
    end
  end
  object JvProgressDialog1: TJvProgressDialog
    ScreenPosition = poDesktopCenter
    Left = 750
    Top = 10
  end
end
