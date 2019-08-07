object FormObservations: TFormObservations
  Left = 0
  Top = 0
  Caption = 'Observations'
  ClientHeight = 566
  ClientWidth = 792
  Color = clBtnFace
  Constraints.MinHeight = 314
  Constraints.MinWidth = 610
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object JvNetscapeSplitter1: TJvNetscapeSplitter
    Left = 0
    Top = 396
    Width = 792
    Height = 5
    Cursor = crVSplit
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
    ExplicitTop = 405
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 547
    Width = 792
    Height = 19
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
    Width = 792
    Height = 396
    Align = alClient
    Constraints.MinHeight = 80
    TabOrder = 1
    DesignSize = (
      792
      396)
    object lblObsTable: TLabel
      Left = 4
      Top = 8
      Width = 124
      Height = 16
      Caption = 'Observations Table'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnAddObs: TButton
      Left = 143
      Top = 4
      Width = 118
      Height = 25
      Caption = 'Add Observation...'
      TabOrder = 0
      OnClick = btnAddObsClick
    end
    object btnDeleteSelectedObservations: TButton
      Left = 267
      Top = 4
      Width = 185
      Height = 25
      Caption = 'Delete Selected Observation(s)'
      TabOrder = 1
      OnClick = btnDeleteSelectedObservationsClick
    end
    object rbwdgObs: TRbwDataGrid4
      Left = 0
      Top = 32
      Width = 792
      Height = 365
      TabStop = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      DefaultColWidth = 50
      FixedCols = 1
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goEditing, goAlwaysShowEditor]
      TabOrder = 3
      OnExit = rbwdgObsExit
      OnMouseDown = rbwdgObsMouseDown
      OnSelectCell = rbwdgObsSelectCell
      AutoDistributeText = True
      AutoIncreaseColCount = False
      AutoIncreaseRowCount = False
      SelectedRowOrColumnColor = clAqua
      UnselectableColor = clBtnFace
      OnStateChange = rbwdgObsStateChange
      ColorRangeSelection = True
      ColorSelectedRow = True
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
          AutoAdjustColWidths = False
        end>
    end
    object btnConfigure: TButton
      Left = 458
      Top = 4
      Width = 123
      Height = 25
      Caption = 'Configure Tables...'
      TabOrder = 2
      OnClick = btnConfigureClick
    end
  end
  object pnlLower: TPanel
    Left = 0
    Top = 401
    Width = 792
    Height = 146
    Align = alBottom
    Constraints.MinHeight = 110
    TabOrder = 2
    DesignSize = (
      792
      146)
    object lblObsGpsTable: TLabel
      Left = 4
      Top = 8
      Width = 166
      Height = 16
      Caption = 'Observation Groups Table'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnDeleteSelObsGps: TButton
      Left = 304
      Top = 4
      Width = 168
      Height = 25
      Caption = 'Delete Selected Group(s)'
      TabOrder = 2
      OnClick = btnDeleteSelObsGpsClick
    end
    object btnAddObsGp: TButton
      Left = 184
      Top = 4
      Width = 114
      Height = 25
      Caption = 'Add Group...'
      TabOrder = 1
      OnClick = btnAddObsGpClick
    end
    object btnCancel: TBitBtn
      Left = 404
      Top = 117
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      TabOrder = 5
      OnClick = btnCancelClick
      Kind = bkCancel
    end
    object btnOK: TBitBtn
      Left = 323
      Top = 117
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'OK'
      Default = True
      TabOrder = 4
      OnClick = btnOKClick
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
    end
    object rbwdgObsGps: TRbwDataGrid4
      Left = 1
      Top = 32
      Width = 789
      Height = 79
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
      AutoDistributeText = True
      AutoIncreaseColCount = False
      AutoIncreaseRowCount = False
      SelectedRowOrColumnColor = clAqua
      UnselectableColor = clBtnFace
      OnColSize = rbwdgObsGpsColSize
      OnStateChange = rbwdgObsGpsStateChange
      ColorRangeSelection = True
      ColorSelectedRow = True
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
          AutoAdjustColWidths = False
        end>
      OnEndUpdate = rbwdgObsGpsEndUpdate
    end
    object btnRenameGp: TButton
      Left = 478
      Top = 4
      Width = 119
      Height = 25
      Caption = 'Rename Group...'
      TabOrder = 3
      OnClick = btnRenameGpClick
    end
  end
  object JvProgressDialog1: TJvProgressDialog
    ScreenPosition = poDesktopCenter
    Left = 654
    Top = 10
  end
end
