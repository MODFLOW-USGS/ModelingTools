object FormPriorInfo: TFormPriorInfo
  Left = 0
  Top = 0
  Caption = 'UCODE Linear Prior Information'
  ClientHeight = 566
  ClientWidth = 792
  Color = clBtnFace
  Constraints.MinHeight = 316
  Constraints.MinWidth = 720
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
    ExplicitTop = 421
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 547
    Width = 792
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    OnClick = StatusBar1Click
  end
  object pnlUpper: TPanel
    Left = 0
    Top = 0
    Width = 792
    Height = 396
    Align = alClient
    Constraints.MinHeight = 80
    TabOrder = 0
    DesignSize = (
      792
      396)
    object lblPriTable: TLabel
      Left = 4
      Top = 9
      Width = 149
      Height = 16
      Caption = 'Prior Information Table'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnAddPri: TButton
      Left = 164
      Top = 5
      Width = 178
      Height = 25
      Caption = 'Add Prior Information Item...'
      TabOrder = 0
      OnClick = btnAddPriClick
    end
    object btnDeleteSelectedPrior: TButton
      Left = 348
      Top = 5
      Width = 230
      Height = 25
      Caption = 'Delete Selected Prior-Information Item'
      TabOrder = 1
      OnClick = btnDeleteSelectedPriorClick
    end
    object btnConfigure: TButton
      Left = 584
      Top = 5
      Width = 124
      Height = 25
      Caption = 'Configure Tables...'
      TabOrder = 2
      OnClick = btnConfigureClick
    end
    object rbwdgPri: TRbwDataGrid4
      Left = 1
      Top = 32
      Width = 789
      Height = 363
      TabStop = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      DefaultColWidth = 50
      FixedCols = 1
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goEditing, goAlwaysShowEditor]
      TabOrder = 3
      OnExit = rbwdgPriExit
      OnMouseDown = rbwdgPriMouseDown
      OnSelectCell = rbwdgPriSelectCell
      AutoDistributeText = True
      AutoIncreaseColCount = False
      AutoIncreaseRowCount = False
      SelectedRowOrColumnColor = clAqua
      UnselectableColor = clBtnFace
      OnStateChange = rbwdgPriStateChange
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
    end
  end
  object pnlLower: TPanel
    Left = 0
    Top = 401
    Width = 792
    Height = 146
    Align = alBottom
    Constraints.MinHeight = 110
    TabOrder = 1
    DesignSize = (
      792
      146)
    object lblPriGpsTable: TLabel
      Left = 4
      Top = 11
      Width = 198
      Height = 16
      Caption = 'Prior Information Groups Table'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnAddPriGp: TButton
      Left = 210
      Top = 7
      Width = 89
      Height = 25
      Caption = 'Add Group...'
      TabOrder = 0
      OnClick = btnAddPriGpClick
    end
    object btnDeleteSelPriGps: TButton
      Left = 305
      Top = 7
      Width = 148
      Height = 25
      Caption = 'Delete Seleted Group(s)'
      TabOrder = 1
      OnClick = btnDeleteSelPriGpsClick
    end
    object btnCancel: TBitBtn
      Left = 400
      Top = 117
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      TabOrder = 4
      OnClick = btnCancelClick
      Kind = bkCancel
    end
    object btnOK: TBitBtn
      Left = 319
      Top = 117
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'OK'
      Default = True
      TabOrder = 3
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
    object rbwdgPriGps: TRbwDataGrid4
      Left = 0
      Top = 34
      Width = 790
      Height = 79
      TabStop = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      DefaultColWidth = 50
      FixedCols = 1
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goEditing, goAlwaysShowEditor]
      TabOrder = 5
      OnExit = rbwdgPriGpsExit
      OnGetEditText = rbwdgPriGpsGetEditText
      OnMouseDown = rbwdgPriGpsMouseDown
      OnSelectCell = rbwdgPriGpsSelectCell
      OnSetEditText = rbwdgPriGpsSetEditText
      AutoDistributeText = True
      AutoIncreaseColCount = False
      AutoIncreaseRowCount = False
      SelectedRowOrColumnColor = clAqua
      UnselectableColor = clBtnFace
      OnColSize = rbwdgPriGpsColSize
      OnStateChange = rbwdgPriGpsStateChange
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
      OnEndUpdate = rbwdgPriGpsEndUpdate
    end
    object btnRenameGp: TButton
      Left = 459
      Top = 7
      Width = 106
      Height = 25
      Caption = 'Rename Group...'
      TabOrder = 2
      OnClick = btnRenameGpClick
    end
  end
end
