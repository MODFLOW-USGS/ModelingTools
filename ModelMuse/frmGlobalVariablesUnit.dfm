inherited frmGlobalVariables: TfrmGlobalVariables
  HelpType = htKeyword
  HelpKeyword = 'Global_Variables_Dialog_Box'
  Caption = 'Global Variables'
  ClientHeight = 284
  ClientWidth = 530
  OnClose = FormClose
  ExplicitWidth = 546
  ExplicitHeight = 323
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 178
    Width = 530
    Height = 106
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    object Label1: TLabel
      Left = 71
      Top = 11
      Width = 185
      Height = 18
      Caption = 'Number of global variables'
    end
    object btnHelp: TBitBtn
      Left = 264
      Top = 72
      Width = 82
      Height = 27
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 4
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 352
      Top = 72
      Width = 82
      Height = 27
      Caption = 'Apply'
      Default = True
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
      ModalResult = 1
      NumGlyphs = 2
      TabOrder = 5
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 440
      Top = 72
      Width = 83
      Height = 27
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 6
    end
    object seGlobalVariableCount: TJvSpinEdit
      Left = 8
      Top = 6
      Width = 57
      Height = 26
      ButtonKind = bkClassic
      MaxValue = 2147483647.000000000000000000
      TabOrder = 0
      OnChange = seGlobalVariableCountChange
      OnEnter = seGlobalVariableCountEnter
    end
    object btnDelete: TButton
      Left = 440
      Top = 39
      Width = 83
      Height = 27
      Caption = 'Delete'
      Enabled = False
      TabOrder = 2
      OnClick = btnDeleteClick
    end
    object btnImportGlobalVariables: TButton
      Left = 8
      Top = 39
      Width = 185
      Height = 27
      Caption = 'Import global variables'
      TabOrder = 1
      OnClick = btnImportGlobalVariablesClick
    end
    object btnSaveGlobalVariables: TButton
      Left = 8
      Top = 72
      Width = 185
      Height = 27
      Caption = 'Save global variables'
      TabOrder = 3
      OnClick = btnSaveGlobalVariablesClick
    end
  end
  object rdgGlobalVariables: TRbwDataGrid4
    Left = 0
    Top = 0
    Width = 530
    Height = 178
    Align = alClient
    ColCount = 4
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs]
    TabOrder = 0
    OnMouseDown = rdgGlobalVariablesMouseDown
    OnSelectCell = rdgGlobalVariablesSelectCell
    OnSetEditText = rdgGlobalVariablesSetEditText
    ExtendedAutoDistributeText = False
    AutoMultiEdit = True
    AutoDistributeText = True
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = True
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    OnBeforeDrawCell = rdgGlobalVariablesBeforeDrawCell
    ColorRangeSelection = False
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
        CheckStyle = csCheck
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
        ComboUsed = True
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Real'
          'Integer'
          'Boolean'
          'Text')
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
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
        CheckStyle = csCheck
        AutoAdjustColWidths = True
      end
      item
        AutoAdjustRowHeights = True
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
        WordWrapCells = True
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = True
      end>
    OnEndUpdate = rdgGlobalVariablesEndUpdate
    WordWrapRowCaptions = False
    ColWidths = (
      64
      80
      64
      225)
  end
  object dlgOpenGlobVar: TOpenDialog
    DefaultExt = '.txt'
    Filter = 'Text files (*.txt)|*.txt|All Files|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 192
    Top = 112
  end
  object dlgSaveGlobalVariables: TSaveDialog
    DefaultExt = '.txt'
    Filter = 'Text files (*.txt)|*.txt|All Files|*.*'
    Left = 280
    Top = 128
  end
end
