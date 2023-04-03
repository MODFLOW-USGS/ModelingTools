inherited frmImportDistributedData: TfrmImportDistributedData
  Left = 392
  Top = 183
  HelpType = htKeyword
  HelpKeyword = 'Import_Distributed_Data_by_Zone'
  Caption = 'Import Distributed Data by Zone'
  ClientHeight = 356
  ClientWidth = 468
  OnResize = FormResize
  ExplicitWidth = 480
  ExplicitHeight = 394
  PixelsPerInch = 120
  TextHeight = 18
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 468
    Height = 169
    Align = alTop
    ParentColor = True
    TabOrder = 0
    ExplicitWidth = 464
    object lblLowerX: TLabel
      Left = 8
      Top = 12
      Width = 61
      Height = 18
      Caption = 'Lower X'#39
    end
    object lblHigherX: TLabel
      Left = 256
      Top = 12
      Width = 64
      Height = 18
      Caption = 'Higher X'#39
    end
    object lblLowerY: TLabel
      Left = 8
      Top = 52
      Width = 59
      Height = 18
      Caption = 'Lower Y'#39
    end
    object lblHigherY: TLabel
      Left = 256
      Top = 52
      Width = 62
      Height = 18
      Caption = 'Higher Y'#39
    end
    object lblLowerZ: TLabel
      Left = 8
      Top = 92
      Width = 56
      Height = 18
      Caption = 'Lower Z'
    end
    object lblHigherZ: TLabel
      Left = 256
      Top = 92
      Width = 59
      Height = 18
      Caption = 'Higher Z'
    end
    object lblImportTo: TLabel
      Left = 8
      Top = 132
      Width = 64
      Height = 18
      Caption = 'Import to '
    end
    object rdeLowerX: TRbwDataEntry
      Left = 104
      Top = 8
      Width = 101
      Height = 28
      Cursor = crIBeam
      TabOrder = 0
      Text = '0'
      DataType = dtReal
      Max = 1.000000000000000000
      ChangeDisabledColor = True
    end
    object rdeHigherX: TRbwDataEntry
      Left = 352
      Top = 8
      Width = 101
      Height = 28
      Cursor = crIBeam
      TabOrder = 1
      Text = '0'
      DataType = dtReal
      Max = 1.000000000000000000
      ChangeDisabledColor = True
    end
    object rdeLowerY: TRbwDataEntry
      Left = 104
      Top = 48
      Width = 101
      Height = 28
      Cursor = crIBeam
      TabOrder = 2
      Text = '0'
      DataType = dtReal
      Max = 1.000000000000000000
      ChangeDisabledColor = True
    end
    object rdeHigherY: TRbwDataEntry
      Left = 352
      Top = 48
      Width = 101
      Height = 28
      Cursor = crIBeam
      TabOrder = 3
      Text = '0'
      DataType = dtReal
      Max = 1.000000000000000000
      ChangeDisabledColor = True
    end
    object rdeLowerZ: TRbwDataEntry
      Left = 104
      Top = 88
      Width = 101
      Height = 28
      Cursor = crIBeam
      TabOrder = 4
      Text = '0'
      DataType = dtReal
      Max = 1.000000000000000000
      ChangeDisabledColor = True
    end
    object rdeHigherZ: TRbwDataEntry
      Left = 352
      Top = 88
      Width = 101
      Height = 28
      Cursor = crIBeam
      TabOrder = 5
      Text = '0'
      DataType = dtReal
      Max = 1.000000000000000000
      ChangeDisabledColor = True
    end
    object comboViewDirection: TComboBox
      Left = 104
      Top = 128
      Width = 101
      Height = 26
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 6
      Text = 'Top'
      OnChange = comboViewDirectionChange
      Items.Strings = (
        'Top'
        'Front'
        'Side')
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 315
    Width = 468
    Height = 41
    Align = alBottom
    ParentColor = True
    TabOrder = 2
    ExplicitTop = 314
    ExplicitWidth = 464
    DesignSize = (
      468
      41)
    object btnOK: TBitBtn
      Left = 261
      Top = 4
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      Enabled = False
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        04000000000068010000120B0000120B00001000000010000000000000000000
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
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 257
    end
    object btnCancel: TBitBtn
      Left = 357
      Top = 4
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 353
    end
    object btnHelp: TBitBtn
      Left = 165
      Top = 4
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 161
    end
  end
  object dgDataSets: TRbwDataGrid4
    Left = 0
    Top = 169
    Width = 468
    Height = 146
    Align = alClient
    ColCount = 2
    DefaultColWidth = 100
    FixedCols = 1
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
    TabOrder = 1
    ExtendedAutoDistributeText = False
    AutoMultiEdit = True
    AutoDistributeText = False
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = False
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    OnButtonClick = dgDataSetsButtonClicked
    ColorRangeSelection = False
    Columns = <
      item
        AutoAdjustRowHeights = False
        AutoAdjustCaptionRowHeights = False
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -11
        ButtonFont.Name = 'MS Sans Serif'
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
        AutoAdjustCaptionRowHeights = False
        ButtonCaption = 'Browse'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clBlack
        ButtonFont.Height = 17
        ButtonFont.Name = 'Microsoft Sans Serif'
        ButtonFont.Pitch = fpVariable
        ButtonFont.Style = []
        ButtonUsed = True
        ButtonWidth = 60
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
      end>
    WordWrapRowCaptions = False
    ExplicitWidth = 464
    ExplicitHeight = 145
    ColWidths = (
      100
      100)
    RowHeights = (
      24
      24
      24
      24
      24)
  end
  object OpenDialogFile: TOpenDialog
    Filter = 'Dat files (*.dat)|*.dat|All fIles (*.*)|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a file containing zone values'
    Left = 232
    Top = 40
  end
end
