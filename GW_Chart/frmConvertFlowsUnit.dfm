object frmConvertFlows: TfrmConvertFlows
  Left = 399
  Top = 199
  Width = 498
  Height = 485
  HelpContext = 640
  Caption = 'Convert Flows or Zeta Files'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object spl1: TSplitter
    Left = 266
    Top = 72
    Height = 313
    Align = alRight
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 482
    Height = 72
    Align = alTop
    TabOrder = 0
    DesignSize = (
      482
      72)
    object lblFlowFile: TLabel
      Left = 8
      Top = 16
      Width = 78
      Height = 13
      Caption = 'Flow or Zeta File'
    end
    object feFlowFile: TJvFilenameEdit
      Left = 8
      Top = 32
      Width = 465
      Height = 21
      Filter = 
        'Flow Files (*.cbc, *.bud, *.out)|*.cbc;*.bud;*.out|Zeta files (*' +
        '.zta)|*.zta|All files (*.*)|*.*'
      DialogOptions = [ofHideReadOnly, ofAllowMultiSelect]
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = feFlowFileChange
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 385
    Width = 482
    Height = 41
    Align = alBottom
    TabOrder = 3
    object btnConvert: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Convert'
      Enabled = False
      TabOrder = 0
      OnClick = btnConvertClick
    end
    object pbProgress: TProgressBar
      Left = 104
      Top = 8
      Width = 369
      Height = 17
      TabOrder = 1
    end
  end
  object rdgExportFiles: TRbwDataGrid4
    Left = 0
    Top = 72
    Width = 266
    Height = 313
    Align = alClient
    ColCount = 2
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
    TabOrder = 1
    OnSelectCell = rdgExportFilesSelectCell
    ExtendedAutoDistributeText = False
    AutoMultiEdit = False
    AutoDistributeText = False
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = False
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    OnBeforeDrawCell = rdgExportFilesBeforeDrawCell
    OnStateChange = rdgExportFilesStateChange
    ColorRangeSelection = False
    Columns = <
      item
        AutoAdjustRowHeights = False
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
        Format = rcf4Boolean
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
        ButtonCaption = 'Browse'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -11
        ButtonFont.Name = 'MS Sans Serif'
        ButtonFont.Style = []
        ButtonUsed = False
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
  end
  object pnlCellsToExport: TPanel
    Left = 269
    Top = 72
    Width = 213
    Height = 313
    Align = alRight
    TabOrder = 2
    object lblCellCount: TLabel
      Left = 56
      Top = 64
      Width = 117
      Height = 13
      Caption = 'Number of cells to export'
    end
    object rdgCellsToExport: TRbwDataGrid4
      Left = 1
      Top = 107
      Width = 211
      Height = 205
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      ColCount = 3
      Enabled = False
      FixedCols = 0
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
      TabOrder = 2
      ExtendedAutoDistributeText = False
      AutoMultiEdit = False
      AutoDistributeText = True
      AutoIncreaseColCount = False
      AutoIncreaseRowCount = True
      SelectedRowOrColumnColor = clAqua
      UnselectableColor = clBtnFace
      ColorRangeSelection = False
      Columns = <
        item
          AutoAdjustRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'MS Sans Serif'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = True
          ComboUsed = False
          Format = rcf4Integer
          LimitToList = False
          Max = 1.000000000000000000
          MaxLength = 0
          Min = 1.000000000000000000
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end
        item
          AutoAdjustRowHeights = False
          ButtonCaption = 'Browse'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'MS Sans Serif'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 60
          CheckMax = False
          CheckMin = True
          ComboUsed = False
          Format = rcf4Integer
          LimitToList = False
          Max = 1.000000000000000000
          MaxLength = 0
          Min = 1.000000000000000000
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
          ButtonFont.Name = 'MS Sans Serif'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = True
          ComboUsed = False
          Format = rcf4Integer
          LimitToList = False
          Max = 1.000000000000000000
          MaxLength = 0
          Min = 1.000000000000000000
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = False
        end>
      OnEndUpdate = rdgCellsToExportEndUpdate
      WordWrapRowCaptions = False
    end
    object rgCellsToExport: TRadioGroup
      Left = 1
      Top = 1
      Width = 211
      Height = 56
      Align = alTop
      Caption = 'Cells to Export'
      ItemIndex = 0
      Items.Strings = (
        'All cells'
        'Selected cells')
      TabOrder = 0
      OnClick = rgCellsToExportClick
    end
    object spinCellCount: TJvSpinEdit
      Left = 8
      Top = 60
      Width = 41
      Height = 21
      CheckMaxValue = False
      MinValue = 1.000000000000000000
      Value = 1.000000000000000000
      Enabled = False
      TabOrder = 1
      OnChange = spinCellCountChange
    end
  end
  object MainMenu1: TMainMenu
    Left = 200
    Top = 136
    object File1: TMenuItem
      Caption = 'File'
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      GroupIndex = 2
      object Help2: TMenuItem
        Caption = 'Help'
        OnClick = Help2Click
      end
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
    end
  end
end
