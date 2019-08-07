object FormParallelRunners: TFormParallelRunners
  Left = 0
  Top = 0
  Caption = 'Parallel-Processing Runner Directories'
  ClientHeight = 407
  ClientWidth = 792
  Color = clBtnFace
  Constraints.MinWidth = 720
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 280
    Width = 792
    Height = 127
    Align = alBottom
    TabOrder = 1
    object btnMoveRowUp: TSpeedButton
      Left = 461
      Top = 18
      Width = 130
      Height = 27
      Caption = 'Move Runner Up'
      OnClick = btnMoveRowUpClick
    end
    object btnMoveRowDown: TSpeedButton
      Left = 461
      Top = 50
      Width = 130
      Height = 27
      Caption = 'Move Runner Down'
      OnClick = btnMoveRowDownClick
    end
    object Label1: TLabel
      Left = 40
      Top = 14
      Width = 196
      Height = 16
      Caption = 'Use Down Arrow key to extend list'
    end
    object btnCancel: TBitBtn
      Left = 375
      Top = 96
      Width = 75
      Height = 25
      TabOrder = 3
      Kind = bkCancel
    end
    object btnOK: TBitBtn
      Left = 294
      Top = 96
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
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
    object btnDeleteRunner: TBitBtn
      Left = 197
      Top = 43
      Width = 154
      Height = 34
      Caption = 'Delete Selected Runner'
      TabOrder = 0
      OnClick = btnDeleteRunnerClick
    end
    object btnSortByRunTime: TButton
      Left = 357
      Top = 18
      Width = 98
      Height = 60
      Caption = 'Sort By Expected Run Time'
      TabOrder = 1
      WordWrap = True
      OnClick = btnSortByRunTimeClick
    end
  end
  object dgRunners: TRbwDataGrid4
    Left = 0
    Top = 0
    Width = 792
    Height = 280
    TabStop = False
    Align = alClient
    ColCount = 4
    DefaultColWidth = 100
    FixedCols = 0
    RowCount = 3
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowMoving, goEditing, goAlwaysShowEditor]
    TabOrder = 0
    OnKeyDown = dgRunnersKeyDown
    AutoDistributeText = False
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = False
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    OnButtonClick = dgRunnersButtonClick
    OnColSize = dgRunnersColSize
    ColorRangeSelection = False
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
        Format = rcf4Boolean
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
        ButtonUsed = True
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
        CheckMin = True
        ComboUsed = False
        Format = rcf4Real
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        AutoAdjustColWidths = False
      end>
  end
  object bffDialog: TJvBrowseForFolderDialog
    Left = 728
    Top = 288
  end
end
