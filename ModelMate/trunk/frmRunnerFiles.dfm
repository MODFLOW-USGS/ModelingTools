object FormRunnerFiles: TFormRunnerFiles
  Left = 0
  Top = 0
  Caption = 'Files To Be Copied To Runner Directories'
  ClientHeight = 432
  ClientWidth = 703
  Color = clBtnFace
  Constraints.MinHeight = 195
  Constraints.MinWidth = 454
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
  DesignSize = (
    703
    432)
  PixelsPerInch = 120
  TextHeight = 18
  object dgRunnerFiles: TRbwDataGrid4
    Left = 0
    Top = 0
    Width = 703
    Height = 347
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabStop = False
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 1
    DefaultColWidth = 588
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    TabOrder = 2
    OnKeyDown = dgRunnerFilesKeyDown
    ExtendedAutoDistributeText = False
    AutoMultiEdit = False
    AutoDistributeText = False
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = True
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    OnButtonClick = dgRunnerFilesButtonClick
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
        CaseSensitivePicklist = False
        AutoAdjustColWidths = False
      end>
  end
  object btnCancel: TBitBtn
    Left = 353
    Top = 392
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
    TabOrder = 1
  end
  object btnOK: TBitBtn
    Left = 257
    Top = 392
    Width = 89
    Height = 30
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnAddRow: TButton
    Left = 40
    Top = 361
    Width = 129
    Height = 30
    Caption = 'Add a Row'
    TabOrder = 3
    OnClick = btnAddRowClick
  end
  object odRunnerFile: TJvOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Height = 454
    Width = 563
    Left = 480
    Top = 360
  end
  object OpenDialog1: TOpenDialog
    Left = 544
    Top = 384
  end
end
