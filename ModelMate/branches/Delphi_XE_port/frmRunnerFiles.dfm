object FormRunnerFiles: TFormRunnerFiles
  Left = 0
  Top = 0
  Caption = 'Files To Be Copied To Runner Directories'
  ClientHeight = 361
  ClientWidth = 592
  Color = clBtnFace
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
  DesignSize = (
    592
    361)
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 23
    Top = 299
    Width = 196
    Height = 16
    Anchors = [akLeft, akBottom]
    Caption = 'Use Down Arrow key to extend list'
  end
  object dgRunnerFiles: TRbwDataGrid4
    Left = 0
    Top = 0
    Width = 592
    Height = 289
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
    AutoDistributeText = False
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = True
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    OnButtonClick = dgRunnerFilesButtonClick
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
      end>
  end
  object btnCancel: TBitBtn
    Left = 297
    Top = 327
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 216
    Top = 327
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    TabOrder = 0
    OnClick = btnOKClick
    Kind = bkOK
  end
  object odRunnerFile: TJvOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Height = 454
    Width = 563
    Left = 464
    Top = 312
  end
  object OpenDialog1: TOpenDialog
    Left = 528
    Top = 320
  end
end
