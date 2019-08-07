object FormDerivedParameters: TFormDerivedParameters
  Left = 0
  Top = 0
  Caption = 'UCODE Derived Parameters'
  ClientHeight = 366
  ClientWidth = 742
  Color = clBtnFace
  Constraints.MinHeight = 154
  Constraints.MinWidth = 470
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    742
    366)
  PixelsPerInch = 96
  TextHeight = 16
  object dgDerParams: TRbwDataGrid4
    Left = 0
    Top = 0
    Width = 742
    Height = 326
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 3
    FixedCols = 1
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
    TabOrder = 0
    AutoDistributeText = False
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = False
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    OnButtonClick = dgDerParamsButtonClick
    ColorRangeSelection = False
    ColorSelectedRow = True
    Columns = <
      item
        AutoAdjustRowHeights = False
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -12
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
        Format = rcf4Boolean
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
        AutoAdjustColWidths = True
      end>
    ExplicitHeight = 324
    ColWidths = (
      107
      60
      546)
  end
  object btnCancel: TBitBtn
    Left = 381
    Top = 332
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    Kind = bkCancel
    ExplicitTop = 330
  end
  object btnOK: TBitBtn
    Left = 296
    Top = 333
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    OnClick = btnOKClick
    Kind = bkOK
    ExplicitTop = 331
  end
end
