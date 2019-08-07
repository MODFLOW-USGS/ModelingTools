object FormConfigureObsTables: TFormConfigureObsTables
  Left = 0
  Top = 0
  Caption = 'Configure Observation Tables'
  ClientHeight = 336
  ClientWidth = 534
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object lblHint: TLabel
    Left = 10
    Top = 237
    Width = 496
    Height = 59
    AutoSize = False
    Caption = 'Click in right column to display explanation of attribute'
    WordWrap = True
  end
  object dgObsCols: TDataGrid
    Left = 0
    Top = 0
    Width = 534
    Height = 220
    Align = alTop
    ColCount = 3
    DefaultColWidth = 140
    DefaultRowHeight = 23
    FixedCols = 2
    RowCount = 9
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSizing, goColSizing, goEditing]
    TabOrder = 0
    OnClick = dgObsColsClick
    OnSelectCell = dgObsColsSelectCell
    Columns = <
      item
        Title.Caption = 'Attribute'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clBlue
        Title.Font.Height = -12
        Title.Font.Name = 'Tahoma'
        Title.Font.Style = [fsBold]
        Title.WordWrap = False
      end
      item
        Title.Caption = 'Default'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clBlue
        Title.Font.Height = -12
        Title.Font.Name = 'Tahoma'
        Title.Font.Style = [fsBold]
        Title.WordWrap = False
      end
      item
        LimitToList = True
        Title.Caption = 'Select Table or Default'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clBlue
        Title.Font.Height = -12
        Title.Font.Name = 'Tahoma'
        Title.Font.Style = [fsBold]
        Title.WordWrap = False
      end>
    RowCountMin = 0
    OnUserChanged = dgObsColsUserChanged
    SelectedIndex = 2
    Version = '2.0'
    ColWidths = (
      212
      136
      180)
    RowHeights = (
      23
      23
      23
      23
      23
      23
      22
      23
      23)
  end
  object btnCancel: TBitBtn
    Left = 272
    Top = 304
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 186
    Top = 304
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
end
