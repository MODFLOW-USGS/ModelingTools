object FormConfigurePredTables: TFormConfigurePredTables
  Left = 0
  Top = 0
  Caption = 'Configure Prediction Tables'
  ClientHeight = 230
  ClientWidth = 515
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object lblHint: TLabel
    Left = 16
    Top = 149
    Width = 405
    Height = 36
    AutoSize = False
    Caption = 'Click in right column to display explanation of attribute'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object dgPredCols: TDataGrid
    Left = 0
    Top = 0
    Width = 515
    Height = 124
    Align = alTop
    ColCount = 3
    DefaultColWidth = 140
    DefaultRowHeight = 23
    FixedCols = 2
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
    ParentFont = False
    TabOrder = 0
    OnClick = dgPredColsClick
    OnSelectCell = dgPredColsSelectCell
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
        Title.Caption = 'Select Table or Default'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clBlue
        Title.Font.Height = -12
        Title.Font.Name = 'Tahoma'
        Title.Font.Style = [fsBold]
        Title.WordWrap = False
      end>
    RowCountMin = 0
    OnUserChanged = dgPredColsUserChanged
    SelectedIndex = 2
    Version = '2.0'
    ColWidths = (
      168
      139
      202)
  end
  object btnCancel: TBitBtn
    Left = 274
    Top = 195
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 190
    Top = 195
    Width = 75
    Height = 25
    TabOrder = 1
    OnClick = btnOKClick
    Kind = bkOK
  end
end
