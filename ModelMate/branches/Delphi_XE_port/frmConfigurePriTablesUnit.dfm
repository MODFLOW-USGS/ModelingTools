object FormConfigPriTables: TFormConfigPriTables
  Left = 0
  Top = 0
  Caption = 'Configure Prior Information Tables'
  ClientHeight = 292
  ClientWidth = 537
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
    Left = 15
    Top = 196
    Width = 488
    Height = 52
    AutoSize = False
    Caption = 'Click in right column to display explanation of attribute'
    WordWrap = True
  end
  object dgPriCols: TDataGrid
    Left = 0
    Top = 0
    Width = 537
    Height = 172
    Align = alTop
    ColCount = 3
    DefaultColWidth = 140
    DefaultRowHeight = 23
    FixedCols = 2
    RowCount = 7
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
    TabOrder = 0
    OnClick = dgPriColsClick
    OnSelectCell = dgPriColsSelectCell
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
    OnUserChanged = dgPriColsUserChanged
    SelectedIndex = 2
    Version = '2.0'
    ColWidths = (
      194
      135
      202)
  end
  object btnCancel: TBitBtn
    Left = 278
    Top = 257
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 192
    Top = 257
    Width = 75
    Height = 25
    TabOrder = 1
    OnClick = btnOKClick
    Kind = bkOK
  end
end
