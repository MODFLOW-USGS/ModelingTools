object FormConfigureObsTables: TFormConfigureObsTables
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Configure Observation Tables'
  ClientHeight = 401
  ClientWidth = 794
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 18
  object lblHint: TLabel
    Left = 12
    Top = 281
    Width = 589
    Height = 71
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    AutoSize = False
    Caption = 'Click in right column to display explanation of attribute'
    WordWrap = True
  end
  object dgObsCols: TEcDataGrid
    Left = 0
    Top = 0
    Width = 794
    Height = 261
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
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
        Title.Caption = '  Attribute'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clBlue
        Title.Font.Height = -17
        Title.Font.Name = 'Arial'
        Title.Font.Style = [fsBold]
        Title.WordWrap = False
      end
      item
        Title.Caption = '  Default'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clBlue
        Title.Font.Height = -17
        Title.Font.Name = 'Arial'
        Title.Font.Style = [fsBold]
        Title.WordWrap = False
      end
      item
        LimitToList = True
        Title.Caption = '  Select Table or Default'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clBlue
        Title.Font.Height = -17
        Title.Font.Name = 'Arial'
        Title.Font.Style = [fsBold]
        Title.WordWrap = False
      end>
    RowCountMin = 0
    OnUserChanged = dgObsColsUserChanged
    SelectedIndex = 2
    Version = '2.0'
    ColWidths = (
      250
      243
      294)
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
    Left = 323
    Top = 361
    Width = 89
    Height = 30
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 2
  end
  object btnOK: TBitBtn
    Left = 221
    Top = 361
    Width = 89
    Height = 30
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 1
  end
end
