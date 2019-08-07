object FormConfigurePredTables: TFormConfigurePredTables
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Configure Prediction Tables'
  ClientHeight = 298
  ClientWidth = 794
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 19
  object lblHint: TLabel
    Left = 21
    Top = 192
    Width = 620
    Height = 46
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    AutoSize = False
    Caption = 'Click in right column to display explanation of attribute'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object dgPredCols: TEcDataGrid
    Left = 0
    Top = 0
    Width = 794
    Height = 159
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    ColCount = 3
    DefaultColWidth = 140
    DefaultRowHeight = 23
    FixedCols = 2
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
    ParentFont = False
    TabOrder = 0
    OnClick = dgPredColsClick
    OnSelectCell = dgPredColsSelectCell
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
        Title.Caption = '  Select Table or Default'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clBlue
        Title.Font.Height = -17
        Title.Font.Name = 'Arial'
        Title.Font.Style = [fsBold]
        Title.WordWrap = False
      end>
    RowCountMin = 0
    OnUserChanged = dgPredColsUserChanged
    SelectedIndex = 2
    Version = '2.0'
    ExplicitWidth = 665
    ColWidths = (
      244
      259
      285)
  end
  object btnCancel: TBitBtn
    Left = 352
    Top = 251
    Width = 97
    Height = 32
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
    Left = 244
    Top = 251
    Width = 97
    Height = 32
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 1
    OnClick = btnOKClick
  end
end
