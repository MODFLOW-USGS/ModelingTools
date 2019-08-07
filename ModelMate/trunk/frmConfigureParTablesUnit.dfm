object FormConfigureParTables: TFormConfigureParTables
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Configure Parameter Tables'
  ClientHeight = 496
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
  DesignSize = (
    794
    496)
  PixelsPerInch = 120
  TextHeight = 18
  object lblStatus: TLabel
    Left = 7
    Top = 352
    Width = 780
    Height = 66
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Click in right column to display explanation of attribute'
    WordWrap = True
    ExplicitWidth = 663
  end
  object dgParCols: TEcDataGrid
    Left = 0
    Top = 0
    Width = 794
    Height = 318
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    ColCount = 3
    DefaultColWidth = 100
    DefaultRowHeight = 23
    FixedCols = 2
    RowCount = 11
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goColMoving, goEditing, goAlwaysShowEditor, goThumbTracking]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnClick = dgParColsClick
    OnSelectCell = dgParColsSelectCell
    Columns = <
      item
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ReadOnly = True
        Title.Caption = '  Attribute'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clBlue
        Title.Font.Height = -17
        Title.Font.Name = 'Arial'
        Title.Font.Style = [fsBold]
        Title.WordWrap = False
      end
      item
        MaxLength = 22
        LimitToList = True
        DropDownRows = 3
        PickList.Strings = (
          'Parameter Groups Table'
          'Parameters Table'
          'No Table (use default)')
        ReadOnly = True
        Title.Caption = '  Default'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clBlue
        Title.Font.Height = -17
        Title.Font.Name = 'Arial'
        Title.Font.Style = [fsBold]
        Title.WordWrap = False
      end
      item
        MaxLength = 22
        LimitToList = True
        DropDownRows = 3
        PickList.Strings = (
          'Show in Parameters Table'
          'Hide')
        Title.Caption = '  Select Table or Default'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clBlue
        Title.Font.Height = -17
        Title.Font.Name = 'Arial'
        Title.Font.Style = [fsBold]
        Title.WordWrap = False
      end>
    RowCountMin = 0
    OnUserChanged = dgParColsUserChanged
    SelectedIndex = 2
    Version = '2.0'
    ExplicitWidth = 677
    ColWidths = (
      235
      265
      287)
  end
  object btnCancel: TBitBtn
    Left = 349
    Top = 460
    Width = 89
    Height = 29
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 3
  end
  object btnOK: TBitBtn
    Left = 252
    Top = 460
    Width = 89
    Height = 29
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 2
    OnClick = btnOKClick
  end
  object cbFreezeParNames: TCheckBox
    Left = 19
    Top = 435
    Width = 225
    Height = 20
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Freeze Parameter Names'
    TabOrder = 1
  end
end
