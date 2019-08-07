object FormConfigureParTables: TFormConfigureParTables
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Configure Parameter Tables'
  ClientHeight = 418
  ClientWidth = 570
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
  DesignSize = (
    570
    418)
  PixelsPerInch = 96
  TextHeight = 16
  object lblStatus: TLabel
    Left = 6
    Top = 296
    Width = 558
    Height = 56
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Click in right column to display explanation of attribute'
    WordWrap = True
  end
  object dgParCols: TDataGrid
    Left = 0
    Top = 0
    Width = 570
    Height = 268
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
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ReadOnly = True
        Title.Caption = '  Attribute'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clBlue
        Title.Font.Height = -11
        Title.Font.Name = 'Tahoma'
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
        Title.Font.Height = -11
        Title.Font.Name = 'Tahoma'
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
        Title.Font.Height = -11
        Title.Font.Name = 'Tahoma'
        Title.Font.Style = [fsBold]
        Title.WordWrap = False
      end>
    RowCountMin = 0
    OnUserChanged = dgParColsUserChanged
    SelectedIndex = 2
    Version = '2.0'
    ColWidths = (
      146
      190
      256)
  end
  object btnCancel: TBitBtn
    Left = 294
    Top = 387
    Width = 75
    Height = 25
    TabOrder = 3
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 212
    Top = 387
    Width = 75
    Height = 25
    TabOrder = 2
    OnClick = btnOKClick
    Kind = bkOK
  end
  object cbFreezeParNames: TCheckBox
    Left = 16
    Top = 366
    Width = 169
    Height = 17
    Caption = 'Freeze Parameter Names'
    TabOrder = 1
  end
end
