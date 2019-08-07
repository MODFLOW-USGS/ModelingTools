object FormModelIO: TFormModelIO
  Left = 0
  Top = 0
  Caption = 'Model-Input and Template Files'
  ClientHeight = 298
  ClientWidth = 941
  Color = clBtnFace
  Constraints.MinHeight = 221
  Constraints.MinWidth = 582
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
    941
    298)
  PixelsPerInch = 120
  TextHeight = 18
  object Label1: TLabel
    Left = 20
    Top = 215
    Width = 234
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    Caption = 'Use Down Arrow key to extend list'
  end
  object Label2: TLabel
    Left = 20
    Top = 241
    Width = 231
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    Caption = 'Use <Ctrl>-Delete to delete a row'
  end
  object Label3: TLabel
    Left = 20
    Top = 267
    Width = 217
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    Caption = 'Use <Ctrl>-Insert to insert a row'
  end
  object dgModelIO: TEcDataGrid
    Left = 0
    Top = 0
    Width = 941
    Height = 208
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 2
    DefaultRowHeight = 23
    FixedCols = 0
    RowCount = 6
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
    TabOrder = 0
    Columns = <
      item
        MaxLength = 2000
        ButtonStyle = cbsEllipsis
        Title.Caption = 'Model-Input File'
        Title.WordWrap = False
      end
      item
        MaxLength = 2000
        ButtonStyle = cbsEllipsis
        Title.Caption = 'Template File'
        Title.WordWrap = False
      end>
    DataGridOptions = [dgoAppendRow, dgoInsertRow, dgoDeleteRow]
    RowCountMin = 2
    OnEditButtonClick = dgModelIOEditButtonClick
    OnUserChanged = dgModelIOUserChanged
    SelectedIndex = 0
    Version = '2.0'
    ColWidths = (
      377
      393)
  end
  object btnCancel: TBitBtn
    Left = 471
    Top = 248
    Width = 90
    Height = 38
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
    Left = 375
    Top = 248
    Width = 89
    Height = 38
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    DoubleBuffered = True
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    ModalResult = 1
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 2
    OnClick = btnOKClick
  end
  object odModelFile: TOpenDialog
    Left = 649
    Top = 220
  end
  object odAppFile: TOpenDialog
    DefaultExt = 'jtf'
    Filter = 'Jupiter Template Files (*.jtf)|*.jtf|All Files (*.*)|*.*'
    Left = 745
    Top = 220
  end
end
