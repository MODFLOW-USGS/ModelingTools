object FormModelIO: TFormModelIO
  Left = 0
  Top = 0
  Caption = 'Model Input and Template Files'
  ClientHeight = 251
  ClientWidth = 792
  Color = clBtnFace
  Constraints.MinHeight = 186
  Constraints.MinWidth = 490
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
    792
    251)
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 17
    Top = 181
    Width = 196
    Height = 16
    Anchors = [akLeft, akBottom]
    Caption = 'Use Down Arrow key to extend list'
    ExplicitTop = 298
  end
  object Label2: TLabel
    Left = 17
    Top = 203
    Width = 195
    Height = 16
    Anchors = [akLeft, akBottom]
    Caption = 'Use <Ctrl>-Delete to delete a row'
    ExplicitTop = 320
  end
  object Label3: TLabel
    Left = 17
    Top = 225
    Width = 189
    Height = 16
    Anchors = [akLeft, akBottom]
    Caption = 'Use <Ctrl>-Insert to insert a row'
    ExplicitTop = 342
  end
  object dgModelIO: TDataGrid
    Left = 0
    Top = 0
    Width = 792
    Height = 175
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
        Title.Caption = 'Model Input File'
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
    Left = 397
    Top = 209
    Width = 75
    Height = 32
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 316
    Top = 209
    Width = 75
    Height = 32
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOKClick
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
    NumGlyphs = 2
  end
  object odModelFile: TOpenDialog
    Left = 489
    Top = 244
  end
  object odAppFile: TOpenDialog
    DefaultExt = 'jtf'
    Filter = 'Jupiter Template Files (*.jtf)|*.jtf|All Files (*.*)|*.*'
    Left = 529
    Top = 244
  end
end
