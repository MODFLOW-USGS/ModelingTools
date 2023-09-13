inherited frmSubdivide: TfrmSubdivide
  Left = 386
  Top = 582
  HelpType = htKeyword
  HelpKeyword = 'Subdivide_Columns_Rows_and_Layers'
  ActiveControl = btnOK
  Caption = 'Subdivide Columns, Rows, and Layers'
  ClientHeight = 233
  ClientWidth = 724
  Font.Height = 21
  Font.Name = 'adobe-helvetica'
  OnClose = FormClose
  ExplicitWidth = 736
  ExplicitHeight = 271
  TextHeight = 21
  object btnCancel: TBitBtn
    Left = 622
    Top = 192
    Width = 91
    Height = 33
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 5
  end
  object btnOK: TBitBtn
    Left = 525
    Top = 192
    Width = 91
    Height = 33
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 4
    OnClick = btnOKClick
  end
  object btnHelp: TBitBtn
    Left = 429
    Top = 192
    Width = 91
    Height = 33
    HelpType = htKeyword
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 3
    OnClick = btnHelpClick
  end
  object GroupBox1: TGroupBox
    Left = 7
    Top = 8
    Width = 233
    Height = 178
    Caption = 'Columns'
    TabOrder = 0
    object lblFromCol: TLabel
      Left = 16
      Top = 27
      Width = 108
      Height = 21
      Caption = 'From &column '
      FocusControl = seCol1
    end
    object lblThroughCol: TLabel
      Left = 16
      Top = 67
      Width = 123
      Height = 21
      Caption = 'through column'
    end
    object lblSubdivideCol: TLabel
      Left = 16
      Top = 101
      Width = 120
      Height = 42
      Caption = 'subdivide each column into'
      WordWrap = True
    end
    object lblColumns: TLabel
      Left = 16
      Top = 149
      Width = 66
      Height = 21
      Caption = 'columns'
    end
    object seCol1: TJvSpinEdit
      Left = 159
      Top = 24
      Width = 65
      Height = 29
      ButtonKind = bkClassic
      MinValue = 1.000000000000000000
      Value = 1.000000000000000000
      Color = clWhite
      TabOrder = 0
      OnChange = seCol1Changed
    end
    object seCol2: TJvSpinEdit
      Left = 159
      Top = 64
      Width = 65
      Height = 29
      ButtonKind = bkClassic
      MinValue = 1.000000000000000000
      Value = 1.000000000000000000
      Color = clWhite
      TabOrder = 1
      OnChange = seCol2Changed
    end
    object seColCount: TJvSpinEdit
      Left = 159
      Top = 107
      Width = 65
      Height = 29
      ButtonKind = bkClassic
      MaxValue = 100.000000000000000000
      MinValue = 1.000000000000000000
      Value = 1.000000000000000000
      Color = clWhite
      TabOrder = 2
      OnChange = seColCountChange
    end
  end
  object GroupBox2: TGroupBox
    Left = 246
    Top = 8
    Width = 232
    Height = 178
    Caption = 'Rows'
    TabOrder = 1
    object lblFromRow: TLabel
      Left = 16
      Top = 27
      Width = 75
      Height = 21
      Caption = 'From &row'
      FocusControl = seRow1
    end
    object lblRows: TLabel
      Left = 16
      Top = 151
      Width = 38
      Height = 21
      Caption = 'rows'
    end
    object lblSubdivideRow: TLabel
      Left = 13
      Top = 101
      Width = 120
      Height = 42
      Caption = 'subdivide each row into'
      WordWrap = True
    end
    object lblThroughRow: TLabel
      Left = 16
      Top = 67
      Width = 95
      Height = 21
      Caption = 'through row'
    end
    object seRow1: TJvSpinEdit
      Left = 158
      Top = 24
      Width = 65
      Height = 29
      ButtonKind = bkClassic
      MinValue = 1.000000000000000000
      Color = clWhite
      TabOrder = 0
      OnChange = seRow1Changed
    end
    object seRow2: TJvSpinEdit
      Left = 158
      Top = 64
      Width = 65
      Height = 29
      ButtonKind = bkClassic
      MinValue = 1.000000000000000000
      Color = clWhite
      TabOrder = 1
      OnChange = seRow2Changed
    end
    object seRowCount: TJvSpinEdit
      Left = 157
      Top = 107
      Width = 65
      Height = 29
      ButtonKind = bkClassic
      MaxValue = 100.000000000000000000
      MinValue = 1.000000000000000000
      Value = 1.000000000000000000
      Color = clWhite
      TabOrder = 2
      OnChange = seRowCountChange
    end
  end
  object GroupBox3: TGroupBox
    Left = 484
    Top = 8
    Width = 232
    Height = 178
    Caption = 'Layers'
    TabOrder = 2
    object lblFromLayer: TLabel
      Left = 16
      Top = 27
      Width = 85
      Height = 21
      Caption = 'From &layer'
      FocusControl = seLayer1
    end
    object lblThroughLayer: TLabel
      Left = 16
      Top = 67
      Width = 105
      Height = 21
      Caption = 'through layer'
    end
    object lblSubdivideLayer: TLabel
      Left = 16
      Top = 101
      Width = 120
      Height = 42
      Caption = 'subdivide each layer into'
      WordWrap = True
    end
    object lblLayers: TLabel
      Left = 16
      Top = 151
      Width = 48
      Height = 21
      Caption = 'layers'
    end
    object seLayer1: TJvSpinEdit
      Left = 157
      Top = 24
      Width = 65
      Height = 29
      ButtonKind = bkClassic
      MinValue = 1.000000000000000000
      Value = 1.000000000000000000
      Color = clWhite
      TabOrder = 0
      OnChange = seLayer1Changed
    end
    object seLayer2: TJvSpinEdit
      Left = 157
      Top = 63
      Width = 65
      Height = 29
      ButtonKind = bkClassic
      MinValue = 1.000000000000000000
      Value = 1.000000000000000000
      Color = clWhite
      TabOrder = 1
      OnChange = seLayer2Changed
    end
    object seLayerCount: TJvSpinEdit
      Left = 157
      Top = 107
      Width = 65
      Height = 29
      ButtonKind = bkClassic
      MaxValue = 100.000000000000000000
      MinValue = 1.000000000000000000
      Value = 1.000000000000000000
      Color = clWhite
      TabOrder = 2
    end
  end
end
