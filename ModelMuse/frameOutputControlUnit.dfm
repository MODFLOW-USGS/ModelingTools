object frameOutputControl: TframeOutputControl
  Left = 0
  Top = 0
  Width = 445
  Height = 331
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Arial'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  TabStop = True
  object lblOutputType: TLabel
    Left = 16
    Top = 8
    Width = 93
    Height = 18
    Caption = 'lblOutputType'
  end
  object lblFrequency: TLabel
    Left = 16
    Top = 241
    Width = 73
    Height = 18
    Caption = 'Frequency'
  end
  object lblN: TLabel
    Left = 16
    Top = 301
    Width = 24
    Height = 18
    Caption = 'N ='
  end
  object lblExternalFormat: TLabel
    Left = 16
    Top = 110
    Width = 128
    Height = 18
    Caption = 'External file format'
  end
  object lblResult: TLabel
    Left = 207
    Top = 132
    Width = 43
    Height = 18
    Caption = 'Result'
  end
  object lblDot: TLabel
    Left = 174
    Top = 132
    Width = 5
    Height = 13
    Caption = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblListinglFormat: TLabel
    Left = 16
    Top = 185
    Width = 118
    Height = 18
    Caption = 'Listing file format'
  end
  object lblSaveType: TLabel
    Left = 16
    Top = 54
    Width = 113
    Height = 18
    Caption = 'External file type'
  end
  object spN: TJvSpinEdit
    Left = 46
    Top = 298
    Width = 65
    Height = 26
    CheckMaxValue = False
    ButtonKind = bkClassic
    MinValue = 1.000000000000000000
    Value = 1.000000000000000000
    TabOrder = 10
  end
  object adeD: TRbwDataEntry
    Left = 184
    Top = 129
    Width = 17
    Height = 27
    TabOrder = 5
    Text = '5'
    OnChange = adeDChange
    DataType = dtInteger
    Max = 1.000000000000000000
    Min = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object adeW: TRbwDataEntry
    Left = 132
    Top = 129
    Width = 37
    Height = 27
    TabOrder = 4
    Text = '13'
    OnChange = adeWChange
    DataType = dtInteger
    Max = 1.000000000000000000
    Min = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object comboP: TJvImageComboBox
    Left = 16
    Top = 129
    Width = 60
    Height = 28
    Style = csOwnerDrawVariable
    ButtonStyle = fsLighter
    DroppedWidth = 60
    ImageHeight = 0
    ImageWidth = 0
    ItemHeight = 22
    ItemIndex = 1
    TabOrder = 2
    OnChange = comboPChange
    Items = <
      item
        Brush.Style = bsClear
        Indent = 0
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = '1P'
      end>
  end
  object comboREdit: TJvImageComboBox
    Left = 82
    Top = 129
    Width = 44
    Height = 28
    Style = csOwnerDrawVariable
    ButtonStyle = fsLighter
    DroppedWidth = 44
    ImageHeight = 0
    ImageWidth = 0
    ItemHeight = 22
    ItemIndex = 2
    TabOrder = 3
    OnChange = comboREditChange
    Items = <
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'F'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'D'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'E'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'EN'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Es'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'G'
      end>
  end
  object comboFrequency: TJvImageComboBox
    Left = 16
    Top = 264
    Width = 393
    Height = 28
    Style = csOwnerDrawVariable
    ButtonStyle = fsLighter
    DroppedWidth = 409
    ImageHeight = 0
    ImageWidth = 0
    ItemHeight = 22
    ItemIndex = 0
    TabOrder = 9
    OnChange = comboFrequencyChange
    Items = <
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'First N timessteps and each N'#39'th time step thereafter'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Last time step of each N'#39'th stress period'
      end>
  end
  object cbSaveExternal: TCheckBox
    Left = 16
    Top = 31
    Width = 169
    Height = 17
    Caption = 'Save in external file'
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = cbSaveExternalClick
  end
  object cbPrintListing: TCheckBox
    Left = 16
    Top = 162
    Width = 169
    Height = 17
    Caption = 'Print in listing file'
    TabOrder = 6
    OnClick = cbPrintListingClick
  end
  object comboPrintStyle: TJvImageComboBox
    Left = 16
    Top = 204
    Width = 66
    Height = 28
    Style = csOwnerDrawVariable
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 66
    Enabled = False
    ImageHeight = 0
    ImageWidth = 0
    ItemHeight = 22
    ItemIndex = 0
    TabOrder = 7
    Items = <
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'strip'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'wrap'
      end>
  end
  object comboPrintFormat: TJvImageComboBox
    Left = 91
    Top = 204
    Width = 94
    Height = 28
    Style = csOwnerDrawVariable
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 94
    Enabled = False
    ImageHeight = 0
    ImageWidth = 0
    ItemHeight = 22
    ItemIndex = 0
    TabOrder = 8
    Items = <
      item
        Brush.Style = bsClear
        Indent = 0
        Text = '11G10.3'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = '9G13.6'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = '15F7.1'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = '15F7.2'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = '15F7.3'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = '15F7.4'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = '20F5.0'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = '20F5.1'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = '20F5.2'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = '20F5.3'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = '20F5.4'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = '10G11.4'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = '10F6.0'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = '10F6.1'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = '10F6.2'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = '10F6.3'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = '10F6.4'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = '10F6.5'
      end>
  end
  object comboSaveType: TJvImageComboBox
    Left = 16
    Top = 73
    Width = 145
    Height = 28
    Style = csOwnerDrawVariable
    ButtonStyle = fsLighter
    DroppedWidth = 145
    ImageHeight = 0
    ImageWidth = 0
    ItemHeight = 22
    ItemIndex = 0
    TabOrder = 1
    OnChange = comboSaveTypeChange
    Items = <
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Formatted text'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Binary'
      end>
  end
  object rcExternalFormat: TRbwController
    ControlList = <
      item
        Control = adeD
      end
      item
        Control = adeW
      end
      item
        Control = comboP
      end
      item
        Control = comboREdit
      end
      item
        Control = lblResult
      end
      item
        Control = lblDot
      end>
    Left = 305
    Top = 200
  end
  object rcListingFormat: TRbwController
    ControlList = <
      item
        Control = comboPrintFormat
      end
      item
        Control = comboPrintStyle
      end>
    Enabled = False
    Left = 232
    Top = 248
  end
end
