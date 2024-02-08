inherited frameFarmDiversion: TframeFarmDiversion
  Width = 577
  Height = 240
  ExplicitWidth = 577
  ExplicitHeight = 240
  inherited Panel: TPanel
    Top = 199
    Width = 577
    ExplicitTop = 199
    ExplicitWidth = 577
    DesignSize = (
      577
      41)
    inherited sbAdd: TSpeedButton
      Left = 360
      ExplicitLeft = 360
    end
    inherited sbInsert: TSpeedButton
      Left = 381
      ExplicitLeft = 381
    end
    inherited sbDelete: TSpeedButton
      Left = 402
      ExplicitLeft = 402
    end
    object lblLocationMethod: TLabel [4]
      Left = 152
      Top = 9
      Width = 91
      Height = 15
      Caption = 'Location method'
    end
    object comboMethod: TComboBox
      Left = 249
      Top = 6
      Width = 105
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'Object'
      OnChange = comboMethodChange
      Items.Strings = (
        'Object'
        'Location'
        'Cell')
    end
  end
  inherited Grid: TRbwDataGrid4
    Width = 577
    Height = 142
    ColCount = 5
    OnSetEditText = GridSetEditText
    Columns = <
      item
        AutoAdjustRowHeights = False
        AutoAdjustCaptionRowHeights = False
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -11
        ButtonFont.Name = 'Tahoma'
        ButtonFont.Style = []
        ButtonUsed = False
        ButtonWidth = 20
        CheckMax = False
        CheckMin = False
        ComboUsed = True
        Format = rcf4Real
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = True
      end
      item
        AutoAdjustRowHeights = False
        AutoAdjustCaptionRowHeights = False
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -11
        ButtonFont.Name = 'Tahoma'
        ButtonFont.Style = []
        ButtonUsed = False
        ButtonWidth = 20
        CheckMax = False
        CheckMin = False
        ComboUsed = True
        Format = rcf4Real
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = False
      end
      item
        AutoAdjustRowHeights = False
        AutoAdjustCaptionRowHeights = False
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -11
        ButtonFont.Name = 'Tahoma'
        ButtonFont.Style = []
        ButtonUsed = False
        ButtonWidth = 20
        CheckMax = False
        CheckMin = False
        ComboUsed = False
        Format = rcf4String
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = False
      end
      item
        AutoAdjustRowHeights = False
        AutoAdjustCaptionRowHeights = False
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -11
        ButtonFont.Name = 'Tahoma'
        ButtonFont.Style = []
        ButtonUsed = False
        ButtonWidth = 20
        CheckMax = False
        CheckMin = False
        ComboUsed = False
        Format = rcf4String
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = False
      end
      item
        AutoAdjustRowHeights = False
        AutoAdjustCaptionRowHeights = False
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -11
        ButtonFont.Name = 'Tahoma'
        ButtonFont.Style = []
        ButtonUsed = False
        ButtonWidth = 20
        CheckMax = False
        CheckMin = False
        ComboUsed = False
        Format = rcf4String
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = False
      end>
    ExplicitWidth = 577
    ExplicitHeight = 142
    ColWidths = (
      64
      64
      269
      64
      64)
  end
  inherited pnlTop: TPanel
    Width = 577
    ExplicitWidth = 577
    object lblSfrObjects: TLabel [0]
      Left = 40
      Top = 5
      Width = 35
      Height = 15
      Caption = 'Object'
      Enabled = False
    end
    object lblPositionChoice: TLabel [1]
      Left = 79
      Top = 5
      Width = 81
      Height = 15
      Caption = 'Position choice'
      Enabled = False
    end
    object lblVertexNumber: TLabel [2]
      Left = 118
      Top = 5
      Width = 32
      Height = 15
      Caption = 'Vertex'
      Enabled = False
    end
    object lblX: TLabel [3]
      Left = 151
      Top = 5
      Width = 7
      Height = 15
      Caption = 'X'
      Enabled = False
    end
    object lblY: TLabel [4]
      Left = 175
      Top = 5
      Width = 7
      Height = 15
      Caption = 'Y'
      Enabled = False
    end
    object lblRow: TLabel [5]
      Left = 236
      Top = 5
      Width = 23
      Height = 15
      Caption = 'Row'
      Enabled = False
    end
    object lblCol: TLabel [6]
      Left = 293
      Top = 5
      Width = 43
      Height = 15
      Caption = 'Column'
      Enabled = False
    end
    object rdeCol: TRbwDataEntry [7]
      Left = 289
      Top = 27
      Width = 51
      Height = 22
      Color = clBtnFace
      Enabled = False
      TabOrder = 7
      Text = '1'
      OnChange = rdeColChange
      DataType = dtInteger
      Max = 1.000000000000000000
      Min = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
    object comboSfrObjects: TComboBox [8]
      Left = 40
      Top = 27
      Width = 65
      Height = 23
      Style = csDropDownList
      Enabled = False
      TabOrder = 1
      OnChange = comboSfrObjectsChange
    end
    object comboPositionChoice: TComboBox [9]
      Left = 80
      Top = 27
      Width = 65
      Height = 23
      Style = csDropDownList
      Enabled = False
      TabOrder = 2
      OnChange = comboPositionChoiceChange
      Items.Strings = (
        'First vertex'
        'A middle vertex'
        'Last vertex')
    end
    object rdeVertexNumber: TRbwDataEntry [10]
      Left = 118
      Top = 27
      Width = 51
      Height = 22
      Color = clBtnFace
      Enabled = False
      TabOrder = 3
      Text = '1'
      OnChange = rdeVertexNumberChange
      DataType = dtInteger
      Max = 1.000000000000000000
      Min = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
    object rdeX: TRbwDataEntry [11]
      Left = 151
      Top = 27
      Width = 51
      Height = 22
      Color = clBtnFace
      Enabled = False
      TabOrder = 4
      Text = '1'
      OnChange = rdeXChange
      DataType = dtReal
      Max = 1.000000000000000000
      Min = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
    object rdeY: TRbwDataEntry [12]
      Left = 175
      Top = 27
      Width = 51
      Height = 22
      Color = clBtnFace
      Enabled = False
      TabOrder = 5
      Text = '1'
      OnChange = rdeYChange
      DataType = dtReal
      Max = 1.000000000000000000
      Min = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
    object rdeRow: TRbwDataEntry [13]
      Left = 232
      Top = 27
      Width = 51
      Height = 22
      Color = clBtnFace
      Enabled = False
      TabOrder = 6
      Text = '1'
      OnChange = rdeRowChange
      DataType = dtInteger
      Max = 1.000000000000000000
      Min = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
    inherited edFormula: TLabeledEdit
      Left = 8
      Top = 27
      Width = 49
      EditLabel.ExplicitLeft = 8
      EditLabel.ExplicitTop = 9
      Visible = False
      ExplicitLeft = 8
      ExplicitTop = 27
      ExplicitWidth = 49
    end
    inherited cbMultiCheck: TCheckBox
      TabOrder = 8
    end
    inherited comboChoice: TComboBox
      TabOrder = 9
    end
  end
end
