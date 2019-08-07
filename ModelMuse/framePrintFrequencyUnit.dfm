object framePrintFrequency: TframePrintFrequency
  Left = 0
  Top = 0
  Width = 181
  Height = 30
  HorzScrollBar.Range = 0
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = 19
  Font.Name = 'Arial'
  Font.Pitch = fpVariable
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  TabStop = True
  object Splitter: TSplitter
    Left = 79
    Top = 0
    Height = 30
    Align = alRight
  end
  object panelCombo: TPanel
    Left = 82
    Top = 0
    Width = 99
    Height = 30
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      99
      30)
    object comboUnits: TComboBox
      Left = 0
      Top = 0
      Width = 99
      Height = 27
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 10
      ItemHeight = 19
      TabOrder = 0
      OnChange = comboUnitsChange
      Items.Strings = (
        'default'
        'seconds'
        'minutes'
        'hours'
        'days'
        'years'
        'step'
        'end')
    end
  end
  object panelEdit: TPanel
    Left = 0
    Top = 0
    Width = 79
    Height = 30
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      79
      30)
    object rdeFrequency: TRbwDataEntry
      Left = 0
      Top = 0
      Width = 78
      Height = 30
      Cursor = crIBeam
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 19
      TabOrder = 0
      Text = '0'
      DataType = dtReal
      Max = 1.000000000000000000
      ChangeDisabledColor = True
    end
  end
end
