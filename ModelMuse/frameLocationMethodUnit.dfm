object frameLocationMethod: TframeLocationMethod
  Left = 0
  Top = 0
  Width = 555
  Height = 72
  Enabled = False
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = 19
  Font.Name = 'Arial'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  TabStop = True
  object lblLocationMethod: TLabel
    Left = 6
    Top = 9
    Width = 120
    Height = 19
    Caption = 'Location method'
  end
  object pcLocationChoice: TJvPageControl
    Left = 337
    Top = 0
    Width = 218
    Height = 72
    ActivePage = tabLocation
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    ClientBorderWidth = 0
    object tabNone: TTabSheet
      Caption = 'tabNone'
      TabVisible = False
    end
    object tabCell: TTabSheet
      Caption = 'tabCell'
      ImageIndex = 3
      TabVisible = False
      object lblCol: TLabel
        Left = 3
        Top = 7
        Width = 24
        Height = 19
        Caption = 'Col'
      end
      object lblRow: TLabel
        Left = 63
        Top = 7
        Width = 31
        Height = 19
        Caption = 'Row'
      end
      object lblLay: TLabel
        Left = 123
        Top = 7
        Width = 42
        Height = 19
        Caption = 'Layer'
      end
      object rdeLay: TRbwDataEntry
        Left = 123
        Top = 32
        Width = 54
        Height = 27
        ItemHeight = 19
        TabOrder = 0
        Text = '1'
        OnChange = ControlChange
        Items.Strings = (
          'False'
          'True')
        DataType = dtInteger
        Max = 1.000000000000000000
        Min = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeRow: TRbwDataEntry
        Left = 63
        Top = 32
        Width = 54
        Height = 27
        ItemHeight = 19
        TabOrder = 1
        Text = '1'
        OnChange = ControlChange
        Items.Strings = (
          'False'
          'True')
        DataType = dtInteger
        Max = 1.000000000000000000
        Min = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeCol: TRbwDataEntry
        Left = 3
        Top = 32
        Width = 54
        Height = 27
        ItemHeight = 19
        TabOrder = 2
        Text = '1'
        OnChange = ControlChange
        Items.Strings = (
          'False'
          'True')
        DataType = dtInteger
        Max = 1.000000000000000000
        Min = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object tabLocation: TTabSheet
      Caption = 'tabLocation'
      ImageIndex = 2
      TabVisible = False
      object lblX: TLabel
        Left = 3
        Top = 7
        Width = 11
        Height = 19
        Caption = 'X'
      end
      object lblY: TLabel
        Left = 63
        Top = 7
        Width = 11
        Height = 19
        Caption = 'Y'
      end
      object lblZ: TLabel
        Left = 123
        Top = 7
        Width = 9
        Height = 19
        Caption = 'Z'
      end
      object rdeX: TRbwDataEntry
        Left = 3
        Top = 32
        Width = 54
        Height = 27
        ItemHeight = 19
        TabOrder = 0
        Text = '0'
        OnChange = ControlChange
        Items.Strings = (
          'False'
          'True')
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeY: TRbwDataEntry
        Left = 63
        Top = 32
        Width = 54
        Height = 27
        ItemHeight = 19
        TabOrder = 1
        Text = '0'
        OnChange = ControlChange
        Items.Strings = (
          'False'
          'True')
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeZ: TRbwDataEntry
        Left = 123
        Top = 32
        Width = 54
        Height = 27
        ItemHeight = 19
        TabOrder = 2
        Text = '0'
        OnChange = ControlChange
        Items.Strings = (
          'False'
          'True')
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
    end
    object tabObject: TTabSheet
      Caption = 'tabObject'
      ImageIndex = 1
      TabVisible = False
      DesignSize = (
        218
        70)
      object comboObject: TComboBox
        Left = 3
        Top = 4
        Width = 208
        Height = 27
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 19
        TabOrder = 0
        OnChange = ControlChange
      end
    end
  end
  object comboLocationChoice: TJvImageComboBox
    Left = 132
    Top = 4
    Width = 145
    Height = 29
    Style = csOwnerDrawVariable
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 145
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clScrollBar
    Font.Height = 19
    Font.Name = 'Arial'
    Font.Style = []
    ImageHeight = 0
    ImageWidth = 0
    ItemHeight = 23
    ItemIndex = 0
    ParentFont = False
    TabOrder = 1
    OnChange = comboLocationChoiceChange
    Items = <
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'none'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Cell'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Location'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Object'
      end>
  end
end
