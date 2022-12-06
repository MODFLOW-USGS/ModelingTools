inherited framePackageMnw1: TframePackageMnw1
  Width = 595
  Height = 514
  ExplicitWidth = 595
  ExplicitHeight = 514
  DesignSize = (
    595
    514)
  object lblMaxIterations: TLabel [2]
    Left = 143
    Top = 160
    Width = 271
    Height = 15
    Caption = 'Maximum number of MNW iterations (NOMOITER)'
  end
  object lblLosstype: TLabel [3]
    Left = 167
    Top = 195
    Width = 113
    Height = 15
    Caption = 'Loss type (LOSSTYPE)'
  end
  object lblLossExponent: TLabel [4]
    Left = 167
    Top = 227
    Width = 148
    Height = 15
    Caption = 'Loss Exponent (PLossMNW)'
  end
  object lblWellFileName: TLabel [5]
    Left = 16
    Top = 257
    Width = 175
    Height = 15
    Caption = 'Well file name (iunw1) (Optional)'
  end
  object lblByNode: TLabel [6]
    Left = 16
    Top = 327
    Width = 252
    Height = 15
    Caption = 'File name for output by node (iunby) (Optional)'
  end
  object lblQSum: TLabel [7]
    Left = 16
    Top = 391
    Width = 245
    Height = 15
    Caption = 'File name for output by well (iunqs) (Optional)'
  end
  object lblByNodeFrequency: TLabel [8]
    Left = 435
    Top = 327
    Width = 55
    Height = 15
    Anchors = [akTop, akRight]
    Caption = 'Frequency'
  end
  object lblQSumFrequency: TLabel [9]
    Left = 435
    Top = 391
    Width = 55
    Height = 15
    Anchors = [akTop, akRight]
    Caption = 'Frequency'
  end
  inherited memoComments: TMemo
    Width = 564
    ExplicitWidth = 564
  end
  object seMaxIterations: TJvSpinEdit [11]
    Left = 16
    Top = 157
    Width = 121
    Height = 21
    MaxValue = 2147483647.000000000000000000
    MinValue = 1.000000000000000000
    Value = 1.000000000000000000
    Enabled = False
    TabOrder = 1
  end
  object comboLosstype: TJvImageComboBox [12]
    Left = 16
    Top = 192
    Width = 145
    Height = 25
    Style = csOwnerDrawVariable
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 145
    Enabled = False
    ImageHeight = 0
    ImageWidth = 0
    ItemHeight = 19
    ItemIndex = 0
    TabOrder = 2
    OnChange = comboLosstypeChange
    Items = <
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Skin'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Linear'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Nonlinear'
      end>
  end
  object rdeLossExponent: TRbwDataEntry [13]
    Left = 16
    Top = 224
    Width = 145
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 3
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object fedWellFileName: TJvFilenameEdit [14]
    Left = 16
    Top = 276
    Width = 564
    Height = 21
    DefaultExt = '.wel'
    Filter = 'Well files (*.wel)|*.wel|All files (*.*)|*.*'
    Enabled = False
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    Text = ''
    OnChange = fedWellFileNameChange
  end
  object fedByNode: TJvFilenameEdit [15]
    Left = 16
    Top = 348
    Width = 413
    Height = 21
    DefaultExt = '.ByNode'
    Enabled = False
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
    Text = ''
    OnChange = fedByNodeChange
  end
  object fedQSum: TJvFilenameEdit [16]
    Left = 16
    Top = 410
    Width = 413
    Height = 21
    DefaultExt = '.ByWell'
    Enabled = False
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
    Text = ''
    OnChange = fedQSumChange
  end
  object comboByNodeFrequency: TJvImageComboBox [17]
    Left = 435
    Top = 346
    Width = 145
    Height = 25
    Style = csOwnerDrawVariable
    Anchors = [akTop, akRight]
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 145
    Enabled = False
    ImageHeight = 0
    ImageWidth = 0
    ItemHeight = 19
    ItemIndex = -1
    TabOrder = 5
    Items = <
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Output Control'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'All times'
      end>
  end
  object comboQSumFrequency: TJvImageComboBox [18]
    Left = 435
    Top = 410
    Width = 145
    Height = 25
    Style = csOwnerDrawVariable
    Anchors = [akTop, akRight]
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 145
    Enabled = False
    ImageHeight = 0
    ImageWidth = 0
    ItemHeight = 19
    ItemIndex = -1
    TabOrder = 8
    Items = <
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Output Control'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'All times'
      end>
  end
  inherited rcSelectionController: TRbwController
    ControlList = <
      item
        Control = lblComments
      end
      item
        Control = memoComments
      end
      item
        Control = seMaxIterations
      end
      item
        Control = comboLosstype
      end
      item
        Control = fedWellFileName
      end
      item
        Control = fedByNode
      end
      item
        Control = fedQSum
      end>
    OnEnabledChange = rcSelectionControllerEnabledChange
  end
end
