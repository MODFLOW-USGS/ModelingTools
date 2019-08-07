inherited frameDE4: TframeDE4
  Width = 518
  Height = 548
  ExplicitWidth = 518
  ExplicitHeight = 548
  DesignSize = (
    518
    548)
  object lblDe4Itmx: TLabel [2]
    Left = 16
    Top = 168
    Width = 179
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Maximum number of iterations (ITMX)'
  end
  object lblDe4Mxup: TLabel [3]
    Left = 16
    Top = 200
    Width = 249
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Maximum number of equations in upper part (MXUP)'
  end
  object lblDe4Mxlow: TLabel [4]
    Left = 16
    Top = 228
    Width = 257
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Maximum number of equations in lower part (MXLOW)'
  end
  object lblDe4Mxbw: TLabel [5]
    Left = 16
    Top = 260
    Width = 141
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Maximum band width (MXBW)'
  end
  object lblDe4Ifreq: TLabel [6]
    Left = 16
    Top = 288
    Width = 232
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Frequency at which coefficients change (IFREQ)'
  end
  object lblDe4Mutd4: TLabel [7]
    Left = 16
    Top = 320
    Width = 103
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Print control (MUTD4)'
  end
  object lblDe4Accl: TLabel [8]
    Left = 16
    Top = 348
    Width = 149
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Acceleration parameter (ACCL)'
  end
  object lblDe4Hclose: TLabel [9]
    Left = 16
    Top = 380
    Width = 192
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Head change closure criterion (HCLOSE)'
  end
  object lblRdeIprd4: TLabel [10]
    Left = 16
    Top = 408
    Width = 118
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Printout interval (IPRD4)'
  end
  inherited memoComments: TMemo
    Width = 487
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExplicitWidth = 487
  end
  object rdeDe4Itmx: TRbwDataEntry [12]
    Left = 358
    Top = 165
    Width = 145
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 1
    Text = '5'
    DataType = dtInteger
    Max = 1.000000000000000000
    Min = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeDe4Mxup: TRbwDataEntry [13]
    Left = 358
    Top = 197
    Width = 145
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 2
    Text = '0'
    DataType = dtInteger
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeDe4Mxlow: TRbwDataEntry [14]
    Left = 358
    Top = 225
    Width = 145
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 3
    Text = '0'
    DataType = dtInteger
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeDe4Mxbw: TRbwDataEntry [15]
    Left = 358
    Top = 257
    Width = 145
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 4
    Text = '0'
    DataType = dtInteger
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object comboDe4Ifreq: TJvImageComboBox [16]
    Left = 272
    Top = 285
    Width = 231
    Height = 23
    Style = csOwnerDrawVariable
    Anchors = [akLeft, akBottom]
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 231
    Enabled = False
    ImageHeight = 0
    ImageWidth = 0
    ItemHeight = 17
    ItemIndex = -1
    TabOrder = 5
    Items = <
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Coefficients constant (1)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Coefficients vary by stress period (2)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Nonlinear flow equations (3)'
      end>
  end
  object comboDe4Mutd4: TJvImageComboBox [17]
    Left = 272
    Top = 317
    Width = 231
    Height = 23
    Style = csOwnerDrawVariable
    Anchors = [akLeft, akBottom]
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 231
    Enabled = False
    ImageHeight = 0
    ImageWidth = 0
    ItemHeight = 17
    ItemIndex = -1
    TabOrder = 6
    Items = <
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Print maximum information (0)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Print number of iterations (1)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Print nothing (2)'
      end>
  end
  object rdeDe4Accl: TRbwDataEntry [18]
    Left = 358
    Top = 345
    Width = 145
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 7
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeDe4Hclose: TRbwDataEntry [19]
    Left = 358
    Top = 377
    Width = 145
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 8
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeRdeIprd4: TRbwDataEntry [20]
    Left = 358
    Top = 405
    Width = 145
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 9
    Text = '0'
    DataType = dtInteger
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
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
        Control = rdeDe4Itmx
      end
      item
        Control = rdeDe4Mxup
      end
      item
        Control = rdeDe4Mxlow
      end
      item
        Control = rdeDe4Mxbw
      end
      item
        Control = comboDe4Ifreq
      end
      item
        Control = comboDe4Mutd4
      end
      item
        Control = rdeDe4Accl
      end
      item
        Control = rdeDe4Hclose
      end
      item
        Control = rdeRdeIprd4
      end>
  end
end
