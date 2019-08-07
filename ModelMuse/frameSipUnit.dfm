inherited frameSIP: TframeSIP
  Width = 607
  Height = 440
  ExplicitWidth = 607
  ExplicitHeight = 440
  DesignSize = (
    607
    440)
  object lblSipMxiter: TLabel [2]
    Left = 16
    Top = 160
    Width = 172
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Max. number of iterations (MXITER)'
  end
  object lblSipNparm: TLabel [3]
    Left = 16
    Top = 192
    Width = 185
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Number of iteration variables (NPARM)'
  end
  object lblSipAccl: TLabel [4]
    Left = 16
    Top = 224
    Width = 137
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Acceleration variable (ACCL)'
  end
  object lblSipHclose: TLabel [5]
    Left = 16
    Top = 256
    Width = 155
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Head change criterion (HCLOSE)'
  end
  object lblSipIpcalc: TLabel [6]
    Left = 16
    Top = 288
    Width = 106
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Seed source (IPCALC)'
  end
  object lblSipWseed: TLabel [7]
    Left = 16
    Top = 320
    Width = 272
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Seed for seed for calculating iteration variables (WSEED)'
  end
  object lblSipIprsip: TLabel [8]
    Left = 16
    Top = 352
    Width = 121
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Printout interval (IPRSIP)'
  end
  inherited memoComments: TMemo
    Width = 576
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExplicitWidth = 576
  end
  object rdeSipMxiter: TRbwDataEntry [10]
    Left = 433
    Top = 157
    Width = 159
    Height = 22
    Hint = 
      'For linear problems, MXITER, should be 1 unless more than 50 inn' +
      'er iterations are required.  In that case MXITER could be as lar' +
      'ge as 10.  For nonlinear problems, MXITER may need to be larger ' +
      'but should usually be less than 100.'
    HelpContext = 910
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 1
    Text = '20'
    DataType = dtInteger
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeSipNparm: TRbwDataEntry [11]
    Left = 433
    Top = 185
    Width = 159
    Height = 22
    Hint = 
      'For linear problems, MXITER, should be 1 unless more than 50 inn' +
      'er iterations are required.  In that case MXITER could be as lar' +
      'ge as 10.  For nonlinear problems, MXITER may need to be larger ' +
      'but should usually be less than 100.'
    HelpContext = 910
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 2
    Text = '20'
    DataType = dtInteger
    Max = 1.000000000000000000
    Min = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeSipAccl: TRbwDataEntry [12]
    Left = 433
    Top = 221
    Width = 159
    Height = 22
    Hint = 
      'For linear problems, MXITER, should be 1 unless more than 50 inn' +
      'er iterations are required.  In that case MXITER could be as lar' +
      'ge as 10.  For nonlinear problems, MXITER may need to be larger ' +
      'but should usually be less than 100.'
    HelpContext = 910
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 3
    Text = '20'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeSipHclose: TRbwDataEntry [13]
    Left = 433
    Top = 249
    Width = 159
    Height = 22
    Hint = 
      'For linear problems, MXITER, should be 1 unless more than 50 inn' +
      'er iterations are required.  In that case MXITER could be as lar' +
      'ge as 10.  For nonlinear problems, MXITER may need to be larger ' +
      'but should usually be less than 100.'
    HelpContext = 910
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 4
    Text = '20'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object comboSipIpcalc: TJvImageComboBox [14]
    Left = 342
    Top = 277
    Width = 250
    Height = 23
    Style = csOwnerDrawVariable
    Anchors = [akLeft, akBottom]
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 250
    Enabled = False
    ImageHeight = 0
    ImageWidth = 0
    ItemHeight = 17
    ItemIndex = -1
    TabOrder = 5
    OnChange = comboSipIpcalcChange
    Items = <
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Use seed entered by user (0)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Seed will be calculated (1)'
      end>
  end
  object rdeSipWseed: TRbwDataEntry [15]
    Left = 433
    Top = 313
    Width = 159
    Height = 22
    Hint = 
      'For linear problems, MXITER, should be 1 unless more than 50 inn' +
      'er iterations are required.  In that case MXITER could be as lar' +
      'ge as 10.  For nonlinear problems, MXITER may need to be larger ' +
      'but should usually be less than 100.'
    HelpContext = 910
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 6
    Text = '20'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeSipIprsip: TRbwDataEntry [16]
    Left = 433
    Top = 345
    Width = 159
    Height = 22
    Hint = 
      'For linear problems, MXITER, should be 1 unless more than 50 inn' +
      'er iterations are required.  In that case MXITER could be as lar' +
      'ge as 10.  For nonlinear problems, MXITER may need to be larger ' +
      'but should usually be less than 100.'
    HelpContext = 910
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 7
    Text = '20'
    DataType = dtInteger
    Max = 1.000000000000000000
    Min = 1.000000000000000000
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
        Control = rdeSipMxiter
      end
      item
        Control = rdeSipNparm
      end
      item
        Control = rdeSipAccl
      end
      item
        Control = rdeSipHclose
      end
      item
        Control = comboSipIpcalc
      end
      item
        Control = rdeSipIprsip
      end>
  end
end
