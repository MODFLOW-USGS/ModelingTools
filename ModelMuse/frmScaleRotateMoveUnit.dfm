inherited frmScaleRotateMove: TfrmScaleRotateMove
  HelpType = htKeyword
  HelpKeyword = 'Scale_Rotate_and_Move_Objects'
  Caption = 'Scale, Rotate, and Move Objects'
  ClientHeight = 243
  ClientWidth = 623
  ExplicitWidth = 631
  ExplicitHeight = 277
  PixelsPerInch = 96
  TextHeight = 18
  object gbScale: TJvGroupBox
    Left = 8
    Top = 8
    Width = 303
    Height = 105
    Caption = 'Scale'
    TabOrder = 0
    Checkable = True
    PropagateEnable = True
    OnCheckBoxClick = EnableOk
    object lblXScale: TLabel
      Left = 8
      Top = 20
      Width = 95
      Height = 18
      Margins.Left = 8
      Caption = 'X scale factor'
      Enabled = False
    end
    object lblYScale: TLabel
      Left = 152
      Top = 20
      Width = 93
      Height = 18
      Margins.Left = 8
      Caption = 'Y scale factor'
      Enabled = False
    end
    object rdeXScale: TRbwDataEntry
      Left = 8
      Top = 40
      Width = 133
      Height = 22
      Margins.Left = 8
      Color = clBtnFace
      Enabled = False
      TabOrder = 1
      Text = '1'
      OnChange = rdeXScaleChange
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
    object cbLockAspectRatio: TCheckBox
      Left = 8
      Top = 68
      Width = 225
      Height = 17
      Margins.Left = 8
      Caption = 'Lock aspect ratio'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 2
      OnClick = cbLockAspectRatioClick
    end
    object rdeYScale: TRbwDataEntry
      Left = 152
      Top = 40
      Width = 133
      Height = 22
      Margins.Left = 8
      Color = clBtnFace
      Enabled = False
      TabOrder = 3
      Text = '1'
      OnChange = rdeXScaleChange
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
  end
  object gbRotate: TJvGroupBox
    Left = 8
    Top = 119
    Width = 303
    Height = 75
    Caption = 'Rotate'
    TabOrder = 1
    Checkable = True
    PropagateEnable = True
    OnCheckBoxClick = EnableOk
    object lblAngle: TLabel
      Left = 8
      Top = 20
      Width = 246
      Height = 18
      Margins.Left = 8
      Caption = 'Angle of rotation (counterclockwise)'
      Enabled = False
    end
    object rdeAngle: TRbwDataEntry
      Left = 8
      Top = 40
      Width = 133
      Height = 22
      Margins.Left = 8
      Color = clBtnFace
      Enabled = False
      TabOrder = 1
      Text = '0'
      OnChange = rdeAngleChange
      DataType = dtReal
      Max = 1.000000000000000000
      ChangeDisabledColor = True
    end
  end
  object gbMove: TJvGroupBox
    Left = 317
    Top = 119
    Width = 298
    Height = 75
    Caption = 'Move'
    TabOrder = 2
    Checkable = True
    PropagateEnable = True
    OnCheckBoxClick = EnableOk
    object lblMoveX: TLabel
      Left = 8
      Top = 20
      Width = 53
      Height = 18
      Margins.Left = 8
      Caption = 'X offset'
      Enabled = False
    end
    object lblMoveY: TLabel
      Left = 152
      Top = 20
      Width = 51
      Height = 18
      Margins.Left = 8
      Caption = 'Y offset'
      Enabled = False
    end
    object rdeMoveX: TRbwDataEntry
      Left = 8
      Top = 40
      Width = 133
      Height = 22
      Margins.Left = 8
      Color = clBtnFace
      Enabled = False
      TabOrder = 1
      Text = '0'
      OnChange = rdeMoveXChange
      DataType = dtReal
      Max = 1.000000000000000000
      ChangeDisabledColor = True
    end
    object rdeMoveY: TRbwDataEntry
      Left = 152
      Top = 40
      Width = 133
      Height = 22
      Margins.Left = 8
      Color = clBtnFace
      Enabled = False
      TabOrder = 2
      Text = '0'
      OnChange = rdeMoveYChange
      DataType = dtReal
      Max = 1.000000000000000000
      ChangeDisabledColor = True
    end
  end
  object btnHelp: TBitBtn
    Left = 325
    Top = 200
    Width = 91
    Height = 33
    DoubleBuffered = True
    Kind = bkHelp
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 3
    OnClick = btnHelpClick
  end
  object btnOK: TBitBtn
    Left = 424
    Top = 200
    Width = 91
    Height = 33
    DoubleBuffered = True
    Enabled = False
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 4
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 524
    Top = 200
    Width = 91
    Height = 33
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 5
  end
  object gbCenter: TGroupBox
    Left = 317
    Top = 8
    Width = 298
    Height = 105
    Caption = 'Reference point for scaling and rotation'
    TabOrder = 6
    object lblXCenter: TLabel
      Left = 8
      Top = 53
      Width = 62
      Height = 18
      Margins.Left = 8
      Caption = 'X Center'
      Enabled = False
    end
    object lblYCenter: TLabel
      Left = 152
      Top = 53
      Width = 60
      Height = 18
      Margins.Left = 8
      Caption = 'Y Center'
      Enabled = False
    end
    object comboCenterOfRotation: TJvImageComboBox
      Left = 8
      Top = 20
      Width = 145
      Height = 28
      Margins.Left = 8
      Style = csOwnerDrawVariable
      ButtonStyle = fsLighter
      Color = clBtnFace
      DroppedWidth = 145
      Enabled = False
      ImageHeight = 0
      ImageWidth = 0
      ItemHeight = 22
      ItemIndex = 0
      TabOrder = 0
      OnChange = comboCenterOfRotationChange
      Items = <
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'Object center'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'Specified location'
        end>
    end
    object rdeXCenter: TRbwDataEntry
      Left = 8
      Top = 73
      Width = 133
      Height = 22
      Margins.Left = 8
      Color = clBtnFace
      Enabled = False
      TabOrder = 1
      Text = '0'
      DataType = dtReal
      Max = 1.000000000000000000
      ChangeDisabledColor = True
    end
    object rdeYCenter: TRbwDataEntry
      Left = 152
      Top = 73
      Width = 133
      Height = 22
      Margins.Left = 8
      Color = clBtnFace
      Enabled = False
      TabOrder = 2
      Text = '0'
      DataType = dtReal
      Max = 1.000000000000000000
      ChangeDisabledColor = True
    end
  end
end
