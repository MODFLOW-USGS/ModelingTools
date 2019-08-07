inherited frameGMG: TframeGMG
  Width = 625
  Height = 418
  ExplicitWidth = 625
  ExplicitHeight = 418
  DesignSize = (
    625
    418)
  inherited memoComments: TMemo
    Top = 65
    Width = 594
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExplicitTop = 65
    ExplicitWidth = 594
  end
  object pcGMG: TJvPageControl [3]
    Left = 0
    Top = 160
    Width = 625
    Height = 258
    ActivePage = tabControlAndPrint
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object tabControlAndPrint: TTabSheet
      Caption = 'Control and Print'
      DesignSize = (
        617
        230)
      object lblGmgRclose: TLabel
        Left = 16
        Top = 11
        Width = 197
        Height = 13
        Caption = 'Residual convergence criterion (RCLOSE)'
      end
      object lblGmgIiter: TLabel
        Left = 16
        Top = 43
        Width = 209
        Height = 13
        Caption = 'Maximum number of inner iterations (IITER)'
      end
      object lblGmgHclose: TLabel
        Left = 16
        Top = 75
        Width = 220
        Height = 13
        Caption = 'Head change convergence criterion (HCLOSE)'
      end
      object lblGmgMxiter: TLabel
        Left = 16
        Top = 107
        Width = 221
        Height = 13
        Caption = 'Maximum number of outer iterations (MXITER)'
      end
      object lblGmgIoutgmg: TLabel
        Left = 16
        Top = 170
        Width = 117
        Height = 13
        Caption = 'GMG Output (IOUTGMG)'
      end
      object lblGmgIsm: TLabel
        Left = 16
        Top = 141
        Width = 189
        Height = 13
        Caption = 'Multigrid preconditioner smoother (ISM)'
      end
      object rdeGmgRclose: TRbwDataEntry
        Left = 358
        Top = 12
        Width = 78
        Height = 22
        Hint = 
          'For linear problems, MXITER, should be 1 unless more than 50 inn' +
          'er iterations are required.  In that case MXITER could be as lar' +
          'ge as 10.  For nonlinear problems, MXITER may need to be larger ' +
          'but should usually be less than 100.'
        HelpContext = 910
        Color = clBtnFace
        Enabled = False
        TabOrder = 0
        Text = '20'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeGmgIiter: TRbwDataEntry
        Left = 358
        Top = 40
        Width = 78
        Height = 22
        Hint = 
          'For linear problems, MXITER, should be 1 unless more than 50 inn' +
          'er iterations are required.  In that case MXITER could be as lar' +
          'ge as 10.  For nonlinear problems, MXITER may need to be larger ' +
          'but should usually be less than 100.'
        HelpContext = 910
        Color = clBtnFace
        Enabled = False
        TabOrder = 1
        Text = '20'
        DataType = dtInteger
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeGmgHclose: TRbwDataEntry
        Left = 358
        Top = 72
        Width = 78
        Height = 22
        Hint = 
          'For linear problems, MXITER, should be 1 unless more than 50 inn' +
          'er iterations are required.  In that case MXITER could be as lar' +
          'ge as 10.  For nonlinear problems, MXITER may need to be larger ' +
          'but should usually be less than 100.'
        HelpContext = 910
        Color = clBtnFace
        Enabled = False
        TabOrder = 2
        Text = '20'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeGmgMxiter: TRbwDataEntry
        Left = 358
        Top = 104
        Width = 78
        Height = 22
        Hint = 
          'For linear problems, MXITER, should be 1 unless more than 50 inn' +
          'er iterations are required.  In that case MXITER could be as lar' +
          'ge as 10.  For nonlinear problems, MXITER may need to be larger ' +
          'but should usually be less than 100.'
        HelpContext = 910
        Color = clBtnFace
        Enabled = False
        TabOrder = 3
        Text = '20'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object comboGmgIoutgmg: TJvImageComboBox
        Left = 296
        Top = 167
        Width = 297
        Height = 23
        Style = csOwnerDrawVariable
        Anchors = [akLeft, akTop, akRight]
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 305
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 17
        ItemIndex = -1
        TabOrder = 4
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Print solver input (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Print full GMG output (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Print GMG convergence history (2)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Display full GMG output (3)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Display GMG convergence history (4)'
          end>
      end
      object cbGmbIunitmhc: TCheckBox
        Left = 16
        Top = 204
        Width = 598
        Height = 17
        Caption = 'Print maximum head change values (IUNITMHC)'
        Enabled = False
        TabOrder = 5
      end
      object comboGmgIsm: TJvImageComboBox
        Left = 296
        Top = 138
        Width = 297
        Height = 23
        Style = csOwnerDrawVariable
        Anchors = [akLeft, akTop, akRight]
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 305
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
            Text = 'ILU(0) smoothing  (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Symmetric GaussSeidel (SGS) (1)'
          end>
      end
    end
    object tabDampRelax: TTabSheet
      Caption = 'Damping and Relax'
      ImageIndex = 1
      DesignSize = (
        617
        230)
      object lblGmgDup: TLabel
        Left = 16
        Top = 75
        Width = 147
        Height = 13
        Caption = 'Maximum damping value (DUP)'
      end
      object lblGmgDlow: TLabel
        Left = 16
        Top = 107
        Width = 153
        Height = 13
        Caption = 'Minimum damping value (DLOW)'
      end
      object lblGmgChglimit: TLabel
        Left = 16
        Top = 138
        Width = 207
        Height = 13
        Caption = 'Maximum allowed head change (CHGLIMIT)'
      end
      object lblGmgRelax: TLabel
        Left = 16
        Top = 202
        Width = 146
        Height = 13
        Caption = 'Relaxation parameter (RELAX)'
      end
      object lblGmgIadamp: TLabel
        Left = 16
        Top = 43
        Width = 175
        Height = 13
        Caption = 'Adaptive damping method (IADAMP)'
      end
      object lblGmgIsc: TLabel
        Left = 16
        Top = 170
        Width = 138
        Height = 13
        Caption = 'Semicoarsening control (ISC)'
      end
      object lblGmgDamp: TLabel
        Left = 16
        Top = 11
        Width = 133
        Height = 13
        Caption = 'Damping parameter (DAMP)'
      end
      object rdeGmgDup: TRbwDataEntry
        Left = 358
        Top = 72
        Width = 78
        Height = 22
        Hint = 
          'For linear problems, MXITER, should be 1 unless more than 50 inn' +
          'er iterations are required.  In that case MXITER could be as lar' +
          'ge as 10.  For nonlinear problems, MXITER may need to be larger ' +
          'but should usually be less than 100.'
        HelpContext = 910
        Color = clBtnFace
        Enabled = False
        TabOrder = 0
        Text = '1'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeGmgRelax: TRbwDataEntry
        Left = 358
        Top = 199
        Width = 78
        Height = 22
        Hint = 
          'For linear problems, MXITER, should be 1 unless more than 50 inn' +
          'er iterations are required.  In that case MXITER could be as lar' +
          'ge as 10.  For nonlinear problems, MXITER may need to be larger ' +
          'but should usually be less than 100.'
        HelpContext = 910
        Color = clBtnFace
        Enabled = False
        TabOrder = 1
        Text = '20'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeGmgChglimit: TRbwDataEntry
        Left = 358
        Top = 135
        Width = 78
        Height = 22
        Hint = 
          'For linear problems, MXITER, should be 1 unless more than 50 inn' +
          'er iterations are required.  In that case MXITER could be as lar' +
          'ge as 10.  For nonlinear problems, MXITER may need to be larger ' +
          'but should usually be less than 100.'
        HelpContext = 910
        Color = clBtnFace
        Enabled = False
        TabOrder = 2
        Text = '20'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeGmgDlow: TRbwDataEntry
        Left = 358
        Top = 104
        Width = 78
        Height = 22
        Hint = 
          'For linear problems, MXITER, should be 1 unless more than 50 inn' +
          'er iterations are required.  In that case MXITER could be as lar' +
          'ge as 10.  For nonlinear problems, MXITER may need to be larger ' +
          'but should usually be less than 100.'
        HelpContext = 910
        Color = clBtnFace
        Enabled = False
        TabOrder = 3
        Text = '1'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object comboGmgIadamp: TJvImageComboBox
        Left = 296
        Top = 40
        Width = 297
        Height = 23
        Style = csOwnerDrawVariable
        Anchors = [akLeft, akTop, akRight]
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 305
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 17
        ItemIndex = -1
        TabOrder = 4
        OnChange = comboGmgIadampChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Constant damping (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Adaptive damping (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Relative reduced residual (2)'
          end>
      end
      object comboGmgIsc: TJvImageComboBox
        Left = 296
        Top = 167
        Width = 297
        Height = 23
        Style = csOwnerDrawVariable
        Anchors = [akLeft, akTop, akRight]
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 305
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 17
        ItemIndex = -1
        TabOrder = 5
        OnChange = comboGmgIscChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Coarsen rows, columns and layers (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Coarsen rows and columns (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Coarsen columns and layers (2)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Coarsen rows and layers (3)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'No coarsening (4)'
          end>
      end
      object rdeGmgDamp: TRbwDataEntry
        Left = 358
        Top = 8
        Width = 78
        Height = 22
        Hint = 
          'For linear problems, MXITER, should be 1 unless more than 50 inn' +
          'er iterations are required.  In that case MXITER could be as lar' +
          'ge as 10.  For nonlinear problems, MXITER may need to be larger ' +
          'but should usually be less than 100.'
        HelpContext = 910
        Color = clBtnFace
        Enabled = False
        TabOrder = 6
        Text = '1'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
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
      end
      item
      end
      item
      end
      item
      end
      item
      end
      item
      end
      item
      end
      item
        Control = cbGmbIunitmhc
      end
      item
      end
      item
      end>
  end
end
