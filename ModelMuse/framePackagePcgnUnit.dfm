inherited framePackagePcgn: TframePackagePcgn
  Width = 600
  Height = 550
  ExplicitWidth = 600
  ExplicitHeight = 550
  DesignSize = (
    600
    550)
  inherited memoComments: TMemo
    Width = 569
    ExplicitWidth = 569
  end
  object pcControls: TPageControl [3]
    Left = 0
    Top = 157
    Width = 600
    Height = 393
    ActivePage = tabNonLinear
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object tabBasic: TTabSheet
      Caption = 'Basic'
      object lblIter_mo: TLabel
        Left = 79
        Top = 19
        Width = 299
        Height = 15
        Caption = 'Maximum number of Picard (outer) iterations (ITER_MO)'
      end
      object lblIter_mi: TLabel
        Left = 79
        Top = 46
        Width = 282
        Height = 15
        Caption = 'Maximum number of PCG (inner) iterations (ITER_MI)'
      end
      object lblCLOSE_R: TLabel
        Left = 79
        Top = 73
        Width = 319
        Height = 15
        Caption = 'The residual-based stopping criterion for iteration (CLOSE_R)'
      end
      object lblClose_H: TLabel
        Left = 79
        Top = 101
        Width = 306
        Height = 15
        Caption = 'The head-based stopping criterion for iteration (CLOSE_H)'
      end
      object lblRelax: TLabel
        Left = 79
        Top = 129
        Width = 157
        Height = 15
        Caption = 'Relaxation parameter (RELAX)'
      end
      object lblIfill: TLabel
        Left = 79
        Top = 157
        Width = 217
        Height = 15
        Caption = 'Fill level of the MIC preconditioner (IFILL)'
      end
      object seIter_mo: TJvSpinEdit
        Left = 3
        Top = 16
        Width = 70
        Height = 24
        CheckMaxValue = False
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 0
      end
      object seIter_mi: TJvSpinEdit
        Left = 3
        Top = 43
        Width = 70
        Height = 24
        CheckMaxValue = False
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 1
      end
      object rdeCLOSE_R: TRbwDataEntry
        Left = 3
        Top = 70
        Width = 70
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeClose_H: TRbwDataEntry
        Left = 3
        Top = 98
        Width = 70
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeRelax: TRbwDataEntry
        Left = 3
        Top = 126
        Width = 70
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 4
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object seIfill: TJvSpinEdit
        Left = 3
        Top = 154
        Width = 70
        Height = 24
        MaxValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 5
      end
      object cbUnit_pc: TCheckBox
        Left = 3
        Top = 181
        Width = 356
        Height = 17
        Caption = 'Save progress for the inner PCG iteration to file (UNIT_PC)'
        Enabled = False
        TabOrder = 6
      end
      object cbUnit_ts: TCheckBox
        Left = 3
        Top = 204
        Width = 356
        Height = 17
        Caption = 'Save time in the PCG solver to file (UNIT_TS)'
        Enabled = False
        TabOrder = 7
      end
    end
    object tabNonLinear: TTabSheet
      Caption = 'Non-Linear'
      ImageIndex = 1
      object lblDampingMode: TLabel
        Left = 143
        Top = 6
        Width = 136
        Height = 15
        Caption = 'Damping mode (ADAMP)'
      end
      object lblDamp: TLabel
        Left = 143
        Top = 32
        Width = 192
        Height = 15
        Caption = 'Damping parameter control (DAMP)'
      end
      object lblDamp_Lb: TLabel
        Left = 143
        Top = 63
        Width = 177
        Height = 15
        Caption = 'Lower limit for DAMP (DAMP_LB)'
      end
      object lblDamp_D: TLabel
        Left = 143
        Top = 95
        Width = 237
        Height = 15
        Caption = 'Rate parameter for adjusting DAMP (RATE_D)'
      end
      object lblChglimit: TLabel
        Left = 143
        Top = 123
        Width = 191
        Height = 15
        Caption = 'Maximum head change (CHGLIMIT)'
      end
      object lblAcnvg: TLabel
        Left = 143
        Top = 151
        Width = 155
        Height = 15
        Caption = 'Convergence mode (ACNVG)'
      end
      object lblChvg_Lb: TLabel
        Left = 143
        Top = 180
        Width = 275
        Height = 15
        Caption = 'Minimum value for relative convergence (CNVG_LB)'
      end
      object lblMcnvg: TLabel
        Left = 143
        Top = 208
        Width = 332
        Height = 15
        Caption = 'Power factor for increasing PCG convergence criteria (MCNVG)'
      end
      object lblRate_C: TLabel
        Left = 143
        Top = 235
        Width = 237
        Height = 15
        Caption = 'Convergence enhancement control (RATE_C)'
      end
      object lblIpunit: TLabel
        Left = 143
        Top = 263
        Width = 265
        Height = 15
        Caption = 'Progress reporting for the Picard iteration (IPUNIT)'
      end
      object comboDampingMode: TJvImageComboBox
        Left = 3
        Top = 3
        Width = 134
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
        TabOrder = 0
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Ordinary (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Adaptive (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Enhanced (2)'
          end>
      end
      object rdeDamp: TRbwDataEntry
        Left = 3
        Top = 32
        Width = 134
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeDamp_Lb: TRbwDataEntry
        Left = 3
        Top = 60
        Width = 134
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeRate_D: TRbwDataEntry
        Left = 3
        Top = 92
        Width = 134
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeChglimit: TRbwDataEntry
        Left = 3
        Top = 120
        Width = 134
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 4
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object comboAcnvg: TJvImageComboBox
        Left = 3
        Top = 148
        Width = 134
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
        TabOrder = 5
        OnChange = comboAcnvgChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Standard (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Adaptive (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Enhanced (2)'
          end>
      end
      object rdeCnvg_Lb: TRbwDataEntry
        Left = 3
        Top = 177
        Width = 134
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 6
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object seMcnvg: TJvSpinEdit
        Left = 3
        Top = 205
        Width = 134
        Height = 21
        MaxValue = 6.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 7
      end
      object rdeRate_C: TRbwDataEntry
        Left = 3
        Top = 232
        Width = 134
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 8
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object comboIpunit: TJvImageComboBox
        Left = 3
        Top = 260
        Width = 134
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 145
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = 1
        TabOrder = 9
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'None (<0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Listing file (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'External file (>0)'
          end>
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
        Control = seIter_mo
      end
      item
        Control = seIter_mi
      end
      item
        Control = rdeCLOSE_R
      end
      item
        Control = rdeClose_H
      end
      item
        Control = rdeRelax
      end
      item
        Control = seIfill
      end
      item
        Control = cbUnit_pc
      end
      item
        Control = cbUnit_ts
      end
      item
        Control = comboDampingMode
      end
      item
        Control = rdeDamp
      end
      item
        Control = rdeDamp_Lb
      end
      item
        Control = rdeRate_D
      end
      item
        Control = rdeChglimit
      end
      item
        Control = comboAcnvg
      end
      item
        Control = comboIpunit
      end>
  end
end
