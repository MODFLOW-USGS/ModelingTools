inherited frameMt3dBasicPkg: TframeMt3dBasicPkg
  Width = 528
  Height = 432
  ExplicitWidth = 528
  ExplicitHeight = 432
  DesignSize = (
    528
    432)
  inherited memoComments: TMemo
    Width = 497
    Height = 51
    ExplicitWidth = 497
    ExplicitHeight = 51
  end
  object pcMt3d_Basic: TPageControl [3]
    AlignWithMargins = True
    Left = 0
    Top = 120
    Width = 528
    Height = 312
    Margins.Left = 0
    Margins.Top = 120
    Margins.Right = 0
    Margins.Bottom = 0
    ActivePage = tabMT3D_Options
    Align = alClient
    TabOrder = 1
    object tabMT3D_Options: TTabSheet
      Caption = 'MT3D Options'
      object lblInactiveConcentration: TLabel
        Left = 83
        Top = 81
        Width = 212
        Height = 15
        Caption = 'Concentration at inactive cells (CINACT)'
      end
      object lblInitialConcentrationChoice: TLabel
        Left = 273
        Top = 141
        Width = 233
        Height = 15
        Caption = 'Method for using initial concentration file(s)'
      end
      object lblMinimumSaturatedFraction: TLabel
        Left = 83
        Top = 109
        Width = 205
        Height = 15
        Caption = 'Minimum saturated fraction (THKMIN)'
      end
      object lblVersion: TLabel
        Left = 148
        Top = 20
        Width = 72
        Height = 15
        Caption = 'MT3D Version'
      end
      object grpInitialConcentrationTimes: TGroupBox
        Left = 8
        Top = 181
        Width = 509
        Height = 57
        Caption = 'Concentration file transport step for initial concentrations'
        TabOrder = 0
        object lblStressPeriod: TLabel
          Left = 16
          Top = 24
          Width = 67
          Height = 15
          Caption = 'Stress period'
        end
        object lblTimeStep: TLabel
          Left = 176
          Top = 24
          Width = 51
          Height = 15
          Caption = 'Time step'
        end
        object lblTransportStep: TLabel
          Left = 318
          Top = 24
          Width = 74
          Height = 15
          Caption = 'Transport step'
        end
        object seStressPeriod: TJvSpinEdit
          Left = 98
          Top = 22
          Width = 71
          Height = 28
          MaxValue = 2147483647.000000000000000000
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          TabOrder = 2
        end
        object seTimeStep: TJvSpinEdit
          Left = 241
          Top = 21
          Width = 71
          Height = 28
          MaxValue = 2147483647.000000000000000000
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          TabOrder = 0
        end
        object seTransportStep: TJvSpinEdit
          Left = 408
          Top = 21
          Width = 71
          Height = 28
          MaxValue = 2147483647.000000000000000000
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          TabOrder = 1
        end
      end
      object comboInitialConcentrationChoice: TComboBox
        Left = 12
        Top = 138
        Width = 255
        Height = 23
        Style = csDropDownList
        TabOrder = 1
        OnChange = comboInitialConcentrationChoiceChange
        Items.Strings = (
          'Use a specific transport step'
          'Use first transport step in file')
      end
      object edMassUnit: TLabeledEdit
        Left = 12
        Top = 51
        Width = 50
        Height = 28
        EditLabel.Width = 99
        EditLabel.Height = 15
        EditLabel.Caption = 'Mass unit (MUNIT)'
        Enabled = False
        LabelPosition = lpRight
        MaxLength = 4
        TabOrder = 2
        Text = ''
      end
      object rdeInactiveConcentration: TRbwDataEntry
        Left = 12
        Top = 78
        Width = 65
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeMinimumSaturatedFraction: TRbwDataEntry
        Left = 12
        Top = 106
        Width = 65
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
      object comboVersion: TComboBox
        Left = 12
        Top = 17
        Width = 130
        Height = 23
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 5
        Text = 'MT3D-USGS'
        OnChange = comboVersionChange
        Items.Strings = (
          'MT3D-USGS'
          'MT3DMS')
      end
    end
    object tabMT3D_USGS_Options: TTabSheet
      Caption = 'MT3D-USGS Options'
      ImageIndex = 2
      object chklstOptions: TJvgCheckListBox
        Left = 0
        Top = 0
        Width = 520
        Height = 282
        Align = alClient
        Enabled = False
        Items.Strings = (
          'Generate text FTL for for debugging'
          'Allow flow through dry cells with UPW package (DRYCELL)'
          'Print flow-transport link file (FTLPRINT)'
          
            'Print messages indicating the wetting and drying of model cells ' +
            '(inverse of NOWETDRYPRINT)'
          
            'Include mass flowing through dry model cells in global mass bala' +
            'nce calculations (inverse of OMITDRYCELLBUDGET)'
          
            'Use alternative formulation for simulating adsorbed mass (ALTWTS' +
            'ORB)')
        TabOrder = 0
        ItemStyle.Gradient.Active = False
        ItemStyle.Gradient.Orientation = fgdHorizontal
        ItemStyle.Color = clBtnFace
        ItemStyle.DelineateColor = clBlack
        ItemStyle.Font.Charset = ANSI_CHARSET
        ItemStyle.Font.Color = clWindowText
        ItemStyle.Font.Height = -16
        ItemStyle.Font.Name = 'Arial Narrow'
        ItemStyle.Font.Style = []
        ItemStyle.Bevel.Inner = bvNone
        ItemStyle.Bevel.Outer = bvNone
        ItemStyle.Bevel.Bold = False
        ItemStyle.TextStyle = fstNone
        ItemSelStyle.Gradient.Active = False
        ItemSelStyle.Gradient.Orientation = fgdHorizontal
        ItemSelStyle.Color = clBtnShadow
        ItemSelStyle.DelineateColor = clBlack
        ItemSelStyle.Font.Charset = ANSI_CHARSET
        ItemSelStyle.Font.Color = clWindowText
        ItemSelStyle.Font.Height = -16
        ItemSelStyle.Font.Name = 'Arial Narrow'
        ItemSelStyle.Font.Style = []
        ItemSelStyle.Bevel.Inner = bvNone
        ItemSelStyle.Bevel.Outer = bvNone
        ItemSelStyle.Bevel.Bold = False
        ItemSelStyle.TextStyle = fstNone
        ItemHeight = 40
        TransparentColor = clOlive
        Options = [fboExcludeGlyphs, fboHotTrack, fboWordWrap]
        ChangeGlyphColor.FromColor = clBlack
        ChangeGlyphColor.ToColor = clWhite
        ExplicitHeight = 277
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
        Control = edMassUnit
      end
      item
        Control = rdeInactiveConcentration
      end
      item
        Control = rdeMinimumSaturatedFraction
      end>
    OnEnabledChange = rcSelectionControllerEnabledChange
  end
  object dlgOpenSelectFile: TOpenDialog
    Filter = 'Concentration file (*.ucn)|*.ucn|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 368
    Top = 152
  end
end
