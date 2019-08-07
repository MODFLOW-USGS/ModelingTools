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
    ActivePage = tabSpecies
    Align = alClient
    TabOrder = 1
    object tabSpecies: TTabSheet
      Caption = 'Chemical Species'
      ImageIndex = 1
      object pnlSpecies: TPanel
        Left = 0
        Top = 0
        Width = 520
        Height = 284
        Align = alClient
        TabOrder = 0
        object Splitter1: TSplitter
          Left = 249
          Top = 1
          Width = 5
          Height = 282
          ExplicitLeft = 145
          ExplicitHeight = 115
        end
        inline frameGridImmobile: TframeGrid
          Left = 254
          Top = 1
          Width = 265
          Height = 282
          Align = alClient
          Enabled = False
          TabOrder = 0
          ExplicitLeft = 254
          ExplicitTop = 1
          ExplicitWidth = 265
          ExplicitHeight = 282
          inherited Panel: TPanel
            Top = 241
            Width = 265
            ExplicitTop = 241
            ExplicitWidth = 265
            inherited sbAdd: TSpeedButton
              Left = 147
              ExplicitLeft = 147
            end
            inherited sbInsert: TSpeedButton
              Left = 175
              ExplicitLeft = 175
            end
            inherited sbDelete: TSpeedButton
              Left = 204
              ExplicitLeft = 204
            end
          end
          inherited Grid: TRbwDataGrid4
            Width = 265
            Height = 241
            ColCount = 3
            OnSelectCell = frameGridImmobileGridSelectCell
            OnButtonClick = frameGridSpeciesGridButtonClick
            Columns = <
              item
                AutoAdjustRowHeights = True
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
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = True
                ButtonCaption = '...'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = False
                ComboUsed = False
                Format = rcf4Boolean
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
                ButtonCaption = 'Select...'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = True
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
            ExplicitWidth = 265
            ExplicitHeight = 241
          end
        end
        inline frameGridMobile: TframeGrid
          Left = 1
          Top = 1
          Width = 248
          Height = 282
          Align = alLeft
          Enabled = False
          TabOrder = 1
          ExplicitLeft = 1
          ExplicitTop = 1
          ExplicitWidth = 248
          ExplicitHeight = 282
          inherited Panel: TPanel
            Top = 241
            Width = 248
            ExplicitTop = 241
            ExplicitWidth = 248
          end
          inherited Grid: TRbwDataGrid4
            Width = 248
            Height = 241
            ColCount = 3
            OnSelectCell = frameGridMobileGridSelectCell
            OnButtonClick = frameGridSpeciesGridButtonClick
            OnStateChange = frameSpeciesGridStateChange
            Columns = <
              item
                AutoAdjustRowHeights = True
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
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = True
                ButtonCaption = '...'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = False
                ComboUsed = False
                Format = rcf4Boolean
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
                ButtonCaption = 'Select...'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = True
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
                AutoAdjustColWidths = True
              end>
            ExplicitWidth = 248
            ExplicitHeight = 241
          end
        end
      end
    end
    object tabMT3D_Options: TTabSheet
      Caption = 'MT3D Options'
      object lblInactiveConcentration: TLabel
        Left = 83
        Top = 81
        Width = 193
        Height = 13
        Caption = 'Concentration at inactive cells (CINACT)'
      end
      object lblInitialConcentrationChoice: TLabel
        Left = 273
        Top = 141
        Width = 207
        Height = 13
        Caption = 'Method for using initial concentration file(s)'
      end
      object lblMinimumSaturatedFraction: TLabel
        Left = 83
        Top = 109
        Width = 179
        Height = 13
        Caption = 'Minimum saturated fraction (THKMIN)'
      end
      object lblVersion: TLabel
        Left = 148
        Top = 20
        Width = 65
        Height = 13
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
          Width = 63
          Height = 13
          Caption = 'Stress period'
        end
        object lblTimeStep: TLabel
          Left = 176
          Top = 24
          Width = 46
          Height = 13
          Caption = 'Time step'
        end
        object lblTransportStep: TLabel
          Left = 318
          Top = 24
          Width = 71
          Height = 13
          Caption = 'Transport step'
        end
        object seStressPeriod: TJvSpinEdit
          Left = 98
          Top = 22
          Width = 71
          Height = 21
          MaxValue = 2147483647.000000000000000000
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          TabOrder = 2
        end
        object seTimeStep: TJvSpinEdit
          Left = 241
          Top = 21
          Width = 71
          Height = 21
          MaxValue = 2147483647.000000000000000000
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          TabOrder = 0
        end
        object seTransportStep: TJvSpinEdit
          Left = 408
          Top = 21
          Width = 71
          Height = 21
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
        Height = 21
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
        Height = 21
        EditLabel.Width = 88
        EditLabel.Height = 13
        EditLabel.Caption = 'Mass unit (MUNIT)'
        Enabled = False
        LabelPosition = lpRight
        MaxLength = 4
        TabOrder = 2
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
        Height = 21
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
        Height = 284
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
      end
      item
        Control = frameGridImmobile
      end
      item
        Control = frameGridMobile
      end>
    OnEnabledChange = rcSelectionControllerEnabledChange
  end
  object dlgOpenSelectFile: TOpenDialog
    Filter = 'Concentration file (*.ucn)|*.ucn|All files (*.*)|*.*'
    Left = 368
    Top = 152
  end
end
