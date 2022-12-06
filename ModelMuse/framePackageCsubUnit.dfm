inherited framePackageCsub: TframePackageCsub
  Width = 589
  Height = 514
  ExplicitWidth = 589
  ExplicitHeight = 514
  DesignSize = (
    589
    514)
  inherited memoComments: TMemo
    Top = 58
    Width = 558
    Height = 63
    ExplicitTop = 58
    ExplicitWidth = 558
    ExplicitHeight = 63
  end
  object pcCsub: TPageControl [3]
    Left = 0
    Top = 127
    Width = 589
    Height = 387
    ActivePage = tabOptions
    Align = alBottom
    TabOrder = 1
    object tabInterbeds: TTabSheet
      Caption = 'Interbeds'
      ImageIndex = 2
      inline frameInterbeds: TframeGrid
        Left = 0
        Top = 0
        Width = 581
        Height = 359
        Align = alClient
        Enabled = False
        TabOrder = 0
        ExplicitWidth = 581
        ExplicitHeight = 341
        inherited Panel: TPanel
          Top = 318
          Width = 581
          ExplicitTop = 300
          ExplicitWidth = 581
          inherited lbNumber: TLabel
            Width = 150
            Caption = 'Number of interbed systems'
            ExplicitWidth = 150
          end
          inherited sbAdd: TSpeedButton
            Left = 334
            Hint = 'Add interbed|Add an interbed after the last interbed'
            ExplicitLeft = 235
          end
          inherited sbInsert: TSpeedButton
            Left = 363
            Hint = 'Insert interbed|Insert an interbed before the selected interbed.'
            ExplicitLeft = 363
          end
          inherited sbDelete: TSpeedButton
            Left = 392
            Hint = 'Delete interbed|Delete the selected interbed.'
            ExplicitLeft = 392
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 581
          Height = 318
          ColCount = 2
          Columns = <
            item
              AutoAdjustRowHeights = False
              AutoAdjustCaptionRowHeights = False
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
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = False
              AutoAdjustCaptionRowHeights = False
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
              ComboUsed = True
              Format = rcf4String
              LimitToList = True
              MaxLength = 0
              ParentButtonFont = False
              PickList.Strings = (
                'no-delay'
                'delay')
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end>
          ExplicitWidth = 581
          ExplicitHeight = 300
        end
      end
    end
    object tabOptions: TTabSheet
      Caption = 'Options'
      object lblGamma: TLabel
        Left = 155
        Top = 6
        Width = 168
        Height = 15
        Caption = 'Unit weight of water (gammaw)'
      end
      object lblBeta: TLabel
        Left = 155
        Top = 34
        Width = 162
        Height = 15
        Caption = 'Compressibility of water (beta)'
      end
      object lblInterbedThicknessMethod: TLabel
        Left = 106
        Top = 210
        Width = 238
        Height = 30
        Caption = 'Method for specifying thickness of interbeds (CELL_FRACTION)'
        WordWrap = True
      end
      object lblseNDelayCells: TLabel
        Left = 82
        Top = 111
        Width = 356
        Height = 15
        Caption = 'Number of nodes used to discretize delay interbeds (NDELAYCELLS)'
      end
      object lblCompressionMethod: TLabel
        Left = 227
        Top = 146
        Width = 255
        Height = 15
        Caption = 'Compression method (COMPRESSION_INDICES)'
      end
      object rdeGamma: TRbwDataEntry
        Left = 3
        Top = 3
        Width = 145
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 0
        Text = '9806.65'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeBeta: TRbwDataEntry
        Left = 3
        Top = 31
        Width = 145
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 1
        Text = '4.6512e-10'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object cbHeadBased: TCheckBox
        Left = 3
        Top = 62
        Width = 353
        Height = 18
        Caption = 'Use head-based formulation (HEAD_BASED)'
        Enabled = False
        TabOrder = 2
        OnClick = cbHeadBasedClick
      end
      object cbPreconsolidationHeadUsed: TCheckBox
        Left = 3
        Top = 85
        Width = 567
        Height = 18
        Caption = 'Specify preconsolidation heads (INITIAL_PRECONSOLIDATION_HEAD)'
        Enabled = False
        TabOrder = 3
      end
      object seNDelayCells: TJvSpinEdit
        Left = 3
        Top = 108
        Width = 73
        Height = 21
        CheckMaxValue = False
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 4
      end
      object cbUpdateMaterialProperties: TCheckBox
        Left = 3
        Top = 184
        Width = 542
        Height = 18
        Caption = 'Update material properties (UPDATE_MATERIAL_PROPERTIES)'
        Enabled = False
        TabOrder = 5
      end
      object comboInterbedThicknessMethod: TJvImageComboBox
        Left = 3
        Top = 207
        Width = 97
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
        TabOrder = 6
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Thickness'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Cell fraction'
          end>
      end
      object cbEffectiveStressLag: TCheckBox
        Left = 3
        Top = 301
        Width = 558
        Height = 18
        Caption = 
          'Use effective stress from previous time step (EFFECTIVE_STRESS_L' +
          'AG)'
        Enabled = False
        TabOrder = 7
      end
      object cbSpecifyInitialDelayHead: TCheckBox
        Left = 3
        Top = 281
        Width = 567
        Height = 18
        Caption = 
          'Specify absolute initial delay bed head (SPECIFIED_INITIAL_DELAY' +
          '_HEAD)'
        Enabled = False
        TabOrder = 8
      end
      object cbSpecifyInitialPreconsolidationStress: TCheckBox
        Left = 3
        Top = 240
        Width = 567
        Height = 36
        Caption = 
          'Specify initial preconsolidation stress (SPECIFIED_INITIAL_PRECO' +
          'NSOLIDATION_STRESS)'
        Enabled = False
        TabOrder = 9
        WordWrap = True
      end
      object comboCompressionMethod: TJvImageComboBox
        Left = 3
        Top = 143
        Width = 218
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 218
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = 0
        TabOrder = 10
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Use elastic specific storage'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Use recompression index'
          end>
      end
      object cbPackageConvergence: TCheckBox
        Left = 3
        Top = 324
        Width = 455
        Height = 18
        Caption = 'Write package convergence (PACKAGE_CONVERGENCE)'
        TabOrder = 11
      end
    end
    object tabOutputTypes: TTabSheet
      Caption = 'Output types'
      ImageIndex = 1
      object chklstOutput: TCheckListBox
        Left = 0
        Top = 0
        Width = 581
        Height = 357
        Align = alClient
        Enabled = False
        ItemHeight = 15
        Items.Strings = (
          'Interbed strain (comma-separated values)'
          'Coarse strain (comma-separated values)'
          'Compaction (binary)'
          'Elastic compaction (binary)'
          'Inelastic compaction (binary)'
          'Interbed compaction (binary)'
          'Elastic coarse-grained compaction (binary)'
          'Z displacement (binary)')
        TabOrder = 0
        ExplicitHeight = 341
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
        Control = rdeGamma
      end
      item
        Control = rdeBeta
      end
      item
        Control = cbHeadBased
      end
      item
        Control = seNDelayCells
      end
      item
        Control = comboCompressionMethod
      end
      item
        Control = cbUpdateMaterialProperties
      end
      item
        Control = comboInterbedThicknessMethod
      end
      item
        Control = cbSpecifyInitialPreconsolidationStress
      end
      item
        Control = cbSpecifyInitialDelayHead
      end
      item
        Control = cbSpecifyInitialDelayHead
      end
      item
        Control = cbEffectiveStressLag
      end
      item
        Control = chklstOutput
      end
      item
        Control = frameInterbeds
      end>
    OnEnabledChange = rcSelectionControllerEnabledChange
  end
end
