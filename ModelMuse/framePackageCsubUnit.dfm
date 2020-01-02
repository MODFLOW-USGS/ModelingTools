inherited framePackageCsub: TframePackageCsub
  Width = 589
  Height = 514
  ExplicitWidth = 589
  ExplicitHeight = 514
  DesignSize = (
    589
    514)
  inherited memoComments: TMemo
    Width = 558
  end
  object pcCsub: TPageControl [3]
    Left = 0
    Top = 157
    Width = 589
    Height = 357
    ActivePage = tabOptions
    Align = alBottom
    TabOrder = 1
    object tabOptions: TTabSheet
      Caption = 'Options'
      ExplicitLeft = 8
      ExplicitWidth = 414
      ExplicitHeight = 384
      object lblGamma: TLabel
        Left = 155
        Top = 6
        Width = 151
        Height = 13
        Caption = 'Unit weight of water (gammaw)'
      end
      object lblBeta: TLabel
        Left = 155
        Top = 34
        Width = 148
        Height = 13
        Caption = 'Compressibility of water (beta)'
      end
      object lblInterbedThicknessMethod: TLabel
        Left = 106
        Top = 210
        Width = 413
        Height = 26
        Caption = 'Method for specifying thickness of interbeds (CELL_FRACTION)'
        WordWrap = True
      end
      object lblseNDelayCells: TLabel
        Left = 82
        Top = 111
        Width = 324
        Height = 13
        Caption = 'Number of nodes used to discretize delay interbeds (NDELAYCELLS)'
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
        Height = 17
        Caption = 'Use head-based formulation (HEAD_BASED)'
        Enabled = False
        TabOrder = 2
      end
      object cbPreconsolidationHeadUsed: TCheckBox
        Left = 3
        Top = 85
        Width = 391
        Height = 17
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
      object cbUseCompressionIndicies: TCheckBox
        Left = 3
        Top = 146
        Width = 575
        Height = 32
        Caption = 
          'Specify recompression (CR) and compression (CC) indices (COMPRES' +
          'SION_INDICES)'
        Enabled = False
        TabOrder = 5
        WordWrap = True
      end
      object cbUpdateMaterialProperties: TCheckBox
        Left = 3
        Top = 184
        Width = 542
        Height = 17
        Caption = 'Update material properties (UPDATE_MATERIAL_PROPERTIES)'
        Enabled = False
        TabOrder = 6
      end
      object comboInterbedThicknessMethod: TJvImageComboBox
        Left = 3
        Top = 207
        Width = 97
        Height = 23
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 145
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 17
        ItemIndex = 0
        TabOrder = 7
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
        Height = 17
        Caption = 
          'Use effective stress from previous time step (EFFECTIVE_STRESS_L' +
          'AG)'
        Enabled = False
        TabOrder = 8
      end
      object cbSpecifyInitialDelayHead: TCheckBox
        Left = 3
        Top = 281
        Width = 567
        Height = 14
        Caption = 
          'Specify absolute initial delay bed head (SPECIFIED_INITIAL_DELAY' +
          '_HEAD)'
        Enabled = False
        TabOrder = 9
      end
      object cbSpecifyInitialPreconsolidationStress: TCheckBox
        Left = 3
        Top = 242
        Width = 567
        Height = 33
        Caption = 
          'Specify initial preconsolidation stress (SPECIFIED_INITIAL_PRECO' +
          'NSOLIDATION_STRESS)'
        Enabled = False
        TabOrder = 10
        WordWrap = True
      end
    end
    object tabOutputTypes: TTabSheet
      Caption = 'Output types'
      ImageIndex = 1
      ExplicitWidth = 414
      ExplicitHeight = 384
      object chklstOutput: TCheckListBox
        Left = 0
        Top = 0
        Width = 581
        Height = 329
        Align = alClient
        Enabled = False
        ItemHeight = 13
        Items.Strings = (
          'Interbed strain'
          'Coarse strain'
          'Compaction'
          'Elastic compaction'
          'Inelastic compaction'
          'Interbed compaction'
          'Coarse compaction'
          'Z displacement')
        TabOrder = 0
        ExplicitLeft = 64
        ExplicitTop = 80
        ExplicitWidth = 121
        ExplicitHeight = 97
      end
    end
    object tabInterbeds: TTabSheet
      Caption = 'Interbeds'
      ImageIndex = 2
      ExplicitWidth = 414
      ExplicitHeight = 384
      inline frameInterbeds: TframeGrid
        Left = 0
        Top = 0
        Width = 581
        Height = 329
        Align = alClient
        Enabled = False
        TabOrder = 0
        ExplicitLeft = 32
        ExplicitTop = 64
        inherited Panel: TPanel
          Top = 288
          Width = 581
          inherited lbNumber: TLabel
            Width = 135
            Caption = 'Number of interbed systems'
            ExplicitWidth = 135
          end
          inherited sbAdd: TSpeedButton
            Left = 334
            ExplicitLeft = 235
          end
          inherited sbInsert: TSpeedButton
            Left = 363
            ExplicitLeft = 363
          end
          inherited sbDelete: TSpeedButton
            Left = 392
            ExplicitLeft = 392
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 581
          Height = 288
          ColCount = 2
          Columns = <
            item
              AutoAdjustRowHeights = False
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
              AutoAdjustRowHeights = False
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
        end
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
        Control = cbPreconsolidationHeadUsed
      end
      item
        Control = seNDelayCells
      end
      item
        Control = cbUseCompressionIndicies
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
  end
end
