inherited frameScreenObjectSwrReach: TframeScreenObjectSwrReach
  Width = 575
  Height = 382
  ExplicitWidth = 575
  ExplicitHeight = 382
  object pgcSwr: TPageControl
    Left = 0
    Top = 0
    Width = 575
    Height = 382
    ActivePage = tabSteady
    Align = alClient
    TabOrder = 0
    object tabSteady: TTabSheet
      Caption = 'Steady'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlSteady: TPanel
        Left = 0
        Top = 0
        Width = 567
        Height = 177
        Align = alTop
        TabOrder = 0
        object lblGroupNumber: TLabel
          Left = 183
          Top = 101
          Width = 142
          Height = 16
          Caption = 'Group number (IRGNUM)'
        end
        object lblReachLength: TLabel
          Left = 8
          Top = 66
          Width = 117
          Height = 16
          Margins.Left = 8
          Caption = 'Reach length (RLEN)'
        end
        object lblRouteType: TLabel
          Left = 8
          Top = 12
          Width = 174
          Height = 16
          Caption = 'Type of routing (IROUTETYPE)'
        end
        object lblObservationType: TLabel
          Left = 10
          Top = 148
          Width = 102
          Height = 16
          Caption = 'Observation types'
        end
        object rdeGroupNumber: TRbwDataEntry
          Left = 183
          Top = 121
          Width = 119
          Height = 22
          Color = clBtnFace
          Enabled = False
          TabOrder = 4
          Text = '0'
          DataType = dtInteger
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
        object cbGrouped: TCheckBox
          Left = 8
          Top = 125
          Width = 167
          Height = 17
          Caption = 'Grouped (IRGNUM)'
          TabOrder = 5
          OnClick = cbGroupedClick
        end
        object cbMultilayer: TCheckBox
          Left = 8
          Top = 102
          Width = 169
          Height = 17
          Caption = 'Multilayer (KRCH)'
          TabOrder = 3
          OnClick = cbMultilayerClick
        end
        object edReachLength: TRbwEdit
          Left = 183
          Top = 63
          Width = 274
          Height = 24
          TabOrder = 2
        end
        object btnEditReachLength: TButton
          Left = 478
          Top = 61
          Width = 75
          Height = 25
          Caption = 'Edit F()...'
          TabOrder = 1
        end
        object comboRouteType: TJvImageComboBox
          Left = 8
          Top = 31
          Width = 345
          Height = 26
          Style = csOwnerDrawVariable
          ButtonStyle = fsLighter
          DroppedWidth = 474
          ImageHeight = 0
          ImageWidth = 0
          ItemHeight = 20
          ItemIndex = 2
          TabOrder = 0
          Items = <
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Level-pool reservoir routing (1)'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Tilted-pool reservoir routing (2)'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Diffusive-wave approximation (3)'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Kinematic-wave approximation (4)'
            end>
        end
        object cbbObservationTypes: TJvCheckedComboBox
          Left = 183
          Top = 150
          Width = 370
          Height = 24
          Items.Strings = (
            'Stage'
            'Depth'
            'Bottom'
            'Flow'
            'Structure'
            'Base Flow')
          CapSelectAll = '&Select all'
          CapDeSelectAll = '&Deselect all'
          CapInvertAll = '&Invert all'
          TabOrder = 6
        end
      end
      object grpConnections: TGroupBox
        Left = 0
        Top = 177
        Width = 567
        Height = 174
        Align = alClient
        Caption = 'Connections'
        TabOrder = 1
        object lblDescription: TLabel
          AlignWithMargins = True
          Left = 5
          Top = 21
          Width = 557
          Height = 32
          Align = alTop
          Caption = 
            'If multiple reaches are defined by the same object, connections ' +
            'between them are defined automatically. Use this page to define ' +
            'connections with other objects.'
          WordWrap = True
          ExplicitWidth = 522
        end
        inline frameConnections: TframeGrid
          Left = 2
          Top = 56
          Width = 563
          Height = 116
          Align = alClient
          TabOrder = 0
          ExplicitLeft = 2
          ExplicitTop = 53
          ExplicitWidth = 563
          ExplicitHeight = 119
          inherited Panel: TPanel
            Top = 75
            Width = 563
            ExplicitTop = 78
            ExplicitWidth = 563
            inherited sbAdd: TSpeedButton
              Left = 294
              ExplicitLeft = 296
            end
            inherited sbInsert: TSpeedButton
              Left = 348
              ExplicitLeft = 351
            end
            inherited sbDelete: TSpeedButton
              Left = 402
              ExplicitLeft = 405
            end
            inherited seNumber: TJvSpinEdit
              Height = 24
              ExplicitHeight = 24
            end
          end
          inherited Grid: TRbwDataGrid4
            Width = 563
            Height = 75
            ColCount = 3
            OnSelectCell = frameConnectionsGridSelectCell
            OnSetEditText = frameConnectionsGridSetEditText
            OnBeforeDrawCell = frameConnectionsGridBeforeDrawCell
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
                ComboUsed = True
                Format = rcf4String
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                PickList.Strings = (
                  'Object'
                  'Specific reach'
                  'Overlapping reaches')
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
                ButtonFont.Height = -11
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = False
                ComboUsed = False
                Format = rcf4String
                LimitToList = True
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end>
            ExplicitWidth = 563
            ExplicitHeight = 78
          end
        end
      end
    end
    object tabTransient: TTabSheet
      Caption = 'Transient'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      inline frameSwr: TframeScreenObjectNoParam
        Left = 0
        Top = 0
        Width = 567
        Height = 351
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 567
        ExplicitHeight = 351
        inherited pnlBottom: TPanel
          Top = 305
          Width = 567
          ExplicitTop = 305
          ExplicitWidth = 567
          inherited btnDelete: TBitBtn
            Left = 479
            ExplicitLeft = 479
          end
          inherited btnInsert: TBitBtn
            Left = 395
            ExplicitLeft = 395
          end
        end
        inherited pnlTop: TPanel
          Width = 567
          ExplicitWidth = 567
          inherited pnlCaption: TPanel
            Width = 565
            ExplicitWidth = 565
          end
        end
        inherited pnlGrid: TPanel
          Width = 567
          Height = 280
          ExplicitWidth = 567
          ExplicitHeight = 280
          inherited pnlEditGrid: TPanel
            Width = 565
            ExplicitWidth = 565
          end
          inherited rdgModflowBoundary: TRbwDataGrid4
            Width = 565
            Height = 228
            ColCount = 6
            OnSelectCell = frameSwrdgModflowBoundarySelectCell
            OnSetEditText = frameSwrdgModflowBoundarySetEditText
            Columns = <
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 35
                CheckMax = False
                CheckMin = False
                ComboUsed = True
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
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 35
                CheckMax = False
                CheckMin = False
                ComboUsed = True
                Format = rcf4Real
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
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 35
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
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 35
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
                ButtonFont.Height = -13
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
                  'Active (>0)'
                  'Inactive (0)'
                  'Specified Stage (<0)')
                WordWrapCaptions = False
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = False
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = '...'
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
            ExplicitWidth = 565
            ExplicitHeight = 228
          end
        end
      end
    end
  end
end
