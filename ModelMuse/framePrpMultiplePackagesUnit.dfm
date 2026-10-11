inherited framePrpMultiplePackages: TframePrpMultiplePackages
  Width = 665
  Height = 507
  ExplicitWidth = 665
  ExplicitHeight = 507
  inherited lblComments: TLabel
    StyleElements = [seFont, seClient, seBorder]
  end
  inherited lblPackage: TLabel
    StyleElements = [seFont, seClient, seBorder]
  end
  inherited memoComments: TMemo
    Width = 634
    Height = 75
    StyleElements = [seFont, seClient, seBorder]
    ExplicitWidth = 634
    ExplicitHeight = 75
  end
  object pcPrt: TPageControl [3]
    Left = 0
    Top = 151
    Width = 665
    Height = 356
    ActivePage = tabOptions
    Align = alBottom
    TabOrder = 1
    object tabOptions: TTabSheet
      Caption = 'Options'
      object grpMIP: TGroupBox
        Left = 279
        Top = 3
        Width = 274
        Height = 86
        Caption = 'Model Input Package (MIP)'
        TabOrder = 0
        object cbRetardationFactor: TCheckBox
          Left = 16
          Top = 24
          Width = 201
          Height = 17
          Caption = 'Use retardation factor'
          Enabled = False
          TabOrder = 0
        end
        object cbUseParticleStopZones: TCheckBox
          Left = 16
          Top = 48
          Width = 229
          Height = 17
          Caption = 'Use particle stop zones'
          Enabled = False
          TabOrder = 1
        end
      end
      object grpOutputControl: TGroupBox
        Left = 12
        Top = 97
        Width = 541
        Height = 226
        HelpType = htKeyword
        HelpKeyword = 'Particle-Transport'
        Caption = 'Output Control (OC) Package'
        TabOrder = 1
        object lblOutputFiles: TLabel
          Left = 11
          Top = 24
          Width = 64
          Height = 15
          Caption = 'Output Files'
        end
        object lblTrackEvents: TLabel
          Left = 267
          Top = 30
          Width = 65
          Height = 15
          Caption = 'Track Events'
        end
        object chklstOutputFiles: TJvCheckListBox
          Left = 11
          Top = 45
          Width = 250
          Height = 52
          DoubleBuffered = False
          Enabled = False
          ItemHeight = 17
          Items.Strings = (
            'Binary track file'
            'CSV track file')
          ParentDoubleBuffered = False
          ScrollWidth = 109
          TabOrder = 0
        end
        object chklstTrackEvents: TJvCheckListBox
          Left = 267
          Top = 51
          Width = 257
          Height = 172
          DoubleBuffered = False
          Enabled = False
          ItemHeight = 17
          Items.Strings = (
            'TRACK_RELEASE'
            'TRACK_EXIT'
            'TRACK_SUBFEATURE_EXIT'
            'TRACK_TIMESTEP'
            'TRACK_TERMINATE'
            'TRACK_WEAKSINK'
            'TRACK_USERTIME'
            'TRACK_DROPPED')
          ParentDoubleBuffered = False
          ScrollWidth = 167
          TabOrder = 1
        end
        object cbScratchBuffer: TCheckBox
          Left = 11
          Top = 112
          Width = 250
          Height = 17
          Caption = 'SCRATCH_BUFFER'
          TabOrder = 2
        end
      end
      object gbSimulation: TGroupBox
        Left = 12
        Top = 3
        Width = 261
        Height = 86
        Caption = 'Simulation'
        TabOrder = 2
        object cbRunAsSeparateSimulation: TCheckBox
          Left = 3
          Top = 24
          Width = 255
          Height = 17
          Caption = 'Run as a separate simulation'
          Enabled = False
          TabOrder = 0
        end
      end
    end
    object tabPrpPackages: TTabSheet
      Caption = 'PRP Packages'
      ImageIndex = 1
      inline framePrpPackages: TframeGrid
        Left = 0
        Top = 0
        Width = 657
        Height = 326
        Align = alClient
        Enabled = False
        TabOrder = 0
        ExplicitWidth = 657
        ExplicitHeight = 326
        inherited Panel: TPanel
          Top = 285
          Width = 657
          StyleElements = [seFont, seClient, seBorder]
          ExplicitTop = 285
          ExplicitWidth = 657
          inherited lbNumber: TLabel
            Width = 184
            Caption = 'Number of PRP packages in model'
            StyleElements = [seFont, seClient, seBorder]
            ExplicitWidth = 184
          end
          inherited sbAdd: TSpeedButton
            Left = 561
            ExplicitLeft = 561
          end
          inherited sbInsert: TSpeedButton
            Left = 592
            ExplicitLeft = 592
          end
          inherited sbDelete: TSpeedButton
            Left = 623
            ExplicitLeft = 623
          end
          inherited seNumber: TJvSpinEdit
            Left = 6
            Top = 8
            StyleElements = [seFont, seClient, seBorder]
            OnChange = framePrpPackagesseNumberChange
            ExplicitLeft = 6
            ExplicitTop = 8
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 657
          Height = 285
          ColCount = 2
          OnSelectCell = framePrpPackagesGridSelectCell
          OnSetEditText = framePrpPackagesGridSetEditText
          OnBeforeDrawCell = framePrpPackagesGridBeforeDrawCell
          OnEndUpdate = framePrpPackagesGridEndUpdate
          OnStateChange = framePrpPackagesGridStateChange
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
              MaxLength = 16
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
              ButtonFont.Height = -12
              ButtonFont.Name = 'Segoe UI'
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
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end>
          ExplicitWidth = 657
          ExplicitHeight = 285
        end
      end
    end
    object tabTrackTimes: TTabSheet
      Caption = 'Track Times'
      ImageIndex = 2
      inline frameTrackTimes: TframeGrid
        Left = 0
        Top = 57
        Width = 657
        Height = 269
        Align = alClient
        Enabled = False
        TabOrder = 0
        ExplicitTop = 57
        ExplicitWidth = 657
        ExplicitHeight = 269
        inherited Panel: TPanel
          Top = 228
          Width = 657
          StyleElements = [seFont, seClient, seBorder]
          ExplicitTop = 228
          ExplicitWidth = 657
          inherited lbNumber: TLabel
            StyleElements = [seFont, seClient, seBorder]
          end
          inherited sbAdd: TSpeedButton
            Left = 345
            ExplicitLeft = 331
          end
          inherited sbInsert: TSpeedButton
            Left = 408
            ExplicitLeft = 392
          end
          inherited sbDelete: TSpeedButton
            Left = 471
            ExplicitLeft = 453
          end
          inherited seNumber: TJvSpinEdit
            StyleElements = [seFont, seClient, seBorder]
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 657
          Height = 228
          OnExit = frameTrackTimesGridExit
          OnEndUpdate = frameTrackTimesGridEndUpdate
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
            end>
          ExplicitWidth = 657
          ExplicitHeight = 228
        end
      end
      object pnlTrackTime: TPanel
        Left = 0
        Top = 0
        Width = 657
        Height = 57
        Align = alTop
        TabOrder = 1
        object lblTrackTime: TLabel
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 649
          Height = 49
          Align = alClient
          Caption = 
            'Tracking times can be specified as specific times, by specific p' +
            'eriod, or both. In this tab, tracking times are specified by tim' +
            'e.'
          WordWrap = True
          ExplicitWidth = 625
          ExplicitHeight = 30
        end
      end
    end
    object tabTrackByStressPeriod: TTabSheet
      Caption = 'Track By Stress Period'
      ImageIndex = 3
      object pnlTrackPeriodData: TPanel
        Left = 0
        Top = 0
        Width = 657
        Height = 57
        Align = alTop
        TabOrder = 0
        object lblTrackPeriodData: TLabel
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 649
          Height = 49
          Align = alClient
          Caption = 
            'Tracking times can be specified as specific times, by specific p' +
            'eriod, or both. In this tab, tracking times are specified by per' +
            'iod. '
          WordWrap = True
          ExplicitWidth = 625
          ExplicitHeight = 30
        end
      end
      inline frameReleasePeriodData: TframeGrid
        Left = 0
        Top = 57
        Width = 657
        Height = 269
        Align = alClient
        Enabled = False
        TabOrder = 1
        ExplicitTop = 57
        ExplicitWidth = 657
        ExplicitHeight = 269
        inherited Panel: TPanel
          Top = 228
          Width = 657
          StyleElements = [seFont, seClient, seBorder]
          ExplicitTop = 228
          ExplicitWidth = 657
          inherited lbNumber: TLabel
            StyleElements = [seFont, seClient, seBorder]
          end
          inherited sbAdd: TSpeedButton
            Left = 345
            ExplicitLeft = 181
          end
          inherited sbInsert: TSpeedButton
            Left = 408
            ExplicitLeft = 215
          end
          inherited sbDelete: TSpeedButton
            Left = 471
            ExplicitLeft = 249
          end
          inherited seNumber: TJvSpinEdit
            Left = 6
            Top = 8
            StyleElements = [seFont, seClient, seBorder]
            ExplicitLeft = 6
            ExplicitTop = 8
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 657
          Height = 228
          ColCount = 8
          DefaultColWidth = 50
          OnEnter = frameReleasePeriodDataGridEnter
          OnExit = frameReleasePeriodDataGridExit
          OnEndUpdate = frameReleasePeriodDataGridEndUpdate
          Columns = <
            item
              AutoAdjustRowHeights = True
              AutoAdjustCaptionRowHeights = True
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
              AutoAdjustCaptionRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -12
              ButtonFont.Name = 'Segoe UI'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
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
              AutoAdjustCaptionRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -12
              ButtonFont.Name = 'Segoe UI'
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
                'Both'
                'Print'
                'Save')
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = False
              AutoAdjustCaptionRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -12
              ButtonFont.Name = 'Segoe UI'
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
              AutoAdjustCaptionRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -12
              ButtonFont.Name = 'Segoe UI'
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
              AutoAdjustCaptionRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -12
              ButtonFont.Name = 'Segoe UI'
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
              AutoAdjustRowHeights = True
              AutoAdjustCaptionRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -12
              ButtonFont.Name = 'Segoe UI'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4Integer
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
              AutoAdjustCaptionRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -12
              ButtonFont.Name = 'Segoe UI'
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
            end>
          ExplicitWidth = 657
          ExplicitHeight = 228
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
        Control = cbRunAsSeparateSimulation
      end
      item
        Control = cbRetardationFactor
      end
      item
        Control = cbUseParticleStopZones
      end
      item
        Control = chklstOutputFiles
      end
      item
        Control = chklstTrackEvents
      end
      item
        Control = framePrpPackages
      end
      item
        Control = frameTrackTimes
      end
      item
        Control = frameReleasePeriodData
      end>
  end
end
