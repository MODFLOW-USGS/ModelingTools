inherited frameModpathSelection: TframeModpathSelection
  Width = 608
  Height = 508
  ExplicitWidth = 608
  ExplicitHeight = 508
  DesignSize = (
    608
    508)
  inherited memoComments: TMemo
    Width = 589
    Height = 67
    ExplicitWidth = 589
    ExplicitHeight = 67
  end
  object pcModpath: TPageControl [3]
    Left = 3
    Top = 135
    Width = 600
    Height = 364
    ActivePage = tabResponse
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object tabResponse: TTabSheet
      Caption = 'Response file options'
      ImageIndex = 1
      object lblTrackingDirection: TLabel
        Left = 387
        Top = 249
        Width = 197
        Height = 15
        Caption = 'Tracking direction (TrackingDirection)'
      end
      object lblWeakSinkTreatment: TLabel
        Left = 3
        Top = 249
        Width = 225
        Height = 15
        Caption = 'Treatment of weak sinks (WeakSinkOption)'
      end
      object lblWeakSinkThreshold: TLabel
        Left = 89
        Top = 314
        Width = 159
        Height = 15
        Caption = 'Weak sink fractional threshold'
      end
      object lblStopZone: TLabel
        Left = 105
        Top = 166
        Width = 224
        Height = 15
        Caption = 'Zone in which to stop particles (StopZone)'
      end
      object lblWhichEndpoints: TLabel
        Left = 3
        Top = 191
        Width = 200
        Height = 15
        Caption = 'Which endpoints should be recorded?'
      end
      object lblReferenceTime: TLabel
        Left = 387
        Top = 174
        Width = 86
        Height = 45
        Caption = 'Reference time for simulation (ReferenceTime)'
        WordWrap = True
      end
      object lblEvtSink: TLabel
        Left = 154
        Top = 56
        Width = 215
        Height = 15
        Caption = 'Treatment of evapotranspiration (IEVTTP)'
      end
      object lblRchSource: TLabel
        Left = 154
        Top = 94
        Width = 167
        Height = 15
        Caption = 'Treatment of recharge (IRCHTP)'
      end
      object comboTrackingDirection: TJvImageComboBox
        Left = 387
        Top = 272
        Width = 160
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 160
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = 0
        TabOrder = 9
        OnChange = comboTrackingDirectionChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Forward'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Backward'
          end>
      end
      object comboWeakSinkTreatment: TJvImageComboBox
        Left = 3
        Top = 272
        Width = 350
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 350
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = -1
        TabOrder = 8
        OnChange = comboWeakSinkTreatmentChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Pass through'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Stop'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Stop if exiting flow fraction is above threshold'
          end>
      end
      object rdeWeakSinkThreshold: TRbwDataEntry
        Left = 3
        Top = 306
        Width = 80
        Height = 22
        TabOrder = 10
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object cbStopInZone: TCheckBox
        Left = 3
        Top = 123
        Width = 310
        Height = 42
        Caption = 'Use zone arrays (ZoneArrayOption)'
        Enabled = False
        TabOrder = 4
        WordWrap = True
        OnClick = cbStopInZoneClick
      end
      object rdeStopZone: TRbwDataEntry
        Left = 3
        Top = 163
        Width = 96
        Height = 22
        TabOrder = 5
        Text = '2'
        DataType = dtInteger
        Max = 2.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object comboWhichEndpoints: TJvImageComboBox
        Left = 3
        Top = 215
        Width = 350
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 350
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
            Text = 'All'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Particles entering designated zone'
          end>
      end
      object rgOutputMode: TRadioGroup
        Left = 194
        Top = 3
        Width = 395
        Height = 54
        Caption = 'Output mode (SimulationType)'
        Columns = 2
        Enabled = False
        ItemIndex = 0
        Items.Strings = (
          'End points'
          'Pathlines'
          'Time series'
          'Pathlines + Time series')
        TabOrder = 1
        OnClick = rgOutputModeClick
      end
      object rdeReferenceTime: TRbwDataEntry
        Left = 387
        Top = 215
        Width = 145
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 7
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rgModpathVersion: TRadioGroup
        Left = 3
        Top = 3
        Width = 185
        Height = 43
        Caption = 'Modpath version'
        Columns = 3
        Enabled = False
        ItemIndex = 1
        Items.Strings = (
          '5'
          '6'
          '7')
        TabOrder = 0
        OnClick = rgModpathVersionClick
      end
      object comboEvtSink: TJvImageComboBox
        Left = 3
        Top = 52
        Width = 145
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
        TabOrder = 2
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Internal sink'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Top face'
          end>
      end
      object comboRchSource: TJvImageComboBox
        Left = 3
        Top = 90
        Width = 145
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
        TabOrder = 3
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Internal source'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Top face'
          end>
      end
    end
    object tabVersion5Options: TTabSheet
      Caption = 'Version 5 options'
      ImageIndex = 4
      object lblBeginningTime: TLabel
        Left = 154
        Top = 159
        Width = 215
        Height = 15
        Caption = 'Beginning time (BeginPeriod  BeginStep)'
      end
      object lblEndingTime: TLabel
        Left = 154
        Top = 193
        Width = 178
        Height = 15
        Caption = 'Ending time (EndPeriod  EndStep)'
      end
      object lblMaxSize: TLabel
        Left = 154
        Top = 231
        Width = 129
        Height = 90
        Caption = 
          'Maximum size of composite budget file (MAXSIZ)'#13#10'(0 = use default' +
          ' value in MODPATH = 15,000,000 bytes)'
        WordWrap = True
      end
      object lblErrorTolerance: TLabel
        Left = 323
        Top = 3
        Width = 77
        Height = 15
        Caption = 'Error tolerance'
      end
      object lblMaxTime: TLabel
        Left = 323
        Top = 108
        Width = 128
        Height = 15
        Caption = 'Maximum tracking time'
      end
      object lblReleaseTime: TLabel
        Left = 323
        Top = 56
        Width = 179
        Height = 15
        Caption = 'Release time (backwards tracking)'
      end
      object cbCompact: TCheckBox
        Left = 3
        Top = 108
        Width = 257
        Height = 17
        Caption = 'Compact format (Options)'
        Enabled = False
        TabOrder = 6
      end
      object cbBinary: TCheckBox
        Left = 3
        Top = 131
        Width = 257
        Height = 17
        Caption = 'Binary output file (Options)'
        Enabled = False
        TabOrder = 8
      end
      object rdeBeginningTime: TRbwDataEntry
        Left = 3
        Top = 154
        Width = 145
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 9
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeEndingTime: TRbwDataEntry
        Left = 3
        Top = 190
        Width = 145
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 10
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeMaxSize: TRbwDataEntry
        Left = 3
        Top = 238
        Width = 145
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 11
        Text = '0'
        DataType = dtInteger
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeErrorTolerance: TRbwDataEntry
        Left = 323
        Top = 27
        Width = 145
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object cbComputeBudget: TCheckBox
        Left = 3
        Top = 3
        Width = 257
        Height = 17
        Caption = 'Compute budget in all cells'
        Enabled = False
        TabOrder = 0
        OnClick = cbComputeBudgetClick
      end
      object cbSummarize: TCheckBox
        Left = 3
        Top = 26
        Width = 286
        Height = 15
        Caption = 'Summarize final status of particle'
        Enabled = False
        TabOrder = 1
      end
      object cbBigBudget: TCheckBox
        Left = 3
        Top = 47
        Width = 286
        Height = 17
        Caption = 'Allow unlimited budget file size'
        Checked = True
        Enabled = False
        State = cbChecked
        TabOrder = 3
      end
      object cbStopAfterMaxTime: TCheckBox
        Left = 3
        Top = 67
        Width = 234
        Height = 35
        Caption = 'Stop computing paths after a specified maximum time'
        Enabled = False
        TabOrder = 4
        WordWrap = True
        OnClick = cbStopAfterMaxTimeClick
      end
      object rdeMaxTime: TRbwDataEntry
        Left = 323
        Top = 128
        Width = 145
        Height = 22
        TabOrder = 7
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeReleaseTime: TRbwDataEntry
        Left = 323
        Top = 80
        Width = 145
        Height = 22
        TabOrder = 5
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
    end
    object tsVersion6Options: TTabSheet
      Caption = 'Version 6 && 7 options'
      ImageIndex = 3
      object lblWeakSource: TLabel
        Left = 3
        Top = 7
        Width = 253
        Height = 15
        Caption = 'Treatment of weak sources (WeakSourceOption)'
      end
      object lblStopOption: TLabel
        Left = 3
        Top = 63
        Width = 243
        Height = 15
        Caption = 'How partcle tracking terminated (StopOption)'
      end
      object lblStopTime: TLabel
        Left = 89
        Top = 122
        Width = 189
        Height = 15
        Caption = 'Maximum tracking time (StopTime)'
      end
      object lblBudget: TLabel
        Left = 3
        Top = 147
        Width = 213
        Height = 15
        Caption = 'Budget checking (BudgetOutputOption)'
      end
      object lblTraceID: TLabel
        Left = 89
        Top = 206
        Width = 131
        Height = 15
        Caption = 'Particle to track (TraceID)'
      end
      object lblAdvObs: TLabel
        Left = 0
        Top = 254
        Width = 166
        Height = 30
        Caption = 'Advection observations (AdvectiveObservationsOption)'
        WordWrap = True
      end
      object lblUzfIface: TLabel
        Left = 379
        Top = 63
        Width = 122
        Height = 15
        Caption = 'Treatment of UZF flows'
      end
      object lblMnw2Iface: TLabel
        Left = 379
        Top = 111
        Width = 138
        Height = 15
        Caption = 'Treatment of MNW2 flows'
      end
      object lblResIface: TLabel
        Left = 379
        Top = 167
        Width = 149
        Height = 15
        Caption = 'Treatment of Reservoir flows'
      end
      object lblSfrIface: TLabel
        Left = 379
        Top = 223
        Width = 120
        Height = 15
        Caption = 'Treatment of SFR flows'
      end
      object lblEtsIface: TLabel
        Left = 379
        Top = 7
        Width = 119
        Height = 15
        Caption = 'Treatment of ETS flows'
      end
      object lblLakIface: TLabel
        Left = 379
        Top = 276
        Width = 125
        Height = 15
        Caption = 'Treatment of Lake flows'
      end
      object comboWeakSource: TJvImageComboBox
        Left = 3
        Top = 29
        Width = 148
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 350
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = -1
        TabOrder = 0
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Pass through (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Stop (2)'
          end>
      end
      object comboStopOption: TJvImageComboBox
        Left = 3
        Top = 82
        Width = 350
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 350
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = -1
        TabOrder = 2
        OnChange = comboStopOptionChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Stop at beginning or end of simulation (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Stop at termination points (steady-state) (2)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Stop at specified time (3)'
          end>
      end
      object rdeStopTime: TRbwDataEntry
        Left = 3
        Top = 119
        Width = 80
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
      object comboBudget: TJvImageComboBox
        Left = 3
        Top = 166
        Width = 350
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 350
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = -1
        TabOrder = 6
        OnChange = comboBudgetChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'None (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Summary printed (2)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Detailed budgets for specified cells (3)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Trace mode (4)'
          end>
      end
      object rdeTraceID: TRbwDataEntry
        Left = 3
        Top = 203
        Width = 80
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 8
        Text = '1'
        DataType = dtInteger
        Max = 1.000000000000000000
        Min = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object chkRetardation: TCheckBox
        Left = 3
        Top = 231
        Width = 262
        Height = 17
        Caption = 'Use retardation (RetardationOption)'
        Enabled = False
        TabOrder = 9
      end
      object comboAdvObs: TJvImageComboBox
        Left = 3
        Top = 302
        Width = 353
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 353
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = -1
        TabOrder = 12
        OnChange = comboStopOptionChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'None (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'All time points (2)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Final time point (3)'
          end>
      end
      object comboUzfIface: TJvImageComboBox
        Left = 379
        Top = 82
        Width = 145
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
        TabOrder = 3
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Internal sink'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Top face'
          end>
      end
      object comboMnw2Iface: TJvImageComboBox
        Left = 379
        Top = 131
        Width = 145
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
        TabOrder = 5
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Internal sink'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Top face'
          end>
      end
      object comboResIface: TJvImageComboBox
        Left = 379
        Top = 186
        Width = 145
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
        TabOrder = 7
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Internal sink'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Top face'
          end>
      end
      object comboSfrIface: TJvImageComboBox
        Left = 379
        Top = 243
        Width = 145
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
        TabOrder = 10
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Internal sink'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Top face'
          end>
      end
      object comboEtsIface: TJvImageComboBox
        Left = 379
        Top = 29
        Width = 145
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
        TabOrder = 1
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Internal sink'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Top face'
          end>
      end
      object comboLakIface: TJvImageComboBox
        Left = 379
        Top = 295
        Width = 145
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
        TabOrder = 11
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Internal sink'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Top face'
          end>
      end
    end
    object tabOutputTimes: TTabSheet
      Caption = 'Output times'
      ImageIndex = 2
      object lblTimeMethod: TLabel
        Left = 3
        Top = 14
        Width = 247
        Height = 15
        Caption = 'Method of specifying times (TimePointOption)'
      end
      object lblParticleInterval: TLabel
        Left = 3
        Top = 70
        Width = 255
        Height = 15
        Caption = 'Time interval for output (ReleaseTimeIncrement)'
      end
      object lblMaxTimes: TLabel
        Left = 3
        Top = 122
        Width = 95
        Height = 60
        Caption = 'Maximum number of times allowed (TimePointCount)'
        WordWrap = True
      end
      object gbTime: TJvGroupBox
        Left = 380
        Top = 3
        Width = 201
        Height = 252
        Caption = 'Output times (TimePoints)'
        TabOrder = 0
        object lblTimeCount: TLabel
          Left = 66
          Top = 179
          Width = 95
          Height = 30
          Caption = 'Number of times (TimePointCount)'
          WordWrap = True
        end
        object sbAddRow: TSpeedButton
          Left = 38
          Top = 218
          Width = 23
          Height = 22
          Hint = 'Add row|Add a row below the bottom row.'
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
            CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
            FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
            FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
          ParentShowHint = False
          ShowHint = True
          OnClick = sbAddRowClick
        end
        object sbInsertRow: TSpeedButton
          Left = 88
          Top = 220
          Width = 23
          Height = 22
          Hint = 'Insert row|Insert a row above the selected row.'
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
            FF0FFFFF0FFFFFFFFF0FFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
            CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
            FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
          ParentShowHint = False
          ShowHint = True
          OnClick = sbInsertRowClick
        end
        object sbDeleteRow: TSpeedButton
          Left = 139
          Top = 220
          Width = 23
          Height = 22
          Hint = 'Delete row|Delete the selected row.'
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFF000000FFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
            0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
            0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
            0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFF000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000FFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF
            000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
          ParentShowHint = False
          ShowHint = True
          OnClick = sbDeleteRowClick
        end
        object rdgTimes: TRbwDataGrid4
          Left = 3
          Top = 20
          Width = 195
          Height = 149
          ColCount = 2
          DefaultColWidth = 20
          Enabled = False
          FixedCols = 1
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
          TabOrder = 0
          ExtendedAutoDistributeText = False
          AutoMultiEdit = True
          AutoDistributeText = True
          AutoIncreaseColCount = False
          AutoIncreaseRowCount = True
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          OnBeforeDrawCell = rdgTimesBeforeDrawCell
          OnEndUpdate = rdgTimesEndUpdate
          ColorRangeSelection = True
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
              ComboUsed = False
              Format = rcf4Real
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end>
          WordWrapRowCaptions = False
          RowHeights = (
            24
            22)
        end
        object seTimeCount: TJvSpinEdit
          Left = 3
          Top = 183
          Width = 57
          Height = 21
          CheckMinValue = True
          ButtonKind = bkClassic
          Enabled = False
          TabOrder = 1
          OnChange = seTimeCountChange
        end
      end
      object comboTimeMethod: TJvImageComboBox
        Left = 3
        Top = 32
        Width = 200
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 200
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = 0
        TabOrder = 1
        OnChange = comboTimeMethodChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Uniformly spaced times'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Specified times'
          end>
      end
      object rdeParticleInterval: TRbwDataEntry
        Left = 3
        Top = 88
        Width = 145
        Height = 22
        TabOrder = 2
        Text = '1'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeMaxTimes: TRbwDataEntry
        Left = 3
        Top = 159
        Width = 145
        Height = 22
        TabOrder = 3
        Text = '1000'
        DataType = dtInteger
        Max = 1.000000000000000000
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
        Control = cbBinary
      end
      item
        Control = cbCompact
      end
      item
        Control = rdeBeginningTime
      end
      item
        Control = rgOutputMode
      end
      item
        Control = comboTrackingDirection
      end
      item
        Control = comboWeakSinkTreatment
      end
      item
        Control = cbStopInZone
      end
      item
        Control = cbStopAfterMaxTime
      end
      item
        Control = cbBigBudget
      end
      item
        Control = cbSummarize
      end
      item
        Control = cbComputeBudget
      end
      item
        Control = rgModpathVersion
      end
      item
        Control = comboWeakSource
      end
      item
        Control = comboStopOption
      end
      item
        Control = comboBudget
      end
      item
        Control = chkRetardation
      end
      item
        Control = comboAdvObs
      end
      item
        Control = comboEtsIface
      end
      item
        Control = comboUzfIface
      end
      item
        Control = comboMnw2Iface
      end
      item
        Control = comboResIface
      end
      item
        Control = comboSfrIface
      end
      item
        Control = comboLakIface
      end>
  end
end
