inherited frmSutraTimes: TfrmSutraTimes
  HelpType = htKeyword
  HelpKeyword = 'SUTRA_Time_Controls_Dialog_Box'
  Caption = 'SUTRA Time Controls'
  ClientHeight = 549
  ClientWidth = 697
  ExplicitWidth = 713
  ExplicitHeight = 588
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 505
    Width = 697
    Height = 44
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      697
      44)
    object btnHelp: TBitBtn
      Left = 345
      Top = 6
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 459
      Top = 6
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 573
      Top = 6
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnConvertTimeUnits: TButton
      Left = 5
      Top = 6
      Width = 161
      Height = 33
      Caption = 'Convert time units'
      TabOrder = 3
      OnClick = btnConvertTimeUnitsClick
    end
  end
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 697
    Height = 505
    ActivePage = tabSchedules
    Align = alClient
    TabOrder = 0
    object tabSchedules: TTabSheet
      Caption = 'Schedules'
      object pnl1: TPanel
        Left = 0
        Top = 0
        Width = 161
        Height = 472
        Align = alLeft
        Caption = 'pnl1'
        TabOrder = 0
        DesignSize = (
          161
          472)
        object sbAddUnit: TSpeedButton
          Left = 15
          Top = 442
          Width = 23
          Height = 22
          Hint = 'Add time schedule|Add a new time schedule'
          Anchors = []
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
          OnClick = sbAddUnitClick
          ExplicitTop = 461
        end
        object sbDeleteUnit: TSpeedButton
          Left = 102
          Top = 442
          Width = 23
          Height = 22
          Hint = 'Delete time schedule|Delete the selected time schedule'
          Anchors = []
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
          OnClick = sbDeleteUnitClick
          ExplicitTop = 448
        end
        object sbUp: TSpeedButton
          Left = 44
          Top = 442
          Width = 23
          Height = 22
          Hint = 'Move up|Move the selected time schedule up'
          Anchors = []
          Enabled = False
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000333
            3333333333777F33333333333309033333333333337F7F333333333333090333
            33333333337F7F33333333333309033333333333337F7F333333333333090333
            33333333337F7F33333333333309033333333333FF7F7FFFF333333000090000
            3333333777737777F333333099999990333333373F3333373333333309999903
            333333337F33337F33333333099999033333333373F333733333333330999033
            3333333337F337F3333333333099903333333333373F37333333333333090333
            33333333337F7F33333333333309033333333333337373333333333333303333
            333333333337F333333333333330333333333333333733333333}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          OnClick = sbUpClick
          ExplicitTop = 448
        end
        object sbDown: TSpeedButton
          Left = 73
          Top = 442
          Width = 23
          Height = 22
          Hint = 'Move down|Move the selected time schedule down'
          Anchors = []
          Enabled = False
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333303333
            333333333337F33333333333333033333333333333373F333333333333090333
            33333333337F7F33333333333309033333333333337373F33333333330999033
            3333333337F337F33333333330999033333333333733373F3333333309999903
            333333337F33337F33333333099999033333333373333373F333333099999990
            33333337FFFF3FF7F33333300009000033333337777F77773333333333090333
            33333333337F7F33333333333309033333333333337F7F333333333333090333
            33333333337F7F33333333333309033333333333337F7F333333333333090333
            33333333337F7F33333333333300033333333333337773333333}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          OnClick = sbDownClick
          ExplicitTop = 448
        end
        object vstScedules: TVirtualStringTree
          Left = 1
          Top = 1
          Width = 159
          Height = 434
          Align = alTop
          Anchors = [akLeft, akTop, akRight, akBottom]
          Colors.BorderColor = 15987699
          Colors.DisabledColor = clGray
          Colors.DropMarkColor = 15385233
          Colors.DropTargetColor = 15385233
          Colors.DropTargetBorderColor = 15385233
          Colors.FocusedSelectionColor = 15385233
          Colors.FocusedSelectionBorderColor = 15385233
          Colors.GridLineColor = 15987699
          Colors.HeaderHotColor = clBlack
          Colors.HotColor = clBlack
          Colors.SelectionRectangleBlendColor = 15385233
          Colors.SelectionRectangleBorderColor = 15385233
          Colors.SelectionTextColor = clBlack
          Colors.TreeLineColor = 9471874
          Colors.UnfocusedColor = clGray
          Colors.UnfocusedSelectionColor = 15385233
          Colors.UnfocusedSelectionBorderColor = 15385233
          Header.AutoSizeIndex = 0
          Header.MainColumn = -1
          TabOrder = 0
          TreeOptions.SelectionOptions = [toFullRowSelect]
          OnGetText = vstScedulesGetText
          OnGetNodeDataSize = vstScedulesGetNodeDataSize
          OnNodeClick = vstScedulesNodeClick
          Columns = <>
        end
      end
      object grp1: TGroupBox
        Left = 161
        Top = 0
        Width = 528
        Height = 472
        Align = alClient
        Caption = 'Temporal Control and Solution Cycling Data -> Dataset 6'
        TabOrder = 1
        DesignSize = (
          528
          472)
        object lblScheduleType: TLabel
          Left = 158
          Top = 61
          Width = 261
          Height = 18
          Caption = 'Type of time step schedule (SCHTYP)'
        end
        object lblScaleFactor: TLabel
          Left = 158
          Top = 163
          Width = 212
          Height = 18
          Caption = 'Scale factor for times (SCALT)'
        end
        object comboScheduleType: TComboBox
          Left = 6
          Top = 57
          Width = 145
          Height = 26
          Style = csDropDownList
          TabOrder = 1
          OnChange = comboScheduleTypeChange
          Items.Strings = (
            'Time List'
            'Time Cycle'
            'Step List'
            'Step Cycle')
        end
        object jvpglstTemporal: TJvPageList
          Left = 8
          Top = 188
          Width = 517
          Height = 281
          ActivePage = jvspTimeCycle
          PropagateEnable = False
          Anchors = [akLeft, akTop, akRight, akBottom]
          object jvspTimeList: TJvStandardPage
            Left = 0
            Top = 0
            Width = 517
            Height = 281
            Caption = 'jvspTimeList'
            object grp5: TGroupBox
              Left = 0
              Top = 0
              Width = 517
              Height = 281
              Align = alClient
              Caption = 'List of Times'
              TabOrder = 0
              inline frameTimes: TframeGrid
                Left = 2
                Top = 20
                Width = 513
                Height = 259
                Align = alClient
                TabOrder = 0
                ExplicitLeft = 2
                ExplicitTop = 20
                ExplicitWidth = 513
                ExplicitHeight = 259
                inherited Panel: TPanel
                  Top = 219
                  Width = 513
                  Height = 40
                  ExplicitTop = 219
                  ExplicitWidth = 513
                  ExplicitHeight = 40
                  inherited lbNumber: TLabel
                    Width = 55
                    Height = 18
                    ExplicitWidth = 55
                    ExplicitHeight = 18
                  end
                  inherited sbAdd: TSpeedButton
                    Left = 267
                    Width = 26
                    Height = 26
                    ExplicitLeft = 247
                    ExplicitWidth = 26
                    ExplicitHeight = 26
                  end
                  inherited sbInsert: TSpeedButton
                    Left = 316
                    Width = 26
                    Height = 26
                    ExplicitLeft = 293
                    ExplicitWidth = 26
                    ExplicitHeight = 26
                  end
                  inherited sbDelete: TSpeedButton
                    Left = 366
                    Width = 26
                    Height = 26
                    ExplicitLeft = 339
                    ExplicitWidth = 26
                    ExplicitHeight = 26
                  end
                  inherited seNumber: TJvSpinEdit
                    Height = 26
                    ExplicitHeight = 26
                  end
                end
                inherited Grid: TRbwDataGrid4
                  Width = 513
                  Height = 219
                  OnSelectCell = frameTimesGridSelectCell
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
                      Format = rcf4Real
                      LimitToList = False
                      MaxLength = 0
                      ParentButtonFont = False
                      WordWrapCaptions = True
                      WordWrapCells = False
                      CaseSensitivePicklist = False
                      CheckStyle = csCheck
                      AutoAdjustColWidths = True
                    end>
                  ExplicitWidth = 513
                  ExplicitHeight = 219
                end
              end
            end
          end
          object jvspTimeCycle: TJvStandardPage
            Left = 0
            Top = 0
            Width = 517
            Height = 281
            Caption = 'jvspTimeCycle'
            DesignSize = (
              517
              281)
            object grp3: TGroupBox
              Left = 3
              Top = 3
              Width = 514
              Height = 137
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Basic Time Step Control'
              TabOrder = 0
              object lblNTMAX: TLabel
                Left = 119
                Top = 26
                Width = 385
                Height = 18
                Caption = 'Maximum number of times after the initial time (NTMAX)'
              end
              object lblTimei: TLabel
                Left = 119
                Top = 54
                Width = 122
                Height = 18
                Caption = 'Initial time (TIMEI)'
              end
              object lblTimel: TLabel
                Left = 119
                Top = 83
                Width = 148
                Height = 18
                Caption = 'Limiting time (TIMEL)'
              end
              object lblTimec: TLabel
                Left = 119
                Top = 111
                Width = 203
                Height = 18
                Caption = 'Initial time increment (TIMEC)'
              end
              object rdeTimei: TRbwDataEntry
                Left = 7
                Top = 51
                Width = 106
                Height = 22
                TabOrder = 1
                Text = '0'
                DataType = dtReal
                Max = 1.000000000000000000
                CheckMin = True
                ChangeDisabledColor = True
              end
              object rdeTimel: TRbwDataEntry
                Left = 7
                Top = 80
                Width = 106
                Height = 22
                TabOrder = 2
                Text = '1e99'
                DataType = dtReal
                Max = 1.000000000000000000
                CheckMin = True
                ChangeDisabledColor = True
              end
              object rdeTimec: TRbwDataEntry
                Left = 7
                Top = 108
                Width = 106
                Height = 22
                TabOrder = 3
                Text = '1'
                DataType = dtReal
                Max = 1.000000000000000000
                CheckMin = True
                ChangeDisabledColor = True
              end
              object seNtmax: TJvSpinEdit
                Left = 7
                Top = 23
                Width = 106
                Height = 26
                MaxValue = 2147483647.000000000000000000
                MinValue = 1.000000000000000000
                Value = 1.000000000000000000
                TabOrder = 0
              end
            end
            object grp4: TGroupBox
              Left = 3
              Top = 147
              Width = 514
              Height = 135
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Time-Step Multiplier Control'
              TabOrder = 1
              object lblNTCYC: TLabel
                Left = 119
                Top = 26
                Width = 393
                Height = 36
                Caption = 'Number of time steps in time step change cycle (NTCYC)'#13#10
              end
              object lblTcmult: TLabel
                Left = 119
                Top = 55
                Width = 322
                Height = 36
                Caption = 'Multiplier for time step change cycle (TCMULT)'#13#10
              end
              object lblDTMAX: TLabel
                Left = 119
                Top = 111
                Width = 294
                Height = 18
                Caption = 'Maximum allowed time step size (TCMAX)'
              end
              object lblTCMIN: TLabel
                Left = 119
                Top = 83
                Width = 282
                Height = 18
                Caption = 'Minimum allowed time step size (TCMIN)'
              end
              object rdeTcmult: TRbwDataEntry
                Left = 7
                Top = 52
                Width = 106
                Height = 22
                TabOrder = 1
                Text = '1.'
                DataType = dtReal
                Max = 1.000000000000000000
                CheckMin = True
                ChangeDisabledColor = True
              end
              object rdeDTMAX: TRbwDataEntry
                Left = 7
                Top = 108
                Width = 106
                Height = 22
                TabOrder = 3
                Text = '1e99'
                DataType = dtReal
                Max = 1.000000000000000000
                ChangeDisabledColor = True
              end
              object rdeTCMIN: TRbwDataEntry
                Left = 7
                Top = 80
                Width = 106
                Height = 22
                TabOrder = 2
                Text = '1e-20'
                DataType = dtReal
                Max = 1.000000000000000000
                ChangeDisabledColor = True
              end
              object seNTCYC: TJvSpinEdit
                Left = 7
                Top = 23
                Width = 106
                Height = 26
                MaxValue = 2147483647.000000000000000000
                MinValue = 1.000000000000000000
                Value = 9999.000000000000000000
                TabOrder = 0
              end
            end
          end
          object jvspStepList: TJvStandardPage
            Left = 0
            Top = 0
            Width = 517
            Height = 281
            Caption = 'jvspStepList'
            object grp2: TGroupBox
              Left = 0
              Top = 0
              Width = 517
              Height = 281
              Align = alClient
              Caption = 'List of Time Steps'
              TabOrder = 0
              inline frameSteps: TframeGrid
                Left = 2
                Top = 20
                Width = 513
                Height = 259
                Align = alClient
                TabOrder = 0
                ExplicitLeft = 2
                ExplicitTop = 20
                ExplicitWidth = 513
                ExplicitHeight = 259
                inherited Panel: TPanel
                  Top = 218
                  Width = 513
                  ExplicitTop = 218
                  ExplicitWidth = 513
                  inherited lbNumber: TLabel
                    Width = 55
                    Height = 18
                    ExplicitWidth = 55
                    ExplicitHeight = 18
                  end
                  inherited sbAdd: TSpeedButton
                    Left = 267
                    Width = 26
                    Height = 26
                    ExplicitLeft = 247
                    ExplicitWidth = 26
                    ExplicitHeight = 26
                  end
                  inherited sbInsert: TSpeedButton
                    Left = 316
                    Width = 26
                    Height = 26
                    ExplicitLeft = 293
                    ExplicitWidth = 26
                    ExplicitHeight = 26
                  end
                  inherited sbDelete: TSpeedButton
                    Left = 366
                    Width = 26
                    Height = 26
                    ExplicitLeft = 339
                    ExplicitWidth = 26
                    ExplicitHeight = 26
                  end
                  inherited seNumber: TJvSpinEdit
                    Height = 26
                    ExplicitHeight = 26
                  end
                end
                inherited Grid: TRbwDataGrid4
                  Width = 513
                  Height = 218
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
                      CheckMin = True
                      ComboUsed = False
                      Format = rcf4Integer
                      LimitToList = False
                      Max = 1.000000000000000000
                      MaxLength = 0
                      Min = 1.000000000000000000
                      ParentButtonFont = False
                      WordWrapCaptions = True
                      WordWrapCells = False
                      CaseSensitivePicklist = False
                      CheckStyle = csCheck
                      AutoAdjustColWidths = True
                    end>
                  ExplicitWidth = 513
                  ExplicitHeight = 218
                end
              end
            end
          end
          object jvspStepCyle: TJvStandardPage
            Left = 0
            Top = 0
            Width = 517
            Height = 281
            Caption = 'jvspStepCyle'
            DesignSize = (
              517
              281)
            object grp6: TGroupBox
              Left = 3
              Top = 3
              Width = 514
              Height = 166
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Basic Time Step Control'
              TabOrder = 0
              object lblMaxSteps: TLabel
                Left = 143
                Top = 23
                Width = 326
                Height = 18
                Caption = 'Maximum number of time step cycles (NSMAX)'
              end
              object lblInitialStep: TLabel
                Left = 143
                Top = 55
                Width = 165
                Height = 18
                Caption = 'Initial time step (ISTEPI)'
              end
              object lblLimitingStep: TLabel
                Left = 143
                Top = 87
                Width = 191
                Height = 18
                Caption = 'Limiting time step (ISTEPL)'
              end
              object lblTimeStepIncrement: TLabel
                Left = 143
                Top = 119
                Width = 211
                Height = 18
                Caption = 'Time step increment (ISTEPC)'
              end
              object seMaxSteps: TJvSpinEdit
                Left = 7
                Top = 20
                Width = 130
                Height = 26
                MaxValue = 2147483647.000000000000000000
                MinValue = 1.000000000000000000
                Value = 1.000000000000000000
                TabOrder = 0
              end
              object seInitialStep: TJvSpinEdit
                Left = 7
                Top = 52
                Width = 130
                Height = 26
                MaxValue = 2147483647.000000000000000000
                TabOrder = 1
              end
              object seLimitingStep: TJvSpinEdit
                Left = 7
                Top = 84
                Width = 130
                Height = 26
                MaxValue = 2147483647.000000000000000000
                MinValue = 1.000000000000000000
                Value = 2147483647.000000000000000000
                TabOrder = 2
              end
              object seTimeStepIncrement: TJvSpinEdit
                Left = 8
                Top = 116
                Width = 129
                Height = 26
                MaxValue = 2147483647.000000000000000000
                MinValue = 1.000000000000000000
                Value = 1.000000000000000000
                TabOrder = 3
              end
            end
          end
        end
        object rgMannerOfTimeSpecification: TRadioGroup
          Left = 6
          Top = 89
          Width = 504
          Height = 65
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Manner of time specification (CREFT)'
          ItemIndex = 1
          Items.Strings = (
            'ABSOLUTE time (simulation clock time)'
            'ELAPSED time (time since simulation starting time TICS)')
          TabOrder = 2
          OnClick = rgMannerOfTimeSpecificationClick
        end
        object lbledName: TLabeledEdit
          Left = 6
          Top = 25
          Width = 145
          Height = 26
          EditLabel.Width = 191
          EditLabel.Height = 18
          EditLabel.Caption = 'Schedule name (SCHNAM)'
          LabelPosition = lpRight
          LabelSpacing = 6
          MaxLength = 10
          TabOrder = 0
          OnChange = lbledNameChange
        end
        object rdeScaleFactor: TRbwDataEntry
          Left = 6
          Top = 160
          Width = 145
          Height = 22
          TabOrder = 3
          Text = '1'
          DataType = dtReal
          Max = 1.000000000000000000
          Min = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
      end
    end
    object tabOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblInitialtime: TLabel
        Left = 87
        Top = 29
        Width = 378
        Height = 18
        Caption = 'Time at which the initial conditions are specified (TICS)'
      end
      object lblPressureCycles: TLabel
        Left = 87
        Top = 80
        Width = 385
        Height = 18
        Caption = 'Number of steps in the pressure solution cycle (NPCYC)'
      end
      object lblTransportCycles: TLabel
        Left = 87
        Top = 113
        Width = 385
        Height = 18
        Caption = 'Number of steps in the transport solution cycle (NUCYC)'
      end
      object jvgrphdrICS: TJvGroupHeader
        Left = 3
        Top = 3
        Width = 510
        Height = 17
        Caption = 'ICS file, data set 1'
      end
      object jvgrphdrInput6: TJvGroupHeader
        Left = 3
        Top = 54
        Width = 510
        Height = 17
        Caption = 'INP file, data set 6'
      end
      object rdeInitialTime: TRbwDataEntry
        Left = 3
        Top = 26
        Width = 78
        Height = 22
        TabOrder = 0
        Text = '0'
        OnChange = rdeInitialTimeChange
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object sePressureCycles: TJvSpinEdit
        Left = 3
        Top = 77
        Width = 78
        Height = 26
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 1
        OnChange = sePressureCyclesChange
      end
      object seTransportCycles: TJvSpinEdit
        Left = 3
        Top = 110
        Width = 78
        Height = 26
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 2
        OnChange = seTransportCyclesChange
      end
    end
  end
end
