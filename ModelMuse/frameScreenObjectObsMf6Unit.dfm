object frameScreenObjectObsMf6: TframeScreenObjectObsMf6
  Left = 0
  Top = 0
  Width = 590
  Height = 519
  TabOrder = 0
  object pnlCaption: TPanel
    Left = 0
    Top = 0
    Width = 590
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pgcMain: TPageControl
    Left = 0
    Top = 71
    Width = 590
    Height = 448
    ActivePage = tabUZF
    Align = alClient
    TabOrder = 1
    object tabBasic: TTabSheet
      Caption = 'Basic'
      object lblTypesOfFlowObservation: TLabel
        Left = 3
        Top = 74
        Width = 134
        Height = 15
        Caption = 'Types of flow observation'
      end
      object lblBoundaryFlowObservations: TLabel
        Left = 3
        Top = 200
        Width = 147
        Height = 15
        Caption = 'Boundary flow observations'
      end
      object lblSpecies: TLabel
        Left = 296
        Top = 74
        Width = 91
        Height = 15
        Caption = 'Chemical species'
      end
      object lblGwtObs: TLabel
        Left = 296
        Top = 195
        Width = 159
        Height = 15
        Caption = 'Type of mass flow observation'
      end
      object cbHeadObservation: TCheckBox
        Left = 3
        Top = 3
        Width = 278
        Height = 17
        Caption = 'Head observation (head)'
        TabOrder = 0
        OnClick = cbHeadObservationClick
      end
      object cbDrawdownObservation: TCheckBox
        Left = 3
        Top = 26
        Width = 278
        Height = 17
        Caption = 'Drawdown observation (drawdown)'
        TabOrder = 1
        OnClick = cbHeadObservationClick
      end
      object cbGroundwaterFlowObservation: TCheckBox
        Left = 3
        Top = 51
        Width = 390
        Height = 17
        Caption = 'Groundwater flow observation (flow-ja-face)'
        TabOrder = 2
        OnClick = cbGroundwaterFlowObservationClick
      end
      object chklstFlowObs: TCheckListBox
        Left = 3
        Top = 93
        Width = 278
        Height = 97
        Enabled = False
        Items.Strings = (
          'Nearest horizontal neighbor'
          'All horizontal neighbors'
          'Overlying neighbor'
          'Underlying neighbor')
        TabOrder = 3
        OnClick = chklstFlowObsClick
      end
      object chklstBoundaryFlow: TCheckListBox
        Left = 3
        Top = 219
        Width = 278
        Height = 142
        Items.Strings = (
          'CHD flows'
          'DRN flows'
          'EVT flows'
          'GHB flows'
          'RCH flows'
          'RIV flows'
          'WEL flows'
          'To MVR flows'
          'Well Reduction')
        TabOrder = 4
        OnClickCheck = chklstBoundaryFlowClickCheck
      end
      object cbConcentration: TCheckBox
        Left = 296
        Top = 3
        Width = 217
        Height = 42
        Caption = 'Concentration observation (conc)'
        TabOrder = 5
        WordWrap = True
        OnClick = cbConcentrationClick
      end
      object chklstGWT: TCheckListBox
        Left = 296
        Top = 221
        Width = 282
        Height = 97
        Items.Strings = (
          'CNC (Mass flow at spec. conc.)'
          'SRC (Mass source loading rate)')
        TabOrder = 6
        OnClickCheck = chklstGWTClickCheck
      end
      object chklstChemSpecies: TCheckListBox
        Left = 296
        Top = 93
        Width = 275
        Height = 97
        TabOrder = 7
      end
    end
    object tabMAW: TTabSheet
      Caption = 'MAW'
      ImageIndex = 1
      object lblMaw: TLabel
        Left = 3
        Top = 16
        Width = 29
        Height = 15
        Caption = 'MAW'
      end
      object lblMwt: TLabel
        Left = 295
        Top = 16
        Width = 28
        Height = 15
        Caption = 'MWT'
      end
      object chklstMAW: TCheckListBox
        Left = 3
        Top = 35
        Width = 286
        Height = 230
        Items.Strings = (
          'Head'
          'From MVR'
          'Well flow rate (maw)'
          'Well cell flow rates (maw + icon)'
          'Pumping rate'
          'Pumping rate to MVR'
          'Flowing well flow rate'
          'Flowing well flow rate to MVR'
          'Storage flow rate'
          'Constant-flow rate'
          'Well conductance (conductance)'
          'Individual well cell conductances (conductance + icon)'
          'Flowing well conductance')
        TabOrder = 0
        OnClickCheck = chklstMAWClickCheck
      end
      object chklstMwt: TCheckListBox
        Left = 295
        Top = 35
        Width = 284
        Height = 230
        Items.Strings = (
          'Concentration'
          'Storage'
          'Constant'
          'From-MVR'
          'Mass Flow Rate (Aquifer) (MWT)'
          'Mass Flow Rate (Cells) (MWT + iconn)'
          'Well Mass-Flow-Rate (rate)'
          'Flowing Well Mass-Flow-Rate (fw-rate)'
          'Mass Flow Rate to MVR (rate-to-mvr)'
          'Flowing Well Mass Flow Rate to MVR (fw-rate-to-mvr)')
        TabOrder = 1
      end
    end
    object tabSFR: TTabSheet
      Caption = 'SFR'
      ImageIndex = 2
      object lblSFR: TLabel
        Left = 3
        Top = 88
        Width = 19
        Height = 15
        Caption = 'SFR'
      end
      object lblSFT: TLabel
        Left = 247
        Top = 88
        Width = 18
        Height = 15
        Caption = 'SFT'
      end
      object chklstSFR: TCheckListBox
        Left = 2
        Top = 109
        Width = 239
        Height = 252
        Items.Strings = (
          'Stream stage'
          'External inflow'
          'Inflow from upstream'
          'From MVR'
          'Rainfall'
          'Runoff'
          'Groundwater exchange'
          'Evaporation'
          'Outflow to downstream'
          'External outflow'
          'To MVR'
          'Flow from upstream'
          'Flow to downstream'
          'Depth'
          'Wetted perimeter'
          'Wetted cross-section area'
          'Wetted top width')
        TabOrder = 0
        OnClickCheck = chklstSFRClickCheck
      end
      object rgStreamObsLocation: TRadioGroup
        Left = 3
        Top = 3
        Width = 476
        Height = 74
        Caption = 'Stream observation location'
        Columns = 2
        Items.Strings = (
          'All combined'
          'First'
          'Last'
          'Each individually')
        TabOrder = 1
      end
      object chklstSFT: TCheckListBox
        Left = 247
        Top = 109
        Width = 232
        Height = 252
        Items.Strings = (
          'Concentration'
          'Storage'
          'Constant'
          'From-MVR'
          'To-MVR'
          'Mass flow rate with aquifer (SFT)'
          'Rainfall'
          'Evaporation'
          'Runoff'
          'External inflow'
          'External outflow')
        TabOrder = 2
        OnClickCheck = chklstSFTClickCheck
      end
    end
    object tabLAK: TTabSheet
      Caption = 'LAK'
      ImageIndex = 3
      object lblLAK: TLabel
        Left = 3
        Top = 3
        Width = 21
        Height = 15
        Caption = 'LAK'
      end
      object lblLKT: TLabel
        Left = 279
        Top = 3
        Width = 19
        Height = 15
        Caption = 'LKT'
      end
      object chklstLAK: TCheckListBox
        Left = 3
        Top = 32
        Width = 270
        Height = 337
        Items.Strings = (
          'Stage'
          'Specified inflow'
          'Inflow from outlets'
          'Total inflow'
          'From MVR'
          'Rainfall'
          'Runoff'
          'Lake flow rate'
          'Withdrawal'
          'Evaporation'
          'Exteral outflow'
          'To MVR'
          'Storage'
          'Simulated constant flow rate'
          'Outlet flow'
          'Volume'
          'Surface area'
          'Wetted area'
          'Conductance')
        TabOrder = 0
        OnClick = chklstLAKClick
      end
      object chklstGwtOb: TCheckListBox
        Left = 279
        Top = 32
        Width = 300
        Height = 337
        Items.Strings = (
          'Concentration'
          'Storage'
          'Constant'
          'From-MVR'
          'To-MVR'
          'LKT'
          'Rainfall'
          'Evaporation'
          'Runoff'
          'External inflow'
          'Withdrawal'
          'External outflow')
        TabOrder = 1
        OnClick = chklstGwtObClick
      end
    end
    object tabUZF: TTabSheet
      Caption = 'UZF'
      ImageIndex = 4
      DesignSize = (
        582
        418)
      object lblDepthFraction: TLabel
        Left = 159
        Top = 331
        Width = 316
        Height = 15
        Caption = 'Fraction of cell height in water content observations (depth)'
      end
      object rdeDepthFraction: TRbwDataEntry
        Left = 8
        Top = 328
        Width = 145
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 0
        Text = '0.5'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object pnl1: TPanel
        Left = 2
        Top = 3
        Width = 577
        Height = 310
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        object lblUZF: TLabel
          Left = 3
          Top = 9
          Width = 21
          Height = 15
          Caption = 'UZF'
        end
        object lblUZT: TLabel
          Left = 272
          Top = 8
          Width = 21
          Height = 15
          Caption = 'UZT'
        end
        object spl1: TSplitter
          AlignWithMargins = True
          Left = 263
          Top = 31
          Width = 5
          Height = 278
          Margins.Left = 0
          Margins.Top = 30
          Margins.Right = 0
          Margins.Bottom = 0
          ExplicitTop = 1
          ExplicitHeight = 308
        end
        object chklstUZF: TCheckListBox
          AlignWithMargins = True
          Left = 1
          Top = 31
          Width = 262
          Height = 275
          Margins.Left = 0
          Margins.Top = 30
          Margins.Right = 0
          Align = alLeft
          Items.Strings = (
            'Recharge to the aquifer from UZF (uzf-gwrch)'
            'UZF Discharge to land surface (uzf-gwd)'
            'UZF Discharge available to MVR package (uzf-gwd-to-mvr)'
            'UZF groundwater evapotranspiration (uzf-gwet)'
            'UZF specified infiltration rate (infiltration)'
            'Inflow from MVR package to UZF (from-mvr)'
            'UZF rejected infiltration (rej-inf)'
            
              'UZF rejected infiltration available to MVR package (rej-inf-to-m' +
              'vr)'
            'UZF unsaturated zone evapotranspiration (uzet)'
            'UZF storage flow rate (storage)'
            'Net UZF infiltration (net-infiltration)'
            'UZF unsaturated zone water content (water-content)')
          ScrollWidth = 400
          TabOrder = 0
          OnClick = chklstUZFClick
        end
        object chklstUZT: TCheckListBox
          AlignWithMargins = True
          Left = 268
          Top = 31
          Width = 308
          Height = 278
          Margins.Left = 0
          Margins.Top = 30
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alClient
          Items.Strings = (
            'UZT_Concentration'
            'UZT_Storage'
            'UZT_Constant'
            'UZT_FromMvr'
            'UZT_Mass_Flow_Rate (UZT)'
            'UZT_Infiltration'
            'UZT_Rejected_Infiltration'
            'UZT_ET'
            'UZT_Rej_Infil_to_MVR')
          TabOrder = 1
        end
      end
    end
    object tabCSUB: TTabSheet
      Caption = 'CSUB'
      ImageIndex = 5
      object splCSub: TSplitter
        Left = 0
        Top = 313
        Width = 582
        Height = 5
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 317
        ExplicitWidth = 482
      end
      object chklstCSUB: TCheckListBox
        Left = 0
        Top = 0
        Width = 582
        Height = 313
        Align = alClient
        Items.Strings = (
          'Flow: groundwater (GW) and interbeds (IB) (csub)'
          'Flow: GW and IB from inelastic compaction (inelastic-csub)'
          'Flow: GW and IB from elastic compaction (elastic-csub)'
          'Flow: GW and coarse materials (CM) in a GWF cell (coarse-csub)'
          'Flow: GW for all IB and CM (csub-cell)'
          
            'Flow: GW for all IB and CM from water compressibility (wcomp-csu' +
            'b-cell)'
          'Convertible interbed storativity in interbeds (sk)'
          'Elastic interbed storativity in interbeds (ske)'
          
            'Convertible interbed and coarse-grained material storativity (sk' +
            '-cell)'
          'Elastic interbed and coarse material storativity (ske-cell)'
          'Effective stress in a GWF cell (estress-cell)'
          'Geostatic stress in a GWF cell (gstress-cell)'
          'Interbed compaction in interbeds (interbed-compaction)'
          
            'Inelastic interbed compaction in interbeds (inelastic-compaction' +
            ')'
          'Elastic interbed compaction in interbeds (elastic-compaction)'
          'Elastic compaction in CM  in a GWF cell (coarse-compaction)'
          'Compaction in CM and all interbeds (compaction-cell)'
          'Thickness of interbeds (thickness)'
          'Thickness of CM in a GWF cell (coarse-thickness)'
          'Total thickness of CM and all IB in a GWF cell (thickness-cell)'
          'Porosity of an interbed (theta)'
          'Porosity of coarse-grained materials in  GWF cell (coarse-theta)'
          
            'Thickness-weighted porosity of CM and all IB in a GWF cell (thet' +
            'a-cell)'
          'Flow: GW and delay IB across the top of the IB (delay-flowtop)'
          
            'Flow: GW and delay IB across the bottom of the IB (delay-flowbot' +
            ')'
          'Head in interbed delay cell idcellno (delay-head)'
          'Geostatic stress in interbed delay cell idcellno (delay-gstress)'
          'Effective stress in interbed delay cell idcellno (delay-estress)'
          
            'Preconsolidation stress in interbed delay cell idcellno (delay-p' +
            'reconstress)'
          'Compaction in interbed delay cell idcellno (delay-compaction)'
          'Thickness of interbed delay cell idcellno (delay-thickness)'
          'Porosity of interbed delay cell idcellno (delay-theta)'
          
            'Preconsolidation stress in a cell containing at least one IB (pr' +
            'econstress-cell)')
        TabOrder = 0
        OnClick = chklstCSUBClick
      end
      object pnlDelayBeds: TPanel
        Left = 0
        Top = 318
        Width = 582
        Height = 100
        Align = alBottom
        TabOrder = 1
        object lblDelayInterbedNumber: TLabel
          Left = 8
          Top = 9
          Width = 262
          Height = 15
          Caption = 'Sub-layer number within delay interbed (idcellno)'
        end
        object chklstDelayBeds: TCheckListBox
          Left = 1
          Top = 28
          Width = 580
          Height = 71
          Align = alBottom
          Columns = 8
          TabOrder = 0
        end
      end
    end
    object tabCalibration: TTabSheet
      Caption = 'Calibration'
      ImageIndex = 6
      inline framePestObs: TframePestObsMf6
        Left = 0
        Top = 0
        Width = 582
        Height = 418
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 582
        ExplicitHeight = 413
        inherited splObservations: TSplitter
          Top = 241
          Width = 582
          ExplicitTop = 243
          ExplicitWidth = 582
        end
        inherited grpDirectObs: TGroupBox
          Width = 582
          Height = 241
          ExplicitWidth = 582
          ExplicitHeight = 236
          inherited frameObservations: TframeGrid
            Width = 578
            Height = 222
            ExplicitTop = 17
            ExplicitWidth = 578
            ExplicitHeight = 217
            inherited Panel: TPanel
              Top = 164
              Width = 578
              ExplicitTop = 159
              ExplicitWidth = 578
              inherited sbAdd: TSpeedButton
                Left = 447
                ExplicitLeft = 447
              end
              inherited sbInsert: TSpeedButton
                Left = 482
                ExplicitLeft = 482
              end
              inherited sbDelete: TSpeedButton
                Left = 517
                ExplicitLeft = 517
              end
              inherited seNumber: TJvSpinEdit
                OnChange = frameObservationsseNumberChange
              end
            end
            inherited Grid: TRbwDataGrid4
              Width = 578
              Height = 164
              ExplicitWidth = 578
              ExplicitHeight = 164
            end
          end
        end
        inherited grpObsComparisons: TGroupBox
          Top = 246
          Width = 582
          ExplicitTop = 241
          ExplicitWidth = 582
          inherited frameObsComparisons: TframeGrid
            Width = 578
            ExplicitTop = 17
            ExplicitWidth = 578
            ExplicitHeight = 153
            inherited Panel: TPanel
              Width = 578
              ExplicitTop = 118
              ExplicitWidth = 578
              inherited sbAdd: TSpeedButton
                Left = 447
                ExplicitLeft = 447
              end
              inherited sbInsert: TSpeedButton
                Left = 482
                ExplicitLeft = 482
              end
              inherited sbDelete: TSpeedButton
                Left = 517
                ExplicitLeft = 517
              end
            end
            inherited Grid: TRbwDataGrid4
              Width = 578
              ExplicitWidth = 578
            end
          end
        end
      end
    end
  end
  object pnlName: TPanel
    Left = 0
    Top = 25
    Width = 590
    Height = 46
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object edObsName: TLabeledEdit
      Left = 6
      Top = 19
      Width = 278
      Height = 23
      EditLabel.Width = 197
      EditLabel.Height = 15
      EditLabel.Caption = 'Observation location name (obsnam)'
      MaxLength = 40
      TabOrder = 0
      Text = ''
      OnChange = edObsNameChange
    end
  end
end
