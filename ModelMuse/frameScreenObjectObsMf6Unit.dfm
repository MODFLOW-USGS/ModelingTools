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
    ActivePage = tabBasic
    Align = alClient
    TabOrder = 1
    object tabBasic: TTabSheet
      Caption = 'Basic'
      object lblTypesOfFlowObservation: TLabel
        Left = 3
        Top = 74
        Width = 170
        Height = 20
        Caption = 'Types of flow observation'
      end
      object lblBoundaryFlowObservations: TLabel
        Left = 3
        Top = 200
        Width = 184
        Height = 20
        Caption = 'Boundary flow observations'
      end
      object lblSpecies: TLabel
        Left = 296
        Top = 74
        Width = 113
        Height = 20
        Caption = 'Chemical species'
      end
      object lblGwtObs: TLabel
        Left = 296
        Top = 195
        Width = 201
        Height = 20
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
        ItemHeight = 20
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
        ItemHeight = 20
        Items.Strings = (
          'CHD flows'
          'DRN flows'
          'EVT flows'
          'GHB flows'
          'RCH flows'
          'RIV flows'
          'WEL flows'
          'To MVR flows')
        TabOrder = 4
        OnClickCheck = chklstBoundaryFlowClickCheck
      end
      object cbConcentration: TCheckBox
        Left = 296
        Top = 3
        Width = 283
        Height = 17
        Caption = 'Concentration observation (conc)'
        TabOrder = 5
        OnClick = cbConcentrationClick
      end
      object chklstGWT: TCheckListBox
        Left = 296
        Top = 221
        Width = 282
        Height = 97
        ItemHeight = 20
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
        ItemHeight = 20
        TabOrder = 7
      end
    end
    object tabMAW: TTabSheet
      Caption = 'MAW'
      ImageIndex = 1
      object chklstMAW: TCheckListBox
        Left = 3
        Top = 3
        Width = 476
        Height = 230
        ItemHeight = 20
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
          'Individual well cell conductances (conductance + icon'
          'Flowing well conductance')
        TabOrder = 0
        OnClickCheck = chklstMAWClickCheck
      end
    end
    object tabSFR: TTabSheet
      Caption = 'SFR'
      ImageIndex = 2
      object lblSFR: TLabel
        Left = 3
        Top = 88
        Width = 24
        Height = 20
        Caption = 'SFR'
      end
      object lblSFT: TLabel
        Left = 247
        Top = 88
        Width = 23
        Height = 20
        Caption = 'SFT'
      end
      object chklstSFR: TCheckListBox
        Left = 2
        Top = 109
        Width = 239
        Height = 252
        ItemHeight = 20
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
          'Flow to downstream')
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
        ItemHeight = 20
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
      object chklstLAK: TCheckListBox
        Left = 3
        Top = 3
        Width = 270
        Height = 366
        ItemHeight = 20
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
        Top = 3
        Width = 300
        Height = 366
        ItemHeight = 20
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
      object lblDepthFraction: TLabel
        Left = 159
        Top = 299
        Width = 396
        Height = 20
        Caption = 'Fraction of cell height in water content observations (depth)'
      end
      object chklstUZF: TCheckListBox
        Left = 3
        Top = 3
        Width = 476
        Height = 262
        ItemHeight = 20
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
        TabOrder = 0
        OnClick = chklstUZFClick
      end
      object rdeDepthFraction: TRbwDataEntry
        Left = 8
        Top = 296
        Width = 145
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 1
        Text = '0.5'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object tabCSUB: TTabSheet
      Caption = 'CSUB'
      ImageIndex = 5
      object splCSub: TSplitter
        Left = 0
        Top = 308
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
        Height = 308
        Align = alClient
        ItemHeight = 20
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
        Top = 313
        Width = 582
        Height = 100
        Align = alBottom
        TabOrder = 1
        object lblDelayInterbedNumber: TLabel
          Left = 8
          Top = 9
          Width = 329
          Height = 20
          Caption = 'Sub-layer number within delay interbed (idcellno)'
        end
        object chklstDelayBeds: TCheckListBox
          Left = 1
          Top = 28
          Width = 580
          Height = 71
          Align = alBottom
          Columns = 8
          ItemHeight = 20
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
        Height = 413
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 582
        ExplicitHeight = 413
        inherited splObservations: TSplitter
          Top = 236
          Width = 582
          ExplicitTop = 236
          ExplicitWidth = 482
        end
        inherited grpDirectObs: TGroupBox
          Width = 582
          Height = 236
          ExplicitWidth = 582
          ExplicitHeight = 236
          inherited frameObservations: TframeGrid
            Width = 578
            Height = 212
            ExplicitWidth = 578
            ExplicitHeight = 212
            inherited Panel: TPanel
              Top = 154
              Width = 578
              ExplicitTop = 154
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
              Height = 154
              ExplicitWidth = 578
              ExplicitHeight = 154
            end
          end
        end
        inherited grpObsComparisons: TGroupBox
          Top = 241
          Width = 582
          ExplicitTop = 241
          ExplicitWidth = 582
          inherited frameObsComparisons: TframeGrid
            Width = 578
            ExplicitWidth = 578
            inherited Panel: TPanel
              Width = 578
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
      Height = 28
      EditLabel.Width = 246
      EditLabel.Height = 20
      EditLabel.Caption = 'Observation location name (obsnam)'
      MaxLength = 40
      TabOrder = 0
      Text = ''
      OnChange = edObsNameChange
    end
  end
end
