inherited frmModflowPackages: TfrmModflowPackages
  HelpType = htKeyword
  HelpKeyword = 'MODFLOW_Packages_Dialog_Box'
  Caption = 'MODFLOW Packages and Programs'
  ClientHeight = 557
  ClientWidth = 782
  OnClose = FormClose
  OnResize = FormResize
  ExplicitWidth = 798
  ExplicitHeight = 596
  PixelsPerInch = 96
  TextHeight = 18
  object JvNetscapeSplitter1: TJvNetscapeSplitter
    Left = 177
    Top = 0
    Height = 516
    Align = alLeft
    MinSize = 1
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ExplicitLeft = 121
    ExplicitTop = -14
    ExplicitHeight = 252
  end
  object jvplPackages: TJvPageList
    Left = 187
    Top = 0
    Width = 595
    Height = 516
    ActivePage = jvspSMS
    PropagateEnable = False
    Align = alClient
    OnChange = jvplPackagesChange
    object jvspLPF: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'LPF_Layer_Property_Flow_Package'
      Caption = 'LPF (Layer Property Flow)'
      object splitLprParameter: TJvNetscapeSplitter
        Left = 121
        Top = 291
        Height = 225
        Align = alLeft
        MinSize = 1
        Maximized = False
        Minimized = False
        ButtonCursor = crDefault
        ExplicitLeft = 256
        ExplicitTop = 96
        ExplicitHeight = 100
      end
      object JvNetscapeSplitter3: TJvNetscapeSplitter
        Left = 0
        Top = 281
        Width = 595
        Height = 10
        Cursor = crVSplit
        Align = alTop
        MinSize = 1
        Maximized = False
        Minimized = False
        ButtonCursor = crDefault
        ExplicitLeft = -4
        ExplicitTop = 283
        ExplicitWidth = 605
      end
      inline frameLpfParameterDefinition: TframeArrayParameterDefinition
        Left = 131
        Top = 291
        Width = 464
        Height = 225
        Align = alClient
        Enabled = False
        TabOrder = 2
        TabStop = True
        ExplicitLeft = 131
        ExplicitTop = 291
        ExplicitWidth = 464
        ExplicitHeight = 225
        inherited pnlParameterCount: TPanel
          Top = 177
          Width = 464
          ExplicitTop = 177
          ExplicitWidth = 464
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 351
            Top = 9
            Enabled = True
            TabOrder = 1
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 351
            ExplicitTop = 9
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            Enabled = True
            TabOrder = 0
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 458
          Height = 114
          Enabled = True
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
              MaxLength = 10
              ParentButtonFont = False
              WordWrapCaptions = False
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
          ExplicitWidth = 458
          ExplicitHeight = 114
        end
        inherited pnlTop: TPanel
          Width = 464
          ExplicitWidth = 464
          inherited lblParamValue: TLabel
            Width = 101
            Height = 18
            ExplicitWidth = 101
            ExplicitHeight = 18
          end
        end
      end
      object tvLpfParameterTypes: TTreeView
        Left = 0
        Top = 291
        Width = 121
        Height = 225
        Align = alLeft
        Enabled = False
        HideSelection = False
        Indent = 20
        ReadOnly = True
        TabOrder = 1
        OnChange = tvLpfParameterTypesChange
      end
      inline framePkgLPF: TframePackageLpf
        Left = 0
        Top = 0
        Width = 595
        Height = 281
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 281
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited rdgOptions: TRbwDataGrid4
          Width = 567
          ExplicitWidth = 567
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgLPF.lblComments
            end
            item
              Control = framePkgLPF.memoComments
            end
            item
              Control = framePkgLPF.rdgOptions
            end>
          OnEnabledChange = framePkgLPFrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspHUF: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'HUF2_Hydrogeologic_Unit_Flow'
      Caption = 'jvspHUF'
      object JvNetscapeSplitter4: TJvNetscapeSplitter
        Left = 0
        Top = 275
        Width = 595
        Height = 10
        Cursor = crVSplit
        Align = alTop
        MinSize = 1
        Maximized = False
        Minimized = False
        ButtonCursor = crDefault
        ExplicitTop = 204
        ExplicitWidth = 605
      end
      object JvNetscapeSplitter5: TJvNetscapeSplitter
        Left = 121
        Top = 285
        Height = 231
        Align = alLeft
        MinSize = 1
        Maximized = False
        Minimized = False
        ButtonCursor = crDefault
        ExplicitLeft = 256
        ExplicitTop = 96
        ExplicitHeight = 100
      end
      inline framePkgHuf: TframePackageHuf
        Left = 0
        Top = 0
        Width = 595
        Height = 275
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        DesignSize = (
          595
          275)
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited cbSaveHeads: TCheckBox
          Width = 441
          ExplicitWidth = 441
        end
        inherited cbSaveFlows: TCheckBox
          Width = 417
          TabOrder = 3
          ExplicitWidth = 417
        end
        inherited rgElevationSurfaceChoice: TRadioGroup
          Width = 313
          TabOrder = 2
          ExplicitWidth = 313
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgHuf.lblComments
            end
            item
              Control = framePkgHuf.memoComments
            end
            item
              Control = framePkgHuf.cbSaveHeads
            end
            item
              Control = framePkgHuf.cbSaveFlows
            end
            item
              Control = frameHufParameterDefinition
            end
            item
              Control = tvHufParameterTypes
            end>
          OnEnabledChange = framePkgHufrcSelectionControllerEnabledChange
        end
      end
      object tvHufParameterTypes: TTreeView
        Left = 0
        Top = 285
        Width = 121
        Height = 231
        Align = alLeft
        Enabled = False
        HideSelection = False
        Indent = 20
        ReadOnly = True
        TabOrder = 1
        OnChange = tvHufParameterTypesChange
      end
      inline frameHufParameterDefinition: TframeListParameterDefinition
        Left = 131
        Top = 285
        Width = 464
        Height = 231
        Align = alClient
        Enabled = False
        TabOrder = 2
        TabStop = True
        ExplicitLeft = 131
        ExplicitTop = 285
        ExplicitWidth = 464
        ExplicitHeight = 231
        inherited pnlParameterCount: TPanel
          Top = 183
          Width = 464
          ExplicitTop = 183
          ExplicitWidth = 464
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 351
            Top = 9
            Enabled = True
            TabOrder = 1
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 351
            ExplicitTop = 9
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            TabOrder = 0
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 458
          Height = 177
          Enabled = True
          ExplicitWidth = 458
          ExplicitHeight = 177
        end
      end
    end
    object jvspCHD: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'CHD_Time_Variant_Specified_Head'
      Caption = 'CHD (Time-Variant Specified-Head Package)'
      inline framePkgCHD: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 161
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgCHD.lblComments
            end
            item
              Control = framePkgCHD.memoComments
            end
            item
              Control = frameChdParameterDefinition
            end>
        end
      end
      inline frameChdParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 161
        Width = 595
        Height = 355
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 161
        ExplicitWidth = 595
        ExplicitHeight = 355
        inherited pnlParameterCount: TPanel
          Top = 307
          Width = 595
          ExplicitTop = 307
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 481
            Top = 9
            TabOrder = 1
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 481
            ExplicitTop = 9
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            TabOrder = 0
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 301
          ExplicitWidth = 589
          ExplicitHeight = 301
        end
      end
    end
    object jvspGHB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'GHB_General_Head_Boundary_Package'
      Caption = 'GHB (General Head Boundary)'
      inline framePkgGHB: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 161
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgGHB.lblComments
            end
            item
              Control = framePkgGHB.memoComments
            end
            item
              Control = frameGhbParameterDefinition
            end>
        end
      end
      inline frameGhbParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 161
        Width = 595
        Height = 355
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 161
        ExplicitWidth = 595
        ExplicitHeight = 355
        inherited pnlParameterCount: TPanel
          Top = 307
          Width = 595
          ExplicitTop = 307
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 481
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 481
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 301
          ExplicitWidth = 589
          ExplicitHeight = 301
        end
      end
    end
    object jvspPCG: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'PCG_Preconditioned_Conjugate_Gradiant'
      Caption = 'PCG (Preconditioned Conjugate-Gradient)'
      inline framePCG: TframePCG
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        Anchors = [akLeft, akTop, akBottom]
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        DesignSize = (
          595
          516)
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblPCGMaxOuter: TLabel
          Top = 129
          Width = 289
          Height = 18
          ExplicitTop = 127
          ExplicitWidth = 289
          ExplicitHeight = 18
        end
        inherited lblPCGMaxInner: TLabel
          Top = 157
          Width = 273
          Height = 18
          ExplicitTop = 155
          ExplicitWidth = 273
          ExplicitHeight = 18
        end
        inherited lblPCGMethod: TLabel
          Top = 185
          Width = 296
          Height = 18
          ExplicitTop = 183
          ExplicitWidth = 296
          ExplicitHeight = 18
        end
        inherited lblPCGMaxChangeHead: TLabel
          Top = 297
          Width = 261
          Height = 18
          ExplicitTop = 306
          ExplicitWidth = 261
          ExplicitHeight = 18
        end
        inherited lblPCGMaxResidual: TLabel
          Top = 325
          Width = 210
          Height = 18
          ExplicitTop = 334
          ExplicitWidth = 210
          ExplicitHeight = 18
        end
        inherited lblPCGRelaxation: TLabel
          Top = 353
          Width = 220
          Height = 18
          ExplicitTop = 362
          ExplicitWidth = 220
          ExplicitHeight = 18
        end
        inherited lblPCGMaxEigen: TLabel
          Top = 381
          Width = 320
          Height = 18
          ExplicitTop = 390
          ExplicitWidth = 320
          ExplicitHeight = 18
        end
        inherited lblPCGPrintInterval: TLabel
          Top = 408
          Width = 184
          Height = 18
          ExplicitTop = 417
          ExplicitWidth = 184
          ExplicitHeight = 18
        end
        inherited lblPCGPrintControl: TLabel
          Top = 436
          Width = 189
          Height = 18
          ExplicitTop = 445
          ExplicitWidth = 189
          ExplicitHeight = 18
        end
        inherited lblPCGDampingFactor: TLabel
          Top = 463
          Width = 207
          Height = 18
          ExplicitTop = 472
          ExplicitWidth = 207
          ExplicitHeight = 18
        end
        inherited lblPCGDampPcgT: TLabel
          Top = 491
          Width = 280
          Height = 18
          ExplicitTop = 489
          ExplicitWidth = 280
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          Height = 59
          ExplicitWidth = 574
          ExplicitHeight = 59
        end
        inherited rdePCGMaxOuter: TRbwDataEntry
          Top = 126
          Width = 242
          ExplicitTop = 126
          ExplicitWidth = 242
        end
        inherited rdePCGMaxInner: TRbwDataEntry
          Top = 154
          Width = 242
          ExplicitTop = 154
          ExplicitWidth = 242
        end
        inherited comboPCGPrecondMeth: TJvImageComboBox
          Top = 182
          Width = 242
          Height = 26
          ExplicitTop = 182
          ExplicitWidth = 242
          ExplicitHeight = 26
        end
        inherited rdePCGMaxHeadChange: TRbwDataEntry
          Top = 294
          Width = 242
          ExplicitTop = 294
          ExplicitWidth = 242
        end
        inherited rdePCGMaxResChange: TRbwDataEntry
          Top = 322
          Width = 242
          ExplicitTop = 322
          ExplicitWidth = 242
        end
        inherited rdePCGRelax: TRbwDataEntry
          Top = 350
          Width = 242
          ExplicitTop = 350
          ExplicitWidth = 242
        end
        inherited comboPCGEigenValue: TJvImageComboBox
          Top = 378
          Width = 242
          Height = 26
          ExplicitTop = 378
          ExplicitWidth = 242
          ExplicitHeight = 26
        end
        inherited rdePCGPrintInt: TRbwDataEntry
          Top = 405
          Width = 242
          ExplicitTop = 405
          ExplicitWidth = 242
        end
        inherited comboPCGPrint: TJvImageComboBox
          Top = 433
          Width = 242
          Height = 26
          ExplicitTop = 433
          ExplicitWidth = 242
          ExplicitHeight = 26
        end
        inherited rdePCGDamp: TRbwDataEntry
          Top = 460
          Width = 242
          ExplicitTop = 460
          ExplicitWidth = 242
        end
        inherited rdePCGDampPcgT: TRbwDataEntry
          Top = 488
          Width = 242
          ExplicitTop = 488
          ExplicitWidth = 242
        end
        inherited gbIHCOFADD: TGroupBox
          Top = 204
          Anchors = [akLeft, akBottom]
          ExplicitTop = 204
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePCG.lblComments
            end
            item
              Control = framePCG.memoComments
            end
            item
              Control = framePCG.comboPCGPrecondMeth
            end
            item
              Control = framePCG.comboPCGPrint
            end
            item
              Control = framePCG.rdePCGDamp
            end
            item
              Control = framePCG.rdePCGMaxHeadChange
            end
            item
              Control = framePCG.rdePCGMaxInner
            end
            item
              Control = framePCG.rdePCGMaxOuter
            end
            item
              Control = framePCG.rdePCGMaxResChange
            end
            item
              Control = framePCG.rdePCGPrintInt
            end
            item
              Control = framePCG.rdePCGDampPcgT
            end
            item
              Control = framePCG.rbIHCOFADD_0
            end
            item
              Control = framePCG.rbIHCOFADD_1
            end>
        end
      end
    end
    object jvspWEL: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'WEL_Well_Package_Pane'
      Caption = 'jvspWEL'
      inline framePkgWEL: TframePackageWell
        Left = 0
        Top = 0
        Width = 595
        Height = 241
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 241
        DesignSize = (
          595
          241)
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblPhiRamp: TLabel
          Width = 382
          Height = 18
          ExplicitWidth = 382
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          Height = 124
          ExplicitWidth = 564
          ExplicitHeight = 124
        end
        inherited cbTabfiles: TCheckBox
          Width = 569
          ExplicitWidth = 569
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgWEL.lblComments
            end
            item
              Control = framePkgWEL.memoComments
            end
            item
              Control = framePkgWEL.rdePhiRamp
            end
            item
              Control = frameWelParameterDefinition
            end
            item
              Control = framePkgWEL.cbTabfiles
            end>
          Left = 408
          Top = 96
        end
      end
      inline frameWelParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 241
        Width = 595
        Height = 275
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 241
        ExplicitWidth = 595
        ExplicitHeight = 275
        inherited pnlParameterCount: TPanel
          Top = 227
          Width = 595
          ExplicitTop = 227
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 481
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 481
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 221
          ExplicitWidth = 589
          ExplicitHeight = 221
        end
      end
    end
    object jvspRIV: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'RIV_River_Package'
      Caption = 'jvspRIV'
      inline framePkgRIV: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 161
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgRIV.lblComments
            end
            item
              Control = framePkgRIV.memoComments
            end
            item
              Control = frameRivParameterDefinition
            end>
        end
      end
      inline frameRivParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 161
        Width = 595
        Height = 355
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 161
        ExplicitWidth = 595
        ExplicitHeight = 355
        inherited pnlParameterCount: TPanel
          Top = 307
          Width = 595
          ExplicitTop = 307
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 481
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 481
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 301
          ExplicitWidth = 589
          ExplicitHeight = 301
        end
      end
    end
    object jvspDRN: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'DRN_Drain_Package_Pane'
      Caption = 'jvspDRN'
      inline framePkgDRN: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 161
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgDRN.lblComments
            end
            item
              Control = framePkgDRN.memoComments
            end
            item
              Control = frameDrnParameterDefinition
            end>
        end
      end
      inline frameDrnParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 161
        Width = 595
        Height = 355
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 161
        ExplicitWidth = 595
        ExplicitHeight = 355
        inherited pnlParameterCount: TPanel
          Top = 307
          Width = 595
          ExplicitTop = 307
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 481
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 481
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 301
          ExplicitWidth = 589
          ExplicitHeight = 301
        end
      end
    end
    object jvspDRT: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'DRT_Drain_Return_Package_Pane'
      Caption = 'jvspDRT'
      inline framePkgDRT: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 161
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgDRT.lblComments
            end
            item
              Control = framePkgDRT.memoComments
            end
            item
              Control = frameDrtParameterDefinition
            end>
        end
      end
      inline frameDrtParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 161
        Width = 595
        Height = 355
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 161
        ExplicitWidth = 595
        ExplicitHeight = 355
        inherited pnlParameterCount: TPanel
          Top = 307
          Width = 595
          ExplicitTop = 307
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 481
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 481
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 301
          ExplicitWidth = 589
          ExplicitHeight = 301
        end
      end
    end
    object jvspRCH: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'RCH_Recharge_Package_Pane'
      Caption = 'jvspRCH'
      inline framePkgRCH: TframePackageRCH
        Left = 0
        Top = 0
        Width = 595
        Height = 249
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 249
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          Height = 81
          ExplicitWidth = 574
          ExplicitHeight = 81
        end
        inherited pnLayerOption: TPanel
          Top = 149
          Width = 595
          ExplicitTop = 149
          ExplicitWidth = 117
          inherited lblLayerOption: TLabel
            Width = 173
            Height = 18
            Caption = 'Recharge location option'
            ExplicitWidth = 173
            ExplicitHeight = 18
          end
          inherited comboLayerOption: TComboBox
            Height = 26
            ExplicitHeight = 26
          end
          inherited cbTimeVaryingLayers: TCheckBox
            Caption = 'Time varying recharge layers'
          end
          inherited rgAssignmentMethod: TRadioGroup
            Width = 574
            ExplicitWidth = 574
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgRCH.lblComments
            end
            item
              Control = framePkgRCH.memoComments
            end
            item
              Control = framePkgRCH.cbTimeVaryingLayers
            end
            item
              Control = framePkgRCH.comboLayerOption
            end
            item
              Control = framePkgRCH.lblLayerOption
            end
            item
              Control = framePkgRCH.rgAssignmentMethod
            end
            item
              Control = frameRchParameterDefinition
            end>
          OnEnabledChange = framePkgRCHrcSelectionControllerEnabledChange
        end
      end
      inline frameRchParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 249
        Width = 595
        Height = 267
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 249
        ExplicitWidth = 595
        ExplicitHeight = 267
        inherited pnlParameterCount: TPanel
          Top = 219
          Width = 595
          ExplicitTop = 219
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 481
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 481
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 213
          ExplicitWidth = 589
          ExplicitHeight = 213
        end
      end
    end
    object jvspEVT: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'EVT_Evapotranspiration_Package'
      Caption = 'jvspEVT'
      inline framePkgEVT: TframePackageTransientLayerChoice
        Left = 0
        Top = 0
        Width = 595
        Height = 201
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          ExplicitWidth = 574
        end
        inherited pnLayerOption: TPanel
          Width = 595
          ExplicitWidth = 117
          inherited lblLayerOption: TLabel
            Width = 136
            Height = 18
            Caption = 'EVT location option'
            ExplicitWidth = 136
            ExplicitHeight = 18
          end
          inherited comboLayerOption: TComboBox
            Height = 26
            ExplicitHeight = 26
          end
          inherited cbTimeVaryingLayers: TCheckBox
            Caption = 'Time varying EVT layers'
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgEVT.lblComments
            end
            item
              Control = framePkgEVT.memoComments
            end
            item
              Control = framePkgEVT.cbTimeVaryingLayers
            end
            item
              Control = framePkgEVT.comboLayerOption
            end
            item
              Control = framePkgEVT.lblLayerOption
            end
            item
              Control = frameEvtParameterDefinition
            end>
          OnEnabledChange = framePkgEVTrcSelectionControllerEnabledChange
        end
      end
      inline frameEvtParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 201
        Width = 595
        Height = 315
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 201
        ExplicitWidth = 595
        ExplicitHeight = 315
        inherited pnlParameterCount: TPanel
          Top = 267
          Width = 595
          ExplicitTop = 267
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 481
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 481
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 261
          ExplicitWidth = 589
          ExplicitHeight = 261
        end
      end
    end
    object jvspETS: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'ETS_Evapotranspiration_Segments_Package'
      Caption = 'jvspETS'
      inline framePkgETS: TframeEtsPackage
        Left = 0
        Top = 0
        Width = 595
        Height = 226
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          ExplicitWidth = 574
        end
        inherited pnLayerOption: TPanel
          Width = 595
          ExplicitWidth = 117
          inherited lblLayerOption: TLabel
            Width = 136
            Height = 18
            Caption = 'ETS location option'
            ExplicitWidth = 136
            ExplicitHeight = 18
          end
          inherited lblSegments: TLabel
            Width = 144
            Height = 18
            ExplicitWidth = 144
            ExplicitHeight = 18
          end
          inherited comboLayerOption: TComboBox
            Height = 26
            ExplicitHeight = 26
          end
          inherited cbTimeVaryingLayers: TCheckBox
            Caption = 'Time varying ETS layers'
          end
          inherited seSegments: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgETS.lblComments
            end
            item
              Control = framePkgETS.memoComments
            end
            item
              Control = framePkgETS.cbTimeVaryingLayers
            end
            item
              Control = framePkgETS.comboLayerOption
            end
            item
              Control = framePkgETS.lblLayerOption
            end
            item
              Control = framePkgETS.seSegments
            end
            item
              Control = framePkgETS.lblSegments
            end
            item
              Control = frameEtsParameterDefinition
            end>
        end
      end
      inline frameEtsParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 226
        Width = 595
        Height = 290
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 226
        ExplicitWidth = 595
        ExplicitHeight = 290
        inherited pnlParameterCount: TPanel
          Top = 242
          Width = 595
          ExplicitTop = 242
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 481
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 481
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 236
          ExplicitWidth = 589
          ExplicitHeight = 236
        end
      end
    end
    object jvspRES: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'RES_Reservoir_Package_Pane'
      Caption = 'jvspRES'
      inline framePkgRES: TframePackageRes
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          Height = 369
          ExplicitWidth = 574
          ExplicitHeight = 369
        end
        inherited pnLayerOption: TPanel
          Top = 440
          Width = 595
          ExplicitTop = 84
          ExplicitWidth = 117
          inherited lblLayerOption: TLabel
            Top = 5
            Width = 172
            Height = 18
            ExplicitTop = 5
            ExplicitWidth = 172
            ExplicitHeight = 18
          end
          inherited lblTableSize: TLabel
            Top = 54
            Width = 469
            Height = 18
            ExplicitTop = 54
            ExplicitWidth = 469
            ExplicitHeight = 18
          end
          inherited comboLayerOption: TComboBox
            Height = 26
            ExplicitHeight = 26
          end
          inherited cbPrintStage: TCheckBox
            Width = 561
            Height = 18
            ExplicitWidth = 561
            ExplicitHeight = 18
          end
          inherited seTableSize: TJvSpinEdit
            Top = 51
            Height = 26
            ExplicitTop = 51
            ExplicitHeight = 26
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgRES.lblComments
            end
            item
              Control = framePkgRES.memoComments
            end
            item
              Control = framePkgRES.cbTimeVaryingLayers
            end
            item
              Control = framePkgRES.comboLayerOption
            end
            item
              Control = framePkgRES.lblLayerOption
            end
            item
              Control = framePkgRES.cbPrintStage
            end
            item
              Control = framePkgRES.seTableSize
            end
            item
              Control = framePkgRES.lblTableSize
            end>
        end
      end
    end
    object jvspLAK: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'LAK_Lake_Package_Pane'
      Caption = 'jvspLAK'
      inline framePkgLAK: TframePackageLAK
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblTheta: TLabel
          Left = 168
          Top = 297
          Width = 39
          Height = 18
          ExplicitLeft = 168
          ExplicitTop = 306
          ExplicitWidth = 39
          ExplicitHeight = 18
        end
        inherited lblIterations: TLabel
          Left = 168
          Top = 325
          Width = 278
          Height = 18
          ExplicitLeft = 168
          ExplicitTop = 323
          ExplicitWidth = 278
          ExplicitHeight = 18
        end
        inherited lblConvergenceCriterion: TLabel
          Left = 168
          Top = 353
          Width = 235
          Height = 18
          ExplicitLeft = 168
          ExplicitTop = 362
          ExplicitWidth = 235
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblSurfDepth: TLabel
          Left = 168
          Top = 377
          Width = 340
          Height = 18
          ExplicitLeft = 168
          ExplicitTop = 386
          ExplicitWidth = 340
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          Height = 226
          ExplicitWidth = 574
          ExplicitHeight = 226
        end
        inherited rdeTheta: TRbwDataEntry
          Top = 294
          Width = 146
          ExplicitTop = 294
          ExplicitWidth = 146
        end
        inherited rdeIterations: TRbwDataEntry
          Top = 322
          Width = 146
          ExplicitTop = 322
          ExplicitWidth = 146
        end
        inherited rdeConvergenceCriterion: TRbwDataEntry
          Top = 350
          Width = 146
          ExplicitTop = 350
          ExplicitWidth = 146
        end
        inherited cbPrintLake: TCheckBox
          Top = 402
          ExplicitTop = 402
        end
        inherited rdeSurfDepth: TRbwDataEntry
          Top = 374
          Width = 146
          ExplicitTop = 374
          ExplicitWidth = 146
        end
        inherited rgBathymetry: TRadioGroup
          Top = 425
          Width = 345
          Height = 75
          ExplicitTop = 425
          ExplicitWidth = 345
          ExplicitHeight = 75
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgLAK.lblComments
            end
            item
              Control = framePkgLAK.memoComments
            end
            item
              Control = framePkgLAK.cbPrintLake
            end
            item
              Control = framePkgLAK.rdeIterations
            end
            item
              Control = framePkgLAK.rdeConvergenceCriterion
            end
            item
              Control = framePkgLAK.rdeTheta
            end
            item
              Control = framePkgLAK.lblConvergenceCriterion
            end
            item
              Control = framePkgLAK.lblIterations
            end
            item
              Control = framePkgLAK.lblTheta
            end
            item
              Control = framePkgLAK.rdeSurfDepth
            end
            item
              Control = framePkgLAK.lblSurfDepth
            end
            item
              Control = framePkgLAK.rgBathymetry
            end>
          OnEnabledChange = framePkgLAKrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspSFR: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'SFR_Stream_Flow_Routing_Package'
      Caption = 'jvspSFR'
      object pcSFR: TJvPageControl
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        ActivePage = tabSfrGeneral
        Align = alClient
        TabOrder = 0
        ClientBorderWidth = 0
        object tabSfrGeneral: TTabSheet
          Caption = 'General'
          inline framePkgSFR: TframePackageSFR
            Left = 0
            Top = 0
            Width = 595
            Height = 491
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 595
            ExplicitHeight = 491
            inherited lblComments: TLabel
              Left = 6
              Top = 38
              Width = 76
              Height = 18
              ExplicitLeft = 6
              ExplicitTop = 38
              ExplicitWidth = 76
              ExplicitHeight = 18
            end
            inherited lblPackage: TLabel
              Width = 78
              Height = 18
              ExplicitWidth = 78
              ExplicitHeight = 18
            end
            inherited lblPrintStreams: TLabel
              Left = 231
              Top = 170
              Width = 161
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 231
              ExplicitTop = 179
              ExplicitWidth = 161
              ExplicitHeight = 18
            end
            inherited lblStreamTolerance: TLabel
              Left = 112
              Top = 271
              Width = 187
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 269
              ExplicitWidth = 187
              ExplicitHeight = 18
            end
            inherited lblSfrTrailingWaveIncrements: TLabel
              Left = 112
              Top = 295
              Width = 321
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 304
              ExplicitWidth = 321
              ExplicitHeight = 18
            end
            inherited lblSfrMaxTrailingWaves: TLabel
              Left = 112
              Top = 318
              Width = 337
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 327
              ExplicitWidth = 337
              ExplicitHeight = 18
            end
            inherited lblSfrMaxUnsatCells: TLabel
              Left = 112
              Top = 341
              Width = 424
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 350
              ExplicitWidth = 424
              ExplicitHeight = 18
            end
            inherited lblNUMTIM: TLabel
              Left = 112
              Top = 387
              Width = 451
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 396
              ExplicitWidth = 451
              ExplicitHeight = 18
            end
            inherited lblWeight: TLabel
              Left = 112
              Top = 409
              Width = 441
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 407
              ExplicitWidth = 441
              ExplicitHeight = 18
            end
            inherited lblFLWTOL: TLabel
              Left = 112
              Top = 431
              Width = 410
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 429
              ExplicitWidth = 410
              ExplicitHeight = 18
            end
            inherited lblLossAdjustmentFactor: TLabel
              Width = 179
              Height = 18
              ExplicitWidth = 179
              ExplicitHeight = 18
            end
            inherited memoComments: TMemo
              Left = 10
              Width = 567
              Height = 50
              Anchors = [akLeft, akTop, akRight, akBottom]
              ExplicitLeft = 10
              ExplicitWidth = 567
              ExplicitHeight = 50
            end
            inherited cbSfrUnsatflow: TCheckBox95
              Left = 6
              Top = 118
              Anchors = [akLeft, akBottom]
              OnClick = framePkgSFRcbSfrUnsatflowClick
              ExplicitLeft = 6
              ExplicitTop = 118
            end
            inherited cbSfrLpfHydraulicCond: TCheckBox95
              Left = 6
              Top = 138
              Width = 356
              Anchors = [akLeft, akBottom]
              OnClick = framePkgSFRcbSfrLpfHydraulicCondClick
              ExplicitLeft = 6
              ExplicitTop = 138
              ExplicitWidth = 356
            end
            inherited rgSfr2ISFROPT: TRadioGroup
              Left = 6
              Top = 199
              Anchors = [akLeft, akBottom]
              OnClick = framePkgSFRrgSfr2ISFROPTClick
              ExplicitLeft = 6
              ExplicitTop = 199
            end
            inherited comboPrintStreams: TComboBox
              Left = 6
              Top = 167
              Width = 219
              Height = 26
              Anchors = [akLeft, akBottom]
              ItemIndex = 1
              Text = 'Print flows in listing file'
              ExplicitLeft = 6
              ExplicitTop = 167
              ExplicitWidth = 219
              ExplicitHeight = 26
            end
            inherited cbGage8: TCheckBox
              Left = 6
              Top = 452
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 6
              ExplicitTop = 452
            end
            inherited rdeDLEAK: TRbwDataEntry
              Left = 6
              Top = 271
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 6
              ExplicitTop = 271
            end
            inherited rdeNstrail: TRbwDataEntry
              Left = 6
              Top = 295
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 6
              ExplicitTop = 295
              ExplicitHeight = 18
            end
            inherited rdeNsfrsets: TRbwDataEntry
              Left = 6
              Top = 318
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 6
              ExplicitTop = 318
              ExplicitHeight = 18
            end
            inherited rdeIsuzn: TRbwDataEntry
              Left = 6
              Top = 341
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 6
              ExplicitTop = 341
              ExplicitHeight = 18
            end
            inherited cbIRTFLG: TCheckBox
              Left = 6
              Top = 361
              Width = 555
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 6
              ExplicitTop = 361
              ExplicitWidth = 555
            end
            inherited rdeNUMTIM: TRbwDataEntry
              Left = 6
              Top = 387
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 6
              ExplicitTop = 387
              ExplicitHeight = 18
            end
            inherited rdeWeight: TRbwDataEntry
              Left = 6
              Top = 409
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 6
              ExplicitTop = 409
              ExplicitHeight = 18
            end
            inherited rdeFLWTOL: TRbwDataEntry
              Left = 6
              Top = 431
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 6
              ExplicitTop = 431
              ExplicitHeight = 18
            end
            inherited cbUseGsflowFormat: TCheckBox
              Left = 408
              Top = 171
              ExplicitLeft = 408
              ExplicitTop = 171
            end
            inherited cbSeepageLoss: TCheckBox
              Width = 288
              OnClick = framePkgSFRcbSeepageLossClick
              ExplicitWidth = 288
            end
            inherited rcSelectionController: TRbwController
              ControlList = <
                item
                  Control = framePkgSFR.lblComments
                end
                item
                  Control = framePkgSFR.memoComments
                end
                item
                  Control = framePkgSFR.rdeDLEAK
                end
                item
                  Control = framePkgSFR.cbSfrUnsatflow
                end
                item
                  Control = framePkgSFR.comboPrintStreams
                end
                item
                  Control = framePkgSFR.rgSfr2ISFROPT
                end
                item
                  Control = framePkgSFR.cbIRTFLG
                end
                item
                  Control = framePkgSFR.cbGage8
                end
                item
                  Control = framePkgSFR.cbUseGsflowFormat
                end>
              OnEnabledChange = framePkgSFRrcSelectionControllerEnabledChange
            end
          end
        end
        object tabSfrParameters: TTabSheet
          Caption = 'Parameters'
          ImageIndex = 1
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object splitSFR: TSplitter
            Left = 0
            Top = 257
            Width = 595
            Height = 5
            Cursor = crVSplit
            Align = alTop
            ExplicitWidth = 603
          end
          inline frameSFRParameterDefinition: TframeListParameterDefinition
            Left = 0
            Top = 0
            Width = 595
            Height = 257
            Align = alTop
            Enabled = False
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 595
            ExplicitHeight = 257
            inherited pnlParameterCount: TPanel
              Top = 209
              Width = 595
              ExplicitTop = 209
              ExplicitWidth = 595
              inherited lblNumParameters: TLabel
                Width = 192
                Height = 18
                Caption = 'Number of SFR parameters'
                ExplicitWidth = 192
                ExplicitHeight = 18
              end
              inherited btnDelete: TBitBtn
                Left = 481
                OnClick = frameSFRParameterDefinitionbtnDeleteClick
                ExplicitLeft = 481
              end
              inherited seNumberOfParameters: TJvSpinEdit
                Left = 6
                Height = 26
                OnChange = frameSFRParameterDefinitionseNumberOfParametersChange
                ExplicitLeft = 6
                ExplicitHeight = 26
              end
            end
            inherited dgParameters: TRbwDataGrid4
              Width = 589
              Height = 203
              OnSelectCell = frameSFRParameterDefinitiondgParametersSelectCell
              OnSetEditText = frameSFRParameterDefinitiondgParametersSetEditText
              ExplicitWidth = 589
              ExplicitHeight = 203
              ColWidths = (
                64
                64)
            end
          end
          object jplSfrParameters: TJvPageList
            Left = 0
            Top = 262
            Width = 595
            Height = 229
            PropagateEnable = False
            Align = alClient
          end
        end
      end
    end
    object jvspUZF: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'UZF_Unsaturated_Zone_Flow_Package'
      Caption = 'jvspUZF'
      inline framePkgUZF: TframePackageUZF
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        DesignSize = (
          595
          516)
        inherited lblComments: TLabel
          Top = 39
          Width = 76
          Height = 18
          ExplicitTop = 39
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          Height = 69
          Anchors = [akLeft, akTop, akRight, akBottom]
          ExplicitWidth = 574
          ExplicitHeight = 69
        end
        inherited pnLayerOption: TPanel
          Top = 139
          Width = 595
          Height = 377
          ExplicitTop = -217
          ExplicitWidth = 117
          ExplicitHeight = 377
          inherited lblLayerOption: TLabel
            Width = 357
            Height = 18
            Caption = 'Recharge and discharge location option (NUZTOP) '
            ExplicitWidth = 357
            ExplicitHeight = 18
          end
          inherited lblVerticalKSource: TLabel
            Top = 56
            Width = 335
            Height = 18
            Caption = 'Vertical hydraulic conductivity source (IUZFOPT) '
            ExplicitTop = 56
            ExplicitWidth = 335
            ExplicitHeight = 18
          end
          inherited lblNumberOfTrailingWaves: TLabel
            Top = 111
            Width = 251
            Height = 18
            Caption = 'Number of trailing waves (NTRAIL2) '
            ExplicitTop = 111
            ExplicitWidth = 251
            ExplicitHeight = 18
          end
          inherited lblNumberOfWaveSets: TLabel
            Top = 160
            Width = 225
            Height = 18
            Caption = 'Number of wave sets (NSETS2) '
            ExplicitTop = 160
            ExplicitWidth = 225
            ExplicitHeight = 18
          end
          inherited lblSURFDEP: TLabel
            Width = 211
            Height = 54
            Caption = 
              'The average height of undulations in the land surface altitude (' +
              'SURFDEP)'
            ExplicitWidth = 211
            ExplicitHeight = 54
          end
          inherited lblET_SmoothingFactor: TLabel
            Width = 239
            Height = 18
            ExplicitWidth = 239
            ExplicitHeight = 18
          end
          inherited comboLayerOption: TComboBox
            Top = 26
            Height = 26
            ExplicitTop = 26
            ExplicitHeight = 26
          end
          inherited comboVerticalKSource: TComboBox
            Top = 79
            Width = 449
            Height = 26
            ExplicitTop = 79
            ExplicitWidth = 449
            ExplicitHeight = 26
          end
          inherited rdeNumberOfTrailingWaves: TRbwDataEntry
            Top = 132
            ExplicitTop = 132
          end
          inherited rdeNumberOfWaveSets: TRbwDataEntry
            Top = 181
            ExplicitTop = 181
          end
          inherited chklstOptions: TCheckListBox
            Top = 209
            Width = 584
            ItemHeight = 18
            ExplicitTop = 209
            ExplicitWidth = 584
          end
          inherited rgAssignmentMethod: TRadioGroup
            Left = 288
            Top = 111
            Width = 305
            Height = 92
            ExplicitLeft = 288
            ExplicitTop = 111
            ExplicitWidth = 305
            ExplicitHeight = 92
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgUZF.lblComments
            end
            item
              Control = framePkgUZF.memoComments
            end
            item
              Control = framePkgUZF.chklstOptions
            end
            item
              Control = framePkgUZF.comboLayerOption
            end
            item
              Control = framePkgUZF.lblLayerOption
            end
            item
              Control = framePkgUZF.rdeSURFDEP
            end
            item
              Control = framePkgUZF.lblSURFDEP
            end
            item
              Control = framePkgUZF.rdeNumberOfTrailingWaves
            end
            item
              Control = framePkgUZF.lblNumberOfTrailingWaves
            end
            item
              Control = framePkgUZF.rdeNumberOfWaveSets
            end
            item
              Control = framePkgUZF.lblNumberOfWaveSets
            end
            item
              Control = framePkgUZF.comboVerticalKSource
            end
            item
              Control = framePkgUZF.lblVerticalKSource
            end
            item
              Control = framePkgUZF.rgAssignmentMethod
            end>
          OnEnabledChange = framePkgUZFrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspGMG: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'GMG_Geometric_Multigrid_Package'
      Caption = 'jvspGMG'
      OnShow = jvspGMGShow
      inline framePkgGMG: TframeGMG
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        DesignSize = (
          595
          516)
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          Height = 145
          ExplicitWidth = 574
          ExplicitHeight = 145
        end
        inherited pcGMG: TJvPageControl
          Top = 214
          Width = 595
          Height = 302
          ExplicitTop = -142
          ExplicitWidth = 117
          ExplicitHeight = 302
          inherited tabControlAndPrint: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 109
            ExplicitHeight = 269
            DesignSize = (
              587
              269)
            inherited lblGmgRclose: TLabel
              Top = 12
              Width = 294
              Height = 18
              ExplicitTop = 12
              ExplicitWidth = 294
              ExplicitHeight = 18
            end
            inherited lblGmgIiter: TLabel
              Top = 44
              Width = 297
              Height = 18
              ExplicitTop = 44
              ExplicitWidth = 297
              ExplicitHeight = 18
            end
            inherited lblGmgHclose: TLabel
              Top = 76
              Width = 326
              Height = 18
              ExplicitTop = 76
              ExplicitWidth = 326
              ExplicitHeight = 18
            end
            inherited lblGmgMxiter: TLabel
              Top = 108
              Width = 319
              Height = 18
              ExplicitTop = 108
              ExplicitWidth = 319
              ExplicitHeight = 18
            end
            inherited lblGmgIoutgmg: TLabel
              Top = 171
              Width = 172
              Height = 18
              ExplicitTop = 171
              ExplicitWidth = 172
              ExplicitHeight = 18
            end
            inherited lblGmgIsm: TLabel
              Top = 142
              Width = 273
              Height = 18
              ExplicitTop = 142
              ExplicitWidth = 273
              ExplicitHeight = 18
            end
            inherited rdeGmgRclose: TRbwDataEntry
              Left = 366
              Top = 7
              ExplicitLeft = 366
              ExplicitTop = 7
            end
            inherited rdeGmgIiter: TRbwDataEntry
              Left = 366
              Top = 39
              ExplicitLeft = 366
              ExplicitTop = 39
            end
            inherited rdeGmgHclose: TRbwDataEntry
              Left = 366
              Top = 71
              ExplicitLeft = 366
              ExplicitTop = 71
            end
            inherited rdeGmgMxiter: TRbwDataEntry
              Left = 366
              Top = 103
              ExplicitLeft = 366
              ExplicitTop = 103
            end
            inherited comboGmgIoutgmg: TJvImageComboBox
              Left = 304
              Top = 168
              Width = 209
              Height = 28
              ItemHeight = 22
              ExplicitLeft = 304
              ExplicitTop = 168
              ExplicitWidth = 209
              ExplicitHeight = 28
            end
            inherited cbGmbIunitmhc: TCheckBox
              Top = 205
              ExplicitTop = 205
            end
            inherited comboGmgIsm: TJvImageComboBox
              Left = 304
              Top = 139
              Width = 209
              Height = 28
              ItemHeight = 22
              ExplicitLeft = 304
              ExplicitTop = 139
              ExplicitWidth = 209
              ExplicitHeight = 28
            end
          end
          inherited tabDampRelax: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 617
            ExplicitHeight = 225
            inherited lblGmgDup: TLabel
              Top = 76
              Width = 220
              Height = 18
              ExplicitTop = 76
              ExplicitWidth = 220
              ExplicitHeight = 18
            end
            inherited lblGmgDlow: TLabel
              Top = 108
              Width = 230
              Height = 18
              ExplicitTop = 108
              ExplicitWidth = 230
              ExplicitHeight = 18
            end
            inherited lblGmgChglimit: TLabel
              Top = 139
              Width = 304
              Height = 18
              ExplicitTop = 139
              ExplicitWidth = 304
              ExplicitHeight = 18
            end
            inherited lblGmgRelax: TLabel
              Top = 203
              Width = 216
              Height = 18
              ExplicitTop = 203
              ExplicitWidth = 216
              ExplicitHeight = 18
            end
            inherited lblGmgIadamp: TLabel
              Top = 44
              Width = 258
              Height = 18
              ExplicitTop = 44
              ExplicitWidth = 258
              ExplicitHeight = 18
            end
            inherited lblGmgIsc: TLabel
              Top = 171
              Width = 204
              Height = 18
              ExplicitTop = 171
              ExplicitWidth = 204
              ExplicitHeight = 18
            end
            inherited lblGmgDamp: TLabel
              Top = 12
              Width = 201
              Height = 18
              ExplicitTop = 12
              ExplicitWidth = 201
              ExplicitHeight = 18
            end
            inherited rdeGmgDup: TRbwDataEntry
              Top = 73
              ExplicitTop = 73
            end
            inherited rdeGmgRelax: TRbwDataEntry
              Top = 200
              ExplicitTop = 200
            end
            inherited rdeGmgChglimit: TRbwDataEntry
              Top = 136
              ExplicitTop = 136
            end
            inherited rdeGmgDlow: TRbwDataEntry
              Top = 105
              ExplicitTop = 105
            end
            inherited comboGmgIadamp: TJvImageComboBox
              Left = 304
              Top = 41
              Width = 209
              Height = 28
              ItemHeight = 22
              ExplicitLeft = 304
              ExplicitTop = 41
              ExplicitWidth = 209
              ExplicitHeight = 28
            end
            inherited comboGmgIsc: TJvImageComboBox
              Left = 304
              Width = 209
              Height = 28
              ItemHeight = 22
              ExplicitLeft = 304
              ExplicitWidth = 209
              ExplicitHeight = 28
            end
            inherited rdeGmgDamp: TRbwDataEntry
              Top = 9
              ExplicitTop = 9
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgGMG.lblComments
            end
            item
              Control = framePkgGMG.memoComments
            end
            item
              Control = framePkgGMG.rdeGmgDamp
            end
            item
              Control = framePkgGMG.rdeGmgMxiter
            end
            item
              Control = framePkgGMG.rdeGmgHclose
            end
            item
              Control = framePkgGMG.rdeGmgIiter
            end
            item
              Control = framePkgGMG.rdeGmgRclose
            end
            item
              Control = framePkgGMG.comboGmgIsc
            end
            item
              Control = framePkgGMG.comboGmgIsm
            end
            item
              Control = framePkgGMG.cbGmbIunitmhc
            end
            item
              Control = framePkgGMG.comboGmgIoutgmg
            end
            item
              Control = framePkgGMG.comboGmgIadamp
            end>
        end
      end
    end
    object jvspSIP: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'SIP_Strongly_Implicit_Procedure_Package'
      Caption = 'jvspSIP'
      inline framePkgSIP: TframeSIP
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        DesignSize = (
          595
          516)
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblSipMxiter: TLabel
          Top = 183
          Width = 246
          Height = 18
          ExplicitTop = 170
          ExplicitWidth = 246
          ExplicitHeight = 18
        end
        inherited lblSipNparm: TLabel
          Top = 215
          Width = 269
          Height = 18
          ExplicitTop = 213
          ExplicitWidth = 269
          ExplicitHeight = 18
        end
        inherited lblSipAccl: TLabel
          Top = 247
          Width = 204
          Height = 18
          ExplicitTop = 234
          ExplicitWidth = 204
          ExplicitHeight = 18
        end
        inherited lblSipHclose: TLabel
          Top = 279
          Width = 233
          Height = 18
          ExplicitTop = 266
          ExplicitWidth = 233
          ExplicitHeight = 18
        end
        inherited lblSipIpcalc: TLabel
          Top = 311
          Width = 161
          Height = 18
          ExplicitTop = 298
          ExplicitWidth = 161
          ExplicitHeight = 18
        end
        inherited lblSipWseed: TLabel
          Top = 343
          Width = 329
          Height = 36
          WordWrap = True
          ExplicitTop = 352
          ExplicitWidth = 329
          ExplicitHeight = 36
        end
        inherited lblSipIprsip: TLabel
          Top = 391
          Width = 170
          Height = 18
          ExplicitTop = 400
          ExplicitWidth = 170
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          Height = 112
          ExplicitWidth = 574
          ExplicitHeight = 112
        end
        inherited rdeSipMxiter: TRbwDataEntry
          Left = 417
          Top = 180
          ExplicitLeft = 417
          ExplicitTop = 180
        end
        inherited rdeSipNparm: TRbwDataEntry
          Left = 417
          Top = 208
          ExplicitLeft = 417
          ExplicitTop = 208
        end
        inherited rdeSipAccl: TRbwDataEntry
          Left = 417
          Top = 244
          ExplicitLeft = 417
          ExplicitTop = 244
        end
        inherited rdeSipHclose: TRbwDataEntry
          Left = 417
          Top = 272
          ExplicitLeft = 417
          ExplicitTop = 272
        end
        inherited comboSipIpcalc: TJvImageComboBox
          Left = 326
          Top = 300
          Height = 28
          ItemHeight = 22
          ExplicitLeft = 326
          ExplicitTop = 300
          ExplicitHeight = 28
        end
        inherited rdeSipWseed: TRbwDataEntry
          Left = 417
          Top = 336
          ExplicitLeft = 417
          ExplicitTop = 336
        end
        inherited rdeSipIprsip: TRbwDataEntry
          Left = 417
          Top = 384
          ExplicitLeft = 417
          ExplicitTop = 384
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgSIP.lblComments
            end
            item
              Control = framePkgSIP.memoComments
            end
            item
              Control = framePkgSIP.rdeSipMxiter
            end
            item
              Control = framePkgSIP.rdeSipNparm
            end
            item
              Control = framePkgSIP.rdeSipAccl
            end
            item
              Control = framePkgSIP.rdeSipHclose
            end
            item
              Control = framePkgSIP.comboSipIpcalc
            end
            item
              Control = framePkgSIP.rdeSipIprsip
            end>
        end
      end
    end
    object jvspDE4: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'DE4_Direct_Solver_Package_Pane'
      Caption = 'jvspDE4'
      inline framePkgDE4: TframeDE4
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblDe4Itmx: TLabel
          Top = 191
          Width = 258
          Height = 18
          ExplicitTop = 178
          ExplicitWidth = 258
          ExplicitHeight = 18
        end
        inherited lblDe4Mxup: TLabel
          Top = 223
          Width = 363
          Height = 18
          ExplicitTop = 210
          ExplicitWidth = 363
          ExplicitHeight = 18
        end
        inherited lblDe4Mxlow: TLabel
          Top = 251
          Width = 374
          Height = 18
          ExplicitTop = 238
          ExplicitWidth = 374
          ExplicitHeight = 18
        end
        inherited lblDe4Mxbw: TLabel
          Top = 283
          Width = 210
          Height = 18
          ExplicitTop = 270
          ExplicitWidth = 210
          ExplicitHeight = 18
        end
        inherited lblDe4Ifreq: TLabel
          Top = 311
          Width = 275
          Height = 36
          WordWrap = True
          ExplicitTop = 320
          ExplicitWidth = 275
          ExplicitHeight = 36
        end
        inherited lblDe4Mutd4: TLabel
          Top = 359
          Width = 150
          Height = 18
          ExplicitTop = 368
          ExplicitWidth = 150
          ExplicitHeight = 18
        end
        inherited lblDe4Accl: TLabel
          Top = 387
          Width = 221
          Height = 18
          ExplicitTop = 396
          ExplicitWidth = 221
          ExplicitHeight = 18
        end
        inherited lblDe4Hclose: TLabel
          Top = 419
          Width = 287
          Height = 18
          ExplicitTop = 428
          ExplicitWidth = 287
          ExplicitHeight = 18
        end
        inherited lblRdeIprd4: TLabel
          Top = 447
          Width = 166
          Height = 18
          ExplicitTop = 456
          ExplicitWidth = 166
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          Height = 112
          ExplicitWidth = 574
          ExplicitHeight = 112
        end
        inherited rdeDe4Itmx: TRbwDataEntry
          Left = 439
          Top = 188
          ExplicitLeft = 439
          ExplicitTop = 188
        end
        inherited rdeDe4Mxup: TRbwDataEntry
          Left = 439
          Top = 220
          ExplicitLeft = 439
          ExplicitTop = 220
        end
        inherited rdeDe4Mxlow: TRbwDataEntry
          Left = 439
          Top = 248
          ExplicitLeft = 439
          ExplicitTop = 248
        end
        inherited rdeDe4Mxbw: TRbwDataEntry
          Left = 439
          Top = 280
          ExplicitLeft = 439
          ExplicitTop = 280
        end
        inherited comboDe4Ifreq: TJvImageComboBox
          Left = 344
          Top = 307
          Width = 239
          Height = 28
          DroppedWidth = 263
          ItemHeight = 22
          Items = <
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Coefficients constant (1)'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Coefficients vary (2)'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Nonlinear flow equations (3)'
            end>
          ExplicitLeft = 344
          ExplicitTop = 307
          ExplicitWidth = 239
          ExplicitHeight = 28
        end
        inherited comboDe4Mutd4: TJvImageComboBox
          Left = 344
          Top = 356
          Width = 241
          Height = 28
          DroppedWidth = 272
          ItemHeight = 22
          ExplicitLeft = 344
          ExplicitTop = 356
          ExplicitWidth = 241
          ExplicitHeight = 28
        end
        inherited rdeDe4Accl: TRbwDataEntry
          Left = 439
          Top = 384
          ExplicitLeft = 439
          ExplicitTop = 384
        end
        inherited rdeDe4Hclose: TRbwDataEntry
          Left = 439
          Top = 416
          ExplicitLeft = 439
          ExplicitTop = 416
        end
        inherited rdeRdeIprd4: TRbwDataEntry
          Left = 439
          Top = 444
          ExplicitLeft = 439
          ExplicitTop = 444
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgDE4.lblComments
            end
            item
              Control = framePkgDE4.memoComments
            end
            item
              Control = framePkgDE4.rdeDe4Itmx
            end
            item
              Control = framePkgDE4.rdeDe4Mxup
            end
            item
              Control = framePkgDE4.rdeDe4Mxlow
            end
            item
              Control = framePkgDE4.rdeDe4Mxbw
            end
            item
              Control = framePkgDE4.comboDe4Ifreq
            end
            item
              Control = framePkgDE4.comboDe4Mutd4
            end
            item
              Control = framePkgDE4.rdeDe4Accl
            end
            item
              Control = framePkgDE4.rdeDe4Hclose
            end
            item
              Control = framePkgDE4.rdeRdeIprd4
            end>
        end
      end
    end
    object jvspHOB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'HOB_Head_Observation_Package'
      Caption = 'jvspHOB'
      inline framePkgHOB: TframePackageHob
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblDryHead: TLabel
          Top = 466
          Width = 252
          Height = 18
          ExplicitTop = 453
          ExplicitWidth = 252
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          Height = 391
          ExplicitWidth = 574
          ExplicitHeight = 391
        end
        inherited rdeDryHead: TRbwDataEntry
          Top = 485
          ExplicitTop = 485
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgHOB.lblComments
            end
            item
              Control = framePkgHOB.memoComments
            end
            item
              Control = framePkgHOB.rdeDryHead
            end>
        end
      end
    end
    object jvspHFB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'HFB_Horizontal_Flow_Barrier_Package'
      Caption = 'jvspHFB'
      inline framePkgHFB: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 161
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgHFB.lblComments
            end
            item
              Control = framePkgHFB.memoComments
            end
            item
              Control = frameHfbParameterDefinition
            end>
        end
      end
      inline frameHfbParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 161
        Width = 595
        Height = 355
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 161
        ExplicitWidth = 595
        ExplicitHeight = 355
        inherited pnlParameterCount: TPanel
          Top = 307
          Width = 595
          ExplicitTop = 307
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 481
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 481
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            Enabled = True
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 301
          ExplicitWidth = 589
          ExplicitHeight = 301
        end
      end
    end
    object jvspModpath: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'MODPATH'
      Caption = 'jvspModpath'
      inline frameModpath: TframeModpathSelection
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 576
          ExplicitWidth = 576
        end
        inherited pcModpath: TPageControl
          Width = 590
          Height = 377
          ExplicitWidth = 590
          ExplicitHeight = 377
          inherited tabResponse: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 582
            ExplicitHeight = 344
            inherited lblTrackingDirection: TLabel
              Left = 359
              Top = 257
              Width = 132
              Height = 36
              WordWrap = True
              ExplicitLeft = 359
              ExplicitTop = 257
              ExplicitWidth = 132
              ExplicitHeight = 36
            end
            inherited lblWeakSinkTreatment: TLabel
              Width = 299
              Height = 18
              ExplicitWidth = 299
              ExplicitHeight = 18
            end
            inherited lblWeakSinkThreshold: TLabel
              Top = 309
              Width = 207
              Height = 18
              ExplicitTop = 309
              ExplicitWidth = 207
              ExplicitHeight = 18
            end
            inherited lblStopZone: TLabel
              Top = 225
              Width = 290
              Height = 18
              ExplicitTop = 225
              ExplicitWidth = 290
              ExplicitHeight = 18
            end
            inherited lblWhichEndpoints: TLabel
              Top = 166
              Width = 262
              Height = 18
              ExplicitTop = 166
              ExplicitWidth = 262
              ExplicitHeight = 18
            end
            inherited lblReferenceTime: TLabel
              Left = 359
              Top = 115
              Width = 206
              Height = 36
              ExplicitLeft = 359
              ExplicitTop = 115
              ExplicitWidth = 206
              ExplicitHeight = 36
            end
            inherited lblEvtSink: TLabel
              Width = 287
              Height = 18
              ExplicitWidth = 287
              ExplicitHeight = 18
            end
            inherited lblRchSource: TLabel
              Width = 223
              Height = 18
              ExplicitWidth = 223
              ExplicitHeight = 18
            end
            inherited comboTrackingDirection: TJvImageComboBox
              Left = 359
              Top = 296
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitLeft = 359
              ExplicitTop = 296
              ExplicitHeight = 28
            end
            inherited comboWeakSinkTreatment: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited cbStopInZone: TCheckBox
              Width = 288
              ExplicitWidth = 288
            end
            inherited rdeStopZone: TRbwDataEntry
              Top = 224
              TabOrder = 7
              ExplicitTop = 224
            end
            inherited comboWhichEndpoints: TJvImageComboBox
              Top = 190
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitTop = 190
              ExplicitHeight = 28
            end
            inherited rdeReferenceTime: TRbwDataEntry
              Left = 359
              Top = 157
              TabOrder = 5
              ExplicitLeft = 359
              ExplicitTop = 157
            end
            inherited comboEvtSink: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitHeight = 28
            end
            inherited comboRchSource: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitHeight = 28
            end
          end
          inherited tabVersion5Options: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 331
            inherited lblBeginningTime: TLabel
              Width = 288
              Height = 18
              ExplicitWidth = 288
              ExplicitHeight = 18
            end
            inherited lblEndingTime: TLabel
              Width = 241
              Height = 18
              ExplicitWidth = 241
              ExplicitHeight = 18
            end
            inherited lblMaxSize: TLabel
              Width = 386
              Height = 36
              ExplicitWidth = 386
              ExplicitHeight = 36
            end
            inherited lblErrorTolerance: TLabel
              Width = 103
              Height = 18
              ExplicitWidth = 103
              ExplicitHeight = 18
            end
            inherited lblMaxTime: TLabel
              Width = 160
              Height = 18
              ExplicitWidth = 160
              ExplicitHeight = 18
            end
            inherited lblReleaseTime: TLabel
              Width = 241
              Height = 18
              ExplicitWidth = 241
              ExplicitHeight = 18
            end
          end
          inherited tsVersion6Options: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 592
            ExplicitHeight = 331
            inherited lblWeakSource: TLabel
              Width = 337
              Height = 18
              ExplicitWidth = 337
              ExplicitHeight = 18
            end
            inherited lblStopOption: TLabel
              Width = 252
              Height = 18
              Caption = 'End of particle tracking (StopOption)'
              ExplicitWidth = 252
              ExplicitHeight = 18
            end
            inherited lblStopTime: TLabel
              Top = 123
              Width = 241
              Height = 18
              ExplicitTop = 123
              ExplicitWidth = 241
              ExplicitHeight = 18
            end
            inherited lblBudget: TLabel
              Width = 271
              Height = 18
              ExplicitWidth = 271
              ExplicitHeight = 18
            end
            inherited lblTraceID: TLabel
              Width = 176
              Height = 18
              ExplicitWidth = 176
              ExplicitHeight = 18
            end
            inherited lblAdvObs: TLabel
              Left = 3
              Width = 216
              Height = 36
              ExplicitLeft = 3
              ExplicitWidth = 216
              ExplicitHeight = 36
            end
            inherited lblUzfIface: TLabel
              Width = 159
              Height = 18
              ExplicitWidth = 159
              ExplicitHeight = 18
            end
            inherited lblMnw2Iface: TLabel
              Width = 177
              Height = 18
              ExplicitWidth = 177
              ExplicitHeight = 18
            end
            inherited lblResIface: TLabel
              Width = 196
              Height = 18
              ExplicitWidth = 196
              ExplicitHeight = 18
            end
            inherited lblSfrIface: TLabel
              Width = 161
              Height = 18
              ExplicitWidth = 161
              ExplicitHeight = 18
            end
            inherited lblEtsIface: TLabel
              Width = 160
              Height = 18
              ExplicitWidth = 160
              ExplicitHeight = 18
            end
            inherited lblLakIface: TLabel
              Width = 164
              Height = 18
              ExplicitWidth = 164
              ExplicitHeight = 18
            end
            inherited comboWeakSource: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited comboStopOption: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited comboBudget: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited chkRetardation: TCheckBox
              Width = 302
              ExplicitWidth = 302
            end
            inherited comboAdvObs: TJvImageComboBox
              Top = 292
              Height = 28
              ItemHeight = 22
              TabOrder = 11
              ExplicitTop = 292
              ExplicitHeight = 28
            end
            inherited comboUzfIface: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitHeight = 28
            end
            inherited comboMnw2Iface: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitHeight = 28
            end
            inherited comboResIface: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitHeight = 28
            end
            inherited comboSfrIface: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitHeight = 28
            end
            inherited comboEtsIface: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitHeight = 28
            end
            inherited comboLakIface: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              TabOrder = 12
              ExplicitHeight = 28
            end
          end
          inherited tabOutputTimes: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 592
            ExplicitHeight = 331
            inherited lblTimeMethod: TLabel
              Width = 315
              Height = 18
              ExplicitWidth = 315
              ExplicitHeight = 18
            end
            inherited lblParticleInterval: TLabel
              Width = 328
              Height = 18
              ExplicitWidth = 328
              ExplicitHeight = 18
            end
            inherited lblMaxTimes: TLabel
              Width = 243
              Height = 36
              ExplicitWidth = 243
              ExplicitHeight = 36
            end
            inherited gbTime: TJvGroupBox
              inherited lblTimeCount: TLabel
                Width = 121
                Height = 36
                ExplicitWidth = 121
                ExplicitHeight = 36
              end
              inherited rdgTimes: TRbwDataGrid4
                FixedCols = 0
              end
              inherited seTimeCount: TJvSpinEdit
                Height = 26
                ExplicitHeight = 26
              end
            end
            inherited comboTimeMethod: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitHeight = 28
            end
            inherited rdeParticleInterval: TRbwDataEntry
              Top = 94
              ExplicitTop = 94
            end
            inherited rdeMaxTimes: TRbwDataEntry
              Top = 167
              ExplicitTop = 167
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = frameModpath.lblComments
            end
            item
              Control = frameModpath.memoComments
            end
            item
              Control = frameModpath.cbBinary
            end
            item
              Control = frameModpath.cbCompact
            end
            item
              Control = frameModpath.rdeBeginningTime
            end
            item
              Control = frameModpath.rgOutputMode
            end
            item
              Control = frameModpath.comboTrackingDirection
            end
            item
              Control = frameModpath.comboWeakSinkTreatment
            end
            item
              Control = frameModpath.cbStopInZone
            end
            item
              Control = frameModpath.cbStopAfterMaxTime
            end
            item
              Control = frameModpath.cbBigBudget
            end
            item
              Control = frameModpath.cbSummarize
            end
            item
              Control = frameModpath.cbComputeBudget
            end
            item
              Control = frameModpath.rgModpathVersion
            end
            item
              Control = frameModpath.comboWeakSource
            end
            item
              Control = frameModpath.comboStopOption
            end
            item
              Control = frameModpath.comboBudget
            end
            item
              Control = frameModpath.chkRetardation
            end
            item
              Control = frameModpath.comboAdvObs
            end
            item
              Control = frameModpath.comboEtsIface
            end
            item
              Control = frameModpath.comboUzfIface
            end
            item
              Control = frameModpath.comboMnw2Iface
            end
            item
              Control = frameModpath.comboResIface
            end
            item
              Control = frameModpath.comboSfrIface
            end
            item
              Control = frameModpath.comboLakIface
            end>
          OnEnabledChange = frameModpathrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspCHOB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'CHOB_Specified_Head_Flow_Observations'
      Caption = 'jvspCHOB'
      inline framePkgCHOB: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          Height = 448
          Anchors = [akLeft, akTop, akRight, akBottom]
          ExplicitWidth = 564
          ExplicitHeight = 448
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgCHOB.lblComments
            end
            item
              Control = framePkgCHOB.memoComments
            end>
        end
      end
    end
    object jvspDROB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'DROB_Drain_Observation_Package'
      Caption = 'jvspDROB'
      inline framePkgDROB: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          Height = 448
          Anchors = [akLeft, akTop, akRight, akBottom]
          ExplicitWidth = 564
          ExplicitHeight = 448
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgDROB.lblComments
            end
            item
              Control = framePkgDROB.memoComments
            end>
        end
      end
    end
    object jvspGBOB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'GBOB_General_Head_Boundary_Observations'
      Caption = 'jvspGBOB'
      inline framePkgGBOB: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          Height = 448
          Anchors = [akLeft, akTop, akRight, akBottom]
          ExplicitWidth = 564
          ExplicitHeight = 448
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgGBOB.lblComments
            end
            item
              Control = framePkgGBOB.memoComments
            end>
        end
      end
    end
    object jvspRVOB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'RVOB_River_Observation_Package'
      Caption = 'jvspRVOB'
      inline framePkgRVOB: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          Height = 448
          Anchors = [akLeft, akTop, akRight, akBottom]
          ExplicitWidth = 564
          ExplicitHeight = 448
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgRVOB.lblComments
            end
            item
              Control = framePkgRVOB.memoComments
            end>
        end
      end
    end
    object jvspMNW2: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'MNW2_Multi_Node_Well_Package'
      Caption = 'jvspMNW2'
      inline framePkgMnw2: TframePackageMnw2
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblPrintOption: TLabel
          Top = 358
          Width = 174
          Height = 18
          ExplicitTop = 345
          ExplicitWidth = 174
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          Height = 290
          ExplicitWidth = 564
          ExplicitHeight = 290
        end
        inherited comboPrintOption: TJvImageComboBox
          Top = 381
          Height = 28
          ItemHeight = 22
          ItemIndex = -1
          ExplicitTop = 381
          ExplicitHeight = 28
        end
        inherited gbMnwiOptions: TGroupBox
          Top = 415
          Width = 564
          ExplicitTop = 415
          ExplicitWidth = 564
          inherited cbWellOutput: TCheckBox
            Width = 558
            ExplicitWidth = 558
          end
          inherited cbSummarizeByWell: TCheckBox
            Width = 558
            ExplicitWidth = 558
          end
          inherited cbSummarizeByNode: TCheckBox
            Width = 550
            ExplicitWidth = 550
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgMnw2.lblComments
            end
            item
              Control = framePkgMnw2.memoComments
            end
            item
              Control = framePkgMnw2.lblPrintOption
            end
            item
              Control = framePkgMnw2.comboPrintOption
            end>
        end
      end
    end
    object jvspBCF: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'BCF_Block-Centered_Flow_Package'
      Caption = 'jvspBCF'
      inline framePkgBCF: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          Height = 435
          ExplicitWidth = 564
          ExplicitHeight = 435
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgBCF.lblComments
            end
            item
              Control = framePkgBCF.memoComments
            end>
          OnEnabledChange = framePkgBCFrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspSUB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'SUB_Subsidence_and_Aquifer_Sys'
      Caption = 'jvspSUB'
      OnShow = jvspSUBShow
      inline framePkgSUB: TframePackageSub
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Left = 8
          Width = 76
          Height = 18
          ExplicitLeft = 8
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Left = 8
          Width = 78
          Height = 18
          ExplicitLeft = 8
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Left = 8
          Width = 577
          ExplicitLeft = 8
          ExplicitWidth = 577
        end
        inherited pcSub: TPageControl
          Height = 397
          ExplicitTop = -237
          ExplicitWidth = 117
          ExplicitHeight = 397
          inherited tabControls: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 587
            ExplicitHeight = 362
            inherited lblNumberOfNodes: TLabel
              Width = 287
              Height = 18
              ExplicitWidth = 287
              ExplicitHeight = 18
            end
            inherited lblAccel1: TLabel
              Width = 238
              Height = 18
              ExplicitWidth = 238
              ExplicitHeight = 18
            end
            inherited lblAccel2: TLabel
              Width = 265
              Height = 18
              ExplicitWidth = 265
              ExplicitHeight = 18
            end
            inherited lblMinIterations: TLabel
              Width = 353
              Height = 18
              ExplicitWidth = 353
              ExplicitHeight = 18
            end
            inherited lbReadRestart: TLabel
              Width = 380
              Height = 18
              ExplicitWidth = 380
              ExplicitHeight = 18
            end
            inherited lblOutputChoice: TLabel
              Width = 140
              Height = 18
              ExplicitWidth = 140
              ExplicitHeight = 18
            end
            inherited seNumberOfNodes: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited feReadRestart: TJvFilenameEdit
              Left = 2
              Top = 160
              Width = 577
              Height = 26
              ExplicitLeft = 2
              ExplicitTop = 160
              ExplicitWidth = 577
              ExplicitHeight = 26
            end
            inherited comboOutputChoice: TJvImageComboBox
              Top = 213
              Height = 28
              ItemHeight = 22
              ExplicitTop = 213
              ExplicitHeight = 28
            end
            inherited cbLinkSubsidence: TCheckBox
              Width = 545
              ExplicitWidth = 545
            end
          end
          inherited tabPrintSave: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 109
            ExplicitHeight = 364
            inherited lblNumExportPeriods: TLabel
              Top = 149
              Width = 176
              Height = 18
              ExplicitTop = 326
              ExplicitWidth = 176
              ExplicitHeight = 18
            end
            inherited sbAdd: TSpeedButton
              Top = 145
              ExplicitTop = 322
            end
            inherited sbInsert: TSpeedButton
              Top = 145
              ExplicitTop = 322
            end
            inherited sbDelete: TSpeedButton
              Top = 145
              ExplicitTop = 322
            end
            inherited cbMultiPrintSave: TCheckBox
              Width = 209
              ExplicitWidth = 209
            end
            inherited rdgOutput: TRbwDataGrid4
              Height = 102
              ExplicitHeight = 102
            end
            inherited seNumExportPeriods: TJvSpinEdit
              Top = 146
              Height = 26
              ExplicitTop = 146
              ExplicitHeight = 26
            end
            inherited comboMultiFomat: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgSUB.lblComments
            end
            item
              Control = framePkgSUB.memoComments
            end
            item
              Control = framePkgSUB.seNumberOfNodes
            end
            item
              Control = framePkgSUB.rdeAccel1
            end
            item
              Control = framePkgSUB.rdeAccel2
            end
            item
              Control = framePkgSUB.rdeMinIterations
            end
            item
              Control = framePkgSUB.cbSaveRestart
            end
            item
              Control = framePkgSUB.feReadRestart
            end
            item
              Control = framePkgSUB.rdgOutput
            end
            item
              Control = framePkgSUB.seNumExportPeriods
            end
            item
              Control = framePkgSUB.sbAdd
            end
            item
              Control = framePkgSUB.sbInsert
            end
            item
              Control = framePkgSUB.comboOutputChoice
            end
            item
              Control = framePkgSUB.lblOutputChoice
            end>
        end
      end
    end
    object jvspZoneBudget: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'ZONEBUDGET'
      Caption = 'jvspZoneBudget'
      inline frameZoneBudget: TframeZoneBudget
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        HelpType = htKeyword
        HelpKeyword = 'ZONEBUDGET'
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 205
          Height = 18
          ExplicitWidth = 205
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblCompositeZones: TLabel
          Width = 122
          Height = 18
          ExplicitWidth = 122
          ExplicitHeight = 18
        end
        inherited lblNumberOfZones: TLabel
          Top = 481
          Width = 194
          Height = 18
          Caption = 'Number of composite zones'
          ExplicitTop = 468
          ExplicitWidth = 194
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited rdgCompositeZones: TRbwDataGrid4
          Width = 564
          Height = 264
          ExplicitWidth = 564
          ExplicitHeight = 264
        end
        inherited seNumberOfZones: TJvSpinEdit
          Top = 478
          Height = 26
          ExplicitTop = 478
          ExplicitHeight = 26
        end
        inherited btnInsertZone: TButton
          Left = 423
          Top = 479
          ExplicitLeft = 423
          ExplicitTop = 479
        end
        inherited btnDeleteZone: TButton
          Left = 504
          Top = 479
          ExplicitLeft = 504
          ExplicitTop = 479
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = frameZoneBudget.lblComments
            end
            item
              Control = frameZoneBudget.memoComments
            end>
          OnEnabledChange = frameZoneBudgetrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspSWT: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'SWT_Package'
      Caption = 'jvspSWT'
      OnShow = jvspSWTShow
      inline framePkgSwt: TframePackageSwt
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Top = 55
          Width = 76
          Height = 18
          ExplicitTop = 55
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          WordWrap = True
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Top = 78
          Height = 52
          ExplicitTop = 78
          ExplicitHeight = 52
        end
        inherited pcSWT: TPageControl
          Top = 136
          Height = 380
          ExplicitTop = 136
          ExplicitHeight = 380
          inherited tabControls: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 109
            ExplicitHeight = 347
            inherited lblIvoid: TLabel
              Top = 92
              Width = 209
              Height = 18
              ExplicitTop = 92
              ExplicitWidth = 209
              ExplicitHeight = 18
            end
            inherited lblIstpcs: TLabel
              Top = 151
              Width = 379
              Height = 18
              ExplicitTop = 151
              ExplicitWidth = 379
              ExplicitHeight = 18
            end
            inherited lblIcrcc: TLabel
              Top = 210
              Width = 493
              Height = 18
              ExplicitTop = 210
              ExplicitWidth = 493
              ExplicitHeight = 18
            end
            inherited lblOutputChoice: TLabel
              Top = 273
              Width = 140
              Height = 18
              ExplicitTop = 273
              ExplicitWidth = 140
              ExplicitHeight = 18
            end
            inherited gbIthk: TGroupBox
              Top = 0
              Width = 1255
              Height = 86
              ExplicitTop = 0
              ExplicitWidth = 1255
              ExplicitHeight = 86
              inherited rgIthkConstant: TRadioButton
                Width = 1249
                ExplicitWidth = 1249
              end
              inherited rbIthkVariable: TRadioButton
                Top = 39
                ExplicitTop = 39
              end
            end
            inherited comboOutputChoice: TJvImageComboBox
              Top = 297
              Height = 28
              ItemHeight = 22
              ExplicitTop = 297
              ExplicitHeight = 28
            end
            inherited comboIvoid: TJvImageComboBox
              Top = 114
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitTop = 114
              ExplicitHeight = 28
            end
            inherited comboIstpcs: TJvImageComboBox
              Top = 173
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitTop = 173
              ExplicitHeight = 28
            end
            inherited comboIcrcc: TJvImageComboBox
              Top = 236
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitTop = 236
              ExplicitHeight = 28
            end
          end
          inherited tabPrintSave: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 370
            inherited sbAdd: TSpeedButton
              Top = 156
              ExplicitTop = 330
            end
            inherited sbInsert: TSpeedButton
              Top = 156
              ExplicitTop = 330
            end
            inherited sbDelete: TSpeedButton
              Top = 156
              ExplicitTop = 330
            end
            inherited lblNumExportPeriods: TLabel
              Top = 159
              Width = 176
              Height = 18
              ExplicitTop = 333
              ExplicitWidth = 176
              ExplicitHeight = 18
            end
            inherited rdgInitialPrintChoices: TRbwDataGrid4
              FixedCols = 0
              ColWidths = (
                64
                64
                64)
            end
            inherited rdgOutput: TRbwDataGrid4
              Height = 0
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
                  ComboUsed = False
                  Format = rcf4Boolean
                  LimitToList = False
                  MaxLength = 0
                  ParentButtonFont = False
                  PickList.Strings = (
                    '11G10.3'
                    '9G13.6'
                    '15F7.1'
                    '15F7.2'
                    '15F7.3'
                    '15F7.4'
                    '20F5.0'
                    '20F5.1'
                    '20F5.2'
                    '20F5.3'
                    '20F5.4'
                    '10G11.4')
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
                  Format = rcf4Boolean
                  LimitToList = False
                  MaxLength = 0
                  ParentButtonFont = False
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
                  AutoAdjustColWidths = True
                end>
              ExplicitHeight = 0
            end
            inherited seNumExportPeriods: TJvSpinEdit
              Top = 156
              Height = 26
              ExplicitTop = 156
              ExplicitHeight = 26
            end
            inherited comboMultiFomat: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited cbMultiPrintSave: TCheckBox
              Width = 232
              ExplicitWidth = 232
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgSwt.lblComments
            end
            item
              Control = framePkgSwt.memoComments
            end
            item
              Control = framePkgSwt.rgIthkConstant
            end
            item
              Control = framePkgSwt.rbIthkVariable
            end
            item
              Control = framePkgSwt.comboIvoid
            end
            item
              Control = framePkgSwt.comboIstpcs
            end
            item
              Control = framePkgSwt.comboIcrcc
            end
            item
              Control = framePkgSwt.comboOutputChoice
            end
            item
              Control = framePkgSwt.rdgInitialPrintChoices
            end
            item
              Control = framePkgSwt.rdgOutput
            end
            item
              Control = framePkgSwt.seNumExportPeriods
            end
            item
              Control = framePkgSwt.sbAdd
            end
            item
              Control = framePkgSwt.sbInsert
            end>
        end
      end
    end
    object jvspHydmod: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'HYD_HYDMOD_Package'
      Caption = 'jvspHydmod'
      inline framePkgHydmod: TframePkgHydmod
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblHYDNOH: TLabel
          Top = 464
          Width = 396
          Height = 18
          ExplicitTop = 462
          ExplicitWidth = 396
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          Height = 396
          ExplicitWidth = 564
          ExplicitHeight = 396
        end
        inherited rdeHYDNOH: TRbwDataEntry
          Top = 483
          ExplicitTop = 483
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgHydmod.lblComments
            end
            item
              Control = framePkgHydmod.memoComments
            end
            item
              Control = framePkgHydmod.lblHYDNOH
            end
            item
              Control = framePkgHydmod.rdeHYDNOH
            end>
        end
      end
    end
    object jvspUPW: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'UPW_Upstream_Weighting_Package_Pane'
      Caption = 'jvspUPW'
      object JvNetscapeSplitter6: TJvNetscapeSplitter
        Left = 0
        Top = 233
        Width = 595
        Height = 10
        Cursor = crVSplit
        Align = alTop
        Maximized = False
        Minimized = False
        ButtonCursor = crDefault
        ExplicitTop = 192
        ExplicitWidth = 311
      end
      inline framePkgUPW: TframePackageUpw
        Left = 0
        Top = 0
        Width = 595
        Height = 233
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 233
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited cbPrintHDRY: TCheckBox
          Top = 157
          Width = 449
          TabOrder = 2
          ExplicitTop = 157
          ExplicitWidth = 449
        end
        inherited cbNoParCheck: TCheckBox
          Width = 145
          Height = 40
          TabOrder = 1
          ExplicitWidth = 145
          ExplicitHeight = 40
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgUPW.lblComments
            end
            item
              Control = framePkgUPW.memoComments
            end
            item
              Control = framePkgUPW.cbPrintHDRY
            end
            item
              Control = framePkgUPW.cbNoParCheck
            end>
          OnEnabledChange = framePkgUPWrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspNWT: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'NWT_Newton_Solver_Package_Pane'
      Caption = 'jvspNWT'
      inline framePkgNwt: TframePackageNwt
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited pcNWT: TPageControl
          Width = 595
          Height = 359
          OnChange = framePkgNwtpcNWTChange
          ExplicitWidth = 595
          ExplicitHeight = 359
          inherited tabBasic: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 449
            ExplicitHeight = 289
            inherited lblSolverMethod: TLabel
              Left = 126
              Width = 168
              Height = 18
              ExplicitLeft = 126
              ExplicitWidth = 168
              ExplicitHeight = 18
            end
            inherited lblThicknessFactor: TLabel
              Left = 126
              Width = 381
              Height = 36
              WordWrap = True
              ExplicitLeft = 126
              ExplicitWidth = 381
              ExplicitHeight = 36
            end
            inherited lblMaxOuterIt: TLabel
              Left = 126
              Width = 362
              Height = 18
              ExplicitLeft = 126
              ExplicitWidth = 362
              ExplicitHeight = 18
            end
            inherited lblFluxTolerance: TLabel
              Left = 126
              Width = 233
              Height = 18
              ExplicitLeft = 126
              ExplicitWidth = 233
              ExplicitHeight = 18
            end
            inherited lblHeadTolerance: TLabel
              Left = 126
              Width = 218
              Height = 18
              ExplicitLeft = 126
              ExplicitWidth = 218
              ExplicitHeight = 18
            end
            inherited lblOptions: TLabel
              Left = 126
              Width = 203
              Height = 18
              ExplicitLeft = 126
              ExplicitWidth = 203
              ExplicitHeight = 18
            end
            inherited rdeHeadTolerance: TRbwDataEntry
              Width = 117
              ExplicitWidth = 117
            end
            inherited rdeFluxTolerance: TRbwDataEntry
              Width = 117
              ExplicitWidth = 117
            end
            inherited spinMaxOuterIt: TJvSpinEdit
              Width = 117
              Height = 26
              ExplicitWidth = 117
              ExplicitHeight = 26
            end
            inherited rdeThicknessFactor: TRbwDataEntry
              Width = 117
              ExplicitWidth = 117
            end
            inherited comboSolverMethod: TJvImageComboBox
              Width = 118
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitWidth = 118
              ExplicitHeight = 28
            end
            inherited cbPrintFlag: TCheckBox
              Width = 390
              ExplicitWidth = 390
            end
            inherited cbCorrectForCellBottom: TCheckBox
              Width = 566
              ExplicitWidth = 566
            end
            inherited comboOptions: TJvImageComboBox
              Left = 1
              Width = 119
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitLeft = 1
              ExplicitWidth = 119
              ExplicitHeight = 28
            end
            inherited cbContinue: TCheckBox
              Width = 582
              ExplicitWidth = 582
            end
          end
          inherited tabAdditional: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 449
            ExplicitHeight = 289
            inherited lblDbdTheta: TLabel
              Left = 86
              Width = 452
              Height = 36
              ExplicitLeft = 86
              ExplicitWidth = 452
              ExplicitHeight = 36
            end
            inherited lblDbdKappa: TLabel
              Left = 86
              Top = 38
              Width = 464
              Height = 36
              ExplicitLeft = 86
              ExplicitTop = 38
              ExplicitWidth = 464
              ExplicitHeight = 36
            end
            inherited lblDbdGamma: TLabel
              Left = 86
              Top = 88
              Width = 430
              Height = 36
              ExplicitLeft = 86
              ExplicitTop = 88
              ExplicitWidth = 430
              ExplicitHeight = 36
            end
            inherited lblMomentumCoefficient: TLabel
              Left = 86
              Top = 127
              Width = 245
              Height = 18
              Caption = 'Momentum coefficient (MOMFACT)'
              WordWrap = False
              ExplicitLeft = 86
              ExplicitTop = 127
              ExplicitWidth = 245
              ExplicitHeight = 18
            end
            inherited Label4: TLabel
              Left = 86
              Top = 175
              Width = 445
              Height = 36
              ExplicitLeft = 86
              ExplicitTop = 175
              ExplicitWidth = 445
              ExplicitHeight = 36
            end
            inherited lblBackTol: TLabel
              Left = 85
              Top = 216
              Width = 452
              Height = 54
              ExplicitLeft = 85
              ExplicitTop = 216
              ExplicitWidth = 452
              ExplicitHeight = 54
            end
            inherited lblReductionFactor: TLabel
              Left = 86
              Top = 276
              Width = 494
              Height = 36
              ExplicitLeft = 86
              ExplicitTop = 276
              ExplicitWidth = 494
              ExplicitHeight = 36
            end
            inherited rdeDbdTheta: TRbwDataEntry
              Left = 2
              ExplicitLeft = 2
            end
            inherited rdeDbdKappa: TRbwDataEntry
              Left = 2
              Top = 42
              ExplicitLeft = 2
              ExplicitTop = 42
            end
            inherited rdeDbdGamma: TRbwDataEntry
              Left = 2
              Top = 87
              ExplicitLeft = 2
              ExplicitTop = 87
            end
            inherited rdeMomentumCoefficient: TRbwDataEntry
              Left = 2
              Top = 124
              ExplicitLeft = 2
              ExplicitTop = 124
            end
            inherited cbUseResidualControl: TCheckBox
              Left = 2
              Top = 152
              Width = 423
              ExplicitLeft = 2
              ExplicitTop = 152
              ExplicitWidth = 423
            end
            inherited seMaxReductions: TJvSpinEdit
              Left = 1
              Top = 175
              Width = 79
              Height = 26
              ExplicitLeft = 1
              ExplicitTop = 175
              ExplicitWidth = 79
              ExplicitHeight = 26
            end
            inherited rdeBackTol: TRbwDataEntry
              Left = 1
              Top = 224
              ExplicitLeft = 1
              ExplicitTop = 224
            end
            inherited rdeReductionFactor: TRbwDataEntry
              Left = 2
              Top = 281
              ExplicitLeft = 2
              ExplicitTop = 281
            end
          end
          inherited tabGmresVariables: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 449
            ExplicitHeight = 289
            inherited lblMaxIterationsGmres: TLabel
              Width = 462
              Height = 18
              ExplicitWidth = 462
              ExplicitHeight = 18
            end
            inherited lblIluMethod: TLabel
              Left = 2
              Top = 35
              Width = 347
              Height = 18
              ExplicitLeft = 2
              ExplicitTop = 35
              ExplicitWidth = 347
              ExplicitHeight = 18
            end
            inherited lblFillLimit1: TLabel
              Left = 89
              Top = 100
              Width = 271
              Height = 18
              ExplicitLeft = 89
              ExplicitTop = 100
              ExplicitWidth = 271
              ExplicitHeight = 18
            end
            inherited lblFillLimit2: TLabel
              Left = 89
              Top = 132
              Width = 291
              Height = 18
              ExplicitLeft = 89
              ExplicitTop = 132
              ExplicitWidth = 291
              ExplicitHeight = 18
            end
            inherited lblTolerance: TLabel
              Left = 89
              Top = 164
              Width = 398
              Height = 18
              WordWrap = False
              ExplicitLeft = 89
              ExplicitTop = 164
              ExplicitWidth = 398
              ExplicitHeight = 18
            end
            inherited lblRestarts: TLabel
              Left = 89
              Top = 192
              Width = 469
              Height = 18
              ExplicitLeft = 89
              ExplicitTop = 192
              ExplicitWidth = 469
              ExplicitHeight = 18
            end
            inherited seMaxIterationsGmres: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboIluMethod: TJvImageComboBox
              Left = 2
              Top = 63
              Width = 358
              Height = 28
              DroppedWidth = 358
              ItemHeight = 22
              ItemIndex = -1
              ExplicitLeft = 2
              ExplicitTop = 63
              ExplicitWidth = 358
              ExplicitHeight = 28
            end
            inherited seFillLimit1: TJvSpinEdit
              Left = 2
              Top = 97
              Height = 26
              ExplicitLeft = 2
              ExplicitTop = 97
              ExplicitHeight = 26
            end
            inherited seFillLimit2: TJvSpinEdit
              Left = 2
              Top = 129
              Height = 26
              ExplicitLeft = 2
              ExplicitTop = 129
              ExplicitHeight = 26
            end
            inherited rdeTolerance: TRbwDataEntry
              Left = 2
              Top = 161
              ExplicitLeft = 2
              ExplicitTop = 161
            end
            inherited seRestarts: TJvSpinEdit
              Left = 2
              Top = 189
              Height = 26
              ExplicitLeft = 2
              ExplicitTop = 189
              ExplicitHeight = 26
            end
          end
          inherited TabChi_MD_Variables: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 587
            ExplicitHeight = 326
            inherited lblAccelMethod: TLabel
              Left = 239
              Width = 192
              Height = 18
              ExplicitLeft = 239
              ExplicitWidth = 192
              ExplicitHeight = 18
            end
            inherited lblOrderingScheme: TLabel
              Left = 239
              Width = 316
              Height = 18
              ExplicitLeft = 239
              ExplicitWidth = 316
              ExplicitHeight = 18
            end
            inherited lblFillLevel: TLabel
              Left = 86
              Top = 66
              Width = 351
              Height = 18
              ExplicitLeft = 86
              ExplicitTop = 66
              ExplicitWidth = 351
              ExplicitHeight = 18
            end
            inherited lblNumOrtho: TLabel
              Left = 86
              Top = 87
              Width = 479
              Height = 36
              ExplicitLeft = 86
              ExplicitTop = 87
              ExplicitWidth = 479
              ExplicitHeight = 36
            end
            inherited lblResRedCrit: TLabel
              Left = 86
              Top = 157
              Width = 372
              Height = 18
              WordWrap = False
              ExplicitLeft = 86
              ExplicitTop = 157
              ExplicitWidth = 372
              ExplicitHeight = 18
            end
            inherited lblDropTolerance: TLabel
              Left = 86
              Top = 209
              Width = 305
              Height = 18
              WordWrap = False
              ExplicitLeft = 86
              ExplicitTop = 209
              ExplicitWidth = 305
              ExplicitHeight = 18
            end
            inherited lblHeadClosure: TLabel
              Left = 86
              Top = 237
              Width = 440
              Height = 18
              WordWrap = False
              ExplicitLeft = 86
              ExplicitTop = 237
              ExplicitWidth = 440
              ExplicitHeight = 18
            end
            inherited lblMaxIterChimd: TLabel
              Left = 86
              Top = 265
              Width = 462
              Height = 18
              ExplicitLeft = 86
              ExplicitTop = 265
              ExplicitWidth = 462
              ExplicitHeight = 18
            end
            inherited comboAccelMethod: TJvImageComboBox
              Width = 230
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitWidth = 230
              ExplicitHeight = 28
            end
            inherited comboOrderingScheme: TJvImageComboBox
              Width = 230
              Height = 28
              DroppedWidth = 230
              ItemHeight = 22
              ItemIndex = -1
              ExplicitWidth = 230
              ExplicitHeight = 28
            end
            inherited seFillLevel: TJvSpinEdit
              Left = 2
              Height = 26
              ExplicitLeft = 2
              ExplicitHeight = 26
            end
            inherited seNumOrtho: TJvSpinEdit
              Left = 2
              Top = 93
              Height = 26
              ExplicitLeft = 2
              ExplicitTop = 93
              ExplicitHeight = 26
            end
            inherited cbApplyReducedPreconditioning: TCheckBox
              Left = 2
              Top = 131
              ExplicitLeft = 2
              ExplicitTop = 131
            end
            inherited rdeResRedCrit: TRbwDataEntry
              Left = 2
              Top = 154
              ExplicitLeft = 2
              ExplicitTop = 154
            end
            inherited cbUseDropTolerance: TCheckBox
              Left = 2
              Top = 183
              Width = 439
              ExplicitLeft = 2
              ExplicitTop = 183
              ExplicitWidth = 439
            end
            inherited rdeDropTolerance: TRbwDataEntry
              Left = 2
              Top = 206
              ExplicitLeft = 2
              ExplicitTop = 206
            end
            inherited rdeHeadClosure: TRbwDataEntry
              Left = 2
              Top = 234
              ExplicitLeft = 2
              ExplicitTop = 234
            end
            inherited seMaxIterChimd: TJvSpinEdit
              Left = 2
              Top = 262
              Height = 26
              ExplicitLeft = 2
              ExplicitTop = 262
              ExplicitHeight = 26
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgNwt.lblComments
            end
            item
              Control = framePkgNwt.memoComments
            end
            item
              Control = framePkgNwt.rdeHeadTolerance
            end
            item
              Control = framePkgNwt.rdeFluxTolerance
            end
            item
              Control = framePkgNwt.spinMaxOuterIt
            end
            item
              Control = framePkgNwt.rdeThicknessFactor
            end
            item
              Control = framePkgNwt.comboSolverMethod
            end
            item
              Control = framePkgNwt.cbPrintFlag
            end
            item
              Control = framePkgNwt.cbCorrectForCellBottom
            end
            item
              Control = framePkgNwt.comboOptions
            end
            item
              Control = framePkgNwt.rdeDbdTheta
            end
            item
              Control = framePkgNwt.rdeDbdKappa
            end
            item
              Control = framePkgNwt.rdeDbdGamma
            end
            item
              Control = framePkgNwt.rdeMomentumCoefficient
            end
            item
              Control = framePkgNwt.cbUseResidualControl
            end
            item
              Control = framePkgNwt.seMaxReductions
            end
            item
              Control = framePkgNwt.rdeBackTol
            end
            item
              Control = framePkgNwt.rdeReductionFactor
            end
            item
              Control = framePkgNwt.seMaxIterationsGmres
            end
            item
              Control = framePkgNwt.comboIluMethod
            end
            item
              Control = framePkgNwt.rdeTolerance
            end
            item
              Control = framePkgNwt.seRestarts
            end
            item
              Control = framePkgNwt.comboAccelMethod
            end
            item
              Control = framePkgNwt.comboOrderingScheme
            end
            item
              Control = framePkgNwt.seFillLevel
            end
            item
              Control = framePkgNwt.seNumOrtho
            end
            item
              Control = framePkgNwt.cbApplyReducedPreconditioning
            end
            item
              Control = framePkgNwt.rdeResRedCrit
            end
            item
              Control = framePkgNwt.cbUseDropTolerance
            end
            item
              Control = framePkgNwt.rdeDropTolerance
            end
            item
              Control = framePkgNwt.rdeHeadClosure
            end
            item
              Control = framePkgNwt.seMaxIterChimd
            end>
          OnEnabledChange = framePkgNwtrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspMt3dmsBasic: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'BTN_Basic_Transport_Package'
      Caption = 'jvspMt3dmsBasic'
      inline framePkgMt3dBasic: TframeMt3dBasicPkg
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 294
          Height = 18
          Caption = 'Comments (first two lines are the Heading)'
          ExplicitWidth = 294
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited pcMt3d_Basic: TPageControl
          Width = 595
          Height = 396
          ActivePage = framePkgMt3dBasic.tabMT3D_Options
          ExplicitWidth = 595
          ExplicitHeight = 396
          inherited tabSpecies: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 587
            ExplicitHeight = 363
            inherited pnlSpecies: TPanel
              Width = 587
              Height = 363
              ExplicitWidth = 587
              ExplicitHeight = 363
              inherited Splitter1: TSplitter
                Left = 300
                Height = 361
                ExplicitLeft = 300
                ExplicitHeight = 321
              end
              inherited frameGridImmobile: TframeGrid
                Left = 305
                Width = 281
                Height = 361
                ExplicitLeft = 305
                ExplicitWidth = 281
                ExplicitHeight = 361
                inherited Panel: TPanel
                  Top = 320
                  Width = 281
                  ExplicitTop = 320
                  ExplicitWidth = 281
                  inherited lbNumber: TLabel
                    Width = 55
                    Height = 18
                    ExplicitWidth = 55
                    ExplicitHeight = 18
                  end
                  inherited sbDelete: TSpeedButton
                    Left = 203
                    ExplicitLeft = 203
                  end
                  inherited seNumber: TJvSpinEdit
                    Height = 26
                    ExplicitHeight = 26
                  end
                end
                inherited Grid: TRbwDataGrid4
                  Width = 281
                  Height = 320
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
                      ButtonWidth = 80
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
                  ExplicitWidth = 281
                  ExplicitHeight = 320
                end
              end
              inherited frameGridMobile: TframeGrid
                Width = 299
                Height = 361
                ExplicitWidth = 299
                ExplicitHeight = 361
                inherited Panel: TPanel
                  Top = 320
                  Width = 299
                  ExplicitTop = 320
                  ExplicitWidth = 299
                  inherited lbNumber: TLabel
                    Width = 55
                    Height = 18
                    ExplicitWidth = 55
                    ExplicitHeight = 18
                  end
                  inherited seNumber: TJvSpinEdit
                    Height = 26
                    ExplicitHeight = 26
                  end
                end
                inherited Grid: TRbwDataGrid4
                  Width = 299
                  Height = 320
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
                      ButtonWidth = 80
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
                  ExplicitWidth = 299
                  ExplicitHeight = 320
                end
              end
            end
          end
          inherited tabMT3D_Options: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 109
            ExplicitHeight = 7
            inherited lblInactiveConcentration: TLabel
              Width = 278
              Height = 18
              ExplicitWidth = 278
              ExplicitHeight = 18
            end
            inherited lblInitialConcentrationChoice: TLabel
              Width = 294
              Height = 18
              ExplicitWidth = 294
              ExplicitHeight = 18
            end
            inherited lblMinimumSaturatedFraction: TLabel
              Width = 259
              Height = 18
              ExplicitWidth = 259
              ExplicitHeight = 18
            end
            inherited lblVersion: TLabel
              Width = 100
              Height = 18
              ExplicitWidth = 100
              ExplicitHeight = 18
            end
            inherited grpInitialConcentrationTimes: TGroupBox
              Width = 576
              ExplicitWidth = 576
              inherited lblStressPeriod: TLabel
                Left = 8
                Width = 94
                Height = 18
                ExplicitLeft = 8
                ExplicitWidth = 94
                ExplicitHeight = 18
              end
              inherited lblTimeStep: TLabel
                Left = 216
                Width = 68
                Height = 18
                ExplicitLeft = 216
                ExplicitWidth = 68
                ExplicitHeight = 18
              end
              inherited lblTransportStep: TLabel
                Left = 392
                Width = 99
                Height = 18
                ExplicitLeft = 392
                ExplicitWidth = 99
                ExplicitHeight = 18
              end
              inherited seStressPeriod: TJvSpinEdit
                Left = 108
                Top = 21
                Height = 26
                TabOrder = 0
                ExplicitLeft = 108
                ExplicitTop = 21
                ExplicitHeight = 26
              end
              inherited seTimeStep: TJvSpinEdit
                Left = 290
                Height = 26
                TabOrder = 1
                ExplicitLeft = 290
                ExplicitHeight = 26
              end
              inherited seTransportStep: TJvSpinEdit
                Left = 497
                Height = 26
                TabOrder = 2
                ExplicitLeft = 497
                ExplicitHeight = 26
              end
            end
            inherited comboInitialConcentrationChoice: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited edMassUnit: TLabeledEdit
              Width = 65
              Height = 26
              EditLabel.Width = 127
              EditLabel.Height = 18
              EditLabel.ExplicitLeft = 80
              EditLabel.ExplicitTop = 55
              EditLabel.ExplicitWidth = 127
              EditLabel.ExplicitHeight = 18
              ExplicitWidth = 65
              ExplicitHeight = 26
            end
            inherited comboVersion: TComboBox
              Height = 26
              OnChange = framePkgMt3dBasiccomboVersionChange
              ExplicitHeight = 26
            end
          end
          inherited tabMT3D_USGS_Options: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 520
            ExplicitHeight = 279
            inherited chklstOptions: TJvgCheckListBox
              Height = 279
              ExplicitHeight = 279
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgMt3dBasic.lblComments
            end
            item
              Control = framePkgMt3dBasic.memoComments
            end
            item
              Control = framePkgMt3dBasic.edMassUnit
            end
            item
              Control = framePkgMt3dBasic.rdeInactiveConcentration
            end
            item
              Control = framePkgMt3dBasic.rdeMinimumSaturatedFraction
            end
            item
              Control = framePkgMt3dBasic.frameGridMobile
            end
            item
              Control = framePkgMt3dBasic.frameGridImmobile
            end>
          OnEnabledChange = framePkgMt3dBasicrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspMt3dmsGCG: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'GCG_Generalized_Conjugate_Grad'
      Caption = 'jvspMt3dmsGCG'
      inline frameMt3dmsGcgPackage: TframeMt3dmsGcgPackage
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblMaxOuter: TLabel
          Width = 309
          Height = 18
          ExplicitWidth = 309
          ExplicitHeight = 18
        end
        inherited lblMaxInner: TLabel
          Width = 293
          Height = 18
          ExplicitWidth = 293
          ExplicitHeight = 18
        end
        inherited lblPreconditioner: TLabel
          Width = 172
          Height = 18
          ExplicitWidth = 172
          ExplicitHeight = 18
        end
        inherited lblDispersion: TLabel
          Width = 251
          Height = 18
          ExplicitWidth = 251
          ExplicitHeight = 18
        end
        inherited lblRelaxationFactor: TLabel
          Width = 174
          Height = 18
          ExplicitWidth = 174
          ExplicitHeight = 18
        end
        inherited lblConvergence: TLabel
          Width = 234
          Height = 18
          ExplicitWidth = 234
          ExplicitHeight = 18
        end
        inherited lblPrintoutInterval: TLabel
          Width = 181
          Height = 18
          ExplicitWidth = 181
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited spinMaxOuter: TJvSpinEdit
          Height = 26
          ExplicitHeight = 26
        end
        inherited spinMaxInner: TJvSpinEdit
          Height = 26
          ExplicitHeight = 26
        end
        inherited comboPreconditioner: TComboBox
          Width = 259
          Height = 26
          ExplicitWidth = 259
          ExplicitHeight = 26
        end
        inherited comboDispersion: TComboBox
          Width = 425
          Height = 26
          ExplicitWidth = 425
          ExplicitHeight = 26
        end
        inherited spinPrintoutInterval: TJvSpinEdit
          Height = 26
          ExplicitHeight = 26
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = frameMt3dmsGcgPackage.lblComments
            end
            item
              Control = frameMt3dmsGcgPackage.memoComments
            end
            item
              Control = frameMt3dmsGcgPackage.spinMaxOuter
            end
            item
              Control = frameMt3dmsGcgPackage.spinMaxInner
            end
            item
              Control = frameMt3dmsGcgPackage.comboPreconditioner
            end
            item
              Control = frameMt3dmsGcgPackage.comboDispersion
            end
            item
              Control = frameMt3dmsGcgPackage.rdeConvergence
            end
            item
              Control = frameMt3dmsGcgPackage.spinPrintoutInterval
            end>
        end
      end
    end
    object jvspMt3dmsAdv: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'ADV_Advection_Package_Pane'
      Caption = 'jvspMt3dmsAdv'
      inline frameMt3dmsAdvPkg: TframeMt3dmsAdvPkg
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        DesignSize = (
          595
          516)
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited pcAdvection: TPageControl
          Top = 148
          Width = 595
          Height = 368
          ExplicitTop = -208
          ExplicitWidth = 117
          ExplicitHeight = 368
          inherited tabAdvection1: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 109
            ExplicitHeight = 335
            inherited lbllMethod: TLabel
              Top = 6
              Width = 189
              Height = 36
              WordWrap = True
              ExplicitTop = 6
              ExplicitWidth = 189
              ExplicitHeight = 36
            end
            inherited lblParticleTracking: TLabel
              Top = 51
              Width = 251
              Height = 18
              ExplicitTop = 51
              ExplicitWidth = 251
              ExplicitHeight = 18
            end
            inherited lbNumCellsParticle: TLabel
              Left = 2
              Top = 115
              Width = 434
              Height = 36
              WordWrap = True
              ExplicitLeft = 2
              ExplicitTop = 115
              ExplicitWidth = 434
              ExplicitHeight = 36
            end
            inherited lblMaxParticlesCount: TLabel
              Left = 2
              Top = 157
              Width = 369
              Height = 18
              ExplicitLeft = 2
              ExplicitTop = 157
              ExplicitWidth = 369
              ExplicitHeight = 18
            end
            inherited lblConcWeight: TLabel
              Left = 2
              Top = 189
              Width = 321
              Height = 18
              ExplicitLeft = 2
              ExplicitTop = 189
              ExplicitWidth = 321
              ExplicitHeight = 18
            end
            inherited lblNegConcGrad: TLabel
              Left = 2
              Top = 217
              Width = 348
              Height = 18
              ExplicitLeft = 2
              ExplicitTop = 217
              ExplicitWidth = 348
              ExplicitHeight = 18
            end
            inherited lblInitParticlesSmall: TLabel
              Left = 2
              Top = 245
              Width = 373
              Height = 36
              WordWrap = True
              ExplicitLeft = 2
              ExplicitTop = 245
              ExplicitWidth = 373
              ExplicitHeight = 36
            end
            inherited lblInitParticlesLarge: TLabel
              Left = 2
              Top = 289
              Width = 373
              Height = 36
              WordWrap = True
              ExplicitLeft = 2
              ExplicitTop = 289
              ExplicitWidth = 373
              ExplicitHeight = 36
            end
            inherited Label12: TLabel
              Top = 83
              Width = 210
              Height = 18
              ExplicitTop = 83
              ExplicitWidth = 210
              ExplicitHeight = 18
            end
            inherited comboAdvSolScheme: TComboBox
              Left = 224
              Width = 350
              Height = 26
              ItemIndex = 3
              Text = 'Modified method of characterisitics MMOC (2)'
              ExplicitLeft = 224
              ExplicitWidth = 350
              ExplicitHeight = 26
            end
            inherited comboParticleTrackingAlg: TComboBox
              Left = 269
              Top = 48
              Height = 26
              ExplicitLeft = 269
              ExplicitTop = 48
              ExplicitHeight = 26
            end
            inherited adeMaxParticleMovement: TRbwDataEntry
              Left = 485
              Top = 112
              ExplicitLeft = 485
              ExplicitTop = 112
            end
            inherited adeConcWeight: TRbwDataEntry
              Left = 485
              Top = 186
              ExplicitLeft = 485
              ExplicitTop = 186
            end
            inherited adeNeglSize: TRbwDataEntry
              Left = 485
              Top = 214
              ExplicitLeft = 485
              ExplicitTop = 214
            end
            inherited comboAdvWeightingScheme: TComboBox
              Left = 344
              Top = 80
              Width = 230
              Height = 26
              ExplicitLeft = 344
              ExplicitTop = 80
              ExplicitWidth = 230
              ExplicitHeight = 26
            end
            inherited spinMaxParticlesCount: TJvSpinEdit
              Left = 485
              Top = 154
              Height = 26
              ExplicitLeft = 485
              ExplicitTop = 154
              ExplicitHeight = 26
            end
            inherited spinInitParticlesSmall: TJvSpinEdit
              Left = 485
              Top = 242
              Height = 26
              ExplicitLeft = 485
              ExplicitTop = 242
              ExplicitHeight = 26
            end
            inherited spinInitParticlesLarge: TJvSpinEdit
              Left = 485
              Top = 286
              Height = 26
              ExplicitLeft = 485
              ExplicitTop = 286
              ExplicitHeight = 26
            end
          end
          inherited tabAdvection2: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 593
            ExplicitHeight = 303
            inherited lblInitParticlePlacement: TLabel
              Width = 323
              Height = 18
              ExplicitWidth = 323
              ExplicitHeight = 18
            end
            inherited lblInitParticlePlanes: TLabel
              Top = 35
              Width = 361
              Height = 18
              ExplicitTop = 35
              ExplicitWidth = 361
              ExplicitHeight = 18
            end
            inherited lblMinParticles: TLabel
              Left = 2
              Top = 67
              Width = 400
              Height = 18
              ExplicitLeft = 2
              ExplicitTop = 67
              ExplicitWidth = 400
              ExplicitHeight = 18
            end
            inherited lblMaxParticles: TLabel
              Left = 2
              Top = 99
              Width = 309
              Height = 18
              ExplicitLeft = 2
              ExplicitTop = 99
              ExplicitWidth = 309
              ExplicitHeight = 18
            end
            inherited lblSinkParticlePlacement: TLabel
              Left = 2
              Top = 131
              Width = 317
              Height = 18
              ExplicitLeft = 2
              ExplicitTop = 131
              ExplicitWidth = 317
              ExplicitHeight = 18
            end
            inherited lblSinkParticlePlanes: TLabel
              Left = 2
              Top = 163
              Width = 385
              Height = 18
              ExplicitLeft = 2
              ExplicitTop = 163
              ExplicitWidth = 385
              ExplicitHeight = 18
            end
            inherited lblSinkParticleN: TLabel
              Left = 2
              Top = 195
              Width = 404
              Height = 18
              ExplicitLeft = 2
              ExplicitTop = 195
              ExplicitWidth = 404
              ExplicitHeight = 18
            end
            inherited lblCritConcGrad: TLabel
              Left = 2
              Top = 227
              Width = 347
              Height = 18
              ExplicitLeft = 2
              ExplicitTop = 227
              ExplicitWidth = 347
              ExplicitHeight = 18
            end
            inherited comboInitPartPlace: TComboBox
              Left = 437
              Width = 108
              Height = 26
              ExplicitLeft = 437
              ExplicitWidth = 108
              ExplicitHeight = 26
            end
            inherited comboInitPartSinkChoice: TComboBox
              Left = 437
              Top = 128
              Width = 108
              Height = 26
              ExplicitLeft = 437
              ExplicitTop = 128
              ExplicitWidth = 108
              ExplicitHeight = 26
            end
            inherited adeCritRelConcGrad: TRbwDataEntry
              Left = 437
              Top = 224
              Width = 108
              ExplicitLeft = 437
              ExplicitTop = 224
              ExplicitWidth = 108
            end
            inherited spinInitParticlePlanes: TJvSpinEdit
              Left = 437
              Top = 32
              Width = 108
              Height = 26
              ExplicitLeft = 437
              ExplicitTop = 32
              ExplicitWidth = 108
              ExplicitHeight = 26
            end
            inherited spinMinParticles: TJvSpinEdit
              Left = 437
              Top = 64
              Width = 108
              Height = 26
              ExplicitLeft = 437
              ExplicitTop = 64
              ExplicitWidth = 108
              ExplicitHeight = 26
            end
            inherited spinMaxParticles: TJvSpinEdit
              Left = 437
              Top = 96
              Width = 108
              Height = 26
              ExplicitLeft = 437
              ExplicitTop = 96
              ExplicitWidth = 108
              ExplicitHeight = 26
            end
            inherited spinSinkParticlePlanes: TJvSpinEdit
              Left = 437
              Top = 160
              Width = 108
              Height = 26
              ExplicitLeft = 437
              ExplicitTop = 160
              ExplicitWidth = 108
              ExplicitHeight = 26
            end
            inherited spinSinkParticleN: TJvSpinEdit
              Left = 437
              Top = 192
              Width = 108
              Height = 26
              ExplicitLeft = 437
              ExplicitTop = 192
              ExplicitWidth = 108
              ExplicitHeight = 26
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = frameMt3dmsAdvPkg.lblComments
            end
            item
              Control = frameMt3dmsAdvPkg.memoComments
            end
            item
              Control = frameMt3dmsAdvPkg.comboAdvSolScheme
            end
            item
              Control = frameMt3dmsAdvPkg.comboAdvWeightingScheme
            end
            item
              Control = frameMt3dmsAdvPkg.adeMaxParticleMovement
            end
            item
              Control = frameMt3dmsAdvPkg.spinMaxParticlesCount
            end>
        end
      end
    end
    object jvspMt3dmsDsp: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'DSP_Dispersion_Package_Pane'
      Caption = 'jvspMt3dmsDsp'
      inline frameMt3dmsDispersionPkg: TframeMt3dmsDispersionPkg
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          TabOrder = 1
          ExplicitWidth = 564
        end
        inherited cbMultiDiffusion: TCheckBox
          Width = 545
          Height = 44
          TabOrder = 0
          ExplicitWidth = 545
          ExplicitHeight = 44
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = frameMt3dmsDispersionPkg.lblComments
            end
            item
              Control = frameMt3dmsDispersionPkg.memoComments
            end
            item
              Control = frameMt3dmsDispersionPkg.cbMultiDiffusion
            end
            item
              Control = frameMt3dmsDispersionPkg.cbCrossTermsUsed
            end>
        end
      end
    end
    object jvspMt3dmsSsm: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'SSM_Sink__Source_Mixing_Packag'
      Caption = 'jvspMt3dmsSsm'
      inline framePkgSSM: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          Height = 463
          ExplicitWidth = 564
          ExplicitHeight = 463
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgSSM.lblComments
            end
            item
              Control = framePkgSSM.memoComments
            end>
        end
      end
    end
    object jvspMt3dmsRct: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'RCT_Chemical_Reactions_Package'
      Caption = 'jvspMt3dmsRctPkg'
      inline framePkgMt3dmsRct: TframeMt3dmsChemReactionPkg
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        DesignSize = (
          595
          516)
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblSorptionChoice: TLabel
          Width = 182
          Height = 18
          ExplicitWidth = 182
          ExplicitHeight = 18
        end
        inherited lblKineticChoice: TLabel
          Width = 169
          Height = 18
          ExplicitWidth = 169
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited comboSorptionChoice: TJvImageComboBox
          Height = 26
          ExplicitHeight = 26
        end
        inherited comboKineticChoice: TJvImageComboBox
          Height = 26
          ExplicitHeight = 26
        end
        inherited cbInitialConcChoice: TCheckBox
          Width = 561
          ExplicitWidth = 561
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgMt3dmsRct.lblComments
            end
            item
              Control = framePkgMt3dmsRct.memoComments
            end
            item
              Control = framePkgMt3dmsRct.comboSorptionChoice
            end
            item
              Control = framePkgMt3dmsRct.comboKineticChoice
            end
            item
              Control = framePkgMt3dmsRct.cbInitialConcChoice
            end>
        end
      end
    end
    object jvspMt3dmsTOB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'TOB_Transport_Observation_Pack'
      Caption = 'jvspMt3dmsTOB'
      inline framePkgMt3dmsTob: TframeMt3dmsTransObsPkg
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        DesignSize = (
          595
          516)
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited grpbxConcentrationObservations: TGroupBox
          inherited lblConcScaleFactor: TLabel
            Width = 247
            Height = 18
            ExplicitWidth = 247
            ExplicitHeight = 18
          end
          inherited lblSaveType: TLabel
            Top = 70
            Width = 373
            Height = 18
            ExplicitTop = 70
            ExplicitWidth = 373
            ExplicitHeight = 18
          end
          inherited rdeConcScaleFactor: TRbwDataEntry
            Top = 42
            ExplicitTop = 42
          end
          inherited comboSaveConcType: TJvImageComboBox
            Height = 28
            ItemHeight = 22
            ItemIndex = -1
            ExplicitHeight = 28
          end
          inherited cbLogTransform: TCheckBox
            Top = 125
            ExplicitTop = 125
          end
          inherited cbInterpolate: TCheckBox
            Top = 149
            ExplicitTop = 149
          end
        end
        inherited grpbxMassFluxObservations: TGroupBox
          Left = 17
          ExplicitLeft = 17
          inherited lblMassFluxScaleFactor: TLabel
            Width = 212
            Height = 18
            ExplicitWidth = 212
            ExplicitHeight = 18
          end
          inherited lblSaveMassFluxType: TLabel
            Width = 330
            Height = 18
            ExplicitWidth = 330
            ExplicitHeight = 18
          end
          inherited comboSaveMassFluxType: TJvImageComboBox
            Height = 28
            ItemHeight = 22
            ItemIndex = -1
            ExplicitHeight = 28
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgMt3dmsTob.lblComments
            end
            item
              Control = framePkgMt3dmsTob.memoComments
            end
            item
              Control = framePkgMt3dmsTob.cbSaveBinary
            end
            item
              Control = framePkgMt3dmsTob.rdeConcScaleFactor
            end
            item
              Control = framePkgMt3dmsTob.comboSaveConcType
            end
            item
              Control = framePkgMt3dmsTob.cbLogTransform
            end
            item
              Control = framePkgMt3dmsTob.cbInterpolate
            end
            item
              Control = framePkgMt3dmsTob.rdeMassFluxScaleFactor
            end
            item
              Control = framePkgMt3dmsTob.comboSaveMassFluxType
            end>
        end
      end
    end
    object jvspPCGN: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'PCGN_Preconditioned_Conjugate_'
      Caption = 'jvspPCGN'
      inline framePackagePcgn: TframePackagePcgn
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Top = 56
          Width = 76
          Height = 18
          ExplicitTop = 56
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          Anchors = [akLeft, akTop, akRight]
          WordWrap = True
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Top = 80
          Width = 564
          Height = 71
          ExplicitTop = 80
          ExplicitWidth = 564
          ExplicitHeight = 71
        end
        inherited pcControls: TPageControl
          Width = 595
          Height = 359
          ExplicitTop = -199
          ExplicitWidth = 117
          ExplicitHeight = 359
          inherited tabBasic: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 592
            ExplicitHeight = 360
            inherited lblIter_mo: TLabel
              Width = 389
              Height = 18
              ExplicitWidth = 389
              ExplicitHeight = 18
            end
            inherited lblIter_mi: TLabel
              Width = 368
              Height = 18
              ExplicitWidth = 368
              ExplicitHeight = 18
            end
            inherited lblCLOSE_R: TLabel
              Width = 429
              Height = 18
              ExplicitWidth = 429
              ExplicitHeight = 18
            end
            inherited lblClose_H: TLabel
              Width = 409
              Height = 18
              ExplicitWidth = 409
              ExplicitHeight = 18
            end
            inherited lblRelax: TLabel
              Width = 216
              Height = 18
              ExplicitWidth = 216
              ExplicitHeight = 18
            end
            inherited lblIfill: TLabel
              Width = 281
              Height = 18
              ExplicitWidth = 281
              ExplicitHeight = 18
            end
            inherited seIter_mo: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited seIter_mi: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited seIfill: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited cbUnit_pc: TCheckBox
              Width = 470
              ExplicitWidth = 470
            end
          end
          inherited tabNonLinear: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 109
            ExplicitHeight = 326
            inherited lblDampingMode: TLabel
              Width = 180
              Height = 18
              ExplicitWidth = 180
              ExplicitHeight = 18
            end
            inherited lblDamp: TLabel
              Width = 251
              Height = 18
              ExplicitWidth = 251
              ExplicitHeight = 18
            end
            inherited lblDamp_Lb: TLabel
              Width = 238
              Height = 18
              ExplicitWidth = 238
              ExplicitHeight = 18
            end
            inherited lblDamp_D: TLabel
              Width = 324
              Height = 18
              ExplicitWidth = 324
              ExplicitHeight = 18
            end
            inherited lblChglimit: TLabel
              Width = 247
              Height = 18
              ExplicitWidth = 247
              ExplicitHeight = 18
            end
            inherited lblAcnvg: TLabel
              Width = 208
              Height = 18
              ExplicitWidth = 208
              ExplicitHeight = 18
            end
            inherited lblChvg_Lb: TLabel
              Width = 361
              Height = 18
              ExplicitWidth = 361
              ExplicitHeight = 18
            end
            inherited lblMcnvg: TLabel
              Width = 374
              Height = 36
              WordWrap = True
              ExplicitWidth = 374
              ExplicitHeight = 36
            end
            inherited lblRate_C: TLabel
              Top = 255
              Width = 316
              Height = 18
              ExplicitTop = 255
              ExplicitWidth = 316
              ExplicitHeight = 18
            end
            inherited lblIpunit: TLabel
              Top = 280
              Width = 349
              Height = 18
              ExplicitTop = 280
              ExplicitWidth = 349
              ExplicitHeight = 18
            end
            inherited comboDampingMode: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitHeight = 28
            end
            inherited comboAcnvg: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitHeight = 28
            end
            inherited seMcnvg: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited rdeRate_C: TRbwDataEntry
              Top = 252
              ExplicitTop = 252
            end
            inherited comboIpunit: TJvImageComboBox
              Top = 280
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitTop = 280
              ExplicitHeight = 28
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePackagePcgn.lblComments
            end
            item
              Control = framePackagePcgn.memoComments
            end
            item
              Control = framePackagePcgn.seIter_mo
            end
            item
              Control = framePackagePcgn.seIter_mi
            end
            item
              Control = framePackagePcgn.rdeCLOSE_R
            end
            item
              Control = framePackagePcgn.rdeClose_H
            end
            item
              Control = framePackagePcgn.rdeRelax
            end
            item
              Control = framePackagePcgn.seIfill
            end
            item
              Control = framePackagePcgn.cbUnit_pc
            end
            item
              Control = framePackagePcgn.cbUnit_ts
            end
            item
              Control = framePackagePcgn.comboDampingMode
            end
            item
              Control = framePackagePcgn.rdeDamp
            end
            item
              Control = framePackagePcgn.rdeDamp_Lb
            end
            item
              Control = framePackagePcgn.rdeRate_D
            end
            item
              Control = framePackagePcgn.rdeChglimit
            end
            item
              Control = framePackagePcgn.comboAcnvg
            end
            item
              Control = framePackagePcgn.comboIpunit
            end>
        end
      end
    end
    object jvspSTR: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'STR_Stream_package'
      Caption = 'jvspSTR'
      inline framePkgStr: TframePackageStr
        Left = 0
        Top = 0
        Width = 595
        Height = 201
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 201
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          TabOrder = 1
          ExplicitWidth = 564
        end
        inherited cbCalculateStage: TCheckBox
          Height = 20
          TabOrder = 0
          ExplicitHeight = 20
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgStr.lblComments
            end
            item
              Control = framePkgStr.memoComments
            end
            item
              Control = framePkgStr.cbCalculateStage
            end
            item
              Control = frameStrParameterDefinition
            end>
        end
      end
      inline frameStrParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 201
        Width = 595
        Height = 315
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 201
        ExplicitWidth = 595
        ExplicitHeight = 315
        inherited pnlParameterCount: TPanel
          Top = 267
          Width = 595
          ExplicitTop = 267
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 491
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 491
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 261
          ExplicitWidth = 589
          ExplicitHeight = 261
        end
      end
    end
    object jvspSTOB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'STOB_Stream_Observation_Packag'
      Caption = 'jvspSTOB'
      inline framePkgSTOB: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          Height = 446
          ExplicitWidth = 564
          ExplicitHeight = 446
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgSTOB.lblComments
            end
            item
              Control = framePkgSTOB.memoComments
            end>
        end
      end
    end
    object jvspFHB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'FHB_Flow_and_Head_Boundary_Pac'
      Caption = 'jvspFHB'
      inline framePkgFHB: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          Height = 446
          ExplicitWidth = 564
          ExplicitHeight = 446
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgFHB.lblComments
            end
            item
              Control = framePkgFHB.memoComments
            end>
        end
      end
    end
    object jvspFMP: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'FMP_Farm_Process'
      Caption = 'jvspFMP'
      inline frameFmpParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 362
        Width = 595
        Height = 154
        Align = alBottom
        Enabled = False
        TabOrder = 0
        TabStop = True
        ExplicitTop = 362
        ExplicitWidth = 595
        ExplicitHeight = 154
        inherited pnlParameterCount: TPanel
          Top = 106
          Width = 595
          ExplicitTop = 106
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 491
            ExplicitLeft = 491
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 100
          ExplicitWidth = 589
          ExplicitHeight = 100
        end
      end
      inline framePkgFrm: TframePkgFarm
        Left = 0
        Top = 0
        Width = 595
        Height = 362
        Align = alClient
        TabOrder = 1
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 362
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited splttrFarm: TJvNetscapeSplitter
          Left = 128
          Height = 362
          ExplicitLeft = 128
          ExplicitHeight = 350
        end
        inherited memoComments: TMemo
          Width = 163
          Height = 49
          ExplicitWidth = 163
          ExplicitHeight = 49
        end
        inherited jvplFarm: TJvPageList
          Left = 138
          Width = 457
          Height = 362
          OnChange = framePkgFrmjvplFarmChange
          ExplicitLeft = 138
          ExplicitWidth = 457
          ExplicitHeight = 6
          inherited jvspOptions: TJvStandardPage
            Width = 457
            Height = 362
            ExplicitWidth = 457
            ExplicitHeight = 6
            inherited lblCropIrrigationRequirement: TLabel
              Top = 320
              Width = 322
              Height = 18
              ExplicitTop = 320
              ExplicitWidth = 322
              ExplicitHeight = 18
            end
            inherited lblRecomputeFlows: TLabel
              Top = 381
              Width = 419
              Height = 18
              ExplicitTop = 381
              ExplicitWidth = 419
              ExplicitHeight = 18
            end
            inherited rgAssignmentMethod: TRadioGroup
              Top = 255
              Width = 345
              ExplicitTop = 255
              ExplicitWidth = 345
            end
            inherited comboCropIrrigationRequirement: TComboBox
              Top = 342
              Height = 26
              ExplicitTop = 342
              ExplicitHeight = 26
            end
            inherited comboRecomputeFlows: TComboBox
              Top = 403
              Height = 26
              ExplicitTop = 403
              ExplicitHeight = 26
            end
          end
          inherited jvspParameters: TJvStandardPage
            Width = 457
            Height = 360
            ExplicitWidth = 457
            ExplicitHeight = 360
          end
          inherited jvspWhenToRead: TJvStandardPage
            Width = 457
            Height = 360
            ExplicitWidth = 457
            ExplicitHeight = 360
            inherited lblRootingDepth: TLabel
              Width = 153
              Height = 18
              ExplicitWidth = 153
              ExplicitHeight = 18
            end
            inherited lblConsumptiveUse: TLabel
              Width = 179
              Height = 18
              ExplicitWidth = 179
              ExplicitHeight = 18
            end
            inherited lblPrecipitation: TLabel
              Width = 135
              Height = 18
              ExplicitWidth = 135
              ExplicitHeight = 18
            end
            inherited lblInefficiencyLosses: TLabel
              Width = 280
              Height = 18
              ExplicitWidth = 280
              ExplicitHeight = 18
            end
            inherited comboRootingDepth: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboConsumptiveUse: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboInefficiencyLosses: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboPrecipitation: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
          end
          inherited jvspWaterPolicy: TJvStandardPage
            Width = 457
            Height = 360
            ExplicitWidth = 457
            ExplicitHeight = 360
            inherited lblDeficiency: TLabel
              Width = 186
              Height = 18
              ExplicitWidth = 186
              ExplicitHeight = 18
            end
            inherited comboDeficiency: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited frameEfficiencyBehavior: TframeRadioGrid
              Width = 457
              ExplicitWidth = 457
              inherited grpDescription: TGroupBox
                Width = 454
                ExplicitWidth = 454
                inherited lblTop: TLabel
                  Left = 168
                  Width = 214
                  Height = 18
                  ExplicitLeft = 168
                  ExplicitWidth = 214
                  ExplicitHeight = 18
                end
                inherited lblLeft: TMMJLabel
                  Width = 18
                  Height = 106
                  ExplicitWidth = 18
                  ExplicitHeight = 106
                end
                inherited rdgGrid: TRbwDataGrid4
                  Top = 40
                  Width = 417
                  Height = 233
                  Margins.Top = 20
                  ExplicitTop = 40
                  ExplicitWidth = 417
                  ExplicitHeight = 233
                end
              end
            end
          end
          inherited jvspCropConsumptiveUse: TJvStandardPage
            Width = 457
            Height = 360
            ExplicitWidth = 457
            ExplicitHeight = 360
            inherited frameCropConsumptiveUse: TframeRadioGrid
              Width = 457
              Height = 360
              ExplicitWidth = 457
              ExplicitHeight = 360
              inherited grpDescription: TGroupBox
                Width = 454
                Height = 357
                ExplicitWidth = 454
                ExplicitHeight = 357
                inherited lblTop: TLabel
                  Width = 185
                  Height = 18
                  ExplicitWidth = 185
                  ExplicitHeight = 18
                end
                inherited lblLeft: TMMJLabel
                  Width = 18
                  Height = 188
                  ExplicitWidth = 18
                  ExplicitHeight = 188
                end
                inherited rdgGrid: TRbwDataGrid4
                  Top = 40
                  Width = 417
                  Height = 312
                  FixedCols = 0
                  OnSelectCell = frameCropConsumptiveUserdgGridSelectCell
                  ExplicitTop = 40
                  ExplicitWidth = 417
                  ExplicitHeight = 312
                end
              end
            end
          end
          inherited jvspSurfaceWater: TJvStandardPage
            Width = 457
            Height = 360
            ExplicitWidth = 457
            ExplicitHeight = 360
            inherited lblRoutedDelivery: TLabel
              Width = 264
              Height = 18
              ExplicitWidth = 264
              ExplicitHeight = 18
            end
            inherited lblRoutedReturnFlow: TLabel
              Width = 318
              Height = 18
              ExplicitWidth = 318
              ExplicitHeight = 18
            end
            inherited lblAllotment: TLabel
              Width = 255
              Height = 18
              ExplicitWidth = 255
              ExplicitHeight = 18
            end
            inherited lblDiversionCriterion: TLabel
              Width = 260
              Height = 18
              ExplicitWidth = 260
              ExplicitHeight = 18
            end
            inherited comboRoutedDelivery: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboRoutedReturnFlow: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboAllotment: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
          end
          inherited jvspMandatoryPrintFlags1: TJvStandardPage
            Width = 457
            Height = 360
            ExplicitWidth = 457
            ExplicitHeight = 360
            inherited lblSaveWellFlowRates: TLabel
              Width = 245
              Height = 18
              ExplicitWidth = 245
              ExplicitHeight = 18
            end
            inherited lblSaveRecharge: TLabel
              Top = 72
              Width = 199
              Height = 18
              ExplicitTop = 72
              ExplicitWidth = 199
              ExplicitHeight = 18
            end
            inherited lblSupplyAndDemand: TLabel
              Top = 128
              Width = 208
              Height = 18
              ExplicitTop = 128
              ExplicitWidth = 208
              ExplicitHeight = 18
            end
            inherited comboSaveWellFlowRates: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboSaveRecharge: TComboBox
              Top = 94
              Height = 26
              ExplicitTop = 94
              ExplicitHeight = 26
            end
            inherited comboSupplyAndDemand: TComboBox
              Top = 150
              Height = 26
              ExplicitTop = 150
              ExplicitHeight = 26
            end
          end
          inherited jvspOptionalPrintFlags: TJvStandardPage
            Width = 457
            Height = 360
            ExplicitWidth = 457
            ExplicitHeight = 360
            inherited lblDiversionBudgetLocation: TLabel
              Width = 244
              Height = 18
              ExplicitWidth = 244
              ExplicitHeight = 18
            end
            inherited frameAcreageOptimizationPrintSettings: TframeRadioGrid
              Top = 60
              Width = 457
              Height = 300
              TabOrder = 1
              ExplicitTop = 60
              ExplicitWidth = 457
              ExplicitHeight = 300
              inherited grpDescription: TGroupBox
                Width = 454
                Height = 297
                ExplicitWidth = 454
                ExplicitHeight = 297
                inherited lblTop: TLabel
                  Left = 256
                  Width = 90
                  Height = 18
                  ExplicitLeft = 256
                  ExplicitWidth = 90
                  ExplicitHeight = 18
                end
                inherited lblLeft: TMMJLabel
                  Width = 18
                  Height = 86
                  ExplicitWidth = 18
                  ExplicitHeight = 86
                end
                inherited rdgGrid: TRbwDataGrid4
                  Top = 40
                  Width = 401
                  Height = 252
                  FixedCols = 0
                  ExplicitTop = 40
                  ExplicitWidth = 401
                  ExplicitHeight = 252
                end
              end
            end
            inherited comboDiversionBudgetLocation: TComboBox
              Height = 26
              TabOrder = 2
              ExplicitHeight = 26
            end
            inherited frameRoutingInformationPrintFlag: TframeRadioGrid
              Width = 457
              ExplicitWidth = 457
              inherited grpDescription: TGroupBox
                Width = 454
                ExplicitWidth = 454
                inherited lblTop: TLabel
                  Width = 60
                  Height = 18
                  ExplicitWidth = 60
                  ExplicitHeight = 18
                end
                inherited lblLeft: TMMJLabel
                  Width = 77
                  Height = 18
                  ExplicitWidth = 77
                  ExplicitHeight = 18
                end
                inherited rdgGrid: TRbwDataGrid4
                  Top = 40
                  Width = 401
                  Height = 97
                  FixedCols = 0
                  ExplicitTop = 40
                  ExplicitWidth = 401
                  ExplicitHeight = 97
                end
              end
            end
          end
          inherited jvspMandatoryPrintFlags2: TJvStandardPage
            Width = 457
            Height = 360
            ExplicitWidth = 457
            ExplicitHeight = 360
            inherited frameET_PrintFlag: TframeRadioGrid
              Width = 457
              ExplicitWidth = 457
              inherited grpDescription: TGroupBox
                Width = 454
                ExplicitWidth = 454
                inherited lblTop: TLabel
                  Width = 87
                  Height = 18
                  ExplicitWidth = 87
                  ExplicitHeight = 18
                end
                inherited lblLeft: TMMJLabel
                  Left = 3
                  Top = 29
                  Width = 94
                  Height = 18
                  ExplicitLeft = 3
                  ExplicitTop = 29
                  ExplicitWidth = 94
                  ExplicitHeight = 18
                end
                inherited rdgGrid: TRbwDataGrid4
                  Top = 40
                  Width = 401
                  Height = 163
                  FixedCols = 0
                  ExplicitTop = 40
                  ExplicitWidth = 401
                  ExplicitHeight = 163
                end
              end
            end
            inherited frameFarmBudgetPrintFlag: TframeRadioGrid
              Top = 81
              Width = 457
              Height = 279
              ExplicitTop = 81
              ExplicitWidth = 457
              ExplicitHeight = 279
              inherited grpDescription: TGroupBox
                Width = 454
                Height = 276
                ExplicitWidth = 454
                ExplicitHeight = 276
                inherited lblTop: TLabel
                  Width = 65
                  Height = 18
                  ExplicitWidth = 65
                  ExplicitHeight = 18
                end
                inherited lblLeft: TMMJLabel
                  Top = 24
                  Width = 18
                  Height = 146
                  ExplicitTop = 24
                  ExplicitWidth = 18
                  ExplicitHeight = 146
                end
                inherited rdgGrid: TRbwDataGrid4
                  Top = 40
                  Width = 401
                  Height = 231
                  FixedCols = 0
                  ExplicitTop = 40
                  ExplicitWidth = 401
                  ExplicitHeight = 231
                end
              end
            end
          end
          inherited jvspMnwNwtOptions: TJvStandardPage
            Width = 457
            Height = 360
            ExplicitWidth = 457
            ExplicitHeight = 360
            inherited lblMnwExplanation: TLabel
              Width = 309
              Height = 36
              ExplicitWidth = 309
              ExplicitHeight = 36
            end
            inherited grpMNWOptions: TGroupBox
              Width = 457
              ExplicitWidth = 457
              inherited lblRPCT: TLabel
                Width = 280
                Height = 54
                ExplicitWidth = 280
                ExplicitHeight = 54
              end
              inherited lblHPCT: TLabel
                Width = 258
                Height = 54
                ExplicitWidth = 258
                ExplicitHeight = 54
              end
              inherited lblQClose: TLabel
                Width = 282
                Height = 54
                ExplicitWidth = 282
                ExplicitHeight = 54
              end
            end
            inherited grpNwtOptions: TGroupBox
              Width = 457
              ExplicitWidth = 457
              inherited lblPSIRAMPF: TLabel
                Width = 282
                Height = 54
                ExplicitWidth = 282
                ExplicitHeight = 54
              end
              inherited lblSATTHK: TLabel
                Width = 265
                Height = 54
                ExplicitWidth = 265
                ExplicitHeight = 54
              end
            end
          end
        end
        inherited tvpglstFarm: TJvPageListTreeView
          Width = 128
          Height = 362
          Items.Links = {00000000}
          ExplicitWidth = 128
          ExplicitHeight = 362
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgFrm.lblComments
            end
            item
              Control = framePkgFrm.memoComments
            end
            item
              Control = framePkgFrm.comboRecomputeFlows
            end
            item
              Control = framePkgFrm.rgAssignmentMethod
            end
            item
              Control = framePkgFrm.comboCropIrrigationRequirement
            end
            item
              Control = framePkgFrm.frameCropConsumptiveUse
            end
            item
              Control = framePkgFrm.comboSaveWellFlowRates
            end
            item
              Control = framePkgFrm.comboSaveRecharge
            end
            item
              Control = framePkgFrm.comboSupplyAndDemand
            end
            item
              Control = framePkgFrm.frameFarmBudgetPrintFlag
            end
            item
              Control = framePkgFrm.rgAssignmentMethod
            end
            item
              Control = framePkgFrm.comboRoutedDelivery
            end
            item
              Control = framePkgFrm.comboRoutedReturnFlow
            end
            item
              Control = framePkgFrm.comboAllotment
            end
            item
              Control = framePkgFrm.frameEfficiencyBehavior
            end
            item
              Control = framePkgFrm.comboDeficiency
            end
            item
              Control = framePkgFrm.comboRootingDepth
            end
            item
              Control = framePkgFrm.comboConsumptiveUse
            end
            item
              Control = framePkgFrm.comboPrecipitation
            end
            item
              Control = framePkgFrm.comboInefficiencyLosses
            end
            item
              Control = frameFmpParameterDefinition
            end
            item
              Control = framePkgFrm.cbGroundwaterAllotments
            end
            item
              Control = framePkgFrm.cbResetQMax
            end
            item
              Control = framePkgFrm.cbMnwClose
            end
            item
              Control = framePkgFrm.rdePSIRAMPF
            end
            item
              Control = framePkgFrm.rdeSATTHK
            end>
          OnEnabledChange = framePkgFrmrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspCFP: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'CFP_Conduit_Flow_Process'
      Caption = 'jvspCFP'
      inline framePkgCFP: TframePackageCFP
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited pgcConduits: TPageControl
          Top = 137
          Width = 595
          ExplicitTop = -219
          ExplicitWidth = 117
          inherited tabCFP: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 109
            ExplicitHeight = 346
            inherited lblLayerTemperature: TLabel
              Width = 339
              Height = 18
              ExplicitWidth = 339
              ExplicitHeight = 18
            end
            inherited lblRelaxationParameter: TLabel
              Width = 216
              Height = 18
              ExplicitWidth = 216
              ExplicitHeight = 18
            end
            inherited lblMaxIterations: TLabel
              Width = 267
              Height = 18
              ExplicitWidth = 267
              ExplicitHeight = 18
            end
            inherited lblEpsilon: TLabel
              Width = 268
              Height = 18
              ExplicitWidth = 268
              ExplicitHeight = 18
            end
            inherited lblPipeExchange: TLabel
              Width = 401
              Height = 18
              ExplicitWidth = 401
              ExplicitHeight = 18
            end
            inherited lblPipeElevationOffset: TLabel
              Width = 252
              Height = 18
              ExplicitWidth = 252
              ExplicitHeight = 18
            end
            inherited lblElevationChoice: TLabel
              Width = 367
              Height = 18
              ExplicitWidth = 367
              ExplicitHeight = 18
            end
            inherited lblConduitTemperature: TLabel
              Width = 403
              Height = 18
              ExplicitWidth = 403
              ExplicitHeight = 18
            end
            inherited cbPipes: TCheckBox
              OnClick = framePkgCFPcbPipesClick
            end
            inherited seMaxIterations: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboPipeExchange: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited comboElevationChoice: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
          end
          inherited tabCRCH_COC: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 522
            ExplicitHeight = 346
            inherited lblOutputInterval: TLabel
              Width = 381
              Height = 18
              ExplicitWidth = 381
              ExplicitHeight = 18
            end
            inherited cbConduitRecharge: TCheckBox
              Width = 493
              ExplicitWidth = 493
            end
            inherited seOutputInterval: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgCFP.lblComments
            end
            item
              Control = framePkgCFP.memoComments
            end
            item
              Control = framePkgCFP.cbPipes
            end
            item
              Control = framePkgCFP.cbLayers
            end>
          OnEnabledChange = framePkgCFPrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspSWI: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'SWI2_Seawater_Intrusion_Packag'
      Caption = 'jvspSWI'
      inline framePackageSWI: TframePackageSWI
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        DesignSize = (
          595
          516)
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited pcSWI: TPageControl
          Width = 595
          Height = 434
          ExplicitWidth = 595
          ExplicitHeight = 434
          inherited tabBasic: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 587
            ExplicitHeight = 401
            inherited lblNumberOfSurfaces: TLabel
              Width = 192
              Height = 36
              ExplicitWidth = 192
              ExplicitHeight = 36
            end
            inherited lblDensityChoice: TLabel
              Left = 364
              Width = 188
              Height = 18
              ExplicitLeft = 364
              ExplicitWidth = 188
              ExplicitHeight = 18
            end
            inherited lblObservations: TLabel
              Left = 256
              Width = 172
              Height = 18
              ExplicitLeft = 256
              ExplicitWidth = 172
              ExplicitHeight = 18
            end
            inherited lblToeslope: TLabel
              Width = 287
              Height = 18
              ExplicitWidth = 287
              ExplicitHeight = 18
            end
            inherited lblTipSlope: TLabel
              Width = 273
              Height = 18
              ExplicitWidth = 273
              ExplicitHeight = 18
            end
            inherited lblAlpha: TLabel
              Width = 407
              Height = 18
              ExplicitTop = 218
              ExplicitWidth = 407
              ExplicitHeight = 18
            end
            inherited lblBeta: TLabel
              Width = 344
              Height = 18
              ExplicitWidth = 344
              ExplicitHeight = 18
            end
            inherited lblMaxAdaptiveSteps: TLabel
              Width = 410
              Height = 36
              ExplicitTop = 252
              ExplicitWidth = 410
              ExplicitHeight = 36
            end
            inherited lblMinAdaptiveSteps: TLabel
              Width = 406
              Height = 36
              ExplicitTop = 290
              ExplicitWidth = 406
              ExplicitHeight = 36
            end
            inherited lblAdaptiveFactor: TLabel
              Width = 458
              Height = 36
              ExplicitTop = 325
              ExplicitWidth = 458
              ExplicitHeight = 36
            end
            inherited lblModflowPrecision: TLabel
              Width = 151
              Height = 18
              ExplicitWidth = 151
              ExplicitHeight = 18
            end
            inherited comboObservations: TJvImageComboBox
              Width = 238
              Height = 28
              DroppedWidth = 238
              ItemHeight = 22
              ExplicitWidth = 238
              ExplicitHeight = 28
            end
            inherited seNumberOfSurfaces: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited cbSaveZeta: TCheckBox
              Width = 301
              ExplicitWidth = 301
            end
            inherited comboDensityChoice: TJvImageComboBox
              Width = 346
              Height = 28
              DroppedWidth = 346
              ItemHeight = 22
              ItemIndex = -1
              ExplicitWidth = 346
              ExplicitHeight = 28
            end
            inherited cbAdaptive: TCheckBox
              Width = 429
              ExplicitWidth = 429
            end
            inherited seMaxAdaptiveSteps: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited seMinAdaptiveSteps: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited rdeAdaptiveFactor: TRbwDataEntry
              Left = 13
              ExplicitLeft = 13
            end
            inherited comboModflowPrecision: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
          end
          inherited tabSolver: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 437
            ExplicitHeight = 407
            inherited lblSolver: TLabel
              Left = 139
              Width = 183
              Height = 18
              ExplicitLeft = 139
              ExplicitWidth = 183
              ExplicitHeight = 18
            end
            inherited lblPrintoutInterval: TLabel
              Top = 35
              Width = 177
              Height = 18
              ExplicitTop = 35
              ExplicitWidth = 177
              ExplicitHeight = 18
            end
            inherited lblPCGPrintControl: TLabel
              Top = 68
              Width = 182
              Height = 18
              Caption = 'Printing control (MUTSOL)'
              ExplicitTop = 68
              ExplicitWidth = 182
              ExplicitHeight = 18
            end
            inherited lblMaxIterOuter: TLabel
              Width = 319
              Height = 18
              ExplicitWidth = 319
              ExplicitHeight = 18
            end
            inherited lblMaxIterInner: TLabel
              Width = 303
              Height = 18
              ExplicitWidth = 303
              ExplicitHeight = 18
            end
            inherited lblPCGMethod: TLabel
              Left = 281
              Top = 151
              Width = 292
              Height = 18
              ExplicitLeft = 281
              ExplicitTop = 151
              ExplicitWidth = 292
              ExplicitHeight = 18
            end
            inherited lblMaxZetaChange: TLabel
              Width = 249
              Height = 18
              ExplicitWidth = 249
              ExplicitHeight = 18
            end
            inherited lblMaxRes: TLabel
              Width = 206
              Height = 18
              ExplicitWidth = 206
              ExplicitHeight = 18
            end
            inherited lblRelax: TLabel
              Width = 216
              Height = 18
              ExplicitWidth = 216
              ExplicitHeight = 18
            end
            inherited lblEigenValue: TLabel
              Left = 187
              Width = 316
              Height = 18
              ExplicitLeft = 187
              ExplicitWidth = 316
              ExplicitHeight = 18
            end
            inherited lblDamp: TLabel
              Width = 168
              Height = 18
              ExplicitWidth = 168
              ExplicitHeight = 18
            end
            inherited lblDampT: TLabel
              Width = 276
              Height = 18
              ExplicitWidth = 276
              ExplicitHeight = 18
            end
            inherited comboSolver: TJvImageComboBox
              Width = 130
              Height = 28
              ItemHeight = 22
              ExplicitWidth = 130
              ExplicitHeight = 28
            end
            inherited sePrintoutInterval: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboPCGPrint: TJvImageComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited seMaxIterOuter: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited seMaxIterInner: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboPCGPrecondMeth: TJvImageComboBox
              Top = 148
              Width = 273
              Height = 26
              DroppedWidth = 273
              ExplicitTop = 148
              ExplicitWidth = 273
              ExplicitHeight = 26
            end
            inherited comboEigenValue: TJvImageComboBox
              Width = 178
              Height = 26
              ExplicitWidth = 178
              ExplicitHeight = 26
            end
          end
          inherited tabDensity: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 437
            ExplicitHeight = 407
            inherited rdgDensity: TRbwDataGrid4
              Height = 407
              FixedCols = 0
              ExplicitHeight = 407
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePackageSWI.lblComments
            end
            item
              Control = framePackageSWI.memoComments
            end
            item
              Control = framePackageSWI.seNumberOfSurfaces
            end
            item
              Control = framePackageSWI.comboDensityChoice
            end
            item
              Control = framePackageSWI.cbSaveZeta
            end
            item
              Control = framePackageSWI.comboObservations
            end
            item
              Control = framePackageSWI.rdeToeslope
            end
            item
              Control = framePackageSWI.rdeTipSlope
            end
            item
              Control = framePackageSWI.rdeAlpha
            end
            item
              Control = framePackageSWI.rdeBeta
            end
            item
              Control = framePackageSWI.cbAdaptive
            end
            item
              Control = framePackageSWI.comboSolver
            end
            item
              Control = framePackageSWI.sePrintoutInterval
            end
            item
              Control = framePackageSWI.comboPCGPrint
            end
            item
              Control = framePackageSWI.rdgDensity
            end>
        end
      end
    end
    object jvspSWR: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'SWR_Surface_Water_Routing_Proc'
      Caption = 'jvspSWR'
      OnShow = jvspSWRShow
      inline framePkgSWR: TframePackageSwr
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited splttrSwr: TJvNetscapeSplitter
          Left = 140
          Height = 516
          ExplicitLeft = 140
          ExplicitHeight = 516
        end
        inherited memoComments: TMemo
          Height = 91
          ExplicitHeight = 91
        end
        inherited jvplSwr: TJvPageList
          Left = 150
          Width = 445
          Height = 516
          HelpType = htKeyword
          HelpKeyword = 'SWR_Surface_Water_Routing_Proc'
          OnChange = framePkgSWRjvplSwrChange
          ExplicitLeft = 150
          ExplicitWidth = 445
          ExplicitHeight = 516
          inherited jvspSolutionOptions: TJvStandardPage
            inherited lblScaling: TLabel
              Top = 296
              Width = 52
              Height = 18
              ExplicitTop = 296
              ExplicitWidth = 52
              ExplicitHeight = 18
            end
            inherited lblReordering: TLabel
              Top = 344
              Width = 78
              Height = 18
              ExplicitTop = 344
              ExplicitWidth = 78
              ExplicitHeight = 18
            end
            inherited lblNewton: TLabel
              Top = 398
              Width = 130
              Height = 18
              ExplicitTop = 398
              ExplicitWidth = 130
              ExplicitHeight = 18
            end
            inherited cbSwrOnly: TCheckBox
              Height = 41
              WordWrap = True
              ExplicitHeight = 41
            end
            inherited cbContinueNonConverge: TCheckBox
              Top = 50
              ExplicitTop = 50
            end
            inherited cbUpstreamWeighting: TCheckBox
              Top = 90
              ExplicitTop = 90
            end
            inherited cbInexactNewton: TCheckBox
              Top = 130
              ExplicitTop = 130
            end
            inherited cbUseSteadyStateStorage: TCheckBox
              Top = 164
              ExplicitTop = 164
            end
            inherited cbUseLaggedStagesAndFlows: TCheckBox
              Top = 201
              ExplicitTop = 201
            end
            inherited cbUseLinearDepthScaling: TCheckBox
              Top = 249
              Height = 41
              ExplicitTop = 249
              ExplicitHeight = 41
            end
            inherited comboScaling: TJvImageComboBox
              Top = 318
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitTop = 318
              ExplicitHeight = 28
            end
            inherited comboReordering: TJvImageComboBox
              Top = 363
              Height = 28
              ItemHeight = 22
              ExplicitTop = 363
              ExplicitHeight = 28
            end
            inherited comboNewton: TJvImageComboBox
              Top = 420
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitTop = 420
              ExplicitHeight = 28
            end
          end
          inherited jvspTimeStepOptions: TJvStandardPage
            inherited lblInitialTimeStepLength: TLabel
              Width = 199
              Height = 18
              ExplicitWidth = 199
              ExplicitHeight = 18
            end
            inherited lblMinTimeStepLength: TLabel
              Width = 237
              Height = 18
              ExplicitWidth = 237
              ExplicitHeight = 18
            end
            inherited lblMaxTimeStepLength: TLabel
              Width = 249
              Height = 18
              ExplicitWidth = 249
              ExplicitHeight = 18
            end
            inherited lblTimeStepMultiplier: TLabel
              Width = 209
              Height = 18
              ExplicitWidth = 209
              ExplicitHeight = 18
            end
            inherited lblTimeStepIncreaseFrequency: TLabel
              Width = 219
              Height = 36
              ExplicitWidth = 219
              ExplicitHeight = 36
            end
            inherited lblMinGradientForDiffusiveFlow: TLabel
              Width = 242
              Height = 36
              ExplicitWidth = 242
              ExplicitHeight = 36
            end
            inherited lblMinDepthForOutflow: TLabel
              Width = 276
              Height = 18
              ExplicitWidth = 276
              ExplicitHeight = 18
            end
            inherited lblMaxRainfallForStepAdjustment: TLabel
              Width = 255
              Height = 36
              ExplicitWidth = 255
              ExplicitHeight = 36
            end
            inherited lblMaxStageChangePerStep: TLabel
              Width = 230
              Height = 36
              ExplicitWidth = 230
              ExplicitHeight = 36
            end
            inherited lblMaxInflowChange: TLabel
              Width = 230
              Height = 36
              ExplicitWidth = 230
              ExplicitHeight = 36
            end
            inherited seTimeStepIncreaseFrequency: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
          end
          inherited jvspSpecificationMethod: TJvStandardPage
            Width = 445
            Height = 516
            ExplicitWidth = 445
            ExplicitHeight = 516
            inherited grpSpecificationMethod: TGroupBox
              Height = 516
              ExplicitHeight = 516
              inherited rgRainfallSpecification: TRadioGroup
                Items.Strings = (
                  'Specify by reach (> 0)'
                  'Specify by cell (< 0)')
              end
              inherited rgEvapSpecification: TRadioGroup
                Items.Strings = (
                  'Specify by reach (> 0)'
                  'Specify by cell (< 0)')
              end
              inherited rgLateralInflowSpecification: TRadioGroup
                Items.Strings = (
                  'Specify by reach (> 0)'
                  'Specify by cell (< 0)')
              end
              inherited rgStageSpecification: TRadioGroup
                Items.Strings = (
                  'Specify by reach (> 0)'
                  'Specify by cell (< 0)')
              end
            end
            inherited grpAssignmentMethod: TGroupBox
              Left = 227
              Width = 218
              Height = 516
              Align = alClient
              ExplicitLeft = 227
              ExplicitWidth = 218
              ExplicitHeight = 516
              inherited rgRainAssignmentMethod: TRadioGroup
                Width = 212
                ExplicitWidth = 212
              end
              inherited rgEvapAssignmentMethod: TRadioGroup
                Width = 212
                ExplicitWidth = 212
              end
              inherited rgLateralInflowAssignmentMethod: TRadioGroup
                Width = 212
                ExplicitWidth = 212
              end
              inherited rgStageAssignmentMethod: TRadioGroup
                Width = 212
                ExplicitWidth = 212
              end
            end
          end
          inherited jvspPrintOptions: TJvStandardPage
            inherited lblPrintInflowsAndOutflows: TLabel
              Top = 5
              Width = 254
              Height = 36
              ExplicitTop = 5
              ExplicitWidth = 254
              ExplicitHeight = 36
            end
            inherited lblPrintStage: TLabel
              Width = 176
              Height = 18
              ExplicitWidth = 176
              ExplicitHeight = 18
            end
            inherited lblPrintReachExchangeAndProperties: TLabel
              Width = 227
              Height = 18
              ExplicitWidth = 227
              ExplicitHeight = 18
            end
            inherited lblPrintReachLateralFlow: TLabel
              Width = 246
              Height = 18
              ExplicitWidth = 246
              ExplicitHeight = 18
            end
            inherited lblPrintStructureFlow: TLabel
              Width = 226
              Height = 18
              ExplicitWidth = 226
              ExplicitHeight = 18
            end
            inherited lblSaveSwrTimeStepLength: TLabel
              Width = 194
              Height = 36
              ExplicitWidth = 194
              ExplicitHeight = 36
            end
            inherited lblSaveRiver: TLabel
              Top = 338
              Width = 141
              Height = 18
              ExplicitTop = 338
              ExplicitWidth = 141
              ExplicitHeight = 18
            end
            inherited lblSaveObs: TLabel
              Top = 392
              Width = 129
              Height = 18
              ExplicitTop = 392
              ExplicitWidth = 129
              ExplicitHeight = 18
            end
            inherited lblSaveFrequency: TLabel
              Top = 475
              Width = 248
              Height = 36
              ExplicitTop = 475
              ExplicitWidth = 248
              ExplicitHeight = 36
            end
            inherited lblObsFormat: TLabel
              Top = 448
              Width = 215
              Height = 18
              ExplicitTop = 448
              ExplicitWidth = 215
              ExplicitHeight = 18
            end
            inherited comboPrintInflowsAndOutflows: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited comboPrintStage: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited comboPrintReachExchangeAndProperties: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited comboPrintReachLateralFlow: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited comboPrintStructureFlow: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited comboSaveSwrTimeStepLength: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited cbSaveConvergenceHistory: TCheckBox
              Top = 299
              ExplicitTop = 299
            end
            inherited comboSaveRiver: TJvImageComboBox
              Top = 358
              Height = 28
              ItemHeight = 22
              Items = <
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'None'
                end
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'SAVE_RIVER_PACKAGE'
                end
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'SAVE_RIVER_PACKAGE_ALL'
                end>
              ExplicitTop = 358
              ExplicitHeight = 28
            end
            inherited comboSaveObs: TJvImageComboBox
              Top = 414
              Height = 28
              ItemHeight = 22
              Items = <
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'None'
                end
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'SAVE_SWROBSERVATIONS'
                end
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'SAVE_SWROBSERVATIONS_ALL'
                end>
              ExplicitTop = 414
              ExplicitHeight = 28
            end
            inherited rdeSaveFrequency: TRbwDataEntry
              Top = 478
              ExplicitTop = 478
            end
            inherited cbSaveAverageSimulatedResults: TCheckBox
              Top = 257
              ExplicitTop = 257
            end
            inherited comboObsFormat: TJvImageComboBox
              Top = 445
              Height = 28
              ItemHeight = 22
              ExplicitTop = 445
              ExplicitHeight = 28
            end
          end
          inherited jvspSolverMandatory: TJvStandardPage
            inherited lblSolver: TLabel
              Width = 125
              Height = 18
              ExplicitWidth = 125
              ExplicitHeight = 18
            end
            inherited lblMaxOuterIterations: TLabel
              Width = 251
              Height = 36
              ExplicitWidth = 251
              ExplicitHeight = 36
            end
            inherited lblMaxInnerIterations: TLabel
              Width = 250
              Height = 36
              ExplicitWidth = 250
              ExplicitHeight = 36
            end
            inherited lblMaxLineSearchIterations: TLabel
              Width = 223
              Height = 36
              ExplicitWidth = 223
              ExplicitHeight = 36
            end
            inherited lblStageTolerance: TLabel
              Width = 165
              Height = 18
              ExplicitWidth = 165
              ExplicitHeight = 18
            end
            inherited lblFlowToleranceOption: TLabel
              Width = 148
              Height = 18
              ExplicitWidth = 148
              ExplicitHeight = 18
            end
            inherited lblFlowTolerance: TLabel
              Width = 156
              Height = 18
              ExplicitWidth = 156
              ExplicitHeight = 18
            end
            inherited lblExchangeToleranceOption: TLabel
              Width = 184
              Height = 18
              ExplicitWidth = 184
              ExplicitHeight = 18
            end
            inherited lblExchangeTolerance: TLabel
              Width = 192
              Height = 18
              ExplicitWidth = 192
              ExplicitHeight = 18
            end
            inherited lblSteadyStateDampingFactor: TLabel
              Width = 199
              Height = 36
              Caption = 'Steady state damping factor (DAMPSS) '
              WordWrap = True
              ExplicitWidth = 199
              ExplicitHeight = 36
            end
            inherited lblTransientDampingFactor: TLabel
              Top = 361
              Width = 252
              Height = 18
              ExplicitTop = 361
              ExplicitWidth = 252
              ExplicitHeight = 18
            end
            inherited lblConvergencePrintoutInterval: TLabel
              Top = 390
              Width = 277
              Height = 18
              ExplicitTop = 390
              ExplicitWidth = 277
              ExplicitHeight = 18
            end
            inherited lblPrintConvergence: TLabel
              Top = 419
              Width = 209
              Height = 18
              ExplicitTop = 419
              ExplicitWidth = 209
              ExplicitHeight = 18
            end
            inherited comboSolver: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited seMaxOuterIterations: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited seMaxInnerIterations: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited seMaxLineSearchIterations: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboFlowToleranceOption: TJvImageComboBox
              Top = 193
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              Items = <
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'None'
                end
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'USE_FRACTIONAL_TOLR'
                end
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'USE_L2NORM_TOLR'
                end>
              ExplicitTop = 193
              ExplicitHeight = 28
            end
            inherited comboExchangeToleranceOption: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              Items = <
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'None'
                end
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'USE_GLOBAL_TOLA'
                end
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'USE_ABSOLUTE_TOLA'
                end>
              ExplicitHeight = 28
            end
            inherited rdeExchangeTolerance: TRbwDataEntry
              Top = 288
              ExplicitTop = 288
            end
            inherited rdeSteadyStateDampingFactor: TRbwDataEntry
              Top = 322
              ExplicitTop = 322
            end
            inherited rdeTransientDampingFactor: TRbwDataEntry
              Top = 358
              ExplicitTop = 358
            end
            inherited seConvergencePrintoutInterval: TJvSpinEdit
              Top = 386
              Height = 26
              ExplicitTop = 386
              ExplicitHeight = 26
            end
            inherited comboPrintConvergence: TJvImageComboBox
              Top = 438
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              Items = <
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'Print max residual every time step (0)'
                end
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'Print number of iterations (1)'
                end
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'Print none (2)'
                end
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'Print if convergence fails (3)'
                end>
              ExplicitTop = 438
              ExplicitHeight = 28
            end
          end
          inherited jvspSolverOptional: TJvStandardPage
            inherited lblPreconditioner: TLabel
              Left = 255
              Width = 142
              Height = 18
              ExplicitLeft = 255
              ExplicitWidth = 142
              ExplicitHeight = 18
            end
            inherited lblMaxLevels: TLabel
              Width = 270
              Height = 18
              ExplicitWidth = 270
              ExplicitHeight = 18
            end
            inherited lblDropThreshold: TLabel
              Width = 192
              Height = 18
              ExplicitWidth = 192
              ExplicitHeight = 18
            end
            inherited lblPrintLineSearchInterval: TLabel
              Width = 289
              Height = 36
              ExplicitWidth = 289
              ExplicitHeight = 36
            end
            inherited lblAlternativeFlowTolerance: TLabel
              Width = 238
              Height = 18
              ExplicitWidth = 238
              ExplicitHeight = 18
            end
            inherited comboPreconditioner: TJvImageComboBox
              Width = 243
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitWidth = 243
              ExplicitHeight = 28
            end
            inherited seMaxLevels: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited sePrintLineSearchInterval: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
          end
        end
        inherited tvpglstSwr: TJvPageListTreeView
          Width = 140
          Height = 516
          Items.Links = {00000000}
          ExplicitWidth = 140
          ExplicitHeight = 516
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgSWR.lblComments
            end
            item
              Control = framePkgSWR.memoComments
            end>
        end
      end
    end
    object jvspMNW1: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'MNW1_Multi_Node_Well_Package_V'
      Caption = 'jvspMNW1'
      inline framePkgMnw1: TframePackageMnw1
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblMaxIterations: TLabel
          Width = 347
          Height = 18
          ExplicitWidth = 347
          ExplicitHeight = 18
        end
        inherited lblLosstype: TLabel
          Width = 164
          Height = 18
          ExplicitWidth = 164
          ExplicitHeight = 18
        end
        inherited lblLossExponent: TLabel
          Width = 201
          Height = 18
          ExplicitWidth = 201
          ExplicitHeight = 18
        end
        inherited lblWellFileName: TLabel
          Width = 223
          Height = 18
          ExplicitWidth = 223
          ExplicitHeight = 18
        end
        inherited lblByNode: TLabel
          Width = 318
          Height = 18
          ExplicitWidth = 318
          ExplicitHeight = 18
        end
        inherited lblQSum: TLabel
          Width = 310
          Height = 18
          ExplicitWidth = 310
          ExplicitHeight = 18
        end
        inherited lblByNodeFrequency: TLabel
          Width = 73
          Height = 18
          ExplicitWidth = 73
          ExplicitHeight = 18
        end
        inherited lblQSumFrequency: TLabel
          Width = 73
          Height = 18
          ExplicitWidth = 73
          ExplicitHeight = 18
        end
        inherited seMaxIterations: TJvSpinEdit
          Height = 26
          ExplicitHeight = 26
        end
        inherited comboLosstype: TJvImageComboBox
          Height = 28
          ItemHeight = 22
          ItemIndex = -1
          ExplicitHeight = 28
        end
        inherited fedWellFileName: TJvFilenameEdit
          Height = 26
          ExplicitHeight = 26
        end
        inherited fedByNode: TJvFilenameEdit
          Height = 26
          ExplicitHeight = 26
        end
        inherited fedQSum: TJvFilenameEdit
          Height = 26
          ExplicitHeight = 26
        end
        inherited comboByNodeFrequency: TJvImageComboBox
          Height = 28
          ItemHeight = 22
          ExplicitHeight = 28
        end
        inherited comboQSumFrequency: TJvImageComboBox
          Height = 28
          ItemHeight = 22
          ExplicitHeight = 28
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgMnw1.lblComments
            end
            item
              Control = framePkgMnw1.memoComments
            end
            item
              Control = framePkgMnw1.seMaxIterations
            end
            item
              Control = framePkgMnw1.comboLosstype
            end
            item
              Control = framePkgMnw1.fedWellFileName
            end
            item
              Control = framePkgMnw1.fedByNode
            end
            item
              Control = framePkgMnw1.fedQSum
            end>
        end
      end
    end
    object jvspNPF: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'NPF_Node_Property_Flow_Package'
      Caption = 'jvspNPF'
      inline framePkgNpf: TframePackageNpf
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblInterblockMethod: TLabel
          Width = 470
          Height = 18
          ExplicitWidth = 470
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited rdgOptions: TRbwDataGrid4
          Width = 564
          Height = 340
          ExplicitWidth = 564
          ExplicitHeight = 340
        end
        inherited comboInterblockMethod: TJvImageComboBox
          Width = 564
          Height = 28
          DroppedWidth = 564
          ItemHeight = 22
          ExplicitWidth = 564
          ExplicitHeight = 28
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgNpf.lblComments
            end
            item
              Control = framePkgNpf.memoComments
            end
            item
              Control = framePkgNpf.comboInterblockMethod
            end
            item
              Control = framePkgNpf.rdgOptions
            end>
        end
      end
    end
    object jvspSTO: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'STO_Storage_Package_Pane'
      Caption = 'jvspSTO'
      inline framePkgSto: TframePkgSto
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        Enabled = False
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblConfinedStorageMethod: TLabel
          Width = 371
          Height = 18
          ExplicitWidth = 371
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited comboStorageChoice: TJvImageComboBox
          Height = 28
          ItemHeight = 22
          ItemIndex = -1
          ExplicitHeight = 28
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgSto.lblComments
            end
            item
              Control = framePkgSto.memoComments
            end
            item
              Control = framePkgSto
            end>
        end
      end
    end
    object jvspSMS: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'SMS_Sparse_Matrix_Solution_Pac'
      Caption = 'jvspSMS'
      inline framePkgSMS: TframePkgSms
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited pgcControls: TPageControl
          Width = 595
          Height = 359
          ExplicitWidth = 595
          ExplicitHeight = 359
          inherited tabBasic: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 587
            ExplicitHeight = 326
            inherited lblPrintOption: TLabel
              Width = 79
              Height = 18
              ExplicitWidth = 79
              ExplicitHeight = 18
            end
            inherited lblComplexity: TLabel
              Width = 77
              Height = 18
              ExplicitWidth = 77
              ExplicitHeight = 18
            end
            inherited lblSolutionGroupMaxIter: TLabel
              Width = 201
              Height = 18
              ExplicitWidth = 201
              ExplicitHeight = 18
            end
            inherited lblUsePTC: TLabel
              Width = 570
              Height = 18
              ExplicitWidth = 570
              ExplicitHeight = 18
            end
            inherited lblMaxErrors: TLabel
              Width = 519
              Height = 18
              ExplicitWidth = 519
              ExplicitHeight = 18
            end
            inherited lblMemoryPrint: TLabel
              Width = 383
              Height = 18
              ExplicitWidth = 383
              ExplicitHeight = 18
            end
            inherited comboPrintOption: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited comboComplexity: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited seSolutionGroupMaxIter: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboUsePTC: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitHeight = 28
            end
            inherited seMaxErrors: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboMemoryPrint: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
          end
          inherited tabNonLinear: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 303
            inherited rdgNonlinearOptions: TRbwDataGrid4
              Height = 303
              FixedCols = 0
              ExplicitHeight = 303
            end
          end
          inherited tabLinear: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 303
            inherited rdgLinearOptions: TRbwDataGrid4
              Height = 303
              FixedCols = 0
              ExplicitHeight = 303
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgSMS.lblComments
            end
            item
              Control = framePkgSMS.memoComments
            end
            item
              Control = framePkgSMS.comboPrintOption
            end
            item
              Control = framePkgSMS.comboComplexity
            end
            item
              Control = framePkgSMS.seSolutionGroupMaxIter
            end
            item
              Control = framePkgSMS.cbContinue
            end
            item
              Control = framePkgSMS.cbCsvOutput
            end
            item
              Control = framePkgSMS.rdgNonlinearOptions
            end
            item
              Control = framePkgSMS.rdgLinearOptions
            end
            item
              Control = framePkgSMS.lblUsePTC
            end
            item
              Control = framePkgSMS.comboUsePTC
            end
            item
              Control = framePkgSMS.lblMaxErrors
            end
            item
              Control = framePkgSMS.seMaxErrors
            end
            item
              Control = framePkgSMS.cbCheckInput
            end
            item
              Control = framePkgSMS.lblMemoryPrint
            end
            item
              Control = framePkgSMS.comboMemoryPrint
            end
            item
              Control = framePkgSMS.cbNewton
            end>
          OnEnabledChange = framePkgSMSrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspRIP: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'RIP_Riparian_Pkg'
      Caption = 'jvspRIP'
      inline framePkgRip: TframePackageRip
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          Height = 414
          TabOrder = 1
          ExplicitWidth = 564
          ExplicitHeight = 414
        end
        inherited cbWritePlantGroupFlows: TCheckBox
          Top = 482
          TabOrder = 0
          ExplicitTop = 482
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgRip.lblComments
            end
            item
              Control = framePkgRip.memoComments
            end
            item
              Control = framePkgRip.cbWritePlantGroupFlows
            end>
        end
      end
    end
    object jvspMt3dUZT: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'UZT_Unsaturated_Zone_Transport'
      Caption = 'jvspMt3dUZT'
      inline framePkgMt3dUZT: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          Height = 448
          Anchors = [akLeft, akTop, akRight, akBottom]
          ExplicitWidth = 564
          ExplicitHeight = 448
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgMt3dUZT.lblComments
            end
            item
              Control = framePkgMt3dUZT.memoComments
            end>
        end
      end
    end
    object jvspSfrMf6: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'SFR_MODFLOW_6_Streamflow_Rout2'
      Caption = 'jvspSfrMf6'
      inline framePackageSfrMF6: TframePackageSfrMF6
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        DesignSize = (
          595
          516)
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblMaxIterations: TLabel
          Width = 291
          Height = 36
          ExplicitWidth = 291
          ExplicitHeight = 36
        end
        inherited lblMaxDepthChange: TLabel
          Width = 401
          Height = 18
          WordWrap = False
          ExplicitWidth = 401
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited seMaxIterations: TJvSpinEdit
          Height = 26
          ExplicitHeight = 26
        end
        inherited rdeMaxDepthChange: TRbwDataEntry
          Height = 26
          Items.Strings = (
            'False'
            'True')
          ExplicitHeight = 26
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePackageSfrMF6.lblComments
            end
            item
              Control = framePackageSfrMF6.memoComments
            end
            item
              Control = framePackageSfrMF6.cbSaveBudget
            end
            item
              Control = framePackageSfrMF6.cbSaveStage
            end
            item
              Control = framePackageSfrMF6.rdeMaxDepthChange
            end
            item
              Control = framePackageSfrMF6.seMaxIterations
            end
            item
              Control = framePackageSfrMF6.cbPrintStage
            end
            item
              Control = framePackageSfrMF6.cbPrintFlows
            end>
          OnEnabledChange = framePackageSfrMF6rcSelectionControllerEnabledChange
        end
      end
    end
    object jvspMAW: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'MAW_MultiAquifer_Well_Package'
      Caption = 'jvspMAW'
      inline framePkgMAW: TframePackageMaw
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblShutDownTheta: TLabel
          Top = 459
          Width = 106
          Height = 18
          ExplicitTop = 457
          ExplicitWidth = 106
          ExplicitHeight = 18
        end
        inherited lblShutDownKappa: TLabel
          Top = 487
          Width = 116
          Height = 18
          ExplicitTop = 485
          ExplicitWidth = 116
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          Height = 293
          ExplicitWidth = 564
          ExplicitHeight = 293
        end
        inherited cbPrintHeads: TCheckBox
          Top = 364
          ExplicitTop = 364
        end
        inherited cbSaveHeads: TCheckBox
          Top = 387
          ExplicitTop = 387
        end
        inherited cbSaveFlows: TCheckBox
          Top = 410
          ExplicitTop = 410
        end
        inherited cbIncludeWellStorage: TCheckBox
          Top = 433
          Width = 553
          ExplicitTop = 433
          ExplicitWidth = 553
        end
        inherited rdeShutDownTheta: TRbwDataEntry
          Top = 456
          ExplicitTop = 456
        end
        inherited rdeShutDownKappa: TRbwDataEntry
          Top = 484
          ExplicitTop = 484
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgMAW.lblComments
            end
            item
              Control = framePkgMAW.memoComments
            end
            item
              Control = framePkgMAW.cbPrintHeads
            end
            item
              Control = framePkgMAW.cbSaveHeads
            end
            item
              Control = framePkgMAW.cbSaveFlows
            end
            item
              Control = framePkgMAW.cbIncludeWellStorage
            end
            item
              Control = framePkgMAW.rdeShutDownTheta
            end
            item
              Control = framePkgMAW.rdeShutDownKappa
            end>
        end
      end
    end
    object jvspGNC: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'GNC_Ghost_Node_Correction_Pack'
      Caption = 'jvspGNC'
      inline framePkgGNC: TframePackageGNC
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited rgFormulation: TRadioGroup
          Top = 157
          Width = 564
          ExplicitTop = 157
          ExplicitWidth = 564
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgGNC.lblComments
            end
            item
              Control = framePkgGNC.memoComments
            end
            item
              Control = framePkgGNC.rgFormulation
            end>
        end
      end
    end
    object jvspMf6Obs: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'OBS_Observation_Utility_Packag'
      Caption = 'jvspMf6Obs'
      inline framePackageMf6Obs: TframePackageMf6Obs
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblOutputFormat: TLabel
          Width = 93
          Height = 18
          ExplicitWidth = 93
          ExplicitHeight = 18
        end
        inherited lblNumberOfDigits: TLabel
          Width = 142
          Height = 18
          ExplicitWidth = 142
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited comboOutputFormat: TComboBox
          Height = 26
          ExplicitHeight = 26
        end
        inherited seNumberOfDigits: TJvSpinEdit
          Height = 26
          ExplicitHeight = 26
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePackageMf6Obs.lblComments
            end
            item
              Control = framePackageMf6Obs.memoComments
            end
            item
              Control = framePackageMf6Obs.comboOutputFormat
            end>
        end
      end
    end
    object jvspLakMf6: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'LAK_Lake_package_for_MODFLOW_6'
      Caption = 'jvspLakMf6'
      inline framePackageLakMf6: TframePackageLakMf6
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblSurfaceDepressionDepth: TLabel
          Width = 245
          Height = 18
          ExplicitWidth = 245
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePackageLakMf6.lblComments
            end
            item
              Control = framePackageLakMf6.memoComments
            end
            item
              Control = framePackageLakMf6.cbPrintStage
            end
            item
              Control = framePackageLakMf6.cbSaveStage
            end
            item
              Control = framePackageLakMf6.cbSaveBudget
            end
            item
              Control = framePackageLakMf6.rdeSurfaceDepressionDepth
            end>
          OnEnabledChange = framePackageLakMf6rcSelectionControllerEnabledChange
        end
      end
    end
    object jvspMVR: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'MVR_Water_Mover_Package_Pane2'
      Caption = 'jvspMVR'
      inline framePkgMVR: TframePackageMvr
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          Height = 403
          ExplicitWidth = 564
          ExplicitHeight = 403
        end
        inherited cbSaveBudget: TCheckBox
          Top = 480
          Height = 21
          ExplicitTop = 480
          ExplicitHeight = 21
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgMVR.lblComments
            end
            item
              Control = framePkgMVR.memoComments
            end
            item
              Control = framePkgMVR.cbSaveBudget
            end>
        end
      end
    end
    object jvspUzfMf6: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'UZF6_Unsaturated_Zone_Flow_Pa2'
      Caption = 'jvspUzfMf6'
      inline framePackageUzfMf6: TframePackageUzfMf6
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblNumberOfTrailingWaves: TLabel
          Width = 261
          Height = 18
          ExplicitWidth = 261
          ExplicitHeight = 18
        end
        inherited lblNumberOfWaveSets: TLabel
          Width = 232
          Height = 18
          ExplicitWidth = 232
          ExplicitHeight = 18
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePackageUzfMf6.lblComments
            end
            item
              Control = framePackageUzfMf6.memoComments
            end
            item
              Control = framePackageUzfMf6.rgEvapotranspiration
            end
            item
              Control = framePackageUzfMf6.cbSeepage
            end
            item
              Control = framePackageUzfMf6.cbSaveBudget
            end
            item
              Control = framePackageUzfMf6.lblNumberOfTrailingWaves
            end
            item
              Control = framePackageUzfMf6.rdeNumberOfTrailingWaves
            end
            item
              Control = framePackageUzfMf6.lblNumberOfWaveSets
            end
            item
              Control = framePackageUzfMf6.rdeNumberOfWaveSets
            end>
          OnEnabledChange = framePackageUzfMf6rcSelectionControllerEnabledChange
        end
      end
    end
    object jvspMt3dLkt: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'LKT_Lake_Transport_Package_Pan'
      Caption = 'jvspMt3dLkt'
      inline frameMt3dLktPkg: TframeMt3dLktPkg
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited cbPrintLakeBudget: TCheckBox
          Width = 553
          ExplicitWidth = 553
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = frameMt3dLktPkg.lblComments
            end
            item
              Control = frameMt3dLktPkg.memoComments
            end
            item
              Control = frameMt3dLktPkg.cbSoluteEvap
            end
            item
              Control = frameMt3dLktPkg.cbPrintLakeBudget
            end>
        end
      end
    end
    object jvspMt3dSft: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'SFT_Stream_Flow_Transport_Pack'
      Caption = 'jvspMt3dSft'
      inline frameMt3dSftPkg: TframeMt3dSftPkg
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblTimeWeightingFactor: TLabel
          Width = 299
          Height = 18
          ExplicitWidth = 299
          ExplicitHeight = 18
        end
        inherited lblSpaceWeightingFactor: TLabel
          Width = 318
          Height = 18
          ExplicitWidth = 318
          ExplicitHeight = 18
        end
        inherited lblClosureCriterion: TLabel
          Width = 216
          Height = 18
          ExplicitWidth = 216
          ExplicitHeight = 18
        end
        inherited lblMaxIterations: TLabel
          Top = 298
          Width = 301
          Height = 18
          ExplicitTop = 298
          ExplicitWidth = 301
          ExplicitHeight = 18
        end
        inherited lblSolverPrintChoice: TLabel
          Width = 221
          Height = 18
          ExplicitWidth = 221
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited seMaxIterations: TJvSpinEdit
          Height = 26
          ExplicitHeight = 26
        end
        inherited comboPrintChoice: TJvImageComboBox
          Height = 28
          ItemHeight = 22
          ExplicitHeight = 28
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = frameMt3dSftPkg.lblComments
            end
            item
              Control = frameMt3dSftPkg.memoComments
            end
            item
              Control = frameMt3dSftPkg.cbEvaporateMass
            end
            item
              Control = frameMt3dSftPkg.rdeTimeWeightingFactor
            end
            item
              Control = frameMt3dSftPkg.rdeSpaceWeightingFactor
            end
            item
              Control = frameMt3dSftPkg.rdeClosureCriterion
            end
            item
              Control = frameMt3dSftPkg.seMaxIterations
            end
            item
              Control = frameMt3dSftPkg.comboPrintChoice
            end
            item
              Control = frameMt3dSftPkg.cbSimulateStreamTransport
            end>
        end
      end
    end
    object jvspMt3dCts: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'CTS_Contaminant_Treatment_Syst'
      Caption = 'jvspMt3dCts'
      inline frameMt3dCtsPkg: TframeMt3dCtsPkg
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblForce: TLabel
          Width = 448
          Height = 18
          ExplicitWidth = 448
          ExplicitHeight = 18
        end
        inherited lblWellPackageChoice: TLabel
          Width = 228
          Height = 18
          ExplicitWidth = 228
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited comboForce: TComboBox
          Width = 565
          Height = 26
          ExplicitWidth = 565
          ExplicitHeight = 26
        end
        inherited comboWellPackageChoice: TComboBox
          Width = 565
          Height = 26
          ExplicitWidth = 565
          ExplicitHeight = 26
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = frameMt3dCtsPkg.lblComments
            end
            item
              Control = frameMt3dCtsPkg.memoComments
            end
            item
              Control = frameMt3dCtsPkg.comboForce
            end
            item
              Control = frameMt3dCtsPkg.comboWellPackageChoice
            end>
        end
      end
    end
    object jvspCSUB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 516
      HelpType = htKeyword
      HelpKeyword = 'CSUB_Skeletal_Storage__Compact'
      Caption = 'jvspCSUB'
      inline framePackageCsub: TframePackageCsub
        Left = 0
        Top = 0
        Width = 595
        Height = 516
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 516
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited pcCsub: TPageControl
          Top = 159
          Width = 595
          ExplicitTop = -197
          ExplicitWidth = 117
          inherited tabInterbeds: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 581
            ExplicitHeight = 324
            inherited frameInterbeds: TframeGrid
              Height = 324
              ExplicitHeight = 324
              inherited Panel: TPanel
                Top = 283
                ExplicitTop = 283
                inherited lbNumber: TLabel
                  Width = 194
                  Height = 18
                  ExplicitWidth = 194
                  ExplicitHeight = 18
                end
                inherited sbAdd: TSpeedButton
                  ExplicitLeft = 338
                end
                inherited sbInsert: TSpeedButton
                  ExplicitLeft = 367
                end
                inherited sbDelete: TSpeedButton
                  ExplicitLeft = 396
                end
                inherited seNumber: TJvSpinEdit
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited Grid: TRbwDataGrid4
                Height = 283
                ExplicitHeight = 283
              end
            end
          end
          inherited tabOptions: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 109
            ExplicitHeight = 324
            inherited lblGamma: TLabel
              Width = 213
              Height = 18
              ExplicitWidth = 213
              ExplicitHeight = 18
            end
            inherited lblBeta: TLabel
              Width = 212
              Height = 18
              ExplicitWidth = 212
              ExplicitHeight = 18
            end
            inherited lblInterbedThicknessMethod: TLabel
              Width = 446
              Height = 18
              ExplicitWidth = 446
              ExplicitHeight = 18
            end
            inherited lblseNDelayCells: TLabel
              Width = 483
              Height = 18
              ExplicitWidth = 483
              ExplicitHeight = 18
            end
            inherited lblCompressionMethod: TLabel
              Width = 354
              Height = 18
              ExplicitWidth = 354
              ExplicitHeight = 18
            end
            inherited seNDelayCells: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboInterbedThicknessMethod: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitHeight = 28
            end
            inherited comboCompressionMethod: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ItemIndex = -1
              ExplicitHeight = 28
            end
          end
          inherited tabOutputTypes: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 581
            ExplicitHeight = 324
            inherited chklstOutput: TCheckListBox
              Height = 324
              ItemHeight = 18
              ExplicitHeight = 324
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePackageCsub.lblComments
            end
            item
              Control = framePackageCsub.memoComments
            end
            item
              Control = framePackageCsub.rdeGamma
            end
            item
              Control = framePackageCsub.rdeBeta
            end
            item
              Control = framePackageCsub.cbHeadBased
            end
            item
              Control = framePackageCsub.cbPreconsolidationHeadUsed
            end
            item
              Control = framePackageCsub.seNDelayCells
            end
            item
              Control = framePackageCsub.comboCompressionMethod
            end
            item
              Control = framePackageCsub.cbUpdateMaterialProperties
            end
            item
              Control = framePackageCsub.comboInterbedThicknessMethod
            end
            item
              Control = framePackageCsub.cbSpecifyInitialPreconsolidationStress
            end
            item
              Control = framePackageCsub.cbSpecifyInitialDelayHead
            end
            item
              Control = framePackageCsub.cbSpecifyInitialDelayHead
            end
            item
              Control = framePackageCsub.cbEffectiveStressLag
            end
            item
              Control = framePackageCsub.chklstOutput
            end
            item
              Control = framePackageCsub.frameInterbeds
            end>
        end
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 516
    Width = 782
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      782
      41)
    object btnHelp: TBitBtn
      Left = 441
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
      Left = 555
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
      Left = 669
      Top = 6
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 177
    Height = 516
    Align = alLeft
    TabOrder = 0
    object pnlModel: TPanel
      Left = 1
      Top = 1
      Width = 175
      Height = 64
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object lblModel: TLabel
        Left = 8
        Top = 11
        Width = 43
        Height = 18
        Caption = 'Model'
      end
      object comboModel: TComboBox
        Left = 8
        Top = 32
        Width = 162
        Height = 26
        Style = csDropDownList
        TabOrder = 0
        OnChange = comboModelChange
      end
    end
    object tvPackages: TTreeView
      Left = 1
      Top = 65
      Width = 175
      Height = 450
      Align = alClient
      HideSelection = False
      Indent = 20
      ReadOnly = True
      StateImages = ilCheckImages
      TabOrder = 1
      OnChange = tvPackagesChange
      OnCustomDrawItem = tvPackagesCustomDrawItem
      OnExpanded = tvPackagesExpanded
      OnMouseUp = tvPackagesMouseUp
    end
  end
  object rbwLpfParamCountController: TRbwController
    ControlList = <
      item
        Control = frameLpfParameterDefinition.btnDelete
      end
      item
        Control = frameLpfParameterDefinition.dgParameters
      end
      item
        Control = frameLpfParameterDefinition.lblNumParameters
      end
      item
        Control = frameLpfParameterDefinition.seNumberOfParameters
      end>
    Left = 72
    Top = 216
  end
  object ilCheckImages: TImageList
    Left = 136
    Top = 96
    Bitmap = {
      494C010108000D00040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C0C0C000C0C0C000C0C0C000C0C0C000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C0C0C000C0C0C000C0C0C000C0C0C000000000000000
      00000000000000000000000000000000000000000000C0C0C000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C0C0C000C0C0C000C0C0C000C0C0C000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000C0C0C000C0C0C00000000000000000000000000000000000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000008080
      8000C0C0C000C0C0C00000000000000000000000000000000000C0C0C000C0C0
      C0000000000000000000000000000000000000000000C0C0C00080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000008080
      8000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000008080
      800000000000000000000000000000000000000000000000000000000000C0C0
      C000000000000000000000000000000000000000000000000000000000008080
      800000000000000000000000000000000000000000000000000000000000C0C0
      C0000000000000000000000000000000000000000000C0C0C00080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000008080
      800000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C00000000000000000000000000000000000C0C0C00080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000808080000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C00000000000000000000000000000000000C0C0C00080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000808080000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C00000000000000000000000000000000000C0C0C00080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000808080000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C00000000000000000000000000000000000C0C0C00080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000808080000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0000000000000000000000000000000000000000000000000008080
      800000000000000000000000000000000000000000000000000000000000C0C0
      C000000000000000000000000000000000000000000000000000000000008080
      800000000000000000000000000000000000000000000000000000000000C0C0
      C0000000000000000000000000000000000000000000C0C0C00080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000008080
      800000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C00080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000008080
      80000000000000000000C0C0C000C0C0C000C0C0C000C0C0C000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080008080800000000000000000000000000000000000808080008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000808080008080800000000000000000000000000000000000808080008080
      80000000000000000000000000000000000000000000C0C0C00080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      0000808080008080800000000000000000000000000000000000808080008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000808080008080800080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000808080008080800080808000000000000000
      00000000000000000000000000000000000000000000C0C0C000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000808080008080800080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099A8AC00E2EFF100E2EF
      F100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EF
      F100E2EFF1000000000000000000000000000000000099A8AC00E2EFF100E2EF
      F100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EF
      F100E2EFF1000000000000000000000000000000000099A8AC00E2EFF100E2EF
      F100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EF
      F100E2EFF1000000000000000000000000000000000099A8AC00E2EFF100E2EF
      F100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EF
      F100E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000D8E9EC0000000000D8E9EC0000000000D8E9EC0000000000D8E9EC000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F7100D8E9
      EC0000000000D8E9EC0099A8AC00D8E9EC0000000000D8E9EC0000000000D8E9
      EC00E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000D8E9EC0099A8AC0099A8AC0099A8AC00D8E9EC0000000000D8E9EC000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F7100D8E9
      EC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC00D8E9EC0000000000D8E9
      EC00E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      000099A8AC0099A8AC00D8E9EC0099A8AC0099A8AC0099A8AC00D8E9EC000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F7100D8E9
      EC0099A8AC00D8E9EC0000000000D8E9EC0099A8AC0099A8AC0099A8AC00D8E9
      EC00E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000D8E9EC0000000000D8E9EC0000000000D8E9EC0099A8AC0099A8AC000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F7100D8E9
      EC0000000000D8E9EC0000000000D8E9EC0000000000D8E9EC0099A8AC00D8E9
      EC00E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000D8E9EC0000000000D8E9EC0000000000D8E9EC0000000000D8E9EC000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F7100646F
      7100646F7100646F7100646F7100646F7100646F7100646F7100646F7100646F
      7100E2EFF1000000000000000000000000000000000099A8AC00646F7100646F
      7100646F7100646F7100646F7100646F7100646F7100646F7100646F7100646F
      7100E2EFF1000000000000000000000000000000000099A8AC00646F7100646F
      7100646F7100646F7100646F7100646F7100646F7100646F7100646F7100646F
      7100E2EFF1000000000000000000000000000000000099A8AC00646F7100646F
      7100646F7100646F7100646F7100646F7100646F7100646F7100646F7100646F
      7100E2EFF1000000000000000000000000000000000099A8AC0099A8AC0099A8
      AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8
      AC0099A8AC000000000000000000000000000000000099A8AC0099A8AC0099A8
      AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8
      AC0099A8AC000000000000000000000000000000000099A8AC0099A8AC0099A8
      AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8
      AC0099A8AC000000000000000000000000000000000099A8AC0099A8AC0099A8
      AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8
      AC0099A8AC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFC3FFC3FBFFFFC3FE3CFE3CF800FE00FE7EFE7EF800FE00F
      CFF7CE77800FC007CFF7CC37800FC007CFF7CC37800FC007CFF7CE77800FC007
      E7EFE7EF800FE00FE3CFE3CF800FE00FF00FF00F800FF00FFC3FFC3F800FFC3F
      FFFFFFFF8007FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFF80078007800780079FF79FF79FF795579FF79FF79DF788A7
      9FF79FF798F790579FF79FF7907780279FF79FF7923790179FF79FF797178207
      9FF79FF79F9795179FF79FF79FD78A879FF79FF79FF795578007800780078007
      8007800780078007FFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object rbwHufParamCountController: TRbwController
    ControlList = <
      item
        Control = frameHufParameterDefinition.btnDelete
      end
      item
        Control = frameHufParameterDefinition.dgParameters
      end
      item
        Control = frameHfbParameterDefinition.lblNumParameters
      end
      item
        Control = frameHfbParameterDefinition.seNumberOfParameters
      end>
    Left = 48
    Top = 256
  end
end
