inherited frmFarm: TfrmFarm
  HelpType = htKeyword
  HelpKeyword = 'Farms_Dialog_Box'
  Caption = 'Water Balance Subregions (formerly Farms)'
  ClientHeight = 462
  ClientWidth = 788
  ExplicitWidth = 800
  ExplicitHeight = 500
  TextHeight = 18
  object Splitter1: TSplitter
    Left = 145
    Top = 0
    Width = 5
    Height = 421
    ExplicitLeft = 185
  end
  inline frameFarm: TframeFarm
    Left = 150
    Top = 0
    Width = 638
    Height = 421
    Align = alClient
    Enabled = False
    TabOrder = 1
    ExplicitLeft = 150
    ExplicitWidth = 638
    ExplicitHeight = 421
    inherited pcMain: TJvgPageControl
      Width = 638
      Height = 421
      ActivePage = frameFarm.tabGW_Allocation
      Font.Pitch = fpVariable
      OnChange = frameFarmpcMainChange
      ExplicitWidth = 638
      ExplicitHeight = 421
      inherited tabName: TTabSheet
        ExplicitWidth = 634
        ExplicitHeight = 361
        inherited pnlTop: TPanel
          Width = 634
          ExplicitWidth = 634
          inherited seFarmId: TJvSpinEdit
            OnChange = frameFarmseFarmIdChange
          end
          inherited pnlCaption: TPanel
            Width = 634
            ExplicitWidth = 634
          end
          inherited edFarmName: TLabeledEdit
            OnChange = frameFarmedFarmNameChange
          end
        end
        inherited PanelOwhm2: TPanel
          Width = 634
          Height = 296
          ExplicitWidth = 634
          ExplicitHeight = 296
          inherited lblPumpSpread: TLabel
            Width = 235
            Height = 48
            ExplicitWidth = 235
            ExplicitHeight = 48
          end
        end
      end
      inherited tabCrops: TTabSheet
        ExplicitWidth = 630
        ExplicitHeight = 361
        inherited frameFormulaGridCrops: TframeFormulaGrid
          Width = 630
          Height = 361
          ExplicitWidth = 626
          ExplicitHeight = 360
          inherited Panel: TPanel
            Top = 320
            Width = 630
            ExplicitTop = 319
            ExplicitWidth = 626
            inherited sbAdd: TSpeedButton
              Left = 362
              ExplicitLeft = 260
            end
            inherited sbInsert: TSpeedButton
              Left = 429
              ExplicitLeft = 343
            end
            inherited sbDelete: TSpeedButton
              Left = 495
              ExplicitLeft = 397
            end
          end
          inherited Grid: TRbwDataGrid4
            Width = 630
            Height = 263
            OnSetEditText = frameFormulaGridCropsGridSetEditText
            OnButtonClick = frameFormulaGridCropsGridButtonClick
            ExplicitWidth = 626
            ExplicitHeight = 262
          end
          inherited pnlTop: TPanel
            Width = 630
            ExplicitWidth = 626
            inherited edFormula: TLabeledEdit
              OnChange = frameFormulaGridCropsedFormulaChange
            end
          end
        end
      end
      inherited tabEfficiencyImprovement: TTabSheet
        inherited frameFormulaGridEfficiencyImprovement: TframeFormulaGrid
          inherited Panel: TPanel
            inherited sbAdd: TSpeedButton
              Left = 314
              ExplicitLeft = 314
            end
            inherited sbInsert: TSpeedButton
              Left = 372
              ExplicitLeft = 372
            end
            inherited sbDelete: TSpeedButton
              Left = 431
              ExplicitLeft = 431
            end
          end
        end
      end
      inherited tabAddedDemandRunoffSplit: TTabSheet
        inherited frameAddedDemandRunoffSplit: TframeFormulaGrid
          inherited Panel: TPanel
            inherited sbAdd: TSpeedButton
              Left = 293
              ExplicitLeft = 295
            end
            inherited sbInsert: TSpeedButton
              Left = 348
              ExplicitLeft = 350
            end
            inherited sbDelete: TSpeedButton
              Left = 402
              ExplicitLeft = 405
            end
          end
          inherited Grid: TRbwDataGrid4
            ColCount = 3
            Columns = <
              item
                AutoAdjustRowHeights = False
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
          end
        end
      end
      inherited tabDeficiencyScenario: TTabSheet
        inherited frameDeficiencyScenario: TframeFormulaGrid
          inherited Panel: TPanel
            inherited sbAdd: TSpeedButton
              Left = 299
              ExplicitLeft = 299
            end
            inherited sbInsert: TSpeedButton
              Left = 355
              ExplicitLeft = 355
            end
            inherited sbDelete: TSpeedButton
              Left = 410
              ExplicitLeft = 410
            end
          end
        end
      end
      inherited tabBareRunoffFractions: TTabSheet
        inherited frameBareRunoffFractions: TframeFormulaGrid
          inherited Panel: TPanel
            inherited sbAdd: TSpeedButton
              Left = 314
              ExplicitLeft = 314
            end
            inherited sbInsert: TSpeedButton
              Left = 372
              ExplicitLeft = 372
            end
            inherited sbDelete: TSpeedButton
              Left = 431
              ExplicitLeft = 431
            end
          end
        end
      end
      inherited tabAddedCropDemandRate: TTabSheet
        inherited frameAddedCropDemandRate: TframeFormulaGrid
          inherited Panel: TPanel
            inherited sbAdd: TSpeedButton
              Left = 299
              ExplicitLeft = 299
            end
            inherited sbInsert: TSpeedButton
              Left = 355
              ExplicitLeft = 355
            end
            inherited sbDelete: TSpeedButton
              Left = 410
              ExplicitLeft = 410
            end
          end
        end
      end
      inherited tabCosts: TTabSheet
        inherited frameFormulaGridCosts: TframeFormulaGrid
          inherited Panel: TPanel
            inherited sbAdd: TSpeedButton
              Left = 249
              ExplicitLeft = 249
            end
            inherited sbInsert: TSpeedButton
              Left = 295
              ExplicitLeft = 295
            end
            inherited sbDelete: TSpeedButton
              Left = 341
              ExplicitLeft = 341
            end
          end
          inherited Grid: TRbwDataGrid4
            OnSetEditText = frameFormulaGridCostsGridSetEditText
          end
        end
      end
      inherited tabDiversionLocation: TTabSheet
        inherited frameFormulaGridDiversion: TframeFarmDiversion
          inherited Panel: TPanel
            inherited sbAdd: TSpeedButton
              Left = 518
              ExplicitLeft = 477
            end
            inherited sbInsert: TSpeedButton
              Left = 550
              ExplicitLeft = 506
            end
            inherited sbDelete: TSpeedButton
              Left = 583
              ExplicitLeft = 536
            end
          end
          inherited Grid: TRbwDataGrid4
            OnSetEditText = frameFormulaGridDiversionGridSetEditText
          end
          inherited pnlTop: TPanel
            inherited rdeCol: TRbwDataEntry
              Height = 28
              ExplicitHeight = 28
            end
            inherited rdeVertexNumber: TRbwDataEntry
              Height = 28
              ExplicitHeight = 28
            end
            inherited rdeX: TRbwDataEntry
              Height = 28
              ExplicitHeight = 28
            end
            inherited rdeY: TRbwDataEntry
              Height = 28
              ExplicitHeight = 28
            end
            inherited rdeRow: TRbwDataEntry
              Height = 28
              ExplicitHeight = 28
            end
          end
        end
      end
      inherited tabReturnFlowLocation: TTabSheet
        inherited frameFormulaGridReturnFlow: TframeFarmDiversion
          inherited Panel: TPanel
            inherited sbAdd: TSpeedButton
              Left = 473
              ExplicitLeft = 471
            end
            inherited sbInsert: TSpeedButton
              Left = 503
              ExplicitLeft = 499
            end
            inherited sbDelete: TSpeedButton
              Left = 533
              ExplicitLeft = 524
            end
          end
          inherited Grid: TRbwDataGrid4
            OnSetEditText = frameFormulaGridReturnFlowGridSetEditText
          end
          inherited pnlTop: TPanel
            inherited rdeCol: TRbwDataEntry
              Height = 28
              ExplicitHeight = 28
            end
            inherited rdeVertexNumber: TRbwDataEntry
              Height = 28
              ExplicitHeight = 28
            end
            inherited rdeX: TRbwDataEntry
              Height = 28
              ExplicitHeight = 28
            end
            inherited rdeY: TRbwDataEntry
              Height = 28
              ExplicitHeight = 28
            end
            inherited rdeRow: TRbwDataEntry
              Height = 28
              ExplicitHeight = 28
            end
          end
        end
      end
      inherited tabNonRoutedDelivery: TTabSheet
        inherited frameDelivery: TframeDeliveryGrid
          inherited Panel: TPanel
            inherited sbAdd: TSpeedButton
              Left = 399
              Top = 9
              ExplicitLeft = 391
              ExplicitTop = 9
            end
            inherited sbInsert: TSpeedButton
              Left = 427
              Top = 9
              ExplicitLeft = 420
              ExplicitTop = 9
            end
            inherited sbDelete: TSpeedButton
              Left = 452
              Top = 9
              ExplicitLeft = 443
              ExplicitTop = 9
            end
            inherited lblNumberOfDeliveryTypes: TLabel
              Width = 82
              Height = 32
              ExplicitWidth = 82
              ExplicitHeight = 32
            end
          end
          inherited Grid: TRbwDataGrid4
            OnSetEditText = frameDeliveryGridSetEditText
          end
        end
      end
      inherited tabWaterRights: TTabSheet
        inherited frameFormulaGridWaterRights: TframeFormulaGrid
          inherited Panel: TPanel
            inherited sbAdd: TSpeedButton
              Left = 267
              ExplicitLeft = 256
            end
            inherited sbInsert: TSpeedButton
              Left = 316
              ExplicitLeft = 303
            end
            inherited sbDelete: TSpeedButton
              Left = 366
              ExplicitLeft = 351
            end
          end
          inherited Grid: TRbwDataGrid4
            OnSetEditText = frameFormulaGridWaterRightsGridSetEditText
          end
        end
      end
      inherited tabGW_Allocation: TTabSheet
        ExplicitWidth = 630
        ExplicitHeight = 361
        inherited frameGW_Allocation: TframeFormulaGrid
          Width = 630
          Height = 361
          inherited Panel: TPanel
            Top = 319
            Width = 630
            inherited sbAdd: TSpeedButton
              Left = 323
              ExplicitLeft = 295
            end
            inherited sbInsert: TSpeedButton
              Left = 383
              ExplicitLeft = 350
            end
            inherited sbDelete: TSpeedButton
              Left = 444
              ExplicitLeft = 405
            end
          end
          inherited Grid: TRbwDataGrid4
            Width = 630
            Height = 262
            OnSetEditText = frameGW_AllocationGridSetEditText
          end
          inherited pnlTop: TPanel
            Width = 630
          end
        end
      end
      inherited tabNoReturnFlow: TTabSheet
        Caption = 'No Return Flow Choice'
        inherited frameNoReturnFlow: TframeFormulaGrid
          inherited Panel: TPanel
            inherited sbAdd: TSpeedButton
              Left = 339
              ExplicitLeft = 314
            end
            inherited sbInsert: TSpeedButton
              Left = 403
              ExplicitLeft = 372
            end
            inherited sbDelete: TSpeedButton
              Left = 464
              ExplicitLeft = 430
            end
          end
        end
      end
      inherited tabDiversionsOwhm2: TTabSheet
        inherited frameDiversionsOwhm2: TframeMultSemiRouted
          inherited Splitter1: TSplitter
            ExplicitHeight = 320
          end
          inherited pnlBottom: TPanel
            inherited lbNumber: TLabel
              Width = 169
              Caption = 'Number of diversion locations'
              ExplicitWidth = 169
            end
            inherited sbAdd: TSpeedButton
              Left = 522
              ExplicitLeft = 477
            end
            inherited sbDelete: TSpeedButton
              Left = 585
              ExplicitLeft = 535
            end
          end
          inherited Panel1: TPanel
            inherited frameFarmDiversions: TframeFarmDiversion
              inherited Panel: TPanel
                inherited lbNumber: TLabel
                  Width = 95
                  Caption = 'Number of times'
                  ExplicitWidth = 95
                end
                inherited sbAdd: TSpeedButton
                  Left = 400
                  ExplicitLeft = 357
                end
                inherited sbInsert: TSpeedButton
                  Left = 423
                  ExplicitLeft = 377
                end
                inherited sbDelete: TSpeedButton
                  Left = 446
                  ExplicitLeft = 398
                end
              end
              inherited Grid: TRbwDataGrid4
                ColWidths = (
                  64
                  64
                  64
                  64
                  64)
              end
            end
          end
          inherited Controller: TRbwController
            ControlList = <
              item
                Control = frameFarm.frameDiversionsOwhm2.seNumber
              end
              item
                Control = frameFarm.frameDiversionsOwhm2.sbAdd
              end>
          end
        end
      end
      inherited tabReturnFlowOwhm2: TTabSheet
        inherited frameReturnFlowsOwhm2: TframeMultSemiRouted
          inherited Splitter1: TSplitter
            ExplicitHeight = 320
          end
          inherited pnlBottom: TPanel
            inherited lbNumber: TLabel
              Width = 181
              Caption = 'Number of return flow locations'
              ExplicitWidth = 181
            end
            inherited sbAdd: TSpeedButton
              Left = 555
              ExplicitLeft = 473
            end
            inherited sbDelete: TSpeedButton
              Left = 628
              ExplicitLeft = 535
            end
          end
          inherited Panel1: TPanel
            inherited frameFarmDiversions: TframeFarmDiversion
              inherited Panel: TPanel
                inherited lbNumber: TLabel
                  Width = 95
                  Caption = 'Number of times'
                  ExplicitWidth = 95
                end
                inherited sbAdd: TSpeedButton
                  Left = 338
                  ExplicitLeft = 275
                end
                inherited sbInsert: TSpeedButton
                  Left = 358
                  ExplicitLeft = 292
                end
                inherited sbDelete: TSpeedButton
                  Left = 378
                  ExplicitLeft = 308
                end
              end
              inherited Grid: TRbwDataGrid4
                ColWidths = (
                  64
                  64
                  64
                  64
                  64)
              end
            end
          end
          inherited Controller: TRbwController
            ControlList = <
              item
                Control = frameFarm.frameReturnFlowsOwhm2.seNumber
              end
              item
                Control = frameFarm.frameReturnFlowsOwhm2.sbAdd
              end>
          end
        end
      end
    end
  end
  object pnlFarms: TPanel
    Left = 0
    Top = 0
    Width = 145
    Height = 421
    Align = alLeft
    TabOrder = 0
    ExplicitHeight = 420
    object vstFarms: TVirtualStringTree
      Left = 1
      Top = 1
      Width = 143
      Height = 335
      Align = alClient
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
      Colors.UnfocusedSelectionColor = clWhite
      Colors.UnfocusedSelectionBorderColor = clWhite
      Header.AutoSizeIndex = 0
      Header.MainColumn = -1
      TabOrder = 0
      TreeOptions.SelectionOptions = [toMultiSelect]
      OnAddToSelection = vstFarmsAddToSelection
      OnGetText = vstFarmsGetText
      OnGetNodeDataSize = vstFarmsGetNodeDataSize
      OnInitNode = vstFarmsInitNode
      OnRemoveFromSelection = vstFarmsRemoveFromSelection
      Touch.InteractiveGestures = [igPan, igPressAndTap]
      Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
      ExplicitHeight = 334
      Columns = <>
    end
    object pnlControls: TPanel
      Left = 1
      Top = 336
      Width = 143
      Height = 84
      Align = alBottom
      TabOrder = 1
      ExplicitTop = 335
      object sbAddUnit: TSpeedButton
        Left = 11
        Top = 6
        Width = 23
        Height = 22
        Hint = 'Add farn|Add a farm below the bottom farm.'
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
      end
      object sbInsertUnit: TSpeedButton
        Left = 40
        Top = 6
        Width = 23
        Height = 22
        Hint = 'Insert farm|Insert a farm above the selected farm.'
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
        OnClick = sbInsertUnitClick
      end
      object sbDeleteUnit: TSpeedButton
        Left = 69
        Top = 6
        Width = 23
        Height = 22
        Hint = 'Delete farm|Delete the selected farm.'
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
      end
      object lblFarmCount: TLabel
        Left = 11
        Top = 34
        Width = 121
        Height = 18
        Caption = 'Number of WBSs'
      end
      object seFarms: TJvSpinEdit
        Left = 11
        Top = 53
        Width = 115
        Height = 26
        CheckMinValue = True
        TabOrder = 0
        OnChange = seFarmsChange
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 421
    Width = 788
    Height = 41
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 420
    ExplicitWidth = 784
    DesignSize = (
      788
      41)
    object btnHelp: TBitBtn
      Left = 369
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 365
    end
    object btnOK: TBitBtn
      Left = 483
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnOKClick
      ExplicitLeft = 479
    end
    object btnCancel: TBitBtn
      Left = 597
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 1
      ExplicitLeft = 593
    end
  end
end
