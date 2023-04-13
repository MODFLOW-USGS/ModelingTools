inherited frmFarm: TfrmFarm
  HelpType = htKeyword
  HelpKeyword = 'Farms_Dialog_Box'
  Caption = 'Farms'
  ClientHeight = 462
  ClientWidth = 759
  ExplicitWidth = 771
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
    Width = 609
    Height = 421
    Align = alClient
    Enabled = False
    TabOrder = 1
    ExplicitLeft = 150
    ExplicitWidth = 609
    ExplicitHeight = 421
    inherited pcMain: TJvgPageControl
      Width = 609
      Height = 421
      ActivePage = frameFarm.tabNonRoutedDelivery
      Font.Pitch = fpVariable
      OnChange = frameFarmpcMainChange
      ExplicitWidth = 609
      ExplicitHeight = 421
      inherited tabCrops: TTabSheet
        inherited frameFormulaGridCrops: TframeFormulaGrid
          inherited Panel: TPanel
            ExplicitTop = 255
            ExplicitWidth = 601
            inherited sbAdd: TSpeedButton
              Left = 302
              ExplicitLeft = 260
            end
            inherited sbInsert: TSpeedButton
              Left = 358
              ExplicitLeft = 343
            end
            inherited sbDelete: TSpeedButton
              Left = 414
              ExplicitLeft = 397
            end
          end
          inherited Grid: TRbwDataGrid4
            OnSetEditText = frameFormulaGridCropsGridSetEditText
            OnButtonClick = frameFormulaGridCropsGridButtonClick
          end
          inherited pnlTop: TPanel
            inherited edFormula: TLabeledEdit
              OnChange = frameFormulaGridCropsedFormulaChange
            end
          end
        end
        inherited pnlTop: TPanel
          ExplicitWidth = 601
          inherited seFarmId: TJvSpinEdit
            OnChange = frameFarmseFarmIdChange
          end
          inherited pnlCaption: TPanel
            ExplicitWidth = 601
          end
          inherited edFarmName: TLabeledEdit
            OnChange = frameFarmedFarmNameChange
          end
        end
      end
      inherited tabEfficiencyImprovement: TTabSheet
        inherited frameFormulaGridEfficiencyImprovement: TframeFormulaGrid
          inherited Panel: TPanel
            ExplicitTop = 320
            ExplicitWidth = 601
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
            ExplicitTop = 320
            ExplicitWidth = 601
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
              Left = 497
              ExplicitLeft = 477
            end
            inherited sbInsert: TSpeedButton
              Left = 528
              ExplicitLeft = 506
            end
            inherited sbDelete: TSpeedButton
              Left = 559
              ExplicitLeft = 536
            end
          end
          inherited Grid: TRbwDataGrid4
            OnSetEditText = frameFormulaGridDiversionGridSetEditText
          end
        end
      end
      inherited tabReturnFlowLocation: TTabSheet
        inherited frameFormulaGridReturnFlow: TframeFarmDiversion
          inherited Panel: TPanel
            inherited sbAdd: TSpeedButton
              Left = 454
              ExplicitLeft = 471
            end
            inherited sbInsert: TSpeedButton
              Left = 482
              ExplicitLeft = 499
            end
            inherited sbDelete: TSpeedButton
              Left = 511
              ExplicitLeft = 524
            end
          end
          inherited Grid: TRbwDataGrid4
            OnSetEditText = frameFormulaGridReturnFlowGridSetEditText
          end
        end
      end
      inherited tabNonRoutedDelivery: TTabSheet
        ExplicitWidth = 601
        ExplicitHeight = 361
        inherited frameDelivery: TframeDeliveryGrid
          Width = 601
          Height = 361
          ExplicitWidth = 595
          ExplicitHeight = 352
          inherited Panel: TPanel
            Top = 311
            Width = 601
            ExplicitTop = 302
            ExplicitWidth = 595
            inherited sbAdd: TSpeedButton
              Left = 404
              Top = 9
              ExplicitLeft = 391
              ExplicitTop = 9
            end
            inherited sbInsert: TSpeedButton
              Left = 433
              Top = 9
              ExplicitLeft = 420
              ExplicitTop = 9
            end
            inherited sbDelete: TSpeedButton
              Left = 458
              Top = 9
              ExplicitLeft = 443
              ExplicitTop = 9
            end
            inherited lblNumberOfDeliveryTypes: TLabel
              Width = 112
              Height = 32
              ExplicitWidth = 112
              ExplicitHeight = 32
            end
          end
          inherited Grid: TRbwDataGrid4
            Width = 601
            Height = 254
            OnSetEditText = frameDeliveryGridSetEditText
            ExplicitWidth = 595
            ExplicitHeight = 245
          end
          inherited pnlTop: TPanel
            Width = 601
            ExplicitWidth = 595
          end
        end
      end
      inherited tabWaterRights: TTabSheet
        inherited frameFormulaGridWaterRights: TframeFormulaGrid
          inherited Panel: TPanel
            inherited sbAdd: TSpeedButton
              Left = 256
              ExplicitLeft = 256
            end
            inherited sbInsert: TSpeedButton
              Left = 303
              ExplicitLeft = 303
            end
            inherited sbDelete: TSpeedButton
              Left = 351
              ExplicitLeft = 351
            end
          end
          inherited Grid: TRbwDataGrid4
            OnSetEditText = frameFormulaGridWaterRightsGridSetEditText
          end
        end
      end
      inherited tabGW_Allocation: TTabSheet
        inherited frameGW_Allocation: TframeFormulaGrid
          inherited Panel: TPanel
            inherited sbAdd: TSpeedButton
              Left = 283
              ExplicitLeft = 295
            end
            inherited sbInsert: TSpeedButton
              Left = 336
              ExplicitLeft = 350
            end
            inherited sbDelete: TSpeedButton
              Left = 389
              ExplicitLeft = 405
            end
          end
          inherited Grid: TRbwDataGrid4
            OnSetEditText = frameGW_AllocationGridSetEditText
          end
        end
      end
      inherited tabSwAllotment: TTabSheet
        inherited frameSwAllotment: TframeFormulaGrid
          inherited Panel: TPanel
            ExplicitTop = 320
            ExplicitWidth = 601
          end
        end
      end
      inherited tabWaterSupplyConcentration: TTabSheet
        ExplicitWidth = 601
        ExplicitHeight = 361
        inherited frameWaterSupplyConcentration: TframeFormulaGrid
          Width = 601
          Height = 361
          ExplicitWidth = 601
          ExplicitHeight = 361
          inherited Panel: TPanel
            Top = 320
            Width = 601
            ExplicitTop = 320
            ExplicitWidth = 601
          end
          inherited Grid: TRbwDataGrid4
            Width = 601
            Height = 263
            ExplicitWidth = 601
            ExplicitHeight = 263
          end
          inherited pnlTop: TPanel
            Width = 601
            ExplicitWidth = 601
          end
        end
      end
      inherited tabNoReturnFlow: TTabSheet
        inherited frameNoReturnFlow: TframeFormulaGrid
          inherited Panel: TPanel
            ExplicitTop = 319
            ExplicitWidth = 597
            inherited sbAdd: TSpeedButton
              Left = 310
              ExplicitLeft = 314
            end
            inherited sbInsert: TSpeedButton
              Left = 368
              ExplicitLeft = 372
            end
            inherited sbDelete: TSpeedButton
              Left = 424
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
            inherited sbAdd: TSpeedButton
              Left = 477
              ExplicitLeft = 477
            end
            inherited sbDelete: TSpeedButton
              Left = 535
              ExplicitLeft = 535
            end
          end
          inherited Panel1: TPanel
            inherited frameFarmDiversions: TframeFarmDiversion
              inherited Panel: TPanel
                inherited sbAdd: TSpeedButton
                  Left = 357
                  ExplicitLeft = 357
                end
                inherited sbInsert: TSpeedButton
                  Left = 377
                  ExplicitLeft = 377
                end
                inherited sbDelete: TSpeedButton
                  Left = 398
                  ExplicitLeft = 398
                end
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
            ExplicitTop = 320
            ExplicitWidth = 601
            inherited sbAdd: TSpeedButton
              Left = 493
              ExplicitLeft = 473
            end
            inherited sbDelete: TSpeedButton
              Left = 558
              ExplicitLeft = 535
            end
          end
          inherited Panel1: TPanel
            inherited pnlName: TPanel
              ExplicitWidth = 473
            end
            inherited frameFarmDiversions: TframeFarmDiversion
              inherited Panel: TPanel
                inherited sbAdd: TSpeedButton
                  Left = 290
                  ExplicitLeft = 275
                end
                inherited sbInsert: TSpeedButton
                  Left = 308
                  ExplicitLeft = 292
                end
                inherited sbDelete: TSpeedButton
                  Left = 325
                  ExplicitLeft = 308
                end
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
    ExplicitHeight = 412
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
      Columns = <>
    end
    object pnlControls: TPanel
      Left = 1
      Top = 336
      Width = 143
      Height = 84
      Align = alBottom
      TabOrder = 1
      ExplicitTop = 327
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
        Width = 115
        Height = 18
        Caption = 'Number of farms'
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
    Width = 759
    Height = 41
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 412
    ExplicitWidth = 753
    DesignSize = (
      759
      41)
    object btnHelp: TBitBtn
      Left = 364
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 358
    end
    object btnOK: TBitBtn
      Left = 478
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnOKClick
      ExplicitLeft = 472
    end
    object btnCancel: TBitBtn
      Left = 592
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 1
      ExplicitLeft = 586
    end
  end
end
