inherited frmFarm: TfrmFarm
  HelpType = htKeyword
  HelpKeyword = 'Farms_Dialog_Box'
  Caption = 'Farms'
  ClientHeight = 462
  ClientWidth = 759
  ExplicitWidth = 771
  ExplicitHeight = 500
  TextHeight = 18
  inline frameFarm: TframeFarm
    Left = 185
    Top = 0
    Width = 574
    Height = 421
    Align = alClient
    Enabled = False
    TabOrder = 1
    ExplicitLeft = 185
    ExplicitWidth = 574
    ExplicitHeight = 421
    inherited pcMain: TJvgPageControl
      Width = 574
      Height = 421
      ActivePage = frameFarm.tabReturnFlowLocation
      Font.Pitch = fpVariable
      OnChange = frameFarmpcMainChange
      ExplicitWidth = 570
      ExplicitHeight = 420
      inherited tabCrops: TTabSheet
        inherited frameFormulaGridCrops: TframeFormulaGrid
          inherited Panel: TPanel
            ExplicitTop = 226
            ExplicitWidth = 577
            inherited sbAdd: TSpeedButton
              Left = 289
              ExplicitLeft = 260
            end
            inherited sbInsert: TSpeedButton
              Left = 343
              ExplicitLeft = 343
            end
            inherited sbDelete: TSpeedButton
              Left = 397
              ExplicitLeft = 397
            end
          end
          inherited Grid: TRbwDataGrid4
            OnSetEditText = frameFormulaGridCropsGridSetEditText
            OnButtonClick = frameFormulaGridCropsGridButtonClick
          end
          inherited pnlTop: TPanel
            ExplicitWidth = 577
            inherited edFormula: TLabeledEdit
              OnChange = frameFormulaGridCropsedFormulaChange
            end
          end
        end
        inherited pnlTop: TPanel
          ExplicitWidth = 577
          inherited seFarmId: TJvSpinEdit
            OnChange = frameFarmseFarmIdChange
          end
          inherited pnlCaption: TPanel
            ExplicitWidth = 577
          end
          inherited edFarmName: TLabeledEdit
            OnChange = frameFarmedFarmNameChange
          end
        end
      end
      inherited tabEfficiencyImprovement: TTabSheet
        inherited frameFormulaGridEfficiencyImprovement: TframeFormulaGrid
          inherited Panel: TPanel
            ExplicitTop = 291
            ExplicitWidth = 577
          end
          inherited pnlTop: TPanel
            ExplicitWidth = 577
          end
        end
      end
      inherited tabAddedDemandRunoffSplit: TTabSheet
        inherited frameAddedDemandRunoffSplit: TframeFormulaGrid
          ExplicitWidth = 577
          ExplicitHeight = 332
          inherited Panel: TPanel
            ExplicitTop = 291
            ExplicitWidth = 577
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
          inherited pnlTop: TPanel
            ExplicitWidth = 577
          end
        end
      end
      inherited tabIrrigationUniformity: TTabSheet
        inherited frameIrrigationUniformity: TframeFormulaGrid
          inherited Panel: TPanel
            ExplicitTop = 291
            ExplicitWidth = 577
          end
          inherited pnlTop: TPanel
            ExplicitWidth = 577
          end
        end
      end
      inherited tabDeficiencyScenario: TTabSheet
        inherited frameDeficiencyScenario: TframeFormulaGrid
          inherited Panel: TPanel
            ExplicitTop = 291
            ExplicitWidth = 577
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
          inherited pnlTop: TPanel
            ExplicitWidth = 577
          end
        end
      end
      inherited tabWaterSource: TTabSheet
        inherited frameWaterSource: TframeFormulaGrid
          inherited Panel: TPanel
            ExplicitTop = 291
            ExplicitWidth = 577
          end
          inherited pnlTop: TPanel
            ExplicitWidth = 577
          end
        end
      end
      inherited tabBareRunoffFractions: TTabSheet
        inherited frameBareRunoffFractions: TframeFormulaGrid
          inherited Panel: TPanel
            ExplicitTop = 291
            ExplicitWidth = 577
          end
          inherited pnlTop: TPanel
            ExplicitWidth = 577
          end
        end
      end
      inherited tabAddedCropDemandFlux: TTabSheet
        inherited frameAddedCropDemandFlux: TframeFormulaGrid
          inherited Panel: TPanel
            ExplicitTop = 291
            ExplicitWidth = 577
          end
          inherited pnlTop: TPanel
            ExplicitWidth = 577
          end
        end
      end
      inherited tabAddedCropDemandRate: TTabSheet
        inherited frameAddedCropDemandRate: TframeFormulaGrid
          inherited Panel: TPanel
            ExplicitTop = 291
            ExplicitWidth = 577
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
          inherited pnlTop: TPanel
            ExplicitWidth = 577
          end
        end
      end
      inherited tabCosts: TTabSheet
        inherited frameFormulaGridCosts: TframeFormulaGrid
          inherited Panel: TPanel
            ExplicitTop = 290
            ExplicitWidth = 577
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
          inherited pnlTop: TPanel
            ExplicitWidth = 577
          end
        end
      end
      inherited tabDiversionLocation: TTabSheet
        ExplicitWidth = 566
        ExplicitHeight = 361
        inherited frameFormulaGridDiversion: TframeFarmDiversion
          Width = 566
          Height = 361
          ExplicitWidth = 562
          ExplicitHeight = 360
          inherited Panel: TPanel
            Top = 319
            Width = 566
            ExplicitTop = 318
            ExplicitWidth = 562
            inherited sbAdd: TSpeedButton
              Left = 477
            end
            inherited sbInsert: TSpeedButton
              Left = 506
            end
            inherited sbDelete: TSpeedButton
              Left = 536
            end
          end
          inherited Grid: TRbwDataGrid4
            Width = 566
            Height = 262
            OnSetEditText = frameFormulaGridDiversionGridSetEditText
            ExplicitWidth = 562
            ExplicitHeight = 261
          end
          inherited pnlTop: TPanel
            Width = 566
            ExplicitWidth = 562
          end
        end
      end
      inherited tabReturnFlowLocation: TTabSheet
        ExplicitWidth = 566
        ExplicitHeight = 361
        inherited frameFormulaGridReturnFlow: TframeFarmDiversion
          Width = 566
          Height = 361
          inherited Panel: TPanel
            Top = 319
            Width = 566
            inherited sbAdd: TSpeedButton
              Left = 458
              ExplicitLeft = 471
            end
            inherited sbInsert: TSpeedButton
              Left = 486
              ExplicitLeft = 499
            end
            inherited sbDelete: TSpeedButton
              Left = 514
              ExplicitLeft = 524
            end
          end
          inherited Grid: TRbwDataGrid4
            Width = 566
            Height = 262
            OnSetEditText = frameFormulaGridReturnFlowGridSetEditText
          end
          inherited pnlTop: TPanel
            Width = 566
          end
        end
      end
      inherited tabNonRoutedDelivery: TTabSheet
        inherited frameDelivery: TframeDeliveryGrid
          ExplicitWidth = 577
          ExplicitHeight = 332
          inherited Panel: TPanel
            ExplicitTop = 282
            ExplicitWidth = 577
            inherited sbAdd: TSpeedButton
              Left = 391
              Top = 9
              ExplicitLeft = 391
              ExplicitTop = 9
            end
            inherited sbInsert: TSpeedButton
              Left = 420
              Top = 9
              ExplicitLeft = 420
              ExplicitTop = 9
            end
            inherited sbDelete: TSpeedButton
              Left = 443
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
            OnSetEditText = frameDeliveryGridSetEditText
            ExplicitWidth = 577
            ExplicitHeight = 225
          end
          inherited pnlTop: TPanel
            ExplicitWidth = 577
          end
        end
      end
      inherited tabWaterRights: TTabSheet
        inherited frameFormulaGridWaterRights: TframeFormulaGrid
          inherited Panel: TPanel
            ExplicitTop = 290
            ExplicitWidth = 577
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
          inherited pnlTop: TPanel
            ExplicitWidth = 577
          end
        end
      end
      inherited tabGW_Allocation: TTabSheet
        inherited frameGW_Allocation: TframeFormulaGrid
          inherited Panel: TPanel
            ExplicitTop = 290
            ExplicitWidth = 577
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
          inherited pnlTop: TPanel
            ExplicitWidth = 577
          end
        end
      end
    end
  end
  object pnlFarms: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 421
    Align = alLeft
    TabOrder = 0
    ExplicitHeight = 420
    object vstFarms: TVirtualStringTree
      Left = 1
      Top = 1
      Width = 183
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
      Width = 183
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
    ExplicitTop = 420
    ExplicitWidth = 755
    DesignSize = (
      759
      41)
    object btnHelp: TBitBtn
      Left = 394
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 390
    end
    object btnOK: TBitBtn
      Left = 508
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnOKClick
      ExplicitLeft = 504
    end
    object btnCancel: TBitBtn
      Left = 622
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 1
      ExplicitLeft = 618
    end
  end
end
