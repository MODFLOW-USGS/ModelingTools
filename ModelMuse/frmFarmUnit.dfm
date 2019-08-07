inherited frmFarm: TfrmFarm
  HelpType = htKeyword
  HelpKeyword = 'Farms_Dialog_Box'
  Caption = 'Farms'
  ClientHeight = 462
  ClientWidth = 759
  ExplicitWidth = 775
  ExplicitHeight = 500
  PixelsPerInch = 96
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
      ExplicitWidth = 574
      ExplicitHeight = 421
      inherited tabCrops: TTabSheet
        ExplicitLeft = 4
        ExplicitTop = 46
        ExplicitWidth = 577
        ExplicitHeight = 342
        inherited frameFormulaGridCrops: TframeFormulaGrid
          inherited Panel: TPanel
            inherited sbAdd: TSpeedButton
              Left = 289
              ExplicitLeft = 260
            end
            inherited sbInsert: TSpeedButton
              Left = 343
              ExplicitLeft = 308
            end
            inherited sbDelete: TSpeedButton
              Left = 397
              ExplicitLeft = 357
            end
          end
          inherited Grid: TRbwDataGrid4
            OnSetEditText = frameFormulaGridCropsGridSetEditText
            OnButtonClick = frameFormulaGridCropsGridButtonClick
          end
          inherited pnlTop: TPanel
            inherited edFormula: TLabeledEdit
              EditLabel.ExplicitHeight = 18
              OnChange = frameFormulaGridCropsedFormulaChange
            end
          end
        end
        inherited pnlTop: TPanel
          inherited seFarmId: TJvSpinEdit
            OnChange = frameFarmseFarmIdChange
          end
          inherited edFarmName: TLabeledEdit
            OnChange = frameFarmedFarmNameChange
          end
        end
      end
      inherited tabCosts: TTabSheet
        ExplicitLeft = 4
        ExplicitTop = 46
        ExplicitWidth = 577
        ExplicitHeight = 342
        inherited frameFormulaGridCosts: TframeFormulaGrid
          inherited Panel: TPanel
            inherited sbAdd: TSpeedButton
              Left = 256
              ExplicitLeft = 260
            end
            inherited sbInsert: TSpeedButton
              Left = 303
              ExplicitLeft = 308
            end
            inherited sbDelete: TSpeedButton
              Left = 351
              ExplicitLeft = 357
            end
          end
          inherited Grid: TRbwDataGrid4
            OnSetEditText = frameFormulaGridCostsGridSetEditText
          end
          inherited pnlTop: TPanel
            inherited edFormula: TLabeledEdit
              EditLabel.ExplicitHeight = 18
            end
          end
        end
      end
      inherited tabDiversionLocation: TTabSheet
        ExplicitLeft = 4
        ExplicitTop = 46
        ExplicitWidth = 566
        ExplicitHeight = 371
        inherited frameFormulaGridDiversion: TframeFarmDiversion
          Width = 566
          Height = 371
          ExplicitWidth = 566
          ExplicitHeight = 371
          inherited Panel: TPanel
            Top = 330
            Width = 566
            ExplicitTop = 330
            ExplicitWidth = 566
          end
          inherited Grid: TRbwDataGrid4
            Width = 566
            Height = 273
            OnSetEditText = frameFormulaGridDiversionGridSetEditText
            ExplicitWidth = 566
            ExplicitHeight = 273
          end
          inherited pnlTop: TPanel
            Width = 566
            ExplicitWidth = 566
            inherited edFormula: TLabeledEdit
              Height = 24
              EditLabel.ExplicitHeight = 18
              ExplicitHeight = 24
            end
          end
        end
      end
      inherited tabReturnFlowLocation: TTabSheet
        ExplicitLeft = 4
        ExplicitTop = 46
        ExplicitWidth = 577
        ExplicitHeight = 342
        inherited frameFormulaGridReturnFlow: TframeFarmDiversion
          Width = 566
          Height = 371
          inherited Panel: TPanel
            Top = 330
            Width = 566
            inherited sbAdd: TSpeedButton
              Left = 471
            end
            inherited sbInsert: TSpeedButton
              Left = 499
            end
            inherited sbDelete: TSpeedButton
              Left = 528
            end
          end
          inherited Grid: TRbwDataGrid4
            Width = 566
            Height = 273
            OnSetEditText = frameFormulaGridReturnFlowGridSetEditText
          end
          inherited pnlTop: TPanel
            Width = 566
            inherited edFormula: TLabeledEdit
              Height = 24
              EditLabel.ExplicitHeight = 18
              ExplicitHeight = 24
            end
          end
        end
      end
      inherited tabNonRoutedDelivery: TTabSheet
        ExplicitLeft = 4
        ExplicitTop = 46
        ExplicitWidth = 577
        ExplicitHeight = 342
        inherited frameDelivery: TframeDeliveryGrid
          inherited Panel: TPanel
            inherited sbAdd: TSpeedButton
              Left = 356
              Top = 9
              ExplicitLeft = 362
              ExplicitTop = 9
            end
            inherited sbInsert: TSpeedButton
              Left = 404
              Top = 9
              ExplicitLeft = 411
              ExplicitTop = 9
            end
            inherited sbDelete: TSpeedButton
              Left = 452
              Top = 9
              ExplicitLeft = 460
              ExplicitTop = 9
            end
          end
          inherited Grid: TRbwDataGrid4
            OnSetEditText = frameDeliveryGridSetEditText
          end
          inherited pnlTop: TPanel
            inherited edFormula: TLabeledEdit
              EditLabel.ExplicitHeight = 18
            end
          end
        end
      end
      inherited tabWaterRights: TTabSheet
        ExplicitLeft = 4
        ExplicitTop = 46
        ExplicitWidth = 577
        ExplicitHeight = 342
        inherited frameFormulaGridWaterRights: TframeFormulaGrid
          inherited Panel: TPanel
            inherited sbAdd: TSpeedButton
              Left = 256
              ExplicitLeft = 260
            end
            inherited sbInsert: TSpeedButton
              Left = 303
              ExplicitLeft = 308
            end
            inherited sbDelete: TSpeedButton
              Left = 351
              ExplicitLeft = 357
            end
          end
          inherited Grid: TRbwDataGrid4
            OnSetEditText = frameFormulaGridWaterRightsGridSetEditText
          end
          inherited pnlTop: TPanel
            inherited edFormula: TLabeledEdit
              EditLabel.ExplicitHeight = 18
            end
          end
        end
      end
      inherited tabGW_Allocation: TTabSheet
        ExplicitLeft = 4
        ExplicitTop = 46
        ExplicitWidth = 577
        ExplicitHeight = 342
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
          inherited pnlTop: TPanel
            inherited edFormula: TLabeledEdit
              EditLabel.ExplicitHeight = 18
            end
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
    object vstFarms: TVirtualStringTree
      Left = 1
      Top = 1
      Width = 183
      Height = 335
      Align = alClient
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.MainColumn = -1
      TabOrder = 0
      TreeOptions.SelectionOptions = [toMultiSelect]
      OnAddToSelection = vstFarmsAddToSelection
      OnGetText = vstFarmsGetText
      OnGetNodeDataSize = vstFarmsGetNodeDataSize
      OnInitNode = vstFarmsInitNode
      OnRemoveFromSelection = vstFarmsRemoveFromSelection
      Columns = <>
    end
    object pnlControls: TPanel
      Left = 1
      Top = 336
      Width = 183
      Height = 84
      Align = alBottom
      TabOrder = 1
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
    DesignSize = (
      759
      41)
    object btnHelp: TBitBtn
      Left = 418
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 532
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 646
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 1
    end
  end
end
