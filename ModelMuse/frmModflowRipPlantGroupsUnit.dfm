inherited frmModflowRipPlantGroups: TfrmModflowRipPlantGroups
  HelpType = htKeyword
  HelpKeyword = 'Riparian_ET_Plant_Groups_Dialo'
  Caption = 'Riparian ET Plant Groups'
  ClientHeight = 414
  ClientWidth = 613
  ExplicitWidth = 625
  ExplicitHeight = 452
  TextHeight = 18
  object splMain: TSplitter
    Left = 177
    Top = 0
    Width = 5
    Height = 373
    ExplicitLeft = 137
    ExplicitHeight = 374
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 373
    Width = 613
    Height = 41
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 372
    ExplicitWidth = 609
    DesignSize = (
      613
      41)
    object btnHelp: TBitBtn
      Left = 264
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 260
    end
    object btnOK: TBitBtn
      Left = 378
      Top = 6
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnOKClick
      ExplicitLeft = 374
    end
    object btnCancel: TBitBtn
      Left = 492
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 1
      ExplicitLeft = 488
    end
  end
  object pnlRipPlantGroupSelection: TPanel
    Left = 0
    Top = 0
    Width = 177
    Height = 373
    Align = alLeft
    TabOrder = 0
    ExplicitHeight = 372
    object grdpnl1: TGridPanel
      Left = 1
      Top = 340
      Width = 175
      Height = 32
      Align = alBottom
      ColumnCollection = <
        item
          Value = 33.333333333333340000
        end
        item
          Value = 33.333333333333340000
        end
        item
          Value = 33.333333333333310000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = btnAddPlantGroup
          Row = 0
        end
        item
          Column = 1
          Control = btnInsertPlantGroup
          Row = 0
        end
        item
          Column = 2
          Control = btnDeletePlantGroup
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 1
      ExplicitTop = 339
      DesignSize = (
        175
        32)
      object btnAddPlantGroup: TSpeedButton
        Left = 18
        Top = 5
        Width = 23
        Height = 22
        Hint = 'Add plant group|Add a plant group below the bottom plant group.'
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
        OnClick = btnAddPlantGroupClick
        ExplicitLeft = 11
        ExplicitTop = 6
      end
      object btnInsertPlantGroup: TSpeedButton
        Left = 76
        Top = 5
        Width = 23
        Height = 22
        Hint = 
          'Insert a plant group|Insert a plant group above the selected pla' +
          'nt group.'
        Anchors = []
        Enabled = False
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
        OnClick = btnInsertPlantGroupClick
        ExplicitLeft = 48
        ExplicitTop = 15
      end
      object btnDeletePlantGroup: TSpeedButton
        Left = 133
        Top = 5
        Width = 23
        Height = 22
        Hint = 'Delete plant group|Delete the selected plant group.'
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
        OnClick = btnDeletePlantGroupClick
        ExplicitLeft = 107
        ExplicitTop = 6
      end
    end
    object tvPlantGroups: TTreeView
      Left = 1
      Top = 1
      Width = 175
      Height = 339
      Align = alClient
      HideSelection = False
      Indent = 20
      MultiSelect = True
      ReadOnly = True
      TabOrder = 0
      OnChange = tvPlantGroupsChange
      ExplicitHeight = 338
    end
  end
  object pgcMain: TPageControl
    Left = 182
    Top = 0
    Width = 431
    Height = 373
    ActivePage = tsProperties
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 427
    ExplicitHeight = 372
    object tsProperties: TTabSheet
      Caption = 'Properties'
      DesignSize = (
        423
        340)
      object lblSatExtDepth: TLabel
        Left = 16
        Top = 72
        Width = 221
        Height = 18
        Caption = 'Saturated extinction depth (Sxd)'
      end
      object lblActiveRootDepth: TLabel
        Left = 16
        Top = 136
        Width = 156
        Height = 18
        Caption = 'Active root depth (Ard)'
      end
      object lblMaxEtFlus: TLabel
        Left = 16
        Top = 192
        Width = 133
        Height = 18
        Caption = 'Max ET flux (Rmax)'
      end
      object lblMaxEvapFlux: TLabel
        Left = 16
        Top = 248
        Width = 356
        Height = 18
        Caption = 'Evaporative flux at saturated extinction depth (Rsxd)'
      end
      object lbledtRipName: TLabeledEdit
        Left = 16
        Top = 40
        Width = 396
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 105
        EditLabel.Height = 18
        EditLabel.Caption = 'Name (RIPNM)'
        MaxLength = 24
        TabOrder = 0
        Text = ''
        ExplicitWidth = 392
      end
      object cedSatExtDepth: TJvComboEdit
        Left = 16
        Top = 96
        Width = 396
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        ButtonWidth = 50
        DisabledColor = clBtnFace
        Glyph.Data = {
          26020000424D260200000000000076000000280000002D000000120000000100
          040000000000B001000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
          FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
          FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
          F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
          F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
          FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
          F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
          FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
          FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
          F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFF000}
        TabOrder = 1
        Text = ''
        OnButtonClick = EditFormula
        OnChange = ChangeComboEdColor
        ExplicitWidth = 392
      end
      object cedActiveRootDepth: TJvComboEdit
        Left = 16
        Top = 160
        Width = 396
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        ButtonWidth = 50
        DisabledColor = clBtnFace
        Glyph.Data = {
          26020000424D260200000000000076000000280000002D000000120000000100
          040000000000B001000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
          FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
          FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
          F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
          F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
          FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
          F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
          FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
          FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
          F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFF000}
        TabOrder = 2
        Text = ''
        OnButtonClick = EditFormula
        OnChange = ChangeComboEdColor
        ExplicitWidth = 392
      end
      object cedMaxEtFlux: TJvComboEdit
        Left = 16
        Top = 216
        Width = 396
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        ButtonWidth = 50
        DisabledColor = clBtnFace
        Glyph.Data = {
          26020000424D260200000000000076000000280000002D000000120000000100
          040000000000B001000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
          FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
          FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
          F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
          F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
          FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
          F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
          FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
          FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
          F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFF000}
        TabOrder = 3
        Text = ''
        OnButtonClick = EditFormula
        OnChange = ChangeComboEdColor
        ExplicitWidth = 392
      end
      object cedSatExtinctEvapFlux: TJvComboEdit
        Left = 16
        Top = 272
        Width = 396
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        ButtonWidth = 50
        DisabledColor = clBtnFace
        Glyph.Data = {
          26020000424D260200000000000076000000280000002D000000120000000100
          040000000000B001000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
          FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
          FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
          F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
          F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
          FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
          F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
          FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
          FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
          F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFF000}
        TabOrder = 4
        Text = ''
        OnButtonClick = EditFormula
        OnChange = ChangeComboEdColor
        ExplicitWidth = 392
      end
    end
    object tabTranspirationRateCurve: TTabSheet
      Caption = 'Transpiration Rate Curve'
      ImageIndex = 1
      object pbTranspirationCurve: TPaintBox
        Left = 275
        Top = 0
        Width = 152
        Height = 340
        Align = alClient
        OnPaint = pbTranspirationCurvePaint
        ExplicitLeft = 0
        ExplicitWidth = 520
        ExplicitHeight = 233
      end
      inline frameTranspirationCurve: TframeGrid
        Left = 0
        Top = 0
        Width = 275
        Height = 340
        Align = alLeft
        TabOrder = 0
        ExplicitWidth = 275
        ExplicitHeight = 341
        inherited Panel: TPanel
          Top = 303
          Width = 275
          Height = 37
          ExplicitTop = 304
          ExplicitWidth = 275
          ExplicitHeight = 37
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
          Width = 275
          Height = 303
          ColCount = 3
          FixedCols = 1
          OnBeforeDrawCell = frameTranspirationCurveGridBeforeDrawCell
          OnEndUpdate = frameTranspirationCurveGridEndUpdate
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
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              AutoAdjustCaptionRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -13
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = True
              CheckMin = True
              ComboUsed = False
              Format = rcf4Real
              LimitToList = False
              Max = 1.000000000000000000
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
              AutoAdjustCaptionRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -13
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = True
              CheckMin = True
              ComboUsed = False
              Format = rcf4Real
              LimitToList = False
              Max = 1.000000000000000000
              MaxLength = 0
              Min = -1.000000000000000000
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end>
          ExplicitWidth = 275
          ExplicitHeight = 304
          ColWidths = (
            18
            64
            64)
        end
      end
    end
  end
end
