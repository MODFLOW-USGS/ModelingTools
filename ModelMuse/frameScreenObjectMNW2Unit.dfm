inherited frameScreenObjectMNW2: TframeScreenObjectMNW2
  Width = 609
  Height = 476
  Font.Charset = ANSI_CHARSET
  Font.Height = -16
  Font.Name = 'Arial'
  ParentFont = False
  TabStop = True
  ExplicitWidth = 609
  ExplicitHeight = 476
  object pnlCaption: TPanel
    Left = 0
    Top = 0
    Width = 609
    Height = 22
    Align = alTop
    BevelOuter = bvNone
    Caption = 'pnlCaption'
    TabOrder = 0
  end
  object pcMnw2: TPageControl
    Left = 0
    Top = 22
    Width = 609
    Height = 454
    ActivePage = tabObservations
    Align = alClient
    TabOrder = 1
    object tabBasic: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'MNW2_Basic_Tab'
      Caption = 'Basic'
      DesignSize = (
        601
        421)
      object lblWellId: TLabel
        Left = 3
        Top = 9
        Width = 199
        Height = 18
        Caption = 'Unique well name (WELLID) '
      end
      object lblLossType: TLabel
        Left = 3
        Top = 41
        Width = 224
        Height = 18
        Caption = 'Model for well loss (LOSSTYPE)'
      end
      object lblPartialPenetration: TLabel
        Left = 3
        Top = 251
        Width = 218
        Height = 18
        Caption = 'Partial penetration fraction (PP)'
      end
      object lblZPump: TLabel
        Left = 3
        Top = 96
        Width = 169
        Height = 18
        Caption = 'Pump elevation (Zpump)'
      end
      object edWellId: TRbwEdit
        Left = 233
        Top = 6
        Width = 253
        Height = 26
        MaxLength = 20
        TabOrder = 0
        Text = 'WELLID'
        OnChange = edWellIdChange
      end
      object comboLossType: TJvImageComboBox
        Left = 233
        Top = 38
        Width = 253
        Height = 28
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 253
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 22
        ItemIndex = 0
        TabOrder = 1
        OnChange = comboLossTypeChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'None'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Thiem'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Skin'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'General'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Specify cell to well conductance'
          end>
      end
      object cbConstrainPumping: TCheckBox
        Left = 3
        Top = 189
        Width = 224
        Height = 17
        Caption = 'Constrain pumping (Qlimit)'
        TabOrder = 5
        OnClick = cbConstrainPumpingClick
      end
      object cbPartialPenetrationFlag: TCheckBox
        Left = 3
        Top = 221
        Width = 321
        Height = 17
        Caption = 'Correct for partial penetration (PPFLAG)'
        TabOrder = 6
        OnClick = cbPartialPenetrationFlagClick
      end
      object cbPumpCap: TCheckBox
        Left = 3
        Top = 280
        Width = 377
        Height = 17
        Caption = 'Adjust discharge for changes in lift (PUMPCAP)'
        TabOrder = 8
        OnClick = cbPumpCapClick
      end
      object cbSpecifyPump: TCheckBox
        Left = 3
        Top = 72
        Width = 224
        Height = 17
        Caption = 'Specify pump (PUMPLOC)'
        TabOrder = 2
        OnClick = cbSpecifyPumpClick
      end
      object edPartialPenetration: TJvComboEdit
        Left = 233
        Top = 248
        Width = 253
        Height = 26
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
        TabOrder = 7
        Text = ''
        OnChange = edPartialPenetrationChange
      end
      inline framePumpLocationMethod: TframeLocationMethod
        Left = -4
        Top = 121
        Width = 605
        Height = 62
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = 19
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        TabStop = True
        ExplicitLeft = -4
        ExplicitTop = 121
        ExplicitWidth = 605
        ExplicitHeight = 62
        inherited lblLocationMethod: TLabel
          Left = 7
          Width = 161
          Caption = 'Pump location method'
          ExplicitLeft = 7
          ExplicitWidth = 161
        end
        inherited pcLocationChoice: TJvPageControl
          Width = 268
          Height = 62
          ActivePage = framePumpLocationMethod.tabNone
          ExplicitWidth = 268
          ExplicitHeight = 62
          inherited tabNone: TTabSheet
            ExplicitWidth = 268
            ExplicitHeight = 60
          end
          inherited tabObject: TTabSheet
            inherited comboObject: TComboBox
              Width = 258
              ExplicitWidth = 258
            end
          end
        end
        inherited comboLocationChoice: TJvImageComboBox
          Left = 173
          OnChange = framePumpLocationMethodcomboLocationChoiceChange
          ExplicitLeft = 173
        end
      end
      object rdeZPump: TRbwDataEntry
        Left = 229
        Top = 93
        Width = 257
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 3
        Text = '0'
        OnChange = edWellIdChange
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object gbMNWI: TGroupBox
        Left = 3
        Top = 303
        Width = 582
        Height = 87
        Caption = 'Multi-Node Well Information (MNWI) Package Options'
        TabOrder = 9
        object cbSaveExternal: TCheckBox
          Left = 3
          Top = 44
          Width = 576
          Height = 17
          Caption = 
            'Save flows between each well node and the aquifer to a separate ' +
            'file (QNDflag)'
          TabOrder = 1
          OnClick = cbSaveExternalClick
        end
        object cbSaveInternal: TCheckBox
          Left = 3
          Top = 67
          Width = 550
          Height = 17
          Caption = 'Save intraborehole flows to a separate file (QBHflag)'
          TabOrder = 2
          OnClick = cbSaveInternalClick
        end
        object cbSaveMnwiBasic: TCheckBox
          Left = 3
          Top = 21
          Width = 278
          Height = 17
          Caption = 'Save well fluxes and head'
          TabOrder = 0
          OnClick = cbSaveMnwiBasicClick
        end
      end
    end
    object tabLossControls: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'MNW2_Loss_Controls_Tab'
      Caption = 'Loss Controls'
      ImageIndex = 1
      object lblWellRadius: TLabel
        Left = 3
        Top = 3
        Width = 113
        Height = 18
        Caption = 'Well radius (Rw)'
      end
      object lblSkinRadius: TLabel
        Left = 3
        Top = 32
        Width = 131
        Height = 18
        Caption = 'Skin radius (Rskin)'
      end
      object lblBCoefficient: TLabel
        Left = 3
        Top = 91
        Width = 206
        Height = 18
        Caption = 'Linear well loss coefficient (B)'
      end
      object lblCCoefficient: TLabel
        Left = 3
        Top = 123
        Width = 234
        Height = 18
        Caption = 'Non-linear well loss coefficient (C)'
      end
      object lblPCoefficient: TLabel
        Left = 3
        Top = 155
        Width = 105
        Height = 18
        Caption = 'Power term (P)'
      end
      object lblCellToWellConductance: TLabel
        Left = 3
        Top = 187
        Width = 219
        Height = 18
        Caption = 'Cell to well conductance (CWC)'
      end
      object lblKSkin: TLabel
        Left = 3
        Top = 61
        Width = 233
        Height = 18
        Caption = 'Skin hydraulic conductivity (Kskin)'
      end
      object edWellRadius: TJvComboEdit
        Left = 122
        Top = 0
        Width = 364
        Height = 26
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
        TabOrder = 0
        Text = ''
        OnChange = edWellRadiusChange
      end
      object edSkinRadius: TJvComboEdit
        Left = 140
        Top = 29
        Width = 346
        Height = 26
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
        OnChange = edWellRadiusChange
      end
      object edKSkin: TJvComboEdit
        Left = 242
        Top = 58
        Width = 244
        Height = 26
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
        OnChange = edWellRadiusChange
      end
      object edBCoefficient: TJvComboEdit
        Left = 215
        Top = 88
        Width = 271
        Height = 26
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
        OnChange = edWellRadiusChange
      end
      object edCCoefficient: TJvComboEdit
        Left = 242
        Top = 120
        Width = 244
        Height = 26
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
        OnChange = edWellRadiusChange
      end
      object edPCoefficient: TJvComboEdit
        Left = 114
        Top = 152
        Width = 372
        Height = 26
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
        TabOrder = 5
        Text = ''
        OnChange = edWellRadiusChange
      end
      object edCellToWellConductance: TJvComboEdit
        Left = 228
        Top = 184
        Width = 258
        Height = 26
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
        TabOrder = 6
        Text = ''
        OnChange = edWellRadiusChange
      end
    end
    object tabDischargeAdjustment: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'MNW2_Discharge_Adjustment_Tab'
      Caption = 'Discharge Adjustment'
      ImageIndex = 2
      DesignSize = (
        601
        421)
      object lblReferenceHead: TLabel
        Left = 8
        Top = 6
        Width = 151
        Height = 18
        Caption = 'Reference head (Hlift)'
      end
      object lblLiftQ0: TLabel
        Left = 8
        Top = 38
        Width = 149
        Height = 18
        Caption = 'Maximum lift (LIFTq0)'
      end
      object lblLiftQMax: TLabel
        Left = 8
        Top = 67
        Width = 287
        Height = 18
        Caption = 'Lift at maximum pumping rate (LIFTqmax)'
      end
      object lblWellTolerance: TLabel
        Left = 8
        Top = 95
        Width = 154
        Height = 18
        Caption = 'Well tolerance (HWtol)'
      end
      object lflLiftTableRows: TLabel
        Left = 3
        Top = 227
        Width = 203
        Height = 18
        Caption = 'Number of rows (PUMPCAP)'
      end
      object lblLiftTable: TLabel
        Left = 232
        Top = 131
        Width = 139
        Height = 18
        Caption = 'Head-capacity table'
      end
      object rdeReferenceHead: TRbwDataEntry
        Left = 314
        Top = 7
        Width = 172
        Height = 22
        TabOrder = 0
        Text = '0'
        OnChange = edWellIdChange
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeLiftQ0: TRbwDataEntry
        Left = 314
        Top = 35
        Width = 172
        Height = 22
        TabOrder = 1
        Text = '0'
        OnChange = edWellIdChange
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeLiftQMax: TRbwDataEntry
        Left = 314
        Top = 63
        Width = 172
        Height = 22
        TabOrder = 2
        Text = '0'
        OnChange = edWellIdChange
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeWellTolerance: TRbwDataEntry
        Left = 314
        Top = 91
        Width = 172
        Height = 22
        TabOrder = 3
        Text = '0'
        OnChange = edWellIdChange
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdgLiftTable: TRbwDataGrid4
        Left = 232
        Top = 155
        Width = 334
        Height = 254
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColCount = 2
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 5
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = True
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = False
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
        OnEndUpdate = rdgLiftTableEndUpdate
        WordWrapRowCaptions = False
      end
      object seLiftTableRows: TJvSpinEdit
        Left = 3
        Top = 248
        Width = 121
        Height = 26
        ButtonKind = bkClassic
        MaxValue = 25.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 7
        OnChange = seLiftTableRowsChange
      end
      object btnInsertLift: TButton
        Left = 3
        Top = 152
        Width = 75
        Height = 25
        Caption = 'Insert'
        TabOrder = 4
        OnClick = btnInsertLiftClick
      end
      object btnDeleteLift: TButton
        Left = 3
        Top = 183
        Width = 75
        Height = 25
        Caption = 'Delete'
        TabOrder = 6
        OnClick = btnDeleteLiftClick
      end
    end
    object tabPumpingRate: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'MNW2_Pumping_Rate_Tab'
      Caption = 'Pumping Rate'
      ImageIndex = 3
      object rdgTimeTable: TRbwDataGrid4
        Left = 0
        Top = 65
        Width = 601
        Height = 315
        Align = alClient
        ColCount = 8
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
        TabOrder = 1
        OnMouseUp = rdgTimeTableMouseUp
        OnSelectCell = rdgTimeTableSelectCell
        OnSetEditText = rdgTimeTableSetEditText
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = True
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnColSize = rdgTimeTableColSize
        ColorRangeSelection = False
        OnHorizontalScroll = rdgTimeTableHorizontalScroll
        Columns = <
          item
            AutoAdjustRowHeights = True
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
            ComboUsed = True
            Format = rcf4Real
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
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 20
            CheckMax = False
            CheckMin = False
            ComboUsed = True
            Format = rcf4Real
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
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 35
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
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 35
            CheckMax = False
            CheckMin = True
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
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 35
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
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 20
            CheckMax = False
            CheckMin = False
            ComboUsed = True
            Format = rcf4String
            LimitToList = True
            MaxLength = 0
            ParentButtonFont = False
            PickList.Strings = (
              'No minimum'
              'Rate'
              'Fraction')
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            AutoAdjustCaptionRowHeights = False
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 35
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
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 35
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
          end>
        OnEndUpdate = rdgTimeTableEndUpdate
        WordWrapRowCaptions = False
        ColWidths = (
          64
          64
          64
          64
          64
          109
          64
          64)
      end
      object Panel1: TPanel
        Left = 0
        Top = 380
        Width = 601
        Height = 41
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
        object lblTimeTableRows: TLabel
          Left = 79
          Top = 9
          Width = 163
          Height = 18
          Caption = 'Number of time periods'
        end
        object seTimeTableRows: TJvSpinEdit
          Left = 8
          Top = 6
          Width = 65
          Height = 26
          ButtonKind = bkClassic
          MaxValue = 1000000.000000000000000000
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          TabOrder = 0
          OnChange = seTimeTableRowsChange
        end
        object btnInsertTime: TButton
          Left = 272
          Top = 6
          Width = 75
          Height = 25
          Caption = 'Insert'
          TabOrder = 1
          OnClick = btnInsertTimeClick
        end
        object btnDeleteTime: TButton
          Left = 353
          Top = 6
          Width = 75
          Height = 25
          Caption = 'Delete'
          TabOrder = 2
          OnClick = btnDeleteTimeClick
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 601
        Height = 65
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object lblFormula: TLabel
          Left = 136
          Top = 3
          Width = 57
          Height = 18
          Alignment = taCenter
          Caption = 'Formula'
        end
        object lblQCUT: TLabel
          Left = 264
          Top = 3
          Width = 44
          Height = 18
          Caption = 'QCUT'
        end
        object rdeFormula: TRbwDataEntry
          Left = 136
          Top = 27
          Width = 57
          Height = 25
          Color = clBtnFace
          Enabled = False
          TabOrder = 0
          Text = ''
          OnChange = rdeFormulaChange
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
        object comboQCUT: TJvImageComboBox
          Left = 264
          Top = 24
          Width = 65
          Height = 28
          Style = csOwnerDrawVariable
          ButtonStyle = fsLighter
          Color = clBtnFace
          DroppedWidth = 145
          Enabled = False
          ImageHeight = 0
          ImageWidth = 0
          ItemHeight = 22
          ItemIndex = -1
          TabOrder = 1
          OnChange = comboQCUTChange
          Items = <>
        end
      end
    end
    object tabWellScreens: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Well_Screens'
      Caption = 'Well Screens'
      ImageIndex = 4
      object Panel3: TPanel
        Left = 0
        Top = 380
        Width = 601
        Height = 41
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
        object Label1: TLabel
          Left = 79
          Top = 9
          Width = 161
          Height = 18
          Caption = 'Number of well screens'
        end
        object seVerticalScreens: TJvSpinEdit
          Left = 8
          Top = 6
          Width = 65
          Height = 26
          ButtonKind = bkClassic
          MaxValue = 1000000.000000000000000000
          TabOrder = 0
          OnChange = seVerticalScreensChange
        end
        object btnInsertVertialScreen: TButton
          Left = 246
          Top = 7
          Width = 75
          Height = 25
          Caption = 'Insert'
          TabOrder = 1
          OnClick = btnInsertVertialScreenClick
        end
        object btnDeleteVertialScreen: TButton
          Left = 327
          Top = 6
          Width = 75
          Height = 25
          Caption = 'Delete'
          TabOrder = 2
          OnClick = btnDeleteVertialScreenClick
        end
      end
      object rdgVerticalScreens: TRbwDataGrid4
        Left = 0
        Top = 97
        Width = 601
        Height = 283
        Align = alClient
        ColCount = 9
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
        TabOrder = 1
        OnMouseUp = rdgVerticalScreensMouseUp
        OnSelectCell = rdgVerticalScreensSelectCell
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnColSize = rdgVerticalScreensColSize
        ColorRangeSelection = False
        OnHorizontalScroll = rdgVerticalScreensHorizontalScroll
        Columns = <
          item
            AutoAdjustRowHeights = True
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
          end
          item
            AutoAdjustRowHeights = True
            AutoAdjustCaptionRowHeights = False
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 35
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
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 35
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
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 35
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
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 35
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
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 35
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
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 35
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
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 35
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
          end>
        OnEndUpdate = rdgVerticalScreensEndUpdate
        WordWrapRowCaptions = False
      end
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 601
        Height = 97
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label2: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 594
          Height = 36
          Align = alTop
          Caption = 
            'Vertical screens are optional; if none are defined here, the top' +
            ' and bottom of the object as defined by the higher and lower Z c' +
            'oordinates will define a well screen.'
          WordWrap = True
        end
        object lblWellScreenFormula: TLabel
          Left = 136
          Top = 43
          Width = 57
          Height = 18
          Alignment = taCenter
          Caption = 'Formula'
        end
        object rdeWellScreenFormula: TRbwDataEntry
          Left = 136
          Top = 66
          Width = 57
          Height = 25
          Color = clBtnFace
          Enabled = False
          TabOrder = 0
          Text = ''
          OnChange = rdeWellScreenFormulaChange
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
      end
    end
    object tabObservations: TTabSheet
      Caption = 'Calibration'
      ImageIndex = 5
      inline framePestObsMnw2: TframePestObs
        Left = 0
        Top = 0
        Width = 601
        Height = 421
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 601
        ExplicitHeight = 421
        inherited splObservations: TSplitter
          Top = 244
          Width = 601
          ExplicitTop = 243
          ExplicitWidth = 601
        end
        inherited grpDirectObs: TGroupBox
          Width = 601
          Height = 244
          ExplicitWidth = 601
          ExplicitHeight = 244
          inherited frameObservations: TframeGrid
            Top = 20
            Width = 597
            Height = 222
            ExplicitTop = 20
            ExplicitWidth = 597
            ExplicitHeight = 222
            inherited Panel: TPanel
              Top = 190
              Width = 597
              ExplicitTop = 190
              ExplicitWidth = 597
              inherited lbNumber: TLabel
                Width = 208
                Height = 18
                ExplicitWidth = 208
                ExplicitHeight = 18
              end
              inherited seNumber: TJvSpinEdit
                Height = 26
                ExplicitHeight = 26
              end
            end
            inherited Grid: TRbwDataGrid4
              Width = 597
              Height = 190
              Columns = <
                item
                  AutoAdjustRowHeights = True
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
                  MaxLength = 14
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
                  PickList.Strings = (
                    'MNW2_Qin'
                    'MNW2_Qout'
                    'MNW2_Qnet'
                    'MNW2_QCumu'
                    'MNW2_Hwell')
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
                  ButtonFont.Height = -11
                  ButtonFont.Name = 'Tahoma'
                  ButtonFont.Style = []
                  ButtonUsed = False
                  ButtonWidth = 20
                  CheckMax = False
                  CheckMin = False
                  ComboUsed = True
                  Format = rcf4String
                  LimitToList = True
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
                end
                item
                  AutoAdjustRowHeights = True
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
                end
                item
                  AutoAdjustRowHeights = True
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
                end>
              ExplicitWidth = 597
              ExplicitHeight = 190
            end
          end
        end
        inherited grpObsComparisons: TGroupBox
          Top = 249
          Width = 601
          ExplicitTop = 249
          ExplicitWidth = 601
          inherited frameObsComparisons: TframeGrid
            Top = 20
            Width = 597
            Height = 150
            ExplicitTop = 20
            ExplicitWidth = 597
            ExplicitHeight = 150
            inherited Panel: TPanel
              Top = 115
              Width = 597
              ExplicitTop = 115
              ExplicitWidth = 597
              inherited lbNumber: TLabel
                Width = 251
                Height = 18
                ExplicitWidth = 251
                ExplicitHeight = 18
              end
              inherited seNumber: TJvSpinEdit
                Height = 26
                ExplicitHeight = 26
              end
            end
            inherited Grid: TRbwDataGrid4
              Width = 597
              Height = 115
              ExplicitWidth = 597
              ExplicitHeight = 115
            end
          end
        end
      end
    end
  end
end
