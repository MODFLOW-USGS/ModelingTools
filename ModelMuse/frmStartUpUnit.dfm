inherited frmStartUp: TfrmStartUp
  Left = 303
  Top = 276
  HelpType = htKeyword
  HelpKeyword = 'Start_Up_Dialog_Box'
  ActiveControl = btnNext
  Caption = 'GoPhast'
  ClientHeight = 406
  ClientWidth = 751
  OnClose = FormClose
  ExplicitWidth = 767
  ExplicitHeight = 445
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 359
    Width = 751
    Height = 47
    Align = alBottom
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    DesignSize = (
      751
      47)
    object btnNext: TBitBtn
      Left = 655
      Top = 6
      Width = 86
      Height = 33
      Anchors = [akTop, akRight]
      Caption = 'Next'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000010000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333FF3333333333333003333
        3333333333773FF3333333333309003333333333337F773FF333333333099900
        33333FFFFF7F33773FF30000000999990033777777733333773F099999999999
        99007FFFFFFF33333F7700000009999900337777777F333F7733333333099900
        33333333337F3F77333333333309003333333333337F77333333333333003333
        3333333333773333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333}
      Layout = blGlyphRight
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnNextClick
    end
    object btnDontCreateGrid: TBitBtn
      Left = 468
      Top = 6
      Width = 100
      Height = 33
      Anchors = [akTop, akRight]
      Caption = 'No grid'
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 0
      Visible = False
      OnClick = btnDontCreateGridClick
    end
    object btnHelp: TBitBtn
      Left = 574
      Top = 6
      Width = 75
      Height = 33
      HelpType = htKeyword
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnHelpClick
    end
  end
  object pcStartup: TPageControl
    Left = 0
    Top = 0
    Width = 751
    Height = 359
    ActivePage = tabModelChoice
    Align = alClient
    TabOrder = 0
    OnChange = pcStartupChange
    object tabModelChoice: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Start_Up_Dialog_Box'
      Caption = 'tabModelChoice'
      object rgChoice: TRadioGroup
        Left = 0
        Top = 0
        Width = 743
        Height = 326
        HelpType = htKeyword
        HelpKeyword = 'Start_Up_Dialog_Box'
        Align = alClient
        Caption = 'What do you want to do?'
        ItemIndex = 0
        Items.Strings = (
          'Create new MODFLOW model'
          'Create new PHAST model'
          'Create new SUTRA model'
          'Create new WellFootprint project'
          'Open an existing ModelMuse project'
          'Import MODFLOW-2005 or MODFLOW-NWT model')
        TabOrder = 0
      end
    end
    object tabGeoRef: TTabSheet
      Caption = 'tabGeoRef'
      ImageIndex = 5
      object grpGeoRef: TGroupBox
        Left = 0
        Top = 0
        Width = 743
        Height = 326
        Align = alClient
        Caption = 'Geo Reference and Model Description'
        TabOrder = 0
        object lblSimStartTime: TLabel
          Left = 11
          Top = 267
          Width = 162
          Height = 18
          Caption = 'Simulation starting time'
        end
        object lblSimStartDate: TLabel
          Left = 11
          Top = 200
          Width = 163
          Height = 18
          Caption = 'Simulation starting date'
        end
        object lblModelDescription: TLabel
          Left = 2
          Top = 20
          Width = 739
          Height = 18
          Align = alTop
          Caption = 'Model description'
          ExplicitWidth = 124
        end
        object lblLengthUnit: TLabel
          Left = 427
          Top = 200
          Width = 75
          Height = 18
          Caption = 'Length unit'
        end
        object lblTimeUnit: TLabel
          Left = 570
          Top = 200
          Width = 62
          Height = 18
          Caption = 'Time unit'
        end
        object jvtmdtSimStartTime: TJvTimeEdit
          Left = 11
          Top = 291
          Width = 121
          Height = 26
          ShowSeconds = True
          TabOrder = 3
        end
        object lbledtProjection: TLabeledEdit
          Left = 203
          Top = 291
          Width = 488
          Height = 26
          EditLabel.Width = 70
          EditLabel.Height = 18
          EditLabel.Caption = 'Projection'
          TabOrder = 4
        end
        object rgProjectionType: TRadioGroup
          Left = 203
          Top = 200
          Width = 185
          Height = 57
          Caption = 'Projection type'
          Columns = 2
          ItemIndex = 0
          Items.Strings = (
            'epsg'
            'proj4')
          TabOrder = 1
        end
        object mmoModelDescription: TMemo
          Left = 2
          Top = 38
          Width = 739
          Height = 156
          Align = alTop
          TabOrder = 0
        end
        object calSimStartDate: TJvDateTimePicker
          Left = 11
          Top = 224
          Width = 186
          Height = 26
          Date = 36526.000000000000000000
          Time = 0.415539120360335800
          TabOrder = 2
          DropDownDate = 42572.000000000000000000
        end
        object comboLengthUnit: TComboBox
          Left = 427
          Top = 224
          Width = 121
          Height = 26
          Style = csDropDownList
          TabOrder = 5
        end
        object comboTimeUnit: TComboBox
          Left = 570
          Top = 224
          Width = 121
          Height = 26
          Style = csDropDownList
          TabOrder = 6
        end
      end
    end
    object tabInitialGrid: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Initial_Grid_Dialog_Box'
      Caption = 'tabInitialGrid'
      ImageIndex = 1
      object gbInitialGrid: TGroupBox
        Left = 0
        Top = 0
        Width = 743
        Height = 326
        Align = alClient
        Caption = 'Specify initial grid (optional)'
        TabOrder = 0
        object lblZDist: TLabel
          Left = 520
          Top = 110
          Width = 185
          Height = 18
          Caption = 'Distance between Z nodes'
        end
        object lblYDist: TLabel
          Left = 520
          Top = 70
          Width = 185
          Height = 18
          Caption = 'Distance between Y nodes'
        end
        object lblXDist: TLabel
          Left = 520
          Top = 30
          Width = 187
          Height = 18
          Caption = 'Distance between X nodes'
        end
        object lblNumNodesZ: TLabel
          Left = 91
          Top = 110
          Width = 259
          Height = 18
          Caption = 'Number of nodes in Z (layer) direction'
        end
        object lblNumNodesY: TLabel
          Left = 91
          Top = 70
          Width = 251
          Height = 18
          Caption = 'Number of nodes in Y (row) direction'
        end
        object lblNumNodesX: TLabel
          Left = 91
          Top = 30
          Width = 277
          Height = 18
          Caption = 'Number of nodes in X (column) direction'
        end
        object rdeLayerHeight: TRbwDataEntry
          Left = 440
          Top = 106
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 5
          Text = '5'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeRowWidth: TRbwDataEntry
          Left = 440
          Top = 66
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 3
          Text = '100'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeColWidth: TRbwDataEntry
          Left = 440
          Top = 26
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 1
          Text = '100'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeNLay: TRbwDataEntry
          Left = 11
          Top = 106
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 4
          Text = '5'
          DataType = dtInteger
          Max = 2.000000000000000000
          Min = 2.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeNRow: TRbwDataEntry
          Left = 11
          Top = 66
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 2
          Text = '10'
          DataType = dtInteger
          Max = 2.000000000000000000
          Min = 2.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeNCol: TRbwDataEntry
          Left = 11
          Top = 26
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 0
          Text = '10'
          DataType = dtInteger
          Max = 2.000000000000000000
          Min = 2.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        inline frameInitialGridPosition: TframeInitialGridPosition
          Left = 3
          Top = 140
          Width = 407
          Height = 150
          TabOrder = 6
          TabStop = True
          ExplicitLeft = 3
          ExplicitTop = 140
          ExplicitWidth = 407
          inherited lblGridAngle: TLabel
            Width = 148
            Height = 18
            ExplicitWidth = 148
            ExplicitHeight = 18
          end
          inherited lblVerticalExaggeration: TLabel
            Width = 147
            Height = 18
            ExplicitWidth = 147
            ExplicitHeight = 18
          end
          inherited lblGridOrigin: TLabel
            Width = 77
            Height = 18
            ExplicitWidth = 77
            ExplicitHeight = 18
          end
          inherited lblOriginX: TLabel
            Width = 11
            Height = 18
            ExplicitWidth = 11
            ExplicitHeight = 18
          end
          inherited lblOriginY: TLabel
            Width = 9
            Height = 18
            ExplicitWidth = 9
            ExplicitHeight = 18
          end
          inherited lblOriginZ: TLabel
            Width = 9
            Height = 18
            ExplicitWidth = 9
            ExplicitHeight = 18
          end
          inherited rdeExaggeration: TRbwDataEntry
            Text = ''
          end
        end
      end
    end
    object tabInitialModflowGrid: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Initial_Grid_Dialog_Box'
      Caption = 'tabInitialModflowGrid'
      ImageIndex = 2
      object gbInitialGridModflow: TGroupBox
        Left = 0
        Top = 41
        Width = 743
        Height = 285
        Margins.Left = 0
        Margins.Top = 40
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        Caption = 'Specify initial grid (optional)'
        TabOrder = 0
        object Label8: TLabel
          Left = 344
          Top = 68
          Width = 71
          Height = 18
          Caption = 'Row width'
        end
        object Label9: TLabel
          Left = 344
          Top = 28
          Width = 93
          Height = 18
          Caption = 'Column width'
        end
        object Label10: TLabel
          Left = 91
          Top = 108
          Width = 117
          Height = 18
          Caption = 'Number of layers'
        end
        object Label11: TLabel
          Left = 91
          Top = 68
          Width = 109
          Height = 18
          Caption = 'Number of rows'
        end
        object Label12: TLabel
          Left = 91
          Top = 28
          Width = 133
          Height = 18
          Caption = 'Number of columns'
        end
        object rdeModflowRowWidth: TRbwDataEntry
          Left = 264
          Top = 64
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 4
          Text = '100'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeModflowColWidth: TRbwDataEntry
          Left = 264
          Top = 24
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 2
          Text = '100'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeModflowLayerCount: TRbwDataEntry
          Left = 11
          Top = 104
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 5
          Text = '3'
          OnChange = rdeModflowLayerCountChange
          DataType = dtInteger
          Max = 1.000000000000000000
          Min = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeModflowRowCount: TRbwDataEntry
          Left = 11
          Top = 64
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 3
          Text = '10'
          DataType = dtInteger
          Max = 1.000000000000000000
          Min = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeModflowColumnCount: TRbwDataEntry
          Left = 11
          Top = 24
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 1
          Text = '10'
          DataType = dtInteger
          Max = 1.000000000000000000
          Min = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdgInitialLayers: TRbwDataGrid4
          Left = 455
          Top = 20
          Width = 286
          Height = 263
          Align = alRight
          Anchors = [akLeft, akTop, akRight, akBottom]
          ColCount = 2
          FixedCols = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs]
          TabOrder = 0
          OnSelectCell = rdgInitialLayersSelectCell
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
              WordWrapCaptions = False
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
          OnEndUpdate = rdgInitialLayersEndUpdate
          WordWrapRowCaptions = False
          ColWidths = (
            64
            64)
          RowHeights = (
            24
            24
            24
            24
            24)
        end
      end
      object pnlModflowChoice: TPanel
        Left = 0
        Top = 0
        Width = 743
        Height = 41
        Align = alTop
        TabOrder = 1
        object lblModflowSelection: TLabel
          Left = 175
          Top = 10
          Width = 140
          Height = 18
          Caption = 'MODFLOW Version'
        end
        object comboModflowSelection: TComboBox
          Left = 11
          Top = 7
          Width = 158
          Height = 26
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 0
          Text = 'MODFLOW 6'
          Items.Strings = (
            'MODFLOW 6'
            'MODFLOW-2005'
            'MODFLOW-NWT'
            'MODFLOW-OWHM'
            'MODFLOW-CFP'
            'MODFLOW-LGR')
        end
      end
    end
    object tabInitialSutraMesh: TTabSheet
      Caption = 'tabInitialSutraMesh'
      ImageIndex = 3
      DesignSize = (
        743
        326)
      object lblLayerGroups: TLabel
        Left = 464
        Top = 8
        Width = 91
        Height = 18
        Caption = 'Layer groups'
      end
      object lblModelPosition: TLabel
        Left = 6
        Top = 179
        Width = 102
        Height = 18
        Caption = 'Model position'
      end
      object lblMinimumThickness: TLabel
        Left = 6
        Top = 101
        Width = 132
        Height = 18
        Caption = 'Minimum thickness'
      end
      object rgMeshType: TRadioGroup
        Left = 6
        Top = 8
        Width = 204
        Height = 87
        Caption = 'Mesh type (MSHSTR)'
        ItemIndex = 0
        Items.Strings = (
          '2D areal'
          '2D profile'
          '3D')
        TabOrder = 0
        OnClick = rgMeshTypeClick
      end
      inline frameModelLayers: TframeGrid
        Left = 464
        Top = 29
        Width = 269
        Height = 294
        Anchors = [akLeft, akTop, akRight, akBottom]
        Enabled = False
        TabOrder = 2
        ExplicitLeft = 464
        ExplicitTop = 29
        ExplicitWidth = 269
        ExplicitHeight = 294
        inherited Panel: TPanel
          Top = 229
          Width = 269
          Height = 65
          ExplicitTop = 229
          ExplicitWidth = 269
          ExplicitHeight = 65
          inherited lbNumber: TLabel
            Width = 161
            Height = 18
            Caption = 'Number of layer groups'
            ExplicitWidth = 161
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 6
            Top = 38
            OnClick = frameModelLayerssbAddClick
            ExplicitLeft = 8
            ExplicitTop = 38
          end
          inherited sbInsert: TSpeedButton
            Left = 32
            Top = 38
            OnClick = frameModelLayerssbInsertClick
            ExplicitLeft = 37
            ExplicitTop = 38
          end
          inherited sbDelete: TSpeedButton
            Left = 58
            Top = 38
            OnClick = frameModelLayerssbDeleteClick
            ExplicitLeft = 66
            ExplicitTop = 38
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            Value = 3.000000000000000000
            OnChange = frameModelLayersseNumberChange
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 269
          Height = 229
          ColCount = 2
          RowCount = 5
          OnSelectCell = frameModelLayersGridSelectCell
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
              ButtonFont.Height = -13
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
          OnEndUpdate = frameModelLayersGridEndUpdate
          ExplicitWidth = 269
          ExplicitHeight = 229
          ColWidths = (
            64
            64)
          RowHeights = (
            24
            24
            24
            24
            24)
        end
      end
      object rgTransport: TRadioGroup
        Left = 216
        Top = 8
        Width = 230
        Height = 87
        Caption = 'Transport (SIMULA)'
        ItemIndex = 1
        Items.Strings = (
          'Solute using pressure'
          'Solute using head'
          'Energy')
        TabOrder = 1
        OnClick = rgTransportClick
      end
      object rdgLocation: TRbwDataGrid4
        Left = 6
        Top = 203
        Width = 320
        Height = 117
        ColCount = 3
        FixedCols = 1
        RowCount = 3
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 5
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnBeforeDrawCell = rdgLocationBeforeDrawCell
        ColorRangeSelection = False
        ColorSelectedRow = False
        Columns = <
          item
            AutoAdjustRowHeights = False
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = False
          end
          item
            AutoAdjustRowHeights = False
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = False
          end
          item
            AutoAdjustRowHeights = False
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
        WordWrapRowCaptions = False
        ColWidths = (
          64
          64
          64)
        RowHeights = (
          24
          24
          24)
      end
      object rdeMinimumThickness: TRbwDataEntry
        Left = 6
        Top = 125
        Width = 100
        Height = 22
        TabOrder = 4
        Text = '1e-3'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rgSaturation: TRadioGroup
        Left = 216
        Top = 101
        Width = 230
        Height = 96
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Flow conditions (CUNSAT)'
        Enabled = False
        ItemIndex = 0
        Items.Strings = (
          'Saturated'
          'Unsaturated')
        TabOrder = 3
      end
    end
    object tabInitialGridFootprint: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Initial_Grid_Dialog_Box_for_Fo'
      Caption = 'tabInitialGridFootprint'
      ImageIndex = 4
      object gbFootprint: TGroupBox
        Left = 0
        Top = 0
        Width = 743
        Height = 326
        Align = alClient
        Caption = 'Specify initial grid (optional)'
        TabOrder = 0
        object lblRowCountFootprint: TLabel
          Left = 91
          Top = 68
          Width = 109
          Height = 18
          Caption = 'Number of rows'
        end
        object lblColumnCountFootprint: TLabel
          Left = 91
          Top = 28
          Width = 133
          Height = 18
          Caption = 'Number of columns'
        end
        object lblCellSizeFootprint: TLabel
          Left = 344
          Top = 28
          Width = 62
          Height = 18
          Caption = 'Cell Size'
        end
        object rdeColumnCountFootprint: TRbwDataEntry
          Left = 11
          Top = 24
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 0
          Text = '10'
          DataType = dtInteger
          Max = 1.000000000000000000
          Min = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeRowCountFootprint: TRbwDataEntry
          Left = 11
          Top = 64
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 2
          Text = '10'
          DataType = dtInteger
          Max = 1.000000000000000000
          Min = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeCellSizeFootprint: TRbwDataEntry
          Left = 264
          Top = 24
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 1
          Text = '100'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
      end
    end
  end
end
