inherited frmScreenObjectProperties: TfrmScreenObjectProperties
  Left = 354
  Top = 158
  HelpType = htKeyword
  HelpKeyword = 'Object_Properties_Dialog_Box'
  ActiveControl = pageMain
  Caption = 'Object Properties'
  ClientHeight = 557
  ClientWidth = 740
  Font.Height = 19
  KeyPreview = True
  StyleElements = [seFont, seClient, seBorder]
  OnClose = FormClose
  OnKeyUp = FormKeyUp
  ExplicitWidth = 756
  ExplicitHeight = 596
  TextHeight = 19
  object pageMain: TPageControl
    Left = 0
    Top = 0
    Width = 740
    Height = 516
    ActivePage = tabModflowBoundaryConditions
    Align = alClient
    TabHeight = 28
    TabOrder = 0
    OnChange = pageMainChange
    object tabProperties: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Properties_Tab'
      Caption = 'Properties'
      DesignSize = (
        732
        478)
      object shpFillColor: TShape
        Left = 457
        Top = 215
        Width = 25
        Height = 21
      end
      object shpLineColor: TShape
        Left = 457
        Top = 196
        Width = 25
        Height = 3
      end
      object lblName: TLabel
        Left = 8
        Top = 56
        Width = 42
        Height = 19
        Caption = 'Name'
        FocusControl = edName
      end
      object lblZ: TLabel
        Left = 8
        Top = 384
        Width = 92
        Height = 19
        Caption = 'Z-coordinate'
        Enabled = False
      end
      object lblHighZ: TLabel
        Left = 8
        Top = 417
        Width = 145
        Height = 19
        Caption = 'Higher Z-coordinate'
        Enabled = False
      end
      object lblLowZ: TLabel
        Left = 8
        Top = 450
        Width = 141
        Height = 19
        Caption = 'Lower Z-coordinate'
        Enabled = False
      end
      object lblGridCellSize: TLabel
        Left = 8
        Top = 141
        Width = 126
        Height = 19
        Caption = 'Grid element size'
        Enabled = False
      end
      object lblMinimumCellFraction: TLabel
        Left = 296
        Top = 248
        Width = 130
        Height = 38
        Caption = 'Minimum fraction of cell length'
        WordWrap = True
      end
      object lblQuadTreeRefinement: TLabel
        Left = 289
        Top = 91
        Width = 149
        Height = 19
        Caption = 'Quadtree refinement'
      end
      object btnLineColor: TButton
        Left = 226
        Top = 189
        Width = 225
        Height = 23
        Caption = 'Set object line color'
        TabOrder = 8
        OnClick = btnColorClick
      end
      object btnFillColor: TButton
        Left = 226
        Top = 216
        Width = 225
        Height = 23
        Caption = 'Set object fill color'
        TabOrder = 10
        OnClick = btnColorClick
      end
      object cbIntersectedCells: TCheckBox
        Left = 8
        Top = 272
        Width = 282
        Height = 24
        AllowGrayed = True
        Caption = 'Set values of intersected cells'
        TabOrder = 13
        OnClick = cbIntersectedCellsClick
      end
      object cbEnclosedCells: TCheckBox
        Left = 8
        Top = 249
        Width = 265
        Height = 24
        AllowGrayed = True
        Caption = 'Set values of enclosed cells'
        TabOrder = 11
        OnClick = cbEnclosedCellsClick
      end
      object cbFillColor: TCheckBox
        Left = 8
        Top = 216
        Width = 209
        Height = 23
        AllowGrayed = True
        Caption = 'Color object interior'
        TabOrder = 9
        OnClick = cbFillColorClick
      end
      object cbLineColor: TCheckBox
        Left = 8
        Top = 191
        Width = 209
        Height = 19
        AllowGrayed = True
        Caption = 'Color object line'
        TabOrder = 7
        OnClick = cbLineColorClick
      end
      object edName: TEdit
        Left = 72
        Top = 55
        Width = 377
        Height = 27
        Cursor = crIBeam
        TabOrder = 2
        OnExit = edNameExit
      end
      object edHighZ: TRbwEdit
        Left = 201
        Top = 414
        Width = 264
        Height = 27
        Cursor = crIBeam
        Anchors = [akLeft, akTop, akRight]
        Color = clBtnFace
        Enabled = False
        TabOrder = 19
        Text = '0'
        OnExit = edHighZExit
      end
      object edLowZ: TRbwEdit
        Left = 201
        Top = 447
        Width = 264
        Height = 27
        Cursor = crIBeam
        Anchors = [akLeft, akTop, akRight]
        Color = clBtnFace
        Enabled = False
        TabOrder = 22
        Text = '0'
        OnExit = edLowZExit
      end
      object btnZ: TButton
        Left = 471
        Top = 380
        Width = 90
        Height = 30
        Anchors = [akTop, akRight]
        Caption = 'Edit F()...'
        Enabled = False
        TabOrder = 16
        OnClick = btnFormulaClick
      end
      object btnHighZ: TButton
        Left = 471
        Top = 415
        Width = 90
        Height = 30
        Anchors = [akTop, akRight]
        Caption = 'Edit F()...'
        Enabled = False
        TabOrder = 18
        OnClick = btnFormulaClick
      end
      object btnLowZ: TButton
        Left = 471
        Top = 448
        Width = 90
        Height = 30
        Anchors = [akTop, akRight]
        Caption = 'Edit F()...'
        Enabled = False
        TabOrder = 20
        OnClick = btnFormulaClick
      end
      object cbInterpolation: TCheckBox
        Left = 8
        Top = 296
        Width = 481
        Height = 24
        AllowGrayed = True
        Caption = 'Set values of cells by interpolation'
        TabOrder = 14
        OnClick = cbInterpolationClick
      end
      object rdeGridCellSize: TRbwDataEntry
        Left = 210
        Top = 138
        Width = 137
        Height = 30
        Cursor = crIBeam
        Color = clBtnFace
        Enabled = False
        TabOrder = 6
        Text = '1'
        OnExit = rdeGridCellSizeExit
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rgElevationCount: TRadioGroup
        Left = 8
        Top = 326
        Width = 337
        Height = 49
        Caption = 'Number of Z formulas'
        Columns = 3
        ItemIndex = 0
        Items.Strings = (
          'Zero'
          'One'
          'Two')
        TabOrder = 15
        OnClick = rgElevationCountClick
      end
      object cbSetGridCellSize: TCheckBox
        Left = 8
        Top = 115
        Width = 281
        Height = 17
        AllowGrayed = True
        Caption = 'Use to set grid element size'
        TabOrder = 5
        OnClick = cbSetGridCellSizeClick
      end
      object rgEvaluatedAt: TRadioGroup
        Left = 8
        Top = 0
        Width = 297
        Height = 49
        Caption = 'Evaluated at'
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Elements'
          'Nodes')
        TabOrder = 0
        OnClick = rgEvaluatedAtClick
      end
      object edZ: TRbwEdit
        Left = 201
        Top = 381
        Width = 264
        Height = 27
        Anchors = [akLeft, akTop, akRight]
        Color = clBtnFace
        Enabled = False
        TabOrder = 17
        Text = '0'
        OnExit = edZExit
      end
      object jvplObjectInfo: TJvPageList
        Left = 487
        Top = 16
        Width = 274
        Height = 329
        ActivePage = jvspSingleObject
        PropagateEnable = False
        object jvspSingleObject: TJvStandardPage
          Left = 0
          Top = 0
          Width = 274
          Height = 329
          Caption = 'jvspSingleObject'
          object gbObjectInfo: TGroupBox
            Left = 0
            Top = 0
            Width = 274
            Height = 329
            Align = alClient
            Caption = 'Object information (not editable)'
            TabOrder = 0
            object lblObjectLength: TLabel
              Left = 16
              Top = 27
              Width = 94
              Height = 19
              Caption = 'Object length'
            end
            object lblObjectArea: TLabel
              Left = 16
              Top = 82
              Width = 84
              Height = 19
              Caption = 'Object area'
            end
            object lblObjectOrder: TLabel
              Left = 16
              Top = 137
              Width = 90
              Height = 19
              Caption = 'Object order'
            end
            object edObjectLength: TEdit
              Left = 16
              Top = 49
              Width = 145
              Height = 27
              ReadOnly = True
              TabOrder = 0
              Text = 'edObjectLength'
            end
            object edObjectArea: TEdit
              Left = 16
              Top = 104
              Width = 145
              Height = 27
              ReadOnly = True
              TabOrder = 1
              Text = 'edObjectLength'
            end
            object edObjectOrder: TEdit
              Left = 16
              Top = 159
              Width = 145
              Height = 27
              ReadOnly = True
              TabOrder = 2
              Text = 'edObjectLength'
            end
          end
        end
        object jvspMultipleObjects: TJvStandardPage
          Left = 0
          Top = 0
          Width = 274
          Height = 329
          Caption = 'jvspMultipleObjects'
          object lblNames: TLabel
            Left = 9
            Top = 3
            Width = 190
            Height = 19
            Caption = 'Names of selected objects'
          end
          object memoNames: TMemo
            Left = 9
            Top = 25
            Width = 257
            Height = 262
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
          end
        end
      end
      object cbLock: TCheckBox
        Left = 320
        Top = 16
        Width = 153
        Height = 17
        AllowGrayed = True
        Caption = 'Position locked'
        TabOrder = 1
        OnClick = cbLockClick
      end
      object cbDuplicatesAllowed: TCheckBox
        Left = 8
        Top = 88
        Width = 225
        Height = 17
        AllowGrayed = True
        Caption = 'Duplicates allowed'
        TabOrder = 3
        OnClick = cbDuplicatesAllowedClick
      end
      object rdeMinimumCellFraction: TRbwDataEntry
        Left = 296
        Top = 292
        Width = 145
        Height = 22
        TabOrder = 12
        Text = '0'
        OnChange = rdeMinimumCellFractionChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeQuadTreeRefinement: TRbwDataEntry
        Left = 233
        Top = 88
        Width = 50
        Height = 22
        TabOrder = 4
        Text = '0'
        DataType = dtInteger
        Max = 6.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object tabLGR: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Footprint_Well_Tab'
      Caption = 'LGR'
      ImageIndex = 7
      object Splitter1: TSplitter
        Left = 0
        Top = 246
        Width = 732
        Height = 5
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 245
        ExplicitWidth = 778
      end
      object pnlLgrTop: TPanel
        Left = 0
        Top = 0
        Width = 732
        Height = 246
        Align = alClient
        TabOrder = 0
        object lblObjectUsedWithModels: TLabel
          Left = 4
          Top = 8
          Width = 115
          Height = 19
          Caption = 'Models affected'
        end
        object cbLgrAllModels: TCheckBox
          Left = 16
          Top = 32
          Width = 97
          Height = 17
          Caption = 'All models'
          TabOrder = 0
          OnClick = cbLgrAllModelsClick
        end
        object clbLgrUsedModels: TCheckListBox
          Left = 1
          Top = 56
          Width = 730
          Height = 189
          Align = alBottom
          Anchors = [akLeft, akTop, akRight, akBottom]
          ItemHeight = 19
          TabOrder = 1
          OnClickCheck = clbLgrUsedModelsClickCheck
        end
      end
      object pnlLgrBottom: TPanel
        Left = 0
        Top = 251
        Width = 732
        Height = 227
        Align = alBottom
        TabOrder = 1
        object lblLgrChildModel: TLabel
          Left = 4
          Top = 8
          Width = 357
          Height = 19
          Caption = 'Define boundary of child model (2D objects only)'
        end
        object clbChildModels: TJvxCheckListBox
          AlignWithMargins = True
          Left = 4
          Top = 40
          Width = 724
          Height = 183
          CheckKind = ckRadioButtons
          Align = alBottom
          Anchors = [akLeft, akTop, akRight, akBottom]
          ItemHeight = 19
          TabOrder = 0
          OnClickCheck = clbChildModelsClickCheck
          InternalVersion = 202
        end
      end
    end
    object tabDataSets: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Data_Sets_Tab'
      Caption = 'Data Sets'
      ImageIndex = 2
      object gbPhastInterpolation: TGroupBox
        Left = 0
        Top = 302
        Width = 732
        Height = 176
        Align = alBottom
        Caption = 'PHAST-style interpolation'
        TabOrder = 1
        DesignSize = (
          732
          176)
        inline framePhastInterpolationData: TframePhastInterpolation
          Left = 3
          Top = 30
          Width = 630
          Height = 143
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          TabStop = True
          ExplicitLeft = 3
          ExplicitTop = 30
          ExplicitWidth = 630
          ExplicitHeight = 143
          inherited lblDistance1: TLabel
            Top = 68
            Width = 78
            Height = 19
            StyleElements = [seFont, seClient, seBorder]
            ExplicitTop = 68
            ExplicitWidth = 78
            ExplicitHeight = 19
          end
          inherited lblDistance2: TLabel
            Top = 108
            Width = 78
            Height = 19
            StyleElements = [seFont, seClient, seBorder]
            ExplicitTop = 108
            ExplicitWidth = 78
            ExplicitHeight = 19
          end
          inherited lblValue1: TLabel
            Top = 68
            Width = 54
            Height = 19
            StyleElements = [seFont, seClient, seBorder]
            ExplicitTop = 68
            ExplicitWidth = 54
            ExplicitHeight = 19
          end
          inherited lblValue2: TLabel
            Top = 108
            Width = 54
            Height = 19
            StyleElements = [seFont, seClient, seBorder]
            ExplicitTop = 108
            ExplicitWidth = 54
            ExplicitHeight = 19
          end
          inherited lblMixtureFormula: TLabel
            Top = 68
            Width = 111
            Height = 19
            StyleElements = [seFont, seClient, seBorder]
            ExplicitTop = 68
            ExplicitWidth = 111
            ExplicitHeight = 19
          end
          inherited cbPhastInterpolation: TJvCheckBox
            Width = 238
            Height = 19
            OnClick = framePhastInterpolationDatacbPhastInterpolationClick
            HotTrackFont.Charset = ANSI_CHARSET
            HotTrackFont.Height = 19
            HotTrackFont.Name = 'Arial'
            HotTrackFont.Pitch = fpVariable
            ExplicitWidth = 238
            ExplicitHeight = 19
          end
          inherited rdeDistance1: TRbwDataEntry
            Top = 68
            Height = 30
            TabOrder = 3
            StyleElements = [seFont, seClient, seBorder]
            OnExit = framePhastInterpolationDatardeDistance1Exit
            ExplicitTop = 68
            ExplicitHeight = 30
          end
          inherited rdeDistance2: TRbwDataEntry
            Left = 92
            Top = 104
            Height = 30
            StyleElements = [seFont, seClient, seBorder]
            OnExit = framePhastInterpolationDatardeDistance2Exit
            ExplicitLeft = 92
            ExplicitTop = 104
            ExplicitHeight = 30
          end
          inherited rdeValue1: TRbwDataEntry
            Top = 64
            Height = 30
            TabOrder = 2
            StyleElements = [seFont, seClient, seBorder]
            OnExit = framePhastInterpolationDatardeValue1Exit
            ExplicitTop = 64
            ExplicitHeight = 30
          end
          inherited rdeValue2: TRbwDataEntry
            Top = 104
            Height = 30
            StyleElements = [seFont, seClient, seBorder]
            OnExit = framePhastInterpolationDatardeValue2Exit
            ExplicitTop = 104
            ExplicitHeight = 30
          end
          inherited rgInterpolationDirection: TRadioGroup
            Left = 392
            Width = 297
            Height = 57
            OnClick = framePhastInterpolationDatargInterpolationDirectionClick
            ExplicitLeft = 392
            ExplicitWidth = 297
            ExplicitHeight = 57
          end
          inherited edMixFormula: TRbwEdit
            Top = 104
            Width = 134
            Height = 27
            Anchors = [akLeft, akTop, akRight]
            StyleElements = [seFont, seClient, seBorder]
            OnEnter = framePhastInterpolationDataedMixFormulaEnter
            OnExit = framePhastInterpolationDataedMixFormulaExit
            ExplicitTop = 104
            ExplicitWidth = 134
            ExplicitHeight = 27
          end
          inherited btnEditMixtureFormula: TButton
            Left = 532
            Top = 104
            Height = 30
            Anchors = [akTop, akRight]
            OnClick = framePhastInterpolationDatabtnEditMixtureFormulaClick
            ExplicitLeft = 532
            ExplicitTop = 104
            ExplicitHeight = 30
          end
        end
      end
      object pnlDataSets: TPanel
        Left = 0
        Top = 0
        Width = 732
        Height = 302
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object JvNetscapeSplitter2: TJvNetscapeSplitter
          Left = 285
          Top = 0
          Height = 302
          Align = alLeft
          MinSize = 1
          Maximized = False
          Minimized = False
          ButtonCursor = crDefault
          ExplicitLeft = 216
          ExplicitTop = 192
          ExplicitHeight = 100
        end
        object Panel1: TPanel
          Left = 295
          Top = 0
          Width = 437
          Height = 302
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
          DesignSize = (
            437
            302)
          object lblDataSetFormula: TLabel
            Left = 107
            Top = 5
            Width = 59
            Height = 19
            Caption = 'Formula'
          end
          object btnDataSetFormula: TButton
            Left = 6
            Top = 6
            Width = 89
            Height = 25
            Caption = 'Edit F()...'
            Enabled = False
            TabOrder = 0
            WordWrap = True
            OnClick = btnDataSetFormulaClick
          end
          object Panel3: TPanel
            Left = 0
            Top = 184
            Width = 485
            Height = 116
            Anchors = [akLeft, akBottom]
            BevelOuter = bvNone
            TabOrder = 2
            object lblDataComment: TLabel
              Left = 10
              Top = 8
              Width = 131
              Height = 19
              Caption = 'Data set comment'
            end
            object lblAssociatedModelDataSets: TLabel
              Left = 239
              Top = 7
              Width = 164
              Height = 19
              Caption = 'Associated model data'
            end
            object reDataSetComment: TRichEdit
              Left = 10
              Top = 30
              Width = 223
              Height = 80
              ReadOnly = True
              ScrollBars = ssVertical
              TabOrder = 0
            end
            object reAssocModDataSets: TRichEdit
              Left = 239
              Top = 30
              Width = 210
              Height = 80
              ReadOnly = True
              ScrollBars = ssVertical
              TabOrder = 1
            end
          end
          object reDataSetFormula: TRichEdit
            Left = 6
            Top = 37
            Width = 399
            Height = 149
            Anchors = [akLeft, akTop, akRight, akBottom]
            Enabled = False
            TabOrder = 1
            OnChange = reDataSetFormulaChange
            OnEnter = reDataSetFormulaEnter
            OnExit = reDataSetFormulaExit
          end
        end
        object Panel2: TPanel
          Left = 0
          Top = 0
          Width = 285
          Height = 302
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 0
          object tvDataSets: TTreeView
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 279
            Height = 220
            Margins.Bottom = 6
            Align = alClient
            HideSelection = False
            Indent = 21
            ReadOnly = True
            StateImages = ilCheckImages
            TabOrder = 0
            OnChange = tvDataSetsChange
            OnMouseDown = tvDataSetsMouseDown
          end
          object pnl1: TPanel
            Left = 0
            Top = 229
            Width = 285
            Height = 73
            Align = alBottom
            TabOrder = 1
            object lblLayerElevationExplaination: TLabel
              AlignWithMargins = True
              Left = 4
              Top = 4
              Width = 277
              Height = 65
              Align = alClient
              Caption = 
                'Layer elevations can only be specified using objects with Zero Z' +
                ' formulas.'
              WordWrap = True
              ExplicitWidth = 256
              ExplicitHeight = 57
            end
          end
        end
      end
    end
    object tabBoundaries: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'PHAST_Boundary_Conditions_Tab'
      Caption = 'PHAST Boundary Conditions'
      ImageIndex = 3
      object pcPhastBoundaries: TJvPageList
        Left = 0
        Top = 201
        Width = 732
        Height = 277
        ActivePage = tabBoundaryWell
        PropagateEnable = False
        Align = alClient
        OnChange = pcPhastBoundariesChange
        object tabBoundaryNone: TJvStandardPage
          Left = 0
          Top = 0
          Width = 732
          Height = 277
          Caption = 'None'
        end
        object tabBoundarySpecifiedHead: TJvStandardPage
          Left = 0
          Top = 0
          Width = 732
          Height = 277
          Caption = 'Specified Head'
          object pnlSolutionType: TPanel
            Left = 0
            Top = 0
            Width = 732
            Height = 81
            Align = alTop
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 0
            object lblSolutionType: TLabel
              Left = 16
              Top = 6
              Width = 114
              Height = 19
              Caption = 'Type of solution'
            end
            object comboSolutionType: TComboBox
              Left = 16
              Top = 40
              Width = 273
              Height = 27
              Style = csDropDownList
              ItemIndex = 0
              TabOrder = 0
              Text = 'Associated solution'
              OnChange = comboSolutionTypeChange
              Items.Strings = (
                'Associated solution'
                'Specified solution')
            end
          end
          object dgSpecifiedHead: TRbwDataGrid4
            Left = 0
            Top = 81
            Width = 732
            Height = 196
            Align = alClient
            ColCount = 6
            DefaultColWidth = 20
            FixedCols = 1
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goTabs, goAlwaysShowEditor]
            TabOrder = 1
            OnDrawCell = dgBoundaryDrawCell
            OnSelectCell = dgBoundarySelectCell
            OnSetEditText = dgBoundarySetEditText
            ExtendedAutoDistributeText = False
            AutoMultiEdit = True
            AutoDistributeText = True
            AutoIncreaseColCount = False
            AutoIncreaseRowCount = True
            SelectedRowOrColumnColor = clAqua
            UnselectableColor = clBtnFace
            OnButtonClick = dgBoundaryButtonClick
            OnStateChange = dgBoundaryStateChanged
            ColorRangeSelection = False
            OnDistributeTextProgress = dgSpecifiedHeadDistributeTextProgress
            Columns = <
              item
                AutoAdjustRowHeights = False
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
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
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
                CheckMax = False
                CheckMin = True
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
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 40
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
                AutoAdjustRowHeights = False
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 25
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
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 40
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
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 25
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
            WordWrapRowCaptions = False
            ColWidths = (
              20
              20
              20
              24
              20
              24)
            RowHeights = (
              24
              24)
          end
        end
        object tabBoundaryFlux: TJvStandardPage
          Left = 0
          Top = 0
          Width = 732
          Height = 277
          Caption = 'Flux'
          object dgBoundaryFlux: TRbwDataGrid4
            Left = 0
            Top = 0
            Width = 732
            Height = 277
            Align = alClient
            ColCount = 6
            DefaultColWidth = 20
            FixedCols = 1
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goTabs, goAlwaysShowEditor]
            TabOrder = 0
            OnDrawCell = dgBoundaryDrawCell
            OnSelectCell = dgBoundarySelectCell
            OnSetEditText = dgBoundarySetEditText
            ExtendedAutoDistributeText = False
            AutoMultiEdit = True
            AutoDistributeText = True
            AutoIncreaseColCount = False
            AutoIncreaseRowCount = True
            SelectedRowOrColumnColor = clAqua
            UnselectableColor = clBtnFace
            OnButtonClick = dgBoundaryButtonClick
            OnStateChange = dgBoundaryStateChanged
            ColorRangeSelection = False
            OnDistributeTextProgress = dgBoundaryFluxDistributeTextProgress
            Columns = <
              item
                AutoAdjustRowHeights = False
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
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
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
                CheckMax = False
                CheckMin = True
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
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 40
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
                AutoAdjustRowHeights = False
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
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
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 40
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
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 25
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
            WordWrapRowCaptions = False
            ColWidths = (
              20
              20
              20
              20
              20
              24)
            RowHeights = (
              24
              24)
          end
        end
        object tabBoundaryLeaky: TJvStandardPage
          Left = 0
          Top = 0
          Width = 732
          Height = 277
          Caption = 'Leaky'
          object pnlLeaky: TPanel
            Left = 0
            Top = 0
            Width = 732
            Height = 105
            Align = alTop
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 0
            DesignSize = (
              732
              105)
            object lblLeakyHydraulicConductivity: TLabel
              Left = 8
              Top = 12
              Width = 160
              Height = 19
              Caption = 'Hydraulic conductivity'
            end
            object lblLeakyThickness: TLabel
              Left = 8
              Top = 60
              Width = 73
              Height = 19
              Caption = 'Thickness'
            end
            object edLeakyHydraulicConductivity: TEdit
              Left = 200
              Top = 8
              Width = 379
              Height = 27
              Cursor = crIBeam
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnExit = edLeakyHydraulicConductivityExit
            end
            object edLeakyThickness: TEdit
              Left = 200
              Top = 56
              Width = 379
              Height = 27
              Cursor = crIBeam
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 2
              OnExit = edLeakyHydraulicConductivityExit
            end
            object btnLeakyHydraulicConductivity: TButton
              Left = 586
              Top = 8
              Width = 94
              Height = 30
              Anchors = [akTop, akRight]
              Caption = 'Edit F()...'
              TabOrder = 1
              OnClick = btnFormulaClick
            end
            object btnLeakyThickness: TButton
              Left = 586
              Top = 56
              Width = 94
              Height = 30
              Anchors = [akTop, akRight]
              Caption = 'Edit F()...'
              TabOrder = 3
              OnClick = btnFormulaClick
            end
          end
          object dgBoundaryLeaky: TRbwDataGrid4
            Left = 0
            Top = 105
            Width = 732
            Height = 172
            Align = alClient
            ColCount = 6
            DefaultColWidth = 20
            FixedCols = 1
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goTabs, goAlwaysShowEditor]
            TabOrder = 1
            OnDrawCell = dgBoundaryDrawCell
            OnSelectCell = dgBoundarySelectCell
            OnSetEditText = dgBoundarySetEditText
            ExtendedAutoDistributeText = False
            AutoMultiEdit = True
            AutoDistributeText = True
            AutoIncreaseColCount = False
            AutoIncreaseRowCount = True
            SelectedRowOrColumnColor = clAqua
            UnselectableColor = clBtnFace
            OnButtonClick = dgBoundaryButtonClick
            OnStateChange = dgBoundaryStateChanged
            ColorRangeSelection = False
            OnDistributeTextProgress = dgBoundaryLeakyDistributeTextProgress
            Columns = <
              item
                AutoAdjustRowHeights = False
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
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
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
                CheckMax = False
                CheckMin = True
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
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 40
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
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
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
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 40
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
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 25
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
            WordWrapRowCaptions = False
            ColWidths = (
              20
              20
              20
              20
              20
              24)
            RowHeights = (
              24
              24)
          end
        end
        object tabBoundaryRiver: TJvStandardPage
          Left = 0
          Top = 0
          Width = 732
          Height = 277
          Caption = 'River'
          object pnlRiver: TPanel
            Left = 0
            Top = 0
            Width = 732
            Height = 129
            Align = alTop
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 0
            object lblRiverHydraulicConductivity: TLabel
              Left = 8
              Top = 52
              Width = 86
              Height = 38
              Caption = 'Hydraulic conductivity'
              WordWrap = True
            end
            object lblRiverWidth: TLabel
              Left = 196
              Top = 64
              Width = 43
              Height = 19
              Caption = 'Width'
            end
            object lblRiverDepth: TLabel
              Left = 384
              Top = 64
              Width = 43
              Height = 19
              Caption = 'Depth'
            end
            object lblRiverBedThickness: TLabel
              Left = 574
              Top = 52
              Width = 68
              Height = 38
              Caption = 'Bed thickness'
              WordWrap = True
            end
            object lblRiverDescripton: TLabel
              Left = 8
              Top = 16
              Width = 42
              Height = 19
              Caption = 'Name'
            end
            object edRiverHydraulicConductivity: TEdit
              Left = 6
              Top = 96
              Width = 184
              Height = 27
              Cursor = crIBeam
              TabOrder = 5
              OnExit = edRiverExit
            end
            object edRiverWidth: TEdit
              Left = 196
              Top = 96
              Width = 184
              Height = 27
              Cursor = crIBeam
              TabOrder = 6
              OnExit = edRiverExit
            end
            object edRiverDepth: TEdit
              Left = 384
              Top = 96
              Width = 184
              Height = 27
              Cursor = crIBeam
              TabOrder = 7
              OnExit = edRiverExit
            end
            object edRiverBedThickness: TEdit
              Left = 574
              Top = 96
              Width = 160
              Height = 27
              Cursor = crIBeam
              TabOrder = 8
              OnExit = edRiverExit
            end
            object edRiverDescripton: TEdit
              Left = 88
              Top = 15
              Width = 394
              Height = 27
              Cursor = crIBeam
              TabOrder = 0
              OnExit = edRiverDescriptonExit
            end
            object btnRiverHydraulicConductivity: TButton
              Left = 98
              Top = 60
              Width = 92
              Height = 30
              Caption = 'Edit F()...'
              TabOrder = 1
              OnClick = btnFormulaClick
            end
            object btnRiverWidth: TButton
              Left = 288
              Top = 60
              Width = 92
              Height = 30
              Caption = 'Edit F()...'
              TabOrder = 2
              OnClick = btnFormulaClick
            end
            object btnRiverDepth: TButton
              Left = 476
              Top = 60
              Width = 92
              Height = 30
              Caption = 'Edit F()...'
              TabOrder = 3
              OnClick = btnFormulaClick
            end
            object btnRiverBedThickness: TButton
              Left = 654
              Top = 60
              Width = 80
              Height = 30
              Caption = 'Edit F()...'
              TabOrder = 4
              OnClick = btnFormulaClick
            end
          end
          object dgBoundaryRiver: TRbwDataGrid4
            Left = 0
            Top = 129
            Width = 732
            Height = 148
            Align = alClient
            ColCount = 4
            DefaultColWidth = 20
            FixedCols = 1
            RowCount = 2
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = 19
            Font.Name = 'Arial'
            Font.Pitch = fpVariable
            Font.Style = []
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goTabs, goAlwaysShowEditor]
            ParentFont = False
            TabOrder = 1
            OnDrawCell = dgBoundaryDrawCell
            OnSelectCell = dgBoundarySelectCell
            OnSetEditText = dgBoundarySetEditText
            ExtendedAutoDistributeText = False
            AutoMultiEdit = True
            AutoDistributeText = True
            AutoIncreaseColCount = False
            AutoIncreaseRowCount = True
            SelectedRowOrColumnColor = clAqua
            UnselectableColor = clBtnFace
            OnButtonClick = dgBoundaryButtonClick
            ColorRangeSelection = False
            OnDistributeTextProgress = dgBoundaryRiverDistributeTextProgress
            Columns = <
              item
                AutoAdjustRowHeights = False
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
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
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
                CheckMax = False
                CheckMin = True
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
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 40
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
                AutoAdjustRowHeights = False
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 40
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
            WordWrapRowCaptions = False
            ColWidths = (
              20
              20
              20
              24)
            RowHeights = (
              24
              24)
          end
        end
        object tabBoundaryWell: TJvStandardPage
          Left = 0
          Top = 0
          Width = 732
          Height = 277
          Caption = 'Well'
          object splitterWell: TSplitter
            Left = 431
            Top = 145
            Width = 5
            Height = 132
            ExplicitLeft = 0
            ExplicitTop = 296
            ExplicitHeight = 767
          end
          object pnlWellBoundary: TPanel
            Left = 0
            Top = 0
            Width = 732
            Height = 145
            Align = alTop
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 0
            DesignSize = (
              732
              145)
            object lblWellDiameter: TLabel
              Left = 8
              Top = 48
              Width = 66
              Height = 19
              Caption = 'Diameter'
            end
            object lblWellLandSurfaceDatum: TLabel
              Left = 8
              Top = 88
              Width = 145
              Height = 19
              Caption = 'Land surface datum'
              Enabled = False
              FocusControl = rdeWellLandSurfaceDatum
            end
            object lblWellIntervalStyle: TLabel
              Left = 306
              Top = 76
              Width = 135
              Height = 19
              Caption = 'Specify interval by'
            end
            object lblWellIntervals: TLabel
              Left = 306
              Top = 113
              Width = 140
              Height = 19
              Caption = 'Number of intervals'
            end
            object lblWellDescription: TLabel
              Left = 8
              Top = 8
              Width = 77
              Height = 19
              Caption = 'Well name'
            end
            object rdeWellDiameter: TRbwDataEntry
              Left = 192
              Top = 48
              Width = 101
              Height = 30
              Cursor = crIBeam
              TabOrder = 2
              Text = '0'
              OnExit = edWellExit
              DataType = dtReal
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
            object rdeWellLandSurfaceDatum: TRbwDataEntry
              Left = 192
              Top = 88
              Width = 101
              Height = 30
              Cursor = crIBeam
              Color = clBtnFace
              Enabled = False
              TabOrder = 4
              Text = '0'
              OnExit = edWellExit
              DataType = dtReal
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
            object cbWellPumpAllocation: TCheckBox
              Left = 306
              Top = 41
              Width = 329
              Height = 31
              Caption = 'Allocate by pressure and mobility'
              TabOrder = 1
              OnClick = cbWellPumpAllocationClick
            end
            object comboWellIntervalStyle: TComboBox
              Left = 490
              Top = 73
              Width = 105
              Height = 27
              Style = csDropDownList
              ItemIndex = 0
              TabOrder = 3
              Text = 'Elevation'
              OnChange = comboWellIntervalStyleChange
              Items.Strings = (
                'Elevation'
                'Depth')
            end
            object seWellIntervals: TJvSpinEdit
              Left = 490
              Top = 106
              Width = 105
              Height = 27
              ButtonKind = bkClassic
              MaxValue = 2147483647.000000000000000000
              MinValue = 1.000000000000000000
              Value = 1.000000000000000000
              TabOrder = 5
              OnChange = seWellIntervalsChange
            end
            object edWellDescription: TEdit
              Left = 104
              Top = 8
              Width = 208
              Height = 27
              Cursor = crIBeam
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnExit = edWellExit
            end
          end
          object dgWellElevations: TRbwDataGrid4
            Left = 436
            Top = 145
            Width = 296
            Height = 132
            Align = alClient
            ColCount = 3
            DefaultColWidth = 20
            FixedCols = 1
            RowCount = 2
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = 19
            Font.Name = 'Arial'
            Font.Pitch = fpVariable
            Font.Style = []
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowMoving, goEditing, goTabs, goAlwaysShowEditor]
            ParentFont = False
            TabOrder = 2
            OnSetEditText = dgWellElevationsSetEditText
            ExtendedAutoDistributeText = False
            AutoMultiEdit = True
            AutoDistributeText = True
            AutoIncreaseColCount = False
            AutoIncreaseRowCount = True
            SelectedRowOrColumnColor = clAqua
            UnselectableColor = clBtnFace
            ColorRangeSelection = False
            OnDistributeTextProgress = dgWellElevationsDistributeTextProgress
            Columns = <
              item
                AutoAdjustRowHeights = False
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 25
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
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
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
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
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
            WordWrapRowCaptions = False
            ColWidths = (
              20
              20
              20)
            RowHeights = (
              24
              24)
          end
          object dgWell: TRbwDataGrid4
            Left = 0
            Top = 145
            Width = 431
            Height = 132
            Align = alLeft
            ColCount = 4
            DefaultColWidth = 20
            FixedCols = 1
            RowCount = 2
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = 19
            Font.Name = 'Arial'
            Font.Pitch = fpVariable
            Font.Style = []
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goTabs, goAlwaysShowEditor]
            ParentFont = False
            TabOrder = 1
            OnDrawCell = dgBoundaryDrawCell
            OnSelectCell = dgBoundarySelectCell
            OnSetEditText = dgBoundarySetEditText
            ExtendedAutoDistributeText = False
            AutoMultiEdit = True
            AutoDistributeText = True
            AutoIncreaseColCount = False
            AutoIncreaseRowCount = True
            SelectedRowOrColumnColor = clAqua
            UnselectableColor = clBtnFace
            OnButtonClick = dgBoundaryButtonClick
            ColorRangeSelection = False
            OnDistributeTextProgress = dgWellDistributeTextProgress
            Columns = <
              item
                AutoAdjustRowHeights = False
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
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
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
                CheckMax = False
                CheckMin = True
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
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 40
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
                AutoAdjustRowHeights = False
                AutoAdjustCaptionRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 40
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
            WordWrapRowCaptions = False
            ColWidths = (
              20
              20
              20
              24)
            RowHeights = (
              24
              24)
          end
        end
      end
      object pnlBoundaries: TPanel
        Left = 0
        Top = 0
        Width = 732
        Height = 201
        HelpType = htKeyword
        Align = alTop
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        object lblBoundaryTimes: TLabel
          Left = 0
          Top = 172
          Width = 119
          Height = 19
          Caption = 'Number of times'
        end
        object rgBoundaryType: TRadioGroup
          Left = 0
          Top = -3
          Width = 267
          Height = 166
          Caption = 'Boundary type'
          ItemIndex = 0
          Items.Strings = (
            'None'
            'Specified head'
            'Flux boundary'
            'Leaky boundary'
            'River boundary'
            'Well boundary')
          TabOrder = 0
          OnClick = rgBoundaryTypeClick
        end
        object gbBoundaryPhastInterpolation: TGroupBox
          Left = 273
          Top = 0
          Width = 488
          Height = 169
          TabOrder = 1
          inline framePhastInterpolationBoundaries: TframePhastInterpolation
            Left = 4
            Top = 16
            Width = 469
            Height = 145
            TabOrder = 0
            TabStop = True
            OnExit = framePhastInterpolationBoundariesExit
            ExplicitLeft = 4
            ExplicitTop = 16
            ExplicitWidth = 469
            ExplicitHeight = 145
            inherited lblDistance1: TLabel
              Top = 84
              Width = 78
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 84
              ExplicitWidth = 78
              ExplicitHeight = 19
            end
            inherited lblDistance2: TLabel
              Left = 115
              Top = 84
              Width = 78
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitLeft = 115
              ExplicitTop = 84
              ExplicitWidth = 78
              ExplicitHeight = 19
            end
            inherited lblValue1: TLabel
              Left = 224
              Top = 84
              Width = 54
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitLeft = 224
              ExplicitTop = 84
              ExplicitWidth = 54
              ExplicitHeight = 19
            end
            inherited lblValue2: TLabel
              Left = 336
              Top = 84
              Width = 54
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitLeft = 336
              ExplicitTop = 84
              ExplicitWidth = 54
              ExplicitHeight = 19
            end
            inherited lblMixtureFormula: TLabel
              Left = 256
              Top = 3
              Width = 111
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitLeft = 256
              ExplicitTop = 3
              ExplicitWidth = 111
              ExplicitHeight = 19
            end
            inherited cbPhastInterpolation: TJvCheckBox
              Left = 16
              Top = 65
              Width = 238
              Height = 19
              TabOrder = 3
              Visible = False
              OnClick = framePhastInterpolationBoundariescbPhastInterpolationClick
              HotTrackFont.Charset = ANSI_CHARSET
              HotTrackFont.Height = 19
              HotTrackFont.Name = 'Arial'
              HotTrackFont.Pitch = fpVariable
              ExplicitLeft = 16
              ExplicitTop = 65
              ExplicitWidth = 238
              ExplicitHeight = 19
            end
            inherited rdeDistance1: TRbwDataEntry
              Left = 8
              Top = 109
              Height = 30
              TabOrder = 4
              StyleElements = [seFont, seClient, seBorder]
              OnExit = framePhastInterpolationBoundariesrdeDistance1Exit
              ExplicitLeft = 8
              ExplicitTop = 109
              ExplicitHeight = 30
            end
            inherited rdeDistance2: TRbwDataEntry
              Left = 115
              Top = 109
              Height = 30
              TabOrder = 5
              StyleElements = [seFont, seClient, seBorder]
              OnExit = framePhastInterpolationBoundariesrdeDistance2Exit
              ExplicitLeft = 115
              ExplicitTop = 109
              ExplicitHeight = 30
            end
            inherited rdeValue1: TRbwDataEntry
              Left = 224
              Top = 109
              Height = 30
              TabOrder = 6
              StyleElements = [seFont, seClient, seBorder]
              OnExit = framePhastInterpolationBoundariesrdeValue1Exit
              ExplicitLeft = 224
              ExplicitTop = 109
              ExplicitHeight = 30
            end
            inherited rdeValue2: TRbwDataEntry
              Left = 336
              Top = 109
              Height = 30
              TabOrder = 7
              StyleElements = [seFont, seClient, seBorder]
              OnExit = framePhastInterpolationBoundariesrdeValue2Exit
              ExplicitLeft = 336
              ExplicitTop = 109
              ExplicitHeight = 30
            end
            inherited rgInterpolationDirection: TRadioGroup
              Left = 3
              Width = 233
              Height = 59
              Caption = 'Interp. dir. or mixture'
              Columns = 2
              OnClick = framePhastInterpolationBoundariesrgInterpolationDirectionClick
              ExplicitLeft = 3
              ExplicitWidth = 233
              ExplicitHeight = 59
            end
            inherited edMixFormula: TRbwEdit
              Left = 256
              Top = 32
              Width = 193
              Height = 27
              TabOrder = 2
              StyleElements = [seFont, seClient, seBorder]
              OnExit = framePhastInterpolationBoundariesedMixFormulaExit
              ExplicitLeft = 256
              ExplicitTop = 32
              ExplicitWidth = 193
              ExplicitHeight = 27
            end
            inherited btnEditMixtureFormula: TButton
              Left = 377
              Top = 1
              Anchors = [akTop, akRight]
              TabOrder = 1
              OnClick = btnFormulaClick
              ExplicitLeft = 377
              ExplicitTop = 1
            end
          end
        end
        object seBoundaryTimes: TJvSpinEdit
          Left = 144
          Top = 169
          Width = 101
          Height = 27
          ButtonKind = bkClassic
          MaxValue = 2147483647.000000000000000000
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          TabOrder = 2
          OnChange = seBoundaryTimesChange
        end
      end
    end
    object tabModflowBoundaryConditions: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'MODFLOW_Features_Tab'
      Caption = 'MODFLOW Features'
      ImageIndex = 4
      OnShow = tabModflowBoundaryConditionsShow
      object splttrModflowFeature: TJvNetscapeSplitter
        Left = 185
        Top = 0
        Height = 478
        Align = alLeft
        MinSize = 1
        Maximized = False
        Minimized = False
        ButtonCursor = crDefault
        ExplicitHeight = 606
      end
      object jvtlModflowBoundaryNavigator: TJvPageListTreeView
        Left = 0
        Top = 0
        Width = 185
        Height = 478
        HelpType = htKeyword
        HelpKeyword = 'OBS_Observation_Utility_Pane'
        AutoExpand = False
        ShowLines = True
        PageDefault = 0
        PageList = jvplModflowBoundaries
        Align = alLeft
        HideSelection = False
        StateImages = ilCheckImages
        Indent = 21
        TabOrder = 0
        OnChanging = jvtlModflowBoundaryNavigatorChanging
        OnCustomDrawItem = jvtlModflowBoundaryNavigatorCustomDrawItem
        OnMouseDown = jvtlModflowBoundaryNavigatorMouseDown
        Items.NodeData = {
          07020000001054004A007600500061006700650049006E006400650078004E00
          6F006400650071000000000000000000000001000000FFFFFFFFFFFFFFFF0000
          00000000000000012943004800440020002800540069006D0065002D00560061
          007200690061006E00740020005300700065006300690066006900650064002D
          00480065006100640020007000610063006B0061006700650029000000650000
          00000000000000000001000000FFFFFFFFFFFFFFFF0000000000000000000123
          47004800420020002800470065006E006500720061006C002000480065006100
          6400200042006F0075006E00640061007200790020007000610063006B006100
          670065002900}
        Items.Links = {020000000000000000000000}
      end
      object jvplModflowBoundaries: TJvPageList
        Left = 195
        Top = 0
        Width = 537
        Height = 478
        ActivePage = jvspMf6Obs
        PropagateEnable = False
        Align = alClient
        OnChange = jvplModflowBoundariesChange
        object jvspCHD: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'CHD_Object_Pane'
          Caption = 'jvspCHD'
          inline frameChdParam: TframeScreenObjectParam
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited splitHorizontal: TSplitter
              Width = 537
              ExplicitWidth = 585
            end
            inherited pnlBottom: TPanel
              Top = 402
              Width = 537
              Height = 76
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 402
              ExplicitWidth = 537
              ExplicitHeight = 76
              DesignSize = (
                537
                76)
              inherited lblNumTimes: TLabel
                Left = 78
                Top = 9
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 78
                ExplicitTop = 9
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblTimeSeriesInterpolation: TLabel
                Left = 9
                Top = 42
                Width = 177
                Height = 19
                Visible = True
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 9
                ExplicitTop = 42
                ExplicitWidth = 177
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Left = 8
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                TabOrder = 2
                OnChange = frameChdParamseNumberOfTimesChange
                ExplicitLeft = 8
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 433
                Top = 5
                TabOrder = 1
                ExplicitLeft = 433
                ExplicitTop = 5
              end
              inherited btnInsert: TBitBtn
                Left = 349
                Top = 5
                TabOrder = 0
                ExplicitLeft = 349
                ExplicitTop = 5
              end
              inherited comboTimeSeriesInterpolation: TComboBox
                Left = 200
                Top = 39
                Height = 27
                ItemIndex = 2
                Text = 'LINEAR-END'
                Visible = True
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameChdParamcomboTimeSeriesInterpolationChange
                ExplicitLeft = 200
                ExplicitTop = 39
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 535
                ItemHeight = 19
                OnStateChange = frameChdParamclbParametersStateChange
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 310
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 310
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 258
                ColCount = 4
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSelectCell = frameChdParamrdgModflowBoundarySelectCell
                OnSetEditText = frameChdParamdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameChdParamdgModflowBoundaryEndUpdate
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    ButtonFont.Pitch = fpVariable
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
                    ButtonFont.Pitch = fpVariable
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
                ExplicitWidth = 535
                ExplicitHeight = 258
                ColWidths = (
                  64
                  64
                  64
                  64)
                RowHeights = (
                  24
                  25)
              end
            end
          end
        end
        object jvspGHB: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'GHB_Object_Pane'
          Caption = 'jvspGHB'
          inline frameGhbParam: TframeScreenObjectCondParam
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited splitHorizontal: TSplitter
              Width = 537
              ExplicitWidth = 585
            end
            inherited pnlBottom: TPanel
              Top = 357
              Width = 537
              Height = 121
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 357
              ExplicitWidth = 537
              ExplicitHeight = 121
              DesignSize = (
                537
                121)
              inherited lblNumTimes: TLabel
                Top = 9
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitTop = 9
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblConductanceInterpretation: TLabel
                Left = 9
                Width = 197
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 9
                ExplicitWidth = 197
                ExplicitHeight = 19
              end
              inherited lblTimeSeriesInterpolation: TLabel
                Left = 9
                Top = 84
                Width = 177
                Height = 19
                Visible = True
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 9
                ExplicitTop = 84
                ExplicitWidth = 177
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                Value = 1.000000000000000000
                OnChange = frameGhbParamseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 437
                Top = 3
                ExplicitLeft = 437
                ExplicitTop = 3
              end
              inherited btnInsert: TBitBtn
                Left = 353
                Top = 3
                ExplicitLeft = 353
                ExplicitTop = 3
              end
              inherited comboTimeSeriesInterpolation: TComboBox
                Left = 217
                Top = 81
                Height = 27
                Visible = True
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameGhbParamcomboTimeSeriesInterpolationChange
                ExplicitLeft = 217
                ExplicitTop = 81
                ExplicitHeight = 27
              end
              inherited comboFormulaInterp: TComboBox
                Left = 217
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameGhbParamcomboFormulaInterpChange
                ExplicitLeft = 217
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 535
                ItemHeight = 19
                OnStateChange = frameGhbParamclbParametersStateChange
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 265
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 265
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 213
                ColCount = 4
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSelectCell = frameGhbParamrdgModflowBoundarySelectCell
                OnSetEditText = frameGhbParamdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameGhbParamdgModflowBoundaryEndUpdate
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    AutoAdjustRowHeights = True
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                    ButtonFont.Pitch = fpVariable
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
                ExplicitWidth = 535
                ExplicitHeight = 213
                ColWidths = (
                  64
                  64
                  64
                  64)
                RowHeights = (
                  24
                  25)
              end
            end
          end
        end
        object jvspWell: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'WEL_Object_Pane'
          Caption = 'jvspWell'
          inline frameWellParam: TframeScreenObjectWel
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited splitHorizontal: TSplitter
              Width = 537
              ExplicitWidth = 585
            end
            inherited pnlBottom: TPanel
              Top = 330
              Width = 537
              Height = 148
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 330
              ExplicitWidth = 537
              ExplicitHeight = 148
              DesignSize = (
                537
                148)
              inherited lblNumTimes: TLabel
                Top = 8
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitTop = 8
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblConductanceInterpretation: TLabel
                Left = 9
                Width = 197
                Height = 19
                Caption = 'Pumping rate interpretation'
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 9
                ExplicitWidth = 197
                ExplicitHeight = 19
              end
              inherited lblTabfile: TLabel
                Left = 504
                Top = 122
                Width = 46
                Height = 19
                Enabled = False
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 504
                ExplicitTop = 122
                ExplicitWidth = 46
                ExplicitHeight = 19
              end
              inherited lblTimeSeriesInterpolation: TLabel
                Left = 9
                Top = 84
                Width = 177
                Height = 19
                Visible = True
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 9
                ExplicitTop = 84
                ExplicitWidth = 177
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                Value = 1.000000000000000000
                OnChange = frameWellParamseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 437
                ExplicitLeft = 437
              end
              inherited btnInsert: TBitBtn
                Left = 353
                ExplicitLeft = 353
              end
              inherited comboTimeSeriesInterpolation: TComboBox
                Left = 217
                Top = 81
                Height = 27
                Visible = True
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameWellParamcomboTimeSeriesInterpolationChange
                ExplicitLeft = 217
                ExplicitTop = 81
                ExplicitHeight = 27
              end
              inherited comboFormulaInterp: TComboBox
                Left = 217
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameWellParamcomboFormulaInterpChange
                ExplicitLeft = 217
                ExplicitHeight = 27
              end
              inherited fedTabfile: TJvFilenameEdit
                Left = 9
                Top = 119
                Width = 489
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ReadOnly = True
                ShowButton = False
                OnChange = frameWellParamfedTabfileChange
                ExplicitLeft = 9
                ExplicitTop = 119
                ExplicitWidth = 489
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 535
                ItemHeight = 19
                OnStateChange = frameWellParamclbParametersStateChange
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 238
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 238
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 186
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSelectCell = frameWellParamrdgModflowBoundarySelectCell
                OnSetEditText = frameWellParamdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameWellParamdgModflowBoundaryEndUpdate
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                ExplicitWidth = 535
                ExplicitHeight = 186
                ColWidths = (
                  64
                  64
                  64)
                RowHeights = (
                  24
                  25)
              end
            end
          end
        end
        object jvspRIV: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'RIV_Object_Pane'
          Caption = 'jvspRIV'
          inline frameRivParam: TframeScreenObjectCondParam
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited splitHorizontal: TSplitter
              Width = 537
              ExplicitWidth = 585
            end
            inherited pnlBottom: TPanel
              Top = 365
              Width = 537
              Height = 113
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 365
              ExplicitWidth = 537
              ExplicitHeight = 113
              DesignSize = (
                537
                113)
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblConductanceInterpretation: TLabel
                Left = 6
                Width = 197
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 6
                ExplicitWidth = 197
                ExplicitHeight = 19
              end
              inherited lblTimeSeriesInterpolation: TLabel
                Left = 7
                Top = 84
                Width = 177
                Height = 19
                Visible = True
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 7
                ExplicitTop = 84
                ExplicitWidth = 177
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                Value = 1.000000000000000000
                OnChange = frameRivParamseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 437
                ExplicitLeft = 437
              end
              inherited btnInsert: TBitBtn
                Left = 353
                ExplicitLeft = 353
              end
              inherited comboTimeSeriesInterpolation: TComboBox
                Left = 212
                Top = 81
                Height = 27
                Visible = True
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameRivParamcomboTimeSeriesInterpolationChange
                ExplicitLeft = 212
                ExplicitTop = 81
                ExplicitHeight = 27
              end
              inherited comboFormulaInterp: TComboBox
                Left = 212
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameRivParamcomboFormulaInterpChange
                ExplicitLeft = 212
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 535
                ItemHeight = 19
                OnStateChange = frameRivParamclbParametersStateChange
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 273
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 273
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 221
                ColCount = 5
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSelectCell = frameRivParamrdgModflowBoundarySelectCell
                OnSetEditText = frameRivParamdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameRivParamdgModflowBoundaryEndUpdate
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    AutoAdjustRowHeights = True
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                ExplicitWidth = 535
                ExplicitHeight = 221
                ColWidths = (
                  64
                  64
                  64
                  64
                  64)
                RowHeights = (
                  24
                  25)
              end
            end
          end
        end
        object jvspDRN: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'DRN_Object_Pane'
          Caption = 'jvspDRN'
          inline frameDrnParam: TframeScreenObjectCondParam
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited splitHorizontal: TSplitter
              Width = 537
              ExplicitWidth = 585
            end
            inherited pnlBottom: TPanel
              Top = 354
              Width = 537
              Height = 124
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 354
              ExplicitWidth = 537
              ExplicitHeight = 124
              inherited lblNumTimes: TLabel
                Top = 9
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitTop = 9
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblConductanceInterpretation: TLabel
                Left = 9
                Top = 55
                Width = 197
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 9
                ExplicitTop = 55
                ExplicitWidth = 197
                ExplicitHeight = 19
              end
              inherited lblTimeSeriesInterpolation: TLabel
                Left = 9
                Top = 88
                Width = 177
                Height = 19
                Visible = True
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 9
                ExplicitTop = 88
                ExplicitWidth = 177
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                Value = 1.000000000000000000
                OnChange = frameDrnParamseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                Top = 3
                ExplicitLeft = 445
                ExplicitTop = 3
              end
              inherited btnInsert: TBitBtn
                Left = 361
                Top = 3
                ExplicitLeft = 361
                ExplicitTop = 3
              end
              inherited comboTimeSeriesInterpolation: TComboBox
                Left = 212
                Top = 85
                Height = 27
                Visible = True
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameDrnParamcomboTimeSeriesInterpolationChange
                ExplicitLeft = 212
                ExplicitTop = 85
                ExplicitHeight = 27
              end
              inherited comboFormulaInterp: TComboBox
                Left = 212
                Top = 52
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameDrnParamcomboFormulaInterpChange
                ExplicitLeft = 212
                ExplicitTop = 52
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 535
                ItemHeight = 19
                OnStateChange = frameDrnParamclbParametersStateChange
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 262
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 262
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 210
                ColCount = 4
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSelectCell = frameDrnParamrdgModflowBoundarySelectCell
                OnSetEditText = frameDrnParamdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameDrnParamdgModflowBoundaryEndUpdate
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    AutoAdjustRowHeights = True
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                ExplicitWidth = 535
                ExplicitHeight = 210
                ColWidths = (
                  64
                  64
                  64
                  64)
                RowHeights = (
                  24
                  25)
              end
            end
          end
        end
        object jvspDRT: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'DRT_Object_Pane'
          Caption = 'jvspDRT'
          inline frameDrtParam: TframeScreenObjectCondParam
            Left = 0
            Top = 0
            Width = 537
            Height = 396
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 396
            inherited splitHorizontal: TSplitter
              Width = 537
              ExplicitWidth = 585
            end
            inherited pnlBottom: TPanel
              Top = 315
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 315
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 81
                Height = 38
                WordWrap = True
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 81
                ExplicitHeight = 38
              end
              inherited lblConductanceInterpretation: TLabel
                Top = 52
                Width = 197
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitTop = 52
                ExplicitWidth = 197
                ExplicitHeight = 19
              end
              inherited lblTimeSeriesInterpolation: TLabel
                Left = 352
                Top = 7
                Width = 177
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 352
                ExplicitTop = 7
                ExplicitWidth = 177
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                Value = 1.000000000000000000
                OnChange = frameDrtParamseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 254
                Top = 5
                ExplicitLeft = 254
                ExplicitTop = 5
              end
              inherited btnInsert: TBitBtn
                Left = 170
                Top = 5
                ExplicitLeft = 170
                ExplicitTop = 5
              end
              inherited comboTimeSeriesInterpolation: TComboBox
                Left = 384
                Top = 32
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 384
                ExplicitTop = 32
                ExplicitHeight = 27
              end
              inherited comboFormulaInterp: TComboBox
                Left = 212
                Top = 49
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameDrtParamcomboFormulaInterpChange
                ExplicitLeft = 212
                ExplicitTop = 49
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 535
                ItemHeight = 19
                OnStateChange = frameDrtParamclbParametersStateChange
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 223
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 223
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 171
                ColCount = 5
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSetEditText = frameDrtParamdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameDrtParamdgModflowBoundaryEndUpdate
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    AutoAdjustRowHeights = True
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                ExplicitWidth = 535
                ExplicitHeight = 171
                ColWidths = (
                  64
                  64
                  64
                  64
                  64)
                RowHeights = (
                  24
                  25)
              end
            end
          end
          object pnlDrtLocation: TPanel
            Left = 0
            Top = 396
            Width = 537
            Height = 82
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 1
            object lblReturnLocationMethod: TLabel
              Left = 6
              Top = 9
              Width = 167
              Height = 19
              Caption = 'Return location method'
            end
            object comboDrtLocationChoice: TComboBox
              Left = 6
              Top = 34
              Width = 145
              Height = 27
              Style = csDropDownList
              TabOrder = 1
              OnChange = comboDrtLocationChoiceChange
              Items.Strings = (
                'none'
                'Object'
                'Location'
                'Cell')
            end
            object pcDrtReturnLChoice: TJvPageControl
              Left = 250
              Top = 0
              Width = 287
              Height = 82
              ActivePage = tabDrtNone
              Align = alRight
              Anchors = [akLeft, akTop, akRight, akBottom]
              TabOrder = 0
              ClientBorderWidth = 0
              object tabDrtNone: TTabSheet
                Caption = 'tabDrtNone'
                TabVisible = False
              end
              object tabDrtObject: TTabSheet
                Caption = 'tabDrtObject'
                ImageIndex = 1
                TabVisible = False
                object comboDrtReturnObject: TComboBox
                  Left = 3
                  Top = 0
                  Width = 208
                  Height = 27
                  Style = csDropDownList
                  TabOrder = 0
                  OnChange = comboDrtReturnObjectChange
                  OnExit = rdeDrtLocationControlExit
                end
              end
              object tabDrtLocation: TTabSheet
                Caption = 'tabDrtLocation'
                ImageIndex = 2
                TabVisible = False
                object lblDrtX: TLabel
                  Left = 3
                  Top = 7
                  Width = 11
                  Height = 19
                  Caption = 'X'
                end
                object lblDrtY: TLabel
                  Left = 63
                  Top = 7
                  Width = 11
                  Height = 19
                  Caption = 'Y'
                end
                object lblDrtZ: TLabel
                  Left = 123
                  Top = 7
                  Width = 9
                  Height = 19
                  Caption = 'Z'
                end
                object rdeDrtX: TRbwDataEntry
                  Left = 3
                  Top = 32
                  Width = 54
                  Height = 27
                  TabOrder = 0
                  Text = '0'
                  OnChange = comboDrtReturnObjectChange
                  OnExit = rdeDrtLocationControlExit
                  Items.Strings = (
                    'False'
                    'True')
                  DataType = dtReal
                  Max = 1.000000000000000000
                  ChangeDisabledColor = True
                end
                object rdeDrtY: TRbwDataEntry
                  Left = 63
                  Top = 32
                  Width = 54
                  Height = 27
                  TabOrder = 1
                  Text = '0'
                  OnChange = comboDrtReturnObjectChange
                  OnExit = rdeDrtLocationControlExit
                  Items.Strings = (
                    'False'
                    'True')
                  DataType = dtReal
                  Max = 1.000000000000000000
                  ChangeDisabledColor = True
                end
                object rdeDrtZ: TRbwDataEntry
                  Left = 123
                  Top = 32
                  Width = 54
                  Height = 27
                  TabOrder = 2
                  Text = '0'
                  OnChange = comboDrtReturnObjectChange
                  OnExit = rdeDrtLocationControlExit
                  Items.Strings = (
                    'False'
                    'True')
                  DataType = dtReal
                  Max = 1.000000000000000000
                  ChangeDisabledColor = True
                end
              end
              object tabDrtCell: TTabSheet
                Caption = 'tabDrtCell'
                ImageIndex = 3
                TabVisible = False
                object lblDrtCol: TLabel
                  Left = 3
                  Top = 15
                  Width = 24
                  Height = 19
                  Caption = 'Col'
                end
                object lblDrtRow: TLabel
                  Left = 95
                  Top = 15
                  Width = 31
                  Height = 19
                  Caption = 'Row'
                end
                object lblDrtLay: TLabel
                  Left = 187
                  Top = 15
                  Width = 42
                  Height = 19
                  Caption = 'Layer'
                end
                object rdeDrtLay: TRbwDataEntry
                  Left = 187
                  Top = 40
                  Width = 54
                  Height = 27
                  TabOrder = 2
                  Text = '1'
                  OnChange = comboDrtReturnObjectChange
                  OnExit = rdeDrtLocationControlExit
                  Items.Strings = (
                    'False'
                    'True')
                  DataType = dtInteger
                  Max = 1.000000000000000000
                  Min = 1.000000000000000000
                  CheckMin = True
                  ChangeDisabledColor = True
                end
                object rdeDrtRow: TRbwDataEntry
                  Left = 95
                  Top = 40
                  Width = 54
                  Height = 27
                  TabOrder = 1
                  Text = '1'
                  OnChange = comboDrtReturnObjectChange
                  OnExit = rdeDrtLocationControlExit
                  Items.Strings = (
                    'False'
                    'True')
                  DataType = dtInteger
                  Max = 1.000000000000000000
                  Min = 1.000000000000000000
                  CheckMin = True
                  ChangeDisabledColor = True
                end
                object rdeDrtCol: TRbwDataEntry
                  Left = 3
                  Top = 40
                  Width = 54
                  Height = 27
                  TabOrder = 0
                  Text = '1'
                  OnChange = comboDrtReturnObjectChange
                  OnExit = rdeDrtLocationControlExit
                  Items.Strings = (
                    'False'
                    'True')
                  DataType = dtInteger
                  Max = 1.000000000000000000
                  Min = 1.000000000000000000
                  CheckMin = True
                  ChangeDisabledColor = True
                end
              end
            end
          end
        end
        object jvspRCH: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'RCH_Object_Pane'
          Caption = 'jvspRCH'
          inline frameRchParam: TframeScreenObjectParam
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited splitHorizontal: TSplitter
              Width = 537
              ExplicitWidth = 585
            end
            inherited pnlBottom: TPanel
              Top = 405
              Width = 537
              Height = 73
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 405
              ExplicitWidth = 537
              ExplicitHeight = 73
              inherited lblNumTimes: TLabel
                Top = 9
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitTop = 9
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblTimeSeriesInterpolation: TLabel
                Left = 9
                Top = 42
                Width = 177
                Height = 19
                Visible = True
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 9
                ExplicitTop = 42
                ExplicitWidth = 177
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                Value = 1.000000000000000000
                OnChange = frameRchParamseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                Top = 3
                ExplicitLeft = 445
                ExplicitTop = 3
              end
              inherited btnInsert: TBitBtn
                Left = 361
                Top = 3
                ExplicitLeft = 361
                ExplicitTop = 3
              end
              inherited comboTimeSeriesInterpolation: TComboBox
                Left = 200
                Top = 39
                Height = 27
                Visible = True
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameRchParamcomboTimeSeriesInterpolationChange
                ExplicitLeft = 200
                ExplicitTop = 39
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 535
                ItemHeight = 19
                OnStateChange = frameRchParamclbParametersStateChange
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 313
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 313
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 261
                ColCount = 4
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSelectCell = frameRchParamrdgModflowBoundarySelectCell
                OnSetEditText = frameRchParamdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameRchParamdgModflowBoundaryEndUpdate
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                ExplicitWidth = 535
                ExplicitHeight = 261
                ColWidths = (
                  64
                  64
                  64
                  64)
                RowHeights = (
                  24
                  25)
              end
            end
          end
        end
        object jvspBlank: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          Caption = 'jvspBlank'
        end
        object jvspEVT: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'EVT_Object_Pane'
          Caption = 'jvspEVT'
          inline frameEvtParam: TframeScreenObjectParam
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited splitHorizontal: TSplitter
              Width = 537
              ExplicitWidth = 585
            end
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblTimeSeriesInterpolation: TLabel
                Top = 9
                Width = 177
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitTop = 9
                ExplicitWidth = 177
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                Value = 1.000000000000000000
                OnChange = frameEvtParamseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
              inherited comboTimeSeriesInterpolation: TComboBox
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 535
                ItemHeight = 19
                OnStateChange = frameEvtParamclbParametersStateChange
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 340
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 340
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 288
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSetEditText = frameEvtParamdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameEvtParamdgModflowBoundaryEndUpdate
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                ExplicitWidth = 535
                ExplicitHeight = 288
                ColWidths = (
                  64
                  64
                  64)
                RowHeights = (
                  24
                  25)
              end
            end
          end
        end
        object jvspETS: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'ETS_Object_Pane'
          Caption = 'jvspETS'
          inline frameEtsParam: TframeScreenObjectParam
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited splitHorizontal: TSplitter
              Width = 537
              ExplicitWidth = 585
            end
            inherited pnlBottom: TPanel
              Top = 402
              Width = 537
              Height = 76
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 402
              ExplicitWidth = 537
              ExplicitHeight = 76
              inherited lblNumTimes: TLabel
                Top = 13
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitTop = 13
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblTimeSeriesInterpolation: TLabel
                Left = 9
                Top = 43
                Width = 177
                Height = 19
                Visible = True
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 9
                ExplicitTop = 43
                ExplicitWidth = 177
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Top = 10
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                Value = 1.000000000000000000
                TabOrder = 2
                OnChange = frameEtsParamseNumberOfTimesChange
                ExplicitTop = 10
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                Top = 7
                TabOrder = 1
                ExplicitLeft = 445
                ExplicitTop = 7
              end
              inherited btnInsert: TBitBtn
                Left = 361
                Top = 7
                TabOrder = 0
                ExplicitLeft = 361
                ExplicitTop = 7
              end
              inherited comboTimeSeriesInterpolation: TComboBox
                Left = 200
                Top = 40
                Height = 27
                Visible = True
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameEtsParamcomboTimeSeriesInterpolationChange
                ExplicitLeft = 200
                ExplicitTop = 40
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 535
                ItemHeight = 19
                OnStateChange = frameEtsParamclbParametersStateChange
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 310
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 310
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 258
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSelectCell = frameEtsParamrdgModflowBoundarySelectCell
                OnSetEditText = frameEtsParamdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameEtsParamdgModflowBoundaryEndUpdate
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                ExplicitWidth = 535
                ExplicitHeight = 258
                ColWidths = (
                  64
                  64
                  64)
                RowHeights = (
                  24
                  25)
              end
            end
          end
        end
        object jvspRES: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'RES_Object_Pane'
          Caption = 'jvspRES'
          inline frameRes: TframeScreenObjectNoParam
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameResseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                ColCount = 4
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSetEditText = frameResdgModflowBoundarySetEditText
                OnButtonClick = frameResdgModflowBoundaryButtonClick
                OnEndUpdate = frameResdgModflowBoundaryEndUpdate
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    AutoAdjustRowHeights = True
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                ExplicitWidth = 535
                ExplicitHeight = 355
                ColWidths = (
                  64
                  64
                  64
                  64)
                RowHeights = (
                  24
                  25)
              end
            end
          end
        end
        object jvspLAK: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'LAK_Object_Pane'
          Caption = 'jvspLAK'
          inline frameLak: TframeScreenObjectLAK
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 369
              Width = 537
              Height = 109
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 369
              ExplicitWidth = 537
              ExplicitHeight = 109
              DesignSize = (
                537
                109)
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblInitialStage: TLabel
                Top = 76
                Width = 82
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitTop = 76
                ExplicitWidth = 82
                ExplicitHeight = 19
              end
              inherited lblCenterLake: TLabel
                Width = 83
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 83
                ExplicitHeight = 19
              end
              inherited lblSill: TLabel
                Top = 76
                Width = 21
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitTop = 76
                ExplicitWidth = 21
                ExplicitHeight = 19
              end
              inherited lblLakeID: TLabel
                Width = 57
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 57
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameLakseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 437
                ExplicitLeft = 437
              end
              inherited btnInsert: TBitBtn
                Left = 349
                ExplicitLeft = 349
              end
              inherited rdeInitialStage: TRbwDataEntry
                Top = 73
                TabOrder = 5
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameLakrdeInitialStageChange
                ExplicitTop = 73
              end
              inherited rdeCenterLake: TRbwDataEntry
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameLakrdeCenterLakeChange
              end
              inherited rdeSill: TRbwDataEntry
                TabOrder = 6
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameLakrdeSillChange
              end
              inherited rdeLakeID: TRbwDataEntry
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameLakrdeLakeIDChange
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 144
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 144
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 92
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSetEditText = frameLakdgModflowBoundarySetEditText
                OnButtonClick = frameResdgModflowBoundaryButtonClick
                OnEndUpdate = frameLakdgModflowBoundaryEndUpdate
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                ExplicitWidth = 535
                ExplicitHeight = 92
                RowHeights = (
                  24
                  25)
              end
            end
            inherited pcLake: TPageControl
              Top = 169
              Width = 537
              Height = 200
              ActivePage = frameLak.tabBathymetry
              ExplicitTop = 169
              ExplicitWidth = 537
              ExplicitHeight = 200
              inherited tabLakeProperties: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 293
              end
              inherited tabGage: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 293
                DesignSize = (
                  533
                  293)
                inherited gbGage: TGroupBox
                  Width = 530
                  ExplicitWidth = 530
                  DesignSize = (
                    530
                    160)
                  inherited cbGagStandard: TCheckBox
                    Left = 5
                    OnClick = frameLakcbGagStandardClick
                    ExplicitLeft = 5
                  end
                  inherited cbGagFluxAndCond: TCheckBox
                    Left = 5
                    OnClick = frameLakcbGagFluxAndCondClick
                    ExplicitLeft = 5
                  end
                  inherited cbGagDelta: TCheckBox
                    Left = 5
                    OnClick = frameLakcbGagDeltaClick
                    ExplicitLeft = 5
                  end
                  inherited cbGage4: TCheckBox
                    Width = 524
                    OnClick = frameLakcbGage4Click
                    ExplicitWidth = 524
                  end
                end
              end
              inherited tabBathymetry: TTabSheet
                ExplicitTop = 30
                ExplicitWidth = 529
                ExplicitHeight = 166
                inherited rdgLakeTable: TRbwDataGrid4
                  Width = 529
                  Height = 61
                  OnEndUpdate = frameLakrdgLakeTableEndUpdate
                  ExplicitWidth = 529
                  ExplicitHeight = 61
                  ColWidths = (
                    64
                    64
                    64)
                  RowHeights = (
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24)
                end
                inherited pnlBathChoice: TPanel
                  Width = 529
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 529
                  inherited rgBathChoice: TRadioGroup
                    OnClick = frameLakrgBathChoiceClick
                  end
                  inherited feLakeBathymetry: TJvFilenameEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnChange = frameLakfeLakeBathymetryChange
                    ExplicitHeight = 27
                  end
                end
              end
              inherited tabObservations: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 293
                inherited framePestObsLak: TframePestObs
                  Height = 293
                  ExplicitHeight = 293
                  inherited splObservations: TSplitter
                    Top = 116
                    ExplicitTop = 116
                    ExplicitWidth = 533
                  end
                  inherited grpDirectObs: TGroupBox
                    Height = 116
                    ExplicitHeight = 116
                    inherited frameObservations: TframeGrid
                      Top = 21
                      Height = 93
                      ExplicitTop = 21
                      ExplicitHeight = 93
                      inherited Panel: TPanel
                        Top = 61
                        StyleElements = [seFont, seClient, seBorder]
                        ExplicitTop = 61
                        ExplicitWidth = 529
                        inherited lbNumber: TLabel
                          Width = 218
                          Height = 19
                          StyleElements = [seFont, seClient, seBorder]
                          ExplicitWidth = 218
                          ExplicitHeight = 19
                        end
                        inherited sbAdd: TSpeedButton
                          Left = 356
                          ExplicitLeft = 365
                        end
                        inherited sbInsert: TSpeedButton
                          Left = 385
                          ExplicitLeft = 394
                        end
                        inherited sbDelete: TSpeedButton
                          Left = 414
                          ExplicitLeft = 423
                        end
                        inherited seNumber: TJvSpinEdit
                          Height = 27
                          StyleElements = [seFont, seClient, seBorder]
                          ExplicitHeight = 27
                        end
                      end
                      inherited Grid: TRbwDataGrid4
                        Height = 61
                        ExplicitHeight = 61
                      end
                    end
                  end
                  inherited grpObsComparisons: TGroupBox
                    Top = 121
                    ExplicitTop = 121
                    inherited frameObsComparisons: TframeGrid
                      Top = 21
                      Height = 149
                      ExplicitTop = 21
                      ExplicitHeight = 149
                      inherited Panel: TPanel
                        Top = 114
                        StyleElements = [seFont, seClient, seBorder]
                        ExplicitTop = 114
                        ExplicitWidth = 529
                        inherited lbNumber: TLabel
                          Width = 262
                          Height = 19
                          StyleElements = [seFont, seClient, seBorder]
                          ExplicitWidth = 262
                          ExplicitHeight = 19
                        end
                        inherited sbAdd: TSpeedButton
                          Left = 356
                          ExplicitLeft = 365
                        end
                        inherited sbInsert: TSpeedButton
                          Left = 385
                          ExplicitLeft = 394
                        end
                        inherited sbDelete: TSpeedButton
                          Left = 414
                          ExplicitLeft = 423
                        end
                        inherited seNumber: TJvSpinEdit
                          Height = 27
                          StyleElements = [seFont, seClient, seBorder]
                          ExplicitHeight = 27
                        end
                      end
                      inherited Grid: TRbwDataGrid4
                        Height = 114
                        ExplicitHeight = 114
                      end
                    end
                  end
                end
              end
            end
          end
        end
        object jvspSFR: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'SFR_Object_Pane'
          Caption = 'jvspSFR'
          inline frameScreenObjectSFR: TframeScreenObjectSFR
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pcSFR: TPageControl
              Width = 537
              Height = 478
              ActivePage = frameScreenObjectSFR.tabObservations
              OnChange = frameScreenObjectSFRpcSFRChange
              ExplicitWidth = 537
              ExplicitHeight = 478
              inherited tabBasic: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 472
                DesignSize = (
                  533
                  472)
                inherited Label1: TLabel
                  Width = 186
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 186
                  ExplicitHeight = 19
                end
                inherited rdeSegmentNumber: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited gReachProperties: TGroupBox
                  Left = 2
                  Width = 981
                  ExplicitLeft = 2
                  ExplicitWidth = 981
                  inherited lblStreamTop: TLabel
                    Left = 219
                    Top = 55
                    Width = 186
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitLeft = 219
                    ExplicitTop = 55
                    ExplicitWidth = 186
                    ExplicitHeight = 19
                  end
                  inherited lblSlope: TLabel
                    Left = 219
                    Top = 82
                    Width = 166
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitLeft = 219
                    ExplicitTop = 82
                    ExplicitWidth = 166
                    ExplicitHeight = 19
                  end
                  inherited lblStreambedThickness: TLabel
                    Left = 219
                    Top = 109
                    Width = 248
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitLeft = 219
                    ExplicitTop = 109
                    ExplicitWidth = 248
                    ExplicitHeight = 19
                  end
                  inherited lblStreambedK: TLabel
                    Left = 219
                    Top = 136
                    Width = 182
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitLeft = 219
                    ExplicitTop = 136
                    ExplicitWidth = 182
                    ExplicitHeight = 19
                  end
                  inherited lblSaturatedVolumetricWater: TLabel
                    Left = 219
                    Top = 163
                    Width = 307
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitLeft = 219
                    ExplicitTop = 163
                    ExplicitWidth = 307
                    ExplicitHeight = 19
                  end
                  inherited lblInitialVolumetricWater: TLabel
                    Left = 219
                    Top = 190
                    Width = 269
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitLeft = 219
                    ExplicitTop = 190
                    ExplicitWidth = 269
                    ExplicitHeight = 19
                  end
                  inherited lblBrooksCoreyExponent: TLabel
                    Left = 219
                    Top = 217
                    Width = 222
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitLeft = 219
                    ExplicitTop = 217
                    ExplicitWidth = 222
                    ExplicitHeight = 19
                  end
                  inherited lblMaxUnsaturatedKz: TLabel
                    Left = 219
                    Top = 244
                    Width = 194
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitLeft = 219
                    ExplicitTop = 244
                    ExplicitWidth = 194
                    ExplicitHeight = 19
                  end
                  inherited lblReachLength: TLabel
                    Left = 219
                    Top = 30
                    Width = 177
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitLeft = 219
                    ExplicitTop = 30
                    ExplicitWidth = 177
                    ExplicitHeight = 19
                  end
                  inherited jceStreamTop: TJvComboEdit
                    Top = 51
                    Width = 210
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitTop = 51
                    ExplicitWidth = 210
                    ExplicitHeight = 27
                  end
                  inherited jceSlope: TJvComboEdit
                    Left = 3
                    Top = 78
                    Width = 210
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitLeft = 3
                    ExplicitTop = 78
                    ExplicitWidth = 210
                    ExplicitHeight = 27
                  end
                  inherited jceStreambedThickness: TJvComboEdit
                    Left = 3
                    Top = 105
                    Width = 210
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitLeft = 3
                    ExplicitTop = 105
                    ExplicitWidth = 210
                    ExplicitHeight = 27
                  end
                  inherited jceStreambedK: TJvComboEdit
                    Top = 132
                    Width = 210
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitTop = 132
                    ExplicitWidth = 210
                    ExplicitHeight = 27
                  end
                  inherited jceSaturatedVolumetricWater: TJvComboEdit
                    Top = 159
                    Width = 210
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitTop = 159
                    ExplicitWidth = 210
                    ExplicitHeight = 27
                  end
                  inherited jceInitialVolumetricWater: TJvComboEdit
                    Top = 186
                    Width = 210
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitTop = 186
                    ExplicitWidth = 210
                    ExplicitHeight = 27
                  end
                  inherited jceBrooksCoreyExponent: TJvComboEdit
                    Top = 213
                    Width = 210
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitTop = 213
                    ExplicitWidth = 210
                    ExplicitHeight = 27
                  end
                  inherited jceMaxUnsaturatedKz: TJvComboEdit
                    Top = 240
                    Width = 210
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitTop = 240
                    ExplicitWidth = 210
                    ExplicitHeight = 27
                  end
                  inherited jvcReachLength: TJvComboEdit
                    Left = 3
                    Top = 24
                    Width = 210
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitLeft = 3
                    ExplicitTop = 24
                    ExplicitWidth = 210
                    ExplicitHeight = 27
                  end
                end
                inherited pnlCaption: TPanel
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited tabTime: TTabSheet
                ExplicitTop = 30
                ExplicitWidth = 529
                ExplicitHeight = 444
                inherited pnlParamTop: TPanel
                  Width = 529
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 529
                  inherited lblParameterChoices: TLabel
                    Width = 76
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 76
                    ExplicitHeight = 19
                  end
                  inherited lblIcalcChoice: TLabel
                    Width = 190
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 190
                    ExplicitHeight = 19
                  end
                  inherited comboParameterChoices: TJvImageComboBox
                    Height = 29
                    StyleElements = [seFont, seClient, seBorder]
                    ItemHeight = 23
                    ExplicitHeight = 29
                  end
                  inherited comboIcalcChoice: TJvImageComboBox
                    Height = 29
                    StyleElements = [seFont, seClient, seBorder]
                    ItemHeight = 23
                    ExplicitHeight = 29
                  end
                end
                inherited rdgParameters: TRbwDataGrid4
                  Width = 529
                  Height = 346
                  Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                  Columns = <
                    item
                      AutoAdjustRowHeights = True
                      AutoAdjustCaptionRowHeights = False
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = False
                      ButtonWidth = 35
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
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = False
                      ButtonWidth = 35
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
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = False
                      ButtonWidth = 35
                      CheckMax = False
                      CheckMin = False
                      ComboUsed = False
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
                      AutoAdjustRowHeights = False
                      AutoAdjustCaptionRowHeights = False
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = False
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
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = False
                      ButtonWidth = 35
                      CheckMax = False
                      CheckMin = False
                      ComboUsed = False
                      Format = rcf4String
                      LimitToList = True
                      MaxLength = 0
                      ParentButtonFont = False
                      PickList.Strings = (
                        'Specified stage (0)'
                        'Rectangular channel (1)'
                        'Eight-point chanel (2)'
                        'Power function (3)'
                        'Table (4)')
                      WordWrapCaptions = True
                      WordWrapCells = False
                      CaseSensitivePicklist = False
                      CheckStyle = csCheck
                      AutoAdjustColWidths = True
                    end>
                  ExplicitWidth = 529
                  ExplicitHeight = 346
                  RowHeights = (
                    24
                    24)
                end
                inherited pnlParamBottom: TPanel
                  Top = 403
                  Width = 529
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 403
                  ExplicitWidth = 529
                  inherited lblParametersCount: TLabel
                    Width = 119
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 119
                    ExplicitHeight = 19
                  end
                  inherited seParametersCount: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitHeight = 27
                  end
                end
              end
              inherited tabNetwork: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 472
                inherited pnlNetwork: TPanel
                  StyleElements = [seFont, seClient, seBorder]
                  inherited lblSegment: TLabel
                    Width = 59
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 59
                    ExplicitHeight = 19
                  end
                  inherited rdeNetwork: TRbwDataEntry
                    StyleElements = [seFont, seClient, seBorder]
                  end
                  inherited comboMultiIprior: TJvImageComboBox
                    Height = 29
                    StyleElements = [seFont, seClient, seBorder]
                    ItemHeight = 23
                    ItemIndex = -1
                    ExplicitHeight = 29
                  end
                end
                inherited rdgNetwork: TRbwDataGrid4
                  Height = 415
                  OnButtonClick = frameScreenObjectSFRrdgNetworkButtonClick
                  ExplicitHeight = 415
                  ColWidths = (
                    50
                    50
                    50
                    50
                    50)
                  RowHeights = (
                    24
                    24)
                end
              end
              inherited tabFlows: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 472
                inherited dgFlowTimes: TRbwDataGrid4
                  Height = 415
                  Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
                  OnSetEditText = frameScreenObjectSFRdgFlowTimesSetEditText
                  OnButtonClick = frameResdgModflowBoundaryButtonClick
                  Columns = <
                    item
                      AutoAdjustRowHeights = True
                      AutoAdjustCaptionRowHeights = False
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = False
                      ButtonWidth = 35
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
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = False
                      ButtonWidth = 35
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
                      ButtonFont.Pitch = fpVariable
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
                      ButtonFont.Pitch = fpVariable
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
                      ButtonFont.Pitch = fpVariable
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
                      ButtonFont.Pitch = fpVariable
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
                  ExplicitHeight = 415
                  ColWidths = (
                    50
                    50
                    50
                    50
                    50
                    50)
                end
                inherited pnlFlowTop: TPanel
                  StyleElements = [seFont, seClient, seBorder]
                  inherited lblFlowFormula: TLabel
                    Width = 59
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 59
                    ExplicitHeight = 19
                  end
                  inherited rdeFlowFormula: TRbwDataEntry
                    StyleElements = [seFont, seClient, seBorder]
                  end
                end
              end
              inherited tabSegment: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 472
                inherited Splitter1: TSplitter
                  ExplicitWidth = 533
                end
                inherited pnlSegmentUpstream: TPanel
                  StyleElements = [seFont, seClient, seBorder]
                  inherited dgUp: TRbwDataGrid4
                    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
                    OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                    Columns = <
                      item
                        AutoAdjustRowHeights = True
                        AutoAdjustCaptionRowHeights = False
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = False
                        ButtonWidth = 35
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
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = False
                        ButtonWidth = 35
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
                        ButtonFont.Pitch = fpVariable
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
                        ButtonFont.Pitch = fpVariable
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
                        ButtonFont.Pitch = fpVariable
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
                        ButtonFont.Pitch = fpVariable
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
                        ButtonFont.Pitch = fpVariable
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
                    ColWidths = (
                      50
                      50
                      50
                      50
                      50
                      50
                      50)
                  end
                  inherited pnlUpstream: TPanel
                    StyleElements = [seFont, seClient, seBorder]
                    inherited lblUpstreamFormula: TLabel
                      Width = 59
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 59
                      ExplicitHeight = 19
                    end
                    inherited rdeUpstreamFormula: TRbwDataEntry
                      StyleElements = [seFont, seClient, seBorder]
                    end
                  end
                end
                inherited pnlSegmentDownstream: TPanel
                  Height = 255
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitHeight = 255
                  inherited dgDown: TRbwDataGrid4
                    Height = 201
                    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
                    OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                    Columns = <
                      item
                        AutoAdjustRowHeights = True
                        AutoAdjustCaptionRowHeights = False
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = False
                        ButtonWidth = 35
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
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = False
                        ButtonWidth = 35
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
                        ButtonFont.Pitch = fpVariable
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
                        ButtonFont.Pitch = fpVariable
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
                        ButtonFont.Pitch = fpVariable
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
                        ButtonFont.Pitch = fpVariable
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
                        ButtonFont.Pitch = fpVariable
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
                    ExplicitHeight = 201
                    ColWidths = (
                      50
                      50
                      50
                      50
                      50
                      50
                      50)
                  end
                  inherited pnlDownstream: TPanel
                    StyleElements = [seFont, seClient, seBorder]
                    inherited lblDownstreamFormula: TLabel
                      Width = 59
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 59
                      ExplicitHeight = 19
                    end
                    inherited rdeDownstreamFormula: TRbwDataEntry
                      StyleElements = [seFont, seClient, seBorder]
                    end
                  end
                end
              end
              inherited tabChannel: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 472
                inherited Panel5: TPanel
                  Height = 363
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 533
                  ExplicitHeight = 363
                  inherited Splitter3: TSplitter
                    Left = 361
                    Width = 5
                    Height = 304
                    ExplicitLeft = 277
                    ExplicitTop = 58
                    ExplicitWidth = 5
                    ExplicitHeight = 289
                  end
                  inherited dgSfrRough: TRbwDataGrid4
                    Width = 360
                    Height = 304
                    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
                    OnButtonClick = frameResdgModflowBoundaryButtonClick
                    Columns = <
                      item
                        AutoAdjustRowHeights = True
                        AutoAdjustCaptionRowHeights = False
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = False
                        ButtonWidth = 35
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
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = False
                        ButtonWidth = 35
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
                        ButtonFont.Pitch = fpVariable
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
                        ButtonFont.Pitch = fpVariable
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
                    ExplicitWidth = 360
                    ExplicitHeight = 304
                    ColWidths = (
                      64
                      64
                      64
                      64)
                  end
                  inherited pnlChannelTop: TPanel
                    StyleElements = [seFont, seClient, seBorder]
                    inherited lblChannelFormula: TLabel
                      Width = 59
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 59
                      ExplicitHeight = 19
                    end
                    inherited rdeChannelFormula: TRbwDataEntry
                      StyleElements = [seFont, seClient, seBorder]
                    end
                  end
                  inherited frameCrossSection1: TframeCrossSection
                    Height = 304
                    ExplicitHeight = 304
                    inherited dg8Point: TRbwDataGrid4
                      Height = 304
                      ExplicitHeight = 304
                      ColWidths = (
                        64
                        64)
                      RowHeights = (
                        24
                        24
                        24
                        24
                        24
                        24
                        24
                        24
                        24)
                    end
                  end
                end
                inherited zbChannel: TQRbwZoomBox2
                  Top = 363
                  StyleElements = [seFont, seClient, seBorder]
                  Image32.Top = 0
                  ExplicitTop = 363
                  ExplicitWidth = 533
                end
              end
              inherited tabEquation: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 472
                inherited dgSfrEquation: TRbwDataGrid4
                  Height = 415
                  Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
                  OnButtonClick = frameResdgModflowBoundaryButtonClick
                  Columns = <
                    item
                      AutoAdjustRowHeights = True
                      AutoAdjustCaptionRowHeights = False
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = False
                      ButtonWidth = 35
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
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = False
                      ButtonWidth = 35
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
                      ButtonFont.Pitch = fpVariable
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
                      ButtonFont.Pitch = fpVariable
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
                      ButtonFont.Pitch = fpVariable
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
                      ButtonFont.Pitch = fpVariable
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
                  ExplicitHeight = 415
                  ColWidths = (
                    64
                    64
                    64
                    64
                    64
                    64)
                  RowHeights = (
                    24
                    24)
                end
                inherited pnlEquationTop: TPanel
                  StyleElements = [seFont, seClient, seBorder]
                  inherited lblEquationFormula: TLabel
                    Width = 59
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 59
                    ExplicitHeight = 19
                  end
                  inherited rdeEquationFormula: TRbwDataEntry
                    StyleElements = [seFont, seClient, seBorder]
                  end
                end
              end
              inherited tabTable: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 472
                inherited Splitter2: TSplitter
                  Height = 315
                  ExplicitLeft = 198
                  ExplicitHeight = 315
                end
                inherited Panel4: TPanel
                  Height = 315
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitHeight = 315
                  inherited dgTableTime: TRbwDataGrid4
                    Height = 313
                    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goTabs]
                    Columns = <
                      item
                        AutoAdjustRowHeights = False
                        AutoAdjustCaptionRowHeights = False
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = False
                        ButtonWidth = 35
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
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = False
                        ButtonWidth = 35
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
                    ExplicitHeight = 313
                    ColWidths = (
                      64
                      64)
                    RowHeights = (
                      24
                      24)
                  end
                end
                inherited gpFlowTable: TGridPanel
                  Top = 315
                  ColumnCollection = <
                    item
                      Value = 49.909323456075350000
                    end
                    item
                      Value = 50.090676543924650000
                    end>
                  ControlCollection = <
                    item
                      Column = 0
                      Control = frameScreenObjectSFR.zbFlowDepthTable
                      Row = 0
                    end
                    item
                      Column = 1
                      Control = frameScreenObjectSFR.zbFlowWidthTable
                      Row = 0
                    end>
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 315
                  inherited zbFlowDepthTable: TQRbwZoomBox2
                    StyleElements = [seFont, seClient, seBorder]
                    Image32.Top = 0
                    ExplicitWidth = 259
                  end
                  inherited zbFlowWidthTable: TQRbwZoomBox2
                    StyleElements = [seFont, seClient, seBorder]
                    Image32.Top = 0
                    ExplicitLeft = 269
                    ExplicitWidth = 260
                  end
                end
                inherited frameFlowTable1: TframeFlowTable
                  Height = 315
                  ExplicitHeight = 315
                  inherited dgSfrTable: TRbwDataGrid4
                    Height = 243
                    OnSetEditText = frameFlowTable1dgSfrTableSetEditText
                    ExplicitHeight = 243
                    ColWidths = (
                      64
                      64
                      64)
                  end
                  inherited pnl1: TPanel
                    Top = 243
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitTop = 243
                    inherited lblNumberOfPoints: TLabel
                      Width = 124
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 124
                      ExplicitHeight = 19
                    end
                    inherited btnDeleteFlowTableRow: TBitBtn
                      OnClick = frameFlowTable1btnDeleteFlowTableRowClick
                    end
                    inherited btnInsertFlowTableRow: TBitBtn
                      OnClick = frameFlowTable1btnInsertFlowTableRowClick
                    end
                    inherited seTableCount: TJvSpinEdit
                      Height = 27
                      StyleElements = [seFont, seClient, seBorder]
                      OnChange = frameFlowTable1seTableCountChange
                      ExplicitHeight = 27
                    end
                  end
                end
              end
              inherited tabUnsaturatedProperties: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 472
                DesignSize = (
                  533
                  472)
                inherited gbUnsatUpstream: TGroupBox
                  Width = 782
                  ExplicitWidth = 782
                  inherited Label6: TLabel
                    Left = 211
                    Width = 316
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitLeft = 211
                    ExplicitWidth = 316
                    ExplicitHeight = 19
                  end
                  inherited Label17: TLabel
                    Left = 211
                    Width = 278
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitLeft = 211
                    ExplicitWidth = 278
                    ExplicitHeight = 19
                  end
                  inherited Label18: TLabel
                    Left = 211
                    Width = 231
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitLeft = 211
                    ExplicitWidth = 231
                    ExplicitHeight = 19
                  end
                  inherited Label19: TLabel
                    Left = 211
                    Width = 203
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitLeft = 211
                    ExplicitWidth = 203
                    ExplicitHeight = 19
                  end
                  inherited jceSaturatedVolumetricWaterUpstream: TJvComboEdit
                    Width = 202
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitWidth = 202
                    ExplicitHeight = 27
                  end
                  inherited jceInitialVolumetricWaterUpstream: TJvComboEdit
                    Width = 202
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitWidth = 202
                    ExplicitHeight = 27
                  end
                  inherited jceBrooksCoreyExponentUpstream: TJvComboEdit
                    Width = 202
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitWidth = 202
                    ExplicitHeight = 27
                  end
                  inherited jceMaxUnsaturatedKzUpstream: TJvComboEdit
                    Width = 202
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitWidth = 202
                    ExplicitHeight = 27
                  end
                end
                inherited gbUnsatDownstream: TGroupBox
                  Width = 782
                  ExplicitWidth = 782
                  inherited Label20: TLabel
                    Left = 211
                    Width = 316
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitLeft = 211
                    ExplicitWidth = 316
                    ExplicitHeight = 19
                  end
                  inherited Label21: TLabel
                    Left = 211
                    Width = 278
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitLeft = 211
                    ExplicitWidth = 278
                    ExplicitHeight = 19
                  end
                  inherited Label22: TLabel
                    Left = 211
                    Width = 231
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitLeft = 211
                    ExplicitWidth = 231
                    ExplicitHeight = 19
                  end
                  inherited Label23: TLabel
                    Left = 211
                    Width = 203
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitLeft = 211
                    ExplicitWidth = 203
                    ExplicitHeight = 19
                  end
                  inherited jceSaturatedVolumetricWaterDownstream: TJvComboEdit
                    Width = 202
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitWidth = 202
                    ExplicitHeight = 27
                  end
                  inherited jceInitialVolumetricWaterDownstream: TJvComboEdit
                    Width = 202
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitWidth = 202
                    ExplicitHeight = 27
                  end
                  inherited jceBrooksCoreyExponentDownstream: TJvComboEdit
                    Width = 202
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitWidth = 202
                    ExplicitHeight = 27
                  end
                  inherited jceMaxUnsaturatedKzDownstream: TJvComboEdit
                    Width = 202
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitWidth = 202
                    ExplicitHeight = 27
                  end
                end
              end
              inherited tabExternalFlowFile: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 472
                inherited frameExternalFileValues: TframeGrid
                  Height = 327
                  ExplicitHeight = 327
                  inherited Panel: TPanel
                    Top = 286
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitTop = 286
                    inherited lbNumber: TLabel
                      Width = 57
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 57
                      ExplicitHeight = 19
                    end
                    inherited sbAdd: TSpeedButton
                      Left = 289
                      ExplicitLeft = 300
                    end
                    inherited sbInsert: TSpeedButton
                      Left = 343
                      ExplicitLeft = 356
                    end
                    inherited sbDelete: TSpeedButton
                      Left = 396
                      ExplicitLeft = 411
                    end
                    inherited seNumber: TJvSpinEdit
                      Height = 27
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitHeight = 27
                    end
                  end
                  inherited Grid: TRbwDataGrid4
                    Height = 286
                    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
                    ExplicitHeight = 286
                    ColWidths = (
                      64
                      64)
                    RowHeights = (
                      24
                      24)
                  end
                end
                inherited pnlFlowFile: TPanel
                  StyleElements = [seFont, seClient, seBorder]
                  inherited lblExternalFileName: TLabel
                    Width = 129
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 129
                    ExplicitHeight = 19
                  end
                  inherited fedExternalFileName: TJvFilenameEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitHeight = 27
                  end
                end
              end
              inherited tabGage: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 472
                DesignSize = (
                  533
                  472)
                inherited rgGages: TRadioGroup
                  Height = 95
                  ExplicitHeight = 95
                end
                inherited gbObservationTypes: TGroupBox
                  Left = 2
                  Top = 104
                  Width = 947
                  Height = 329
                  ExplicitLeft = 2
                  ExplicitTop = 104
                  ExplicitWidth = 947
                  ExplicitHeight = 329
                  inherited cbGag2: TCheckBox
                    Width = 553
                    ExplicitWidth = 553
                  end
                  inherited cbGag5: TCheckBox
                    Top = 158
                    Width = 553
                    ExplicitTop = 158
                    ExplicitWidth = 553
                  end
                  inherited cbGag6: TCheckBox
                    Top = 223
                    Width = 553
                    ExplicitTop = 223
                    ExplicitWidth = 553
                  end
                  inherited cbGag7: TCheckBox
                    Top = 287
                    Width = 553
                    ExplicitTop = 287
                    ExplicitWidth = 553
                  end
                end
              end
              inherited tabObservations: TTabSheet
                HelpType = htKeyword
                HelpKeyword = 'Calibration_tabSfr'
                ExplicitTop = 30
                ExplicitWidth = 529
                ExplicitHeight = 444
                inherited frameSfrPestObs: TframePestObs
                  Width = 529
                  Height = 444
                  ExplicitWidth = 529
                  ExplicitHeight = 444
                  inherited splObservations: TSplitter
                    Top = 267
                    Width = 529
                    ExplicitTop = 295
                    ExplicitWidth = 533
                  end
                  inherited grpDirectObs: TGroupBox
                    Width = 529
                    Height = 267
                    ExplicitWidth = 529
                    ExplicitHeight = 267
                    inherited frameObservations: TframeGrid
                      Top = 21
                      Width = 525
                      Height = 244
                      ExplicitTop = 21
                      ExplicitWidth = 525
                      ExplicitHeight = 244
                      inherited Panel: TPanel
                        Top = 212
                        Width = 525
                        StyleElements = [seFont, seClient, seBorder]
                        ExplicitTop = 212
                        ExplicitWidth = 525
                        inherited lbNumber: TLabel
                          Width = 218
                          Height = 19
                          StyleElements = [seFont, seClient, seBorder]
                          ExplicitWidth = 218
                          ExplicitHeight = 19
                        end
                        inherited sbAdd: TSpeedButton
                          Left = 339
                          ExplicitLeft = 355
                        end
                        inherited sbInsert: TSpeedButton
                          Left = 366
                          ExplicitLeft = 383
                        end
                        inherited sbDelete: TSpeedButton
                          Left = 393
                          ExplicitLeft = 411
                        end
                        inherited seNumber: TJvSpinEdit
                          Height = 27
                          StyleElements = [seFont, seClient, seBorder]
                          ExplicitHeight = 27
                        end
                      end
                      inherited Grid: TRbwDataGrid4
                        Width = 525
                        Height = 212
                        ExplicitWidth = 525
                        ExplicitHeight = 212
                      end
                    end
                  end
                  inherited grpObsComparisons: TGroupBox
                    Top = 272
                    Width = 529
                    ExplicitTop = 272
                    ExplicitWidth = 529
                    inherited frameObsComparisons: TframeGrid
                      Top = 21
                      Width = 525
                      Height = 149
                      ExplicitTop = 21
                      ExplicitWidth = 525
                      ExplicitHeight = 149
                      inherited Panel: TPanel
                        Top = 114
                        Width = 525
                        StyleElements = [seFont, seClient, seBorder]
                        ExplicitTop = 114
                        ExplicitWidth = 525
                        inherited lbNumber: TLabel
                          Width = 262
                          Height = 19
                          StyleElements = [seFont, seClient, seBorder]
                          ExplicitWidth = 262
                          ExplicitHeight = 19
                        end
                        inherited sbAdd: TSpeedButton
                          Left = 339
                          ExplicitLeft = 355
                        end
                        inherited sbInsert: TSpeedButton
                          Left = 366
                          ExplicitLeft = 383
                        end
                        inherited sbDelete: TSpeedButton
                          Left = 393
                          ExplicitLeft = 411
                        end
                        inherited seNumber: TJvSpinEdit
                          Height = 27
                          StyleElements = [seFont, seClient, seBorder]
                          ExplicitHeight = 27
                        end
                      end
                      inherited Grid: TRbwDataGrid4
                        Width = 525
                        Height = 114
                        ExplicitWidth = 525
                        ExplicitHeight = 114
                      end
                    end
                  end
                end
              end
            end
          end
        end
        object jvspUZF: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'UZF_Object_Pane'
          Caption = 'jvspUZF'
          inline frameScreenObjectUZF: TframeScreenObjectNoParam
            Left = 0
            Top = 0
            Width = 537
            Height = 332
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 332
            inherited pnlBottom: TPanel
              Top = 286
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 286
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Top = 9
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameScreenObjectUZFseNumberOfTimesChange
                ExplicitTop = 9
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 261
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 261
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 209
                ColCount = 6
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSetEditText = frameScreenObjectUZFdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameScreenObjectUZFdgModflowBoundaryEndUpdate
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                ExplicitWidth = 535
                ExplicitHeight = 209
                ColWidths = (
                  64
                  64
                  64
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
          object pnlUzfGage: TPanel
            Left = 0
            Top = 332
            Width = 537
            Height = 146
            Align = alBottom
            TabOrder = 1
            object cbUzfGage1: TCheckBox
              Left = 6
              Top = 8
              Width = 530
              Height = 33
              Caption = 
                'Print time, groundwater head, and thickness of unsaturated zone,' +
                ' and cumulative volumes (IUZOPT = 1)'
              TabOrder = 0
              WordWrap = True
              OnClick = cbUzfGage1Click
            end
            object cbUzfGage2: TCheckBox
              Left = 6
              Top = 51
              Width = 563
              Height = 18
              Caption = 'Also print rates (IUZOPT = 2)'
              Enabled = False
              TabOrder = 1
              OnClick = cbUzfGage2Click
            end
            object cbUzfGage3: TCheckBox
              Left = 6
              Top = 75
              Width = 530
              Height = 57
              Caption = 
                'Print time, groundwater head, thickness of unsaturated zone, fol' +
                'lowed by a series of depths and water contents in the unsaturate' +
                'd zone (IUZOPT = 3)'
              TabOrder = 2
              WordWrap = True
              OnClick = cbUzfGage3Click
            end
          end
        end
        object jvspHOB: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'HOB_Head_Observation_Package_P'
          Caption = 'jvspHOB'
          inline frameHeadObservations: TframeHeadObservations
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            HelpType = htKeyword
            HelpKeyword = 'HOB_Head_Observation_Package_P'
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pcData: TJvPageControl
              Width = 537
              Height = 344
              ExplicitWidth = 537
              ExplicitHeight = 344
              inherited tabTimes: TTabSheet
                ExplicitTop = 30
                ExplicitWidth = 529
                ExplicitHeight = 310
                inherited Panel5: TPanel
                  Width = 529
                  Height = 41
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 529
                  ExplicitHeight = 41
                  inherited rdeMultiValueEdit: TRbwDataEntry
                    Height = 29
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitHeight = 29
                  end
                  inherited comboMultiStatFlag: TJvImageComboBox
                    Height = 29
                    StyleElements = [seFont, seClient, seBorder]
                    ItemHeight = 23
                    ExplicitHeight = 29
                  end
                end
                inherited Panel2: TPanel
                  Top = 271
                  Width = 529
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 271
                  ExplicitWidth = 529
                  inherited lblNumberOfTimes: TLabel
                    Width = 119
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 119
                    ExplicitHeight = 19
                  end
                  inherited seTimes: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnChange = frameHeadObservationsseTimesChange
                    ExplicitHeight = 27
                  end
                  inherited btnDeleteValue: TButton
                    Left = 464
                    ExplicitLeft = 464
                  end
                  inherited btnInsertValue: TButton
                    Left = 401
                    ExplicitLeft = 401
                  end
                end
                inherited rdgObservations: TRbwDataGrid4
                  Top = 41
                  Width = 529
                  Height = 230
                  OnSetEditText = frameHeadObservationsrdgHeadsSetEditText
                  ExplicitTop = 41
                  ExplicitWidth = 529
                  ExplicitHeight = 230
                  ColWidths = (
                    64
                    64
                    64
                    64
                    64)
                  RowHeights = (
                    24
                    24)
                end
              end
              inherited tabLayers: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 182
                inherited Panel4: TPanel
                  Top = 143
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 143
                  inherited lblNumberOfLayers: TLabel
                    Width = 125
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 125
                    ExplicitHeight = 19
                  end
                  inherited seLayers: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitHeight = 27
                  end
                end
                inherited Panel6: TPanel
                  StyleElements = [seFont, seClient, seBorder]
                  inherited rdeMultiLayerEdit: TRbwDataEntry
                    StyleElements = [seFont, seClient, seBorder]
                  end
                end
                inherited rdgLayers: TRbwDataGrid4
                  Height = 108
                  ExplicitWidth = 533
                  ExplicitHeight = 108
                  ColWidths = (
                    64
                    64)
                  RowHeights = (
                    24
                    24)
                end
              end
            end
            inherited pnlCaption: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
            end
            inherited pnlName: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited lblTreatment: TLabel
                Top = 0
                Width = 122
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitTop = 0
                ExplicitWidth = 122
                ExplicitHeight = 19
              end
              inherited edObsName: TLabeledEdit
                Height = 27
                EditLabel.Width = 131
                EditLabel.Height = 19
                EditLabel.ExplicitLeft = 8
                EditLabel.ExplicitTop = 0
                EditLabel.ExplicitWidth = 131
                EditLabel.ExplicitHeight = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited comboTreatment: TComboBox
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
            end
          end
        end
        object jvspHFB: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'HFB_Horizontal_Flow_Barrier_Pane'
          Caption = 'jvspHFB'
          inline frameHfbBoundary: TframeHfbScreenObject
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            Enabled = False
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited lblParameterName: TLabel
              Width = 121
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 121
              ExplicitHeight = 19
            end
            inherited lblHydraulicConductivity: TLabel
              Width = 214
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 214
              ExplicitHeight = 19
            end
            inherited lblBarrierThickness: TLabel
              Width = 124
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 124
              ExplicitHeight = 19
            end
            inherited comboHfbParameters: TJvImageComboBox
              Height = 29
              StyleElements = [seFont, seClient, seBorder]
              ItemHeight = 23
              ExplicitHeight = 29
            end
            inherited rgAngleAdjustment: TRadioGroup
              Width = 553
              ExplicitWidth = 553
            end
            inherited pnlCaption: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
            end
            inherited edHydraulicConductivity: TRbwEdit
              Height = 27
              StyleElements = [seFont, seClient, seBorder]
              ExplicitHeight = 27
            end
            inherited edBarrierThickness: TRbwEdit
              Height = 27
              TabOrder = 4
              StyleElements = [seFont, seClient, seBorder]
              ExplicitHeight = 27
            end
            inherited btnEditHfbHydraulicConductivityFormula: TButton
              Top = 58
              OnClick = frameHfbBoundarybtnEditHfbHydraulicConductivityFormulaClick
              ExplicitTop = 58
            end
            inherited btnEditHfbThicknessyFormula: TButton
              Top = 89
              TabOrder = 5
              OnClick = frameHfbBoundarybtnEditHfbThicknessyFormulaClick
              ExplicitTop = 89
            end
          end
        end
        object jvspModpath: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'MODPATH_MODPATH_Object_Options'
          Caption = 'jvspModpath'
          inline frameIface: TframeIface
            Left = 8
            Top = 0
            Width = 489
            Height = 169
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Arial'
            Font.Pitch = fpVariable
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            TabStop = True
            ExplicitLeft = 8
            ExplicitWidth = 489
            ExplicitHeight = 169
            inherited gbIface: TGroupBox
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 483
              Height = 163
              ExplicitLeft = 3
              ExplicitTop = 3
              ExplicitWidth = 483
              ExplicitHeight = 163
              inherited lblMessage: TLabel
                Left = 296
                Top = 33
                Width = 162
                Height = 54
                Visible = True
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 296
                ExplicitTop = 33
                ExplicitWidth = 162
                ExplicitHeight = 54
              end
              inherited rbBottom: TJvRadioButton
                Top = 135
                OnClick = frameIfacerbHorizontalClick
                ExplicitTop = 135
              end
              inherited rbFront: TJvRadioButton
                Top = 111
                OnClick = frameIfacerbHorizontalClick
                ExplicitTop = 111
              end
              inherited rbRight: TJvRadioButton
                Left = 102
                Top = 87
                OnClick = frameIfacerbHorizontalClick
                ExplicitLeft = 102
                ExplicitTop = 87
              end
              inherited rbLeft: TJvRadioButton
                OnClick = frameIfacerbHorizontalClick
              end
              inherited rbTop: TJvRadioButton
                Left = 103
                Top = 135
                OnClick = frameIfacerbHorizontalClick
                ExplicitLeft = 103
                ExplicitTop = 135
              end
              inherited rbBack: TJvRadioButton
                Left = 103
                Top = 111
                OnClick = frameIfacerbHorizontalClick
                ExplicitLeft = 103
                ExplicitTop = 111
              end
              inherited rbInternal: TJvRadioButton
                TabOrder = 2
                OnClick = frameIfacerbHorizontalClick
              end
              inherited rbHorizontal: TJvRadioButton
                Width = 246
                OnClick = frameIfacerbHorizontalClick
                ExplicitWidth = 246
              end
              inherited glsvViewer: TGLSceneViewer
                Left = 287
                Top = 21
                Height = 132
                FieldOfView = 105.706626892089800000
                Visible = False
                TabOrder = 1
                ExplicitLeft = 287
                ExplicitTop = 21
                ExplicitHeight = 132
              end
            end
            inherited glsIface: TGLScene
              inherited GLDummyCube1: TGLDummyCube
                Direction.Coordinates = {CAC431BE27DC82BB5C1C7C3F00000000}
                Scale.Coordinates = {00000041000000410000004100000000}
                Up.Coordinates = {EC65BCBCABEE7F3FA89E092E00000000}
                inherited LeftFace: TGLPlane
                  Material.BackProperties.Diffuse.Color = {000000000000803FF8FEFE3E0000803F}
                  Material.FrontProperties.Diffuse.Color = {F8FEFE3E0000803F000000000000803F}
                  Direction.Coordinates = {2EBDBBB300000000000080BF00000000}
                  Position.Coordinates = {00000000000000000000C03F0000803F}
                end
                inherited GLCube1: TGLCube
                  Position.Coordinates = {00000000000000000000C0BF0000803F}
                end
                inherited GLLightSource2: TGLLightSource
                  Position.Coordinates = {0000E0400000C040000000C10000803F}
                  Specular.Color = {6666E63E6666E63E6666E63E0000803F}
                  inherited GLSphere1: TGLSphere
                    Material.FrontProperties.Diffuse.Color = {000000000000003F0000003F0000803F}
                    Scale.Coordinates = {9A99993E9A99993E9A99993E00000000}
                  end
                end
                inherited FrontFace: TGLPlane
                  Direction.Coordinates = {0000803F000000000000000000000000}
                  Position.Coordinates = {0000003F000000000000803F0000803F}
                end
                inherited TopFace: TGLPlane
                  Direction.Coordinates = {000000000000803F0000000000000000}
                  Position.Coordinates = {000000000000003F0000803F0000803F}
                  Up.Coordinates = {0000000000000000000080BF00000000}
                end
                inherited BottomFace: TGLPlane
                  Direction.Coordinates = {004474990000803F2EBD3BB400000000}
                  Position.Coordinates = {00000000000000BF0000803F0000803F}
                  Up.Coordinates = {00004A1D2EBD3BB4000080BF00000000}
                end
                inherited RightFace: TGLPlane
                  Direction.Coordinates = {0000000000000000000080BF00000000}
                  Position.Coordinates = {00000000000000000000003F0000803F}
                end
                inherited BackFace: TGLPlane
                  Material.BackProperties.Diffuse.Color = {F8FEFE3E0000803F000000000000803F}
                  Material.FrontProperties.Diffuse.Color = {F8FEFE3E0000803F000000000000803F}
                  Direction.Coordinates = {0000803F000000000000000000000000}
                  Position.Coordinates = {000000BF000000000000803F0000803F}
                end
                inherited CentralSphere: TGLSphere
                  Material.BackProperties.Diffuse.Color = {000000000000803FF8FEFE3E0000803F}
                  Material.FrontProperties.Diffuse.Color = {000000000000803FF8FEFE3E0000803F}
                  Position.Coordinates = {00000000000000000000803F0000803F}
                  Scale.Coordinates = {9A99993E9A99993E9A99993E00000000}
                end
                inherited Tube1: TGLCylinder
                  Material.FrontProperties.Diffuse.Color = {0000003F0000003F0000003F0000803F}
                  Position.Coordinates = {0000003F000000000000003F0000803F}
                end
                inherited Tube2: TGLCylinder
                  Material.FrontProperties.Diffuse.Color = {0000003F0000003F0000003F0000803F}
                  Position.Coordinates = {000000000000003F0000003F0000803F}
                  Up.Coordinates = {000080BF2EBD3BB30000000000000000}
                end
                inherited Tube3: TGLCylinder
                  Material.FrontProperties.Diffuse.Color = {0000003F0000003F0000003F0000803F}
                  Direction.Coordinates = {000000000000803F2EBD3BB300000000}
                  Position.Coordinates = {0000003F0000003F0000803F0000803F}
                  Up.Coordinates = {000000002EBD3BB3000080BF00000000}
                end
              end
              inherited GLCamera1: TGLCamera
                Position.Coordinates = {0000F04100002041000000000000803F}
              end
            end
          end
          inline frameModpathParticles: TframeModpathParticles
            Left = 11
            Top = 172
            Width = 555
            Height = 316
            TabOrder = 1
            TabStop = True
            ExplicitLeft = 11
            ExplicitTop = 172
            ExplicitWidth = 555
            ExplicitHeight = 316
            inherited gbParticles: TJvGroupBox
              Width = 555
              Height = 316
              OnCheckBoxClick = frameModpathParticlesgbParticlesCheckBoxClick
              ExplicitWidth = 555
              ExplicitHeight = 316
              DesignSize = (
                555
                316)
              inherited lblTimeCount: TLabel
                Width = 43
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 43
                ExplicitHeight = 19
              end
              inherited lblMessage: TLabel
                Top = 150
                Height = 133
                Visible = True
                StyleElements = [seFont, seClient, seBorder]
                ExplicitTop = 150
                ExplicitHeight = 133
              end
              inherited rgChoice: TRadioGroup
                OnClick = frameModpathParticlesrgChoiceClick
              end
              inherited GLSceneViewer1: TGLSceneViewer
                Visible = False
              end
              inherited plParticlePlacement: TJvPageList
                inherited jvspGrid: TJvStandardPage
                  inherited lblX: TLabel
                    Width = 138
                    Height = 38
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 138
                    ExplicitHeight = 38
                  end
                  inherited lblY: TLabel
                    Width = 138
                    Height = 38
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 138
                    ExplicitHeight = 38
                  end
                  inherited lblZ: TLabel
                    Width = 138
                    Height = 38
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 138
                    ExplicitHeight = 38
                  end
                  inherited cbLeftFace: TCheckBox
                    OnClick = frameModpathParticlescbLeftFaceClick
                  end
                  inherited cbRightFace: TCheckBox
                    OnClick = frameModpathParticlescbLeftFaceClick
                  end
                  inherited cbFrontFace: TCheckBox
                    OnClick = frameModpathParticlescbLeftFaceClick
                  end
                  inherited cbBackFace: TCheckBox
                    OnClick = frameModpathParticlescbLeftFaceClick
                  end
                  inherited cbBottomFace: TCheckBox
                    OnClick = frameModpathParticlescbLeftFaceClick
                  end
                  inherited cbTopFace: TCheckBox
                    OnClick = frameModpathParticlescbLeftFaceClick
                  end
                  inherited cbInternal: TCheckBox
                    OnClick = frameModpathParticlescbLeftFaceClick
                  end
                  inherited seX: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnChange = frameModpathParticlesseXChange
                    ExplicitHeight = 27
                  end
                  inherited seY: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnChange = frameModpathParticlesseXChange
                    ExplicitHeight = 27
                  end
                  inherited seZ: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnChange = frameModpathParticlesseXChange
                    ExplicitHeight = 27
                  end
                end
                inherited jvspCylinder: TJvStandardPage
                  inherited lblCylParticleCount: TLabel
                    Width = 147
                    Height = 38
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 147
                    ExplicitHeight = 38
                  end
                  inherited lblClylLayerCount: TLabel
                    Width = 130
                    Height = 38
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 130
                    ExplicitHeight = 38
                  end
                  inherited lblCylRadius: TLabel
                    Width = 111
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 111
                    ExplicitHeight = 19
                  end
                  inherited rgCylinderOrientation: TRadioGroup
                    OnClick = frameModpathParticlesrgCylinderOrientationClick
                  end
                  inherited seCylParticleCount: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnChange = frameModpathParticlesseXChange
                    ExplicitHeight = 27
                  end
                  inherited seCylLayerCount: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnChange = frameModpathParticlesseXChange
                    ExplicitHeight = 27
                  end
                  inherited seCylRadius: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnChange = frameModpathParticlesseCylRadiusClick
                    ExplicitHeight = 27
                  end
                end
                inherited jvspSphere: TJvStandardPage
                  inherited lblSpherParticleCount: TLabel
                    Width = 147
                    Height = 38
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 147
                    ExplicitHeight = 38
                  end
                  inherited lblSpherelLayerCount: TLabel
                    Width = 130
                    Height = 38
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 130
                    ExplicitHeight = 38
                  end
                  inherited lblSphereRadius: TLabel
                    Width = 103
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 103
                    ExplicitHeight = 19
                  end
                  inherited rgSphereOrientation: TRadioGroup
                    OnClick = frameModpathParticlesrgCylinderOrientationClick
                  end
                  inherited seSphereParticleCount: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnChange = frameModpathParticlesseXChange
                    ExplicitHeight = 27
                  end
                  inherited seSphereLayerCount: TJvSpinEdit
                    Top = 120
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnChange = frameModpathParticlesseXChange
                    ExplicitTop = 120
                    ExplicitHeight = 27
                  end
                  inherited seSphereRadius: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnChange = frameModpathParticlesseCylRadiusClick
                    ExplicitHeight = 27
                  end
                end
                inherited jvspIndividual: TJvStandardPage
                  inherited rdgSpecific: TRbwDataGrid4
                    FixedCols = 0
                    OnSetEditText = frameModpathParticlesrdgSpecificSetEditText
                    ColWidths = (
                      20
                      20
                      20
                      20)
                    RowHeights = (
                      24
                      24)
                  end
                  inherited pnlBottom: TPanel
                    StyleElements = [seFont, seClient, seBorder]
                    inherited lblCount: TLabel
                      Width = 43
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 43
                      ExplicitHeight = 19
                    end
                    inherited seSpecificParticleCount: TJvSpinEdit
                      Height = 27
                      StyleElements = [seFont, seClient, seBorder]
                      OnChange = frameModpathParticlesseSpecificParticleCountChange
                      ExplicitHeight = 27
                    end
                  end
                end
              end
              inherited seTimeCount: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameModpathParticlesseTimeCountChange
                ExplicitHeight = 27
              end
              inherited rdgReleaseTimes: TRbwDataGrid4
                FixedCols = 0
                ColWidths = (
                  20
                  20)
                RowHeights = (
                  24
                  24)
              end
            end
            inherited GLScene1: TGLScene
              inherited GLDummyCube: TGLDummyCube
                Direction.Coordinates = {D36D79B2D7B35DBF010000BF00000000}
                Scale.Coordinates = {00000040000000400000004000000000}
                Up.Coordinates = {1C1DAFBEBC8FF0BE1155503F00000000}
                inherited GLLightSource1: TGLLightSource
                  Position.Coordinates = {0000204100002041000020410000803F}
                end
                inherited BottomPlane: TGLPlane
                  Material.BackProperties.Diffuse.Color = {0AD7633FD7A3F03ECDCC4C3E0000803F}
                  Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
                  Direction.Coordinates = {000000002EBDBBB3000080BF00000000}
                  Position.Coordinates = {00000000000000000000003F0000803F}
                  Up.Coordinates = {00000000000080BF2EBDBB3300000000}
                end
                inherited LeftPlane: TGLPlane
                  Material.FrontProperties.Diffuse.Color = {8FC2753FCDCC4C3FD7A3303F0000803F}
                  Direction.Coordinates = {0000803F000000002EBD3BB300000000}
                  Position.Coordinates = {000000BF00000000000000000000803F}
                end
                inherited BackPlane: TGLPlane
                  Material.FrontProperties.Diffuse.Color = {EBE0E03EE4DB5B3FE4DB5B3F0000803F}
                  Direction.Coordinates = {000000000000803F2EBD3BB300000000}
                  Position.Coordinates = {00000000000000BF000000000000803F}
                  Up.Coordinates = {000000002EBD3BB3000080BF00000000}
                end
                inherited GLCylinder1: TGLCylinder
                  Position.Coordinates = {0000003F00000000000000BF0000803F}
                end
                inherited GLCylinder2: TGLCylinder
                  Direction.Coordinates = {000000000000803F2EBD3BB300000000}
                  Position.Coordinates = {0000003F0000003F000000000000803F}
                  Up.Coordinates = {000000002EBD3BB3000080BF00000000}
                end
                inherited GLCylinder3: TGLCylinder
                  Position.Coordinates = {000000000000003F000000BF0000803F}
                  Up.Coordinates = {000080BF2EBD3BB30000000000000000}
                end
              end
              inherited GLLightSource2: TGLLightSource
                Position.Coordinates = {0000803F00000000000040400000803F}
                SpotDirection.Coordinates = {00000000000000000000803F00000000}
              end
              inherited GLCamera: TGLCamera
                Position.Coordinates = {0000000000000000000020410000803F}
              end
            end
          end
        end
        object jvspCHOB: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'CHOB_Specified_Head_Flow_ObsObjects'
          Caption = 'jvspCHOB'
          inline frameCHOB: TframeFluxObs
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            DesignSize = (
              537
              478)
            inherited lblFluxObservations: TLabel
              Width = 125
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 125
              ExplicitHeight = 19
            end
            inherited rdgObservationGroups: TRbwDataGrid4
              Top = 64
              Width = 493
              Height = 402
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
              OnSetEditText = frameCHOBrdgObservationGroupsSetEditText
              OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
              OnStateChange = frameCHOBrdgObservationGroupsStateChange
              ExplicitTop = 64
              ExplicitWidth = 493
              ExplicitHeight = 402
              RowHeights = (
                24
                24)
            end
            inherited btnAddOrRemoveFluxObservations: TButton
              Top = 33
              OnClick = frameFluxObsbtnAddOrRemoveFluxObservationsClick
              ExplicitTop = 33
            end
          end
        end
        object jvspDROB: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'DROB_Drain_ObservationObjects'
          Caption = 'jvspDROB'
          inline frameDROB: TframeFluxObs
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            DesignSize = (
              537
              478)
            inherited lblFluxObservations: TLabel
              Width = 125
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 125
              ExplicitHeight = 19
            end
            inherited rdgObservationGroups: TRbwDataGrid4
              Width = 493
              Height = 434
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
              OnSetEditText = frameDROBrdgObservationGroupsSetEditText
              OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
              OnStateChange = frameDROBrdgObservationGroupsStateChange
              ExplicitWidth = 493
              ExplicitHeight = 434
              RowHeights = (
                24
                24)
            end
            inherited btnAddOrRemoveFluxObservations: TButton
              OnClick = frameFluxObsbtnAddOrRemoveFluxObservationsClick
            end
          end
        end
        object jvspGBOB: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'GBOB_General_Head_Boundary_ObsObjects'
          Caption = 'jvspGBOB'
          inline frameGBOB: TframeFluxObs
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            DesignSize = (
              537
              478)
            inherited lblFluxObservations: TLabel
              Width = 125
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 125
              ExplicitHeight = 19
            end
            inherited rdgObservationGroups: TRbwDataGrid4
              Top = 64
              Width = 493
              Height = 403
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
              OnSetEditText = frameGBOBrdgObservationGroupsSetEditText
              OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
              OnStateChange = frameGBOBrdgObservationGroupsStateChange
              ExplicitTop = 64
              ExplicitWidth = 493
              ExplicitHeight = 403
              RowHeights = (
                24
                24)
            end
            inherited btnAddOrRemoveFluxObservations: TButton
              Top = 33
              OnClick = frameFluxObsbtnAddOrRemoveFluxObservationsClick
              ExplicitTop = 33
            end
          end
        end
        object jvspRVOB: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'RVOB_River_ObservationObjects'
          Caption = 'jvspRVOB'
          inline frameRVOB: TframeFluxObs
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            DesignSize = (
              537
              478)
            inherited lblFluxObservations: TLabel
              Width = 125
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 125
              ExplicitHeight = 19
            end
            inherited rdgObservationGroups: TRbwDataGrid4
              Width = 485
              Height = 406
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
              OnSetEditText = frameRVOBrdgObservationGroupsSetEditText
              OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
              OnStateChange = frameRVOBrdgObservationGroupsStateChange
              ExplicitWidth = 485
              ExplicitHeight = 406
              RowHeights = (
                24
                24)
            end
            inherited btnAddOrRemoveFluxObservations: TButton
              OnClick = frameFluxObsbtnAddOrRemoveFluxObservationsClick
            end
          end
        end
        object jvspGAGE: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'GAGE_for_SFR'
          Caption = 'jvspGAGE'
          object pnlGageTop: TPanel
            Left = 0
            Top = 0
            Width = 537
            Height = 65
            Align = alTop
            TabOrder = 0
            object lblGageCaption: TLabel
              Left = 11
              Top = 10
              Width = 110
              Height = 19
              Caption = 'lblGageCaption'
            end
          end
          object pcGage: TPageControl
            Left = 0
            Top = 65
            Width = 537
            Height = 413
            ActivePage = tabGageObservations
            Align = alClient
            TabOrder = 1
            object tabGageTypes: TTabSheet
              Caption = 'Gage Types'
              object cbGage1: TCheckBox
                Left = 8
                Top = 55
                Width = 530
                Height = 17
                Caption = 'Default values plus depth, width, and flow at midpoint'
                TabOrder = 0
                OnClick = cbGageStandardClick
              end
              object cbGage2: TCheckBox
                Left = 8
                Top = 86
                Width = 553
                Height = 43
                Caption = 
                  'Default values plus streambed conductance for the reach, head di' +
                  'fference across streambed, and hydraulic gradient across streamb' +
                  'ed.'
                TabOrder = 1
                WordWrap = True
                OnClick = cbGageStandardClick
              end
              object cbGage3: TCheckBox
                Left = 8
                Top = 135
                Width = 530
                Height = 17
                Caption = 'Default values plus solute load in stream (if GWT is active). '
                TabOrder = 2
                OnClick = cbGageStandardClick
              end
              object cbGage5: TCheckBox
                Left = 8
                Top = 158
                Width = 553
                Height = 59
                Caption = 
                  'Use for diversions to provide a listing of time, stage, flow div' +
                  'erted, maximum assigned diversion rate, flow at end of upstream ' +
                  'segment prior to diversion, solute concentration, and solute loa' +
                  'd.'
                TabOrder = 3
                WordWrap = True
                OnClick = cbGageStandardClick
              end
              object cbGage6: TCheckBox
                Left = 8
                Top = 223
                Width = 553
                Height = 58
                Caption = 
                  'Used for unsaturated flow routing to provide a listing of time, ' +
                  'stage, ground-water head, streambed seepage, change in unsaturat' +
                  'ed storage, and recharge.'
                TabOrder = 4
                WordWrap = True
                OnClick = cbGageStandardClick
              end
              object cbGage7: TCheckBox
                Left = 3
                Top = 287
                Width = 553
                Height = 34
                Caption = 
                  'Used for unsaturated flow routing to provide a listing of time a' +
                  'nd the unsaturated water content profile beneath the stream.'
                TabOrder = 5
                WordWrap = True
                OnClick = cbGageStandardClick
              end
              object cbGageStandard: TCheckBox
                Left = 3
                Top = 24
                Width = 537
                Height = 17
                Caption = 
                  'Standard default - time, stage, outflow, and solute concentratio' +
                  'n.'
                TabOrder = 6
                OnClick = cbGageStandardClick
              end
            end
            object tabGageObservations: TTabSheet
              Caption = 'Calibration'
              ImageIndex = 1
              inline frameGagePestObs: TframePestObs
                Left = 0
                Top = 0
                Width = 529
                Height = 379
                Align = alClient
                TabOrder = 0
                ExplicitWidth = 529
                ExplicitHeight = 379
                inherited splObservations: TSplitter
                  Top = 202
                  Width = 529
                  ExplicitTop = 199
                  ExplicitWidth = 573
                end
                inherited grpDirectObs: TGroupBox
                  Width = 529
                  Height = 202
                  ExplicitWidth = 529
                  ExplicitHeight = 202
                  inherited frameObservations: TframeGrid
                    Top = 21
                    Width = 525
                    Height = 179
                    ExplicitTop = 21
                    ExplicitWidth = 525
                    ExplicitHeight = 179
                    inherited Panel: TPanel
                      Top = 147
                      Width = 525
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitTop = 147
                      ExplicitWidth = 525
                      inherited lbNumber: TLabel
                        Width = 218
                        Height = 19
                        StyleElements = [seFont, seClient, seBorder]
                        ExplicitWidth = 218
                        ExplicitHeight = 19
                      end
                      inherited sbAdd: TSpeedButton
                        Left = 347
                        ExplicitLeft = 365
                      end
                      inherited sbInsert: TSpeedButton
                        Left = 376
                        ExplicitLeft = 394
                      end
                      inherited sbDelete: TSpeedButton
                        Left = 405
                        ExplicitLeft = 423
                      end
                      inherited seNumber: TJvSpinEdit
                        Height = 27
                        StyleElements = [seFont, seClient, seBorder]
                        ExplicitHeight = 27
                      end
                    end
                    inherited Grid: TRbwDataGrid4
                      Width = 525
                      Height = 147
                      ExplicitWidth = 525
                      ExplicitHeight = 147
                    end
                  end
                end
                inherited grpObsComparisons: TGroupBox
                  Top = 207
                  Width = 529
                  ExplicitTop = 207
                  ExplicitWidth = 529
                  inherited frameObsComparisons: TframeGrid
                    Top = 21
                    Width = 525
                    Height = 149
                    ExplicitTop = 21
                    ExplicitWidth = 525
                    ExplicitHeight = 149
                    inherited Panel: TPanel
                      Top = 114
                      Width = 525
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitTop = 114
                      ExplicitWidth = 525
                      inherited lbNumber: TLabel
                        Width = 262
                        Height = 19
                        StyleElements = [seFont, seClient, seBorder]
                        ExplicitWidth = 262
                        ExplicitHeight = 19
                      end
                      inherited sbAdd: TSpeedButton
                        Left = 347
                        ExplicitLeft = 365
                      end
                      inherited sbInsert: TSpeedButton
                        Left = 376
                        ExplicitLeft = 394
                      end
                      inherited sbDelete: TSpeedButton
                        Left = 405
                        ExplicitLeft = 423
                      end
                      inherited seNumber: TJvSpinEdit
                        Height = 27
                        StyleElements = [seFont, seClient, seBorder]
                        ExplicitHeight = 27
                      end
                    end
                    inherited Grid: TRbwDataGrid4
                      Width = 525
                      Height = 114
                      ExplicitWidth = 525
                      ExplicitHeight = 114
                    end
                  end
                end
              end
            end
          end
        end
        object jvspMNW2: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'MNW2_Object_Pane'
          Caption = 'jvspMNW2'
          inline frameMNW2: TframeScreenObjectMNW2
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Arial'
            Font.Pitch = fpVariable
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlCaption: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
            end
            inherited pcMnw2: TPageControl
              Width = 537
              Height = 456
              ActivePage = frameMNW2.tabDischargeAdjustment
              OnChange = frameMNW2pcMnw2Change
              ExplicitWidth = 537
              ExplicitHeight = 456
              inherited tabBasic: TTabSheet
                inherited lblWellId: TLabel
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited lblLossType: TLabel
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited lblPartialPenetration: TLabel
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited lblZPump: TLabel
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited edWellId: TRbwEdit
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited comboLossType: TJvImageComboBox
                  StyleElements = [seFont, seClient, seBorder]
                  ItemIndex = -1
                end
                inherited edPartialPenetration: TJvComboEdit
                  Left = 227
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                  ExplicitLeft = 227
                end
                inherited framePumpLocationMethod: TframeLocationMethod
                  Width = 325
                  Font.Pitch = fpVariable
                  ExplicitWidth = 325
                  inherited lblLocationMethod: TLabel
                    StyleElements = [seFont, seClient, seBorder]
                  end
                  inherited pcLocationChoice: TJvPageControl
                    Left = 325
                    Width = 0
                    ExplicitLeft = 325
                    ExplicitWidth = 0
                    inherited tabNone: TTabSheet
                      ExplicitWidth = 0
                    end
                    inherited tabCell: TTabSheet
                      inherited lblCol: TLabel
                        StyleElements = [seFont, seClient, seBorder]
                      end
                      inherited lblRow: TLabel
                        StyleElements = [seFont, seClient, seBorder]
                      end
                      inherited lblLay: TLabel
                        StyleElements = [seFont, seClient, seBorder]
                      end
                      inherited rdeLay: TRbwDataEntry
                        StyleElements = [seFont, seClient, seBorder]
                      end
                      inherited rdeRow: TRbwDataEntry
                        StyleElements = [seFont, seClient, seBorder]
                      end
                      inherited rdeCol: TRbwDataEntry
                        StyleElements = [seFont, seClient, seBorder]
                      end
                    end
                    inherited tabLocation: TTabSheet
                      inherited lblX: TLabel
                        StyleElements = [seFont, seClient, seBorder]
                      end
                      inherited lblY: TLabel
                        StyleElements = [seFont, seClient, seBorder]
                      end
                      inherited lblZ: TLabel
                        StyleElements = [seFont, seClient, seBorder]
                      end
                      inherited rdeX: TRbwDataEntry
                        StyleElements = [seFont, seClient, seBorder]
                      end
                      inherited rdeY: TRbwDataEntry
                        StyleElements = [seFont, seClient, seBorder]
                      end
                      inherited rdeZ: TRbwDataEntry
                        StyleElements = [seFont, seClient, seBorder]
                      end
                    end
                    inherited tabObject: TTabSheet
                      inherited comboObject: TComboBox
                        Left = 0
                        Width = 243
                        StyleElements = [seFont, seClient, seBorder]
                        ExplicitLeft = 0
                        ExplicitWidth = 243
                      end
                    end
                  end
                  inherited comboLocationChoice: TJvImageComboBox
                    StyleElements = [seFont, seClient, seBorder]
                    Font.Pitch = fpVariable
                  end
                end
                inherited rdeZPump: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited gbMNWI: TGroupBox
                  Width = 530
                  Height = 124
                  ExplicitWidth = 530
                  ExplicitHeight = 124
                  inherited cbSaveExternal: TCheckBox
                    Width = 530
                    Height = 41
                    WordWrap = True
                    ExplicitWidth = 530
                    ExplicitHeight = 41
                  end
                  inherited cbSaveInternal: TCheckBox
                    Top = 91
                    ExplicitTop = 91
                  end
                end
              end
              inherited tabLossControls: TTabSheet
                inherited lblWellRadius: TLabel
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited lblSkinRadius: TLabel
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited lblBCoefficient: TLabel
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited lblCCoefficient: TLabel
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited lblPCoefficient: TLabel
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited lblCellToWellConductance: TLabel
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited lblKSkin: TLabel
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited edWellRadius: TJvComboEdit
                  Width = 407
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                  ExplicitWidth = 407
                end
                inherited edSkinRadius: TJvComboEdit
                  Width = 389
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                  ExplicitWidth = 389
                end
                inherited edKSkin: TJvComboEdit
                  Width = 287
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                  ExplicitWidth = 287
                end
                inherited edBCoefficient: TJvComboEdit
                  Width = 314
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                  ExplicitWidth = 314
                end
                inherited edCCoefficient: TJvComboEdit
                  Width = 287
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                  ExplicitWidth = 287
                end
                inherited edPCoefficient: TJvComboEdit
                  Width = 415
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                  ExplicitWidth = 415
                end
                inherited edCellToWellConductance: TJvComboEdit
                  Width = 301
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                  ExplicitWidth = 301
                end
              end
              inherited tabDischargeAdjustment: TTabSheet
                ExplicitWidth = 529
                ExplicitHeight = 423
                DesignSize = (
                  529
                  423)
                inherited lblReferenceHead: TLabel
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited lblLiftQ0: TLabel
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited lblLiftQMax: TLabel
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited lblWellTolerance: TLabel
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited lflLiftTableRows: TLabel
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited lblLiftTable: TLabel
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited rdeReferenceHead: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited rdeLiftQ0: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited rdeLiftQMax: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited rdeWellTolerance: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited rdgLiftTable: TRbwDataGrid4
                  Width = 122
                  Height = 252
                  ExplicitWidth = 122
                  ExplicitHeight = 252
                  ColWidths = (
                    64
                    64)
                  RowHeights = (
                    24
                    24)
                end
                inherited seLiftTableRows: TJvSpinEdit
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited tabPumpingRate: TTabSheet
                inherited rdgTimeTable: TRbwDataGrid4
                  Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                  OnButtonClick = frameResdgModflowBoundaryButtonClick
                  RowHeights = (
                    24
                    24)
                end
                inherited Panel1: TPanel
                  StyleElements = [seFont, seClient, seBorder]
                  inherited lblTimeTableRows: TLabel
                    StyleElements = [seFont, seClient, seBorder]
                  end
                  inherited seTimeTableRows: TJvSpinEdit
                    StyleElements = [seFont, seClient, seBorder]
                  end
                  inherited btnInsertTime: TButton
                    Left = 376
                    Top = 7
                    ExplicitLeft = 376
                    ExplicitTop = 7
                  end
                  inherited btnDeleteTime: TButton
                    Left = 457
                    Top = 7
                    ExplicitLeft = 457
                    ExplicitTop = 7
                  end
                end
                inherited Panel2: TPanel
                  StyleElements = [seFont, seClient, seBorder]
                  inherited lblFormula: TLabel
                    StyleElements = [seFont, seClient, seBorder]
                  end
                  inherited lblQCUT: TLabel
                    StyleElements = [seFont, seClient, seBorder]
                  end
                  inherited rdeFormula: TRbwDataEntry
                    StyleElements = [seFont, seClient, seBorder]
                  end
                  inherited comboQCUT: TJvImageComboBox
                    StyleElements = [seFont, seClient, seBorder]
                  end
                end
              end
              inherited tabWellScreens: TTabSheet
                inherited Panel3: TPanel
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 533
                  inherited Label1: TLabel
                    StyleElements = [seFont, seClient, seBorder]
                  end
                  inherited seVerticalScreens: TJvSpinEdit
                    StyleElements = [seFont, seClient, seBorder]
                  end
                end
                inherited rdgVerticalScreens: TRbwDataGrid4
                  OnButtonClick = frameResdgModflowBoundaryButtonClick
                  ExplicitWidth = 533
                  ColWidths = (
                    64
                    64
                    64
                    64
                    64
                    64
                    64
                    64
                    64)
                  RowHeights = (
                    24
                    24)
                end
                inherited Panel4: TPanel
                  StyleElements = [seFont, seClient, seBorder]
                  inherited Label2: TLabel
                    StyleElements = [seFont, seClient, seBorder]
                  end
                  inherited lblWellScreenFormula: TLabel
                    StyleElements = [seFont, seClient, seBorder]
                  end
                  inherited rdeWellScreenFormula: TRbwDataEntry
                    StyleElements = [seFont, seClient, seBorder]
                  end
                end
              end
              inherited tabObservations: TTabSheet
                HelpType = htKeyword
                HelpKeyword = 'Calibration_TabMNW2'
                inherited framePestObsMnw2: TframePestObs
                  inherited splObservations: TSplitter
                    ExplicitTop = 244
                    ExplicitWidth = 533
                  end
                  inherited grpDirectObs: TGroupBox
                    inherited frameObservations: TframeGrid
                      inherited Panel: TPanel
                        StyleElements = [seFont, seClient, seBorder]
                        inherited lbNumber: TLabel
                          StyleElements = [seFont, seClient, seBorder]
                        end
                        inherited sbAdd: TSpeedButton
                          Left = 289
                          ExplicitLeft = 350
                        end
                        inherited sbInsert: TSpeedButton
                          Left = 312
                          ExplicitLeft = 378
                        end
                        inherited sbDelete: TSpeedButton
                          Left = 335
                          ExplicitLeft = 406
                        end
                        inherited seNumber: TJvSpinEdit
                          StyleElements = [seFont, seClient, seBorder]
                        end
                      end
                    end
                  end
                  inherited grpObsComparisons: TGroupBox
                    inherited frameObsComparisons: TframeGrid
                      inherited Panel: TPanel
                        StyleElements = [seFont, seClient, seBorder]
                        inherited lbNumber: TLabel
                          StyleElements = [seFont, seClient, seBorder]
                        end
                        inherited sbAdd: TSpeedButton
                          Left = 289
                          ExplicitLeft = 350
                        end
                        inherited sbInsert: TSpeedButton
                          Left = 312
                          ExplicitLeft = 378
                        end
                        inherited sbDelete: TSpeedButton
                          Left = 335
                          ExplicitLeft = 406
                        end
                        inherited seNumber: TJvSpinEdit
                          StyleElements = [seFont, seClient, seBorder]
                        end
                      end
                    end
                  end
                end
              end
            end
          end
        end
        object jvspHYDMOD: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'HYD_HYDMOD_Package_Pane'
          Caption = 'jvspHYDMOD'
          inline frameHydmod: TframeScreenObjectHydmod
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited lblHYDLBL: TLabel
              Width = 204
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 204
              ExplicitHeight = 19
            end
            inherited pnlCaption: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
            end
            inherited edHYDLBL: TRbwEdit
              Height = 27
              StyleElements = [seFont, seClient, seBorder]
              ExplicitHeight = 27
            end
            inherited gbBasic: TGroupBox
              Width = 217
              ExplicitWidth = 217
              inherited clbBasic: TCheckListBox
                Top = 21
                Width = 213
                Height = 87
                ItemHeight = 19
                StyleElements = [seFont, seClient, seBorder]
                OnClickCheck = frameHydmodclbBasicClickCheck
                ExplicitTop = 21
                ExplicitWidth = 213
                ExplicitHeight = 87
              end
            end
            inherited gbSubsidence: TGroupBox
              inherited lblLayerGroup: TLabel
                Width = 89
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 89
                ExplicitHeight = 19
              end
              inherited lblNoDelayBed: TLabel
                Width = 97
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 97
                ExplicitHeight = 19
              end
              inherited lblLayer: TLabel
                Width = 50
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 50
                ExplicitHeight = 19
              end
              inherited clbSub: TCheckListBox
                Top = 21
                ItemHeight = 19
                StyleElements = [seFont, seClient, seBorder]
                OnClickCheck = frameHydmodclbSubClickCheck
                ExplicitTop = 21
              end
              inherited comboLayerGroup: TJvImageComboBox
                Height = 29
                StyleElements = [seFont, seClient, seBorder]
                ItemHeight = 23
                OnChange = frameHydmodcomboLayerGroupChange
                ExplicitHeight = 29
              end
              inherited comboNoDelayBed: TJvImageComboBox
                Height = 29
                StyleElements = [seFont, seClient, seBorder]
                ItemHeight = 23
                OnChange = frameHydmodcomboNoDelayBedChange
                ExplicitHeight = 29
              end
              inherited clbLayer: TCheckListBox
                ItemHeight = 19
                StyleElements = [seFont, seClient, seBorder]
              end
            end
            inherited gbSFR: TGroupBox
              Left = 231
              Width = 306
              ExplicitLeft = 231
              ExplicitWidth = 306
              inherited clbSFR: TCheckListBox
                Top = 21
                Width = 302
                Height = 87
                ItemHeight = 19
                StyleElements = [seFont, seClient, seBorder]
                OnClickCheck = frameHydmodclbSFRClickCheck
                ExplicitTop = 21
                ExplicitWidth = 302
                ExplicitHeight = 87
              end
            end
          end
        end
        object jvspMT3DMS_SSM: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'SSM_Sink__Source_Mixing_Packa2'
          Caption = 'jvspMT3DMS_SSM'
          inline frameMT3DMS_SSM: TframeScreenObjectSsm
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameMT3DMSseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
              inherited cbSpecifiedConcentration: TCheckBox
                OnClick = frameMT3DMScbSpecifiedConcentrationClick
              end
              inherited cbMassLoading: TCheckBox
                OnClick = frameMT3DMScbMassLoadingClick
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 356
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 356
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 304
                OnSetEditText = frameMT3DMSdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameMT3DMSdgModflowBoundaryEndUpdate
                ExplicitWidth = 535
                ExplicitHeight = 304
                ColWidths = (
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
        end
        object jvspMT3DMS_TOB_Conc: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Concentration_Observations_TOB'
          Caption = 'jvspMT3DMS_TOB_Conc'
          inline frameMt3dmsTobConc: TframeConcentrationObservation
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pcData: TJvPageControl
              Width = 537
              Height = 379
              ExplicitWidth = 537
              ExplicitHeight = 379
              inherited tabTimes: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 217
                inherited Panel5: TPanel
                  Height = 41
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitHeight = 41
                  inherited rdeMultiValueEdit: TRbwDataEntry
                    Height = 29
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitHeight = 29
                  end
                  inherited comboSpeciesNames: TComboBox
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitHeight = 27
                  end
                end
                inherited Panel2: TPanel
                  Top = 170
                  Height = 47
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 170
                  ExplicitHeight = 47
                  inherited lblNumberOfTimes: TLabel
                    Width = 119
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 119
                    ExplicitHeight = 19
                  end
                  inherited seTimes: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    OnChange = frameMt3dmsTobConcseTimesChange
                    ExplicitHeight = 27
                  end
                  inherited btnDeleteValue: TButton
                    Left = 464
                    ExplicitLeft = 464
                  end
                  inherited btnInsertValue: TButton
                    Left = 401
                    ExplicitLeft = 401
                  end
                end
                inherited rdgObservations: TRbwDataGrid4
                  Top = 41
                  Height = 129
                  OnSetEditText = frameMt3dmsTobConcrdgObservationsSetEditText
                  ExplicitTop = 41
                  ExplicitHeight = 129
                  ColWidths = (
                    187
                    40
                    40
                    40
                    40
                    40)
                  RowHeights = (
                    24
                    24)
                end
              end
              inherited tabLayers: TTabSheet
                ExplicitTop = 30
                ExplicitWidth = 529
                ExplicitHeight = 345
                inherited Panel4: TPanel
                  Top = 306
                  Width = 529
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 306
                  ExplicitWidth = 529
                  inherited lblNumberOfLayers: TLabel
                    Width = 125
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 125
                    ExplicitHeight = 19
                  end
                  inherited seLayers: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitHeight = 27
                  end
                  inherited btnDeleteLayer: TButton
                    Left = 464
                    ExplicitLeft = 464
                  end
                  inherited btnInsertLayer: TButton
                    Left = 401
                    ExplicitLeft = 401
                  end
                end
                inherited Panel6: TPanel
                  Width = 529
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 529
                  inherited rdeMultiLayerEdit: TRbwDataEntry
                    StyleElements = [seFont, seClient, seBorder]
                  end
                end
                inherited rdgLayers: TRbwDataGrid4
                  Width = 529
                  Height = 271
                  ExplicitWidth = 529
                  ExplicitHeight = 271
                  ColWidths = (
                    64
                    64)
                  RowHeights = (
                    24
                    24)
                end
              end
            end
            inherited pnlCaption: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
            end
            inherited pnlName: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited lblTreatment: TLabel
                Top = 5
                Width = 122
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitTop = 5
                ExplicitWidth = 122
                ExplicitHeight = 19
              end
              inherited edObsName: TLabeledEdit
                Height = 27
                EditLabel.Width = 131
                EditLabel.Height = 19
                EditLabel.ExplicitTop = 5
                EditLabel.ExplicitWidth = 131
                EditLabel.ExplicitHeight = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited comboTreatment: TComboBox
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
            end
          end
        end
        object jvspMT3DMS_TOB_Flux: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Flux_Observations_TOB_Transpor'
          Caption = 'jvspMT3DMS_TOB_Flux'
          inline frameMt3dmsFluxObs: TframeMt3dmsFluxObs
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited lblFluxObservations: TLabel
              AlignWithMargins = True
              Left = 3
              Width = 531
              Height = 19
              Margins.Top = 8
              Align = alTop
              StyleElements = [seFont, seClient, seBorder]
              ExplicitLeft = 3
              ExplicitWidth = 125
              ExplicitHeight = 19
            end
            inherited rdgObservationGroups: TRbwDataGrid4
              Top = 64
              Width = 531
              Height = 411
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
              OnSetEditText = frameMt3dmsFluxObsrdgObservationGroupsSetEditText
              OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
              OnStateChange = frameMt3dmsFluxObsrdgObservationGroupsStateChange
              ExplicitTop = 64
              ExplicitWidth = 531
              ExplicitHeight = 411
              RowHeights = (
                24
                24)
            end
            inherited btnAddOrRemoveFluxObservations: TButton
              Top = 33
              OnClick = frameFluxObsbtnAddOrRemoveFluxObservationsClick
              ExplicitTop = 33
            end
          end
        end
        object jvspSTR: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'STR_Stream_Package_Pane'
          Caption = 'jvspSTR'
          inline frameScreenObjectSTR: TframeScreenObjectStr
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited splitHorizontal: TSplitter
              Top = 141
              Width = 537
              ExplicitTop = 141
              ExplicitWidth = 575
            end
            inherited pnlBottom: TPanel
              Top = 397
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 397
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblConductanceInterpretation: TLabel
                Width = 197
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 197
                ExplicitHeight = 19
              end
              inherited lblTimeSeriesInterpolation: TLabel
                Width = 177
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 177
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                Top = 5
                ExplicitLeft = 445
                ExplicitTop = 5
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
              inherited comboTimeSeriesInterpolation: TComboBox
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited comboFormulaInterp: TComboBox
                Left = 211
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 211
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              Height = 141
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 141
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 535
                Height = 74
                ItemHeight = 19
                ExplicitWidth = 535
                ExplicitHeight = 74
              end
              inherited pnlNumber: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblSegmentNumber: TLabel
                  Width = 170
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 170
                  ExplicitHeight = 19
                end
                inherited seSegmentNumber: TJvSpinEdit
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitHeight = 27
                end
              end
            end
            inherited pnlGrid: TPanel
              Top = 144
              Width = 537
              Height = 253
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 144
              ExplicitWidth = 537
              ExplicitHeight = 253
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 195
                OnButtonClick = frameScreenObjectSTRdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 195
                ColWidths = (
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
        end
        object jvspSTOB: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'STOB_Stream_Observation_Pane'
          Caption = 'jvspSTOB'
          inline frameSTOB: TframeFluxObs
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited lblFluxObservations: TLabel
              Width = 125
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 125
              ExplicitHeight = 19
            end
            inherited rdgObservationGroups: TRbwDataGrid4
              Top = 34
              Width = 531
              Height = 443
              OnSetEditText = frameSTOBrdgObservationGroupsSetEditText
              OnStateChange = frameSTOBrdgObservationGroupsStateChange
              ExplicitTop = 34
              ExplicitWidth = 531
              ExplicitHeight = 443
              RowHeights = (
                24
                24)
            end
          end
        end
        object jvspFhbHeads: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Heads_in_FHB_Flow_and_Head_Bou'
          Caption = 'jvspFhbHeads'
          inline frameFhbHead: TframeScreenObjectFhbHead
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
                ColWidths = (
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
        end
        object jvspFhbFlows: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Flows_in_FHB_Flow_and_Head_Bou'
          Caption = 'jvspFhbFlows'
          inline frameFhbFlow: TframeScreenObjectFhbFlow
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 402
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 402
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblConductanceInterpretation: TLabel
                Width = 166
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 166
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
              inherited comboFormulaInterp: TComboBox
                Left = 190
                Top = 47
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 190
                ExplicitTop = 47
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 377
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 377
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 325
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 325
                ColWidths = (
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
        end
        object jvspFarmWell: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Farms_Wells_in_FMP_Farm_Proces'
          Caption = 'jvspFarmWell'
          inline frameFarmWell: TframeScreenObjectCondParam
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited splitHorizontal: TSplitter
              Width = 537
              ExplicitWidth = 573
            end
            inherited pnlBottom: TPanel
              Top = 397
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 397
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblConductanceInterpretation: TLabel
                Width = 197
                Height = 19
                Caption = 'Pumping rate interpretation'
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 197
                ExplicitHeight = 19
              end
              inherited lblTimeSeriesInterpolation: TLabel
                Width = 177
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 177
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameFarmWellseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
              inherited comboTimeSeriesInterpolation: TComboBox
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited comboFormulaInterp: TComboBox
                Left = 211
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameFarmWellcomboFormulaInterpChange
                ExplicitLeft = 211
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 535
                ItemHeight = 19
                OnStateChange = frameFarmWellclbParametersStateChange
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 305
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 305
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 253
                ColCount = 5
                OnSetEditText = frameFarmWelldgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameFarmWelldgModflowBoundaryEndUpdate
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    ButtonFont.Height = -13
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Style = []
                    ButtonUsed = False
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
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
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
                    AutoAdjustCaptionRowHeights = False
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
                ExplicitWidth = 535
                ExplicitHeight = 253
                ColWidths = (
                  64
                  64
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
        end
        object jvspFarmPrecip: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Precip_in_FMP_Farm_Process'
          Caption = 'jvspFarmPrecip'
          inline frameFarmPrecip: TframeScreenObjectFmpPrecip
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
                ColWidths = (
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
        end
        object jvspFarmRefEvap: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Ref_Evap_in_FMP_Farm_Process'
          Caption = 'jvspFarmRefEvap'
          inline frameFarmRefEvap: TframeScreenObjectFmpEvap
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
                ColWidths = (
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
        end
        object jvspFarmCropID: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Crop_ID_in_FMP_Farm_Process'
          Caption = 'jvspFarmCropID'
          inline frameFarmCropID: TframeScreenObjectCropID
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
                ColWidths = (
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
        end
        object jvspCfpPipes: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'CFP_Conduit_Flow_Process_Pane'
          Caption = 'jvspCfpPipes'
          inline frameCfpPipes: TframeScreenObjectCfpPipes
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlCaption: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
            end
            inherited pcCfpPipes: TPageControl
              Width = 537
              Height = 461
              ExplicitWidth = 537
              ExplicitHeight = 461
              inherited tabProperties: TTabSheet
                ExplicitTop = 30
                ExplicitWidth = 529
                ExplicitHeight = 427
                inherited edDiameter: TLabeledEdit
                  Top = 21
                  Width = 378
                  Height = 27
                  EditLabel.Width = 166
                  EditLabel.Height = 19
                  EditLabel.ExplicitLeft = 3
                  EditLabel.ExplicitTop = -1
                  EditLabel.ExplicitWidth = 166
                  EditLabel.ExplicitHeight = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 21
                  ExplicitWidth = 378
                  ExplicitHeight = 27
                end
                inherited btnDiameter: TButton
                  Left = 387
                  OnClick = btnFormulaClick
                  ExplicitLeft = 387
                end
                inherited edTortuosity: TLabeledEdit
                  Width = 378
                  Height = 27
                  EditLabel.Width = 186
                  EditLabel.Height = 19
                  EditLabel.ExplicitLeft = 3
                  EditLabel.ExplicitTop = 50
                  EditLabel.ExplicitWidth = 186
                  EditLabel.ExplicitHeight = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 378
                  ExplicitHeight = 27
                end
                inherited btnTortuosity: TButton
                  Left = 387
                  OnClick = btnFormulaClick
                  ExplicitLeft = 387
                end
                inherited edRoughnessHeight: TLabeledEdit
                  Width = 378
                  Height = 27
                  EditLabel.Width = 217
                  EditLabel.Height = 19
                  EditLabel.ExplicitLeft = 3
                  EditLabel.ExplicitTop = 100
                  EditLabel.ExplicitWidth = 217
                  EditLabel.ExplicitHeight = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 378
                  ExplicitHeight = 27
                end
                inherited btnRoughnessHeight: TButton
                  Left = 387
                  OnClick = btnFormulaClick
                  ExplicitLeft = 387
                end
                inherited edLowerCriticalR: TLabeledEdit
                  Width = 378
                  Height = 27
                  EditLabel.Width = 500
                  EditLabel.Height = 19
                  EditLabel.ExplicitLeft = 3
                  EditLabel.ExplicitTop = 150
                  EditLabel.ExplicitWidth = 500
                  EditLabel.ExplicitHeight = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 378
                  ExplicitHeight = 27
                end
                inherited btnLowerCriticalR: TButton
                  Left = 387
                  OnClick = btnFormulaClick
                  ExplicitLeft = 387
                end
                inherited edHigherCriticalR: TLabeledEdit
                  Width = 378
                  Height = 27
                  EditLabel.Width = 504
                  EditLabel.Height = 19
                  EditLabel.ExplicitLeft = 3
                  EditLabel.ExplicitTop = 199
                  EditLabel.ExplicitWidth = 504
                  EditLabel.ExplicitHeight = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 378
                  ExplicitHeight = 27
                end
                inherited btnHigherCriticalR: TButton
                  Left = 387
                  OnClick = btnFormulaClick
                  ExplicitLeft = 387
                end
                inherited edConductancePermeability: TLabeledEdit
                  Width = 378
                  Height = 27
                  EditLabel.Width = 337
                  EditLabel.Height = 19
                  EditLabel.ExplicitLeft = 3
                  EditLabel.ExplicitTop = 249
                  EditLabel.ExplicitWidth = 337
                  EditLabel.ExplicitHeight = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 378
                  ExplicitHeight = 27
                end
                inherited btnConductancePermeability: TButton
                  Left = 387
                  OnClick = btnFormulaClick
                  ExplicitLeft = 387
                end
                inherited edElevation: TLabeledEdit
                  Width = 378
                  Height = 27
                  EditLabel.Width = 206
                  EditLabel.Height = 19
                  EditLabel.ExplicitLeft = 3
                  EditLabel.ExplicitTop = 297
                  EditLabel.ExplicitWidth = 206
                  EditLabel.ExplicitHeight = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 378
                  ExplicitHeight = 27
                end
                inherited btnElevation: TButton
                  Left = 387
                  OnClick = btnFormulaClick
                  ExplicitLeft = 387
                end
                inherited edCads: TLabeledEdit
                  Width = 378
                  Height = 27
                  EditLabel.Width = 410
                  EditLabel.Height = 19
                  EditLabel.ExplicitLeft = 3
                  EditLabel.ExplicitTop = 350
                  EditLabel.ExplicitWidth = 410
                  EditLabel.ExplicitHeight = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 378
                  ExplicitHeight = 27
                end
                inherited btnCads: TButton
                  Left = 387
                  Top = 369
                  Width = 90
                  ExplicitLeft = 387
                  ExplicitTop = 369
                  ExplicitWidth = 90
                end
              end
              inherited tabOutput: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 416
                inherited cbRecordTimeSeriesPipes: TCheckBox
                  Caption = 'Record output terms for pipes (TSAT_TUBE_NUMBERS)'
                end
                inherited cbRecordTimeSeriesNodes: TCheckBox
                  Caption = 'Record output terms for nodes (TSAT_NTS)'
                end
              end
            end
          end
        end
        object jvspCfpFixedHeads: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'CFP_Fixed_Heads_Pane'
          Caption = 'jvspCfpFixedHeads'
          inline frameCfpFixedHeads: TframeScreenObjectCfpFixed
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlCaption: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
            end
            inherited pcCfp: TPageControl
              Width = 537
              Height = 461
              ExplicitWidth = 537
              ExplicitHeight = 461
              inherited tabSteady: TTabSheet
                ExplicitTop = 30
                ExplicitWidth = 529
                ExplicitHeight = 427
                inherited lblValue2: TLabel
                  Width = 127
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 127
                  ExplicitHeight = 19
                end
                inherited lblValue3: TLabel
                  Width = 154
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 154
                  ExplicitHeight = 19
                end
                inherited lblHint: TLabel
                  Width = 377
                  Height = 152
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 377
                  ExplicitHeight = 152
                end
                inherited edValue2: TEdit
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitHeight = 27
                end
                inherited btnValue2: TButton
                  Left = 448
                  Top = 310
                  Width = 81
                  ExplicitLeft = 448
                  ExplicitTop = 310
                  ExplicitWidth = 81
                end
                inherited edValue3: TEdit
                  Left = 3
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitLeft = 3
                  ExplicitHeight = 27
                end
                inherited btnValue3: TButton
                  Left = 448
                  Width = 81
                  ExplicitLeft = 448
                  ExplicitWidth = 81
                end
                inherited btnFixedHead: TButton
                  Left = 261
                  Top = 70
                  OnClick = btnFormulaClick
                  ExplicitLeft = 261
                  ExplicitTop = 70
                end
                inherited edFixedHead: TLabeledEdit
                  Width = 96
                  Height = 27
                  EditLabel.Width = 80
                  EditLabel.Height = 19
                  EditLabel.ExplicitLeft = 159
                  EditLabel.ExplicitTop = 49
                  EditLabel.ExplicitWidth = 80
                  EditLabel.ExplicitHeight = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 96
                  ExplicitHeight = 27
                end
              end
              inherited tabTransient: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 396
                inherited frameTimeDependent: TframeGrid
                  Height = 396
                  ExplicitHeight = 396
                  inherited Panel: TPanel
                    Top = 355
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitTop = 355
                    inherited lbNumber: TLabel
                      Width = 57
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 57
                      ExplicitHeight = 19
                    end
                    inherited sbAdd: TSpeedButton
                      Left = 279
                      ExplicitLeft = 285
                    end
                    inherited sbInsert: TSpeedButton
                      Left = 329
                      ExplicitLeft = 337
                    end
                    inherited sbDelete: TSpeedButton
                      Left = 380
                      ExplicitLeft = 389
                    end
                    inherited seNumber: TJvSpinEdit
                      Height = 27
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitHeight = 27
                    end
                  end
                  inherited Grid: TRbwDataGrid4
                    Height = 355
                    ExplicitHeight = 355
                  end
                end
              end
            end
          end
        end
        object jvspCfpRechargeFraction: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'CFP_Conduit_Recharge_Pane'
          Caption = 'jvspCfpRechargeFraction'
          inline frameCfpRechargeFraction1: TframeScreenObjectCfpRechargeFraction
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 378
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 378
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 325
                ExplicitLeft = 325
              end
              inherited btnInsert: TBitBtn
                Left = 241
                ExplicitLeft = 241
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 353
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 353
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 301
                ExplicitWidth = 535
                ExplicitHeight = 301
              end
            end
          end
        end
        object jvspSWR_Rain: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Rain_in_SWR_Surface_Water_Rout'
          Caption = 'jvspSWR_Rain'
          inline frameSWR_Rain: TframeScreenObjectNoParam
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameSWR_RainseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnSetEditText = frameSWR_RaindgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameSWR_RaindgModflowBoundaryEndUpdate
                ExplicitWidth = 535
                ExplicitHeight = 355
                ColWidths = (
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
        end
        object jvspSWR_Evap: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Evaporation_in_SWR_Surface_Wat'
          Caption = 'jvspSWR_Evap'
          inline frameSWR_Evap: TframeScreenObjectNoParam
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameSWR_EvapseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnSetEditText = frameSWR_EvapdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameSWR_EvapdgModflowBoundaryEndUpdate
                ExplicitWidth = 535
                ExplicitHeight = 355
                ColWidths = (
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
        end
        object jvspSwr_LatInfl: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Lateral_Inflow_in_SWR_Surface'
          Caption = 'jvspSwr_LatInfl'
          inline frameSWR_LatInfl: TframeScreenObjectSwr
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 394
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 394
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblConductanceInterpretation: TLabel
                Width = 195
                Height = 19
                Caption = 'Lateral inflow interpretation'
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 195
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameSWR_LatInflseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
              inherited comboFormulaInterp: TComboBox
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameSWR_LatInflcomboFormulaInterpChange
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 369
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 369
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 317
                OnSetEditText = frameSWR_LatInfldgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameSWR_LatInfldgModflowBoundaryEndUpdate
                ExplicitWidth = 535
                ExplicitHeight = 317
                ColWidths = (
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
        end
        object jvspSWR_Stage: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Stage_in_SWR_Surface_Water_Rou'
          Caption = 'jvspSWR_Stage'
          inline frameSWR_Stage: TframeScreenObjectNoParam
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameSWR_StageseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnSetEditText = frameSWR_StagedgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameSWR_StagedgModflowBoundaryEndUpdate
                ExplicitWidth = 535
                ExplicitHeight = 355
                ColWidths = (
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
        end
        object jvspSWR_DirectRunoff: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Direct_Runoff_in_SWR_Surface_W'
          Caption = 'jvspSWR_DirectRunoff'
          inline frameSWR_DirectRunoff: TframeScreenObjectNoParam
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameSWR_DirectRunoffseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnSetEditText = frameSWR_DirectRunoffdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameSWR_DirectRunoffdgModflowBoundaryEndUpdate
                ExplicitWidth = 535
                ExplicitHeight = 355
                ColWidths = (
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
        end
        object jvspSwrReaches: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Reaches_in_SWR_Surface_Water_R'
          Caption = 'jvspSwrReaches'
          inline frameSwrReach: TframeScreenObjectSwrReach
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pgcSwr: TPageControl
              Width = 537
              Height = 478
              ActivePage = frameSwrReach.tabSteady
              ExplicitWidth = 537
              ExplicitHeight = 478
              inherited tabSteady: TTabSheet
                ExplicitTop = 30
                ExplicitWidth = 529
                ExplicitHeight = 444
                inherited pnlSteady: TPanel
                  Width = 529
                  Height = 185
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 529
                  ExplicitHeight = 185
                  inherited lblGroupNumber: TLabel
                    Width = 185
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 185
                    ExplicitHeight = 19
                  end
                  inherited lblReachLength: TLabel
                    Width = 154
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 154
                    ExplicitHeight = 19
                  end
                  inherited lblRouteType: TLabel
                    Width = 227
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 227
                    ExplicitHeight = 19
                  end
                  inherited lblObservationType: TLabel
                    Width = 130
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 130
                    ExplicitHeight = 19
                  end
                  inherited rdeGroupNumber: TRbwDataEntry
                    StyleElements = [seFont, seClient, seBorder]
                  end
                  inherited edReachLength: TRbwEdit
                    Width = 258
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 258
                    ExplicitHeight = 27
                  end
                  inherited btnEditReachLength: TButton
                    Left = 447
                    Top = 64
                    OnClick = btnFormulaClick
                    ExplicitLeft = 447
                    ExplicitTop = 64
                  end
                  inherited comboRouteType: TJvImageComboBox
                    Height = 29
                    StyleElements = [seFont, seClient, seBorder]
                    ItemHeight = 23
                    ItemIndex = -1
                    ExplicitHeight = 29
                  end
                  inherited cbbObservationTypes: TJvCheckedComboBox
                    Top = 149
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitTop = 149
                    ExplicitHeight = 27
                  end
                end
                inherited grpConnections: TGroupBox
                  Top = 185
                  Width = 529
                  Height = 259
                  ExplicitTop = 185
                  ExplicitWidth = 529
                  ExplicitHeight = 259
                  inherited lblDescription: TLabel
                    Top = 24
                    Width = 519
                    Height = 57
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitTop = 24
                    ExplicitWidth = 468
                    ExplicitHeight = 57
                  end
                  inherited frameConnections: TframeGrid
                    Top = 84
                    Width = 525
                    Height = 173
                    ExplicitTop = 84
                    ExplicitWidth = 525
                    ExplicitHeight = 173
                    inherited Panel: TPanel
                      Top = 132
                      Width = 525
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitTop = 132
                      ExplicitWidth = 525
                      inherited lbNumber: TLabel
                        Width = 57
                        Height = 19
                        StyleElements = [seFont, seClient, seBorder]
                        ExplicitWidth = 57
                        ExplicitHeight = 19
                      end
                      inherited sbAdd: TSpeedButton
                        Left = 292
                        ExplicitLeft = 294
                      end
                      inherited sbInsert: TSpeedButton
                        Left = 345
                        ExplicitLeft = 348
                      end
                      inherited sbDelete: TSpeedButton
                        Left = 400
                        ExplicitLeft = 402
                      end
                      inherited seNumber: TJvSpinEdit
                        Height = 27
                        StyleElements = [seFont, seClient, seBorder]
                        ExplicitHeight = 27
                      end
                    end
                    inherited Grid: TRbwDataGrid4
                      Width = 525
                      Height = 132
                      ExplicitWidth = 525
                      ExplicitHeight = 132
                      ColWidths = (
                        172
                        64
                        64)
                      RowHeights = (
                        24
                        24)
                    end
                  end
                end
              end
              inherited tabTransient: TTabSheet
                ExplicitTop = 30
                ExplicitWidth = 529
                ExplicitHeight = 444
                inherited frameSwr: TframeScreenObjectNoParam
                  Width = 529
                  Height = 444
                  ExplicitWidth = 529
                  ExplicitHeight = 444
                  inherited pnlBottom: TPanel
                    Top = 398
                    Width = 529
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitTop = 398
                    ExplicitWidth = 529
                    inherited lblNumTimes: TLabel
                      Width = 119
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 119
                      ExplicitHeight = 19
                    end
                    inherited seNumberOfTimes: TJvSpinEdit
                      Height = 27
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitHeight = 27
                    end
                    inherited btnDelete: TBitBtn
                      Left = 447
                      Top = 5
                      ExplicitLeft = 447
                      ExplicitTop = 5
                    end
                    inherited btnInsert: TBitBtn
                      Left = 359
                      Top = 5
                      ExplicitLeft = 359
                      ExplicitTop = 5
                    end
                  end
                  inherited pnlTop: TPanel
                    Width = 529
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 529
                    inherited pnlCaption: TPanel
                      Width = 527
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 527
                    end
                  end
                  inherited pnlGrid: TPanel
                    Width = 529
                    Height = 373
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 529
                    ExplicitHeight = 373
                    inherited pnlEditGrid: TPanel
                      Width = 527
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 527
                      inherited lblFormula: TLabel
                        Width = 59
                        Height = 19
                        StyleElements = [seFont, seClient, seBorder]
                        ExplicitWidth = 59
                        ExplicitHeight = 19
                      end
                      inherited rdeFormula: TRbwDataEntry
                        StyleElements = [seFont, seClient, seBorder]
                      end
                    end
                    inherited rdgModflowBoundary: TRbwDataGrid4
                      Width = 527
                      Height = 321
                      OnSetEditText = frameSwrdgModflowBoundarySetEditText
                      OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                      Columns = <
                        item
                          AutoAdjustRowHeights = False
                          AutoAdjustCaptionRowHeights = False
                          ButtonCaption = 'F()'
                          ButtonFont.Charset = DEFAULT_CHARSET
                          ButtonFont.Color = clWindowText
                          ButtonFont.Height = -11
                          ButtonFont.Name = 'Tahoma'
                          ButtonFont.Style = []
                          ButtonUsed = False
                          ButtonWidth = 35
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
                          AutoAdjustRowHeights = True
                          AutoAdjustCaptionRowHeights = False
                          ButtonCaption = 'F()'
                          ButtonFont.Charset = DEFAULT_CHARSET
                          ButtonFont.Color = clWindowText
                          ButtonFont.Height = -11
                          ButtonFont.Name = 'Tahoma'
                          ButtonFont.Style = []
                          ButtonUsed = False
                          ButtonWidth = 35
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
                          ButtonFont.Height = -13
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
                            'Active (>0)'
                            'Inactive (0)'
                            'Specified Stage (<0)')
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
                          ButtonFont.Height = -13
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
                          PickList.Strings = (
                            'Active (>0)'
                            'Inactive (0)'
                            'Specified Stage (<0)')
                          WordWrapCaptions = True
                          WordWrapCells = False
                          CaseSensitivePicklist = False
                          CheckStyle = csCheck
                          AutoAdjustColWidths = True
                        end>
                      ExplicitWidth = 527
                      ExplicitHeight = 321
                      ColWidths = (
                        64
                        64
                        64
                        64
                        174
                        64)
                      RowHeights = (
                        24
                        24)
                    end
                  end
                end
              end
            end
          end
        end
        object jvspMNW1: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'MNW1_Multi_Node_Well_Package_2'
          Caption = 'jvspMNW1'
          inline frameMNW1: TframeScreenObjectMnw1
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 394
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 394
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblSite: TLabel
                Width = 228
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 228
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                Top = 49
                ExplicitLeft = 445
                ExplicitTop = 49
              end
              inherited btnInsert: TBitBtn
                Left = 361
                Top = 49
                ExplicitLeft = 361
                ExplicitTop = 49
              end
              inherited edSiteLabel: TEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 369
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 369
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited comboConductance: TJvImageComboBox
                  Height = 29
                  StyleElements = [seFont, seClient, seBorder]
                  ItemHeight = 23
                  ExplicitHeight = 29
                end
                inherited comboWaterLevelLimit: TJvImageComboBox
                  Height = 29
                  StyleElements = [seFont, seClient, seBorder]
                  ItemHeight = 23
                  ExplicitHeight = 29
                end
                inherited comboPumpingLevelLimit: TJvImageComboBox
                  Height = 29
                  StyleElements = [seFont, seClient, seBorder]
                  ItemHeight = 23
                  ExplicitHeight = 29
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 317
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 317
                ColWidths = (
                  64
                  64
                  64
                  64
                  64
                  64
                  64
                  64
                  64
                  64
                  64
                  64
                  64
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
        end
        object jvspFarmID: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Farm_ID_in_FMP_Farm_Process_Pa'
          Caption = 'jvspFarmID'
          inline frameFarmID: TframeScreenObjectFarmID
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
                ColWidths = (
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
        end
        object jvspSWI_Obs: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'SWI2_Seawater_Intrusion_Packa2'
          Caption = 'jvspSWI_Obs'
          inline frameSwiObs: TframeSwiObsInterpolated
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlCaption: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
            end
            inherited frameSwiObs: TframeGrid
              Width = 537
              Height = 373
              ExplicitWidth = 537
              ExplicitHeight = 373
              inherited Panel: TPanel
                Top = 332
                Width = 537
                StyleElements = [seFont, seClient, seBorder]
                ExplicitTop = 332
                ExplicitWidth = 537
                inherited lbNumber: TLabel
                  Width = 207
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 207
                  ExplicitHeight = 19
                end
                inherited sbAdd: TSpeedButton
                  Left = 288
                  ExplicitLeft = 301
                end
                inherited sbInsert: TSpeedButton
                  Left = 336
                  ExplicitLeft = 355
                end
                inherited sbDelete: TSpeedButton
                  Left = 392
                  ExplicitLeft = 411
                end
                inherited seNumber: TJvSpinEdit
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitHeight = 27
                end
              end
              inherited Grid: TRbwDataGrid4
                Width = 537
                Height = 332
                OnSetEditText = frameSwiObsGridSetEditText
                ExplicitWidth = 537
                ExplicitHeight = 332
                ColWidths = (
                  64
                  64
                  64
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
            inherited pnlMultiEdit: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited lblZetaSurfaceNumber: TLabel
                Width = 151
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 151
                ExplicitHeight = 19
              end
              inherited lblTreatment: TLabel
                Width = 71
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 71
                ExplicitHeight = 19
              end
              inherited comboMultiStatFlag: TJvImageComboBox
                Height = 29
                StyleElements = [seFont, seClient, seBorder]
                ItemHeight = 23
                ExplicitHeight = 29
              end
              inherited rdeZetaSurfaceNumber: TRbwDataEntry
                StyleElements = [seFont, seClient, seBorder]
              end
              inherited comboTreatment: TComboBox
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
            end
          end
        end
        object jvspRIP: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'RIP_Riparian_ET_Pane'
          Caption = 'jvspRIP'
          inline frameRIP: TframeScreenObjectRIP
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
                ColWidths = (
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
        end
        object jvspMT3D_UZT_Rech: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Recharge_conc_in_UZT_Unsaturat'
          Caption = 'jvspMT3D_UZT_Rech'
          inline frameMt3d_UZT_Rech: TframeScreenObjectNoParam
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                Top = 5
                ExplicitLeft = 445
                ExplicitTop = 5
              end
              inherited btnInsert: TBitBtn
                Left = 361
                Top = 3
                ExplicitLeft = 361
                ExplicitTop = 3
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnSetEditText = frameMt3d_UZTdgModflowBoundarySetEditText
                ExplicitWidth = 535
                ExplicitHeight = 355
                ColWidths = (
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
        end
        object jvspMT3D_UZT_Unsat: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Unsat_ET_conc_in_UZT_Unsaturat'
          Caption = 'jvspMT3D_UZT_Unsat'
          inline frameMt3d_UZT_Unsat: TframeScreenObjectNoParam
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnSetEditText = frameMt3d_UZT_UnsatdgModflowBoundarySetEditText
                ExplicitWidth = 535
                ExplicitHeight = 355
                ColWidths = (
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
        end
        object jvspMT3D_UZT_Sat: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Sat_ET_conc_in_UZT_Unsaturated'
          Caption = 'jvspMT3D_UZT_Sat'
          inline frameMt3d_UZT_Sat: TframeScreenObjectNoParam
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnSetEditText = frameMt3d_UZT_SatdgModflowBoundarySetEditText
                ExplicitWidth = 535
                ExplicitHeight = 355
                ColWidths = (
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
        end
        object jvspMT3D_Uzf_Ssm_Conc: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'UZF_sink_conc_in_SSM_Sink_and_'
          Caption = 'jvspMT3D_Uzf_Ssm_Conc'
          inline frameMT3D_Uzf_Ssm_Conc: TframeScreenObjectNoParam
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnSetEditText = frameMT3D_Uzf_SeepagedgModflowBoundarySetEditText
                ExplicitWidth = 535
                ExplicitHeight = 355
                ColWidths = (
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
          end
        end
        object jvspSFR6: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'SFR_MODFLOW_6_Streamflow_Routi'
          Caption = 'jvspSFR6'
          inline frameScreenObjectSfr6: TframeScreenObjectSfr6
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pgcSfr6: TPageControl
              Width = 537
              Height = 453
              ActivePage = frameScreenObjectSfr6.tabCrossSection
              ExplicitWidth = 537
              ExplicitHeight = 453
              inherited tabConfiguration: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 376
                inherited lblSegmentNumber: TLabel
                  Width = 124
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 124
                  ExplicitHeight = 19
                end
                inherited lblCrossSection: TLabel
                  Left = 384
                  Top = 0
                  Width = 100
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitLeft = 384
                  ExplicitTop = 0
                  ExplicitWidth = 100
                  ExplicitHeight = 19
                end
                inherited rdgFormulas: TRbwDataGrid4
                  Height = 326
                  FixedCols = 0
                  OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                  ExplicitHeight = 326
                  ColWidths = (
                    64
                    64)
                  RowHeights = (
                    24
                    24
                    24
                    24
                    24
                    24
                    24
                    24)
                end
                inherited rdeSegmentNumber: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited comboCrossSection: TComboBox
                  Left = 384
                  Top = 18
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitLeft = 384
                  ExplicitTop = 18
                  ExplicitHeight = 27
                end
              end
              inherited tabRates: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 376
                inherited pnlGrid: TPanel
                  Height = 331
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitHeight = 331
                  inherited pnlEditGrid: TPanel
                    StyleElements = [seFont, seClient, seBorder]
                    inherited lblFormula: TLabel
                      Width = 59
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 59
                      ExplicitHeight = 19
                    end
                    inherited rdeFormula: TRbwDataEntry
                      StyleElements = [seFont, seClient, seBorder]
                    end
                  end
                  inherited rdgModflowBoundary: TRbwDataGrid4
                    Height = 279
                    OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                    ExplicitHeight = 279
                    ColWidths = (
                      64
                      64
                      64)
                    RowHeights = (
                      24
                      24)
                  end
                end
                inherited pnlBottom: TPanel
                  Top = 331
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 331
                  inherited lblNumTimes: TLabel
                    Width = 119
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 119
                    ExplicitHeight = 19
                  end
                  inherited seNumberOfTimes: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitHeight = 27
                  end
                end
              end
              inherited tabDownstreamSegments: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 376
                inherited frmgrdDownstreamSegments: TframeGrid
                  Height = 376
                  ExplicitHeight = 376
                  inherited Panel: TPanel
                    Top = 336
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitTop = 336
                    inherited lbNumber: TLabel
                      Width = 57
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 57
                      ExplicitHeight = 19
                    end
                    inherited sbAdd: TSpeedButton
                      Left = 172
                      ExplicitLeft = 269
                    end
                    inherited sbInsert: TSpeedButton
                      Left = 197
                      ExplicitLeft = 306
                    end
                    inherited sbDelete: TSpeedButton
                      Left = 221
                      ExplicitLeft = 344
                    end
                    inherited seNumber: TJvSpinEdit
                      Height = 27
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitHeight = 27
                    end
                  end
                  inherited Grid: TRbwDataGrid4
                    Height = 336
                    OnButtonClick = frmgrdDownstreamSegmentsGridButtonClick
                    ExplicitHeight = 336
                    ColWidths = (
                      64)
                    RowHeights = (
                      24
                      24)
                  end
                end
              end
              inherited tabDiversions: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 376
                inherited frmgrdDiversions: TframeGrid
                  Height = 376
                  ExplicitHeight = 376
                  inherited Panel: TPanel
                    Top = 336
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitTop = 336
                    inherited lbNumber: TLabel
                      Width = 57
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 57
                      ExplicitHeight = 19
                    end
                    inherited sbAdd: TSpeedButton
                      Left = 295
                      ExplicitLeft = 296
                    end
                    inherited sbInsert: TSpeedButton
                      Left = 351
                      ExplicitLeft = 351
                    end
                    inherited sbDelete: TSpeedButton
                      Left = 403
                      ExplicitLeft = 406
                    end
                    inherited seNumber: TJvSpinEdit
                      Height = 27
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitHeight = 27
                    end
                  end
                  inherited Grid: TRbwDataGrid4
                    Height = 336
                    OnButtonClick = frmgrdDiversionsGridButtonClick
                    ExplicitHeight = 336
                    ColWidths = (
                      64
                      64)
                    RowHeights = (
                      24
                      24)
                  end
                end
              end
              inherited tabCrossSection: TTabSheet
                ExplicitTop = 30
                ExplicitWidth = 529
                ExplicitHeight = 419
                inherited Splitter1: TSplitter
                  Top = 306
                  Width = 529
                  ExplicitTop = 308
                  ExplicitWidth = 537
                end
                inherited frameCrossSection: TframeGrid
                  Width = 329
                  Height = 265
                  ExplicitWidth = 329
                  ExplicitHeight = 265
                  inherited Panel: TPanel
                    Top = 224
                    Width = 329
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitTop = 224
                    ExplicitWidth = 329
                    inherited lbNumber: TLabel
                      Width = 76
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 76
                      ExplicitHeight = 19
                    end
                    inherited sbAdd: TSpeedButton
                      Left = 235
                      ExplicitLeft = 238
                    end
                    inherited sbInsert: TSpeedButton
                      Left = 263
                      ExplicitLeft = 266
                    end
                    inherited sbDelete: TSpeedButton
                      Left = 290
                      ExplicitLeft = 315
                    end
                    inherited seNumber: TJvSpinEdit
                      Height = 27
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitHeight = 27
                    end
                  end
                  inherited Grid: TRbwDataGrid4
                    Width = 329
                    Height = 224
                    ExplicitWidth = 329
                    ExplicitHeight = 224
                  end
                end
                inherited Panel1: TPanel
                  Width = 529
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 529
                end
                inherited zbChannel: TQRbwZoomBox2
                  Top = 311
                  Width = 529
                  StyleElements = [seFont, seClient, seBorder]
                  Image32.Top = 0
                  Image32.Width = 529
                  Image32.ExplicitWidth = 529
                  ExplicitTop = 311
                  ExplicitWidth = 529
                end
                inherited frameCrossSectionTime: TframeGrid
                  Height = 265
                  ExplicitHeight = 265
                  inherited Panel: TPanel
                    Top = 224
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitTop = 224
                    inherited lbNumber: TLabel
                      Width = 39
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 39
                      ExplicitHeight = 19
                    end
                    inherited seNumber: TJvSpinEdit
                      Height = 27
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitHeight = 27
                    end
                  end
                  inherited Grid: TRbwDataGrid4
                    Height = 224
                    ExplicitHeight = 224
                  end
                end
              end
              inherited tabGWT: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 376
                inherited splSplit: TSplitter
                  Height = 376
                  ExplicitHeight = 213
                end
                inherited tvGwt: TJvPageListTreeView
                  Height = 376
                  Items.Links = {00000000}
                  ExplicitHeight = 376
                end
                inherited jplGwt: TJvPageList
                  Height = 376
                  ExplicitHeight = 376
                end
              end
            end
          end
        end
        object jvspMAW: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'MAW_Multi_Aquifer_Well_Package'
          Caption = 'jvspMAW'
          inline frameMAW: TframeScreenObjectMAW
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 529
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 529
              end
            end
            inherited pgcMain: TPageControl
              Width = 537
              Height = 445
              ExplicitWidth = 537
              ExplicitHeight = 445
              inherited tabWell: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 226
                inherited lblRadius: TLabel
                  Width = 50
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 50
                  ExplicitHeight = 19
                end
                inherited lblBottom: TLabel
                  Width = 50
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 50
                  ExplicitHeight = 19
                end
                inherited lblStartingHead: TLabel
                  Width = 136
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 136
                  ExplicitHeight = 19
                end
                inherited lblConductanceEquation: TLabel
                  Width = 244
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 244
                  ExplicitHeight = 19
                end
                inherited edWellRadius: TJvComboEdit
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                  ExplicitHeight = 27
                end
                inherited edBottom: TJvComboEdit
                  Top = 40
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                  ExplicitTop = 40
                  ExplicitHeight = 27
                end
                inherited edStartingHead: TJvComboEdit
                  Left = 145
                  Top = 67
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                  ExplicitLeft = 145
                  ExplicitTop = 67
                  ExplicitHeight = 27
                end
                inherited comboConductEq: TJvImageComboBox
                  Height = 29
                  StyleElements = [seFont, seClient, seBorder]
                  ItemHeight = 23
                  ItemIndex = -1
                  ExplicitHeight = 29
                end
              end
              inherited tabWellScreens: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 226
                inherited frameWellScreens: TframeGrid
                  Height = 226
                  ExplicitHeight = 226
                  inherited Panel: TPanel
                    Top = 185
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitTop = 185
                    inherited lbNumber: TLabel
                      Width = 170
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 170
                      ExplicitHeight = 19
                    end
                    inherited sbAdd: TSpeedButton
                      ExplicitLeft = 388
                    end
                    inherited sbInsert: TSpeedButton
                      ExplicitLeft = 443
                    end
                    inherited sbDelete: TSpeedButton
                      ExplicitLeft = 495
                    end
                    inherited seNumber: TJvSpinEdit
                      Height = 27
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitHeight = 27
                    end
                  end
                  inherited Grid: TRbwDataGrid4
                    Height = 185
                    OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                    ExplicitHeight = 185
                  end
                end
              end
              inherited tabTransient: TTabSheet
                ExplicitTop = 30
                ExplicitWidth = 529
                ExplicitHeight = 411
                inherited pnlBottom: TPanel
                  Top = 365
                  Width = 529
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 365
                  ExplicitWidth = 529
                  inherited lblNumTimes: TLabel
                    Width = 119
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 119
                    ExplicitHeight = 19
                  end
                  inherited seNumberOfTimes: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitHeight = 27
                  end
                  inherited btnDelete: TBitBtn
                    Left = 437
                    ExplicitLeft = 437
                  end
                  inherited btnInsert: TBitBtn
                    Left = 349
                    ExplicitLeft = 349
                  end
                end
                inherited pnlGrid: TPanel
                  Width = 529
                  Height = 365
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 529
                  ExplicitHeight = 365
                  inherited pnlEditGrid: TPanel
                    Width = 527
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 527
                    inherited lblFormula: TLabel
                      Width = 59
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 59
                      ExplicitHeight = 19
                    end
                    inherited lblStatus: TLabel
                      Width = 45
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 45
                      ExplicitHeight = 19
                    end
                    inherited lblRateLimitation: TLabel
                      Width = 101
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 101
                      ExplicitHeight = 19
                    end
                    inherited rdeFormula: TRbwDataEntry
                      StyleElements = [seFont, seClient, seBorder]
                    end
                    inherited comboStatus: TJvImageComboBox
                      Height = 29
                      StyleElements = [seFont, seClient, seBorder]
                      ItemHeight = 23
                      ExplicitHeight = 29
                    end
                    inherited comboRateLimitation: TJvImageComboBox
                      Height = 29
                      StyleElements = [seFont, seClient, seBorder]
                      ItemHeight = 23
                      ExplicitHeight = 29
                    end
                  end
                  inherited rdgModflowBoundary: TRbwDataGrid4
                    Width = 527
                    Height = 307
                    OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                    ExplicitWidth = 527
                    ExplicitHeight = 307
                  end
                end
              end
              inherited tabGwt: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 226
                inherited splSplit: TSplitter
                  Height = 226
                  ExplicitHeight = 226
                end
                inherited tvGwt: TJvPageListTreeView
                  Height = 226
                  Items.Links = {00000000}
                  ExplicitHeight = 226
                end
                inherited jplGwt: TJvPageList
                  Height = 226
                  ExplicitWidth = 407
                  ExplicitHeight = 226
                end
              end
            end
          end
        end
        object jvspMf6Obs: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'OBS_Observation_Utility_Pane'
          Caption = 'jvspMf6Obs'
          inline frameObsMf6: TframeScreenObjectObsMf6
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlCaption: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
            end
            inherited pgcMain: TPageControl
              Width = 537
              Height = 407
              OnChange = frameObsMf6pgcMainChange
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited tabBasic: TTabSheet
                ExplicitTop = 30
                ExplicitWidth = 529
                ExplicitHeight = 373
                inherited lblTypesOfFlowObservation: TLabel
                  Width = 183
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 183
                  ExplicitHeight = 19
                end
                inherited lblBoundaryFlowObservations: TLabel
                  Width = 200
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 200
                  ExplicitHeight = 19
                end
                inherited lblSpecies: TLabel
                  Width = 216
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 216
                end
                inherited lblGwtObs: TLabel
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited chklstFlowObs: TCheckListBox
                  Left = 2
                  Top = 97
                  ItemHeight = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitLeft = 2
                  ExplicitTop = 97
                end
                inherited chklstBoundaryFlow: TCheckListBox
                  Width = 277
                  Columns = 2
                  ItemHeight = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 277
                end
                inherited chklstGWT: TCheckListBox
                  Width = 225
                  ItemHeight = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 225
                end
                inherited chklstChemSpecies: TCheckListBox
                  Width = 225
                  ItemHeight = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 225
                end
              end
              inherited tabMAW: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 414
                inherited lblMaw: TLabel
                  Width = 40
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 40
                  ExplicitHeight = 19
                end
                inherited lblMwt: TLabel
                  Width = 39
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 39
                  ExplicitHeight = 19
                end
                inherited chklstMAW: TCheckListBox
                  Height = 270
                  ItemHeight = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitHeight = 270
                end
                inherited chklstMwt: TCheckListBox
                  Width = 234
                  ItemHeight = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 234
                end
              end
              inherited tabSFR: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 414
                inherited lblSFR: TLabel
                  Width = 32
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 32
                  ExplicitHeight = 19
                end
                inherited lblSFT: TLabel
                  Width = 30
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 30
                  ExplicitHeight = 19
                end
                inherited chklstSFR: TCheckListBox
                  ItemHeight = 19
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited chklstSFT: TCheckListBox
                  ItemHeight = 19
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited tabLAK: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 414
                inherited lblLAK: TLabel
                  Width = 31
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 31
                  ExplicitHeight = 19
                end
                inherited lblLKT: TLabel
                  Width = 29
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 29
                  ExplicitHeight = 19
                end
                inherited chklstLAK: TCheckListBox
                  Height = 341
                  ItemHeight = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitHeight = 341
                end
                inherited chklstGwtOb: TCheckListBox
                  Width = 250
                  Height = 341
                  ItemHeight = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 250
                  ExplicitHeight = 341
                end
              end
              inherited tabUZF: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 414
                inherited lblDepthFraction: TLabel
                  Top = 322
                  Width = 282
                  Height = 38
                  WordWrap = True
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 322
                  ExplicitWidth = 282
                  ExplicitHeight = 38
                end
                inherited rdeDepthFraction: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited pnl1: TPanel
                  Left = 0
                  Top = 0
                  Width = 582
                  Align = alTop
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 582
                  inherited lblUZF: TLabel
                    Width = 30
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 30
                    ExplicitHeight = 19
                  end
                  inherited lblUZT: TLabel
                    Width = 29
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 29
                    ExplicitHeight = 19
                  end
                  inherited chklstUZF: TCheckListBox
                    ItemHeight = 19
                    StyleElements = [seFont, seClient, seBorder]
                  end
                  inherited chklstUZT: TCheckListBox
                    Width = 313
                    ItemHeight = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 313
                  end
                end
              end
              inherited tabCSUB: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 414
                inherited splCSub: TSplitter
                  Top = 309
                  ExplicitTop = 309
                  ExplicitWidth = 582
                end
                inherited chklstCSUB: TCheckListBox
                  Height = 309
                  ItemHeight = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitHeight = 309
                end
                inherited pnlDelayBeds: TPanel
                  Top = 314
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 314
                  inherited lblDelayInterbedNumber: TLabel
                    Width = 357
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 357
                    ExplicitHeight = 19
                  end
                  inherited chklstDelayBeds: TCheckListBox
                    ItemHeight = 19
                    StyleElements = [seFont, seClient, seBorder]
                  end
                end
              end
              inherited tabCalibration: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 414
                inherited framePestObs: TframePestObsMf6
                  Height = 414
                  ExplicitHeight = 414
                  inherited splObservations: TSplitter
                    Top = 237
                    ExplicitTop = 194
                    ExplicitWidth = 573
                  end
                  inherited grpDirectObs: TGroupBox
                    Height = 237
                    ExplicitHeight = 237
                    inherited frameObservations: TframeGrid
                      Top = 21
                      Height = 214
                      ExplicitTop = 21
                      ExplicitHeight = 214
                      inherited Panel: TPanel
                        Top = 156
                        StyleElements = [seFont, seClient, seBorder]
                        ExplicitTop = 156
                        inherited lbNumber: TLabel
                          Width = 218
                          Height = 19
                          StyleElements = [seFont, seClient, seBorder]
                          ExplicitWidth = 218
                          ExplicitHeight = 19
                        end
                        inherited sbAdd: TSpeedButton
                          Left = 312
                          ExplicitLeft = 307
                        end
                        inherited sbInsert: TSpeedButton
                          Left = 337
                          ExplicitLeft = 332
                        end
                        inherited sbDelete: TSpeedButton
                          Left = 360
                          ExplicitLeft = 356
                        end
                        inherited seNumber: TJvSpinEdit
                          Height = 27
                          StyleElements = [seFont, seClient, seBorder]
                          ExplicitHeight = 27
                        end
                      end
                      inherited Grid: TRbwDataGrid4
                        Height = 156
                        OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                        ExplicitHeight = 156
                      end
                    end
                  end
                  inherited grpObsComparisons: TGroupBox
                    Top = 242
                    ExplicitTop = 242
                    inherited frameObsComparisons: TframeGrid
                      Top = 21
                      Height = 149
                      ExplicitTop = 21
                      ExplicitHeight = 149
                      inherited Panel: TPanel
                        Top = 114
                        StyleElements = [seFont, seClient, seBorder]
                        ExplicitTop = 114
                        inherited lbNumber: TLabel
                          Width = 262
                          Height = 19
                          StyleElements = [seFont, seClient, seBorder]
                          ExplicitWidth = 262
                          ExplicitHeight = 19
                        end
                        inherited sbAdd: TSpeedButton
                          Left = 312
                          ExplicitLeft = 307
                        end
                        inherited sbInsert: TSpeedButton
                          Left = 337
                          ExplicitLeft = 332
                        end
                        inherited sbDelete: TSpeedButton
                          Left = 360
                          ExplicitLeft = 356
                        end
                        inherited seNumber: TJvSpinEdit
                          Height = 27
                          StyleElements = [seFont, seClient, seBorder]
                          ExplicitHeight = 27
                        end
                      end
                      inherited Grid: TRbwDataGrid4
                        Height = 114
                        ExplicitHeight = 114
                      end
                    end
                  end
                end
              end
            end
            inherited pnlName: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited edObsName: TLabeledEdit
                Height = 27
                EditLabel.Width = 266
                EditLabel.Height = 19
                EditLabel.ExplicitLeft = 6
                EditLabel.ExplicitTop = -3
                EditLabel.ExplicitWidth = 266
                EditLabel.ExplicitHeight = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
            end
          end
        end
        object jvspHFB_MF6: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'HFB_Horizontal_Flow_Barrier_Pane'
          Caption = 'jvspHFB_MF6'
          inline frameHfbMf6: TframeScreenObjectHfbMf6
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 306
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 306
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblParameterName: TLabel
                Width = 121
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 121
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameHfbMf6seNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 449
                ExplicitLeft = 449
              end
              inherited btnInsert: TBitBtn
                Left = 365
                ExplicitLeft = 365
              end
              inherited rgAngleAdjustment: TRadioGroup
                Width = 561
                OnClick = frameHfbMf6rgAngleAdjustmentClick
                ExplicitWidth = 561
              end
              inherited comboHfbParameters: TJvImageComboBox
                Width = 166
                Height = 29
                StyleElements = [seFont, seClient, seBorder]
                Anchors = [akLeft, akTop, akRight]
                ItemHeight = 23
                ExplicitWidth = 166
                ExplicitHeight = 29
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 281
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 281
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 229
                OnSetEditText = frameHfbMf6rdgModflowBoundarySetEditText
                OnButtonClick = frameHfbMf6rdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 229
              end
            end
          end
        end
        object jvspLakMf6: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'LAK_Lake_package_for_MODFLOW_2'
          Caption = 'jvspLakMf6'
          inline frameLakMf6: TframeScreenObjectLakMf6
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 430
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 430
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Top = 286
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 286
              ExplicitWidth = 537
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                OnButtonClick = frameResdgModflowBoundaryButtonClick
                ExplicitWidth = 535
              end
            end
            inherited pcLake: TPageControl
              Width = 537
              Height = 261
              ExplicitWidth = 537
              ExplicitHeight = 261
              inherited tabLakeProperties: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 301
                inherited lblOutletCount: TLabel
                  Width = 127
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 127
                  ExplicitHeight = 19
                end
                inherited lblBottomElev: TLabel
                  Width = 172
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 172
                  ExplicitHeight = 19
                end
                inherited lblTopElev: TLabel
                  Width = 142
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 142
                  ExplicitHeight = 19
                end
                inherited lblLakebedK: TLabel
                  Width = 151
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 151
                  ExplicitHeight = 19
                end
                inherited lblLakebedThickness: TLabel
                  Width = 208
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 208
                  ExplicitHeight = 19
                end
                inherited lblConnLength: TLabel
                  Width = 205
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 205
                  ExplicitHeight = 19
                end
                inherited lblStartingStage: TLabel
                  Width = 139
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 139
                  ExplicitHeight = 19
                end
                inherited seOutletCount: TJvSpinEdit
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitHeight = 27
                end
                inherited edBottomElev: TJvComboEdit
                  Left = 219
                  Width = 334
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = btnFormulaClick
                  ExplicitLeft = 219
                  ExplicitWidth = 334
                  ExplicitHeight = 27
                end
                inherited edTopElev: TJvComboEdit
                  Left = 219
                  Width = 334
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = btnFormulaClick
                  ExplicitLeft = 219
                  ExplicitWidth = 334
                  ExplicitHeight = 27
                end
                inherited edLakebedK: TJvComboEdit
                  Left = 219
                  Width = 334
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = btnFormulaClick
                  ExplicitLeft = 219
                  ExplicitWidth = 334
                  ExplicitHeight = 27
                end
                inherited edLakebedThickness: TJvComboEdit
                  Left = 219
                  Width = 334
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = btnFormulaClick
                  ExplicitLeft = 219
                  ExplicitWidth = 334
                  ExplicitHeight = 27
                end
                inherited edConnLength: TJvComboEdit
                  Left = 219
                  Width = 334
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = btnFormulaClick
                  ExplicitLeft = 219
                  ExplicitWidth = 334
                  ExplicitHeight = 27
                end
                inherited edStartingStage: TJvComboEdit
                  Left = 219
                  Width = 334
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                  ExplicitLeft = 219
                  ExplicitWidth = 334
                  ExplicitHeight = 27
                end
              end
              inherited tabLakeTransientProperties: TTabSheet
                ExplicitTop = 30
                ExplicitWidth = 529
                ExplicitHeight = 227
              end
              inherited tabLakeTable: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 301
                inherited frameLakeTable: TframeFormulaGrid
                  Height = 301
                  ExplicitHeight = 301
                  inherited Panel: TPanel
                    Top = 260
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitTop = 260
                    inherited lbNumber: TLabel
                      Width = 206
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 206
                      ExplicitHeight = 19
                    end
                    inherited sbAdd: TSpeedButton
                      Left = 330
                      ExplicitLeft = 388
                    end
                    inherited sbInsert: TSpeedButton
                      Left = 359
                      ExplicitLeft = 417
                    end
                    inherited sbDelete: TSpeedButton
                      Left = 390
                      ExplicitLeft = 448
                    end
                    inherited seNumber: TJvSpinEdit
                      Height = 27
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitHeight = 27
                    end
                  end
                  inherited Grid: TRbwDataGrid4
                    Height = 203
                    OnButtonClick = frameResdgModflowBoundaryButtonClick
                    ExplicitHeight = 203
                  end
                  inherited pnlTop: TPanel
                    StyleElements = [seFont, seClient, seBorder]
                    inherited edFormula: TLabeledEdit
                      Height = 27
                      EditLabel.Width = 59
                      EditLabel.Height = 19
                      EditLabel.ExplicitLeft = 128
                      EditLabel.ExplicitTop = 8
                      EditLabel.ExplicitWidth = 59
                      EditLabel.ExplicitHeight = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitHeight = 27
                    end
                    inherited comboChoice: TComboBox
                      Height = 27
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitHeight = 27
                    end
                  end
                end
              end
              inherited tabGWT: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 301
                inherited splSplit: TSplitter
                  Left = 115
                  Height = 301
                  ExplicitLeft = 115
                  ExplicitHeight = 225
                end
                inherited tvGwt: TJvPageListTreeView
                  Width = 115
                  Height = 301
                  Items.Links = {00000000}
                  ExplicitWidth = 115
                  ExplicitHeight = 301
                end
                inherited jplGwt: TJvPageList
                  Left = 120
                  Width = 413
                  Height = 301
                  ExplicitLeft = 120
                  ExplicitWidth = 413
                  ExplicitHeight = 301
                end
              end
            end
          end
        end
        object jvspMVR: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'MVR_Water_Mover_Package_Pane'
          Caption = 'jvspMVR'
          inline frameMVR: TframeScreenObjectMvr
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Top = 343
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 343
              ExplicitWidth = 537
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited lblMvrType: TLabel
                  Width = 80
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 80
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
                inherited comboMvrType: TComboBox
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitHeight = 27
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                OnButtonClick = frameMVRrdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ColWidths = (
                  64
                  64
                  64
                  64)
              end
            end
            inherited pcMain: TPageControl
              Width = 537
              Height = 278
              ActivePage = frameMVR.tabTime
              ExplicitWidth = 537
              ExplicitHeight = 278
              inherited tabConnections: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 205
                inherited lblSourcePackage: TLabel
                  Width = 120
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 120
                  ExplicitHeight = 19
                end
                inherited comboSourcePackage: TComboBox
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitHeight = 27
                end
                inherited frameReceivers: TframeGrid
                  Height = 141
                  ExplicitWidth = 533
                  ExplicitHeight = 141
                  inherited Panel: TPanel
                    Top = 100
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitTop = 100
                    ExplicitWidth = 533
                    inherited lbNumber: TLabel
                      Width = 148
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 148
                      ExplicitHeight = 19
                    end
                    inherited sbAdd: TSpeedButton
                      Left = 407
                      ExplicitLeft = 306
                    end
                    inherited sbInsert: TSpeedButton
                      Left = 444
                      ExplicitLeft = 334
                    end
                    inherited sbDelete: TSpeedButton
                      Left = 482
                      ExplicitLeft = 363
                    end
                    inherited seNumber: TJvSpinEdit
                      Height = 27
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitHeight = 27
                    end
                  end
                  inherited Grid: TRbwDataGrid4
                    Height = 100
                    ExplicitWidth = 533
                    ExplicitHeight = 100
                  end
                end
              end
              inherited tabTime: TTabSheet
                ExplicitTop = 30
                ExplicitWidth = 529
                ExplicitHeight = 244
              end
              inherited tabMvrMap: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 205
                inherited JvNetscapeSplitter1: TJvNetscapeSplitter
                  Height = 205
                  ExplicitHeight = 205
                end
                inherited frameMapNames: TframeGrid
                  Height = 205
                  ExplicitHeight = 205
                  inherited Panel: TPanel
                    Top = 116
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitTop = 116
                    inherited lbNumber: TLabel
                      Width = 57
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 57
                      ExplicitHeight = 19
                    end
                    inherited seNumber: TJvSpinEdit
                      Height = 27
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitHeight = 27
                    end
                  end
                  inherited Grid: TRbwDataGrid4
                    Height = 116
                    ExplicitHeight = 116
                  end
                end
                inherited pnl1: TPanel
                  Height = 205
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 413
                  ExplicitHeight = 205
                  inherited rdgMap: TRbwDataGrid4
                    Height = 162
                    FixedCols = 0
                    ExplicitWidth = 411
                    ExplicitHeight = 162
                  end
                  inherited pnl2: TPanel
                    Top = 163
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitTop = 163
                    ExplicitWidth = 411
                    inherited lblReceiverNumber: TLabel
                      Width = 226
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 226
                      ExplicitHeight = 19
                    end
                    inherited seReceiverNumber: TJvSpinEdit
                      Height = 27
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitHeight = 27
                    end
                  end
                end
              end
            end
            inherited pnlBottom: TPanel
              Top = 303
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 303
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameMVRseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
          end
        end
        object jvspUzfMf6: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'UZF6_Unsaturated_Zone_Flow_Pac'
          Caption = 'jvspUzfMf6'
          inline frameScreenObjectUzfMf6: TframeScreenObjectUzfMf6
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 449
                ExplicitLeft = 449
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
              end
            end
            inherited pcUzf: TPageControl
              Width = 537
              Height = 255
              ActivePage = frameScreenObjectUzfMf6.tabSteadyProperties
              ExplicitWidth = 537
              ExplicitHeight = 255
              inherited tabSteadyProperties: TTabSheet
                ExplicitTop = 30
                ExplicitWidth = 529
                ExplicitHeight = 221
                inherited lblSurfaceDepressionDepth: TLabel
                  Width = 260
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 260
                  ExplicitHeight = 19
                end
                inherited lblVerticalSaturatedK: TLabel
                  Width = 134
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 134
                  ExplicitHeight = 19
                end
                inherited lblResidualWaterContent: TLabel
                  Width = 204
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 204
                  ExplicitHeight = 19
                end
                inherited lblSaturatedWaterContent: TLabel
                  Width = 214
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 214
                  ExplicitHeight = 19
                end
                inherited lblInitialWaterContent: TLabel
                  Width = 178
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 178
                  ExplicitHeight = 19
                end
                inherited lblBrooksCoreyEpsilon: TLabel
                  Width = 200
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 200
                  ExplicitHeight = 19
                end
                inherited edSurfaceDepressionDepth: TJvComboEdit
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                  ExplicitHeight = 27
                end
                inherited edVerticalSaturatedK: TJvComboEdit
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                  ExplicitHeight = 27
                end
                inherited edResidualWaterContent: TJvComboEdit
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                  ExplicitHeight = 27
                end
                inherited edSaturatedWaterContent: TJvComboEdit
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                  ExplicitHeight = 27
                end
                inherited edInitialWaterContent: TJvComboEdit
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                  ExplicitHeight = 27
                end
                inherited edBrooksCoreyEpsilon: TJvComboEdit
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                  ExplicitHeight = 27
                end
              end
              inherited tabTime: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 178
              end
              inherited tabGWT: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 178
                inherited splSplit: TSplitter
                  Height = 178
                  ExplicitHeight = 219
                end
                inherited tvGwt: TJvPageListTreeView
                  Height = 178
                  Items.Links = {00000000}
                  ExplicitHeight = 178
                end
                inherited jplGwt: TJvPageList
                  Height = 178
                  ExplicitWidth = 407
                  ExplicitHeight = 178
                end
              end
            end
          end
        end
        object jvspMTD_Lkt: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'LKT_Lake_Transport_Package_Pa2'
          Caption = 'jvspMTD_Lkt'
          inline frameMT3D_LKT: TframeScreenObjectLkt
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Top = 290
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 290
              ExplicitWidth = 537
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                OnSetEditText = frameMT3D_LKTrdgModflowBoundarySetEditText
                OnButtonClick = frameResdgModflowBoundaryButtonClick
                ExplicitWidth = 535
              end
            end
            inherited pcLkt: TPageControl
              Width = 537
              Height = 265
              ActivePage = frameMT3D_LKT.tabInitialConc
              ExplicitWidth = 537
              ExplicitHeight = 265
              inherited tabInitialConc: TTabSheet
                ExplicitTop = 30
                ExplicitWidth = 529
                ExplicitHeight = 231
                inherited lblInitialConcentration: TLabel
                  Width = 232
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 232
                  ExplicitHeight = 19
                end
                inherited rdgInitialConcentration: TRbwDataGrid4
                  Width = 523
                  Height = 198
                  FixedCols = 0
                  OnButtonClick = frameResdgModflowBoundaryButtonClick
                  ExplicitWidth = 523
                  ExplicitHeight = 198
                end
              end
              inherited tabPrecipConc: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 117
              end
              inherited tabRunoffConc: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 117
                inherited pnlButtonsRunoff: TPanel
                  Top = 71
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 71
                  inherited lbl1: TLabel
                    Width = 119
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 119
                    ExplicitHeight = 19
                  end
                  inherited seRunoffTimes: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitHeight = 27
                  end
                  inherited btnDeleteRunoff: TBitBtn
                    Top = 5
                    ExplicitTop = 5
                  end
                end
                inherited pnlGridRunoff: TPanel
                  Height = 71
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitHeight = 71
                  inherited pnl3: TPanel
                    StyleElements = [seFont, seClient, seBorder]
                    inherited lblRunoffFormula: TLabel
                      Width = 59
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 59
                      ExplicitHeight = 19
                    end
                    inherited rdeRunoffFormula: TRbwDataEntry
                      StyleElements = [seFont, seClient, seBorder]
                    end
                  end
                  inherited rdgRunoffConc: TRbwDataGrid4
                    Height = 19
                    OnSetEditText = frameMT3D_LKTrdgRunoffConcSetEditText
                    OnButtonClick = frameResdgModflowBoundaryButtonClick
                    ExplicitHeight = 19
                  end
                end
              end
            end
          end
        end
        object jvspMT3D_SFT: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'SFT_Stream_Flow_Transport_Pac2'
          Caption = 'jvspMT3D_SFT'
          inline frameMT3D_SFT: TframeScreenObjectMt3dSft
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 357
                ExplicitLeft = 357
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 214
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 214
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 162
                OnSelectCell = frameMT3D_SFTrdgModflowBoundarySelectCell
                OnSetEditText = frameMT3D_SFTrdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 162
              end
            end
            inherited pgcSft: TPageControl
              Top = 239
              Width = 537
              ActivePage = frameMT3D_SFT.tsPrecipitation
              ExplicitTop = 239
              ExplicitWidth = 537
              inherited tsSteady: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 159
                inherited lblObsLocation: TLabel
                  Width = 235
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 235
                  ExplicitHeight = 19
                end
                inherited rdgSftInitConcAndDisp: TRbwDataGrid4
                  Height = 124
                  FixedCols = 0
                  OnSetEditText = frameMT3D_SFTrdgSftInitConcAndDispSetEditText
                  OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                  ExplicitWidth = 533
                  ExplicitHeight = 124
                end
                inherited comboObsLocation: TComboBox
                  Height = 27
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitHeight = 27
                end
              end
              inherited tsHeadWaters: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 159
              end
              inherited tsPrecipitation: TTabSheet
                ExplicitTop = 30
                ExplicitWidth = 529
                ExplicitHeight = 159
                inherited pnlBottomPrecip: TPanel
                  Top = 113
                  Width = 529
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 113
                  ExplicitWidth = 529
                  inherited lblNumberOfTimesPrecip: TLabel
                    Width = 119
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 119
                    ExplicitHeight = 19
                  end
                  inherited seNumberOfTimesPrecip: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitHeight = 27
                  end
                  inherited btnDeletePrecip: TBitBtn
                    Left = 441
                    ExplicitLeft = 441
                  end
                  inherited btnInsertPrecip: TBitBtn
                    Left = 357
                    ExplicitLeft = 357
                  end
                end
                inherited pnlPrecip: TPanel
                  Width = 529
                  Height = 113
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 529
                  ExplicitHeight = 113
                  inherited pnlPrecipFormula: TPanel
                    Width = 527
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 527
                    inherited lblPrecipFormula: TLabel
                      Width = 59
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 59
                      ExplicitHeight = 19
                    end
                    inherited rdePrecipFormula: TRbwDataEntry
                      StyleElements = [seFont, seClient, seBorder]
                    end
                  end
                  inherited rdgPrecip: TRbwDataGrid4
                    Width = 527
                    Height = 61
                    OnSetEditText = frameMT3D_SFTrdgPrecipSetEditText
                    OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                    ExplicitWidth = 527
                    ExplicitHeight = 61
                  end
                end
              end
              inherited tsRunoff: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 159
                inherited pnlBottomRunoff: TPanel
                  Top = 113
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 113
                  inherited lblNumberOfTimesRunoff: TLabel
                    Width = 119
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 119
                    ExplicitHeight = 19
                  end
                  inherited seNumberOfTimesRunoff: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitHeight = 27
                  end
                  inherited btnDeleteRunoff: TBitBtn
                    Left = 453
                    ExplicitLeft = 453
                  end
                  inherited btnInsertRunoff: TBitBtn
                    Left = 369
                    ExplicitLeft = 369
                  end
                end
                inherited pnlRunoff: TPanel
                  Height = 113
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitHeight = 113
                  inherited pnlFormulaRunoff: TPanel
                    StyleElements = [seFont, seClient, seBorder]
                    inherited lblFormulaRunoff: TLabel
                      Width = 59
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 59
                      ExplicitHeight = 19
                    end
                    inherited rdeFormulaRunoff: TRbwDataEntry
                      StyleElements = [seFont, seClient, seBorder]
                    end
                  end
                  inherited rdgRunoff: TRbwDataGrid4
                    Height = 61
                    OnSetEditText = frameMT3D_SFTrdgRunoffSetEditText
                    OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                    ExplicitHeight = 61
                  end
                end
              end
              inherited tsConstantConcentration: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 159
                inherited pnlBottomConstConc: TPanel
                  Top = 113
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 113
                  inherited lblNumberOfTimesConstConc: TLabel
                    Width = 119
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 119
                    ExplicitHeight = 19
                  end
                  inherited seNumberOfTimesConstConc: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitHeight = 27
                  end
                  inherited btnDeleteConstConc: TBitBtn
                    Left = 453
                    ExplicitLeft = 453
                  end
                  inherited btnInsertConstConc: TBitBtn
                    Left = 369
                    ExplicitLeft = 369
                  end
                end
                inherited pnlGridConstConc: TPanel
                  Height = 113
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitHeight = 113
                  inherited pnlFormulaConstConc: TPanel
                    StyleElements = [seFont, seClient, seBorder]
                    inherited lblFormulaConstConc: TLabel
                      Width = 59
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 59
                      ExplicitHeight = 19
                    end
                    inherited rdeFormulaConstConc: TRbwDataEntry
                      StyleElements = [seFont, seClient, seBorder]
                    end
                  end
                  inherited rdgConstConc: TRbwDataGrid4
                    Height = 61
                    OnSetEditText = frameMT3D_SFTrdgConstConcSetEditText
                    OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                    ExplicitHeight = 61
                    ColWidths = (
                      64
                      64
                      64)
                  end
                end
              end
            end
          end
        end
        object jvspCSUB: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'CSUB_Skeletal_Storage__Compac2'
          Caption = 'jvspCSUB'
          OnShow = jvspCSUBShow
          inline frameCSUB: TframeScreenObjectCSub
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pcMain: TPageControl
              Width = 537
              Height = 437
              ExplicitWidth = 537
              ExplicitHeight = 437
              inherited tabInterbedSystems: TTabSheet
                ExplicitTop = 30
                ExplicitWidth = 529
                ExplicitHeight = 403
                inherited rdgSubGroups: TRbwDataGrid4
                  Width = 529
                  Height = 347
                  FixedCols = 0
                  OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                  ExplicitWidth = 529
                  ExplicitHeight = 347
                end
                inherited pnlEditPkgProp: TPanel
                  Width = 529
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 529
                  inherited lblFormulaPkgProp: TLabel
                    Width = 59
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 59
                    ExplicitHeight = 19
                  end
                  inherited rdeFormulaPkgProp: TRbwDataEntry
                    StyleElements = [seFont, seClient, seBorder]
                  end
                end
              end
              inherited tabTransient: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 257
                inherited pnlBottom: TPanel
                  Top = 211
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 211
                  inherited lblNumTimes: TLabel
                    Top = 8
                    Width = 119
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitTop = 8
                    ExplicitWidth = 119
                    ExplicitHeight = 19
                  end
                  inherited seNumberOfTimes: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitHeight = 27
                  end
                  inherited btnDelete: TBitBtn
                    Left = 593
                    Top = 2
                    ExplicitLeft = 593
                    ExplicitTop = 2
                  end
                  inherited btnInsert: TBitBtn
                    Left = 505
                    Top = 2
                    ExplicitLeft = 505
                    ExplicitTop = 2
                  end
                end
                inherited pnlGrid: TPanel
                  Height = 211
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitHeight = 211
                  inherited pnlEditGrid: TPanel
                    StyleElements = [seFont, seClient, seBorder]
                    inherited lblFormula: TLabel
                      Width = 59
                      Height = 19
                      StyleElements = [seFont, seClient, seBorder]
                      ExplicitWidth = 59
                      ExplicitHeight = 19
                    end
                    inherited rdeFormula: TRbwDataEntry
                      StyleElements = [seFont, seClient, seBorder]
                    end
                  end
                  inherited rdgModflowBoundary: TRbwDataGrid4
                    Height = 153
                    OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                    ExplicitHeight = 153
                  end
                end
              end
            end
            inherited pnlCaption: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
            end
          end
        end
        object jvspSubPestObs: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Subsidence_and_Aquifer_System_'
          Caption = 'jvspSubPestObs'
          inline framePestObsSub: TframeSubPestObs
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited splObservations: TSplitter
              Top = 301
              Width = 537
              ExplicitTop = 298
              ExplicitWidth = 581
            end
            inherited grpDirectObs: TGroupBox
              Width = 537
              Height = 277
              ExplicitWidth = 537
              ExplicitHeight = 277
              inherited frameObservations: TframeGrid
                Top = 21
                Width = 533
                Height = 254
                ExplicitTop = 21
                ExplicitWidth = 533
                ExplicitHeight = 254
                inherited Panel: TPanel
                  Top = 222
                  Width = 533
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 222
                  ExplicitWidth = 533
                  inherited lbNumber: TLabel
                    Width = 218
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 218
                    ExplicitHeight = 19
                  end
                  inherited sbAdd: TSpeedButton
                    Left = 347
                    ExplicitLeft = 365
                  end
                  inherited sbInsert: TSpeedButton
                    Left = 376
                    ExplicitLeft = 394
                  end
                  inherited sbDelete: TSpeedButton
                    Left = 405
                    ExplicitLeft = 423
                  end
                  inherited seNumber: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitHeight = 27
                  end
                end
                inherited Grid: TRbwDataGrid4
                  Width = 533
                  Height = 222
                  ExplicitWidth = 533
                  ExplicitHeight = 222
                end
              end
            end
            inherited grpObsComparisons: TGroupBox
              Top = 306
              Width = 537
              ExplicitTop = 306
              ExplicitWidth = 537
              inherited frameObsComparisons: TframeGrid
                Top = 21
                Width = 533
                Height = 149
                ExplicitTop = 21
                ExplicitWidth = 533
                ExplicitHeight = 149
                inherited Panel: TPanel
                  Top = 114
                  Width = 533
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 114
                  ExplicitWidth = 533
                  inherited lbNumber: TLabel
                    Width = 262
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 262
                    ExplicitHeight = 19
                  end
                  inherited sbAdd: TSpeedButton
                    Left = 347
                    ExplicitLeft = 365
                  end
                  inherited sbInsert: TSpeedButton
                    Left = 376
                    ExplicitLeft = 394
                  end
                  inherited sbDelete: TSpeedButton
                    Left = 405
                    ExplicitLeft = 423
                  end
                  inherited seNumber: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitHeight = 27
                  end
                end
                inherited Grid: TRbwDataGrid4
                  Width = 533
                  Height = 114
                  ExplicitWidth = 533
                  ExplicitHeight = 114
                end
              end
            end
            inherited pnlCaption: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
            end
          end
        end
        object jvspSwtPestObs: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'SWT_Subsidence_and_Aquifer_Sys'
          Caption = 'jvspSwtPestObs'
          inline framePestObsSwt: TframeSubPestObs
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited splObservations: TSplitter
              Top = 301
              Width = 537
              ExplicitTop = 298
              ExplicitWidth = 581
            end
            inherited grpDirectObs: TGroupBox
              Width = 537
              Height = 277
              ExplicitWidth = 537
              ExplicitHeight = 277
              inherited frameObservations: TframeGrid
                Top = 21
                Width = 533
                Height = 254
                ExplicitTop = 21
                ExplicitWidth = 533
                ExplicitHeight = 254
                inherited Panel: TPanel
                  Top = 222
                  Width = 533
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 222
                  ExplicitWidth = 533
                  inherited lbNumber: TLabel
                    Width = 218
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 218
                    ExplicitHeight = 19
                  end
                  inherited sbAdd: TSpeedButton
                    Left = 438
                    ExplicitLeft = 447
                  end
                  inherited sbInsert: TSpeedButton
                    Left = 464
                    ExplicitLeft = 476
                  end
                  inherited sbDelete: TSpeedButton
                    Left = 493
                    ExplicitLeft = 505
                  end
                  inherited seNumber: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitHeight = 27
                  end
                end
                inherited Grid: TRbwDataGrid4
                  Width = 533
                  Height = 222
                  ExplicitWidth = 533
                  ExplicitHeight = 222
                end
              end
            end
            inherited grpObsComparisons: TGroupBox
              Top = 306
              Width = 537
              ExplicitTop = 306
              ExplicitWidth = 537
              inherited frameObsComparisons: TframeGrid
                Top = 21
                Width = 533
                Height = 149
                ExplicitTop = 21
                ExplicitWidth = 533
                ExplicitHeight = 149
                inherited Panel: TPanel
                  Top = 114
                  Width = 533
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 114
                  ExplicitWidth = 533
                  inherited lbNumber: TLabel
                    Width = 262
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 262
                    ExplicitHeight = 19
                  end
                  inherited sbAdd: TSpeedButton
                    Left = 438
                    ExplicitLeft = 447
                  end
                  inherited sbInsert: TSpeedButton
                    Left = 464
                    ExplicitLeft = 476
                  end
                  inherited sbDelete: TSpeedButton
                    Left = 493
                    ExplicitLeft = 505
                  end
                  inherited seNumber: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitHeight = 27
                  end
                end
                inherited Grid: TRbwDataGrid4
                  Width = 533
                  Height = 114
                  ExplicitWidth = 533
                  ExplicitHeight = 114
                end
              end
            end
            inherited pnlCaption: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
            end
          end
        end
        object jvspGwtCNC: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'CNC-GWT-Constant-Concentratio2'
          Caption = 'jvspGwtCNC'
          inline frameGwtCnc: TframeScreenObjectCnc
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 410
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 410
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblChemSpecies: TLabel
                Left = 157
                Width = 129
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 157
                ExplicitWidth = 129
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
              inherited comboChemSpecies: TComboBox
                Left = 6
                Top = 39
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameGwtCnccomboChemSpeciesChange
                ExplicitLeft = 6
                ExplicitTop = 39
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 385
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 385
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 333
                OnSetEditText = frameGwtCncrdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 333
              end
            end
          end
        end
        object jvspGwtSRC: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'SRC-GWT-Mass-Source-Loading-P2'
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'jvspGwtSRC'
          inline frameGwtSRC: TframeScreenObjectSrc
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 410
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 410
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblChemSpecies: TLabel
                Width = 129
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 129
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
              inherited comboChemSpecies: TComboBox
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameGwtSRCcomboChemSpeciesChange
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 385
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 385
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 333
                OnSetEditText = frameGwtSRCrdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 333
              end
            end
          end
        end
        object jvspFmp4Efficiency: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Efficiency-in-FMP-Farm-Process'
          Caption = 'jvspFmp4Efficiency'
          inline frameFmp4Efficiency: TframeScreenObjectFmp4Efficiency
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameFmp4EfficiencyseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnSetEditText = frameFmp4EfficiencyrdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4EfficiencyImprovement: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Efficiency-Improvement-in-FMP-'
          Caption = 'jvspFmp4EfficiencyImprovement'
          inline frameFmp4EfficiencyImprovement: TframeScreenObjectFmp4EfficiencyImprovement
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                Top = 7
                ExplicitLeft = 445
                ExplicitTop = 7
              end
              inherited btnInsert: TBitBtn
                Left = 361
                Top = 7
                ExplicitLeft = 361
                ExplicitTop = 7
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4BareRunoffFraction: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Bare-Runoff-Fraction-in-FMP-Fa'
          Caption = 'jvspFmp4BareRunoffFraction'
          inline frameFmp4BareRunoffFraction: TframeScreenObjectBareRunoffFraction
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4BarePrecipitationConsumptionFraction: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Bare-Precipitation-Consumption'
          Caption = 'jvspFmp4BarePrecipitationConsumptionFraction'
          inline frameFmp4BarePrecipitationConsumptionFraction: TframeScreenObjectFmp4BarePrecipitationConsumptionFraction
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4BareEvap: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Potential-Evaporation-Bare-in-'
          Caption = 'jvspFmp4BareEvap'
          inline frameFmp4BareEvap: TframeScreenObjectFmp4BareEvap
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4DirectRecharge: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Direct-Recharge-in-Climate-Far'
          Caption = 'jvspFmp4DirectRecharge'
          inline frameFmp4DirectRecharge: TframeScreenObjectFmp4DirectRecharge
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4PrecipPotConsumption: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Precipitaiton-Potential-Consum'
          Caption = 'jvspFmp4PrecipPotConsumption'
          inline frameFmp4PrecipPotConsumption: TframeScreenObjectFmp4PrecipPotConsumption
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4NrdInfilLoc: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          Caption = 'jvspFmp4NrdInfilLoc'
          inline frameFmp4NrdInfilLocation: TframeScreenObjectFmp4NrdInfilLoc
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4CropCoefficient: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Crop-Coefficient-in-LAND_USE-F'
          Caption = 'jvspFmp4CropCoefficient'
          inline frameFmp4CropCoefficient: TframeScreenObjectFmp4CropCoefficient
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4LandUseAreaFraction: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Land-Use-Area-Fraction-in-LAND'
          Caption = 'jvspFmp4LandUseAreaFraction'
          inline frameFmp4LandUseAreaFraction: TframeScreenObjectLandUseAreaFraction
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4LandUseAreaFractionMult: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Land-Use-Area-Fraction-in-LAND'
          Caption = 'jvspFmp4LandUseAreaFractionMult'
          inline frameMultLandUseAreaFraction: TframeScreenObjectMultLandUseAreaFraction
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4CropCoefficientMult: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Crop-Coefficient-in-LAND_USE-F'
          Caption = 'jvspFmp4CropCoefficientMult'
          inline frameFmp4MultCropCoefficients: TframeScreenObjectMultCropCoefficients
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4ConsumptiveUse: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Consumptive-Use-in-LAND_USE-Fa'
          Caption = 'jvspFmp4ConsumptiveUse'
          inline frameFmp4ConsumptiveUse: TframeScreenObjectFmp4ConsumptiveUse
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4ConsumptiveUseMult: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Consumptive-Use-in-LAND_USE-Fa'
          Caption = 'jvspFmp4ConsumptiveUseMult'
          inline frameFmp4MultConsumptiveUse: TframeScreenObjectMultConsumptiveUse
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4Irrigation: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Irrigation-in-LAND_USE-Farm-Pr'
          Caption = 'jvspFmp4Irrigation'
          inline frameFmp4Irrigation: TframeScreenObjectFmp4Irrigation
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4IrrigationMult: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Irrigation-in-LAND_USE-Farm-Pr'
          Caption = 'jvspFmp4IrrigationMult'
          inline frameFmp4MultIrrigation: TframeScreenObjectMultIrrigation
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4RootDepth: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Root-Depth-in-LAND_USE-Farm-Pr'
          Caption = 'jvspFmp4RootDepth'
          inline frameFmp4RootDepth: TframeScreenObjectFmp4RootDepth
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4RootDepthMult: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Root-Depth-in-LAND_USE-Farm-Pr'
          Caption = 'jvspFmp4RootDepthMult'
          inline frameFmp4MultRootDepth: TframeScreenObjectMultRootDepth
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4TranspirationFraction: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Transpiration-Fraction-in-LAND'
          Caption = 'jvspFmp4TranspirationFraction'
          inline frameFmp4TranspirationFraction: TframeScreenObjectFmp4TranspirationFraction
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4TranspirationFractionMult: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Transpiration-Fraction-in-LAND'
          Caption = 'jvspFmp4TranspirationFractionMult'
          inline frameFmp4MultTranspirationFraction: TframeScreenObjectMultTranspirationFraction
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4EvaporationIrrigationFraction: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Evaporation-Irrigation-Fractio'
          Caption = 'jvspFmp4EvaporationIrrigationFraction'
          inline frameFmp4EvaporationIrrigationFraction: TframeScreenObjectFmp4EvaporationIrrigationFraction
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4EvaporationIrrigationFractionMult: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Evaporation-Irrigation-Fractio'
          Caption = 'jvspFmp4EvaporationIrrigationFractionMult'
          inline frameFmp4MultEvaporationIrrigationFraction: TframeScreenObjectMultEvaporationIrrigationFraction
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4FractionOfPrecipToSurfaceWater: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Frac-Excess-Precip-in-SW-in-LA'
          Caption = 'jvspFmp4FractionOfPrecipToSurfaceWater'
          inline frameFmp4FractionOfPrecipToSurfaceWater: TframeScreenObjectFmp4FractionOfPrecipToSurfaceWater
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4FractionOfPrecipToSurfaceWaterMult: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Frac-Excess-Precip-in-SW-in-LA'
          Caption = 'jvspFmp4FractionOfPrecipToSurfaceWaterMult'
          inline frameFmp4MultFractionOfPrecipToSurfaceWater: TframeScreenObjectMultFractionOfPrecipToSurfaceWater
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4FractionOfIrrigToSurfaceWater: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Frac-Excess-Irrig-in-SW-in-LAN'
          Caption = 'jvspFmp4FractionOfIrrigToSurfaceWater'
          inline frameFmp4FractionOfIrrigToSurfaceWater: TframeScreenObjectFmp4FractionOfIrrigToSurfaceWater
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4FractionOfIrrigToSurfaceWaterMult: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Frac-Excess-Irrig-in-SW-in-LAN'
          Caption = 'jvspFmp4FractionOfIrrigToSurfaceWaterMult'
          inline frameFmp4MultFractionOfIrrigToSurfaceWater: TframeScreenObjectMultFractionOfIrrigToSurfaceWater
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4AddedDemand: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Added-Demand-in-LAND_USE-Farm-'
          Caption = 'jvspFmp4AddedDemand'
          inline frameFmp4AddedDemand: TframeScreenObjectFmp4AddedDemand
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4AddedDemandMult: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Added-Demand-in-LAND_USE-Farm-'
          Caption = 'jvspFmp4AddedDemandMult'
          inline frameFmp4MultAddedDemand: TframeScreenObjectMultAddedDemand
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4CropHasSalinityDemand: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Crop-has-Salinity-Demand-in-SA'
          Caption = 'jvspFmp4CropHasSalinityDemand'
          inline frameFmp4CropHasSalinityDemand: TframeScreenObjectFmp4CropHasSalinityRequirement
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4CropHasSalinityDemandMult: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Crop-has-Salinity-Demand-in-SA'
          Caption = 'jvspFmp4CropHasSalinityDemandMult'
          inline frameFmp4MultCropHasSalinityDemand: TframeScreenObjectMultCropHasSalinityDemand
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspFmp4AddedDemandRunoffSplit: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Added-Demand-Runoff-Split-in-W'
          Caption = 'jvspFmp4AddedDemandRunoffSplit'
          inline frameFmp4AddedDemandRunoffSplit: TframeScreenObjectAddedDemandRunoffSplit
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspTvk: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'TVK-Time-Varying-Hydraulic-Co2'
          Caption = 'jvspTVK'
          inline frameScreenObjectTvk: TframeScreenObjectTransientK
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnSetEditText = frameScreenObjectTvkrdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspTvs: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'TVS-Time-Varying-Storage-Pack2'
          Caption = 'jvspTvs'
          inline frameScreenObjectTvs: TframeScreenObjectTransientS
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 445
                ExplicitLeft = 445
              end
              inherited btnInsert: TBitBtn
                Left = 361
                ExplicitLeft = 361
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 407
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 407
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 355
                OnSetEditText = frameScreenObjectTvsrdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 535
                ExplicitHeight = 355
              end
            end
          end
        end
        object jvspGweCTP: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          Caption = 'jvspGweCTP'
          inline frameGweCTP: TframeScreenObjectCtp
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 410
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 410
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblChemSpecies: TLabel
                Width = 129
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 129
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 449
                ExplicitLeft = 449
              end
              inherited btnInsert: TBitBtn
                Left = 365
                ExplicitLeft = 365
              end
              inherited comboChemSpecies: TComboBox
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameGweCTPcomboChemSpeciesChange
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 385
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 385
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 333
                OnSetEditText = frameGweCTPrdgModflowBoundarySetEditText
                ExplicitWidth = 535
                ExplicitHeight = 333
              end
            end
          end
        end
        object jvspGweESL: TJvStandardPage
          Left = 0
          Top = 0
          Width = 537
          Height = 478
          Caption = 'jvspGweESL'
          inline frameGweESL: TframeScreenObjectEsl
            Left = 0
            Top = 0
            Width = 537
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 537
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 410
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 410
              ExplicitWidth = 537
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblChemSpecies: TLabel
                Width = 129
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 129
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 449
                ExplicitLeft = 449
              end
              inherited btnInsert: TBitBtn
                Left = 365
                ExplicitLeft = 365
              end
              inherited comboChemSpecies: TComboBox
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                OnChange = frameGweESLcomboChemSpeciesChange
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 537
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              inherited pnlCaption: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
              end
            end
            inherited pnlGrid: TPanel
              Width = 537
              Height = 385
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 537
              ExplicitHeight = 385
              inherited pnlEditGrid: TPanel
                Width = 535
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 535
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
              inherited rdgModflowBoundary: TRbwDataGrid4
                Width = 535
                Height = 333
                OnSetEditText = frameGweESLrdgModflowBoundarySetEditText
                ExplicitWidth = 535
                ExplicitHeight = 333
              end
            end
          end
        end
      end
    end
    object tabSutraFeatures: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'SUTRA_Features_Tab'
      Caption = 'SUTRA Features'
      ImageIndex = 9
      object splttrSutraFeatures: TJvNetscapeSplitter
        Left = 161
        Top = 0
        Height = 478
        Align = alLeft
        Maximized = False
        Minimized = False
        ButtonCursor = crDefault
        ExplicitLeft = 240
        ExplicitTop = 208
        ExplicitHeight = 100
      end
      object jvplSutraFeatures: TJvPageList
        Left = 171
        Top = 0
        Width = 561
        Height = 478
        ActivePage = jvspSutraStateObs
        PropagateEnable = False
        Align = alClient
        OnChange = jvplSutraFeaturesChange
        object jvspSutraObservations: TJvStandardPage
          Left = 0
          Top = 0
          Width = 561
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'SutraObservations'
          Caption = 'jvspSutraObservations'
          inline frameSutraObservations: TframeSutraObservations
            Left = 0
            Top = 0
            Width = 561
            Height = 478
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 561
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 432
              Width = 561
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 432
              ExplicitWidth = 561
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 473
                OnClick = frameSutraObservationsbtnDeleteClick
                ExplicitLeft = 473
              end
              inherited btnInsert: TBitBtn
                Left = 385
                Top = 7
                OnClick = frameSutraObservationsbtnInsertClick
                ExplicitLeft = 385
                ExplicitTop = 7
              end
            end
            inherited pnlGrid: TPanel
              Width = 561
              Height = 239
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 561
              ExplicitHeight = 239
              inherited rdgSutraFeature: TRbwDataGrid4
                Width = 559
                Height = 237
                ExplicitWidth = 559
                ExplicitHeight = 237
                RowHeights = (
                  24
                  24)
              end
            end
            inherited pnlTop: TPanel
              Width = 561
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 561
              inherited lblSchedule: TLabel
                Width = 153
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 153
                ExplicitHeight = 19
              end
              inherited lblObservationFormat: TLabel
                Width = 220
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 220
                ExplicitHeight = 19
              end
              inherited lblName: TLabel
                Width = 128
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 128
                ExplicitHeight = 19
              end
              inherited pnlCaption: TPanel
                Width = 559
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 559
              end
              inherited comboSchedule: TComboBox
                Width = 173
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 173
                ExplicitHeight = 27
              end
              inherited comboObservationFormat: TComboBox
                Width = 129
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 129
                ExplicitHeight = 27
              end
              inherited edName: TEdit
                Width = 126
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 126
                ExplicitHeight = 27
              end
            end
          end
        end
        object jvspSutraSpecifiedPressure: TJvStandardPage
          Left = 0
          Top = 0
          Width = 561
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'SUTRA_Boundary_Condition_Panes'
          Caption = 'jvspSutraSpecifiedPressure'
          inline frameSutraSpecifiedPressure: TframeSutraBoundary
            Left = 0
            Top = 0
            Width = 561
            Height = 478
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 561
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 350
              Width = 561
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 350
              ExplicitWidth = 561
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblFluidSourceInLakesPresent: TLabel
                Width = 220
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 220
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 474
                ExplicitLeft = 474
              end
              inherited btnInsert: TBitBtn
                Left = 386
                ExplicitLeft = 386
              end
              inherited comboFluidSourceInLakesPresent: TComboBox
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
            end
            inherited pnlGrid: TPanel
              Width = 561
              Height = 269
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 561
              ExplicitHeight = 269
              inherited rdgSutraFeature: TRbwDataGrid4
                Width = 559
                Height = 217
                OnButtonClick = SutraBoundaryButtonClick
                Columns = <
                  item
                    AutoAdjustRowHeights = True
                    AutoAdjustCaptionRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    AutoAdjustCaptionRowHeights = True
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
                    AutoAdjustColWidths = False
                  end
                  item
                    AutoAdjustRowHeights = True
                    AutoAdjustCaptionRowHeights = True
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
                    AutoAdjustCaptionRowHeights = True
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
                ExplicitWidth = 559
                ExplicitHeight = 217
                ColWidths = (
                  64
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
              inherited pnlEditGrid: TPanel
                Width = 559
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 559
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
            end
            inherited pnlTop: TPanel
              Width = 561
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 561
              inherited lblSchedule: TLabel
                Width = 153
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 153
                ExplicitHeight = 19
              end
              inherited pnlCaption: TPanel
                Width = 559
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 559
              end
              inherited comboSchedule: TComboBox
                Left = 6
                Width = 173
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 6
                ExplicitWidth = 173
                ExplicitHeight = 27
              end
            end
          end
        end
        object jvspSutraSpecTempConc: TJvStandardPage
          Left = 0
          Top = 0
          Width = 561
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'SUTRA_Boundary_Condition_Panes'
          Caption = 'jvspSutraSpecTempConc'
          inline frameSutraSpecTempConc: TframeSutraBoundary
            Left = 0
            Top = 0
            Width = 561
            Height = 478
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 561
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 350
              Width = 561
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 350
              ExplicitWidth = 561
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblFluidSourceInLakesPresent: TLabel
                Width = 220
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 220
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 474
                ExplicitLeft = 474
              end
              inherited btnInsert: TBitBtn
                Left = 386
                ExplicitLeft = 386
              end
              inherited comboFluidSourceInLakesPresent: TComboBox
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
            end
            inherited pnlGrid: TPanel
              Width = 561
              Height = 269
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 561
              ExplicitHeight = 269
              inherited rdgSutraFeature: TRbwDataGrid4
                Width = 559
                Height = 217
                OnButtonClick = SutraBoundaryButtonClick
                ExplicitWidth = 559
                ExplicitHeight = 217
                ColWidths = (
                  64
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
              inherited pnlEditGrid: TPanel
                Width = 559
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 559
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
            end
            inherited pnlTop: TPanel
              Width = 561
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 561
              inherited lblSchedule: TLabel
                Width = 153
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 153
                ExplicitHeight = 19
              end
              inherited pnlCaption: TPanel
                Width = 559
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 559
              end
              inherited comboSchedule: TComboBox
                Width = 175
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 175
                ExplicitHeight = 27
              end
            end
          end
        end
        object jvspSutraFluidFlux: TJvStandardPage
          Left = 0
          Top = 0
          Width = 561
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'SUTRA_Boundary_Condition_Panes'
          Caption = 'jvspSutraFluidFlux'
          inline frameSutraFluidFlux: TframeSutraBoundary
            Left = 0
            Top = 0
            Width = 561
            Height = 478
            HelpType = htKeyword
            HelpKeyword = 'SUTRA_Boundary_Condition_Panes'
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 561
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 350
              Width = 561
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 350
              ExplicitWidth = 561
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblFluidSourceInLakesPresent: TLabel
                Width = 220
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 220
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 474
                ExplicitLeft = 474
              end
              inherited btnInsert: TBitBtn
                Left = 386
                ExplicitLeft = 386
              end
              inherited comboFluidSourceInLakesPresent: TComboBox
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
            end
            inherited pnlGrid: TPanel
              Width = 561
              Height = 269
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 561
              ExplicitHeight = 269
              inherited rdgSutraFeature: TRbwDataGrid4
                Width = 559
                Height = 217
                OnButtonClick = SutraBoundaryButtonClick
                ExplicitWidth = 559
                ExplicitHeight = 217
                ColWidths = (
                  64
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
              inherited pnlEditGrid: TPanel
                Width = 559
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 559
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
            end
            inherited pnlTop: TPanel
              Width = 561
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 561
              inherited lblSchedule: TLabel
                Width = 153
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 153
                ExplicitHeight = 19
              end
              inherited pnlCaption: TPanel
                Width = 559
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 559
              end
              inherited comboSchedule: TComboBox
                Width = 175
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 175
                ExplicitHeight = 27
              end
            end
          end
        end
        object jvspSutraMassEnergyFlux: TJvStandardPage
          Left = 0
          Top = 0
          Width = 561
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'SUTRA_Boundary_Condition_Panes'
          Caption = 'jvspSutraMassEnergyFlux'
          inline frameSutraMassEnergyFlux: TframeSutraBoundary
            Left = 0
            Top = 0
            Width = 561
            Height = 478
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 561
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 350
              Width = 561
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 350
              ExplicitWidth = 561
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblFluidSourceInLakesPresent: TLabel
                Width = 220
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 220
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 474
                Top = 85
                ExplicitLeft = 474
                ExplicitTop = 85
              end
              inherited btnInsert: TBitBtn
                Left = 386
                ExplicitLeft = 386
              end
              inherited comboFluidSourceInLakesPresent: TComboBox
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
            end
            inherited pnlGrid: TPanel
              Width = 561
              Height = 269
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 561
              ExplicitHeight = 269
              inherited rdgSutraFeature: TRbwDataGrid4
                Width = 559
                Height = 217
                OnButtonClick = SutraBoundaryButtonClick
                Columns = <
                  item
                    AutoAdjustRowHeights = True
                    AutoAdjustCaptionRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    AutoAdjustCaptionRowHeights = True
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
                    AutoAdjustColWidths = False
                  end
                  item
                    AutoAdjustRowHeights = True
                    AutoAdjustCaptionRowHeights = True
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
                    AutoAdjustCaptionRowHeights = True
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
                ExplicitWidth = 559
                ExplicitHeight = 217
                ColWidths = (
                  64
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
              inherited pnlEditGrid: TPanel
                Width = 559
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 559
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited rdeFormula: TRbwDataEntry
                  StyleElements = [seFont, seClient, seBorder]
                end
              end
            end
            inherited pnlTop: TPanel
              Width = 561
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 561
              inherited lblSchedule: TLabel
                Width = 153
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 153
                ExplicitHeight = 19
              end
              inherited pnlCaption: TPanel
                Width = 559
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 559
              end
              inherited comboSchedule: TComboBox
                Width = 175
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 175
                ExplicitHeight = 27
              end
            end
          end
        end
        object jvspSutraBlank: TJvStandardPage
          Left = 0
          Top = 0
          Width = 561
          Height = 478
          Caption = 'jvspSutraBlank'
        end
        object jvspSutraLake: TJvStandardPage
          Left = 0
          Top = 0
          Width = 561
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Lake'
          Caption = 'jvspSutraLake'
          inline frameSutraLake: TframeSutraLake
            Left = 0
            Top = 0
            Width = 561
            Height = 478
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 561
            ExplicitHeight = 478
            inherited lblInitialStage: TLabel
              Width = 136
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 136
              ExplicitHeight = 19
            end
            inherited lblInitialU: TLabel
              Width = 304
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 304
              ExplicitHeight = 19
            end
            inherited lblFractionRechargeDiverted: TLabel
              Width = 273
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 273
              ExplicitHeight = 19
            end
            inherited lblFractionDischargeDiverted: TLabel
              Width = 280
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 280
              ExplicitHeight = 19
            end
            inherited pnlCaption: TPanel
              Width = 555
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 555
            end
            inherited btnedInitialStage: TButtonedEdit
              Width = 539
              Height = 27
              StyleElements = [seFont, seClient, seBorder]
              OnRightButtonClick = btnFormulaClick
              ExplicitWidth = 539
              ExplicitHeight = 27
            end
            inherited btnedInitialU: TButtonedEdit
              Top = 124
              Width = 539
              Height = 27
              StyleElements = [seFont, seClient, seBorder]
              OnRightButtonClick = btnFormulaClick
              ExplicitTop = 124
              ExplicitWidth = 539
              ExplicitHeight = 27
            end
            inherited btnedFractionRechargeDiverted: TButtonedEdit
              Width = 539
              Height = 27
              StyleElements = [seFont, seClient, seBorder]
              OnRightButtonClick = btnFormulaClick
              ExplicitWidth = 539
              ExplicitHeight = 27
            end
            inherited btnedFractionDischargeDiverted: TButtonedEdit
              Top = 262
              Width = 539
              Height = 27
              StyleElements = [seFont, seClient, seBorder]
              OnRightButtonClick = btnFormulaClick
              ExplicitTop = 262
              ExplicitWidth = 539
              ExplicitHeight = 27
            end
          end
        end
        object jvspGeneralizedFlow: TJvStandardPage
          Left = 0
          Top = 0
          Width = 561
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Generalized_Flow_Pane'
          Caption = 'jvspGeneralizedFlow'
          inline frameSutraGeneralizedFlowBoundary: TframeSutraGeneralizedFlowBoundary
            Left = 0
            Top = 0
            Width = 561
            Height = 478
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 561
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 351
              Width = 561
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 351
              ExplicitWidth = 561
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblGeneralizedFlowPresent: TLabel
                Width = 220
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 220
                ExplicitHeight = 19
              end
              inherited lblLakeGeneralizedFlowType: TLabel
                Left = 298
                Width = 113
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 298
                ExplicitWidth = 113
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 470
                ExplicitLeft = 470
              end
              inherited btnInsert: TBitBtn
                Left = 382
                ExplicitLeft = 382
              end
              inherited comboGeneralizedFlowPresent: TComboBox
                Top = 44
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitTop = 44
                ExplicitHeight = 27
              end
              inherited comboLakeGeneralizedFlowType: TComboBox
                Left = 298
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 298
                ExplicitHeight = 27
              end
            end
            inherited pnlGrid: TPanel
              Top = 139
              Width = 561
              Height = 212
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 139
              ExplicitWidth = 561
              ExplicitHeight = 212
              inherited rdgSutraFeature: TRbwDataGrid4
                Width = 559
                Height = 210
                OnButtonClick = SutraBoundaryButtonClick
                ExplicitWidth = 559
                ExplicitHeight = 210
                ColWidths = (
                  64
                  64
                  64
                  64
                  64
                  64
                  64
                  64
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
            inherited pnlTop: TPanel
              Width = 561
              Height = 89
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 561
              ExplicitHeight = 89
              inherited lblSchedule: TLabel
                Width = 153
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 153
                ExplicitHeight = 19
              end
              inherited pnlCaption: TPanel
                Width = 559
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 559
              end
              inherited comboSchedule: TComboBox
                Left = 6
                Top = 49
                Width = 160
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 6
                ExplicitTop = 49
                ExplicitWidth = 160
                ExplicitHeight = 27
              end
            end
            inherited pnlEditGrid: TPanel
              Top = 89
              Width = 561
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 89
              ExplicitWidth = 561
              inherited lblFormula: TLabel
                Width = 59
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 59
                ExplicitHeight = 19
              end
              inherited rdeFormula: TRbwDataEntry
                StyleElements = [seFont, seClient, seBorder]
              end
              inherited comboLimit: TJvImageComboBox
                Height = 29
                StyleElements = [seFont, seClient, seBorder]
                ItemHeight = 23
                ExplicitHeight = 29
              end
              inherited comboExit: TJvImageComboBox
                Height = 29
                StyleElements = [seFont, seClient, seBorder]
                ItemHeight = 23
                ExplicitHeight = 29
              end
            end
          end
        end
        object jvspGeneralizedTransport: TJvStandardPage
          Left = 0
          Top = 0
          Width = 561
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Generalized_Transport_Pane'
          Caption = 'jvspGeneralizedTransport'
          inline frameSutraGeneralizeTransBoundary: TframeSutraGeneralizeTransBoundary
            Left = 0
            Top = 0
            Width = 561
            Height = 478
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 561
            ExplicitHeight = 478
            inherited pnlBottom: TPanel
              Top = 357
              Width = 561
              StyleElements = [seFont, seClient, seBorder]
              ExplicitTop = 357
              ExplicitWidth = 561
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblGeneralizedTransportPresent: TLabel
                Width = 220
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 220
                ExplicitHeight = 19
              end
              inherited lbl1: TLabel
                Left = 300
                Width = 113
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 300
                ExplicitWidth = 113
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 468
                ExplicitLeft = 468
              end
              inherited btnInsert: TBitBtn
                Left = 384
                ExplicitLeft = 384
              end
              inherited comboGeneralizedTransportPresent: TComboBox
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitHeight = 27
              end
              inherited comboLakeGeneralizedTransportType: TComboBox
                Left = 300
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitLeft = 300
                ExplicitHeight = 27
              end
            end
            inherited pnlGrid: TPanel
              Width = 561
              Height = 226
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 561
              ExplicitHeight = 226
              inherited rdgSutraFeature: TRbwDataGrid4
                Width = 559
                Height = 224
                OnButtonClick = SutraBoundaryButtonClick
                ExplicitWidth = 559
                ExplicitHeight = 224
                ColWidths = (
                  64
                  64
                  64
                  64
                  64
                  64)
                RowHeights = (
                  24
                  24)
              end
            end
            inherited pnlTop: TPanel
              Width = 561
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 561
              inherited lblSchedule: TLabel
                Width = 153
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 153
                ExplicitHeight = 19
              end
              inherited pnlCaption: TPanel
                Width = 559
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 559
              end
              inherited comboSchedule: TComboBox
                Width = 173
                Height = 27
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 173
                ExplicitHeight = 27
              end
            end
            inherited pnlEditGrid: TPanel
              Width = 561
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 561
              inherited lblFormula: TLabel
                Width = 59
                Height = 19
                StyleElements = [seFont, seClient, seBorder]
                ExplicitWidth = 59
                ExplicitHeight = 19
              end
              inherited rdeFormula: TRbwDataEntry
                StyleElements = [seFont, seClient, seBorder]
              end
            end
          end
        end
        object jvspSutraStateObs: TJvStandardPage
          Left = 0
          Top = 0
          Width = 561
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'Sutra_State_Calibration_Observ'
          Caption = 'jvspSutraStateObs'
          inline frameSutraPestObsState: TframePestObsCaptioned
            Left = 0
            Top = 0
            Width = 561
            Height = 478
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 561
            ExplicitHeight = 478
            inherited splObservations: TSplitter
              Top = 301
              Width = 561
              ExplicitTop = 298
              ExplicitWidth = 605
            end
            inherited grpDirectObs: TGroupBox
              Width = 561
              Height = 260
              ExplicitTop = 41
              ExplicitWidth = 561
              ExplicitHeight = 260
              inherited frameObservations: TframeGrid
                Top = 21
                Width = 557
                Height = 237
                ExplicitTop = 21
                ExplicitWidth = 557
                ExplicitHeight = 237
                inherited Panel: TPanel
                  Top = 205
                  Width = 557
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 205
                  ExplicitWidth = 557
                  inherited lbNumber: TLabel
                    Width = 218
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 218
                    ExplicitHeight = 19
                  end
                  inherited sbAdd: TSpeedButton
                    Left = 347
                    ExplicitLeft = 365
                  end
                  inherited sbInsert: TSpeedButton
                    Left = 376
                    ExplicitLeft = 394
                  end
                  inherited sbDelete: TSpeedButton
                    Left = 405
                    ExplicitLeft = 423
                  end
                  inherited seNumber: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitHeight = 27
                  end
                end
                inherited Grid: TRbwDataGrid4
                  Width = 557
                  Height = 205
                  ExplicitWidth = 557
                  ExplicitHeight = 205
                end
              end
            end
            inherited grpObsComparisons: TGroupBox
              Top = 306
              Width = 561
              ExplicitTop = 306
              ExplicitWidth = 561
              inherited frameObsComparisons: TframeGrid
                Top = 21
                Width = 557
                Height = 149
                ExplicitTop = 21
                ExplicitWidth = 557
                ExplicitHeight = 149
                inherited Panel: TPanel
                  Top = 114
                  Width = 557
                  StyleElements = [seFont, seClient, seBorder]
                  ExplicitTop = 114
                  ExplicitWidth = 557
                  inherited lbNumber: TLabel
                    Width = 262
                    Height = 19
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitWidth = 262
                    ExplicitHeight = 19
                  end
                  inherited sbAdd: TSpeedButton
                    Left = 347
                    ExplicitLeft = 365
                  end
                  inherited sbInsert: TSpeedButton
                    Left = 376
                    ExplicitLeft = 394
                  end
                  inherited sbDelete: TSpeedButton
                    Left = 405
                    ExplicitLeft = 423
                  end
                  inherited seNumber: TJvSpinEdit
                    Height = 27
                    StyleElements = [seFont, seClient, seBorder]
                    ExplicitHeight = 27
                  end
                end
                inherited Grid: TRbwDataGrid4
                  Width = 557
                  Height = 114
                  ExplicitWidth = 557
                  ExplicitHeight = 114
                end
              end
            end
            inherited pnlCaption: TPanel
              Width = 561
              StyleElements = [seFont, seClient, seBorder]
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 561
            end
          end
        end
        object jvspSutraSpecPresObs: TJvStandardPage
          Left = 0
          Top = 0
          Width = 561
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'SUTRA_Boundary_Observations'
          Caption = 'jvspSutraSpecPresObs'
          OnShow = jvspSutraSpecPresObsShow
          inline frameSutraSpecPresObs: TframeFluxObs
            Left = 0
            Top = 0
            Width = 561
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 561
            ExplicitHeight = 478
            inherited lblFluxObservations: TLabel
              Width = 125
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 125
              ExplicitHeight = 19
            end
            inherited rdgObservationGroups: TRbwDataGrid4
              Width = 555
              Height = 443
              OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
              OnStateChange = frameSutraSpecPresObsrdgObservationGroupsStateChange
              ExplicitWidth = 555
              ExplicitHeight = 443
            end
            inherited btnAddOrRemoveFluxObservations: TButton
              OnClick = frameSutraUObsbtnAddOrRemoveFluxObservationsClick
            end
          end
        end
        object jvspSutraSpecFlowObs: TJvStandardPage
          Left = 0
          Top = 0
          Width = 561
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'SUTRA_Boundary_Observations'
          Caption = 'jvspSutraSpecFlowObs'
          OnShow = jvspSutraSpecFlowObsShow
          inline frameSutraSpecifiedFluidFlowObs: TframeFluxObs
            Left = 0
            Top = 0
            Width = 561
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 561
            ExplicitHeight = 478
            inherited lblFluxObservations: TLabel
              Width = 125
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 125
              ExplicitHeight = 19
            end
            inherited rdgObservationGroups: TRbwDataGrid4
              Width = 555
              Height = 443
              OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
              OnStateChange = frameSutraSpecifiedFluidFlowObsrdgObservationGroupsStateChange
              ExplicitWidth = 555
              ExplicitHeight = 443
            end
            inherited btnAddOrRemoveFluxObservations: TButton
              OnClick = frameSutraUObsbtnAddOrRemoveFluxObservationsClick
            end
          end
        end
        object jvspSutraSpecUObs: TJvStandardPage
          Left = 0
          Top = 0
          Width = 561
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'SUTRA_Boundary_Observations'
          Caption = 'jvspSutraSpecUObs'
          OnShow = jvspSutraSpecUObsShow
          inline frameSutraSpecUObs: TframeFluxObs
            Left = 0
            Top = 0
            Width = 561
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 561
            ExplicitHeight = 478
            inherited lblFluxObservations: TLabel
              Width = 125
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 125
              ExplicitHeight = 19
            end
            inherited rdgObservationGroups: TRbwDataGrid4
              Top = 34
              Width = 555
              Height = 443
              OnStateChange = frameSutraSpecUObsrdgObservationGroupsStateChange
              ExplicitTop = 34
              ExplicitWidth = 555
              ExplicitHeight = 443
            end
          end
        end
        object jvspSutraGenFlowObs: TJvStandardPage
          Left = 0
          Top = 0
          Width = 561
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'SUTRA_Boundary_Observations'
          Caption = 'jvspSutraGenFlowObs'
          OnShow = jvspSutraGenFlowObsShow
          inline frameSutraGenFlowObs: TframeFluxObs
            Left = 0
            Top = 0
            Width = 561
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 561
            ExplicitHeight = 478
            inherited lblFluxObservations: TLabel
              Width = 125
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 125
              ExplicitHeight = 19
            end
            inherited rdgObservationGroups: TRbwDataGrid4
              Width = 555
              Height = 443
              OnStateChange = frameSutraGenFlowObsrdgObservationGroupsStateChange
              ExplicitWidth = 555
              ExplicitHeight = 443
            end
          end
        end
        object jvspSutraGenTransObs: TJvStandardPage
          Left = 0
          Top = 0
          Width = 561
          Height = 478
          HelpType = htKeyword
          HelpKeyword = 'SUTRA_Boundary_Observations'
          Caption = 'jvspSutraGenTransObs'
          OnShow = jvspSutraGenTransObsShow
          inline frameSutraGenTransObs: TframeFluxObs
            Left = 0
            Top = 0
            Width = 561
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 561
            ExplicitHeight = 478
            inherited lblFluxObservations: TLabel
              Width = 125
              Height = 19
              StyleElements = [seFont, seClient, seBorder]
              ExplicitWidth = 125
              ExplicitHeight = 19
            end
            inherited rdgObservationGroups: TRbwDataGrid4
              Top = 33
              Width = 555
              Height = 443
              OnStateChange = frameSutraGenTransObsrdgObservationGroupsStateChange
              ExplicitTop = 33
              ExplicitWidth = 555
              ExplicitHeight = 443
            end
          end
        end
      end
      object jvpltvSutraFeatures: TJvPageListTreeView
        Left = 0
        Top = 0
        Width = 161
        Height = 478
        AutoExpand = False
        ShowLines = True
        PageDefault = 0
        PageList = jvplSutraFeatures
        Align = alLeft
        HideSelection = False
        StateImages = ilCheckImages
        Indent = 19
        TabOrder = 0
        OnCustomDrawItem = jvpltvSutraFeaturesCustomDrawItem
        OnMouseDown = jvpltvSutraFeaturesMouseDown
        Items.NodeData = {
          07010000001054004A007600500061006700650049006E006400650078004E00
          6F006400650043000000000000000000000001000000FFFFFFFF000000000000
          0000000000000001125300750074007200610020004F00620073006500720076
          006100740069006F006E007300}
        Items.Links = {0100000000000000}
      end
    end
    object tabFootprintFeatures: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Footprint_Well_Tab'
      Caption = 'Footprint Well'
      ImageIndex = 10
      inline frameScreenObjectFootprintWell: TframeScreenObjectFootprintWell
        Left = 0
        Top = 0
        Width = 732
        Height = 478
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 732
        ExplicitHeight = 478
        inherited lblPumpingRate: TLabel
          Width = 170
          Height = 19
          StyleElements = [seFont, seClient, seBorder]
          ExplicitWidth = 170
          ExplicitHeight = 19
        end
        inherited edPumpingRate: TRbwEdit
          Left = 192
          Width = 442
          Height = 27
          Anchors = [akLeft, akTop, akRight]
          StyleElements = [seFont, seClient, seBorder]
          ExplicitLeft = 192
          ExplicitWidth = 442
          ExplicitHeight = 27
        end
        inherited btnPumpingRate: TButton
          Left = 640
          Width = 89
          Height = 31
          Anchors = [akTop, akRight]
          OnClick = btnFormulaClick
          ExplicitLeft = 640
          ExplicitWidth = 89
          ExplicitHeight = 31
        end
      end
    end
    object tabNodes: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Vertices_Tab'
      Caption = 'Vertices'
      ImageIndex = 1
      object dgVerticies: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 732
        Height = 437
        Align = alClient
        DefaultColWidth = 25
        FixedCols = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = 19
        Font.Name = 'Arial'
        Font.Pitch = fpVariable
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
        ParentFont = False
        TabOrder = 0
        OnEnter = dgVerticiesEnter
        OnExit = dgVerticiesExit
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = True
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnBeforeDrawCell = dgVerticiesBeforeDrawCell
        OnEndUpdate = dgVerticiesEndUpdate
        OnStateChange = dgVerticiesStateChange
        ColorRangeSelection = False
        Columns = <
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = False
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'MS Sans Serif'
            ButtonFont.Pitch = fpVariable
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 25
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
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = False
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'MS Sans Serif'
            ButtonFont.Pitch = fpVariable
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 25
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4Real
            LimitToList = False
            MaxLength = 40
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
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'MS Sans Serif'
            ButtonFont.Pitch = fpVariable
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 25
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
            AutoAdjustRowHeights = True
            AutoAdjustCaptionRowHeights = False
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Pitch = fpVariable
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
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
        WordWrapRowCaptions = False
        ColWidths = (
          25
          25
          25
          25
          25)
        RowHeights = (
          24
          24
          24
          24
          24)
      end
      object Panel5: TPanel
        Left = 0
        Top = 437
        Width = 732
        Height = 41
        Align = alBottom
        TabOrder = 1
        object Button1: TButton
          Left = 16
          Top = 6
          Width = 185
          Height = 25
          Caption = 'Import Vertex Values'
          TabOrder = 0
          OnClick = btnImportVertexValuesClick
        end
      end
    end
    object tabVertexValues: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Vertex_Values_Tab'
      Caption = 'Vertex Values'
      ImageIndex = 8
      ParentShowHint = False
      ShowHint = False
      object rdgVertexValues: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 732
        Height = 437
        Align = alClient
        ColCount = 2
        FixedCols = 1
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = False
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
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = False
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
          64)
        RowHeights = (
          24
          24)
      end
      object Panel4: TPanel
        Left = 0
        Top = 437
        Width = 732
        Height = 41
        Align = alBottom
        TabOrder = 1
        object btnImportVertexValues: TButton
          Left = 16
          Top = 6
          Width = 185
          Height = 25
          Caption = 'Import Vertex Values'
          TabOrder = 0
          OnClick = btnImportVertexValuesClick
        end
      end
    end
    object tabImportedData: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Imported_Data_Tab'
      Caption = 'Imported Data'
      ImageIndex = 5
      DesignSize = (
        732
        478)
      object rdgImportedData: TRbwDataGrid4
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 732
        Height = 438
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 40
        Align = alClient
        ColCount = 2
        FixedCols = 1
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs]
        TabOrder = 0
        OnSetEditText = rdgImportedDataSetEditText
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
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
        WordWrapRowCaptions = False
        ColWidths = (
          64
          64)
        RowHeights = (
          24
          24)
      end
      object btnCopyImportedData: TButton
        Left = 3
        Top = 443
        Width = 75
        Height = 33
        Anchors = [akLeft, akBottom]
        Caption = 'Copy'
        TabOrder = 1
        OnClick = btnCopyImportedDataClick
      end
    end
    object tabDynamicTimeSeries: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Time-Series-Tab'
      Caption = 'Time Series'
      ImageIndex = 11
      inline frameDynamicTimeSeries: TframeDynamicScreenObjectsContainer
        Left = 0
        Top = 0
        Width = 732
        Height = 478
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 732
        ExplicitHeight = 478
        inherited plTimeSeries: TJvPageList
          Width = 611
          Height = 437
          ExplicitWidth = 611
          ExplicitHeight = 437
        end
        inherited pnlBottom: TPanel
          Top = 437
          Width = 732
          StyleElements = [seFont, seClient, seBorder]
          ExplicitTop = 437
          ExplicitWidth = 732
          inherited btnAddGroup: TButton
            OnClick = frameDynamicTimeSeriesbtnAddGroupClick
          end
        end
        inherited tvTimeSeries: TJvPageListTreeView
          Height = 437
          Items.Links = {00000000}
          ExplicitHeight = 437
        end
      end
    end
    object tabComments: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Comments_Tab'
      Caption = 'Comments/Captions'
      ImageIndex = 6
      object splComment: TSplitter
        Left = 0
        Top = 273
        Width = 732
        Height = 3
        Cursor = crVSplit
        Align = alTop
        ExplicitTop = 105
        ExplicitWidth = 47
      end
      object grpComment: TGroupBox
        Left = 0
        Top = 276
        Width = 732
        Height = 202
        Align = alClient
        Caption = 'Comment'
        TabOrder = 1
        object lblComments: TLabel
          AlignWithMargins = True
          Left = 5
          Top = 31
          Width = 722
          Height = 44
          Margins.Top = 10
          Align = alClient
          Caption = 
            'If there is anything unusual or complicated about this object, y' +
            'ou can describe it here so you will understand how it works late' +
            'r.'
          WordWrap = True
          ExplicitWidth = 705
          ExplicitHeight = 38
        end
        object memoComments: TMemo
          Left = 2
          Top = 78
          Width = 728
          Height = 122
          Align = alBottom
          Anchors = [akLeft, akTop, akRight, akBottom]
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object pnlText: TPanel
        Left = 0
        Top = 0
        Width = 732
        Height = 273
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object grpCaption: TGroupBox
          Left = 0
          Top = 0
          Width = 732
          Height = 137
          Align = alTop
          Caption = 'Caption to appear near object'
          TabOrder = 0
          DesignSize = (
            732
            137)
          object lblCaptionX: TLabel
            Left = 16
            Top = 50
            Width = 56
            Height = 19
            Caption = 'X offset'
          end
          object lblCaptionY: TLabel
            Left = 16
            Top = 78
            Width = 56
            Height = 19
            Caption = 'Y offset'
          end
          object btnCaptionFont: TButton
            Left = 16
            Top = 106
            Width = 75
            Height = 25
            Caption = 'Font'
            TabOrder = 4
            OnClick = btnCaptionFontClick
          end
          object memoCaption: TMemo
            Left = 176
            Top = 24
            Width = 531
            Height = 107
            Anchors = [akLeft, akTop, akRight, akBottom]
            Lines.Strings = (
              'Caption')
            ScrollBars = ssBoth
            TabOrder = 1
            WordWrap = False
            OnChange = memoCaptionChange
          end
          object rdeCaptionX: TRbwDataEntry
            Left = 78
            Top = 47
            Width = 83
            Height = 22
            TabOrder = 2
            Text = '0'
            DataType = dtInteger
            Max = 1.000000000000000000
            ChangeDisabledColor = True
          end
          object rdeCaptionY: TRbwDataEntry
            Left = 78
            Top = 75
            Width = 83
            Height = 22
            TabOrder = 3
            Text = '0'
            DataType = dtInteger
            Max = 1.000000000000000000
            ChangeDisabledColor = True
          end
          object cbCaptionVisible: TCheckBox
            Left = 16
            Top = 24
            Width = 97
            Height = 17
            Caption = 'Visible'
            TabOrder = 0
            OnClick = cbCaptionVisibleClick
          end
        end
        object Panel6: TPanel
          Left = 0
          Top = 137
          Width = 732
          Height = 136
          Align = alClient
          TabOrder = 1
          object grpLabelVertices: TGroupBox
            Left = 1
            Top = 1
            Width = 360
            Height = 134
            Align = alLeft
            Caption = 'Vertex labels'
            TabOrder = 0
            object lblVertexXOffset: TLabel
              Left = 207
              Top = 26
              Width = 56
              Height = 19
              Caption = 'X offset'
            end
            object lblVertexYOffset: TLabel
              Left = 207
              Top = 51
              Width = 56
              Height = 19
              Caption = 'Y offset'
            end
            object lblVertexValueLabels: TLabel
              Left = 3
              Top = 64
              Width = 133
              Height = 19
              Caption = 'Vertex value labels'
            end
            object btnVertexFont: TButton
              Left = 277
              Top = 84
              Width = 75
              Height = 27
              Caption = 'Font'
              TabOrder = 4
              OnClick = btnVertexFontClick
            end
            object rdeVertexXOffset: TRbwDataEntry
              Left = 269
              Top = 23
              Width = 83
              Height = 22
              TabOrder = 0
              Text = '0'
              DataType = dtInteger
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
            object rdeVertexYOffset: TRbwDataEntry
              Left = 269
              Top = 48
              Width = 83
              Height = 22
              TabOrder = 1
              Text = '0'
              DataType = dtInteger
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
            object cbVertexLabelVisible: TCheckBox
              Left = 3
              Top = 28
              Width = 190
              Height = 17
              Caption = 'Vertex numbers visible'
              TabOrder = 2
              OnClick = cbVertexLabelVisibleClick
            end
            object comboVertexValueLabels: TComboBox
              Left = 3
              Top = 84
              Width = 268
              Height = 27
              Style = csDropDownList
              TabOrder = 3
              Items.Strings = (
                'None')
            end
          end
          object gbSectionLabels: TGroupBox
            Left = 361
            Top = 1
            Width = 370
            Height = 134
            Align = alClient
            Caption = 'Section labels'
            TabOrder = 1
            object lblSectionXOffset: TLabel
              Left = 221
              Top = 26
              Width = 56
              Height = 19
              Caption = 'X offset'
            end
            object lblSectionYOffset: TLabel
              Left = 221
              Top = 51
              Width = 56
              Height = 19
              Caption = 'Y offset'
            end
            object btnSectionFont: TButton
              Left = 6
              Top = 84
              Width = 75
              Height = 27
              Caption = 'Font'
              TabOrder = 3
              OnClick = btnSectionFontClick
            end
            object rdeSectionXOffset: TRbwDataEntry
              Left = 283
              Top = 23
              Width = 83
              Height = 22
              TabOrder = 0
              Text = '0'
              DataType = dtInteger
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
            object rdeSectionYOffset: TRbwDataEntry
              Left = 283
              Top = 48
              Width = 83
              Height = 22
              TabOrder = 1
              Text = '0'
              DataType = dtInteger
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
            object cbSectionLabelVisible: TCheckBox
              Left = 3
              Top = 28
              Width = 190
              Height = 17
              Caption = 'Section numbers visible'
              TabOrder = 2
              OnClick = cbVertexLabelVisibleClick
            end
          end
        end
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 516
    Width = 740
    Height = 41
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    DesignSize = (
      740
      41)
    object btnOK: TBitBtn
      Left = 534
      Top = 6
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        04000000000068010000120B0000120B00001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
      TabOrder = 4
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 629
      Top = 6
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 5
      OnClick = btnCancelClick
    end
    object btnHelp: TBitBtn
      Left = 449
      Top = 6
      Width = 79
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 3
      OnClick = btnHelpClick
    end
    object btnCopyVertices: TButton
      Left = 4
      Top = 6
      Width = 117
      Height = 33
      Caption = 'Copy vertices'
      TabOrder = 0
      Visible = False
      OnClick = btnCopyVerticesClick
    end
    object btnConvertTimeUnits: TButton
      Left = 127
      Top = 6
      Width = 161
      Height = 33
      Caption = 'Convert time units'
      TabOrder = 1
      Visible = False
      OnClick = btnConvertTimeUnitsClick
    end
    object btnEditFeatureFormulas: TButton
      Left = 294
      Top = 6
      Width = 169
      Height = 33
      Caption = 'Edit feature formulas'
      TabOrder = 2
      Visible = False
      OnClick = btnEditFeatureFormulasClick
    end
  end
  object coldlgColors: TColorDialog
    Left = 640
    Top = 328
  end
  object rparserTopFormulaElements: TRbwParser
    Left = 424
    Top = 376
  end
  object rparserFrontFormulaElements: TRbwParser
    Left = 448
    Top = 304
  end
  object rparserSideFormulaElements: TRbwParser
    Left = 424
    Top = 280
  end
  object rparserThreeDFormulaElements: TRbwParser
    Left = 432
    Top = 160
  end
  object rparserTopFormulaNodes: TRbwParser
    Left = 352
    Top = 240
  end
  object rparserFrontFormulaNodes: TRbwParser
    Left = 296
    Top = 312
  end
  object rparserSideFormulaNodes: TRbwParser
    Left = 272
    Top = 272
  end
  object rparserThreeDFormulaNodes: TRbwParser
    Left = 376
    Top = 152
  end
  object ilCheckImages: TImageList
    Left = 640
    Top = 248
    Bitmap = {
      494C010104000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
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
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFF80078007800780079FF79FF79FF795579FF79FF79DF788A7
      9FF79FF798F790579FF79FF7907780279FF79FF7923790179FF79FF797178207
      9FF79FF79F9795179FF79FF79FD78A879FF79FF79FF795578007800780078007
      8007800780078007FFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object dlgFontCaption: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 56
    Top = 40
  end
end
