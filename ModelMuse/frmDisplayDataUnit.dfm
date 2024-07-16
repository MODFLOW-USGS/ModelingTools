inherited frmDisplayData: TfrmDisplayData
  HelpType = htKeyword
  HelpKeyword = 'Data_Visualization_Dialog_Box'
  Caption = 'Data Visualization'
  ClientHeight = 562
  ClientWidth = 778
  ExplicitWidth = 790
  ExplicitHeight = 600
  TextHeight = 18
  object splSplit: TSplitter
    Left = 201
    Top = 0
    Width = 5
    Height = 521
    ExplicitLeft = 178
    ExplicitHeight = 420
  end
  object pglstMain: TJvPageList
    Left = 206
    Top = 0
    Width = 572
    Height = 521
    ActivePage = jvspPestObsResults
    PropagateEnable = False
    Align = alClient
    OnChange = pglstMainChange
    object jvspModpathPathline: TJvStandardPage
      Left = 0
      Top = 0
      Width = 572
      Height = 521
      HelpType = htKeyword
      HelpKeyword = 'MODPATH_Display_Dialog_Box'
      Caption = 'jvspModpathPathline'
      inline frameModpathDisplay: TframeModpathDisplay
        Left = 0
        Top = 0
        Width = 572
        Height = 521
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 572
        ExplicitHeight = 521
        inherited pcMain: TPageControl
          Width = 572
          Height = 521
          ExplicitWidth = 572
          ExplicitHeight = 521
          inherited tabBasic: TTabSheet
            ExplicitTop = 29
            ExplicitWidth = 564
            ExplicitHeight = 488
            DesignSize = (
              564
              488)
            inherited lblModpathFile: TLabel
              Width = 159
              Height = 18
              ExplicitWidth = 159
              ExplicitHeight = 18
            end
            inherited lblColorScheme: TLabel
              Width = 97
              Height = 18
              ExplicitWidth = 97
              ExplicitHeight = 18
            end
            inherited pbColorScheme: TPaintBox
              Top = 172
              Width = 497
              ExplicitTop = 172
              ExplicitWidth = 517
            end
            inherited lblColorAdjustment: TLabel
              Top = 216
              Width = 117
              Height = 18
              ExplicitTop = 216
              ExplicitWidth = 117
              ExplicitHeight = 18
            end
            inherited lblCycles: TLabel
              Left = 402
              Top = 222
              Width = 47
              Height = 18
              ExplicitLeft = 422
              ExplicitTop = 222
              ExplicitWidth = 47
              ExplicitHeight = 18
            end
            inherited lblMaxTime: TLabel
              Width = 78
              Height = 18
              ExplicitWidth = 78
              ExplicitHeight = 18
            end
            inherited lblModelSelection: TLabel
              Top = 278
              Width = 109
              Height = 18
              ExplicitTop = 278
              ExplicitWidth = 109
              ExplicitHeight = 18
            end
            inherited fedModpathFile: TJvFilenameEdit
              Width = 497
              Height = 26
              ExplicitWidth = 501
              ExplicitHeight = 26
            end
            inherited comboColorScheme: TComboBox
              Width = 351
              Height = 26
              ExplicitWidth = 351
              ExplicitHeight = 26
            end
            inherited jsColorExponent: TJvxSlider
              Top = 232
            end
            inherited seColorExponent: TJvSpinEdit
              Top = 246
              Height = 26
              ExplicitTop = 246
              ExplicitHeight = 26
            end
            inherited seCycles: TJvSpinEdit
              Left = 402
              Top = 246
              Height = 26
              ExplicitLeft = 406
              ExplicitTop = 246
              ExplicitHeight = 26
            end
            inherited comboModelSelection: TComboBox
              Top = 297
              Height = 26
              ExplicitTop = 297
              ExplicitHeight = 26
            end
            inherited btnColorSchemes: TButton
              Left = 365
              Top = 120
              Width = 140
              Anchors = [akTop, akRight]
              OnClick = frameModpathDisplaybtnColorSchemesClick
              ExplicitLeft = 369
              ExplicitTop = 120
              ExplicitWidth = 140
            end
          end
          inherited tabOptions: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 453
            DesignSize = (
              457
              453)
            inherited rgShow2D: TRadioGroup
              Width = 550
              Items.Strings = (
                'Show all'
                'Specify columns, rows, layers, times and/or groups to show'
                
                  'Specify starting columns, rows, layers, times and/or groups to s' +
                  'how'
                
                  'Specify ending columns, rows, layers, times and/or groups to sho' +
                  'w')
              ExplicitWidth = 550
            end
            inherited rdgLimits: TRbwDataGrid4
              Width = 407
              ExplicitWidth = 407
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
                24)
            end
          end
        end
      end
    end
    object jvspSfrStreamLinks: TJvStandardPage
      Left = 0
      Top = 0
      Width = 572
      Height = 521
      HelpType = htKeyword
      HelpKeyword = 'Stream_Links_Pane'
      Caption = 'jvspSfrStreamLinks'
      inline frameSfrStreamLink: TframeStreamLink
        Left = 0
        Top = 0
        Width = 572
        Height = 521
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 572
        ExplicitHeight = 521
        inherited shpStreamColor: TShape
          Top = 4
          ExplicitTop = 4
        end
        inherited shpDiversionColor: TShape
          Top = 35
          ExplicitTop = 35
        end
        inherited lblTimeToPlot: TLabel
          Width = 84
          Height = 18
          ExplicitWidth = 84
          ExplicitHeight = 18
        end
        inherited shpUnconnectedColor: TShape
          Top = 66
          ExplicitTop = 66
        end
        inherited lblSquareSize: TLabel
          Width = 140
          Height = 18
          ExplicitWidth = 140
          ExplicitHeight = 18
        end
        inherited btnStreamColor: TButton
          Top = 4
          ExplicitTop = 4
        end
        inherited btnDiversionColor: TButton
          Top = 35
          ExplicitTop = 35
        end
        inherited rgItemsToPlot: TRadioGroup
          Height = 112
          ExplicitHeight = 112
        end
        inherited cbStreams: TCheckBox
          Width = 150
          ExplicitWidth = 150
        end
        inherited cbPlotDiversions: TCheckBox
          Width = 166
          ExplicitWidth = 166
        end
        inherited comboTimeToPlot: TJvComboBox
          Height = 26
          ExplicitHeight = 26
        end
        inherited cbPlotUnconnected: TCheckBox
          Width = 222
          ExplicitWidth = 222
        end
        inherited btnUnconnectedColor: TButton
          Top = 66
          ExplicitTop = 66
        end
        inherited seSquareSize: TJvSpinEdit
          Height = 26
          ExplicitHeight = 26
        end
        inherited dlgLinkColor: TColorDialog
          Left = 208
        end
      end
    end
    object jvspHeadObsResults: TJvStandardPage
      Left = 0
      Top = 0
      Width = 572
      Height = 521
      HelpType = htKeyword
      HelpKeyword = 'Head_Observation_Results'
      Caption = 'jvspHeadObsResults'
      inline frameHeadObservationResults: TframeHeadObservationResults
        Left = 0
        Top = 0
        Width = 572
        Height = 521
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 572
        ExplicitHeight = 521
        inherited pgcHeadObs: TPageControl
          Width = 572
          Height = 485
          ExplicitWidth = 572
          ExplicitHeight = 485
          inherited tabControls: TTabSheet
            ExplicitTop = 29
            ExplicitWidth = 564
            ExplicitHeight = 452
            inherited lblNegativeColor: TLabel
              Width = 190
              Height = 36
              ExplicitWidth = 190
              ExplicitHeight = 36
            end
            inherited lblColorPositive: TLabel
              Width = 185
              Height = 36
              ExplicitWidth = 185
              ExplicitHeight = 36
            end
            inherited lblMaxSymbolSize: TLabel
              Top = 351
              Width = 206
              Height = 18
              ExplicitTop = 351
              ExplicitWidth = 206
              ExplicitHeight = 18
            end
            inherited lblHeadObsResults: TLabel
              Width = 69
              Height = 18
              ExplicitWidth = 69
              ExplicitHeight = 18
            end
            inherited flnmedHeadObsResults: TJvFilenameEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited grpbxFilter: TGroupBox
              inherited lblMaximumTime: TLabel
                Width = 101
                Height = 18
                ExplicitWidth = 101
                ExplicitHeight = 18
              end
              inherited lblMaxResidual: TLabel
                Width = 126
                Height = 18
                ExplicitWidth = 126
                ExplicitHeight = 18
              end
              inherited lblMinimumTime: TLabel
                Width = 97
                Height = 18
                ExplicitWidth = 97
                ExplicitHeight = 18
              end
              inherited lblMinResidual: TLabel
                Width = 122
                Height = 18
                ExplicitWidth = 122
                ExplicitHeight = 18
              end
              inherited lblMinLayer: TLabel
                Width = 100
                Height = 18
                ExplicitWidth = 100
                ExplicitHeight = 18
              end
              inherited lblMaxLayer: TLabel
                Width = 104
                Height = 18
                ExplicitWidth = 104
                ExplicitHeight = 18
              end
              inherited framelmtMinimumTime: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited framelmtMaxResidual: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited framelmtMaximumTime: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited framelmtMinResidual: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited framelmtMinLayer: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited framelmtMaxLayer: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
            end
            inherited clrbtnNegative: TJvColorButton
              Top = 321
              ExplicitTop = 321
            end
            inherited clrbtnPositive: TJvColorButton
              Top = 321
              ExplicitTop = 321
            end
            inherited spinSymbolSize: TJvSpinEdit
              Top = 348
              Height = 26
              ExplicitTop = 348
              ExplicitHeight = 26
            end
          end
          inherited tabValues: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 399
            inherited rdgHeadObs: TRbwDataGrid4
              Height = 334
              ExplicitHeight = 334
              ColWidths = (
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
                24
                24
                24
                24)
            end
            inherited pnlValueControls: TPanel
              Top = 334
              ExplicitTop = 334
              inherited btnCopy: TButton
                Left = 151
                Width = 146
                ExplicitLeft = 151
                ExplicitWidth = 146
              end
              inherited btnHightlightObjects: TButton
                Top = 2
                Width = 141
                Height = 55
                ExplicitTop = 2
                ExplicitWidth = 141
                ExplicitHeight = 55
              end
              inherited btnRestore: TButton
                Left = 151
                Top = 2
                Width = 146
                ExplicitLeft = 151
                ExplicitTop = 2
                ExplicitWidth = 146
              end
            end
          end
          inherited tabLegend: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 399
            inherited lblMax: TLabel
              Width = 44
              Height = 18
              ExplicitWidth = 44
              ExplicitHeight = 18
            end
            inherited lblHalfMax: TLabel
              Width = 71
              Height = 18
              ExplicitWidth = 71
              ExplicitHeight = 18
            end
          end
          inherited tabGraph: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 399
            inherited pbHeadObs: TPaintBox
              Height = 318
              ExplicitWidth = 570
              ExplicitHeight = 403
            end
            inherited pnlGraphControls: TPanel
              Top = 318
              ExplicitTop = 318
              inherited lblGraphInstructions: TLabel
                Width = 73
                Height = 54
                Anchors = [akLeft, akTop, akRight]
                ExplicitWidth = 73
                ExplicitHeight = 54
              end
            end
          end
        end
        inherited pnlBottom: TPanel
          Top = 485
          Width = 572
          ExplicitTop = 485
          ExplicitWidth = 572
          inherited lblRMS: TLabel
            Width = 222
            Height = 18
            ExplicitWidth = 222
            ExplicitHeight = 18
          end
          inherited comboModels: TComboBox
            Top = 4
            Height = 26
            ExplicitTop = 4
            ExplicitHeight = 26
          end
        end
      end
    end
    object jvspModpathTimeSeries: TJvStandardPage
      Left = 0
      Top = 0
      Width = 572
      Height = 521
      HelpType = htKeyword
      HelpKeyword = 'MODPATH_Time_Series_Display'
      Caption = 'jvspModpathTimeSeries'
      inline frameModpathTimeSeriesDisplay: TframeModpathTimeSeriesDisplay
        Left = 0
        Top = 0
        Width = 572
        Height = 521
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 572
        ExplicitHeight = 521
        inherited pcMain: TPageControl
          Width = 572
          Height = 521
          ExplicitWidth = 572
          ExplicitHeight = 521
          inherited tabBasic: TTabSheet
            ExplicitTop = 29
            ExplicitWidth = 564
            ExplicitHeight = 488
            DesignSize = (
              564
              488)
            inherited lblModpathFile: TLabel
              Width = 182
              Height = 18
              ExplicitWidth = 182
              ExplicitHeight = 18
            end
            inherited lblTimeToPlot: TLabel
              Width = 80
              Height = 18
              ExplicitWidth = 80
              ExplicitHeight = 18
            end
            inherited lblColorScheme: TLabel
              Width = 97
              Height = 18
              ExplicitWidth = 97
              ExplicitHeight = 18
            end
            inherited pbColorScheme: TPaintBox
              Width = 492
              ExplicitWidth = 545
            end
            inherited lblColorAdjustment: TLabel
              Width = 117
              Height = 18
              ExplicitWidth = 117
              ExplicitHeight = 18
            end
            inherited lblCycles: TLabel
              Left = 297
              Width = 47
              Height = 18
              ExplicitLeft = 317
              ExplicitWidth = 47
              ExplicitHeight = 18
            end
            inherited lblModelSelection: TLabel
              Width = 109
              Height = 18
              ExplicitWidth = 109
              ExplicitHeight = 18
            end
            inherited lbltimeSeriesSize: TLabel
              Left = 297
              Width = 167
              Height = 18
              ExplicitLeft = 317
              ExplicitWidth = 167
              ExplicitHeight = 18
            end
            inherited fedModpathFile: TJvFilenameEdit
              Top = 32
              Width = 492
              Height = 26
              ExplicitTop = 32
              ExplicitWidth = 492
              ExplicitHeight = 26
            end
            inherited comboTimeToPlot: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboColorScheme: TComboBox
              Left = 2
              Top = 190
              Width = 368
              Height = 26
              ExplicitLeft = 2
              ExplicitTop = 190
              ExplicitWidth = 368
              ExplicitHeight = 26
            end
            inherited seColorExponent: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited seCycles: TJvSpinEdit
              Left = 297
              Height = 26
              ExplicitLeft = 297
              ExplicitHeight = 26
            end
            inherited comboModelSelection: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited setimeSeriesSize: TJvSpinEdit
              Left = 297
              Height = 26
              ExplicitLeft = 297
              ExplicitHeight = 26
            end
            inherited btnColorSchemes: TButton
              Left = 376
              Top = 178
              Width = 128
              Anchors = [akTop, akRight]
              OnClick = frameModpathTimeSeriesDisplaybtnColorSchemesClick
              ExplicitLeft = 376
              ExplicitTop = 178
              ExplicitWidth = 128
            end
          end
          inherited tabOptions: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 454
            DesignSize = (
              457
              454)
            inherited rgShow2D: TRadioGroup
              Width = 551
              ExplicitWidth = 551
            end
            inherited rdgLimits: TRbwDataGrid4
              Width = 737
              Height = 354
              Anchors = [akLeft, akTop, akRight, akBottom]
              ExplicitWidth = 737
              ExplicitHeight = 354
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
                24)
            end
          end
        end
      end
    end
    object jvspModpathEndpoints: TJvStandardPage
      Left = 0
      Top = 0
      Width = 572
      Height = 521
      HelpType = htKeyword
      HelpKeyword = 'MODPATH_Endpoint_Display'
      Caption = 'jvspModpathEndpoints'
      inline frameModpathEndpointDisplay1: TframeModpathEndpointDisplay
        Left = 0
        Top = 0
        Width = 572
        Height = 521
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 572
        ExplicitHeight = 521
        inherited pcMain: TPageControl
          Width = 572
          Height = 521
          ExplicitWidth = 572
          ExplicitHeight = 521
          inherited tabBasic: TTabSheet
            ExplicitTop = 29
            ExplicitWidth = 564
            ExplicitHeight = 488
            DesignSize = (
              564
              488)
            inherited lblModpathFile: TLabel
              Width = 165
              Height = 18
              ExplicitWidth = 165
              ExplicitHeight = 18
            end
            inherited lblColorScheme: TLabel
              Width = 97
              Height = 18
              ExplicitWidth = 97
              ExplicitHeight = 18
            end
            inherited pbColorScheme: TPaintBox
              Width = 500
              ExplicitWidth = 556
            end
            inherited lblColorAdjustment: TLabel
              Width = 117
              Height = 18
              ExplicitWidth = 117
              ExplicitHeight = 18
            end
            inherited lblCycles: TLabel
              Left = 329
              Width = 47
              Height = 18
              ExplicitLeft = 349
              ExplicitWidth = 47
              ExplicitHeight = 18
            end
            inherited lblModelSelection: TLabel
              Width = 109
              Height = 18
              ExplicitWidth = 109
              ExplicitHeight = 18
            end
            inherited lblEndPointSize: TLabel
              Left = 329
              Width = 148
              Height = 18
              ExplicitLeft = 349
              ExplicitWidth = 148
              ExplicitHeight = 18
            end
            inherited fedModpathFile: TJvFilenameEdit
              Top = 30
              Width = 500
              Height = 26
              ExplicitTop = 30
              ExplicitWidth = 504
              ExplicitHeight = 26
            end
            inherited comboColorScheme: TComboBox
              Width = 353
              Height = 26
              ExplicitWidth = 353
              ExplicitHeight = 26
            end
            inherited seColorExponent: TJvSpinEdit
              Top = 235
              Height = 26
              ExplicitTop = 235
              ExplicitHeight = 26
            end
            inherited seCycles: TJvSpinEdit
              Left = 329
              Top = 235
              Width = 84
              Height = 26
              ExplicitLeft = 333
              ExplicitTop = 235
              ExplicitWidth = 84
              ExplicitHeight = 26
            end
            inherited comboModelSelection: TComboBox
              Left = 2
              Top = 297
              Height = 26
              TabOrder = 8
              ExplicitLeft = 2
              ExplicitTop = 297
              ExplicitHeight = 26
            end
            inherited seEndPointSize: TJvSpinEdit
              Left = 329
              Top = 297
              Height = 26
              TabOrder = 7
              ExplicitLeft = 333
              ExplicitTop = 297
              ExplicitHeight = 26
            end
            inherited btnColorSchemes: TButton
              Left = 367
              Top = 120
              Width = 138
              Anchors = [akTop, akRight]
              OnClick = frameModpathEndpointDisplay1btnColorSchemesClick
              ExplicitLeft = 371
              ExplicitTop = 120
              ExplicitWidth = 138
            end
          end
          inherited tabOptions: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 465
            DesignSize = (
              468
              465)
            inherited rgShow2D: TRadioGroup
              Width = 316
              ExplicitWidth = 316
            end
            inherited rgWhereToPlot: TRadioGroup
              Left = 331
              ExplicitLeft = 331
            end
            inherited rgColorBy: TRadioGroup
              Height = 270
              ExplicitHeight = 270
            end
            inherited rdgLimits: TRbwDataGrid4
              Width = 481
              Height = 496
              Anchors = [akLeft, akTop, akRight, akBottom]
              ExplicitWidth = 481
              ExplicitHeight = 496
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
                24)
            end
          end
          inherited tabLegend: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 465
            inherited imLegend: TImage
              Height = 465
              ExplicitWidth = 262
              ExplicitHeight = 468
            end
            inherited splColor: TSplitter
              Height = 465
              ExplicitHeight = 468
            end
            inherited pnlLegend: TPanel
              Height = 465
              ExplicitHeight = 465
              DesignSize = (
                201
                465)
              inherited lblMethod: TLabel
                Width = 52
                Height = 18
                ExplicitWidth = 52
                ExplicitHeight = 18
              end
              inherited lblColorLegendRows: TLabel
                Width = 109
                Height = 18
                ExplicitWidth = 109
                ExplicitHeight = 18
              end
              inherited comboMethod: TComboBox
                Height = 26
                ExplicitHeight = 26
              end
              inherited seLegendRows: TJvSpinEdit
                Height = 26
                ExplicitHeight = 26
              end
              inherited rdgLegend: TRbwDataGrid4
                ColWidths = (
                  64)
                RowHeights = (
                  24
                  24
                  24
                  24
                  24)
              end
            end
          end
        end
      end
    end
    object jvspColorGrid: TJvStandardPage
      Left = 0
      Top = 0
      Width = 572
      Height = 521
      HelpType = htKeyword
      HelpKeyword = 'Color_Grid_Dialog_Box'
      Caption = 'jvspColorGrid'
      inline frameColorGrid: TframeColorGrid
        Left = 0
        Top = 0
        Width = 572
        Height = 521
        Align = alClient
        TabOrder = 0
        OnResize = frameColorGridResize
        ExplicitWidth = 572
        ExplicitHeight = 521
        inherited pcChoices: TPageControl
          Width = 572
          Height = 521
          ExplicitWidth = 572
          ExplicitHeight = 521
          inherited tabSelection: TTabSheet
            ExplicitTop = 29
            ExplicitWidth = 564
            ExplicitHeight = 488
            inherited lblDataSet: TLabel
              Width = 212
              Height = 18
              ExplicitWidth = 212
              ExplicitHeight = 18
            end
            inherited lblColorScheme: TLabel
              Top = 284
              Width = 97
              Height = 18
              ExplicitTop = 289
              ExplicitWidth = 97
              ExplicitHeight = 18
            end
            inherited lblCycles: TLabel
              Left = 411
              Top = 335
              Width = 47
              Height = 18
              ExplicitLeft = 463
              ExplicitTop = 315
              ExplicitWidth = 47
              ExplicitHeight = 18
            end
            inherited pbColorScheme: TPaintBox
              Left = 8
              Top = 352
              Width = 397
              ExplicitLeft = 8
              ExplicitTop = 332
              ExplicitWidth = 449
            end
            inherited lblColorAdjustment: TLabel
              Top = 391
              Width = 117
              Height = 18
              ExplicitTop = 401
              ExplicitWidth = 117
              ExplicitHeight = 18
            end
            inherited lblComment: TLabel
              Width = 204
              Height = 18
              ExplicitWidth = 204
              ExplicitHeight = 18
            end
            inherited lblTime: TLabel
              Left = 419
              Top = 2
              Width = 34
              Height = 18
              ExplicitLeft = 443
              ExplicitTop = 2
              ExplicitWidth = 34
              ExplicitHeight = 18
            end
            inherited comboColorScheme: TComboBox
              Left = 8
              Top = 308
              Width = 253
              Height = 26
              ExplicitLeft = 8
              ExplicitTop = 308
              ExplicitWidth = 253
              ExplicitHeight = 26
            end
            inherited seCycles: TJvSpinEdit
              Left = 411
              Top = 359
              Height = 26
              ExplicitLeft = 415
              ExplicitTop = 359
              ExplicitHeight = 26
            end
            inherited jsColorExponent: TJvxSlider
              Left = 2
              Top = 415
            end
            inherited seColorExponent: TJvSpinEdit
              Left = 158
              Top = 418
              Height = 26
              ExplicitLeft = 158
              ExplicitTop = 418
              ExplicitHeight = 26
            end
            inherited cbLogTransform: TCheckBox
              Left = 257
              Top = 421
              Height = 20
              ExplicitLeft = 257
              ExplicitTop = 421
              ExplicitHeight = 20
            end
            inherited udDataSets: TJvUpDown
              Left = 381
              Height = 26
              ExplicitLeft = 385
              ExplicitHeight = 26
            end
            inherited rgUpdateLimitChoice: TRadioGroup
              Top = 200
              Width = 334
              ExplicitTop = 200
              ExplicitWidth = 338
            end
            inherited virttreecomboDataSets: TRbwStringTreeCombo
              Left = 7
              Width = 374
              Height = 26
              Tree.DefaultNodeHeight = 20
              ExplicitLeft = 7
              ExplicitWidth = 394
              ExplicitHeight = 26
            end
            inherited reComment: TRichEdit
              Width = 501
              Height = 111
              ParentFont = True
              ExplicitWidth = 501
              ExplicitHeight = 111
            end
            inherited btnColorSchemes: TButton
              Left = 284
              Top = 301
              Width = 120
              ExplicitLeft = 288
              ExplicitTop = 301
              ExplicitWidth = 120
            end
            inherited udTime: TJvUpDown
              Left = 500
              Top = 26
              Height = 26
              ExplicitLeft = 504
              ExplicitTop = 26
              ExplicitHeight = 26
            end
            inherited comboTime3D: TJvComboBox
              Left = 409
              Height = 26
              ExplicitLeft = 409
              ExplicitHeight = 26
            end
          end
          inherited tabFilters: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 468
            inherited lblLowerLimit: TLabel
              Width = 75
              Height = 18
              ExplicitWidth = 75
              ExplicitHeight = 18
            end
            inherited lblUpperLimit: TLabel
              Width = 75
              Height = 18
              ExplicitWidth = 75
              ExplicitHeight = 18
            end
            inherited lblValuesToIgnore: TLabel
              Width = 112
              Height = 18
              ExplicitWidth = 112
              ExplicitHeight = 18
            end
            inherited lblNumberOfValuesToIgnore: TLabel
              Left = 135
              Top = 453
              Width = 185
              Height = 18
              ExplicitLeft = 135
              ExplicitTop = 442
              ExplicitWidth = 185
              ExplicitHeight = 18
            end
            inherited lblEpsilon: TLabel
              Width = 168
              Height = 18
              ExplicitWidth = 168
              ExplicitHeight = 18
            end
            inherited frameCheck3DMax: TframeDisplayLimit
              inherited cbCheck: TCheckBox
                Left = 2
                Top = -2
                ExplicitLeft = 2
                ExplicitTop = -2
              end
              inherited comboBoolLimit: TComboBox
                Height = 26
                ExplicitHeight = 26
              end
            end
            inherited frameCheck3DMin: TframeDisplayLimit
              inherited cbCheck: TCheckBox
                Top = -2
                ExplicitTop = -2
              end
              inherited comboBoolLimit: TComboBox
                Height = 26
                ExplicitHeight = 26
              end
            end
            inherited cbActiveOnly: TCheckBox
              Width = 145
              ExplicitWidth = 145
            end
            inherited rdgValuesToIgnore: TRbwDataGrid4
              Height = 327
              ExplicitHeight = 327
              ColWidths = (
                64)
              RowHeights = (
                24
                24)
            end
            inherited seNumberOfValuesToIgnore: TJvSpinEdit
              Top = 445
              Height = 26
              ExplicitTop = 445
              ExplicitHeight = 26
            end
          end
          inherited tabLegend: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 468
            inherited imLegend: TImage
              Height = 468
              ExplicitWidth = 357
              ExplicitHeight = 395
            end
            inherited splColor: TSplitter
              Height = 468
              OnMoved = frameColorGridsplColorMoved
              ExplicitHeight = 468
            end
            inherited pnlLegend: TPanel
              Height = 468
              ExplicitHeight = 468
              inherited lblMethod: TLabel
                Width = 52
                Height = 18
                ExplicitWidth = 52
                ExplicitHeight = 18
              end
              inherited lblColorLegendRows: TLabel
                Width = 109
                Height = 18
                ExplicitWidth = 109
                ExplicitHeight = 18
              end
              inherited comboMethod: TComboBox
                Height = 26
                ExplicitHeight = 26
              end
              inherited seLegendRows: TJvSpinEdit
                Height = 26
                ExplicitHeight = 26
              end
              inherited rdgLegend: TRbwDataGrid4
                ColWidths = (
                  64)
                RowHeights = (
                  24
                  24
                  24
                  24
                  24)
              end
              inherited btnFont: TButton
                Top = 433
                Anchors = [akLeft, akBottom]
                ExplicitTop = 433
              end
            end
          end
        end
        inherited timerLegend: TTimer
          Left = 304
          Top = 160
        end
        inherited dlgFontLegend: TFontDialog
          Top = 456
        end
      end
    end
    object jvspContourData: TJvStandardPage
      Left = 0
      Top = 0
      Width = 572
      Height = 521
      HelpType = htKeyword
      HelpKeyword = 'Contour_Data_Dialog_Box'
      Caption = 'jvspContourData'
      inline frameContourData: TframeContourData
        Left = 0
        Top = 0
        Width = 572
        Height = 521
        Align = alClient
        TabOrder = 0
        OnResize = frameContourDataResize
        ExplicitWidth = 572
        ExplicitHeight = 521
        inherited pcChoices: TPageControl
          Width = 572
          Height = 521
          ActivePage = frameContourData.tabSelection
          ExplicitWidth = 572
          ExplicitHeight = 521
          inherited tabSelection: TTabSheet
            ExplicitTop = 29
            ExplicitWidth = 564
            ExplicitHeight = 488
            DesignSize = (
              564
              488)
            inherited lblDataSet: TLabel
              Width = 59
              Height = 18
              Caption = 'Data set'
              ExplicitWidth = 59
              ExplicitHeight = 18
            end
            inherited lblColorScheme: TLabel
              Top = 347
              Width = 93
              Height = 18
              ExplicitTop = 347
              ExplicitWidth = 97
              ExplicitHeight = 18
            end
            inherited lblCycles: TLabel
              Left = 414
              Top = 370
              Width = 47
              Height = 18
              ExplicitLeft = 434
              ExplicitTop = 341
              ExplicitWidth = 47
              ExplicitHeight = 18
            end
            inherited pbColorScheme: TPaintBox
              Top = 394
              Width = 400
              ExplicitTop = 371
              ExplicitWidth = 426
            end
            inherited lblColorAdjustment: TLabel
              Top = 433
              Width = 113
              Height = 18
              ExplicitTop = 433
              ExplicitWidth = 117
              ExplicitHeight = 18
            end
            inherited lblComment: TLabel
              Width = 204
              Height = 18
              ExplicitWidth = 204
              ExplicitHeight = 18
            end
            inherited lblAlgorithm: TLabel
              Top = 211
              Width = 52
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitTop = 182
              ExplicitWidth = 52
              ExplicitHeight = 18
            end
            inherited lblContourInterval: TLabel
              Left = 418
              Width = 107
              Height = 18
              Anchors = [akTop, akRight]
              ExplicitLeft = 438
              ExplicitWidth = 107
              ExplicitHeight = 18
            end
            inherited lblSpacing: TLabel
              Left = 321
              Top = 264
              Width = 152
              Height = 18
              Anchors = [akRight, akBottom]
              ExplicitLeft = 341
              ExplicitTop = 244
              ExplicitWidth = 152
              ExplicitHeight = 18
            end
            inherited comboColorScheme: TComboBox
              Top = 364
              Width = 397
              Height = 26
              ExplicitTop = 364
              ExplicitWidth = 397
              ExplicitHeight = 26
            end
            inherited seCycles: TJvSpinEdit
              Left = 414
              Top = 394
              Height = 26
              Anchors = [akRight, akBottom]
              ExplicitLeft = 418
              ExplicitTop = 394
              ExplicitHeight = 26
            end
            inherited jsColorExponent: TJvxSlider
              Left = 2
              Top = 448
            end
            inherited seColorExponent: TJvSpinEdit
              Top = 448
              Height = 26
              ExplicitTop = 448
              ExplicitHeight = 26
            end
            inherited cbLogTransform: TCheckBox
              Top = 452
              Height = 26
              ExplicitTop = 452
              ExplicitHeight = 26
            end
            inherited udDataSets: TJvUpDown
              Left = 286
              ExplicitLeft = 290
            end
            inherited rgUpdateLimitChoice: TRadioGroup
              Top = 268
              Width = 305
              ExplicitTop = 268
              ExplicitWidth = 309
            end
            inherited virttreecomboDataSets: TRbwStringTreeCombo
              Width = 279
              Height = 26
              Tree.DefaultNodeHeight = 20
              Anchors = [akLeft, akTop, akRight]
              OnChange = frameContourDatavirttreecomboDataSetsChange
              ExplicitWidth = 283
              ExplicitHeight = 26
            end
            inherited reComment: TRichEdit
              Width = 533
              Height = 125
              ParentFont = True
              ExplicitWidth = 533
              ExplicitHeight = 125
            end
            inherited btnColorSchemes: TButton
              Left = 321
              Top = 317
              Width = 121
              Height = 40
              ExplicitLeft = 325
              ExplicitTop = 317
              ExplicitWidth = 121
              ExplicitHeight = 40
            end
            inherited btnEditContours: TButton
              Left = 418
              ExplicitLeft = 422
            end
            inherited cbSpecifyContours: TJvCheckBox
              Left = 321
              HotTrackFont.Charset = ANSI_CHARSET
              HotTrackFont.Height = -16
              HotTrackFont.Name = 'Arial'
              HotTrackFont.Pitch = fpVariable
              ExplicitLeft = 325
            end
            inherited cbLabelContours: TCheckBox
              Left = 321
              Top = 211
              Height = 16
              Anchors = [akRight, akBottom]
              ExplicitLeft = 325
              ExplicitTop = 211
              ExplicitHeight = 16
            end
            inherited btnContourFont: TButton
              Left = 321
              Top = 234
              Height = 24
              Anchors = [akRight, akBottom]
              ExplicitLeft = 325
              ExplicitTop = 234
              ExplicitHeight = 24
            end
            inherited comboAlgorithm: TComboBox
              Top = 230
              Height = 26
              Anchors = [akLeft, akBottom]
              ExplicitTop = 230
              ExplicitHeight = 26
            end
            inherited rdeContourInterval: TRbwDataEntry
              Left = 318
              Width = 79
              Anchors = [akTop, akRight]
              ExplicitLeft = 318
              ExplicitWidth = 79
            end
            inherited seLabelSpacing: TJvSpinEdit
              Left = 321
              Top = 284
              Height = 26
              Anchors = [akRight, akBottom]
              ExplicitLeft = 325
              ExplicitTop = 284
              ExplicitHeight = 26
            end
          end
          inherited tabFilters: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 468
            DesignSize = (
              570
              468)
            inherited lblLowerLimit: TLabel
              Width = 75
              Height = 18
              ExplicitWidth = 75
              ExplicitHeight = 18
            end
            inherited lblUpperLimit: TLabel
              Width = 75
              Height = 18
              ExplicitWidth = 75
              ExplicitHeight = 18
            end
            inherited lblValuesToIgnore: TLabel
              Width = 112
              Height = 18
              ExplicitWidth = 112
              ExplicitHeight = 18
            end
            inherited lblNumberOfValuesToIgnore: TLabel
              Top = 437
              Width = 185
              Height = 18
              ExplicitTop = 437
              ExplicitWidth = 185
              ExplicitHeight = 18
            end
            inherited lblEpsilon: TLabel
              Width = 168
              Height = 18
              ExplicitWidth = 168
              ExplicitHeight = 18
            end
            inherited lblModel: TLabel
              Width = 123
              Height = 18
              ExplicitWidth = 123
              ExplicitHeight = 18
            end
            inherited frameCheck3DMax: TframeDisplayLimit
              inherited cbCheck: TCheckBox
                Top = 0
                ExplicitTop = 0
              end
              inherited comboBoolLimit: TComboBox
                Height = 26
                ExplicitHeight = 26
              end
            end
            inherited frameCheck3DMin: TframeDisplayLimit
              inherited cbCheck: TCheckBox
                Top = 0
                ExplicitTop = 0
              end
              inherited comboBoolLimit: TComboBox
                Height = 26
                ExplicitHeight = 26
              end
            end
            inherited cbActiveOnly: TCheckBox
              Width = 177
              ExplicitWidth = 177
            end
            inherited rdgValuesToIgnore: TRbwDataGrid4
              Height = 316
              ExplicitHeight = 316
              ColWidths = (
                64)
              RowHeights = (
                24
                24)
            end
            inherited seNumberOfValuesToIgnore: TJvSpinEdit
              Top = 434
              Height = 26
              ExplicitTop = 434
              ExplicitHeight = 26
            end
            inherited clbxModel: TJvCheckListBox
              ItemHeight = 18
            end
          end
          inherited tabLegend: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 468
            inherited imLegend: TImage
              Height = 468
              ExplicitWidth = 358
              ExplicitHeight = 366
            end
            inherited splColor: TSplitter
              Height = 468
              OnMoved = frameContourDatasplColorMoved
              ExplicitHeight = 468
            end
            inherited pnlLegend: TPanel
              Height = 468
              ExplicitHeight = 468
              DesignSize = (
                201
                468)
              inherited lblMethod: TLabel
                Left = 2
                Top = 7
                Width = 52
                Height = 18
                Anchors = [akLeft, akBottom]
                ExplicitLeft = 2
                ExplicitTop = 7
                ExplicitWidth = 52
                ExplicitHeight = 18
              end
              inherited lblColorLegendRows: TLabel
                Left = 2
                Top = 371
                Width = 109
                Height = 18
                ExplicitLeft = 2
                ExplicitTop = 380
                ExplicitWidth = 109
                ExplicitHeight = 18
              end
              inherited comboMethod: TComboBox
                Left = 2
                Height = 26
                ExplicitLeft = 2
                ExplicitHeight = 26
              end
              inherited seLegendRows: TJvSpinEdit
                Left = 2
                Top = 392
                Height = 26
                ExplicitLeft = 2
                ExplicitTop = 392
                ExplicitHeight = 26
              end
              inherited rdgLegend: TRbwDataGrid4
                Left = 2
                Width = 193
                Height = 306
                ExplicitLeft = 2
                ExplicitWidth = 193
                ExplicitHeight = 306
                ColWidths = (
                  64)
                RowHeights = (
                  24
                  24
                  24
                  24
                  24)
              end
              inherited btnFont: TButton
                Left = 2
                Top = 424
                Anchors = [akLeft, akBottom]
                ExplicitLeft = 2
                ExplicitTop = 424
              end
            end
          end
        end
        inherited dlgFontLegend: TFontDialog
          Left = 280
          Top = 248
        end
      end
    end
    object jvspVectors: TJvStandardPage
      Left = 0
      Top = 0
      Width = 572
      Height = 521
      HelpType = htKeyword
      HelpKeyword = 'Vectors_Pane'
      Caption = 'jvspVectors'
      inline frameVectors: TframeVectors
        Left = 0
        Top = 0
        Width = 572
        Height = 521
        HelpType = htKeyword
        HelpKeyword = 'Vectors_Pane'
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 572
        ExplicitHeight = 521
        inherited lblScale: TLabel
          Width = 130
          Height = 18
          ExplicitWidth = 130
          ExplicitHeight = 18
        end
        inherited lblMaxColor: TLabel
          Width = 105
          Height = 18
          ExplicitWidth = 105
          ExplicitHeight = 18
        end
        inherited lblMidColor: TLabel
          Width = 85
          Height = 18
          ExplicitWidth = 85
          ExplicitHeight = 18
        end
        inherited lblMinColor: TLabel
          Width = 101
          Height = 18
          ExplicitWidth = 101
          ExplicitHeight = 18
        end
        inherited lblVelocityColor: TLabel
          Width = 92
          Height = 18
          ExplicitWidth = 92
          ExplicitHeight = 18
        end
        inherited lblVectorSource: TLabel
          Width = 96
          Height = 18
          ExplicitWidth = 96
          ExplicitHeight = 18
        end
        inherited lblScale3D: TLabel
          Width = 130
          Height = 18
          ExplicitWidth = 130
          ExplicitHeight = 18
        end
        inherited lblMinSpacing2D: TLabel
          Width = 247
          Height = 18
          ExplicitWidth = 247
          ExplicitHeight = 18
        end
        inherited lblMinHorizontalSpacing3D: TLabel
          Width = 217
          Height = 18
          ExplicitWidth = 217
          ExplicitHeight = 18
        end
        inherited lblMinVerticalSpacing3D: TLabel
          Width = 200
          Height = 18
          ExplicitWidth = 200
          ExplicitHeight = 18
        end
        inherited lblLineThickness: TLabel
          Width = 99
          Height = 18
          ExplicitWidth = 99
          ExplicitHeight = 18
        end
        inherited comboVectorSource: TComboBox
          Width = 487
          Height = 26
          Anchors = [akLeft, akTop, akRight]
          ExplicitWidth = 487
          ExplicitHeight = 26
        end
        inherited udVectors: TJvUpDown
          Left = 494
          Width = 16
          Height = 26
          ExplicitLeft = 494
          ExplicitWidth = 16
          ExplicitHeight = 26
        end
        inherited seMinSpacing2D: TJvSpinEdit
          Height = 26
          ExplicitHeight = 26
        end
        inherited seLineThickness: TJvSpinEdit
          Height = 26
          ExplicitHeight = 26
        end
      end
    end
    object jvspStrStreamLinks: TJvStandardPage
      Left = 0
      Top = 0
      Width = 572
      Height = 521
      HelpType = htKeyword
      HelpKeyword = 'Stream_Links_Pane'
      Caption = 'jvspStrStreamLinks'
      inline frameStrStreamLink: TframeStreamLink
        Left = 0
        Top = 0
        Width = 572
        Height = 521
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 572
        ExplicitHeight = 521
        inherited shpStreamColor: TShape
          Top = 4
          ExplicitTop = 4
        end
        inherited shpDiversionColor: TShape
          Top = 35
          ExplicitTop = 35
        end
        inherited lblTimeToPlot: TLabel
          Width = 84
          Height = 18
          ExplicitWidth = 84
          ExplicitHeight = 18
        end
        inherited shpUnconnectedColor: TShape
          Top = 66
          ExplicitTop = 66
        end
        inherited lblSquareSize: TLabel
          Width = 140
          Height = 18
          ExplicitWidth = 140
          ExplicitHeight = 18
        end
        inherited btnStreamColor: TButton
          Top = 4
          ExplicitTop = 4
        end
        inherited btnDiversionColor: TButton
          Top = 35
          ExplicitTop = 35
        end
        inherited rgItemsToPlot: TRadioGroup
          Height = 112
          ExplicitHeight = 112
        end
        inherited cbStreams: TCheckBox
          Width = 150
          ExplicitWidth = 150
        end
        inherited cbPlotDiversions: TCheckBox
          Width = 166
          ExplicitWidth = 166
        end
        inherited comboTimeToPlot: TJvComboBox
          Height = 26
          ExplicitHeight = 26
        end
        inherited cbPlotUnconnected: TCheckBox
          Width = 222
          ExplicitWidth = 222
        end
        inherited btnUnconnectedColor: TButton
          Top = 66
          ExplicitTop = 66
        end
        inherited seSquareSize: TJvSpinEdit
          Height = 26
          ExplicitHeight = 26
        end
      end
    end
    object jvspCrossSection: TJvStandardPage
      Left = 0
      Top = 0
      Width = 572
      Height = 521
      HelpType = htKeyword
      HelpKeyword = 'Cross_Sections_Pane'
      Caption = 'jvspCrossSection'
      inline frameDrawCrossSection: TframeDrawCrossSection
        Left = 0
        Top = 0
        Width = 572
        Height = 521
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 572
        ExplicitHeight = 521
        inherited btnAddDataSet: TSpeedButton
          Left = 273
          ExplicitLeft = 273
        end
        inherited btnRemoveDataSet: TSpeedButton
          Left = 273
          ExplicitLeft = 273
        end
        inherited vstAvailableDataSets: TVirtualStringTree
          Width = 264
          Height = 515
          ExplicitWidth = 264
          ExplicitHeight = 515
        end
        inherited pnlUsed: TPanel
          Left = 296
          Width = 276
          Height = 521
          ExplicitLeft = 296
          ExplicitWidth = 276
          ExplicitHeight = 521
          inherited spl1: TSplitter
            Top = 320
            Width = 276
            ExplicitTop = 318
            ExplicitWidth = 276
          end
          inherited pnlTop: TPanel
            Width = 276
            Height = 320
            ExplicitWidth = 276
            ExplicitHeight = 320
            inherited lblDataSets: TLabel
              Width = 113
              Height = 18
              ExplicitWidth = 113
              ExplicitHeight = 18
            end
            inherited lstSelectedDataSets: TListBox
              Width = 270
              Height = 287
              ExplicitWidth = 270
              ExplicitHeight = 287
            end
          end
          inherited pnlBottom: TPanel
            Top = 325
            Width = 276
            Height = 196
            ExplicitTop = 325
            ExplicitWidth = 276
            ExplicitHeight = 196
            inherited lblLayers: TLabel
              Width = 93
              Height = 18
              ExplicitWidth = 93
              ExplicitHeight = 18
            end
            inherited lblLineThickness: TLabel
              Top = 173
              Width = 99
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitTop = 173
              ExplicitWidth = 99
              ExplicitHeight = 18
            end
            inherited clbLayers: TJvCheckListBox
              Width = 270
              Height = 136
              ItemHeight = 18
              ExplicitWidth = 270
              ExplicitHeight = 136
            end
            inherited seLineThickness: TJvSpinEdit
              Top = 170
              Height = 26
              Anchors = [akLeft, akBottom]
              ExplicitTop = 170
              ExplicitHeight = 26
            end
          end
        end
      end
    end
    object jvspSwrReachConnections: TJvStandardPage
      Left = 0
      Top = 0
      Width = 572
      Height = 521
      HelpType = htKeyword
      HelpKeyword = 'SWR_Reach_Connections'
      Caption = 'jvspSwrReachConnections'
      inline frameSwrReachConnections: TframeSwrReachConnections
        Left = 0
        Top = 0
        Width = 572
        Height = 521
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 572
        ExplicitHeight = 521
        inherited shpReachColor: TShape
          Left = 453
          Top = 4
          ExplicitLeft = 453
          ExplicitTop = 4
        end
        inherited shpUnconnectedColor: TShape
          Left = 453
          Top = 34
          ExplicitLeft = 453
          ExplicitTop = 34
        end
        inherited shpStructureColor: TShape
          Left = 453
          Top = 64
          ExplicitLeft = 453
          ExplicitTop = 64
        end
        inherited cbReaches: TCheckBox
          Width = 150
          OnClick = nil
          ExplicitWidth = 150
        end
        inherited btnReachColor: TButton
          Left = 247
          Top = 4
          OnClick = frameSwrReachConnectionsbtnReachColorClick
          ExplicitLeft = 247
          ExplicitTop = 4
        end
        inherited cbPlotUnconnected: TCheckBox
          Width = 206
          OnClick = nil
          ExplicitWidth = 206
        end
        inherited btnUnconnectedColor: TButton
          Left = 247
          Top = 34
          OnClick = frameSwrReachConnectionsbtnUnconnectedColorClick
          ExplicitLeft = 247
          ExplicitTop = 34
        end
        inherited rgItemsToPlot: TRadioGroup
          Height = 112
          ExplicitHeight = 112
        end
        inherited cbPlotStructures: TCheckBox
          Top = 67
          ExplicitTop = 67
        end
        inherited btnStructureColor: TButton
          Left = 247
          Top = 64
          ExplicitLeft = 247
          ExplicitTop = 64
        end
      end
    end
    object jvspSwrObsDisplay: TJvStandardPage
      Left = 0
      Top = 0
      Width = 572
      Height = 521
      HelpType = htKeyword
      HelpKeyword = 'SWR_Observations'
      Caption = 'jvspSwrObsDisplay'
      inline frameSwrObsDisplay: TframeSwrObsDisplay
        Left = 0
        Top = 0
        Width = 572
        Height = 521
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 572
        ExplicitHeight = 521
        inherited pbPlot: TPaintBox
          Width = 446
          Height = 392
          ExplicitWidth = 452
          ExplicitHeight = 436
        end
        inherited spl1: TSplitter
          Height = 392
          ExplicitHeight = 436
        end
        inherited pnlTop: TPanel
          Width = 572
          ExplicitWidth = 572
          inherited lblAnimationInterval: TLabel
            Width = 162
            Height = 18
            ExplicitWidth = 162
            ExplicitHeight = 18
          end
          inherited lblObservationFile: TLabel
            Width = 146
            Height = 18
            ExplicitWidth = 146
            ExplicitHeight = 18
          end
          inherited fedObservationFile: TJvFilenameEdit
            Width = 529
            Height = 26
            ExplicitWidth = 533
            ExplicitHeight = 26
          end
          inherited comboObservationType: TComboBox
            Height = 26
            ExplicitHeight = 26
          end
          inherited seIncrement: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited rdgTimes: TRbwDataGrid4
          Height = 392
          ExplicitHeight = 392
          ColWidths = (
            50)
          RowHeights = (
            24
            24
            24
            24
            24)
        end
      end
    end
    object jvspPestObsResults: TJvStandardPage
      Left = 0
      Top = 0
      Width = 572
      Height = 521
      HelpType = htKeyword
      HelpKeyword = 'PEST_Observation_Results_Pane'
      Caption = 'jvspPestObsResults'
      inline framePestObs: TframePestObservationResults
        Left = 0
        Top = 0
        Width = 572
        Height = 521
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 572
        inherited pnlBottom: TPanel
          Width = 572
          ExplicitTop = 454
          ExplicitWidth = 568
          inherited lblRMS: TLabel
            Left = 6
            Width = 293
            Height = 18
            ExplicitLeft = 6
            ExplicitWidth = 293
            ExplicitHeight = 18
          end
          inherited comboModels: TComboBox
            Left = 6
            Height = 26
            ExplicitLeft = 6
            ExplicitHeight = 26
          end
        end
        inherited pgcObservations: TPageControl
          Width = 572
          ActivePage = framePestObs.tabValues
          ExplicitWidth = 568
          ExplicitHeight = 454
          inherited tabControls: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 422
            inherited lblNegativeColor: TLabel
              Width = 190
              Height = 36
              ExplicitWidth = 190
              ExplicitHeight = 36
            end
            inherited lblColorPositive: TLabel
              Width = 185
              Height = 36
              ExplicitWidth = 185
              ExplicitHeight = 36
            end
            inherited lblMaxSymbolSize: TLabel
              Width = 206
              Height = 18
              ExplicitWidth = 206
              ExplicitHeight = 18
            end
            inherited lblHeadObsResults: TLabel
              Width = 69
              Height = 18
              ExplicitWidth = 69
              ExplicitHeight = 18
            end
            inherited fedHeadObsResults: TJvFilenameEdit
              Left = 2
              Top = 27
              Height = 26
              OnChange = framePestObsflnmedHeadObsResultsChange
              ExplicitLeft = 2
              ExplicitTop = 27
              ExplicitHeight = 26
            end
            inherited grpbxFilter: TGroupBox
              inherited lblMaximumTime: TLabel
                Width = 101
                Height = 18
                ExplicitWidth = 101
                ExplicitHeight = 18
              end
              inherited lblMaxResidual: TLabel
                Width = 126
                Height = 18
                ExplicitWidth = 126
                ExplicitHeight = 18
              end
              inherited lblMinimumTime: TLabel
                Width = 97
                Height = 18
                ExplicitWidth = 97
                ExplicitHeight = 18
              end
              inherited lblMinResidual: TLabel
                Width = 122
                Height = 18
                ExplicitWidth = 122
                ExplicitHeight = 18
              end
              inherited lblMinWeightedResidual: TLabel
                Width = 189
                Height = 18
                ExplicitWidth = 189
                ExplicitHeight = 18
              end
              inherited lblMaxWeightedResidual: TLabel
                Width = 193
                Height = 18
                ExplicitWidth = 193
                ExplicitHeight = 18
              end
              inherited framelmtMinimumTime: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited framelmtMaxResidual: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited framelmtMaximumTime: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited framelmtMinResidual: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited framelmtMinWeightedResidual: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited framelmtMaxWeightedResidual: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
            end
            inherited spinSymbolSize: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
          end
          inherited tabValues: TTabSheet
            ExplicitTop = 29
            ExplicitWidth = 564
            ExplicitHeight = 422
            inherited rdgPestObs: TRbwDataGrid4
              Width = 564
              Height = 357
              ExplicitWidth = 564
              ExplicitHeight = 357
            end
            inherited pnlValueControls: TPanel
              Top = 357
              Width = 564
              ExplicitTop = 356
              ExplicitWidth = 560
            end
          end
          inherited tabLegend: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 422
            inherited lblMax: TLabel
              Width = 44
              Height = 18
              ExplicitWidth = 44
              ExplicitHeight = 18
            end
            inherited lblHalfMax: TLabel
              Width = 71
              Height = 18
              ExplicitWidth = 71
              ExplicitHeight = 18
            end
          end
          inherited tabGraph: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 422
            inherited pbObservations: TPaintBox
              Height = 324
              ExplicitWidth = 547
              ExplicitHeight = 324
            end
            inherited pnlGraphControls: TPanel
              Top = 324
              ExplicitTop = 324
              inherited lblGraphInstructions: TLabel
                Width = 201
                Height = 18
                ExplicitWidth = 201
                ExplicitHeight = 18
              end
              inherited lblWhatToPlot: TLabel
                Width = 82
                Height = 18
                ExplicitWidth = 82
                ExplicitHeight = 18
              end
              inherited clbWhatToPlot: TCheckListBox
                ItemHeight = 18
              end
            end
          end
        end
      end
    end
    object jvspMt3dObs: TJvStandardPage
      Left = 0
      Top = 0
      Width = 572
      Height = 521
      HelpType = htKeyword
      HelpKeyword = 'MT3D-Observations-Results-Pane'
      Caption = 'jvspMt3dObs'
      inline frameMt3dObsResults: TframeMt3dObsResults
        Left = 0
        Top = 0
        Width = 572
        Height = 521
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 572
        inherited pgcObservations: TPageControl
          Width = 572
          ExplicitWidth = 572
          inherited tabControls: TTabSheet
            ExplicitTop = 29
            ExplicitWidth = 564
            ExplicitHeight = 422
            inherited lblNegativeColor: TLabel
              Width = 190
              Height = 36
              ExplicitWidth = 190
              ExplicitHeight = 36
            end
            inherited lblColorPositive: TLabel
              Width = 185
              Height = 36
              ExplicitWidth = 185
              ExplicitHeight = 36
            end
            inherited lblMaxSymbolSize: TLabel
              Width = 206
              Height = 18
              ExplicitWidth = 206
              ExplicitHeight = 18
            end
            inherited lblHeadObsResults: TLabel
              Width = 69
              Height = 18
              ExplicitWidth = 69
              ExplicitHeight = 18
            end
            inherited fedHeadObsResults: TJvFilenameEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited grpbxFilter: TGroupBox
              inherited lblMaximumTime: TLabel
                Width = 101
                Height = 18
                ExplicitWidth = 101
                ExplicitHeight = 18
              end
              inherited lblMaxResidual: TLabel
                Width = 126
                Height = 18
                ExplicitWidth = 126
                ExplicitHeight = 18
              end
              inherited lblMinimumTime: TLabel
                Width = 97
                Height = 18
                ExplicitWidth = 97
                ExplicitHeight = 18
              end
              inherited lblMinResidual: TLabel
                Width = 122
                Height = 18
                ExplicitWidth = 122
                ExplicitHeight = 18
              end
              inherited lblMinWeightedResidual: TLabel
                Width = 189
                Height = 18
                ExplicitWidth = 189
                ExplicitHeight = 18
              end
              inherited lblMaxWeightedResidual: TLabel
                Width = 193
                Height = 18
                ExplicitWidth = 193
                ExplicitHeight = 18
              end
              inherited framelmtMinimumTime: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited framelmtMaxResidual: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited framelmtMaximumTime: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited framelmtMinResidual: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited framelmtMinWeightedResidual: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited framelmtMaxWeightedResidual: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
            end
            inherited seSymbolSize: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
          end
          inherited tabValues: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 422
            inherited rdgMt3dObs: TRbwDataGrid4
              Height = 357
              ExplicitHeight = 357
            end
            inherited pnlValueControls: TPanel
              Top = 357
              ExplicitTop = 357
              ExplicitWidth = 564
            end
          end
          inherited tabLegend: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 422
            inherited lblMax: TLabel
              Width = 44
              Height = 18
              ExplicitWidth = 44
              ExplicitHeight = 18
            end
            inherited lblHalfMax: TLabel
              Width = 71
              Height = 18
              ExplicitWidth = 71
              ExplicitHeight = 18
            end
          end
          inherited tabGraph: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 422
            inherited pbObservations: TPaintBox
              Height = 324
              ExplicitHeight = 324
            end
            inherited pnlGraphControls: TPanel
              Top = 324
              ExplicitTop = 324
              inherited lblGraphInstructions: TLabel
                Width = 201
                Height = 18
                ExplicitWidth = 201
                ExplicitHeight = 18
              end
            end
          end
        end
        inherited pnlBottom: TPanel
          Width = 572
          ExplicitWidth = 572
          inherited lblRMS: TLabel
            Width = 293
            Height = 18
            ExplicitWidth = 293
            ExplicitHeight = 18
          end
          inherited comboModels: TComboBox
            Height = 26
            ExplicitHeight = 26
          end
        end
      end
    end
  end
  object tvpglstMain: TJvPageListTreeView
    Left = 0
    Top = 0
    Width = 201
    Height = 521
    PageDefault = 0
    PageList = pglstMain
    Align = alLeft
    HideSelection = False
    Indent = 20
    TabOrder = 0
    OnChanging = tvpglstMainChanging
    OnCustomDrawItem = tvpglstMainCustomDrawItem
    Items.NodeData = {
      0307000000320000000000000000000000FFFFFFFFFFFFFFFF00000000050000
      0000000000010A43006F006C006F007200200047007200690064003600000000
      00000000000000FFFFFFFFFFFFFFFF000000000600000000000000010C43006F
      006E0074006F0075007200200044006100740061004000000000000000000000
      00FFFFFFFFFFFFFFFF00000000000000000000000001114D004F004400500041
      0054004800200050006100740068006C0069006E006500730042000000000000
      0000000000FFFFFFFFFFFFFFFF00000000040000000000000001124D004F0044
      005000410054004800200045006E006400200050006F0069006E007400730044
      0000000000000000000000FFFFFFFFFFFFFFFF00000000030000000000000001
      134D004F00440050004100540048002000540069006D00650020005300650072
      006900650073004E0000000000000000000000FFFFFFFFFFFFFFFF0000000002
      000000000000000118480065006100640020004F006200730065007200760061
      00740069006F006E00200052006500730075006C007400730036000000000000
      0000000000FFFFFFFFFFFFFFFF000000000100000000000000010C5300740072
      00650061006D0020004C0069006E006B007300}
    Items.Links = {
      0700000005000000060000000000000004000000030000000200000001000000}
    ExplicitHeight = 520
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 521
    Width = 778
    Height = 41
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 520
    ExplicitWidth = 774
    DesignSize = (
      778
      41)
    object btnHelp: TBitBtn
      Left = 432
      Top = 6
      Width = 101
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 428
    end
    object btnApply: TBitBtn
      Left = 539
      Top = 6
      Width = 101
      Height = 33
      Anchors = [akTop, akRight]
      Caption = 'Apply'
      Default = True
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
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
      TabOrder = 1
      OnClick = btnApplyClick
      ExplicitLeft = 535
    end
    object btnClose: TBitBtn
      Left = 646
      Top = 6
      Width = 101
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 642
    end
  end
end
