object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Stream Extractor'
  ClientHeight = 612
  ClientWidth = 756
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mm1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 19
  object pb1: TProgressBar
    Left = 0
    Top = 596
    Width = 756
    Height = 16
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alBottom
    Max = 1000
    TabOrder = 1
  end
  object pgcMethods: TPageControl
    Left = 0
    Top = 0
    Width = 756
    Height = 596
    ActivePage = tsExperimental
    Align = alClient
    TabOrder = 0
    object tabExtractStreams: TTabSheet
      Caption = 'Extract Streams'
      object lblStatus: TLabel
        Left = 424
        Top = 274
        Width = 43
        Height = 19
        Margins.Bottom = 2
        Caption = 'Status'
      end
      object lblRasterExtractionMethod: TLabel
        Left = 272
        Top = 274
        Width = 124
        Height = 38
        Caption = 'Raster Extraction Method'
        WordWrap = True
      end
      object lblStreamMethod: TLabel
        Left = 270
        Top = 357
        Width = 130
        Height = 38
        Caption = 'Stream Extraction Method '
        WordWrap = True
      end
      object grpInput: TGroupBox
        Left = 1
        Top = 0
        Width = 266
        Height = 560
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Input'
        TabOrder = 0
        object lblInput: TLabel
          Left = 7
          Top = 15
          Width = 150
          Height = 19
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Input Surfer Grid File'
        end
        object lblClipShapeFile: TLabel
          Left = 7
          Top = 84
          Width = 242
          Height = 19
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Included Area Shapefile (optional)'
        end
        object lblStartingPoints: TLabel
          Left = 7
          Top = 140
          Width = 237
          Height = 19
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Outflow Area Shapefile (optional)'
        end
        object lblMinimumPixels: TLabel
          Left = 7
          Top = 198
          Width = 235
          Height = 38
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Minimum Number of Grid Points Contributing to Stream'
          WordWrap = True
        end
        object lblExcludeAreas: TLabel
          Left = 7
          Top = 314
          Width = 242
          Height = 19
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Exclude Area Shapefiles (optional)'
        end
        object lblMaximumCarvingLength: TLabel
          Left = 8
          Top = 447
          Width = 181
          Height = 19
          Caption = 'Maximum Carving Length'
        end
        object lblMaximumCarvingDepth: TLabel
          Left = 3
          Top = 500
          Width = 175
          Height = 19
          Caption = 'Maximum Carving Depth'
        end
        object fedInput: TJvFilenameEdit
          Left = 3
          Top = 39
          Width = 260
          Height = 27
          Filter = 'Surfer Grid files (*.grd)|*.grd'
          ButtonWidth = 28
          TabOrder = 0
          Text = ''
          OnChange = fedInputChange
        end
        object fedClipShapeFile: TJvFilenameEdit
          Left = 7
          Top = 108
          Width = 256
          Height = 27
          Filter = 'Surfer Grid files (*.shp)|*.shp'
          ButtonWidth = 28
          TabOrder = 1
          Text = ''
        end
        object fedStartingPoints: TJvFilenameEdit
          Left = 7
          Top = 164
          Width = 256
          Height = 27
          Filter = 'Surfer Grid files (*.shp)|*.shp'
          ButtonWidth = 28
          TabOrder = 2
          Text = ''
        end
        object seMinimumPixels: TJvSpinEdit
          Left = 7
          Top = 240
          Width = 91
          Height = 27
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          MaxValue = 2147483647.000000000000000000
          Value = 500.000000000000000000
          TabOrder = 3
        end
        object memoExclude: TMemo
          Left = 2
          Top = 337
          Width = 262
          Height = 100
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          ScrollBars = ssBoth
          TabOrder = 5
          WordWrap = False
        end
        object btnSelectExcludeAreas: TButton
          Left = 7
          Top = 271
          Width = 234
          Height = 31
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Select Exclude Area Shapefiles'
          TabOrder = 4
          OnClick = btnSelectExcludeAreasClick
        end
        object rdeMaximumCarvingLength: TRbwDataEntry
          Left = 61
          Top = 472
          Width = 180
          Height = 22
          TabOrder = 6
          Text = '500'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object cbMaximumCarvingLength: TCheckBox
          Left = 3
          Top = 476
          Width = 52
          Height = 17
          Caption = 'Limit'
          Checked = True
          State = cbChecked
          TabOrder = 7
          OnClick = cbMaximumCarvingLengthClick
        end
        object cbMaximumCarvingDepth: TCheckBox
          Left = 8
          Top = 532
          Width = 52
          Height = 17
          Caption = 'Limit'
          Checked = True
          State = cbChecked
          TabOrder = 9
          OnClick = cbMaximumCarvingDepthClick
        end
        object rdeMaximumCarvingDepth: TRbwDataEntry
          Left = 66
          Top = 528
          Width = 180
          Height = 22
          TabOrder = 8
          Text = '2'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
      end
      object grpOutput: TGroupBox
        Left = 402
        Top = 144
        Width = 336
        Height = 125
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Output'
        TabOrder = 2
        object lblOutput: TLabel
          Left = 9
          Top = 18
          Width = 262
          Height = 19
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Watershed ID Output Surfer Grid File'
        end
        object lblStreamsOutput: TLabel
          Left = 10
          Top = 69
          Width = 126
          Height = 19
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Streams Shapefile'
        end
        object fedOutput: TJvFilenameEdit
          Left = 10
          Top = 42
          Width = 312
          Height = 27
          DialogKind = dkSave
          DefaultExt = '.grd'
          Filter = 'Surfer Grid files (*.grd)|*.grd'
          ButtonWidth = 28
          TabOrder = 0
          Text = ''
        end
        object fedStreamsOutput: TJvFilenameEdit
          Left = 11
          Top = 93
          Width = 311
          Height = 27
          DialogKind = dkSave
          DefaultExt = '.shp'
          Filter = 'ShapeFile (*.shp)|*.shp'
          ButtonWidth = 28
          TabOrder = 1
          Text = ''
        end
      end
      object memo2: TMemo
        Left = 307
        Top = 434
        Width = 431
        Height = 69
        ScrollBars = ssBoth
        TabOrder = 8
        WordWrap = False
      end
      object grpIntermediate: TGroupBox
        Left = 402
        Top = 2
        Width = 337
        Height = 137
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Intermediate'
        TabOrder = 1
        object lblFlowDirections: TLabel
          Left = 2
          Top = 76
          Width = 172
          Height = 19
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Flow Directions Grid File'
        end
        object lblPitlessRaster: TLabel
          Left = 5
          Top = 20
          Width = 156
          Height = 19
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Pitless Surfer Grid File'
        end
        object fedFlowDirections: TJvFilenameEdit
          Left = 6
          Top = 100
          Width = 310
          Height = 27
          DialogKind = dkSave
          DefaultExt = '.grd'
          Filter = 'Surfer Grid files (*.grd)|*.grd'
          ButtonWidth = 28
          TabOrder = 1
          Text = ''
        end
        object fedPitlessRaster: TJvFilenameEdit
          Left = 6
          Top = 44
          Width = 310
          Height = 27
          DialogKind = dkSave
          DefaultExt = '.grd'
          Filter = 'Surfer Grid files (*.grd)|*.grd'
          ButtonWidth = 28
          TabOrder = 0
          Text = ''
        end
      end
      object comboRasterMethod: TComboBox
        Left = 272
        Top = 319
        Width = 105
        Height = 27
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 4
        Text = 'File'
        Items.Strings = (
          'File'
          'Memory')
      end
      object btnExtractRaster: TButton
        Left = 424
        Top = 305
        Width = 249
        Height = 40
        Caption = 'Extract Pitless Raster from Input Raster'
        TabOrder = 3
        WordWrap = True
        OnClick = btnExtractRasterClick
      end
      object comboStreamMethod: TComboBox
        Left = 270
        Top = 401
        Width = 105
        Height = 27
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 6
        Text = 'File'
        Items.Strings = (
          'File'
          'Memory')
      end
      object btnExtractIntermediate: TButton
        Left = 424
        Top = 351
        Width = 249
        Height = 53
        Caption = 'Extract Streams from Pitless Raster'
        TabOrder = 5
        WordWrap = True
        OnClick = btnExtractIntermediateClick
      end
      object btnExtractStreams: TButton
        Left = 424
        Top = 403
        Width = 249
        Height = 25
        Caption = 'Extract Streams from Input Raster'
        TabOrder = 7
        OnClick = btnExtractStreamsClick
      end
    end
    object tabAssignElevationsToShapes: TTabSheet
      Caption = 'Assign Elevations to Shapes'
      ImageIndex = 1
      object grpAssignElevationsInput: TGroupBox
        Left = 3
        Top = 3
        Width = 278
        Height = 438
        Caption = 'Input'
        TabOrder = 0
        object lblStreamToModify: TLabel
          Left = 14
          Top = 15
          Width = 162
          Height = 19
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Input Stream Shapefile'
        end
        object lblSurferElevations: TLabel
          Left = 14
          Top = 97
          Width = 209
          Height = 19
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Surfer Grid Files of Elevations'
        end
        object lblSurferMeasuredValues: TLabel
          Left = 14
          Top = 272
          Width = 210
          Height = 38
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Surfer Grid Files of Measured Values (optional)'
          WordWrap = True
        end
        object fedStreamToModify: TJvFilenameEdit
          Left = 14
          Top = 39
          Width = 196
          Height = 27
          Filter = 'Surfer Grid files (*.shp)|*.shp'
          ButtonWidth = 28
          TabOrder = 0
          Text = ''
        end
        object mmoSurferMeasuredValues: TMemo
          Left = 14
          Top = 317
          Width = 243
          Height = 90
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          ScrollBars = ssBoth
          TabOrder = 4
          WordWrap = False
        end
        object mmoSurferElevations: TMemo
          Left = 14
          Top = 120
          Width = 243
          Height = 100
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          ScrollBars = ssBoth
          TabOrder = 2
          WordWrap = False
        end
        object btn1: TButton
          Left = 14
          Top = 224
          Width = 249
          Height = 28
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Select Measure Value Grid Files'
          TabOrder = 3
          OnClick = btn1Click
        end
        object btn2: TButton
          Left = 14
          Top = 71
          Width = 243
          Height = 22
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Select Elevation Grid Files'
          TabOrder = 1
          OnClick = btn2Click
        end
      end
      object grpAssignElevationsOutput: TGroupBox
        Left = 296
        Top = 14
        Width = 246
        Height = 89
        Caption = 'Output'
        TabOrder = 1
        object lblModfiedStreams: TLabel
          Left = 11
          Top = 20
          Width = 173
          Height = 19
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Output Stream Shapefile'
        end
        object fedModfiedStreams: TJvFilenameEdit
          Left = 11
          Top = 44
          Width = 232
          Height = 27
          DefaultExt = '.shp'
          Filter = 'Surfer Grid files (*.shp)|*.shp'
          ButtonWidth = 28
          TabOrder = 0
          Text = ''
        end
      end
      object btnModifyStreams: TButton
        Left = 296
        Top = 124
        Width = 214
        Height = 25
        Caption = 'Assign Streams Elevations'
        TabOrder = 2
        OnClick = btnModifyStreamsClick
      end
    end
    object tabFilter: TTabSheet
      Caption = 'Filter'
      ImageIndex = 2
      object lblUnfilteredStreams: TLabel
        Left = 3
        Top = 12
        Width = 184
        Height = 19
        Margins.Bottom = 2
        Caption = 'Unfiltered Streams (input)'
      end
      object lblDomainBoundary: TLabel
        Left = 3
        Top = 70
        Width = 180
        Height = 19
        Margins.Bottom = 2
        Caption = 'Domain Boundary (input)'
      end
      object lblFilteredStreams: TLabel
        Left = 3
        Top = 127
        Width = 177
        Height = 19
        Margins.Bottom = 2
        Caption = 'Filtered Streams (output)'
      end
      object fedUnfilteredStreams: TJvFilenameEdit
        Left = 3
        Top = 36
        Width = 311
        Height = 27
        DialogKind = dkSave
        DefaultExt = '.shp'
        Filter = 'ShapeFile (*.shp)|*.shp'
        ButtonWidth = 28
        TabOrder = 0
        Text = ''
      end
      object fedDomainBoundary: TJvFilenameEdit
        Left = 3
        Top = 95
        Width = 311
        Height = 27
        DialogKind = dkSave
        DefaultExt = '.shp'
        Filter = 'ShapeFile (*.shp)|*.shp'
        ButtonWidth = 28
        TabOrder = 1
        Text = ''
      end
      object fedFilteredStreams: TJvFilenameEdit
        Left = 3
        Top = 151
        Width = 311
        Height = 27
        DialogKind = dkSave
        DefaultExt = '.shp'
        Filter = 'ShapeFile (*.shp)|*.shp'
        ButtonWidth = 28
        TabOrder = 2
        Text = ''
      end
      object btnFilter: TButton
        Left = 3
        Top = 184
        Width = 290
        Height = 49
        Caption = 'Filter out streams completely outside the domain boundary'
        TabOrder = 3
        WordWrap = True
        OnClick = btnFilterClick
      end
    end
    object tsExperimental: TTabSheet
      Caption = 'tsExperimental'
      ImageIndex = 3
      object btnCoarsenDEM: TButton
        Left = 32
        Top = 24
        Width = 137
        Height = 25
        Caption = 'Coarsen DEM'
        TabOrder = 0
        OnClick = btnCoarsenDEMClick
      end
      object btnSplitDEM: TButton
        Left = 32
        Top = 88
        Width = 137
        Height = 25
        Caption = 'Split DEM'
        TabOrder = 1
        OnClick = btnSplitDEMClick
      end
      object Button1: TButton
        Left = 32
        Top = 57
        Width = 153
        Height = 25
        Caption = 'Make Random DEM'
        TabOrder = 2
        OnClick = Button1Click
      end
      object btnMergePitlessDEM: TButton
        Left = 32
        Top = 119
        Width = 153
        Height = 25
        Caption = 'Merge Pitless DEMs'
        TabOrder = 3
        OnClick = btnMergePitlessDEMClick
      end
    end
  end
  object dlgOpenSelectExcludeAreas: TOpenDialog
    Filter = 'Shapefiles (*.shp)|*.shp'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 440
    Top = 280
  end
  object dlgOpenGridFiles: TOpenDialog
    DefaultExt = '.grd'
    Filter = 'Surver 7 Binary Grid files (*.grd)|*.grd'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 392
    Top = 280
  end
  object mm1: TMainMenu
    Left = 320
    Top = 48
    object miFile1: TMenuItem
      Caption = 'File'
      object miOpen1: TMenuItem
        Caption = 'Open'
        OnClick = miOpen1Click
      end
      object miSave1: TMenuItem
        Caption = 'Save'
        OnClick = miSave1Click
      end
      object miClose1: TMenuItem
        Caption = 'Close'
        OnClick = miClose1Click
      end
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = '.se'
    Filter = 'Stream Extractor Files|*.se'
    Left = 588
    Top = 310
  end
  object dlgSave: TSaveDialog
    DefaultExt = '.se'
    Filter = 'Stream Extractor Files|*.se'
    Left = 636
    Top = 310
  end
  object odCoarsenDEM: TOpenDialog
    Filter = 'GRD files (*.grd)|*.grd'
    Left = 204
    Top = 118
  end
  object dlgSaveRandomDem: TSaveDialog
    Filter = 'Grd files (*.grd)|*.grd'
    Left = 308
    Top = 142
  end
  object odMergePitlessDEM: TOpenDialog
    Filter = 'GRD files (*.grd)|*.grd'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 212
    Top = 182
  end
end
