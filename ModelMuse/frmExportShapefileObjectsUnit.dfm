inherited frmExportShapefileObjects: TfrmExportShapefileObjects
  HelpType = htKeyword
  HelpKeyword = 'Export_Objects_to_Shapefile'
  Caption = 'Export Objects as Shapefile'
  ClientHeight = 422
  ClientWidth = 617
  OnResize = FormResize
  ExplicitWidth = 633
  ExplicitHeight = 461
  PixelsPerInch = 96
  TextHeight = 18
  object splLeft: TSplitter [0]
    Left = 257
    Top = 41
    Height = 215
    OnMoved = splLeftMoved
    ExplicitLeft = 304
    ExplicitTop = 128
    ExplicitHeight = 100
  end
  object splRight: TSplitter [1]
    Left = 493
    Top = 41
    Height = 215
    Align = alRight
    OnMoved = splRightMoved
    ExplicitLeft = 496
    ExplicitTop = 208
    ExplicitHeight = 100
  end
  inherited pnlBottom: TPanel
    Top = 256
    Width = 617
    Height = 166
    TabOrder = 4
    ExplicitTop = 256
    ExplicitWidth = 617
    ExplicitHeight = 166
    object lblMissingData: TLabel [0]
      Left = 120
      Top = 62
      Width = 129
      Height = 18
      Caption = 'Missing data value'
    end
    inherited btnClose: TBitBtn
      Left = 421
      Top = 126
      Caption = '&OK'
      Kind = bkOK
      TabOrder = 6
      OnClick = btnCloseClick
      ExplicitLeft = 421
      ExplicitTop = 126
    end
    inherited btnHelp: TBitBtn
      Left = 326
      Top = 126
      TabOrder = 5
      OnClick = btnHelpClick
      ExplicitLeft = 326
      ExplicitTop = 126
    end
    object BitBtn1: TBitBtn
      Left = 516
      Top = 126
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 7
    end
    object rdeMissingData: TRbwDataEntry
      Left = 8
      Top = 59
      Width = 105
      Height = 27
      TabOrder = 3
      Text = '-99900000'
      DataType = dtInteger
      Max = 1.000000000000000000
      ChangeDisabledColor = True
    end
    object gbExportAs: TGroupBox
      Left = 8
      Top = 88
      Width = 249
      Height = 73
      Caption = 'Export as'
      TabOrder = 4
      object rbPoints: TRadioButton
        Left = 8
        Top = 24
        Width = 113
        Height = 17
        Margins.Left = 8
        Caption = 'Points'
        TabOrder = 0
      end
      object rbMultipoint: TRadioButton
        Left = 8
        Top = 47
        Width = 113
        Height = 17
        Margins.Left = 8
        Caption = 'Multipoints'
        Checked = True
        TabOrder = 2
        TabStop = True
      end
      object rbPolyline: TRadioButton
        Left = 132
        Top = 24
        Width = 113
        Height = 17
        Margins.Left = 8
        Caption = 'Polylines'
        TabOrder = 1
      end
      object rbPolygons: TRadioButton
        Left = 132
        Top = 47
        Width = 113
        Height = 17
        Margins.Left = 8
        Caption = 'Polygons'
        TabOrder = 3
      end
    end
    object cbExportName: TCheckBox
      Left = 8
      Top = 6
      Width = 177
      Height = 18
      Caption = 'Export object name'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object cbExportElevationFormulas: TCheckBox
      Left = 8
      Top = 30
      Width = 241
      Height = 18
      Caption = 'Export elevation formulas'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object rgView: TRadioGroup
      Left = 326
      Top = 6
      Width = 203
      Height = 89
      Caption = 'Export objects on'
      ItemIndex = 0
      Items.Strings = (
        'Top view'
        'Front view'
        'Side view')
      TabOrder = 1
      OnClick = rgViewClick
    end
  end
  inherited vstObjects: TVirtualStringTree
    Left = 260
    Top = 41
    Width = 233
    Height = 215
    TabOrder = 2
    OnChecked = vstObjectsChecked
    ExplicitLeft = 260
    ExplicitTop = 41
    ExplicitWidth = 233
    ExplicitHeight = 215
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 617
    Height = 41
    Align = alTop
    TabOrder = 0
    object lblObjects: TLabel
      Left = 314
      Top = 11
      Width = 117
      Height = 18
      Caption = 'Objects to export'
    end
    object lblDataArrays: TLabel
      Left = 70
      Top = 12
      Width = 131
      Height = 18
      Caption = 'Data sets to export'
    end
    object lblTimes: TLabel
      Left = 536
      Top = 11
      Width = 42
      Height = 18
      Caption = 'Times'
    end
  end
  object vstDataSets: TVirtualStringTree
    Left = 0
    Top = 41
    Width = 257
    Height = 215
    Align = alLeft
    Header.AutoSizeIndex = 0
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    TabOrder = 1
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toWheelPanning]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnChecked = vstDataSetsChecked
    OnChecking = vstDataSetsChecking
    OnGetText = vstDataSetsGetText
    Columns = <>
  end
  object chklstTimes: TCheckListBox
    Left = 496
    Top = 41
    Width = 121
    Height = 215
    Align = alRight
    Enabled = False
    ItemHeight = 18
    TabOrder = 3
  end
  object sdShapefile: TSaveDialog
    DefaultExt = '.shp'
    Filter = 'Shapefiles (*.shp)|*.shp'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 192
    Top = 96
  end
  object XBaseShapeFile: TXBase
    Active = False
    AutoUpDate = True
    DebugErr = False
    Deleted = False
    Left = 232
    Top = 96
  end
end
