inherited frmLinkRaster: TfrmLinkRaster
  Caption = 'Links to Rasters'
  ClientWidth = 583
  ExplicitWidth = 595
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 173
    Width = 583
    Height = 50
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    ExplicitTop = 172
    ExplicitWidth = 579
    DesignSize = (
      583
      50)
    object btnCancel: TBitBtn
      Left = 465
      Top = 7
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 461
    end
    object btnOK: TBitBtn
      Left = 368
      Top = 6
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnOKClick
      ExplicitLeft = 364
    end
    object btnHelp: TBitBtn
      Left = 271
      Top = 7
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 1
      ExplicitLeft = 267
    end
  end
  inline frameRasters: TframeGrid
    Left = 0
    Top = 0
    Width = 583
    Height = 173
    Align = alClient
    AutoScroll = True
    TabOrder = 0
    ExplicitWidth = 579
    ExplicitHeight = 172
    inherited Panel: TPanel
      Top = 132
      Width = 583
      ExplicitTop = 131
      ExplicitWidth = 579
      inherited lbNumber: TLabel
        Width = 55
        Height = 18
        ExplicitWidth = 55
        ExplicitHeight = 18
      end
      inherited sbAdd: TSpeedButton
        Left = 301
        ExplicitLeft = 305
      end
      inherited sbInsert: TSpeedButton
        Left = 357
        ExplicitLeft = 361
      end
      inherited sbDelete: TSpeedButton
        Left = 411
        ExplicitLeft = 417
      end
      inherited seNumber: TJvSpinEdit
        Height = 26
        ExplicitHeight = 26
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 583
      Height = 132
      ColCount = 3
      OnBeforeDrawCell = frameRastersGridBeforeDrawCell
      OnButtonClick = frameRastersGridButtonClick
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
          AutoAdjustColWidths = True
        end
        item
          AutoAdjustRowHeights = True
          AutoAdjustCaptionRowHeights = False
          ButtonCaption = 'Browse'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = True
          ButtonWidth = 70
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
          ComboUsed = True
          Format = rcf4String
          LimitToList = True
          MaxLength = 0
          ParentButtonFont = False
          PickList.Strings = (
            'Surfer Grid File (*.grd)'
            'ESRI ASCII Raster (*.asc)')
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end>
      ExplicitWidth = 579
      ExplicitHeight = 131
      ColWidths = (
        64
        64
        201)
    end
  end
  object dlgOpenFile: TOpenDialog
    DefaultExt = '.grd'
    Filter = 
      'Surfer Grid files (*.grd)|*.grd|ESRII ASCII Raster files (*.asc)' +
      '|*.asc'
    Options = [ofFileMustExist, ofEnableSizing]
    Title = 'Select a raster file'
    Left = 312
    Top = 55
  end
end
