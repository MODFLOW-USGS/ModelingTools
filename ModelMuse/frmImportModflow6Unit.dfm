inherited frmImportModflow6: TfrmImportModflow6
  Caption = 'Import MODFLOW 6 Model'
  ClientHeight = 343
  ClientWidth = 625
  OnActivate = FormActivate
  ExplicitWidth = 641
  ExplicitHeight = 382
  TextHeight = 18
  inline frameTransportNameFiles: TframeGrid
    Left = 0
    Top = 68
    Width = 625
    Height = 215
    Align = alClient
    TabOrder = 0
    ExplicitTop = 68
    ExplicitWidth = 625
    ExplicitHeight = 215
    inherited Panel: TPanel
      Top = 174
      Width = 625
      ExplicitTop = 174
      ExplicitWidth = 625
      inherited lbNumber: TLabel
        Width = 55
        Height = 18
        ExplicitWidth = 55
        ExplicitHeight = 18
      end
      inherited sbAdd: TSpeedButton
        Left = 322
        ExplicitLeft = 332
      end
      inherited sbInsert: TSpeedButton
        Left = 382
        ExplicitLeft = 393
      end
      inherited sbDelete: TSpeedButton
        Left = 439
        ExplicitLeft = 454
      end
      inherited seNumber: TJvSpinEdit
        Height = 26
        ExplicitHeight = 26
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 625
      Height = 174
      OnExit = frameTransportNameFilesGridExit
      OnButtonClick = frameTransportNameFilesGridButtonClick
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
      ExplicitWidth = 625
      ExplicitHeight = 174
      ColWidths = (
        615)
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 625
    Height = 68
    Align = alTop
    TabOrder = 1
    DesignSize = (
      625
      68)
    object lblFlowSimFile: TLabel
      Left = 8
      Top = 8
      Width = 302
      Height = 18
      Caption = 'MODFLOW 6 Simulation Name File for flow'
    end
    object edFlowSimFile: TJvFilenameEdit
      Left = 8
      Top = 32
      Width = 581
      Height = 26
      Filter = 'Simulation Name File|*mfsim.nam'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = ''
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 283
    Width = 625
    Height = 41
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      625
      41)
    object btnHelp: TBitBtn
      Left = 321
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
    end
    object btnOK: TBitBtn
      Left = 409
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 497
      Top = 6
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
  end
  object stat1: TStatusBar
    Left = 0
    Top = 324
    Width = 625
    Height = 19
    Panels = <>
  end
  object odSimFiles: TOpenDialog
    Filter = 'Simulation Name File|*mfsim.nam'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 416
    Top = 88
  end
end
