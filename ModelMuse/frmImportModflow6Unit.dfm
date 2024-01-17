inherited frmImportModflow6: TfrmImportModflow6
  Caption = 'frmImportModflow6'
  ClientHeight = 345
  ClientWidth = 633
  OnActivate = FormActivate
  ExplicitWidth = 645
  ExplicitHeight = 383
  TextHeight = 18
  inline frameTransportNameFiles: TframeGrid
    Left = 0
    Top = 68
    Width = 633
    Height = 236
    Align = alClient
    TabOrder = 0
    ExplicitTop = 68
    ExplicitWidth = 629
    ExplicitHeight = 235
    inherited Panel: TPanel
      Top = 195
      Width = 633
      ExplicitTop = 194
      ExplicitWidth = 629
      inherited lbNumber: TLabel
        Width = 55
        Height = 18
        ExplicitWidth = 55
        ExplicitHeight = 18
      end
      inherited sbAdd: TSpeedButton
        Left = 326
        ExplicitLeft = 332
      end
      inherited sbInsert: TSpeedButton
        Left = 387
        ExplicitLeft = 393
      end
      inherited sbDelete: TSpeedButton
        Left = 445
        ExplicitLeft = 454
      end
      inherited seNumber: TJvSpinEdit
        Height = 26
        ExplicitHeight = 26
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 633
      Height = 195
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
      ExplicitWidth = 629
      ExplicitHeight = 194
      ColWidths = (
        615)
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 633
    Height = 68
    Align = alTop
    TabOrder = 1
    ExplicitWidth = 629
    DesignSize = (
      633
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
      Width = 605
      Height = 26
      Filter = 'Simulation Name File|*mfsim.nam'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = ''
      ExplicitWidth = 601
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 304
    Width = 633
    Height = 41
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 303
    ExplicitWidth = 629
    DesignSize = (
      633
      41)
    object btnHelp: TBitBtn
      Left = 345
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      ExplicitLeft = 341
    end
    object btnOK: TBitBtn
      Left = 433
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 429
    end
    object btnCancel: TBitBtn
      Left = 521
      Top = 6
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 517
    end
  end
  object odSimFiles: TOpenDialog
    Filter = 'Simulation Name File|*mfsim.nam'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 416
    Top = 88
  end
end
