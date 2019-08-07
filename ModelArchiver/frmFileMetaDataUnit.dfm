object frmFileMetaData: TfrmFileMetaData
  Left = 0
  Top = 0
  Caption = 'Archive File Metadata Descriptions'
  ClientHeight = 330
  ClientWidth = 782
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mm1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object spl1: TSplitter
    Left = 233
    Top = 0
    Width = 5
    Height = 330
    ExplicitLeft = 121
  end
  object tvFiles: TTreeView
    Left = 0
    Top = 0
    Width = 233
    Height = 330
    Align = alLeft
    HideSelection = False
    Indent = 19
    TabOrder = 0
    OnMouseUp = tvFilesMouseUp
  end
  object pnl1: TPanel
    Left = 238
    Top = 0
    Width = 544
    Height = 330
    Align = alClient
    TabOrder = 1
    DesignSize = (
      544
      330)
    object lblDescription: TLabel
      Left = 6
      Top = 176
      Width = 63
      Height = 16
      Caption = 'Description'
    end
    object memoDescription: TMemo
      Left = 6
      Top = 195
      Width = 529
      Height = 127
      Anchors = [akLeft, akTop, akRight]
      Lines.Strings = (
        '')
      TabOrder = 3
      OnChange = memoDescriptionChange
    end
    object edUrl: TLabeledEdit
      Left = 7
      Top = 136
      Width = 528
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 16
      EditLabel.Height = 16
      EditLabel.Caption = 'Url'
      TabOrder = 2
      OnChange = edUrlChange
    end
    object edFiletype: TLabeledEdit
      Left = 6
      Top = 88
      Width = 121
      Height = 24
      EditLabel.Width = 52
      EditLabel.Height = 16
      EditLabel.Caption = 'File Type'
      TabOrder = 1
      OnChange = edFiletypeChange
    end
    object edReportID: TLabeledEdit
      Left = 6
      Top = 32
      Width = 121
      Height = 24
      EditLabel.Width = 54
      EditLabel.Height = 16
      EditLabel.Caption = 'Report ID'
      TabOrder = 0
      OnChange = edReportIDChange
    end
  end
  object mm1: TMainMenu
    Left = 288
    Top = 24
    object miFile: TMenuItem
      Caption = 'File'
      object miSelectArchiveDirectory: TMenuItem
        Caption = 'Select Archive Directory'
        OnClick = miSelectArchiveDirectoryClick
      end
      object miSaveDescriptions: TMenuItem
        Caption = 'Save Descriptions'
        OnClick = miSaveDescriptionsClick
      end
    end
  end
  object sdDescriptions: TSaveDialog
    DefaultExt = '.txt'
    Left = 448
    Top = 56
  end
  object dlgOpenTemplate: TOpenDialog
    DefaultExt = '.xml'
    Filter = '*.xml|*.xml'
    Left = 542
    Top = 56
  end
end
