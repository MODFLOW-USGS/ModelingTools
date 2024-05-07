inherited frmLayersToExport: TfrmLayersToExport
  HelpType = htKeyword
  HelpKeyword = 'Layers-to-Export-Dialog-Box'
  Caption = 'Layers to Export'
  ClientHeight = 237
  ClientWidth = 381
  ExplicitWidth = 397
  ExplicitHeight = 276
  TextHeight = 18
  object clbSelectedLayers: TCheckListBox
    Left = 231
    Top = 8
    Width = 142
    Height = 182
    Anchors = [akLeft, akTop, akBottom]
    Enabled = False
    ItemHeight = 18
    TabOrder = 0
  end
  object rgLayersToExport: TRadioGroup
    Left = 8
    Top = 8
    Width = 217
    Height = 105
    Caption = 'Layer to Export'
    ItemIndex = 0
    Items.Strings = (
      'Current layer'
      'Selected layers'
      'All layers')
    TabOrder = 1
    OnClick = rgLayersToExportClick
  end
  object pnlBase: TPanel
    Left = 0
    Top = 196
    Width = 381
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 197
    DesignSize = (
      381
      41)
    object btnHelp: TBitBtn
      Left = 20
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 134
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
    end
    object btnCancel: TBitBtn
      Left = 248
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
  end
  object rgExportAs: TRadioGroup
    Left = 8
    Top = 120
    Width = 217
    Height = 67
    Caption = 'Export as'
    Enabled = False
    ItemIndex = 0
    Items.Strings = (
      'Single Shapefile'
      'One Shapefile per Layer')
    TabOrder = 3
  end
end
