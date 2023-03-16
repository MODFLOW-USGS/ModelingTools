inherited frmLayersToExport: TfrmLayersToExport
  HelpType = htKeyword
  HelpKeyword = 'Layers-to-Export-Dialog-Box'
  Caption = 'Layers to Export'
  ClientWidth = 381
  ExplicitWidth = 393
  ExplicitHeight = 272
  TextHeight = 18
  object clbSelectedLayers: TCheckListBox
    Left = 231
    Top = 8
    Width = 142
    Height = 179
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
    Top = 193
    Width = 381
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 192
    ExplicitWidth = 352
    DesignSize = (
      381
      41)
    object btnHelp: TBitBtn
      Left = 28
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = -1
    end
    object btnOK: TBitBtn
      Left = 142
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      ExplicitLeft = 113
    end
    object btnCancel: TBitBtn
      Left = 256
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 227
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
