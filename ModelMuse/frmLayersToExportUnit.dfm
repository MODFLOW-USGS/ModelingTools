inherited frmLayersToExport: TfrmLayersToExport
  HelpType = htKeyword
  HelpKeyword = 'Layers-to-Export-Dialog-Box'
  Caption = 'Layers to Export'
  ClientWidth = 356
  ExplicitWidth = 372
  TextHeight = 18
  object clbSelectedLayers: TCheckListBox
    Left = 208
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
    Width = 185
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
    Width = 356
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 426
    DesignSize = (
      356
      41)
    object btnHelp: TBitBtn
      Left = 11
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 81
    end
    object btnOK: TBitBtn
      Left = 125
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      ExplicitLeft = 195
    end
    object btnCancel: TBitBtn
      Left = 239
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 309
    end
  end
end
