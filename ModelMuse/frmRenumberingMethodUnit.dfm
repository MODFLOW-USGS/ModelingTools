inherited frmRenumberingMethod: TfrmRenumberingMethod
  HelpType = htKeyword
  HelpKeyword = 'Mesh_Renumbering_Method_Dialog'
  Caption = 'Mesh Renumbering Method'
  ClientHeight = 253
  ClientWidth = 302
  ExplicitHeight = 298
  PixelsPerInch = 120
  TextHeight = 18
  object rgMethod: TRadioGroup
    Left = 8
    Top = 8
    Width = 285
    Height = 105
    ItemIndex = 0
    Items.Strings = (
      'Vertical alignment (fast)'
      'Complex (slow)')
    TabOrder = 0
  end
  object btnHelp: TBitBtn
    Left = 9
    Top = 214
    Width = 91
    Height = 33
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 2
    OnClick = btnHelpClick
  end
  object btnOK: TBitBtn
    Left = 106
    Top = 214
    Width = 91
    Height = 33
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 3
  end
  object btnCancel: TBitBtn
    Left = 203
    Top = 215
    Width = 91
    Height = 33
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 4
  end
  object rgRenumberingMethod: TRadioGroup
    Left = 8
    Top = 119
    Width = 286
    Height = 90
    Caption = 'Renumbering method'
    ItemIndex = 1
    Items.Strings = (
      'none'
      'Cuthill and McKee (1969)'
      'Sloan and Randolph (1983)')
    TabOrder = 1
  end
end
