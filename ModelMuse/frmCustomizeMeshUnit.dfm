inherited frmCustomizeMesh: TfrmCustomizeMesh
  HelpType = htKeyword
  HelpKeyword = 'Customize_SUTRA_Mesh_Dialog_Bo'
  Caption = 'Customize SUTRA Mesh'
  ClientHeight = 222
  ClientWidth = 400
  ExplicitWidth = 412
  ExplicitHeight = 260
  TextHeight = 18
  object cbShowNodeNumbers: TCheckBox
    Left = 16
    Top = 16
    Width = 201
    Height = 17
    Caption = 'Show node numbers'
    TabOrder = 1
  end
  object cbShowElementNumbers: TCheckBox
    Left = 16
    Top = 47
    Width = 201
    Height = 17
    Caption = 'Show element numbers'
    TabOrder = 3
  end
  object btnEditNodeFont: TButton
    Left = 240
    Top = 13
    Width = 144
    Height = 25
    Caption = 'Edit node font'
    TabOrder = 0
    OnClick = btnEditNodeFontClick
  end
  object btnEditElementFont: TButton
    Left = 240
    Top = 44
    Width = 144
    Height = 25
    Caption = 'Edit element font'
    TabOrder = 2
    OnClick = btnEditElementFontClick
  end
  object btnHelp: TBitBtn
    Left = 105
    Top = 181
    Width = 89
    Height = 33
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 7
    OnClick = btnHelpClick
  end
  object btnOK: TBitBtn
    Left = 200
    Top = 181
    Width = 89
    Height = 33
    Caption = '&Apply'
    Default = True
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    ModalResult = 1
    NumGlyphs = 2
    TabOrder = 8
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 295
    Top = 181
    Width = 89
    Height = 33
    Kind = bkClose
    NumGlyphs = 2
    TabOrder = 9
  end
  object cbNodeCellOutline: TCheckBox
    Left = 16
    Top = 80
    Width = 201
    Height = 17
    Caption = 'Show cell outlines'
    TabOrder = 4
  end
  object cbShowElements: TCheckBox
    Left = 16
    Top = 112
    Width = 209
    Height = 17
    Caption = 'Show element outlines'
    TabOrder = 5
  end
  object cbShowElementCenters: TCheckBox
    Left = 16
    Top = 144
    Width = 368
    Height = 17
    Caption = 'Show element centers along cross section'
    TabOrder = 6
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 216
  end
end
