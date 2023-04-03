inherited frmLinkStreams: TfrmLinkStreams
  HelpType = htKeyword
  HelpKeyword = 'Link_Streams_Dialog_Box'
  Caption = 'Link Streams'
  ClientHeight = 308
  ClientWidth = 246
  ExplicitWidth = 258
  ExplicitHeight = 346
  TextHeight = 18
  object lblTolerance: TLabel
    Left = 7
    Top = 192
    Width = 97
    Height = 18
    Caption = 'Link tolerance'
  end
  object rgWhatToLink: TRadioGroup
    Left = 8
    Top = 121
    Width = 231
    Height = 65
    Caption = 'What to link'
    ItemIndex = 1
    Items.Strings = (
      'All streams'
      'Selected streams')
    TabOrder = 1
  end
  object rdeTolerance: TRbwDataEntry
    Left = 7
    Top = 212
    Width = 231
    Height = 22
    TabOrder = 2
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object cbKeepExistingLinkages: TCheckBox
    Left = 7
    Top = 240
    Width = 177
    Height = 17
    Caption = 'Keep existing linkages'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object btnApply: TBitBtn
    Left = 86
    Top = 263
    Width = 73
    Height = 31
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
    NumGlyphs = 2
    TabOrder = 5
    OnClick = btnApplyClick
  end
  object btnClose: TBitBtn
    Left = 165
    Top = 263
    Width = 73
    Height = 31
    Kind = bkClose
    NumGlyphs = 2
    TabOrder = 6
  end
  object btnHelp: TBitBtn
    Left = 7
    Top = 263
    Width = 73
    Height = 31
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 4
    OnClick = btnHelpClick
  end
  object rgStreamtype: TRadioGroup
    Left = 8
    Top = 8
    Width = 233
    Height = 107
    Caption = 'Stream type'
    ItemIndex = 0
    Items.Strings = (
      'SFR'
      'STR'
      'SWR'
      'SFR (MODFLOW-6)')
    TabOrder = 0
    OnClick = rgStreamtypeClick
  end
  object quadtreeStream: TRbwQuadTree
    MaxPoints = 100
    Left = 120
    Top = 8
  end
end
