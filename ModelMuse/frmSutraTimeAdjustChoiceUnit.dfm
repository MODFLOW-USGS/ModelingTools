inherited frmSutraTimeAdjustChoice: TfrmSutraTimeAdjustChoice
  HelpType = htKeyword
  HelpKeyword = 'Adjust_Times_Dialog_Box'
  Caption = 'Adjust Times'
  ClientHeight = 189
  ClientWidth = 424
  ExplicitWidth = 440
  ExplicitHeight = 228
  TextHeight = 18
  object lblMessage: TLabel
    Left = 8
    Top = 8
    Width = 407
    Height = 54
    Caption = 
      'The times for the schedule do not match the times for this obser' +
      'vation when it was originally created. How should this be treate' +
      'd?'
    WordWrap = True
  end
  object rgTimeTreatment: TRadioGroup
    Left = 8
    Top = 76
    Width = 408
    Height = 77
    Caption = 'Treatment'
    ItemIndex = 1
    Items.Strings = (
      'Use the times from the time schedule'
      'Convert to a custom time schedule')
    TabOrder = 0
  end
  object btnOK: TBitBtn
    Left = 341
    Top = 159
    Width = 75
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 1
  end
  object btnHelp: TBitBtn
    Left = 260
    Top = 159
    Width = 75
    Height = 25
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 2
    OnClick = btnHelpClick
  end
end
