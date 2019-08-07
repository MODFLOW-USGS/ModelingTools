inherited frmDuplicateNames: TfrmDuplicateNames
  HelpType = htKeyword
  HelpKeyword = 'Duplicate_Names_Dialog_Box'
  Caption = 'Duplicate Names'
  ClientHeight = 376
  ClientWidth = 436
  ExplicitWidth = 452
  ExplicitHeight = 414
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 210
    Width = 436
    Height = 166
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      436
      166)
    object btnOK: TBitBtn
      Left = 343
      Top = 124
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Enabled = False
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 3
    end
    object btnHelp: TBitBtn
      Left = 343
      Top = 46
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnHelpClick
    end
    object rgResponse: TRadioGroup
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 333
      Height = 158
      Align = alLeft
      Caption = 'Response'
      Items.Strings = (
        'Create new data sets with different names'
        
          'Assign to the existing data set but keep existing objects for th' +
          'at data set'
        
          'Assign to the existing data set and delete existing objects for ' +
          'that data set')
      TabOrder = 0
      WordWrap = True
      OnClick = rgResponseClick
    end
    object btn1: TBitBtn
      Left = 343
      Top = 85
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 436
    Height = 49
    Align = alTop
    TabOrder = 0
    object lblTop: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 428
      Height = 41
      Align = alClient
      Caption = 
        'The following attributes have names that match existing data set' +
        's. How do you want to to handle this?'
      WordWrap = True
      ExplicitWidth = 417
      ExplicitHeight = 36
    end
  end
  object memoDuplicateNames: TMemo
    Left = 0
    Top = 49
    Width = 436
    Height = 161
    Align = alClient
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
