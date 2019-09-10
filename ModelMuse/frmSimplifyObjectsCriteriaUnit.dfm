inherited frmSimplifyObjectsCriteria: TfrmSimplifyObjectsCriteria
  Caption = 'Criteria for Simplifying Objects'
  ClientWidth = 356
  ExplicitWidth = 372
  PixelsPerInch = 96
  TextHeight = 18
  object lblAngle: TLabel
    Left = 15
    Top = 24
    Width = 321
    Height = 18
    Caption = 'Maximum allowable change in angle (degrees)'
  end
  object lblSpacing: TLabel
    Left = 8
    Top = 80
    Width = 272
    Height = 36
    Caption = 
      'Maximum allowable distance between vertex being removed and its ' +
      'neighbors'
    WordWrap = True
  end
  object rdeAngle: TRbwDataEntry
    Left = 8
    Top = 48
    Width = 145
    Height = 22
    TabOrder = 0
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object rdeSpacing: TRbwDataEntry
    Left = 8
    Top = 122
    Width = 145
    Height = 22
    TabOrder = 1
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object pnl1: TPanel
    Left = 0
    Top = 185
    Width = 356
    Height = 41
    Align = alBottom
    TabOrder = 2
    ExplicitLeft = -44
    ExplicitWidth = 468
    DesignSize = (
      356
      41)
    object btnHelp: TBitBtn
      Left = 91
      Top = 4
      Width = 82
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
    end
    object btnOK: TBitBtn
      Left = 179
      Top = 4
      Width = 82
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
    end
    object btnCancel: TBitBtn
      Left = 267
      Top = 4
      Width = 82
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
  end
end
