inherited frmSimplifyObjectsCriteria: TfrmSimplifyObjectsCriteria
  HelpType = htKeyword
  HelpKeyword = 'Criteria_for_Simplifying_Objec'
  Caption = 'Criteria for Simplifying Objects'
  ClientHeight = 198
  ClientWidth = 416
  ExplicitWidth = 432
  ExplicitHeight = 237
  PixelsPerInch = 96
  TextHeight = 18
  object lblAngle: TLabel
    Left = 8
    Top = 24
    Width = 397
    Height = 18
    Caption = 'Minimum allowable angle at node to be deleted (degrees)'
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
    Text = '175'
    OnChange = EnableOK
    DataType = dtReal
    Max = 180.000000000000000000
    CheckMax = True
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeSpacing: TRbwDataEntry
    Left = 8
    Top = 122
    Width = 145
    Height = 22
    TabOrder = 1
    Text = '0'
    OnChange = EnableOK
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object pnl1: TPanel
    Left = 0
    Top = 157
    Width = 416
    Height = 41
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 185
    ExplicitWidth = 356
    DesignSize = (
      416
      41)
    object btnHelp: TBitBtn
      Left = 151
      Top = 4
      Width = 82
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      ExplicitLeft = 91
    end
    object btnOK: TBitBtn
      Left = 239
      Top = 4
      Width = 82
      Height = 33
      Anchors = [akTop, akRight]
      Enabled = False
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      ExplicitLeft = 179
    end
    object btnCancel: TBitBtn
      Left = 327
      Top = 4
      Width = 82
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 267
    end
  end
end
