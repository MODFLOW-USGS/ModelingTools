inherited frmSelectedObjects: TfrmSelectedObjects
  Left = 554
  Top = 516
  HelpType = htKeyword
  HelpKeyword = 'Selected_Objects_Dialog_Box'
  ActiveControl = lbSelected
  Caption = 'Selected Objects'
  ClientHeight = 223
  ClientWidth = 396
  Position = poDesigned
  ExplicitHeight = 262
  PixelsPerInch = 120
  TextHeight = 18
  object lbSelected: TListBox
    Left = 0
    Top = 0
    Width = 396
    Height = 177
    Align = alClient
    ItemHeight = 18
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 177
    Width = 396
    Height = 46
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    ExplicitWidth = 422
    DesignSize = (
      396
      46)
    object btnClose: TBitBtn
      Left = 94
      Top = 6
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 1
      ExplicitLeft = 98
    end
    object btnHelp: TBitBtn
      Left = -1
      Top = 6
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 3
    end
  end
end
