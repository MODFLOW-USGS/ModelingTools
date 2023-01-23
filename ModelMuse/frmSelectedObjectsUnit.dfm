inherited frmSelectedObjects: TfrmSelectedObjects
  Left = 554
  Top = 516
  HelpType = htKeyword
  HelpKeyword = 'Selected_Objects_Dialog_Box'
  ActiveControl = lbSelected
  Caption = 'Selected Objects'
  ClientHeight = 224
  Position = poDesigned
  ExplicitWidth = 438
  ExplicitHeight = 262
  PixelsPerInch = 120
  TextHeight = 18
  object lbSelected: TListBox
    Left = 0
    Top = 0
    Width = 426
    Height = 178
    Align = alClient
    ItemHeight = 18
    TabOrder = 0
    ExplicitWidth = 422
    ExplicitHeight = 177
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 178
    Width = 426
    Height = 46
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    ExplicitTop = 177
    ExplicitWidth = 422
    DesignSize = (
      426
      46)
    object btnClose: TBitBtn
      Left = 106
      Top = 6
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 1
      ExplicitLeft = 102
    end
    object btnHelp: TBitBtn
      Left = 11
      Top = 6
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 7
    end
  end
end
