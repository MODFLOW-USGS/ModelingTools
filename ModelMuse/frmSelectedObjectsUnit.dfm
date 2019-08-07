inherited frmSelectedObjects: TfrmSelectedObjects
  Left = 554
  Top = 516
  HelpType = htKeyword
  HelpKeyword = 'Selected_Objects_Dialog_Box'
  ActiveControl = lbSelected
  Caption = 'Selected Objects'
  ClientHeight = 224
  Position = poDesigned
  ExplicitWidth = 442
  ExplicitHeight = 269
  PixelsPerInch = 120
  TextHeight = 18
  object lbSelected: TListBox
    Left = 0
    Top = 0
    Width = 424
    Height = 178
    Align = alClient
    ItemHeight = 18
    TabOrder = 0
    ExplicitHeight = 180
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 178
    Width = 424
    Height = 46
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    ExplicitTop = 180
    DesignSize = (
      424
      46)
    object btnClose: TBitBtn
      Left = 110
      Top = 6
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 1
    end
    object btnHelp: TBitBtn
      Left = 15
      Top = 6
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
    end
  end
end
