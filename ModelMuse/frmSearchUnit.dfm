inherited frmSearch: TfrmSearch
  Left = 631
  Top = 301
  HelpType = htKeyword
  HelpKeyword = 'Search_for_Objects_Dialog_Box'
  Caption = 'Search for Objects'
  ExplicitWidth = 426
  ExplicitHeight = 449
  TextHeight = 18
  inherited pnlBottom: TPanel
    Top = 355
    Height = 56
    ExplicitTop = 354
    ExplicitWidth = 410
    ExplicitHeight = 56
    inherited btnClose: TBitBtn
      Left = 313
      Top = 16
      TabOrder = 2
      ExplicitLeft = 309
      ExplicitTop = 16
    end
    inherited btnHelp: TBitBtn
      Left = 220
      Top = 16
      TabOrder = 1
      OnClick = btnHelpClick
      ExplicitLeft = 216
      ExplicitTop = 16
    end
    object rgDirecton: TRadioGroup
      Left = 8
      Top = 8
      Width = 212
      Height = 41
      Caption = 'Direction'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'Top'
        'Front'
        'Side')
      TabOrder = 0
      OnClick = rgDirectonClick
    end
  end
  inherited vstObjects: TVirtualStringTree
    Height = 355
    OnChecked = vstObjectsChecked
    ExplicitWidth = 414
    ExplicitHeight = 355
  end
end
