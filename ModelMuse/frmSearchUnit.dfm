inherited frmSearch: TfrmSearch
  Left = 631
  Top = 301
  Width = 448
  Height = 579
  HelpType = htKeyword
  HelpKeyword = 'Search_for_Objects_Dialog_Box'
  VertScrollBar.Range = 0
  Caption = 'Search for Objects'
  ExplicitWidth = 448
  ExplicitHeight = 579
  PixelsPerInch = 96
  TextHeight = 18
  inherited pnlBottom: TPanel
    Top = 485
    Width = 432
    Height = 56
    ExplicitTop = 478
    ExplicitWidth = 404
    ExplicitHeight = 56
    inherited btnClose: TBitBtn
      Left = 321
      Top = 16
      TabOrder = 2
      ExplicitLeft = 321
      ExplicitTop = 16
    end
    inherited btnHelp: TBitBtn
      Left = 228
      Top = 16
      TabOrder = 1
      OnClick = btnHelpClick
      ExplicitLeft = 228
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
    Width = 432
    Height = 485
    OnChecked = vstObjectsChecked
    ExplicitWidth = 404
    ExplicitHeight = 478
  end
end
