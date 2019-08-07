inherited frmDeleteImage: TfrmDeleteImage
  HelpType = htKeyword
  HelpKeyword = 'Delete_Image_Dialog_Box'
  Caption = 'Delete Image'
  ClientWidth = 422
  ExplicitWidth = 440
  ExplicitHeight = 271
  PixelsPerInch = 120
  TextHeight = 18
  object clbBitmaps: TCheckListBox
    Left = 0
    Top = 0
    Width = 422
    Height = 181
    Align = alClient
    ItemHeight = 18
    TabOrder = 0
    ExplicitWidth = 424
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 181
    Width = 422
    Height = 45
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    ExplicitWidth = 424
    DesignSize = (
      422
      45)
    object btnOK: TBitBtn
      Left = 236
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object BitBtn1: TBitBtn
      Left = 325
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnHelp: TBitBtn
      Left = 147
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
    end
  end
end
