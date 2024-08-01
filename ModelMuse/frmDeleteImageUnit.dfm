inherited frmDeleteImage: TfrmDeleteImage
  HelpType = htKeyword
  HelpKeyword = 'Delete_Image_Dialog_Box'
  Caption = 'Delete Image'
  ClientWidth = 422
  ExplicitWidth = 434
  PixelsPerInch = 120
  TextHeight = 18
  object clbBitmaps: TCheckListBox
    Left = 0
    Top = 0
    Width = 422
    Height = 156
    Align = alClient
    ItemHeight = 18
    TabOrder = 0
    ExplicitWidth = 418
    ExplicitHeight = 155
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 156
    Width = 422
    Height = 45
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    ExplicitTop = 155
    ExplicitWidth = 418
    DesignSize = (
      422
      45)
    object btnOK: TBitBtn
      Left = 228
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 224
    end
    object BitBtn1: TBitBtn
      Left = 317
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 313
    end
    object btnHelp: TBitBtn
      Left = 139
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 135
    end
  end
end
