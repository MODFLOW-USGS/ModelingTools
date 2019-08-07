inherited frmJpegWarning: TfrmJpegWarning
  Left = 435
  Top = 433
  Height = 252
  HelpKeyword = 'ImportEditBitmap'
  VertScrollBar.Range = 0
  ActiveControl = memoURL
  Caption = 'Jpeg Warning'
  ExplicitHeight = 252
  PixelsPerInch = 96
  TextHeight = 17
  object memoURL: TMemo
    Left = 0
    Top = 129
    Width = 436
    Height = 44
    Align = alClient
    Lines.Strings = (
      
        'http://www.microsoft.com/downloads/details.aspx?familyid=6A63AB9' +
        'C-DF12-4D41-933C-BE590FEAA05A')
    TabOrder = 0
    WordWrap = False
  end
  object pnlWarning: TPanel
    Left = 0
    Top = 0
    Width = 436
    Height = 129
    Align = alTop
    ParentColor = True
    TabOrder = 1
    DesignSize = (
      436
      129)
    object lblWarning: TLabel
      Left = 8
      Top = 8
      Width = 431
      Height = 85
      Anchors = [akLeft, akTop, akRight]
      Caption = 
        'Because GDI+ is not installed, importing JPEG files has been dis' +
        'abled.  To enable importing JPEG files, get GDI+ from the URL sh' +
        'own below and put gdiplus.dll in a location where it can be foun' +
        'd by this program.  The directory where the program is installed' +
        ' would be a good place to put it.'
      WordWrap = True
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 173
    Width = 436
    Height = 45
    Align = alBottom
    ParentColor = True
    TabOrder = 2
    DesignSize = (
      436
      45)
    object btnClose: TBitBtn
      Left = 352
      Top = 8
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 2
      Kind = bkClose
    end
    object btnHelp: TBitBtn
      Left = 264
      Top = 8
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnGoToWebSite: TButton
      Left = 8
      Top = 8
      Width = 145
      Height = 33
      Caption = 'Go To Web Site'
      TabOrder = 0
      OnClick = btnGoToWebSiteClick
    end
  end
end
