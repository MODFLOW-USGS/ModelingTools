object frmSettings: TfrmSettings
  Left = 0
  Top = 0
  Caption = 'Settings'
  ClientHeight = 441
  ClientWidth = 649
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  TextHeight = 15
  object pcSettings: TPageControl
    Left = 0
    Top = 0
    Width = 649
    Height = 400
    ActivePage = tabWarnings
    Align = alClient
    TabOrder = 0
    object TabPrimary: TTabSheet
      Caption = 'Primary Identifiers'
      object memoPrimary: TMemo
        Left = 0
        Top = 0
        Width = 641
        Height = 370
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tabSecondary: TTabSheet
      Caption = 'Secondary Identifiers'
      ImageIndex = 1
      object memoSecondary: TMemo
        Left = 0
        Top = 0
        Width = 641
        Height = 370
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tabObsID: TTabSheet
      Caption = 'Observation Identifiers'
      ImageIndex = 2
      object memoObservations: TMemo
        Left = 0
        Top = 0
        Width = 641
        Height = 370
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tabIgnored: TTabSheet
      Caption = 'Ignored'
      ImageIndex = 3
      object memoIgnored: TMemo
        Left = 0
        Top = 0
        Width = 641
        Height = 370
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tabErrors: TTabSheet
      Caption = 'Errors Identifiers'
      ImageIndex = 4
      object memoErrors: TMemo
        Left = 0
        Top = 0
        Width = 641
        Height = 370
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tabWarnings: TTabSheet
      Caption = 'Warning Identifiers'
      ImageIndex = 5
      object memoWarnings: TMemo
        Left = 0
        Top = 0
        Width = 641
        Height = 370
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 400
    Width = 649
    Height = 41
    Align = alBottom
    TabOrder = 1
    object btnCancel: TBitBtn
      Left = 544
      Top = 8
      Width = 75
      Height = 25
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 0
    end
    object btnOK: TBitBtn
      Left = 463
      Top = 8
      Width = 75
      Height = 25
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
  end
end
