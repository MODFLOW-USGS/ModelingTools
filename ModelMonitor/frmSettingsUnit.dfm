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
    ActivePage = TabPrimary
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 645
    ExplicitHeight = 399
    object TabPrimary: TTabSheet
      Caption = 'Primary Identifiers'
      object memoPrimary: TMemo
        Left = 0
        Top = 0
        Width = 641
        Height = 329
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
        ExplicitWidth = 637
        ExplicitHeight = 328
      end
      object pnlPrimary: TPanel
        Left = 0
        Top = 329
        Width = 641
        Height = 41
        Align = alBottom
        Caption = 
          'Primary identifiers are used to identify the highest level items' +
          ' in the table of contents'
        TabOrder = 1
        ExplicitTop = 328
        ExplicitWidth = 637
      end
    end
    object tabSecondary: TTabSheet
      Caption = 'Secondary Identifiers'
      ImageIndex = 1
      object memoSecondary: TMemo
        Left = 0
        Top = 0
        Width = 641
        Height = 329
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object pnlSecondary: TPanel
        Left = 0
        Top = 329
        Width = 641
        Height = 41
        Align = alBottom
        Caption = 
          'Secondary identifiers are for used to identify sub-headings in t' +
          'he table of contents'
        TabOrder = 1
      end
    end
    object tabObsID: TTabSheet
      Caption = 'Observation Identifiers'
      ImageIndex = 2
      object memoObservations: TMemo
        Left = 0
        Top = 0
        Width = 641
        Height = 329
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object pnlObservations: TPanel
        Left = 0
        Top = 329
        Width = 641
        Height = 41
        Align = alBottom
        Caption = 
          'Observation Identifiers are used to identify a list of observati' +
          'ons'
        TabOrder = 1
      end
    end
    object tabIgnored: TTabSheet
      Caption = 'Ignored'
      ImageIndex = 3
      object memoIgnored: TMemo
        Left = 0
        Top = 0
        Width = 641
        Height = 329
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object pnlIgnored: TPanel
        Left = 0
        Top = 329
        Width = 641
        Height = 41
        Align = alBottom
        Caption = 
          'Ignored contains text that serve to identify lines that might ot' +
          'herwise be identified as primary or secondary identifiers'
        TabOrder = 1
      end
    end
    object tabErrors: TTabSheet
      Caption = 'Errors Identifiers'
      ImageIndex = 4
      object memoErrors: TMemo
        Left = 0
        Top = 0
        Width = 641
        Height = 329
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object pnlErrors: TPanel
        Left = 0
        Top = 329
        Width = 641
        Height = 41
        Align = alBottom
        Caption = 'Error identifers is used to identify text in error messages'
        TabOrder = 1
      end
    end
    object tabWarnings: TTabSheet
      Caption = 'Warning Identifiers'
      ImageIndex = 5
      object memoWarnings: TMemo
        Left = 0
        Top = 0
        Width = 641
        Height = 329
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object pnlWarnings: TPanel
        Left = 0
        Top = 329
        Width = 641
        Height = 41
        Align = alBottom
        Caption = 'Warning identifers is used to identify text in warning messages'
        TabOrder = 1
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
    ExplicitTop = 399
    ExplicitWidth = 645
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
