inherited frmPEST: TfrmPEST
  Caption = 'PEST'
  PixelsPerInch = 96
  TextHeight = 18
  object tvPEST: TJvPageListTreeView
    Left = 0
    Top = 0
    Width = 121
    Height = 184
    PageDefault = 0
    PageList = pgMain
    Align = alLeft
    Indent = 19
    TabOrder = 0
    Items.Links = {00000000}
  end
  object pgMain: TJvPageList
    Left = 121
    Top = 0
    Width = 303
    Height = 184
    ActivePage = jvspBasic
    PropagateEnable = False
    Align = alClient
    object jvspBasic: TJvStandardPage
      Left = 0
      Top = 0
      Width = 303
      Height = 184
      Caption = 'jvspBasic'
      object cbPEST: TCheckBox
        Left = 16
        Top = 16
        Width = 97
        Height = 17
        Caption = 'Use PEST'
        TabOrder = 0
      end
      object edTemplateCharacter: TLabeledEdit
        Left = 16
        Top = 64
        Width = 121
        Height = 26
        EditLabel.Width = 230
        EditLabel.Height = 18
        EditLabel.Caption = 'Template character: @, $, %, or ?'
        MaxLength = 1
        TabOrder = 1
        OnChange = MarkerChange
      end
      object edFormulaMarker: TLabeledEdit
        Left = 16
        Top = 120
        Width = 121
        Height = 26
        EditLabel.Width = 208
        EditLabel.Height = 18
        EditLabel.Caption = 'Formula marker: @, $, %, or ?'
        MaxLength = 1
        TabOrder = 2
        OnChange = MarkerChange
      end
    end
    object jvspPilotPoints: TJvStandardPage
      Left = 0
      Top = 0
      Width = 303
      Height = 184
      Caption = 'jvspPilotPoints'
      object lblPilotPointSpacing: TLabel
        Left = 16
        Top = 40
        Width = 128
        Height = 18
        Caption = 'Pilot point spacing'
      end
      object rdePilotPointSpacing: TRbwDataEntry
        Left = 16
        Top = 64
        Width = 145
        Height = 22
        TabOrder = 0
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object cbShowPilotPoints: TCheckBox
        Left = 16
        Top = 17
        Width = 193
        Height = 17
        Caption = 'Show pilot points'
        TabOrder = 1
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 184
    Width = 424
    Height = 42
    Align = alBottom
    TabOrder = 2
    object btnHelp: TBitBtn
      Left = 158
      Top = 6
      Width = 83
      Height = 33
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
    end
    object btnOK: TBitBtn
      Left = 247
      Top = 6
      Width = 83
      Height = 33
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 336
      Top = 6
      Width = 83
      Height = 33
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
  end
end
