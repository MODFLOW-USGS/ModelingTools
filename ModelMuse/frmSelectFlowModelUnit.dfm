inherited frmSelectFlowModel: TfrmSelectFlowModel
  Caption = 'Select Flow Model'
  TextHeight = 18
  object rgFlowModels: TRadioGroup
    Left = 0
    Top = 49
    Width = 764
    Height = 110
    Align = alClient
    Caption = 'Flow Models'
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 764
    Height = 49
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 756
      Height = 41
      Align = alClient
      Caption = 
        'ModelMuse can only import one flow model in a simulation. Pick t' +
        'he flow model to import.'
      WordWrap = True
      ExplicitWidth = 330
      ExplicitHeight = 36
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 159
    Width = 764
    Height = 42
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      764
      42)
    object btnHelp: TBitBtn
      Left = 493
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
    end
    object btnOK: TBitBtn
      Left = 582
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
    end
    object btnCancel: TBitBtn
      Left = 671
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
  end
end
