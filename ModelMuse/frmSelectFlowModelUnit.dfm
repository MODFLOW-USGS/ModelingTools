inherited frmSelectFlowModel: TfrmSelectFlowModel
  Caption = 'Select Flow Model'
  ExplicitWidth = 412
  ExplicitHeight = 258
  TextHeight = 18
  object rgFlowModels: TRadioGroup
    Left = 0
    Top = 49
    Width = 400
    Height = 129
    Align = alClient
    Caption = 'Flow Models'
    TabOrder = 0
    ExplicitLeft = 72
    ExplicitTop = 64
    ExplicitWidth = 185
    ExplicitHeight = 105
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 400
    Height = 49
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 392
      Height = 41
      Align = alClient
      Caption = 
        'ModelMuse can only import one flow model in a simulation. Pick t' +
        'he flow model to import.'
      WordWrap = True
      ExplicitLeft = 1
      ExplicitTop = 1
      ExplicitWidth = 330
      ExplicitHeight = 36
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 178
    Width = 400
    Height = 42
    Align = alBottom
    TabOrder = 2
    ExplicitLeft = -40
    ExplicitWidth = 440
    DesignSize = (
      400
      42)
    object btnHelp: TBitBtn
      Left = 121
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      ExplicitLeft = 161
    end
    object btnOK: TBitBtn
      Left = 210
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      ExplicitLeft = 250
    end
    object btnCancel: TBitBtn
      Left = 299
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 339
    end
  end
end
