inherited frameScreenObjectCfpFixed: TframeScreenObjectCfpFixed
  Width = 350
  Height = 171
  ExplicitWidth = 350
  ExplicitHeight = 171
  DesignSize = (
    350
    171)
  object lblHint: TLabel
    Left = 3
    Top = 69
    Width = 314
    Height = 75
    Caption = 
      'CFP Fixed heads must be greater than or equal to -1. It is only ' +
      'necessary to specify CFP fixed heads for nodes that are CFP fixe' +
      'd head nodes. Other nodes will automatically have their fixed he' +
      'ads set to -1 which is used to indicate that the head in the nod' +
      'e should be calculated.'
    WordWrap = True
  end
  object pnlCaption: TPanel
    Left = 0
    Top = 0
    Width = 350
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 344
  end
  object edFixedHead: TLabeledEdit
    Left = 3
    Top = 39
    Width = 233
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 57
    EditLabel.Height = 15
    EditLabel.Caption = 'Fixed head'
    TabOrder = 2
    Text = ''
    OnChange = edFixedHeadChange
    ExplicitWidth = 227
  end
  object btnFixedHead: TButton
    Left = 248
    Top = 36
    Width = 90
    Height = 30
    Anchors = [akTop, akRight]
    Caption = 'Edit F()...'
    TabOrder = 1
    ExplicitLeft = 242
  end
end
