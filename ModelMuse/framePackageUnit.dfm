object framePackage: TframePackage
  Left = 0
  Top = 0
  Width = 422
  Height = 161
  TabOrder = 0
  TabStop = True
  DesignSize = (
    422
    161)
  object lblComments: TLabel
    Left = 16
    Top = 39
    Width = 50
    Height = 13
    Caption = 'Comments'
    Enabled = False
  end
  object lblPackage: TLabel
    Left = 16
    Top = 12
    Width = 50
    Height = 13
    Caption = 'lblPackage'
  end
  object memoComments: TMemo
    Left = 16
    Top = 62
    Width = 391
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object rcSelectionController: TRbwController
    ControlList = <
      item
        Control = lblComments
      end
      item
        Control = memoComments
      end>
    Enabled = False
    Left = 96
    Top = 16
  end
end
