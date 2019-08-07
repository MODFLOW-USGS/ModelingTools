inherited framePackageHob: TframePackageHob
  Height = 214
  ExplicitHeight = 214
  DesignSize = (
    422
    214)
  object lblDryHead: TLabel [2]
    Left = 16
    Top = 164
    Width = 173
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Head printed for dry cells (HOBDRY)'
  end
  inherited memoComments: TMemo
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object rdeDryHead: TRbwDataEntry [4]
    Left = 16
    Top = 183
    Width = 145
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    TabOrder = 1
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  inherited rcSelectionController: TRbwController
    ControlList = <
      item
        Control = lblComments
      end
      item
        Control = memoComments
      end
      item
        Control = rdeDryHead
      end>
  end
end
