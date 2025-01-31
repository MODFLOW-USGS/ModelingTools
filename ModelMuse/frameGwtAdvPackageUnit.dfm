inherited frameGwtAdvPackage: TframeGwtAdvPackage
  Width = 453
  Height = 375
  ExplicitWidth = 453
  ExplicitHeight = 375
  DesignSize = (
    453
    375)
  inherited lblComments: TLabel
    StyleElements = [seFont, seClient, seBorder]
  end
  inherited lblPackage: TLabel
    StyleElements = [seFont, seClient, seBorder]
  end
  object lblAdePercel: TLabel [2]
    Left = 167
    Top = 283
    Width = 250
    Height = 38
    Caption = 'Fractional cell distance for adaptive time stepping (ATS_PERCEL)'
    WordWrap = True
  end
  inherited memoComments: TMemo
    Width = 422
    StyleElements = [seFont, seClient, seBorder]
  end
  object rgScheme: TRadioGroup [4]
    Left = 16
    Top = 157
    Width = 422
    Height = 105
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Advection Scheme'
    Enabled = False
    ItemIndex = 0
    Items.Strings = (
      'Upstream'
      'Central (rarely used)'
      'Total Variation Diminishing (TVD)')
    TabOrder = 1
    ExplicitWidth = 391
  end
  object rdeAdePercel: TRbwDataEntry [5]
    Left = 16
    Top = 280
    Width = 145
    Height = 22
    TabOrder = 2
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMax = True
    CheckMin = True
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
        Control = rgScheme
      end>
  end
end
