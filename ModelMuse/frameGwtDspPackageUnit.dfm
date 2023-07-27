inherited frameGwtDspPackage: TframeGwtDspPackage
  Width = 525
  Height = 434
  ExplicitWidth = 525
  ExplicitHeight = 434
  DesignSize = (
    525
    434)
  inherited memoComments: TMemo
    Width = 494
    ExplicitWidth = 494
  end
  object cbUseXT3D: TCheckBox [3]
    Left = 16
    Top = 168
    Width = 289
    Height = 17
    Caption = 'Use XT3D (inverse of XT3D_OFF)'
    Checked = True
    Enabled = False
    State = cbChecked
    TabOrder = 1
    OnClick = cbUseXT3DClick
  end
  object cbXT3D_RHS: TCheckBox [4]
    Left = 16
    Top = 191
    Width = 391
    Height = 17
    Caption = 'Some XT3D terms on Right Hand Side (XT3D_RHS)'
    Enabled = False
    TabOrder = 2
  end
  object rgLongDisp: TRadioGroup [5]
    Left = 16
    Top = 216
    Width = 494
    Height = 73
    Caption = 'Longitudinal dispersivity treatment'
    Enabled = False
    ItemIndex = 0
    Items.Strings = (
      'Single data set'
      'Separate data sets for horizontal and vertical flow')
    TabOrder = 3
    WordWrap = True
  end
  object rgTransDisp: TRadioGroup [6]
    Left = 16
    Top = 295
    Width = 494
    Height = 74
    Caption = 'Transverse dispersivity for horizontal flow treatment'
    Enabled = False
    ItemIndex = 0
    Items.Strings = (
      'Single data set'
      'Separate data sets for horizontal and vertical dispersivity')
    TabOrder = 4
    WordWrap = True
  end
  object cbVertFlowTransDisp: TCheckBox [7]
    Left = 16
    Top = 376
    Width = 441
    Height = 17
    Caption = 'Specify transverse dispersivity for vertical flow'
    TabOrder = 5
  end
  object cbSeparateDataSets: TCheckBox [8]
    Left = 16
    Top = 399
    Width = 457
    Height = 17
    Caption = 'Use separate data sets for each species'
    Enabled = False
    TabOrder = 6
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
        Control = cbUseXT3D
      end
      item
        Control = rgLongDisp
      end
      item
        Control = rgTransDisp
      end
      item
        Control = cbSeparateDataSets
      end>
    OnEnabledChange = rcSelectionControllerEnabledChange
  end
end
