inherited frameMt3dmsDispersionPkg: TframeMt3dmsDispersionPkg
  Height = 262
  ExplicitHeight = 262
  DesignSize = (
    422
    262)
  object cbMultiDiffusion: TCheckBox [3]
    Left = 16
    Top = 157
    Width = 265
    Height = 37
    Caption = 
      'Specify diffusion coefficient separately for each mobile compone' +
      'nt  (MultiDiffusion)'
    Enabled = False
    TabOrder = 1
    WordWrap = True
  end
  object cbCrossTermsUsed: TCheckBox [4]
    Left = 16
    Top = 208
    Width = 391
    Height = 41
    Caption = 
      'Cross dispersion terms are enabled (inverse of NOCROSS) (MT3D-US' +
      'GS only)'
    Enabled = False
    TabOrder = 2
    WordWrap = True
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
        Control = cbMultiDiffusion
      end
      item
        Control = cbCrossTermsUsed
      end>
  end
end
